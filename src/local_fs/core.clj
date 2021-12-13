(ns local-fs.core
  "File system utilities in clojure."
  (:refer-clojure :exclude [name parents])
  (:require [clojure.zip :as zip]
            [colorize.core :as colorize]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as sh]
            [clj-compress.core :as c]
            [clojure.tools.logging :as log]
            [clj-file-zip.core :as clj-zip])
  (:import java.net.URL
           java.util.Collections
           java.io.FileNotFoundException
           (java.util.jar JarFile JarEntry)
           (java.nio.file CopyOption
                          Files
                          FileSystem
                          FileSystems
                          LinkOption
                          OpenOption
                          NoSuchFileException
                          Path
                          Paths
                          StandardCopyOption)
           (java.io File FilenameFilter)
           (java.nio.file.attribute FileAttribute
                                    PosixFilePermissions
                                    UserPrincipalNotFoundException)
           (org.apache.commons.compress.archivers.zip ZipFile ZipArchiveEntry)))

;; ---------------------------- Basic API --------------------------------
;; Once you've started a JVM, that JVM's working directory is set in stone
;; and cannot be changed. This library will provide a way to simulate a
;; working directory change. `cwd` is considered to be the current working
;; directory for functions in this library. Unfortunately, this will only
;; apply to functions inside this library since we can't change the JVM's
;; actual working directory.
(def ^{:doc "Current working directory. This cannot be changed in the JVM.
             Changing this will only change the working directory for functions
             in this library."
       :dynamic true}
  *cwd* (.getCanonicalFile (io/file ".")))

(def default-file-system (FileSystems/getDefault))

(def file-separator (.getSeparator default-file-system))

(let [homedir (io/file (System/getProperty "user.home"))
      usersdir (.getParent homedir)]
  (defn home
    "With no arguments, returns the current value of the `user.home` system
     property. If a `user` is passed, returns that user's home directory. It
     is naively assumed to be a directory with the same name as the `user`
     located relative to the parent of the current value of `user.home`."
    ([] homedir)
    ([user] (if (empty? user) homedir (io/file usersdir user)))))

(defn expand-home
  "If `path` begins with a tilde (`~`), expand the tilde to the value
   of the `user.home` system property. If the `path` begins with a
   tilde immediately followed by some characters, they are assumed to
   be a username. This is expanded to the path to that user's home
   directory. This is (naively) assumed to be a directory with the same
   name as the user relative to the parent of the current value of
   `user.home`."
  [path]
  (let [path (str path)]
    (if (.startsWith path "~")
      (let [sep (.indexOf path File/separator)]
        (if (neg? sep)
          (home (subs path 1))
          (.getPath (home (subs path 1 sep)) (subs path (inc sep)))))
      path)))

;; Library functions will call this function on paths/files so that
;; we get the cwd effect on them.
(defn ^File file
  "If `path` is a period, replaces it with cwd and creates a new File object
   out of it and `paths`. Or, if the resulting File object does not constitute
   an absolute path, makes it absolutely by creating a new File object out of
   the `paths` and cwd."
  [^String path & paths]
  (when-let [path (apply
                   io/file (if (= path ".")
                             *cwd*
                             path)
                   paths)]
    (if (.isAbsolute ^File path)
      path
      (io/file *cwd* path))))

(defn ^:private coerce-path-to-string
  [path]
  (if (instance? Path path)
    (str path)
    path))

(defn as-file
  [path & paths]
  (apply io/file (coerce-path-to-string path) (map coerce-path-to-string paths)))

(defn as-path
  [path & paths]
  (.toPath (apply as-file path paths)))

(defmacro varargs
  "Make a properly-tagged Java interop varargs argument. This is basically the same as `into-array` but properly tags
   the result.

   ```clojure
   (u/varargs String)
   (u/varargs String [\"A\" \"B\"])
   ```"
  {:style/indent 1, :arglists '([klass] [klass xs])}
  [klass & [objects]]
  (vary-meta `(into-array ~klass ~objects)
             assoc :tag (format "[L%s;" (.getCanonicalName ^Class (ns-resolve *ns* klass)))))

(def ^:private ^{:arglists '([color-symb x])} colorize
  "Colorize string `x` with the function matching `color` symbol or keyword."
  (fn [color x]
    (colorize/color (keyword color) x)))

(defn format-color
  "Like `format`, but colorizes the output. `color` should be a symbol or keyword like `green`, `red`, `yellow`, `blue`,
   `cyan`, `magenta`, etc. See the entire list of avaliable colors [here](https://github.com/ibdknox/colorize/blob/master/src/colorize/core.clj).

   ```clojure
   (format-color :red \"Fatal error: %s\" error-message)
   ```"
  {:style/indent 2}
  (^String [color x]
   {:pre [((some-fn symbol? keyword?) color)]}
   (colorize color (str x)))

  (^String [color format-string & args]
   (colorize color (apply format (str format-string) args))))

(defn ->link-options
  "Converts a hash-map of options into an array of LinkOption objects.
   
   | key              | description |
   | -----------------|-------------|
   | `:nofollow-links`| Adds LinkOption/NOFOLLOW_LINKS to the array. Default: `false`|"
  ([]
   (make-array LinkOption 0))
  ([{:keys [nofollow-links]}]
   (into-array
    LinkOption
    (if nofollow-links
      [LinkOption/NOFOLLOW_LINKS]
      []))))

(defn ^String base-name
  "Return the base name (final segment/file part) of a `path`.
   If optional `trim-ext` is a string and the `path` ends with that
   string, it is trimmed. If `trim-ext` is true, any extension is trimmed."
  ([path] (.getName (file path)))
  ([path trim-ext]
   (let [base (.getName (file path))]
     (cond (string? trim-ext) (if (.endsWith base trim-ext)
                                (subs base 0 (- (count base) (count trim-ext)))
                                base)
           trim-ext (let [dot (.lastIndexOf base ".")]
                      (if (pos? dot) (subs base 0 dot) base))
           :else base))))

(defn- iterzip
  "Iterate over a zip, returns a sequence of the nodes with a nil suffix"
  [z]
  (when-not (zip/end? z)
    (cons (zip/node z) (lazy-seq (iterzip (zip/next z))))))

(defn- f-dir? [^File f]
  (when f (.isDirectory f)))

(defn- f-children [^File f]
  (.listFiles f))

(defn- iterate-dir* [path]
  (let [root (file path)
        nodes (butlast (iterzip (zip/zipper f-dir? f-children nil root)))]
    (filter f-dir? nodes)))

(defn- walk-map-fn [root]
  (let [kids (f-children root)
        dirs (set (map base-name (filter f-dir? kids)))
        files (set (map base-name (filter (complement f-dir?) kids)))]
    [root dirs files]))

(defn iterate-dir
  "Return a sequence `[root dirs files]`, starting from `path` in depth-first order"
  [path]
  (map walk-map-fn (iterate-dir* path)))

(defn walk
  "Lazily walk depth-first over the directory structure starting at
   `path` calling `func` with three arguments `[root dirs files]`.
   Returns a sequence of the results."
  [func path]
  (map #(apply func %) (iterate-dir path)))

(defn parent
  "Return the parent path."
  [path]
  (.getParentFile (file path)))

(defn parents
  "Get all the parent directories of a path."
  [f]
  (when-let [parent (parent (file f))]
    (cons parent (lazy-seq (parents parent)))))

(defn chdir
  "set!s the value of `*cwd*` to `path`. Only works inside of [[with-mutable-cwd]]"
  [path]
  (set! *cwd* (file path)))

;; ------------------------ Predictor ------------------------
(defn exists?
  "Return true if `path` exists."
  ([path]
   (exists? path {}))
  ([path {:keys [nofollow-links]}]
   (Files/exists (as-path path) (->link-options {:nofollow-links nofollow-links}))))

(defn directory?
  "Return true if `path` is a directory."
  ([path]
   (directory? path {}))
  ([path {:keys [nofollow-links]}]
   (Files/isDirectory (as-path path) (->link-options {:nofollow-links nofollow-links}))))

(defn executable?
  "Return true if `path` is a executable file."
  [path]
  (Files/isExecutable (as-path path)))

(defn hidden?
  "Return true if `path` is a hidden file."
  [path]
  (Files/isHidden (as-path path)))

(defn readable?
  "Return true if `path` is a readable file."
  [path]
  (Files/isReadable (as-path path)))

(defn writable?
  "Return true if `path` is a writable file."
  [path]
  (Files/isWritable (as-path path)))

(defn regular-file?
  "Return true if `path` is a regular file (not a soft link)."
  ([path]
   (regular-file? path {}))
  ([path {:keys [nofollow-links]}]
   (Files/isRegularFile (as-path path) (->link-options {:nofollow-links nofollow-links}))))

(def file? regular-file?)

(defn same-file?
  "Return true if `path1` is same with `path2`."
  [path1 path2]
  (Files/isSameFile (as-path path1) (as-path path2)))

(defn symlink?
  "Return true if `path` is a softlink file."
  [path]
  (Files/isSymbolicLink (as-path path)))

(defn child-of?
  "Takes two paths and checks to see if the first path is a parent
   of the second."
  [p c] (some #{(file p)} (parents c)))

(defn absolute-path?
  [^String path]
  (.isAbsolute (as-file path)))

(defn relative-path?
  [path]
  (not (absolute-path? path)))

;; -------------------- Attributes --------------------
(def ^:private number-to-permissions
  {7 "rwx"
   6 "rw-"
   5 "r-x"
   4 "r--"
   3 "-wx"
   2 "-w-"
   1 "--x"
   0 "---"})

(def ^:private permission-to-numbers
  {"READ" 4
   "WRITE" 2
   "EXECUTE" 1})

(defn ^:private octal->string-permissions
  [permissions]
  (->> (str permissions)
       (map (comp number-to-permissions
                  #(Character/digit % 10)))
       (apply str)))

(defn ^:private permissions->octal
  [permissions]
  (->> permissions
       (into #{}
             (map (comp #(string/split % #"_")
                        str)))
       (map (fn [[who perm]]
              [who (permission-to-numbers perm)]))
       (reduce (fn [m [who perm]]
                 (update m who #(+ perm %)))
               {"OWNER" 0 "GROUP" 0 "OTHERS" 0})
       (#(+ (* 100 (% "OWNER"))
            (* 10 (% "GROUP"))
            (% "OTHERS")))))

(defn ^:private permissions->string
  [permissions]
  (-> permissions
      permissions->octal
      octal->string-permissions))

(defn ->posix-file-permissions
  [permissions]
  (cond
    (string? permissions)
    (PosixFilePermissions/fromString permissions)

    (integer? permissions)
    (->posix-file-permissions (octal->string-permissions permissions))

    :else
    (throw (ex-info "Invalid permissions!" {:permissions permissions}))))

(defn ->file-attributes
  "Converts a seq of file-attributes into an array of FileAttribute objects."
  ([]
   (make-array FileAttribute 0))
  ([{:keys [posix-file-permissions]}]
   (into-array
    FileAttribute
    (if posix-file-permissions
      [(PosixFilePermissions/asFileAttribute (->posix-file-permissions posix-file-permissions))]
      []))))

(defn file-system-for
  [^String path]
  (.getFileSystem (as-path path)))

(defn supported-file-attribute-views
  [^String path]
  (.supportedFileAttributeViews (file-system-for path)))

(defn get-attribute
  ([path attribute]
   (get-attribute path attribute {}))
  ([path attribute {:keys [nofollow-links]}]
   (Files/getAttribute (as-path path)
                       attribute
                       (->link-options {:nofollow-links nofollow-links}))))

(defn set-attribute
  ([path attribute value]
   (set-attribute path attribute value {}))
  ([path attribute value {:keys [nofollow-links]}]
   (Files/setAttribute (as-path path)
                       attribute
                       value
                       (->link-options {:nofollow-links nofollow-links}))))

(defn read-attributes
  ([path attributes]
   (read-attributes path attributes {}))
  ([path attributes {:keys [nofollow-links]}]
   (Files/readAttributes (as-path path)
                         attributes
                         (->link-options {:nofollow-links nofollow-links}))))

(defn read-all-attributes
  ([path]
   (read-all-attributes path {}))
  ([path {:keys [nofollow-links]}]
   (let [path (as-path path)
         file-system (.getFileSystem path)
         views (.supportedFileAttributeViews file-system)
         link-options (->link-options {:nofollow-links nofollow-links})]
     (into {}
           (map #(do [% (Files/readAttributes path (str % ":*") link-options)]))
           views))))

(defn lookup-user!
  ([user-name]
   (lookup-user! user-name default-file-system))
  ([user-name file-system]
   (-> file-system
       .getUserPrincipalLookupService
       (.lookupPrincipalByName user-name))))

(defn lookup-user
  ([user-name]
   (lookup-user user-name default-file-system))
  ([user-name file-system]
   (try
     (lookup-user! user-name file-system)
     (catch UserPrincipalNotFoundException _ nil))))

(defn lookup-group!
  ([group-name]
   (lookup-group! group-name default-file-system))
  ([group-name file-system]
   (-> file-system
       .getUserPrincipalLookupService
       (.lookupPrincipalByGroupName group-name))))

(defn lookup-group
  ([group-name]
   (lookup-group group-name default-file-system))
  ([group-name file-system]
   (try
     (lookup-group! group-name file-system)
     (catch UserPrincipalNotFoundException _ nil))))

(defn set-owner
  ([path user-name]
   (set-owner path user-name {}))
  ([path user-name {:keys [nofollow-links]}]
   (let [file-system (file-system-for path)
         user (lookup-user! user-name file-system)]
     (set-attribute path
                    "owner:owner"
                    user
                    {:nofollow-links nofollow-links}))))

(defn set-group
  ([path group-name]
   (set-group path group-name {}))
  ([path group-name {:keys [nofollow-links]}]
   (let [path (as-path path)
         file-system (.getFileSystem path)
         group (lookup-group! group-name file-system)
         link-options (->link-options {:nofollow-links nofollow-links})]
     (Files/setAttribute path "posix:group" group link-options))))

(defn- char-to-int
  [c]
  (- (int c) 48))

(defn- chmod-octal-digit
  [f i user?]
  (if (> i 7)
    (throw (IllegalArgumentException. "Bad mode"))
    (do (.setReadable f (pos? (bit-and i 4)) user?)
        (.setWritable f (pos? (bit-and i 2)) user?)
        (.setExecutable f (pos? (bit-and i 1)) user?))))

(defn- chmod-octal
  [mode path]
  (let [[user group world] (map char-to-int mode)
        f (file path)]
    (if (not= group world)
      (throw (IllegalArgumentException.
              "Bad mode. Group permissions must be equal to world permissions"))
      (do (chmod-octal-digit f world false)
          (chmod-octal-digit f user true)
          path))))

(defn chmod
  "Change file permissions. Returns path.
   `mode` can be a permissions string in octal or symbolic format.
   Symbolic: any combination of `r` (readable) `w` (writable) and
   `x` (executable). It should be prefixed with `+` to set or `-` to
   unset. And optional prefix of `u` causes the permissions to be set
   for the owner only.

   Octal: a string of three octal digits representing user, group, and
   world permissions. The three bits of each digit signify read, write,
   and execute permissions (in order of significance). Note that group
   and world permissions must be equal.
 
   Examples:
 
   ```clojure
   (chmod \"+x\" \"/tmp/foo\") ; Sets executable for everyone
   (chmod \"u-wx\" \"/tmp/foo\") ; Unsets owner write and executable
   ```"
  [mode path]
  (if (re-matches #"^\d{3}$" mode)
    (chmod-octal mode path)
    (let [[_ u op permissions] (re-find #"^(u?)([+-])([rwx]{1,3})$" mode)]
      (when (nil? op) (throw (IllegalArgumentException. "Bad mode")))
      (let [perm-set (set permissions)
            f (file path)
            flag (= op "+")
            user (seq u)]
        (when (perm-set \r) (.setReadable f flag user))
        (when (perm-set \w) (.setWritable f flag user))
        (when (perm-set \x) (.setExecutable f flag user)))
      path)))

(defn set-posix-file-permissions
  ([path permissions]
   (set-posix-file-permissions path permissions {}))
  ([path permissions {:keys [nofollow-links]}]
   (set-attribute path
                  "posix:permissions"
                  (->posix-file-permissions permissions)
                  {:nofollow-links nofollow-links})))

(defn get-posix-file-permissions
  "Get the file permissions of a file or directory."
  ([path]
   (get-posix-file-permissions path {}))
  ([path {:keys [nofollow-links format]}]
   (let [permissions (get-attribute path "posix:permissions" {:nofollow-links nofollow-links})]
     (case format
       :octal (permissions->octal permissions)
       :string (permissions->string permissions)
       permissions))))

(defn last-modified-time
  "Get the last modified time of a file or directory."
  ([path]
   (last-modified-time path {}))
  ([path {:keys [nofollow-links]}]
   (get-attribute path "basic:lastModifiedTime" {:nofollow-links nofollow-links})))

(defn set-last-modified-time
  "Set the last modified time of a file or directory."
  ([path time]
   (set-last-modified-time path time {}))
  ([path time {:keys [nofollow-links]}]
   (set-attribute path "basic:lastModifiedTime" time {:nofollow-links nofollow-links})))

(defn last-access-time
  "Get the last access time of a file or directory."
  ([path]
   (last-access-time path {}))
  ([path {:keys [nofollow-links]}]
   (get-attribute path "basic:lastAccessTime" {:nofollow-links nofollow-links})))

(defn set-last-access-time
  "Set the last access time of a file or directory."
  ([path time]
   (set-last-access-time path time {}))
  ([path time {:keys [nofollow-links]}]
   (set-attribute path "basic:lastAccessTime" time {:nofollow-links nofollow-links})))

(defn creation-time
  "Get the creation time of a file or directory."
  ([path]
   (creation-time path {}))
  ([path {:keys [nofollow-links]}]
   (get-attribute path "basic:creationTime" {:nofollow-links nofollow-links})))

(defn set-creation-time
  "Set the creation time of a file or directory."
  ([path time]
   (set-creation-time path time {}))
  ([path time {:keys [nofollow-links]}]
   (set-attribute path "basic:creationTime" time {:nofollow-links nofollow-links})))

(defn size
  [path]
  (try
    (Files/size (as-path path))
    (catch NoSuchFileException _ nil)))

;; --------------------------- Path Utility ---------------------------
(defn absolute
  "Return absolute file."
  [path]
  (.getAbsoluteFile (file path)))

(defn normalized
  "Return normalized (canonical) file."
  [path]
  (.getCanonicalFile (file path)))

(def ^{:doc "The root of a unix system is `/`, `nil` on Windows"}
  unix-root (when (= File/separator "/") File/separator))

(defn split
  "Split `path` to components."
  [path]
  (let [pathstr (str path)
        jregx (str "\\Q" File/separator "\\E")]
    (cond (= pathstr unix-root) (list unix-root)
          (and unix-root (.startsWith pathstr unix-root))
          ;; unix absolute path
          (cons unix-root (seq (.split (subs pathstr 1) jregx)))
          :else (seq (.split pathstr jregx)))))

(defn first-path-segment
  [^String path]
  (first (map str (as-path path))))

(defn join-paths
  "Joins one or more path segments into a single String path object."
  [path & paths]
  (.getPath (apply as-file path paths)))

(defn split-path
  [path]
  (let [paths (map str (as-path path))]
    (or (seq paths)
        '(""))))

(defn last-path-segment
  [path]
  (.getName (as-file path)))

(defn filename
  [^String path]
  (when-not (.endsWith path file-separator)
    (last-path-segment path)))

(defn parent-path
  [^String path]
  (.getParent (as-file path)))

(defn parent-paths
  [path]
  (when-let [parent (parent-path path)]
    (cons parent (lazy-seq (parent-paths parent)))))

(defn without-extension
  [path]
  (if-let [filename (filename path)]
    (let [dot-index (string/last-index-of filename ".")]
      (if (and (pos-int? dot-index)
               (not= dot-index (-> filename count dec)))
        (let [chars-to-remove (- (count filename) dot-index)
              full-path-dot-index (- (count path) chars-to-remove)]
          (subs path 0 full-path-dot-index))
        path))
    path))

(defn normalize-path
  [^String path]
  (-> (as-file path)
      .toPath
      .normalize
      .toString))

(defn absolute-path
  [^String path]
  (.getAbsolutePath (as-file path)))

(defn canonical-path
  [^String path]
  (.getCanonicalPath (as-file path)))

(defn children
  ([]
   (children "."))
  ([^String path]
   (vec (.list (as-file path)))))

(defn- get-path-in-filesystem ^Path [^FileSystem filesystem ^String path-component & more-components]
  (.getPath filesystem path-component (varargs String more-components)))

(defn get-path
  "Get a `Path` for a file or directory in the default (i.e., system) filesystem named by string path component(s).
   
   ```clojure
   (get-path \"/Users/cam/tservice/tservice/plugins\")
   ;; -> #object[sun.nio.fs.UnixPath 0x4d378139 \"/Users/cam/tservice/tservice/plugins\"]
   ```"
  ^Path [& path-components]
  (apply get-path-in-filesystem (FileSystems/getDefault) path-components))

(defn- append-to-path ^Path [^Path path & components]
  (loop [^Path path path, [^String component & more] components]
    (let [path (.resolve path component)]
      (if-not (seq more)
        path
        (recur path more)))))

;; ------------------ Copy, Move, Delete, Make Directory -------------------
(defn ->copy-options
  "Converts a hash-map of options into an array of CopyOption objects.
   
   | key                | description |
   | -------------------|-------------|
   | `:replace-existing`| Adds StandardCopyOption/REPLACE_EXISTING to the array. Default: `false`
   | `:atomic-move`     | Adds StandardCopyOption/ATOMIC_MOVE to the array. Default: `false`
   | `:copy-attributes` | Adds StandardCopyOption/COPY_ATTRIBUTES to the array. Default: `false`
   | `:nofollow-links`  | Adds LinkOption/NOFOLLOW_LINKS to the array. Default: `false`"
  ([]
   (make-array CopyOption 0))
  ([{:keys [replace-existing
            atomic-move
            copy-attributes
            nofollow-links]}]
   (into-array
    CopyOption
    (cond-> []
      replace-existing (conj StandardCopyOption/REPLACE_EXISTING)
      atomic-move (conj StandardCopyOption/ATOMIC_MOVE)
      copy-attributes (conj StandardCopyOption/COPY_ATTRIBUTES)
      nofollow-links (conj LinkOption/NOFOLLOW_LINKS)))))

(defn list-dir
  "List files and directories under `path`."
  [path]
  (seq (.listFiles (file path))))

(defn copy
  ([from to]
   (copy from to {}))
  ([from to {:keys [replace-existing
                    copy-attributes
                    nofollow-links]}]
   (Files/copy (as-path from) (as-path to) (->copy-options
                                            {:replace-existing replace-existing
                                             :copy-attributes copy-attributes
                                             :nofollow-links nofollow-links}))
   to))

(defn delete
  [path]
  (Files/delete (as-path path)))

(defn delete-dir
  "Delete a directory tree."
  [root]
  (when (directory? root)
    (doseq [path (.listFiles (file root))]
      (delete-dir path)))
  (delete root))

(defn split-ext
  "Returns a vector of `[name extension]`."
  [path]
  (let [base (base-name path)
        i (.lastIndexOf base ".")]
    (if (pos? i)
      [(subs base 0 i) (subs base i)]
      [base nil])))

(defn extension
  "Return the extension part of a file."
  [path]
  (last (split-ext path)))

(defn name
  "Return the name part of a file."
  [path]
  (first (split-ext path)))

(defn mkdir
  "Create a directory."
  [path]
  (.mkdir (file path)))

(defn mkdirs
  "Make directory tree."
  [path]
  (.mkdirs (file path)))

(defn create-directory
  "Warning: Setting posix-file-permissions on create will not always
   result in the permissions you specify. This is a limitation of the
   implementation. To guarantee those permissions you should set the
   permissions as another step after creating the file."
  ([path]
   (create-directory path {}))
  ([path {:keys [posix-file-permissions]}]
   (Files/createDirectory (as-path path)
                          (->file-attributes {:posix-file-permissions posix-file-permissions}))))

(defn create-directories!
  "Warning: Setting posix-file-permissions on create will not always
   result in the permissions you specify. This is a limitation of the
   implementation. To guarantee those permissions you should set the
   permissions as another step after creating the file."
  ([path]
   (create-directories! path {}))
  ([path {:keys [posix-file-permissions]}]
   (Files/createDirectories (as-path path)
                            (->file-attributes {:posix-file-permissions posix-file-permissions}))))

(defn copy+
  "Copy `src` to `dest`, create directories if needed."
  [src dest]
  (mkdirs (parent dest))
  (copy src dest))

(defn copy-dir
  "Copy a directory from `from` to `to`. If `to` already exists, copy the directory
   to a directory with the same name as `from` within the `to` directory."
  [from to]
  (when (exists? from)
    (if (file? to)
      (throw (IllegalArgumentException. (str to " is a file")))
      (let [from (file from)
            to (if (exists? to)
                 (file to (base-name from))
                 (file to))
            trim-size (-> from str count inc)
            dest #(file to (subs (str %) trim-size))]
        (mkdirs to)
        (dorun
         (walk (fn [root dirs files]
                 (doseq [dir dirs]
                   (when-not (directory? dir)
                     (-> root (file dir) dest mkdirs)))
                 (doseq [f files]
                   (copy+ (file root f) (dest (file root f)))))
               from))
        to))))

(defn copy-dir-into
  "Copy directory into another directory if destination already exists."
  [from to]
  (if-not (exists? to)
    (copy-dir from to)
    (doseq [file (list-dir from)]
      (if (directory? file)
        (copy-dir file to)
        (copy file (io/file to (base-name file)))))))

(defn files-seq
  "Get a sequence of all files in `path`, presumably a directory or an archive of some sort (like a JAR)."
  [^Path path]
  (iterator-seq (.iterator (Files/list path))))

(defn- last-modified-timestamp ^java.time.Instant [^Path path]
  (when (exists? path)
    (.toInstant (Files/getLastModifiedTime path (varargs LinkOption)))))

(defn copy-file!
  "Copy a file from `source` -> `dest`."
  [^Path source ^Path dest]
  (when (or (not (exists? dest))
            (not= (last-modified-timestamp source) (last-modified-timestamp dest)))
    (Files/copy source dest (varargs CopyOption [StandardCopyOption/REPLACE_EXISTING
                                                 StandardCopyOption/COPY_ATTRIBUTES]))))

(defn copy-files!
  "Copy all files in `source-dir` to `dest-dir`. Overwrites existing files if last modified timestamp is not the same as
   that of the source file — see #11699 for more context."
  [^Path source-dir, ^Path dest-dir]
  (doseq [^Path source (files-seq source-dir)
          :let         [target (append-to-path dest-dir (str (.getFileName source)))]]
    (copy-file! source target)))

(defn create
  "Create a new file."
  [^File f]
  (.createNewFile f))

(defn touch
  "Set file modification time (default to now). Returns `path`."
  [path & [time]]
  (let [f (file path)]
    (when-not (create f)
      (.setLastModified f (or time (System/currentTimeMillis))))
    f))

(defn safe-copy
  ;; For fs service
  [from to opts]
  (let [options (merge {:replace-existing false} opts)]
    (if (or (:replace-existing options)
            (= false (.exists to)))
      (try
        (= nil (clojure.java.io/copy from to))
        (catch Exception e (str "exception: " (.getMessage e))))
      false)))

(defn ^:private copy-all
  [copy-options copies]
  (doseq [[from to] copies]
    (Files/copy (as-path from) (as-path to) copy-options)))

(defn ^:private copy-from-to
  [from-root to-root from]
  (let [subpath (subs from (count from-root))]
    [from (str to-root subpath)]))

(defn ^:private recursive-files-and-directories
  ([path]
   (recursive-files-and-directories path {}))
  ([path {:keys [nofollow-links]}]
   (let [paths (->> (as-file path)
                    file-seq
                    (group-by #(directory? % {:nofollow-links nofollow-links})))
         directories (sort (map str (or (paths true) [])))
         files (sort (map str (or (paths false) [])))]
     {:directories directories
      :files files})))

(defn copy-recursively
  ([from to]
   (copy-recursively from to {}))
  ([from to {:keys [replace-existing
                    copy-attributes
                    nofollow-links]}]
   (let [copy-options (->copy-options
                       {:replace-existing replace-existing
                        :copy-attributes copy-attributes
                        :nofollow-links nofollow-links})
         {:keys [files
                 directories]} (recursive-files-and-directories from
                                                                {:nofollow-links nofollow-links})
         to (if (and (directory? to)
                     (not (.endsWith to "/")))
              (join-paths to (last-path-segment from))
              to)]
     (copy-all copy-options
               (map (partial copy-from-to from to)
                    directories))
     (copy-all copy-options
               (map (partial copy-from-to from to)
                    files)))))

(defn move
  ([from to]
   (move from to {}))
  ([from to {:keys [replace-existing
                    atomic-move]}]
   (Files/move (as-path from) (as-path to) (->copy-options
                                            {:replace-existing replace-existing
                                             :atomic-move atomic-move}))))

(defn append-to-file
  [path content]
  (spit (as-file path) content :append true))

(defn create-file
  "Warning: Setting posix-file-permissions on create will not always
   result in the permissions you specify. This is a limitation of the
   implementation. To guarantee those permissions you should set the
   permissions as another step after creating the file."
  ([path]
   (create-file path {}))
  ([path {:keys [posix-file-permissions content]}]
   (let [file (Files/createFile (as-path path)
                                (->file-attributes {:posix-file-permissions posix-file-permissions}))]
     (when content
       (append-to-file path content))
     file)))

(defn create-link
  [link-path target-path]
  (Files/createLink (as-path link-path) (as-path target-path)))

(defn create-symlink
  "Not allowing posix-file-permissions for file attributes since it is not supported on MacOS
   or Linux for this operation."
  [link-path target-path]
  (Files/createSymbolicLink (as-path link-path) (as-path target-path) (->file-attributes)))

(defn delete-if-exists
  [path]
  (Files/deleteIfExists (as-path path)))

(defn delete-recursively
  ([path]
   (delete-recursively path {}))
  ([path {:keys [nofollow-links]
          :or {nofollow-links true}}]
   (let [{:keys [files directories]} (recursive-files-and-directories path
                                                                      {:nofollow-links nofollow-links})]
     (doseq [path (concat files (reverse directories))]
       (.delete (as-file path))))))

(defn rename
  "Rename `old-path` to `new-path`. Only works on files."
  [old-path new-path]
  (.renameTo (file old-path) (file new-path)))

(defn safe-rename
  ([current-path new-name]
   (safe-rename current-path new-name {}))
  ([current-path new-name {:keys [replace-existing
                                  atomic-move]}]
   (let [from-path (as-path current-path)
         to-path (.resolveSibling from-path new-name)]
     (move from-path
           to-path
           {:replace-existing replace-existing
            :atomic-move atomic-move}))))

(defn create-dir-if-not-exists!
  "Self-explanatory. Create a directory with `path` if it does not already exist."
  [^Path path]
  (when-not (exists? path)
    (Files/createDirectories path (varargs FileAttribute))))

(defn zip-files
  "Make a zip file with the from-files."
  [from-files to-filepath]
  (clj-zip/zip-files from-files to-filepath))

;; Note that make-parents is called for every file. Tested with ZIP
;; with ca 80k files. Not significantly slower than testing here for
;; .isDirectory and only then create the parents. Keeping the nicer
;; code with this comment, then.
(defn unzip-file
  [zip-file to-dir]
  (with-open [zipf (ZipFile. (io/file zip-file))]
    (doseq [entry (enumeration-seq (.getEntries zipf))
            :when (not (.isDirectory ^ZipArchiveEntry entry))
            :let  [zip-in-s (.getInputStream zipf entry)
                   out-file (io/file (str to-dir
                                          java.io.File/separator
                                          (.getName ^ZipArchiveEntry entry)))]]
      (io/make-parents out-file)
      (with-open [entry-o-s (io/output-stream out-file)]
        (io/copy zip-in-s entry-o-s)))))

;; ---------------------------- Temp File -----------------------------------
(defn tmpdir
  "The temporary file directory looked up via the `java.io.tmpdir`
   system property. Does not create a temporary directory."
  []
  (System/getProperty "java.io.tmpdir"))

(defn temp-name
  "Create a temporary file name like what is created for [[temp-file]]
   and [[temp-dir]]."
  ([prefix] (temp-name prefix ""))
  ([prefix suffix]
   (format "%s%s-%s%s" prefix (System/currentTimeMillis)
           (long (rand 0x100000000)) suffix)))

(defn- temp-create
  "Create a temporary file or dir, trying n times before giving up."
  [prefix suffix tries f]
  (let [tmp (file (tmpdir) (temp-name prefix suffix))]
    (when (pos? tries)
      (if (f tmp)
        tmp
        (recur prefix suffix (dec tries) f)))))

(defn temp-file
  "Create a temporary file. Returns nil if file could not be created
   even after n tries (default 10)."
  ([prefix]              (temp-file prefix "" 10))
  ([prefix suffix]       (temp-file prefix suffix 10))
  ([prefix suffix tries] (temp-create prefix suffix tries create)))

(defn temp-dir
  "Create a temporary directory. Returns nil if dir could not be created
   even after n tries (default 10)."
  ([prefix]              (temp-dir prefix "" 10))
  ([prefix suffix]       (temp-dir prefix suffix 10))
  ([prefix suffix tries] (temp-create prefix suffix tries mkdirs)))

(defn ephemeral-file
  "Create an ephemeral file (will be deleted on JVM exit).
   Returns nil if file could not be created even after n tries
   (default 10)."
  ([prefix]              (ephemeral-file prefix "" 10))
  ([prefix suffix]       (ephemeral-file prefix suffix 10))
  ([prefix suffix tries] (when-let [created (temp-create prefix suffix tries create)]
                           (doto created .deleteOnExit))))

(defn ephemeral-dir
  "Create an ephemeral directory (will be deleted on JVM exit).
   Returns nil if dir could not be created even after n tries
   (default 10)."
  ([prefix]              (ephemeral-dir prefix "" 10))
  ([prefix suffix]       (ephemeral-dir prefix suffix 10))
  ([prefix suffix tries] (when-let [created (temp-create prefix suffix tries mkdirs)]
                           (doto created .deleteOnExit))))

(defn create-temp-directory
  "Warning: Setting posix-file-permissions on create will not always
   result in the permissions you specify. This is a limitation of the
   implementation. To guarantee those permissions you should set the
   permissions as another step after creating the file."
  ([prefix]
   (create-temp-directory prefix {}))
  ([prefix {:keys [posix-file-permissions]}]
   (Files/createTempDirectory prefix
                              (->file-attributes {:posix-file-permissions posix-file-permissions}))))

(defn create-temp-file
  "Warning: Setting posix-file-permissions on create will not always
   result in the permissions you specify. This is a limitation of the
   implementation. To guarantee those permissions you should set the
   permissions as another step after creating the file."
  ([prefix suffix]
   (create-temp-file prefix suffix {}))
  ([prefix suffix {:keys [posix-file-permissions content]}]
   (let [path (Files/createTempFile prefix
                                    suffix
                                    (->file-attributes {:posix-file-permissions posix-file-permissions}))]
     (when content
       (append-to-file path content))
     path)))

(defn with-temp-directory*
  [f]
  (let [dir (canonical-path (Files/createTempDirectory "fs" (->file-attributes)))]
    (try
      (f dir)
      (finally
        (delete-recursively dir)))))

(defmacro with-temp-directory
  [[path-sym] & body]
  `(with-temp-directory* (fn [~path-sym] ~@body)))

(defn with-temp-file*
  [f]
  (with-temp-directory [^String dir-path]
    (let [file-path (canonical-path (Files/createTempFile (as-path dir-path) "tmp" "tmp" (->file-attributes)))]
      (f dir-path file-path))))

(defmacro with-temp-file
  [[dir-sym path-sym] & body]
  `(with-temp-file* (fn [~dir-sym ~path-sym] ~@body)))

;; ----------------------------- Glob --------------------------------
; Taken from https://github.com/jkk/clj-glob. (thanks Justin!)
(defn- glob->regex
  "Takes a glob-format string and returns a regex."
  [s]
  (loop [stream s
         re ""
         curly-depth 0]
    (let [[c j] stream]
      (cond
        (nil? c) (re-pattern
                    ; We add ^ and $ since we check only for file names
                  (str "^" (if (= \. (first s)) "" "(?=[^\\.])") re "$"))
        (= c \\) (recur (nnext stream) (str re c c) curly-depth)
        (= c \/) (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                        curly-depth)
        (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
        (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
        (= c \{) (recur (next stream) (str re \() (inc curly-depth))
        (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
        (and (= c \,) (< 0 curly-depth)) (recur (next stream) (str re \|)
                                                curly-depth)
        (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c)
                                                 curly-depth)
        :else (recur (next stream) (str re c) curly-depth)))))

(defn glob
  "Returns files matching glob pattern."
  ([pattern]
   (let [parts (split pattern)
         root (apply file (if (= (count parts) 1) ["."] (butlast parts)))]
     (glob root (last parts))))
  ([^File root pattern]
   (let [regex (glob->regex pattern)]
     (seq (.listFiles
           root
           (reify FilenameFilter
             (accept [_ _ filename]
               (boolean (re-find regex filename)))))))))

(defn ns-path
  "Takes a namespace symbol and creates a path to it. Replaces hyphens with
   underscores. Assumes the path should be relative to cwd."
  [n]
  (file
   (str (.. (str n)
            (replace \- \_)
            (replace \. \/))
        ".clj")))

(defn path-ns
  "Takes a `path` to a Clojure file and constructs a namespace symbol
   out of the path."
  [path]
  (symbol
   (.. (.replaceAll (str path) "\\.clj" "")
       (replace \_ \-)
       (replace \/ \.))))

(defn find-files*
  "Find files in `path` by `pred`."
  [path pred]
  (filter pred (-> path file file-seq)))

(defn find-files
  "Find files matching given `pattern`."
  [path pattern]
  (find-files* path #(re-matches pattern (.getName ^File %))))

;; ----------------------------- Shell --------------------------------
(defn exec
  "Execute a shell command in the current directory."
  [& body]
  (sh/with-sh-dir *cwd* (apply sh/sh body)))

(defmacro with-cwd
  "Execute `body` with a changed working directory."
  [cwd & body]
  `(binding [*cwd* (file ~cwd)]
     ~@body))

(defmacro with-mutable-cwd
  "Execute the `body` in a binding with `*cwd*` bound to `*cwd*`.
   This allows you to change `*cwd*` with `set!`."
  [& body]
  `(binding [*cwd* *cwd*]
     ~@body))

(defn which
  [bin-name]
  (let [paths (clojure.string/split (or (System/getenv "PATH") "")
                                    (re-pattern (System/getProperty "path.separator")))
        ;; for windows
        pathexts (clojure.string/split (or (System/getenv "PATHEXT") "")
                                       (re-pattern (System/getProperty "path.separator")))]
    ;; adapted work by taylorwood
    (first
     (for [path (distinct paths)
           pathext pathexts
           :let [exe-file (clojure.java.io/file path (str bin-name pathext))]
           :when (.exists exe-file)]
       (.getAbsolutePath exe-file)))))

;; -------------------------- Jar Utility ----------------------------
(defn- url-inside-jar? [^URL url]
  (when url
    (string/includes? (.getFile url) ".jar!/")))

(defn- jar-file-system-from-url ^FileSystem [^URL url]
  (FileSystems/newFileSystem (.toURI url) Collections/EMPTY_MAP))

(defn do-with-open-path-to-resource
  "Impl for `with-open-path-to-resource`."
  [^String resource, f]
  (let [url (io/resource resource)]
    (when-not url
      (throw (FileNotFoundException. "Resource does not exist.")))
    (if (url-inside-jar? url)
      (with-open [fs (jar-file-system-from-url url)]
        (f (get-path-in-filesystem fs "/" resource)))
      (f (get-path (.toString (Paths/get (.toURI url))))))))

(defmacro with-open-path-to-resource
  "Execute `body` with an Path to a resource file or directory (i.e. a file in the project `resources/` directory, or
   inside the uberjar), cleaning up when finished.
   Throws a FileNotFoundException if the resource does not exist; be sure to check with `io/resource` or similar before
   calling this.

   ```clojure
   (with-open-path-to-resouce [path \"modules\"]
       ...)
   ```"
  [[path-binding resource-filename-str] & body]
  `(do-with-open-path-to-resource
    ~resource-filename-str
    (fn [~(vary-meta path-binding assoc :tag java.nio.file.Path)]
      ~@body)))

(defn exists??
  "Does file at `path` actually exist?
  
   TODO: exists?? is different with exists? why?"
  [^Path path]
  (Files/exists path (varargs LinkOption)))

(defn file-exists-in-archive?
  "True is a file exists in an archive."
  [^Path archive-path & path-components]
  (with-open [fs (FileSystems/newFileSystem archive-path (ClassLoader/getSystemClassLoader))]
    (let [file-path (apply get-path-in-filesystem fs path-components)]
      (exists?? file-path))))

(defn slurp-file-from-archive
  "Read the entire contents of a file from a archive (such as a JAR)."
  [^Path archive-path & path-components]
  (with-open [fs (FileSystems/newFileSystem archive-path (ClassLoader/getSystemClassLoader))]
    (let [file-path (apply get-path-in-filesystem fs path-components)]
      (when (exists?? file-path)
        (with-open [is (Files/newInputStream file-path (varargs OpenOption))]
          (slurp is))))))

(defn long-str [& strings] (string/join " " strings))

(defn extract-dir-from-jar
  "Takes the string path of a jar, a dir name inside that jar and a destination
   dir, and copies the from dir to the to dir."
  [^String jar-dir from to]
  (let [jar (JarFile. jar-dir)]
    (doseq [^JarEntry file (enumeration-seq (.entries jar))]
      ;; Maybe `(.getName file)` is `from`.tar.gz
      (when (.startsWith (.getName file) (str from "/"))
        (let [f (file to (.getName file))]
          (if (.isDirectory file)
            (mkdir f)
            (do (mkdirs (parent f))
                (with-open [is (.getInputStream jar file)
                            os (io/output-stream f)]
                  (io/copy is os)))))))))

(defn extract-env-from-archive
  "Extract the entire contents of a file from a archive (such as a JAR)."
  [^Path archive-path ^String path-component ^String dest-dir]
  (with-open [fs (FileSystems/newFileSystem archive-path (ClassLoader/getSystemClassLoader))]
    (let [file-path (get-path-in-filesystem fs path-component)
          dest-path (join-paths dest-dir path-component)
          env-name (first (string/split path-component #"\."))
          env-path (join-paths dest-dir env-name)
          is-archive? (re-matches #".*\.(gz|bz2|xz|zip)$" path-component)]
      (log/info (format "Extract env archive %s to %s" file-path env-path))
      (when (exists? file-path)
        ;; TODO: Need to check? (fs/exists? env-path)
        (log/info (format-color 'yellow
                                (format (long-str "If it have any problems when the plugin %s is loading"
                                                  "you can remove the directory `%s` and retry.") env-name env-path)))
        (if is-archive?
          (with-open [is (Files/newInputStream file-path (varargs OpenOption))]
            (io/copy is (io/file dest-path))
            (c/decompress-archive dest-path dest-dir))
          (extract-dir-from-jar (.toString archive-path) path-component dest-dir))))))
