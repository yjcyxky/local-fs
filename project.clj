(defproject com.github.yjcyxky/local-fs "0.1.5"
  :description "File system utilities in clojure."
  :url "https://github.com/yjcyxky/local-fs.git"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [clj-file-zip "0.1.0"]
                 [org.clojure/tools.logging "1.1.0"]
                 [middlesphere/clj-compress "0.1.0" :exclusions [org.apache.commons/commons-compress]]
                 [org.apache.commons/commons-compress "1.8"]
                 [colorize "0.1.1" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-cloverage "1.0.13"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]
            [lein-changelog "0.3.2"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.0"]]}
             :test {:jvm-opts ["-Duser.home=/home/test"]}}
  :deploy-repositories [["releases" :clojars]]
  :aliases {"update-readme-version" ["shell" "sed" "-i" "" "s/\\\\[com\\.github\\.yjcyxky\\\\/local-fs \"[0-9.]*\"\\\\]/[com\\.github\\.yjcyxky\\\\/local-fs \"${:version}\"]/" "README.md"]
            "coverage" ["shell" "lein" "with-profile" "test" "cloverage" "--coveralls"]}
  :release-tasks [["shell" "git" "diff" "--exit-code"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["changelog" "release"]
                  ["update-readme-version"]
                  ["coverage"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy"]
                  ["vcs" "push"]])
