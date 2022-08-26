(ns local-fs.core-test
  (:require [clojure.test :refer :all]
            [local-fs.core :as core])
  (:import (java.nio.file Path)
           (java.io File)))

(deftest local-fs
    ;; Need to set a system property "user.home" in test profile firstly.
  (testing "test-home"
    (is (true? (= "/home/test1" (str (core/home "test1")))))
    (is (true? (= "/home/test" (str (core/home)))))
    (is (false? (instance? String (core/home)))))

  (testing "test-as-path"
    (is (true? (instance? Path (core/as-path "/home/test/test.txt")))))

  (testing "test-coerce-path-to-string"
    (is (true? (= "/home/test/test.txt" (core/coerce-path-to-string (core/as-path "/home/test/" "test.txt")))))
    (is (true? (= "/home/test/test.txt" (core/coerce-path-to-string "/home/test/test.txt")))))

  (testing "test-expand-home"
    (is (true? (= "/home/test/test.txt" (str (core/expand-home "~/test.txt")))))
    (is (true? (= "/home/test/test.txt" (str (core/expand-home "/home/test/test.txt")))))
    (is (true? (= "test.txt" (str (core/expand-home "test.txt")))))
    (is (true? (= "/home/test" (str (core/expand-home "~")))))
    (is (true? (instance? Path (core/expand-home "~/test.txt")))))

  (testing "test-file"
    (is (true? (= "/home/test/test.txt" (str (core/file "/home/test/test.txt")))))
    (is (true? (= (core/join-paths core/*cwd* "test/test.txt") (str (core/file "." "test/test.txt")))))
    (is (true? (instance? File (core/file "test.txt")))))

  (testing "test-dirname"
    (is (true? (= (core/join-paths core/*cwd* "data") (core/dirname "data/test.csv"))))
    (is (true? (= (core/join-paths core/*cwd* "data") (core/dirname "data/not-found.csv"))))
    (is (true? (= "/home/test" (core/dirname "/home/test/data/")))))

  (testing "test-basename"
    (is (true? (= "test.csv" (core/basename "data/test.csv"))))
    (is (true? (= "not-found.csv" (core/basename "data/not-found.csv"))))
    (is (true? (= "test" (core/basename "data/test.csv" ".csv"))))
    (is (true? (= "test.csv" (core/basename "data/test.csv" ".txt"))))
    (is (true? (= "test" (core/basename "data/test.csv" true))))
    (is (true? (= "test." (core/basename "data/test..csv" true))))
    (is (true? (= "test" (core/basename "data/test" true))))
    (is (true? (= "test.csv" (core/basename "data/test.csv.gz" true))))
    (is (true? (= "test.csv.gz" (core/basename "data/test.csv.gz" nil))))
    (is (false? (= "test" (core/basename "data/test.csv" "csv"))))
    (is (true? (= "test" (core/basename "data/test")))))

  (testing "test-dirnames"
    (is (true? (= 2 (count (core/dirnames "/home/test/data")))))
    (is (true? (= ["/home/test" "/home"] (map str (core/dirnames "/home/test/data"))))))

  (testing "test-split-ext"
    (is (true? (= ["data" ".txt"] (core/split-ext "/home/test/data.txt"))))
    (is (true? (= ["data.txt" ".gz"] (core/split-ext "/home/test/data.txt.gz"))))
    (is (true? (= ["data" nil] (core/split-ext "/home/test/data"))))
    (is (true? (= ["data" nil] (core/split-ext "data")))))

  (testing "test-extension"
    (is (true? (= ".txt" (core/extension "/home/test/data.txt"))))
    (is (true? (= ".gz" (core/extension "/home/test/data.txt.gz"))))
    (is (true? (= nil (core/extension "/home/test/data"))))
    (is (true? (= nil (core/extension "data")))))

  (testing "test-split-path"
    (is (true? (= ["home" "test" "data.txt"] (core/split-path "/home/test/data.txt"))))
    (is (true? (= ["home" "test" "data.txt.gz"] (core/split-path "/home/test/data.txt.gz"))))
    (is (true? (= ["home" "test" "data"] (core/split-path "/home/test/data"))))
    (is (true? (= ["data"] (core/split-path "data"))))
    (is (true? (= [""] (core/split-path "/")))))

  (testing "test-exists? - file/directory/not-found"
    (is (true? (core/exists? "data/test.csv")))
    (is (false? (core/exists? "data/not-found.csv")))
    (is (true? (core/exists? "data")))
    (is (false? (core/exists? "not-found"))))

  (testing "test-directory? - file/directory/not-found"
    (is (false? (core/directory? "data/test.csv")))
    (is (false? (core/directory? "data/not-found.csv")))
    (is (true? (core/directory? "data")))
    (is (false? (core/directory? "not-found"))))

  (testing "test-executable? - file/directory/not-found"
    (is (false? (core/executable? "data/test.csv")))
    (is (true? (core/executable? "data/test.sh")))
    (is (true? (core/executable? "data")))
    (is (false? (core/executable? "not-found"))))

  (testing "test-size - file/directory/not-found"
    (is (>= (core/size "data/test.csv") 0))
    (is (>= (core/size "data") 0))
    (is (nil? (core/size "data/not-found")))))
