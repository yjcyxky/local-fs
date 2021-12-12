(ns local-fs.core-test
  (:require [clojure.test :refer :all]
            [local-fs.core :as core]))


(deftest local-fs
  (testing "base-name"
    (is (true? (= "test.csv" (core/base-name "data/test.csv"))))
    (is (true? (= "not-found.csv" (core/base-name "data/not-found.csv"))))
    (is (true? (= "test" (core/base-name "data/test.csv" ".csv"))))
    (is (false? (= "test" (core/base-name "data/test.csv" "csv"))))
    (is (true? (= "test" (core/base-name "data/test")))))
  
  (testing "exists? - file/directory/not-found"
    (is (true? (core/exists? "data/test.csv")))
    (is (false? (core/exists? "data/not-found.csv")))
    (is (true? (core/exists? "data")))
    (is (false? (core/exists? "not-found"))))
  
  (testing "directory? - file/directory/not-found"
    (is (false? (core/directory? "data/test.csv")))
    (is (false? (core/directory? "data/not-found.csv")))
    (is (true? (core/directory? "data")))
    (is (false? (core/directory? "not-found"))))
  
  (testing "executable? - file/directory/not-found"
    (is (false? (core/executable? "data/test.csv")))
    (is (true? (core/executable? "data/test.sh")))
    (is (true? (core/executable? "data")))
    (is (false? (core/executable? "not-found"))))
  
  (testing "size - file/directory/not-found"
    (is (>= (core/size "data/test.csv") 0))
    (is (>= (core/size "data") 0))
    (is (nil? (core/size "data/not-found")))))
