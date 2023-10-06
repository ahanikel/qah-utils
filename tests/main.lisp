(defpackage utils/tests/main
  (:use :cl
        :utils
        :rove))
(in-package :utils/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :utils)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
