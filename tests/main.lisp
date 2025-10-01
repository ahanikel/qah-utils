(defpackage qah-utils/tests
  (:use :cl
        :qah-utils
        :rove))
(in-package :qah-utils/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :qah-utils)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
