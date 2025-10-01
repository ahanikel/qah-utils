(defpackage #:qah-utils/tests
  (:use :cl :rove)
  (:import-from :qah-utils :make-object :key :set-key :select :->.))

(in-package :qah-utils/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :qah-utils)' in your Lisp.

(let* ((obj1 (make-object))
       (obj2 (make-object))
       (obj3 (make-object))
       (test-list (list obj1 obj2 obj3)))
  (set-key obj1 "name"  "obj1")
  (set-key obj1 "value" "obj1")
  (set-key obj2 "name"  "obj2")
  (set-key obj2 "value" "obj2")
  (set-key obj3 "name"  "obj3")
  (set-key obj3 "value" "gotcha")

  (deftest test-select
    (testing "finds the right positive"
             (let ((result (->. test-list
                             (select
                               (key "name")
                               (string= "obj3")))))
               (ok (and
                    (= 1 (length result))
                    (string= (key (car result) "value") "gotcha")))))

    (testing "does not find false positives"
             (let ((result (->. test-list
                             (select
                               (key "name")
                               (string= "obj4")))))
               (ok (null result))))))
