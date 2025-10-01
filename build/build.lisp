(defpackage qah-utils/build
  (:use #:cl
        #:qah-utils
        #:cl-api)
  (:export :build))

(in-package :qah-utils/build)

(defun build ()
  (build-docs))

(defun build-docs ()
  (let ((path (merge-pathnames
               #p"docs/api/index"
               (asdf:system-source-directory :qah-utils))))
    (ensure-directories-exist path)
    (cl-api:api-gen :qah-utils path)))
