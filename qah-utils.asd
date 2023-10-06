(defsystem "qah-utils"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:do-urlencode
               :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "qah-utils/tests"))))

(defsystem "qah-utils/tests"
  :author ""
  :license ""
  :depends-on ("qah-utils"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for qah-utils"
  :perform (test-op (op c) (symbol-call :rove :run c)))
