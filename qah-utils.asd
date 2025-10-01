(defsystem "qah-utils"
  :version "0.1.0"
  :author "Axel Hanikel <ahanikel@gmail.com>"
  :license "BSD"
  :depends-on (:do-urlencode
               :cl-ppcre
               :flexi-streams
               :bordeaux-threads
               :vectors)
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "messaging"))))
  :description ""
  :in-order-to ((test-op (test-op "qah-utils/tests"))
                (build-op (build-op "qah-utils/build"))))

(defsystem "qah-utils/build"
  :depends-on (:qah-utils
               :cl-api)
  :components ((:module "build"
                :components
                ((:file "build"))))
  :perform (build-op (op c) (symbol-call :qah-utils/build :build)))

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
