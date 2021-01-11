(defsystem "clx-im"
  :version "0.1.0"
  :author "I-Entropy"
  :license ""
  :depends-on ("clx")
  :components ((:module "src"
                :components
                ((:file "clx-im"))))
  :description "input method for clx"
  :in-order-to ((test-op (test-op "clx-im/tests"))))

(defsystem "clx-im/tests"
  :author "I-Entropy"
  :license ""
  :depends-on ("clx-im"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "clx-im"))))
  :description "Test system for clx-im"
  :perform (test-op (op c) (symbol-call :rove :run c)))
