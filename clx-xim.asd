(defsystem "clx-xim"
  :version "0.1.0"
  :author "I-Entropy"
  :license ""
  :depends-on ("clx" "flexi-streams")
  :components ((:file "packages")
	       (:module "src" :depends-on ("packages")
                :components
                ((:file "ximproth" ;; :depends-on ("clx-xim")
			)
		 (:file "utils")
		 (:file "ximproth-packet" :depends-on ("utils" "ximproth"))
		 (:file "protrocol-handler" :depends-on ("ximproth" "utils"))
		 (:file "clx-xim" :depends-on ("utils" "ximproth" "protrocol-handler")))))
  :description "input method for clx"
  :in-order-to ((test-op (test-op "clx-xim/tests"))))


(defsystem "clx-xim/demo"
  :version "0.1.0"
  :author "I-Entropy"
  :license ""
  :depends-on ("clx-xim")
  :components ((:module "demo"
                :components
                ((:file "clx-xim-demo"))))
  :description "demo for clx-xim")

(defsystem "clx-xim/tests"
  :author "I-Entropy"
  :license ""
  :depends-on ("clx-xim"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "clx-xim"))))
  :description "Test system for clx-xim"
  :perform (test-op (op c) (symbol-call :rove :run c)))
