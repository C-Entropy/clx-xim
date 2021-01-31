(defpackage clx-xim
  (:use #:cl
	#:utils
	#:xlib)
  (:import-from #:uiop #:getenv)
  (:export make-clx-xim
	   clx-xim-open
	   ;;class clx-xim itself
	   display
	   logger
	   im-callback
	   ))
(in-package #:clx-xim)

(defclass-easy clx-xim ()
    (;;basic data which should always be valid
     display
     server-name
     window
     im-callback
     user-data
     ;;some global data
     ;; sequence
     byte-order
     ;;set by -clx-xim-init
     screen
     default-screen
     atoms; ;;add one for SERVER-NAME
     init
     ;;set by -clx-xim-get-servers
     server-atoms
     n-server-atoms
     ;;used by -clx-xim-check-server / -clx-xim-connect
     connect-state
     ;;-clx-xim-check-server
     trans-addr
     im-window
     ;;and also server-atom
     im-client-window
     ;;-clx-xim-connect-wait
     major-code
     minor-code
     accept-win
     ;;xim open
     open-state
     connect-id
     imattr
     icattr
     extensions
     onKeys
     offKeys
     ;;request
     current
     queue
     nExtensions
     auto-connect
     ;;Indicate whether we need a recheck on the new server.
     recheck
     yield-recheck
     ;;some ic values
     client-window
     focus-window
     logger))

(defclass-easy connect-state ()
    (state-phase
     callback
     user-data
     status))

(defclass-easy check-server ()
    (index
     subphase
     window
     requestor-window))

(defun clx-xim-make-im-name (im-name)
  "make im name using im-name, cutting down '@im=', then return the left part"
  (funcall (lambda (length)
	     (cond ((and (> (length im-name) length)
			 (string= (subseq im-name 0 length) "@im="))
		    (subseq im-name length))
		   (t
		    NIL)))
	   (length "@im=")))

(defun make-clx-xim (display window
		     &key imname)
  "create a clx-xim and return it."
  ;; (or imname
  ;;   (getenv "XMODIFIERS"))
  (make-instance 'clx-xim
		 :display display
		 :window window
		 :server-name (clx-xim-make-im-name (or imname
						       (getenv "XMODIFIERS")))
		 :connect-state (make-instance 'connect-state
					       :state-phase :xim-connect-fail)
		 :queue (make-array 5 :fill-pointer 0
				      :adjustable t)
		 :byte-order 1;;waiting
		 ))

(defun clx-xim-set-im-callback (clx-xim callbacks user-data)
  (setf (im-callback clx-xim) callbacks)
  (setf (user-data clx-xim) user-data))

(defun clx-xim-set-log-handler (clx-xim logger)
  (logger clx-xim logger))


(defun clx-xim-open (clx-xim clx-xim-open-callback auto-connect user-data)
  (funcall (lambda (connect-state)
	     (setf (callback connect-state) clx-xim-open-callback)
	     (setf (user-data connect-state) user-data))
	   (connect-state clx-xim))
  (setf (auto-connect clx-xim) auto-connect)
  (-clx-xim-open- clx-xim))

(defun -clx-im-init-atoms- (display atom-names atoms)
  (mapc #'intern-atom display atom-names)
  )

(defun -clx-xim-init- (clx-xim)
  (when (init clx-xim)
    (return-from -clx-xim-init- T))

  (unless (-clx-im-init-atoms- (display clx-xim)
			       '(XIM-SERVERS
				 XIM-LOCALES
				 XIM-TRANSPORT
				 -XIM-PROTOCOL
				 -XIM-XCONNECT)
			       (atoms clx-xim))
    (return-from -clx-xim-init- NIL))
;TODO:   (setf (screen clx-xim) (display clx-xim))
;TODO:   (setf (default-screen im) ())
  (when (or (not (default-screen clx-xim))
	    (not (screen clx-xim)))
    (return-from -clx-xim-init- NIL))
  (setf (init clx-xim) T))

(defun -clx-xim-open- (clx-xim)
  (setf (state-phase (connect-state clx-xim)) 'XIM-CONNECT-FAIL)
  (setf (open-state clx-xim) 'XIM-OPEN-INVALID)
  (unless (-clx-xim-init- clx-xim)
	  (return-from -clx-xim-open- NIL))
  (when (auto-connect clx-xim)
    (-clx-change-event-mask- (display clx-xim) ())))

;; (defun clx-xim-filter-event (clx-xim event)
;;   (setf (yield-recheck clx-xim) NIL)
;;   (let ((result (get-filter-result clx-xim event)))
;;     (when (yield-recheck clx-xim)
;;       (-clx-xim-clean-up- clx-xim))
;;     result)
;;   )

;; (defun get-filter-result (clx-xim event)
;;   (or (-clx-xim-preconnect-im- (clx-xim event))
;;       (-clx-xim-filter-event- (clx-xim event))
;;       (-clx-xim-filter-destroy-window- (clx-xim event))
;;       (-clx-xim-filter-property-changed- (clx-xim event))))

;; 					;TODO: 1. -clx-xim-filter-destroy-window-
;; 					;TODO: 2. -clx-xim-filter-event-
;; 					;TODO: 3. -clx-xim-filter-property-changed-
;; 					;TODO: 4. -clx-xim-preconnect-im-

;; (defun -clx-xim-preconnect-im- (clx-xim event)
;;   (when (funcall (lambda (test)
;; 		   (and (not (eq test XIM-CONNECT-DONE))
;; 			(not (eq test XIM-CONNECT-FAIL))))
;; 		 (connect-state-phase clx-xim))
;;     (case  (connect-state-phase clx-xim)
;;       (XIM-CONNECT-CHECK-SERVER
;;        (when (eq (connect-state-check-server-subphase clx-xim) (n-server-atoms clx-xim))
;; 	 (setf (connect-state-phase clx-xim) XIM-CONNECT-FAIL)
;; 	 (-clx-xim-preconnect-im- clx-xim event)
;; 	 (return-from -clx-xim-preconnect-im-))
;;        (cond ((eq (connect-state-check-server-subphase clx-xim) XIM-CONNECT-CHECK-SERVER-PREPARE)
;; 	      (cond ((-clx-xim-check-server-prepare clx-xim)
;; 		     (setf (connect-state-check-server-subphase im) XIM-CONNECT-CHECK-SERVER-LOCALE))
;; 		    (t
;; 		     (next-server-chek clx-xim)
;; 		     (-clx-xim-preconnect-im- (clx-xim event))
;; 		     (return-from -clx-xim-preconnect-im-))))
;; 	     (case (connect-state-check-server-subphase clx-xim)
;; 	       (XIM-CONNECT-CHECK-SERVER-LOCALE
;; 		())
;; 	       (XIM-CONNECT-CHECK-SERVER-TRANSPORT
;; 		())
;; 	       (XIM-CONNECT-CHECK-SERVER-TRANSPORT-WAIT
;; 		()))))
;;       (XIM-CONNECT-CONNECT
;;        (case (connect-state-connect-subphase clx-xim)
;; 	 (XIM-CONNECT-CONNECT-PREPARE
;; 	  (-clx-xim-connect-prepare clx-xim)
;; 	  (setf (connect-state-connect-subphase clx-xim) XIM-CONNECT-CONNECT-WAIT))
;; 	 (XIM-CONNECT-CONNECT-WAIT
;; 	  ())
;; 	 (XIM-CONNECT-CONNECT-WAIT-REPLY
;; 	  (case (-clx-xim-connect-wait-reply clx-xim event)
;; 	    (ACTION-YIELD
;; 	     (return-from -clx-xim-preconnect-im-))
;; 	    (ACTION-ACCEPT
;; 	     (setf event NIL)
;; 	     (setf (connect-state-phase clx-xim) XIM-CONNECT-DONE)
;; 	     (return-from -clx-xim-preconnect-im-))
;; 	    (ACTION-FAILED
;; 	     (setf event NIL)
;; 	     (setf (connect-state-phase clx-xim) XIM-CONNECT-FAIL)
;; 	     (return-from -clx-xim-preconnect-im-)))))))
;;     (-clx-xim-preconnect-im- clx-xim event))
;;   (eq event NIL))

;; (defun -clx-xim-filter-event- (clx-xim event)
;;   (unless (eq (connect-state-phase) XIM-CONNECT-DONE)
;;     (return-from -clx-xim-filter-event- NIL))
;;   (when )
;;   )
;; (defun -clx-xim-filter-destroy-window- (clx-xim event)
;;   (cond ((not (eq (open-state clx-xim) XIM-OPEN-DONE))
;; 	 (return-from -clx-xim-filter-destroy-window- NIL))
;; 	(not (eq ( event) )
;; 	     )
;; 	))

;; (defun -clx-xim-filter-property-changed- (clx-xim event))
