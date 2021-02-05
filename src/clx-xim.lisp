(defpackage clx-xim
  (:use #:cl
	#:utils
	#:xlib)
  (:import-from #:uiop #:getenv)
  (:export #:make-clx-xim
	   #:clx-xim-open
	   #:clx-xim-set-im-callback
	   #:clx-xim-set-log-handler
	   ;;class clx-xim itself
	   ;; #:display
	   ;; #:logger
	   #:im-callback))
(in-package #:clx-xim)


(defparameter *xim-server-category* "@server=")
(defparameter *xim-locale-category* "@locale=")
(defparameter *xim-transport-category* "@transport=")


(defclass-easy clx-xim ()
    (;;basic data which should always be valid
     display
     server-name
     window
     im-callback
     user-data
     root-window
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
     connect-subphase
     (check-server :initform (make-instance 'check-server)
		   :accessor check-server)))

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

(defun make-clx-xim (display screen
		     &key imname)
  "create a clx-xim and return it."
  ;; (or imname
  ;;   (getenv "XMODIFIERS"))
  (make-instance 'clx-xim
		 :display display
		 :default-screen screen
		 :root-window (screen-root screen)
		 :server-name (clx-xim-make-im-name (or imname
						       (getenv "XMODIFIERS")))
		 :connect-state (make-instance 'connect-state
					       :state-phase :xim-connect-fail)
		 :init NIL
		 :queue (make-array 5 :fill-pointer 0
				      :adjustable t)
		 :byte-order 1;;waiting
		 ))

(defun clx-xim-set-im-callback (clx-xim callbacks user-data)
  (setf (im-callback clx-xim) callbacks)
  (setf (user-data clx-xim) user-data))

(defun clx-xim-set-log-handler (clx-xim logger)
  (setf (logger clx-xim) logger))

(defun -clx-im-init-atoms- (display atom-names)
  (mapc (lambda (atom-name)
	  (intern-atom display atom-name))
	atom-names))

(defun -clx-xim-init- (clx-xim)
  (when (init clx-xim)
    (return-from -clx-xim-init- T))

  (setf (atoms clx-xim)
	(mapcar #'list
		(-clx-im-init-atoms- (display clx-xim)
				     '(:xim_servers
				       :locales
				       :transport
				       :_xim_protocol
				       :_xim_xconnect))))
  ;; (print (list-properties (root-window clx-xim)))
  (setf (screen clx-xim) (display-default-screen (display clx-xim)))
  (setf (window clx-xim) (create-window :parent (screen-root (screen clx-xim))
					:x 10 :y 10
					:width 10 :height 10))
  ;; (when (not (screen clx-xim))
  ;;   ;; (or (not (screen clx-xim))
  ;;   ;;     (not (default-screen clx-xim)))
  ;;   (return-from -clx-xim-init- NIL))
  (setf (init clx-xim) T))


(defun -clx-change-event-mask- (window mask-key remove)
  (let ((event-mask-keys (make-event-keys (window-event-mask window))))
    (when remove
      (when (find mask-key event-mask-keys)
	(setf (window-event-mask window) (remove mask-key event-mask-keys)))
      (return-from -clx-change-event-mask-))
    (unless (find mask-key event-mask-keys);;when remove is NIL, we do add
      (setf (window-event-mask window) (cons mask-key event-mask-keys)))))

(defun -clx-xim-get-servers- (clx-xim)
  "get XIM_SERVERS from root-window, and trans them into atom-name"

  (get-property (root-window clx-xim)
		:xim_servers
		:transform #'(lambda (atom-id)
			       (atom-name (display clx-xim) atom-id))
		))

(defun -clx-xim-check-server-name- (clx-xim)
  "make sure server-name from env is string= to IM server get from Window"
  (string= (concatenate 'string *xim-server-category* (server-name clx-xim))
	   (symbol-name (nth
			 (index (check-server (connect-state clx-xim)))
			 (server-atoms clx-xim)))))

(defun -check-next-server- (clx-xim)
  ((lambda (check-server)
     (setf (subphase check-server) :xim-connect-check-server-prepare
	   (index check-server) (1+ (index check-server)))
     (when (requestor-window check-server)
       (destroy-window (requestor-window check-server))))
   (check-server (connect-state clx-xim))))

(defun -clx-xim-check-server-prepare- (clx-xim)
  (setf (window (check-server (connect-state clx-xim)))
	(selection-owner (display clx-xim)
			 (nth
			  (index (check-server (connect-state clx-xim)))
			  (server-atoms clx-xim))))
  (format t "AAA")
  (unless (-clx-xim-check-server-name- clx-xim)
    (return-from -clx-xim-check-server-prepare- NIL))

  ;; (print (create-window :parent (root-window clx-xim)
  ;; 		       :depth 0;;use the depth of parent
  ;; 		       :x 0 :y 0
  ;; 		       :width 1 :height 1
  ;; 		       :class :input-output
  ;; 		       :visual (screen-root-visual (default-screen clx-xim))))

  ;; (print (root-window clx-xim))
  ;; (print (screen-root-visual (default-screen clx-xim)))

  (setf (requestor-window (check-server (connect-state clx-xim)))
	(create-window :parent (root-window clx-xim)
		       :depth 0;;use the depth of parent
		       :x 0 :y 0
		       :width 1 :height 1
		       :class :input-output
		       :visual (screen-root-visual (default-screen clx-xim)))))


(defun -clx-xim-check-server-transport- (clx-xim)
  ;; (print (find-atom (display clx-xim) :transport))
  ;; (print (list-properties (requestor-window (check-server (connect-state clx-xim)))))
  (convert-selection (nth
		      (index (check-server (connect-state clx-xim)))
		      (server-atoms clx-xim))
		     :transport
		     (requestor-window (check-server (connect-state clx-xim)))
		     :property (find-atom (display clx-xim) :transport))
    ;; (print (list-properties (requestor-window (check-server (connect-state clx-xim)))))
  )

(defun -clx-xim-check-transport- (address)
  (when (and (> (length address) (length *xim-transport-category*))
	     (search *xim-transport-category* address)
	     (search "X/" address))
    "X/");;for now we just consdier "X/"

  ;;take values apart <<<<
  ;; (when (and (> (length address) (length *xim-transport-category*))
  ;; 	     (search *xim-transport-category* address))
  ;;   (let ((=-pos (position #\= address)))
  ;;     (when =-pos
  ;; 	(uiop:split-string (subseq address (1+ =-pos)) :separator ","))))
  ;;take values apart <<<<
  )
(defun -clx-xim-check-server-tranport-wait- (clx-xim)
  ;; (print (list-properties (requestor-window (check-server (connect-state clx-xim)))))
  (let ((display (display clx-xim)))
    (event-case (display)
      ((:selection-notify) (window selection target property time)
       ;; (print ":selection-notify")
       (unless (eq window
		   (requestor-window (check-server (connect-state clx-xim))))
	 (return-from -clx-xim-check-server-tranport-wait- :action-yield))

       (unless property
	 (return-from -clx-xim-check-server-tranport-wait- :action-failed))
       ;; (print (list-properties (requestor-window (check-server (connect-state clx-xim)))))
       (let ((address (get-property window
				    :transport
				    :delete-p T
				    :transform #'code-char
				    :result-type 'string
				    )))
	 ;; (format t "add ~A~%" address)
	 (unless address
	   (return-from -clx-xim-check-server-tranport-wait- :action-failed))
	 (unless (setf (trans-addr clx-xim) (-clx-xim-check-transport- address))
	   (return-from -clx-xim-check-server-tranport-wait- :action-failed))
	 ;; (print window)
	 (destroy-window window)
	 (setf (requestor-window (check-server (connect-state clx-xim))) NIL
	       (im-window clx-xim) (window (check-server (connect-state clx-xim))))
	 (push (cons :server-name
		     (nth
		      (index (check-server (connect-state clx-xim)))
		      (server-atoms clx-xim)))
	       (atoms clx-xim))
	 ;; (print (atoms clx-xim))
	 (return-from -clx-xim-check-server-tranport-wait- :action-accept))))))

(defun -clx-xim-preconnect-im- (clx-xim event)
  ;; (print "-clx-xim-preconnect-im-")
  ;; (print (state-phase (connect-state clx-xim)))
  ;; (print (subphase (check-server (connect-state clx-xim))))
  (block block-check
    (case (state-phase (connect-state clx-xim))
      (:xim-connect-check-server
       (when (eq (length (server-atoms clx-xim))
		 (index (check-server (connect-state clx-xim))))
	 (setf (state-phase (connect-state clx-xim)) :xim-connect-fail)
	 (return-from block-check))
       (case (subphase (check-server (connect-state clx-xim)))
	 (:xim-connect-check-server-prepare
	  (if (-clx-xim-check-server-prepare- clx-xim)
	      (setf (subphase (check-server (connect-state clx-xim)))
		    :xim-connect-check-server-locale)
	      (progn (-check-next-server- clx-xim)
		     (return-from block-check))))
	 (:xim-connect-check-server-locale ;TODO:
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim-connect-check-server-transport))
	 (:xim-connect-check-server-transport
	  (-clx-xim-check-server-transport- clx-xim)
	  ;; (print ":xim-connect-check-server-transport")
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim-connect-check-server-transport-wait))
	 (:xim-connect-check-server-transport-wait

	  (case (-clx-xim-check-server-tranport-wait- clx-xim)
	    (:action-accept
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-connect
		   (connect-subphase (connect-state clx-xim)) :xim-connect-connect-prepare))
	    (:action-failed
	     (-check-next-server- clx-xim))
	    (:action-yield
	     (return-from -clx-xim-preconnect-im- NIL)))
	  (return-from block-check))))
      ;; (:xim-connect-connect
      ;; ;;  (case (subphase (connect (connect-state clx-xim)))
      ;; ;; 	 (:xim-connect-connect_prepare
      ;; ;; 	  (-clx-xim-connect-prepare- clx-xim)
      ;; ;; 	  (setf (subphase (connect (connect-state clx-xim))) :xim-connect-connect_wait))
      ;; ;; 	 (:xim-connect-connect_wait
      ;; ;; 	  (case (-clx-xim-connect-connect-wait clx-xim event)
      ;; ;; 	    (:accept_accept
      ;; ;; 	     (setf event NIL
      ;; ;; 		   (subphase (connect (connect-state clx-xim))) :xim-connect-wait_reply)
      ;; ;; 	     (return-from -clx-xim-preconnect-im-))
      ;; ;; 	    (:accept_failed
      ;; ;; 	     (setf event NIL
      ;; ;; 		   (state-phase (connect-state clx-xim)) :xim-connect-fail)
      ;; ;; 	     (return-from -clx-xim-preconnect-im-)
      ;; ;; 	     (:accept_yield
      ;; ;; 	      (return-from -clx-xim-preconnect-im-))))
      ;; ;; 	  (:xim-connect-connect_wait_reply
      ;; ;; 	   (case (-clx-xim-connect-wait-reply clx-xim event)
      ;; ;; 	     (:action_accept
      ;; ;; 	      (setf event NIL
      ;; ;; 		    (state-phase (connect-state clx-xim)) :xim-connect-done)
      ;; ;; 	      (return-from -clx-xim-preconnect-im-))
      ;; ;; 	     (:action_failed
      ;; ;; 	      (setf event NIL
      ;; ;; 		    (state-phase (connect-state clx-xim)) :xim-connect-fail)
      ;; ;; 	      (return-from -clx-xim-preconnect-im-)
      ;; ;; 	      (:action_yield
      ;; ;; 	       (return-from -clx-xim-preconnect-im-)))))

      ;; ;; 	  (otherwise (return-from -clx-xim-preconnect-im-)))))
      (otherwise (return-from -clx-xim-preconnect-im-))))
  (-clx-xim-preconnect-im- clx-xim event)
  )

(defun -clx-xim-open- (clx-xim)
  (setf (open-state clx-xim) :xim-open-invalid
	(state-phase (connect-state clx-xim)) :xim-connect-fail)

  (unless (-clx-xim-init- clx-xim)
    (return-from -clx-xim-open- NIL))

  (when (auto-connect clx-xim)
    (-clx-change-event-mask- (root-window clx-xim) :property-change NIL))

  (unless (setf (server-atoms clx-xim) (-clx-xim-get-servers- clx-xim))
    (return-from -clx-xim-open- NIL))

  (setf (state-phase (connect-state clx-xim)) :xim-connect-check-server)
  ((lambda (check-server)
     (setf (index check-server) 0)
     (setf (requestor-window check-server) 0)
     (setf (window check-server) 0)
     (setf (subphase check-server) :xim-connect-check-server-prepare))
   (check-server (connect-state clx-xim)))
  (-clx-xim-preconnect-im- clx-xim NIL))


(defun clx-xim-open (clx-xim clx-xim-open-callback auto-connect user-data)
  (funcall (lambda (connect-state)
	     (setf (callback connect-state) clx-xim-open-callback
		   (user-data connect-state) user-data))
	   (connect-state clx-xim))
  (setf (auto-connect clx-xim) auto-connect)
  (-clx-xim-open- clx-xim))


;; (defun clx-xim-process (clx-xim)
;;   (let (())
;;     (event-case ())))
