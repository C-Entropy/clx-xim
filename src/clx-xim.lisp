(in-package #:clx-xim)

(defparameter *xim-server-category* "@server=")
(defparameter *xim-locale-category* "@locale=")
(defparameter *xim-transport-category* "@transport=")
(defparameter *clx-xim-cm-data-size* 20)
(defparameter *clx-xim-header-size* 4
  "doc")

(define-class-easy clx-xim ()
  (;;basic data which should always be valid
   display
   server-name
   window
   im-callback
   user-data
   root-window
   ;;some global data
   xim-sequence
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
   ;;-clx-xim-connect-wait-
   major-code
   minor-code
   accept-win
   ;;xim open
   open-state
   connect-id
   (imattr :initform (make-hash-table :test #'equal) :accessor imattr)
   (icattr :initform (make-hash-table :test #'equal) :accessor icattr)
   extensions
   onkeys
   offkeys
   ;;request
   current
   queue
   auto-connect
   ;;Indicate whether we need a recheck on the new server.
   recheck
   yield-recheck
   ;;some ic values
   client-window
   focus-window
   logger))

(define-class-easy connect-state ()
  (state-phase
   callback
   user-data
   connect-subphase
   (check-server :initform (make-instance 'check-server)
		 :accessor check-server)))

(define-class-easy check-server ()
  (index
   subphase
   window
   requestor-window))

(define-class-easy clx-xim-request ()
  (major-code
   minor-code
   user-data
   frame
   callback))

(defun clx-xim-make-im-name (im-name)
  "make im name using im-name, cutting down '@im=', then return the left part"
  ((lambda (length)
     (cond ((and (> (length im-name) length)
		 (string= (subseq im-name 0 length) "@im="))
	    (subseq im-name length))
	   (t
	    NIL)))
   (length "@im=")))

(defun make-clx-xim (display screen
		     &key imname)
  "create a clx-xim and return it."
  (make-instance 'clx-xim
		 :display display
		 :default-screen screen
		 :root-window (screen-root screen)
		 :server-name (clx-xim-make-im-name (or imname
							(getenv "XMODIFIERS")))
		 :connect-state (make-instance 'connect-state
					       :state-phase :xim-connect-fail)
		 :init NIL
		 :connect-id 0
		 :xim-sequence 0
		 :byte-order (if (= 1 (ldb (byte 8 0) 1))
				 (char-code #\l)
				 (char-code #\B))))

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
  (setf (screen clx-xim) (display-default-screen (display clx-xim)))
  (setf (window clx-xim) (create-window :parent (screen-root (screen clx-xim))
					:x 10 :y 10
					:width 10 :height 10))
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
			       (atom-name (display clx-xim) atom-id))))

(defun -clx-xim-check-server-name- (clx-xim)
  "make sure server-name from env is string= to IM server get from Window"
  (string= (concatenate 'string *xim-server-category* (server-name clx-xim))
	   (symbol-name (nth
			 (index (check-server (connect-state clx-xim)))
			 (server-atoms clx-xim)))))

(defun -check-next-server- (clx-xim)
  ((lambda (check-server)
     (setf (subphase check-server) :xim-connect-check-server-prepare
	   (index check-server) (1+ (index check-server))
	   (requestor-window check-server) NIL))
   (check-server (connect-state clx-xim))))

(defun -clx-xim-check-server-prepare- (clx-xim)
  (setf (window (check-server (connect-state clx-xim)))
	(selection-owner (display clx-xim)
			 (nth
			  (index (check-server (connect-state clx-xim)))
			  (server-atoms clx-xim))))
  (unless (-clx-xim-check-server-name- clx-xim)
    (return-from -clx-xim-check-server-prepare- NIL))

  (setf (requestor-window (check-server (connect-state clx-xim)))
	(create-window :parent (root-window clx-xim)
		       :depth 0;;use the depth of parent
		       :x 0 :y 0
		       :width 1 :height 1
		       :class :input-output
		       :visual (screen-root-visual (default-screen clx-xim)))))


(defun -clx-xim-check-server-transport- (clx-xim)
  (convert-selection (nth
		      (index (check-server (connect-state clx-xim)))
		      (server-atoms clx-xim))
		     :transport
		     (requestor-window (check-server (connect-state clx-xim)))
		     :property (find-atom (display clx-xim) :transport)))

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


(defun -clx-xim-check-server-transport-wait- (clx-xim a b c d e f)
  (let ((display (display clx-xim)))
    (event-case (display)
      ((:selection-notify) (window selection target property time)
       (format t "~A ~A ~A ~A ~A" window selection target property time)
       (unless (eq window
		   (requestor-window (check-server (connect-state clx-xim))))
	 (return-from -clx-xim-check-server-transport-wait- :action-yield))

       (unless property
	 (return-from -clx-xim-check-server-transport-wait- :action-failed))
       (let ((address (get-property window
				    :transport
				    :delete-p T
				    :transform #'code-char
				    :result-type 'string)))
	 (unless address
	   (return-from -clx-xim-check-server-transport-wait- :action-failed))
	 (unless (setf (trans-addr clx-xim) (-clx-xim-check-transport- address))
	   (return-from -clx-xim-check-server-transport-wait- :action-failed))
	 (destroy-window window)
	 (setf (requestor-window (check-server (connect-state clx-xim))) NIL
	       (im-window clx-xim) (window (check-server (connect-state clx-xim))))
	 (push (cons :server-name
		     (nth
		      (index (check-server (connect-state clx-xim)))
		      (server-atoms clx-xim)))
	       (atoms clx-xim))
	 (return-from -clx-xim-check-server-transport-wait- :action-accept))))))



;; (defun -clx-xim-check-server-transport-wait- (clx-xim type window selection target property time)
;;   (print "-clx-xim-check-server-transport-wait--------")
;;   (unless (and (eq window
;; 		   (requestor-window (check-server (connect-state clx-xim))))
;; 	       (eq type :selection-notify))
;;     (return-from -clx-xim-check-server-transport-wait- :action-yield))
;;   (unless property
;;     (print ":action-failed")
;;     (return-from -clx-xim-check-server-transport-wait- :action-failed))
;;   (let ((address (get-property window
;; 			       :transport
;; 			       :delete-p T
;; 			       :transform #'code-char
;; 			       :result-type 'string)))
;;     (unless (and address
;; 		 (setf (trans-addr clx-xim) (-clx-xim-check-transport- address)))
;;       (return-from -clx-xim-check-server-transport-wait- :action-failed))
;;     (destroy-window window)
;;     (setf (requestor-window (check-server (connect-state clx-xim))) NIL
;; 	  (im-window clx-xim) (window (check-server (connect-state clx-xim))))
;;     (push (cons :server-name
;; 		(nth
;; 		 (index (check-server (connect-state clx-xim)))
;; 		 (server-atoms clx-xim)))
;; 	  (atoms clx-xim))
;;     (return-from -clx-xim-check-server-transport-wait- :action-accept)))

(defun -clx-xim-connect-prepare- (clx-xim)
  (setf (im-client-window clx-xim)
	(create-window :parent (root-window clx-xim)
		       :depth 0;;use the depth of parent
		       :x 0 :y 0
		       :width 1 :height 1
		       :class :input-output
		       :visual (screen-root-visual (default-screen clx-xim))))

  (send-event (im-window clx-xim)
	      :client-message
	      0
	      :window (im-window clx-xim)
	      :type :_xim_xconnect
	      :format 32
	      :data (list (window-id (im-client-window clx-xim))
			  0 0 0 0)
	      :propagate-p NIL))

(defun padding-data-list (data times)
  (if (>= (length data) times)
      data
      (padding-data-list (append data (list 0)) times)))

(defun -clx-send-xim-message- (display protocol-atom window data length atom-name)
  (unless data
    (return-from -clx-send-xim-message- NIL))
  (if (> (=+ length *clx-xim-header-size*) *clx-xim-cm-data-size*)
      (progn (intern-atom display atom-name)
	     (change-property window atom-name
			      data :string 8
				   :mode :append)
	     (format t "~A~%" (list length
				    (find-atom display atom-name)
				    0 0 0))
	     (format t "~A~%" data)
	     (send-event window
			 :client-message
			 0
			 :window window
			 :type protocol-atom
			 :format 32
			 :data (list length
				     (find-atom display atom-name)
				     0 0 0)))
      (progn (format t "~A~%" (padding-data-list data *clx-xim-cm-data-size*))
	     (send-event window
			 :client-message
			 0
			 :window window
			 :type protocol-atom
			 :format 8
			 :data (padding-data-list data *clx-xim-cm-data-size*)
			 :propagate-p NIL))))

(defun -clx-xim-send-message- (clx-xim data length)
  (-clx-send-xim-message- (display clx-xim)
			  :_xim_protocol
			  (accept-win clx-xim)
			  data
			  length
			  (get-keyword (concatenate 'string
						    "_client" (write-to-string (connect-id clx-xim))
						    "_" (write-to-string (xim-sequence clx-xim)))))
  (setf (xim-sequence clx-xim) (1+ (xim-sequence clx-xim))))

(defun -clx-xim-send-frame- (clx-xim frame)
  (-clx-xim-send-message- clx-xim
			    (append (obj-to-data (make-instance 'clx-im-packet-header-fr
								:major-opcode (clx-proto-frame-opcode frame)
								:minor-opcode 0
								:header-bytes (/ (size-packet frame) 4)))
				    (obj-to-data frame))
			    (size-packet frame)))

(defun -clx-xim-connect-wait- (clx-xim)
  (let ((display (display clx-xim)))
    (event-case (display)
      ((:client-message) (window type format data)
       (unless (eq type :_xim_xconnect)
	 (return-from -clx-xim-connect-wait- :action-yield))
       (setf (major-code clx-xim) 0
	     (minor-code clx-xim) 0
	     (accept-win clx-xim) (xlib::lookup-window (display clx-xim) (elt data 0)));TODO: get window obj

       (unless (-clx-xim-send-frame- clx-xim (make-instance 'clx-im-connect-fr
							    :byte-order (byte-order clx-xim)
							    :pad 0
							    :client-major-protocol-version 0
							    :client-minor-protocol-version 0
							    :protocol-size 0
							    :protocol-items NIL))
	 (return-from -clx-xim-connect-wait- :action-failed))
       :action-accept))))

(defun -clx-read-xim-message- (display window format data)
  (let ((header)
	(message))
    (case format
      (8
       (setf header (-clx-xim-read-frame- data :clx-im-packet-header-fr)
	     message data))
      (32
       (let ((reply (list->vector
		     (get-property
		      window
		      (atom-name
		       display
		       (aref data (- (length data) 2)))))))
	 (when reply
	   (setf header (-clx-xim-read-frame- reply :clx-im-packet-header-fr)
		 message reply)))))
    (list header message)))

(defun -clx-xim-send-open- (clx-xim)
  (-clx-xim-send-frame- clx-xim (make-instance 'clx-xim-open-fr))
  (setf (open-state clx-xim) :xim-open-wait-open-reply)
  ;; (format t  "-clx-xim-send-open-")
  )



(defun -clx-xim-connect-wait-reply- (clx-xim a b c e f)
  (let ((display (display clx-xim)))
    (event-case (display)
      ((:client-message) (window type format data)
       (unless (eq type :_xim_protocol)
	 (return-from -clx-xim-connect-wait-reply- :action-yield))
       (destructuring-bind (header message)
	   (-clx-read-xim-message- (display clx-xim)
				   (accept-win clx-xim)
				   format
				   (list->vector (coerce data 'list)))
	 (unless (= (major-opcode header) *clx-xim-connect-reply*)
	   (return-from -clx-xim-connect-wait-reply- :action-yield))
	 (let ((reply-frame (-clx-xim-read-frame- message :clx-im-connect-reply-fr)))
	   (when (-clx-xim-send-open- clx-xim)
	     (return-from -clx-xim-connect-wait-reply- :action-accept))))))))



;; (defun -clx-xim-connect-wait-reply- (clx-xim event-type window type format data)
;;   (unless (and (eq event-type :client-message)
;; 	       (eq type :_xim_protocol))
;;     (return-from -clx-xim-connect-wait-reply- :action-yield))
;;   (destructuring-bind (header message)
;; 	   (-clx-read-xim-message- (display clx-xim)
;; 				   (accept-win clx-xim)
;; 				   format
;; 				   (list->vector (coerce data 'list)))
;; 	 (unless (= (major-opcode header) *clx-xim-connect-reply*)
;; 	   (return-from -clx-xim-connect-wait-reply- :action-yield))
;; 	 (let ((reply-frame (-clx-xim-read-frame- message :clx-im-connect-reply-fr)))
;; 	   (when (-clx-xim-send-open- clx-xim)
;; 	     (return-from -clx-xim-connect-wait-reply- :action-accept)))))



(defun clx-xim-preconnect-im (clx-xim event-type &key window selection target property time type format data)
  (block block-preconnect
    (case (state-phase (connect-state clx-xim))
      (print (subphase (check-server (connect-state clx-xim))))
      (:xim-connect-check-server
       (when (eq (length (server-atoms clx-xim))
		 (index (check-server (connect-state clx-xim))))
	 (setf (state-phase (connect-state clx-xim)) :xim-connect-fail)
	 (return-from block-preconnect))
       (case (subphase (check-server (connect-state clx-xim)))
	 (:xim-connect-check-server-prepare
	  (if (-clx-xim-check-server-prepare- clx-xim)
	      (setf (subphase (check-server (connect-state clx-xim)))
		    :xim-connect-check-server-locale)
	      (progn (-check-next-server- clx-xim)
		     (return-from block-preconnect))))
	 (:xim-connect-check-server-locale ;TODO:
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim-connect-check-server-transport))
	 (:xim-connect-check-server-transport
	  (-clx-xim-check-server-transport- clx-xim)
	  ;; (print ":xim-connect-check-server-transport")
	  (setf (subphase (check-server (connect-state clx-xim)))
		:xim-connect-check-server-transport-wait))
	 (:xim-connect-check-server-transport-wait
	  (case (-clx-xim-check-server-transport-wait- clx-xim event-type window selection target property time)
	    (:action-accept
	     (print ":xim-connect-connect:xim-connect-connect:xim-connect-connect:xim-connect-connect")
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-connect
		   (connect-subphase (connect-state clx-xim)) :xim-connect-connect-prepare))
	    (:action-failed
	     (-check-next-server- clx-xim))
	    (:action-yield
	     (return-from clx-xim-preconnect-im NIL)))
	  (return-from block-preconnect))))
      (:xim-connect-connect
       (print (connect-subphase (connect-state clx-xim)))
       (case (connect-subphase (connect-state clx-xim))
	 (:xim-connect-connect-prepare
	  (-clx-xim-connect-prepare- clx-xim)
	  (setf (connect-subphase (connect-state clx-xim)) :xim-connect-connect-wait))
	 (:xim-connect-connect-wait
	  (case (-clx-xim-connect-wait- clx-xim)
	    (:action-accept
	     (setf (connect-subphase (connect-state clx-xim)) :xim-connect-connect-wait-reply)
	     (return-from block-preconnect))
	    (:action-failed
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-fail)
	     (return-from clx-xim-preconnect-im))
	    (:action-yield
	     (return-from clx-xim-preconnect-im NIL))))
	 (:xim-connect-connect-wait-reply
	  (case (-clx-xim-connect-wait-reply- clx-xim event-type window type format data)
	    (:action-accept
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-done)
	     (return-from block-preconnect))
	    (:action-failed
	     (setf (state-phase (connect-state clx-xim)) :xim-connect-fail)
	     (return-from block-preconnect))
	    (:action-yield
	     (return-from clx-xim-preconnect-im NIL)))
	  )))
      (otherwise (print "return")
       (print (state-phase (connect-state clx-xim)))
       (return-from clx-xim-preconnect-im))))
  (clx-xim-preconnect-im clx-xim event-type :window window :selection selection :target target :property property :time time :type type :format format :data data))

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
     (setf (requestor-window check-server) NIL)
     (setf (window check-server) 0)
     (setf (subphase check-server) :xim-connect-check-server-prepare))
   (check-server (connect-state clx-xim)))
  (clx-xim-preconnect-im clx-xim NIL))


(defun clx-xim-open (clx-xim clx-xim-open-callback auto-connect user-data)
  (funcall (lambda (connect-state)
	     (setf (callback connect-state) clx-xim-open-callback
		   (user-data connect-state) user-data))
	   (connect-state clx-xim))
  (setf (auto-connect clx-xim) auto-connect)
  (-clx-xim-open- clx-xim))

(defun clx-xim-client-message (clx-xim format data)
  (destructuring-bind (header message)
      (-clx-read-xim-message- (display clx-xim) (im-client-window clx-xim) format
			      (list->vector (coerce data 'list)))
    (-clx-xim-handle-message- clx-xim header message (major-opcode header))))

(defun -clx-xim-find-icattr- (clx-xim attr)
  (gethash attr (icattr clx-xim)))

(defun clx-xim-create-nested-list (clx-xim attr pos)
  (let ((icattr (-clx-xim-find-icattr- clx-xim attr))
	(fr (make-instance 'clx-im-xicattribute-fr))
	(total-szie 0)
	(data NIL))
    (unless icattr
      (return-from clx-xim-create-nested-list NIL))
    (setf (value-length fr) (clx-im-ic-attr-size (type-of-value icattr)))
    (setf total-szie (size-packet fr))
    (=-append data
	      (data-to-byte (attribute-id icattr) :u2)
	      (data-to-byte (value-length fr) :u2)
	      (clx-im-get-ic-value pos (type-of-value icattr)))
    (=-append data (data-to-byte NIL :pads :length (pad-4 (length data))))
    (cons data total-szie)))

(defun clx-xim-set-ic-focus (clx-xim ic)
  (-clx-xim-send-frame- clx-xim (make-instance 'clx-xim-set-ic-focus-fr
					       :input-method-id (connect-id clx-xim)
					       :input-context-id ic)))

(defun -clx-xim-send-request-frame- (clx-xim request)
  (case (major-code request)
    (#.*clx-xim-forward-event* (-clx-xim-send-message- clx-xim (frame request) 40))
    (otherwise (-clx-xim-send-frame- clx-xim (frame request)))))

(defun -clx-xim-process-fail-callback- (clx-xim request)
  (unless (callback request)
    (return-from -clx-xim-process-fail-callback- NIL))
  (case (major-code request)
    (#.*clx-xim-create-ic*
     (funcall (callback request) clx-xim clx-xim 0 (user-data request))))
  ;; (cond ((eq *clx-xim-create-ic*
  ;; 	     (major-code request))
  ;; 	 )
  ;; 	((eq *clx-xim-destroy-ic*
  ;; 	     (major-code request))
  ;; 	 (funcall (callback request) clx-xim (input-context-id (frame request)))
  ;; 	 )

  ;; 	((eq *clx-xim-get-im-values*
  ;; 	     (major-code request))
  ;; 	 (funcall (callback request) clx-xim )
  ;; 	 )

  ;; 	((eq *clx-xim-get-ic-values*
  ;; 	     (major-code request))
  ;; 	 (funcall (callback request) clx-xim )
  ;; 	 )

  ;; 	((eq *clx-xim-set-ic-values*
  ;; 	     (major-code request))
  ;; 	 (funcall (callback request) clx-xim )
  ;; 	 )

  ;; 	((eq *clx-xim-reset-ic*
  ;; 	     (major-code request))
  ;; 	 (funcall (callback request) clx-xim )
  ;; 	 )
  ;; 	)
  )

(defun -clx-xim-process-queue- (clx-xim)
  (labels ((processor ()
	     (when (and (queue clx-xim)
			(not (current clx-xim)))
	       (let ((request (pop (queue clx-xim))))
		 (if (-clx-xim-send-request-frame- clx-xim request)
		     (unless (eq *clx-xim-forward-event* (major-code request))
		       (setf (current clx-xim) request))
		     (progn (-clx-xim-process-fail-callback- clx-xim request)
			    (setf (current clx-xim) NIL))))
	       (processor))))
    (processor)))

(defun clx-xim-create-ic (clx-xim callback user-data &rest rest)
  (unless (eq (open-state clx-xim)
	      :xim-open-done)
    (return-from clx-xim-create-ic NIL))
  (let ((queue (make-instance 'clx-xim-request
			      :major-code *clx-xim-create-ic* :minor-code 0
			      :callback callback :user-data user-data
			      :frame (make-instance 'clx-im-create-ic-fr
						    :input-method-id (connect-id clx-xim)))))
    (dolist (item rest)
      ;; (print item)
      (when (or (string= (car item) *clx-xim-xnclient-window*)
		(string= (car item) *clx-xim-xnfocus-window*))
	(setf (client-window clx-xim) (second item)))
      (let ((icattr (-clx-xim-find-icattr- clx-xim (car item))))
	(if (eq (type-of-value icattr) *ximtype-nest*)
	    (push (make-instance 'clx-im-xicattribute-fr
					:attribute-id (attribute-id icattr)
					:value-length (cdadr item)
					:value (caadr item))
			 (items (frame queue)))
	    (progn ;; (print item)
		   (push (make-instance 'clx-im-xicattribute-fr
				 :attribute-id (attribute-id icattr)
				 :value-length (clx-im-ic-attr-size (type-of-value icattr))
				 :value (clx-im-get-ic-value (second item) (type-of-value icattr)))
			 (items (frame queue)))))))
    (=-append (queue clx-xim) (list queue)))
  (-clx-xim-process-queue- clx-xim))


(defun clx-xim-destroy-window (clx-xim window)
  (unless (and (eq :xim-open-done
		   (open-state clx-xim))
	       (eq (accept-win clx-xim) window))
    NIL))

(defun clx-xim-property-changed (clx-xim window)
  (unless (and (auto-connect clx-xim)
	       (eq (root-window clx-xim) window))
    (return-from clx-xim-property-changed NIL))
  (setf (recheck clx-xim) T)
  (when (eq :xim-connect-fail (state-phase (connect-state clx-xim)))
    (setf (yield-recheck clx-xim) T)))

(defun clx-xim-forward-key (clx-xim ic event)
  (format t ">>>clx-xim-forward-key ~%")
  (unless (and ic
	       (eq (state-phase (connect-state clx-xim)) :xim-connect-done))
    (return-from clx-xim-forward-key NIL))
  (=-append (queue clx-xim)
	    (list
	     (make-instance 'clx-xim-request
			    :major-code *clx-xim-forward-event* :minor-code 0
			    :callback NIL :user-data NIL
			    :frame
			    (append
			     (obj-to-data (make-instance 'clx-im-packet-header-fr
							 :major-opcode *clx-xim-forward-event*
							 :minor-opcode 0
							 :header-bytes 10))
			     (obj-to-data (make-instance 'clx-im-forward-event-fr
							 :input-method-id (connect-id clx-xim)
							 :input-context-id ic
							 :flag *clx-xim-synchronous*
							 :sequence-number (third event)))
			     (obj-to-data (make-instance 'clx-im-key-press-event-fr
							 :response-type (pop event)
							 :code (pop event)
							 :x-sequence (pop event)
							 :x-time (pop event)
							 :root (window-id (pop event))
							 :event (window-id (pop event))
							 :child (let ((temp (pop event)))
								  (if temp
								      (window-id temp)
								      0))
							 :root-x (pop event)
							 :root-y (pop event)
							 :event-x (pop event)
							 :event-y (pop event)
							 :state (pop event)
							 :same-screen (if (pop event)
									  1
									  0)))))))
  (-clx-xim-process-queue- clx-xim))

(defun -clx-xim-sync- (clx-xim ic)
  ;; (format t "~%-clx-xim-sync- ~A~%" ic)
  (-clx-xim-send-frame- clx-xim (make-instance 'clx-im-sync-reply-fr
					       :input-method-id (connect-id clx-xim)
					       :input-context-id ic)))

(defun clx-xim-set-ic-values (clx-xim ic callback user-data &rest rest)
  (unless (eq (open-state clx-xim)
	      :xim-open-done)
    (return-from clx-xim-create-ic NIL))
  (let ((queue (make-instance 'clx-xim-request
			      :major-code *clx-xim-set-ic-values* :minor-code 0
			      :callback callback :user-data user-data
			      :frame (make-instance 'clx-im-set-ic-values-fr
						    :input-method-id (connect-id clx-xim)
						    :input-context-id ic
						    :))))
    (dolist (item rest)
      ;; (print item)
      (=+ (size (frame queue)) 1)
      (when (or (string= (car item) *clx-xim-xnclient-window*)
		(string= (car item) *clx-xim-xnfocus-window*))
	(setf (client-window clx-xim) (second item)))
      (let ((icattr (-clx-xim-find-icattr- clx-xim (car item))))
	(if (eq (type-of-value icattr) *ximtype-nest*)
	    (push (make-instance 'clx-im-xicattribute-fr
					:attribute-id (attribute-id icattr)
					:value-length (cdadr item)
					:value (caadr item))
			 (items (frame queue)))
	    (progn ;; (print item)
		   (push (make-instance 'clx-im-xicattribute-fr
				 :attribute-id (attribute-id icattr)
				 :value-length (clx-im-ic-attr-size (type-of-value icattr))
				 :value (clx-im-get-ic-value (second item) (type-of-value icattr)))
			 (items (frame queue)))))))
    (=-append (queue clx-xim) (list queue))))
