(defpackage #:demo
  (:use #:cl
	#:xlib
	#:clx-xim
	#:ximproth)
  (:export #:start-demo
	   #:clx-xim-client-message))

(in-package #:demo)

(defun logger (fmt)
  (format t "~A~%" fmt))

(defun forward-event (clx-xim clx-xic code state response-type user-data)
  ;; (format t "forward-event>>")
  ;; (format t "Key ~A ~%" key-press-event)
  ;; (format t "Key ~A Keycode ~A, State ~A~%" key-press-event)
  (format t "forward-event>> ~[press ~; release ~] keycode ~A ~A~%" (- response-type 2) code (keycode->character (get-display) code state))
  )

(defun commit-string (clx-xim clx-xic flag str keysym nKeySym user-data)
  (format t "Key commit: ~A~%" input-content))

(defun create-ic-callback (clx-xim ic user-data)
  (set-ic ic)
  (when (get-ic)
    (format t "icid: ~A~%" (get-ic))
    (clx-xim-set-ic-focus clx-xim (get-ic))))

(defun open-callback (clx-xim user-data)
  (clx-xim-create-ic clx-xim #'create-ic-callback NIL
		     (list *clx-xim-xninput-style* (logior *clx-im-preeditposition* *clx-im-statusarea*))
		     (list *clx-xim-xnclient-window* (get-window))
		     (list *clx-xim-xnfocus-window* (get-window))
		     (list *clx-xim-xnpreedit-attributes* (clx-xim-create-nested-list clx-xim *clx-xim-xnspot-location* '(0 0)))))

(let ((clx-xim NIL)
      (ic NIL))
  (defun set-up-clx-xim (display screen
			 &key im-callback logger)
    (setf clx-xim (make-clx-xim display screen))
    (clx-xim-set-im-callback clx-xim im-callback NIL)
    (clx-xim-set-log-handler clx-xim logger)
    (clx-xim-open clx-xim #'open-callback T NIL))
  (defun print-clx-xim ()
    (format t "~A~%" (im-callback clx-xim)))
  (defun get-clx-xim ()
    clx-xim)
  (defun get-ic ()
    ic)
  (defun set-ic (new-ic)
    (setf ic new-ic)))


(let ((display nil)
      (screen nil)
      (window nil)
      (event-mask (make-event-mask :key-press
				   :key-release
				   :focus-change)))
  (defun get-window ()
    window)
  (defun get-screen ()
    screen)
  (defun get-display ()
    display)

  (defun make-window ()
    (setf display (open-default-display)
	  screen (display-default-screen display))
    (let ((fg-color (screen-white-pixel screen))
	  (bg-color (screen-black-pixel screen))
	  (root (screen-root screen)))
      (setf window (create-window :parent root
				  :x 10 :y 20
				  :event-mask '(:visibility-change)
				  :background fg-color
				  :width 10 :height 100
				  ;; :border-width 2
				  :override-redirect :off))))


  (defun pre-display ()
    (setf (window-event-mask window) event-mask)
    (print (make-event-keys (window-event-mask window)))
    (print "i' ready"))
  (defun display-window ()
    (pre-display)
    (map-window window)
    (display-finish-output display))

  (defun event-loop ()
    "doc"
    (event-case (display)
      ((:client-message) (window type format data)
       ;; (print ":client-message")

       (case type
	 (:_xim_protocol (clx-xim-client-message (get-clx-xim) format data))

       ;; (when (eq type :_xim_protocol)
       ;; 	 ;; (format t "~%window: ~A type:  ~A format: ~A data: ~A~%" window type format data)
       ;; 	 )
       ))

      ((:property-notify) (window atom state time)
       (case atom
	 (:xim_servers (clx-xim-property-changed (get-clx-xim) window))))

      ;; ((:selection-notify) (window selection target property time)
      ;;  (print ":selection-notify")
      ;;  (format t "~%window: ~A selection: ~A target: ~A property: ~A~%" window selection target property)
      ;;  )

      ((:key-release) (window sequence code x y state time root root-x root-y child same-screen-p)
       (format t "~%key release ~A ~%" (keycode->character (get-display) code state))
       (clx-xim-forward-key (get-clx-xim) (get-ic) (list 3 code sequence time root window child  root-x root-y x y state same-screen-p))

       ;; (format t "~{~A ~}~%" (list 2 code sequence time root window child  root-x root-y x y state same-screen-p))
       ;; (print "catch you")
       ;; (print window)
       ;; (print code)
       )

      ((:key-press) (window sequence code x y state time root root-x root-y child same-screen-p)
       (format t "~%key press ~A ~%" (keycode->character (get-display) code state))
       (clx-xim-forward-key (get-clx-xim) (get-ic) (list 2 code sequence time root window child  root-x root-y x y state same-screen-p))

       ;; (format t "~{~A ~}~%" (list 2 code sequence time root window child  root-x root-y x y state same-screen-p))
       ;; (print "catch you")
       ;; (print window)
       ;; (print code)
       )

      )
    (event-loop)
    )
  (defun start-window ()
    (make-window)
    (display-window)
    (set-up-clx-xim display screen
		    :im-callback (list (cons :forward-event #'forward-event)
				       (cons :commit-string #'commit-string))
		    :logger #'logger)

    (event-loop)
    ))


(defun start-demo ()
  (format t "~%New demo~%")
  (start-window))

;; (find-atom (get-display) :xim_servers)
;; (change-property (get-window) :wm_name "hello1" :string 8 :transform #'char-code)
;; (get-property (get-window) :locales)
;; (find-atom (get-display) :locales)
;; (get-property (get-window) :wm_name ;; :type :string
;; 	      :transform #'code-char
;; 	      :result-type 'string
;; 	      )

;; (list-properties (get-window))
;; (get-property (get-window) :locales)
;; (intern-atom (get-display) :locales)

;; (change-property requestor property
;; 			       "Hello, World (from the CLX clipboard)!"
;; 			       target 8
;; 			       :transform #'char-code)
;; (start-demo)

;; (gethash ximproth::*clx-xim-xnspot-location* (clx-xim::icattr (get-clx-xim)))

;; (cdr (cons *clx-xim-xninput-style* (logior *clx-im-preeditposition* *clx-im-statusarea*)))
