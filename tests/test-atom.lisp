(defpackage #:test-atom
  (:use #:cl
	#:xlib))

(in-package #:test-atom)
(display-)
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
  (defun get-root-win ()
    (screen-root screen))
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
    (print (make-event-keys (window-event-mask window))))
  (defun display-window ()
    (pre-display)
    (map-window window)
    (display-finish-output display))

  (defun event-loop ()
    "doc"
    (event-case (display)

      ((:client-message) (window type format data)
       (format t "window: ~A type:  ~A format: ~A data: ~A~%") window type format data)

      ((:selection-notify) (window selection target property time)
       (print ":selection-notify")
       ;; (format t "window: ~A type:  ~A format: ~A data: ~A~%")
       )

      ;; ((:key-press :key-release) (window code)
      ;;  (print "catch you")
      ;;  (print window)
      ;;  (print code))
      )
    (event-loop))

  (defun start-window ()
    (make-window)
    (display-window)
    ;; (event-loop)
    ))


(defun start-demo ()
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

;; (atom-name (get-display) 445)
;; (intern-atom (get-display) :xim_servers)
;; (get-property (get-window) :xim_servers)
;; (get-property )
;; (list-properties (get-window))
;; (get-property )
;; (start-demo)

;; (setf display (open-default-display))
;; (setf screen (display-default-screen display))
;; (setf fg-color (screen-white-pixel screen))
;; (setf bg-color (screen-black-pixel screen))
;; (setf root (screen-root screen))
;; (setf event-mask (make-event-mask :key-press
;; 				  :key-release
;; 				  :focus-change))
;; (setf window (create-window :parent root
;; 			    :x 10 :y 20
;; 			    :event-mask '(:visibility-change)
;; 			    :background fg-color
;; 			    :width 10 :height 100
;; 			    ;; :border-width 2
;; 			    :override-redirect :off))

;; (defun eloop (display)
;;   (event-case (display)
;;     ((:properity-change) (window atom state time)
;;      (print "aa")))
;;   (eloop display))

;; (eloop display)

;; (setf (window-event-mask window) event-mask)
;; (map-window window)
;; (display-finish-output display)
;; (intern-atom display :xim_servers)
;; (print (list-properties window))
(start-demo)
(get-property (get-root-win) :xim_servers)
(get-property (get-root-win) :_xim_xconnect)
(intern-atom (get-display) :_xim_xconnect)
(display-nscreens (get-display))
