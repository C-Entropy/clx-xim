(defpackage #:ximproth
  (:use #:cl)
  (:export
   #:*clx-xim-connect*
   #:*clx-xim-connect-reply*
   #:*clx-xim-disconnect*
   #:*clx-xim-disconnect-reply*
   #:*clx-xim-auth-required*
   #:*clx-xim-auth-reply*
   #:*clx-xim-auth-next*
   #:*clx-xim-auth-setup*
   #:*clx-xim-auth-ng*
   #:*clx-xim-error*
   #:*clx-xim-open*
   #:*clx-xim-open-reply*
   #:*clx-xim-close*
   #:*clx-xim-close-reply*
   #:*clx-xim-register-triggerkeys*
   #:*clx-xim-trigger-notify*
   #:*clx-xim-trigger-notify-reply*
   #:*clx-xim-set-event-mask*
   #:*clx-xim-encoding-negotiation*
   #:*clx-xim-encoding-negotiation-reply*
   #:*clx-xim-query-extension*
   #:*clx-xim-query-extension-reply*
   #:*clx-xim-set-im-values*
   #:*clx-xim-set-im-values-reply*
   #:*clx-xim-get-im-values*
   #:*clx-xim-get-im-values-reply*
   #:*clx-xim-create-ic*
   #:*clx-xim-create-ic-reply*
   #:*clx-xim-destroy-ic*
   #:*clx-xim-destroy-ic-reply*
   #:*clx-xim-set-ic-values*
   #:*clx-xim-set-ic-values-reply*
   #:*clx-xim-get-ic-values*
   #:*clx-xim-get-ic-values-reply*
   #:*clx-xim-set-ic-focus*
   #:*clx-xim-unset-ic-focus*
   #:*clx-xim-forward-event*
   #:*clx-xim-sync*
   #:*clx-xim-sync-reply*
   #:*clx-xim-commit*
   #:*clx-xim-reset-ic*
   #:*clx-xim-reset-ic-reply*
   #:*clx-xim-geometry*
   #:*clx-xim-str-conversion*
   #:*clx-xim-str-conversion-reply*
   #:*clx-xim-preedit-start*
   #:*clx-xim-preedit-start-reply*
   #:*clx-xim-preedit-draw*
   #:*clx-xim-preedit-caret*
   #:*clx-xim-preedit-caret-reply*
   #:*clx-xim-preedit-done*
   #:*clx-xim-status-start*
   #:*clx-xim-status-draw*
   #:*clx-xim-status-done*

   ;; Minor Protocol Number for Extension Protocol
   #:*clx-xim-extension*
   #:*clx-xim-ext-set-event-mask*
   #:*clx-xim-ext-forward-keyevent*
   #:*clx-xim-ext-move*))

(in-package #:ximproth)

(defparameter *clx-xim-connect* 1)
(defparameter *clx-xim-connect-reply* 2)
(defparameter *clx-xim-disconnect* 3)
(defparameter *clx-xim-disconnect-reply* 4)

(defparameter *clx-xim-auth-required* 10)
(defparameter *clx-xim-auth-reply* 11)
(defparameter *clx-xim-auth-next* 12)
(defparameter *clx-xim-auth-setup* 13)
(defparameter *clx-xim-auth-ng* 14)

(defparameter *clx-xim-error* 20)

(defparameter *clx-xim-open* 30)
(defparameter *clx-xim-open-reply* 31)
(defparameter *clx-xim-close* 32)
(defparameter *clx-xim-close-reply* 33)
(defparameter *clx-xim-register-triggerkeys* 34)
(defparameter *clx-xim-trigger-notify* 35)
(defparameter *clx-xim-trigger-notify-reply* 36)
(defparameter *clx-xim-set-event-mask* 37)
(defparameter *clx-xim-encoding-negotiation* 38)
(defparameter *clx-xim-encoding-negotiation-reply* 39)

(defparameter *clx-xim-query-extension* 40)
(defparameter *clx-xim-query-extension-reply* 41)
(defparameter *clx-xim-set-im-values* 42)
(defparameter *clx-xim-set-im-values-reply* 43)
(defparameter *clx-xim-get-im-values* 44)
(defparameter *clx-xim-get-im-values-reply* 45)

(defparameter *clx-xim-create-ic* 50)
(defparameter *clx-xim-create-ic-reply* 51)
(defparameter *clx-xim-destroy-ic* 52)
(defparameter *clx-xim-destroy-ic-reply* 53)
(defparameter *clx-xim-set-ic-values* 54)
(defparameter *clx-xim-set-ic-values-reply* 55)
(defparameter *clx-xim-get-ic-values* 56)
(defparameter *clx-xim-get-ic-values-reply* 57)
(defparameter *clx-xim-set-ic-focus* 58)
(defparameter *clx-xim-unset-ic-focus* 59)

(defparameter *clx-xim-forward-event* 60)
(defparameter *clx-xim-sync* 61)
(defparameter *clx-xim-sync-reply* 62)
(defparameter *clx-xim-commit* 63)
(defparameter *clx-xim-reset-ic* 64)
(defparameter *clx-xim-reset-ic-reply* 65)

(defparameter *clx-xim-geometry* 70)
(defparameter *clx-xim-str-conversion* 71)
(defparameter *clx-xim-str-conversion-reply* 72)
(defparameter *clx-xim-preedit-start* 73)
(defparameter *clx-xim-preedit-start-reply* 74)
(defparameter *clx-xim-preedit-draw* 75)
(defparameter *clx-xim-preedit-caret* 76)
(defparameter *clx-xim-preedit-caret-reply* 77)
(defparameter *clx-xim-preedit-done* 78)
(defparameter *clx-xim-status-start* 79)
(defparameter *clx-xim-status-draw* 80)
(defparameter *clx-xim-status-done* 81)


;; Minor Protocol Number for Extension Protocol
(defparameter *clx-xim-extension* 128)
(defparameter *clx-xim-ext-set-event-mask* #x30)
(defparameter *clx-xim-ext-forward-keyevent* #x32)
(defparameter *clx-xim-ext-move* #x33)
