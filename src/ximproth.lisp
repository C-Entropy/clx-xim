(defpackage #:ximproth
  (:use #:cl
	;; #:utils
	)
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
   #:*clx-xim-ext-move*

   #:*clx-xim-xnquery-inputstyle*
   #:*clx-xim-xnclient-window*
   #:*clx-xim-xninput-style*
   #:*clx-xim-xnfocus-window*
   #:*clx-xim-xnfilter-events*
   #:*clx-xim-xnpreedit-attributes*
   #:*clx-xim-xnstatus-attributes*
   #:*clx-xim-xnarea*
   #:*clx-xim-xnarea-needed*
   #:*clx-xim-xnspot-location*
   #:*clx-xim-xncolormap*
   #:*clx-xim-xnstd-colormap*
   #:*clx-xim-xnforeground*
   #:*clx-xim-xnbackground*
   #:*clx-xim-xnbackground-pixmap*
   #:*clx-xim-xnfont-set*
   #:*clx-xim-xnline-space*
   #:*clx-xim-xnseparatorof-nested-list*

   ;; values for the type of XIMATTR & XICATTR

   #:*ximtype-separatorofnestedlist*
   #:*ximtype-card8*
   #:*ximtype-card16*
   #:*ximtype-card32*
   #:*ximtype-string8*
   #:*ximtype-window*
   #:*ximtype-ximstyles*
   #:*ximtype-xrectangle*
   #:*ximtype-xpoint*
   #:*ximtype-xfontset*
   #:*ximtype-ximoptions*
   #:*ximtype-ximhotkeytriggers*
   #:*ximtype-ximhotkeystate*
   #:*ximtype-ximstringconversion*
   #:*ximtype-ximvalueslist*
   #:*ximtype-nest*


   #:*clx-im-preeditarea*
   #:*clx-im-preeditcallbacks*
   #:*clx-im-preeditposition*
   #:*clx-im-preeditnothing*
   #:*clx-im-preeditnone*
   #:*clx-im-statusarea*
   #:*clx-im-statuscallbacks*
   #:*clx-im-statusnothing*
   #:*clx-im-statusnone*

   #:*clx-xim-synchronous*
   #:*clx-xim-lookup-chars*
   #:*clx-xim-lookup-keysym*
   #:*clx-xim-lookup-both*
))

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


(defparameter *clx-xim-xnquery-inputstyle* "queryInputstyle")
(defparameter *clx-xim-xnclient-window* "clientWindow")
(defparameter *clx-xim-xninput-style* "inputStyle")
(defparameter *clx-xim-xnfocus-window* "focusWindow")
(defparameter *clx-xim-xnfilter-events* "filterEvents")
(defparameter *clx-xim-xnpreedit-attributes* "preeditAttributes")
(defparameter *clx-xim-xnstatus-attributes* "statusAttributes")
(defparameter *clx-xim-xnarea* "area")
(defparameter *clx-xim-xnarea-needed* "areaNeeded")
(defparameter *clx-xim-xnspot-location* "spotLocation")
(defparameter *clx-xim-xncolormap* "colorMap")
(defparameter *clx-xim-xnstd-colormap* "stdColorMap")
(defparameter *clx-xim-xnforeground* "foreground")
(defparameter *clx-xim-xnbackground* "background")
(defparameter *clx-xim-xnbackground-pixmap* "backgroundPixmap")
(defparameter *clx-xim-xnfont-set* "fontSet")
(defparameter *clx-xim-xnline-space* "lineSpace")
(defparameter *clx-xim-xnseparatorof-nested-list* "separatorofNestedlist")


;; values for the type of XIMATTR & XICATTR
(defparameter *ximtype-separatorofnestedlist* 0)
(defparameter *ximtype-card8* 1)
(defparameter *ximtype-card16* 2)
(defparameter *ximtype-card32* 3)
(defparameter *ximtype-string8* 4)
(defparameter *ximtype-window* 5)
(defparameter *ximtype-ximstyles* 10)
(defparameter *ximtype-xrectangle* 11)
(defparameter *ximtype-xpoint* 12)
(defparameter *ximtype-xfontset* 13)
(defparameter *ximtype-ximoptions* 14)
(defparameter *ximtype-ximhotkeytriggers* 15)
(defparameter *ximtype-ximhotkeystate* 16)
(defparameter *ximtype-ximstringconversion* 17)
(defparameter *ximtype-ximvalueslist* 18)
(defparameter *ximtype-nest* #x7fff)


;; (defun clx-im-ic-attr-size (type)
;;   (cond ((or (eq type *ximtype-card32*)
;; 	     (eq type *ximtype-window*))
;; 	 4)
;; 	((eq type *ximtype-xrectangle*)
;; 	 8)
;; 	((eq type *ximtype-xpoint*)
;; 	 4)))

(defparameter *clx-im-preeditarea* #x0001)
(defparameter *clx-im-preeditcallbacks* #x0002)
(defparameter *clx-im-preeditposition* #x0004)
(defparameter *clx-im-preeditnothing* #x0008)
(defparameter *clx-im-preeditnone* #x0010)
(defparameter *clx-im-statusarea* #x0100)
(defparameter *clx-im-statuscallbacks* #x0200)
(defparameter *clx-im-statusnothing* #x0400)
(defparameter *clx-im-statusnone* #x0800)

(defparameter *clx-xim-synchronous* 1)
(defparameter *clx-xim-lookup-chars*  2)
(defparameter *clx-xim-lookup-keysym*  4)
(defparameter *clx-xim-lookup-both*  (logior *clx-xim-lookup-chars* *clx-xim-lookup-keysym*))
