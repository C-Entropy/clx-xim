(defpackage #:protrocol-handler
  (:use #:cl
	#:ximproth)
  (:export #:-clx-xim-handle-message-))

(in-package #:protrocol-handler)

(defgeneric -clx-xim-handle-message- (clx-xim hdr data type)
  (:documentation "doc"))

(defmethod -clx-xim-handle-message- (clx-xim hdr data (type (eql *clx-xim-connect-reply*)))
  (format t "*clx-xim-connect-reply* :~A~%" *clx-xim-connect-reply*))

(defmethod -clx-xim-handle-message- (clx-xim hdr data (type (eql *clx-xim-open-reply*)))
  (format t "*clx-xim-open-reply* :~A~%" *clx-xim-open-reply*))
