(in-package #:clx-xim)
(define-packet clx-im-connect-fr
    ((byte-order :u1)
     (pad :u1)
     (client-major-protocol-version :u2)
     (client-minor-protocol-version :u2)
     (protocol-size :u2)
     (protocol-items :strings))
  :opcode *clx-xim-connect*
  :size-packet
  (+ 8
     (strings-bytes protocol-items)))

(defmethod obj-to-data :before ((frame clx-im-connect-fr))
  (setf (protocol-size frame) (strings-bytes (protocol-items frame))))

(define-packet clx-im-packet-header-fr
    ((major-opcode :u1)
     (minor-opcode :u1)
     (header-bytes :u2)))

;; (defmethod -clx-xim-read-frame- (data (type (eql :clx-im-packet-header-fr)) &key)
;;   (setf (major-opcode obj) (-clx-xim-read-frame- :u1 data))
;;   (setf (minor-opcode obj) (-clx-xim-read-frame- :u1 data))
;;   (setf (header-bytes obj) (-clx-xim-read-frame- :u2 data))
;;   obj)

(define-packet clx-im-connect-reply-fr
    ((server-major-protocol-version :u2)
     (server-minor-protocol-version :u2)))

(defun clx-im-str-fr-size (string)
  (1+ (length string)))

(define-packet clx-xim-open-fr
    ((length-of-string :u1)
     (s-string :s-string))
  :opcode *clx-xim-open*
  :size-packet
  (align-s-4 (clx-im-str-fr-size s-string) NIL))

(defmethod obj-to-data :before ((frame clx-xim-open-fr))
  (setf (length-of-string frame) (length (s-string frame))))

(defmethod obj-to-data :around ((frame clx-xim-open-fr))
  (call-next-method))

(define-packet clx-im-ximattr-fr
    ((attribute-id :u2)
     (type-of-value :u2)
     (length-of-im-attribute :u2)
     (im-attribute :s-string :length length-of-im-attribute)
     (pad :bytes :length (pad-4 (+ 6
				     length-of-im-attribute))))
  :size-packet (align-s-4 (+ 6
			     length-of-im-attribute)
			  NIL))

(define-packet clx-im-xicattr-fr
    ((attribute-id :u2)
     (type-of-value :u2)
     (length-of-ic-attribute :u2)
     (ic-attribute :s-string :length length-of-ic-attribute)
     (pad :bytes :length (pad-4 (+ 6
				     length-of-ic-attribute))))
  :size-packet (align-s-4 (+ 6
			     length-of-ic-attribute)
			  NIL))

(define-packet clx-im-xicattribute-fr
    ((attribute-id :u2)
     (value-length :u2)
     (value :s-string))
  :size-packet (+ 4
		  (align-s-4 value-length
			     NIL)))

(define-packet clx-im-open-reply-fr
    ((input-method-id :u2)
     (im-size :u2)
     (im-ximattr :clx-im-ximattr-fr :bytes im-size)
     (ic-size :u2)
     (pad :u2)
     (ic-ximattr :clx-im-xicattr-fr :bytes ic-size)))

(define-packet clx-im-query-extension-fr
    ((input-method-id :u2)
     (byte-len :u2 :n-data (s-strings-bytes ext))
     (ext :s-strings)
     (pad :pads :length (pad-4 (s-strings-bytes ext))))
  :opcode *clx-xim-query-extension*
  :size-packet (align-s-4 (+ 4
			     (s-strings-bytes ext))
			  NIL))

(define-packet encodinginfo;;should be
    ((info-length :u2)
     (ext-info :s-string :length info-length)
     (pad-0 :bytes :length (pad-4 (+ 2 info-length)))

     (extension-major-opcode :u1)
     (extension-minor-opcode :u1)
     (name-length :u2)
     (ext-name :s-string :length name-length)
     (pad-1 :bytes :length (pad-4 name-length))
     )
  :size-packet (align-s-4 (+ 6 info-length name-length)
			  NIL))

(define-packet clx-im-ext-fr
    ((extension-major-opcode :u1)
     (extension-minor-opcode :u1)
     (name-length :u2)
     (ext-name :s-string :length name-length)
     (pad-1 :bytes :length (pad-4 name-length)))
  :size-packet (+ 4
		  (align-s-4 name-length NIL)))

(define-packet clx-im-query-extension-reply-fr
    ((input-method-id :u2)
     (ext-size :u2)
     (ext :clx-im-ext-fr :bytes ext-size)))


(define-packet clx-im-encoding-negotiation-fr
    ((input-method-id :u2)
     (size :u2 :n-data (s-strings-bytes encodings))
     (encodings :s-strings)
     (pad :pads :length (pad-4 (s-strings-bytes encodings)))
     (size-detail :u2 :n-data (s-strings-bytes encoding-info))
     (pad-1 :pads :length 2)
     (encoding-info :s-strings)
     )
  :opcode *clx-xim-encoding-negotiation*
  :size-packet
  (+ 8 (align-s-4 (s-strings-bytes encodings) NIL)
     (s-strings-bytes encoding-info)))

(define-packet clx-im-encoding-negotiation-reply-fr
    ((input-method-id :u2)
     (category :u2)
     (index :u2))
  :size-packet 6)

(define-packet clx-im-xrectangle-fr
    ((x :u2)
     (y :u2)
     (width :u2)
     (height :u2))
  :size-packet 8)

(define-packet clx-im-xpoint-fr
    ((x :u2)
     (y :u2))
  :size-packet 4)
