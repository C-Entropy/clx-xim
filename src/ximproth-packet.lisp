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

(define-packet clx-im-connect-reply-fr
    ((server-major-protocol-version :u2)
     (server-minor-protocol-version :u2)))

(define-packet clx-xim-open-fr
    ((length-of-string :u1)
     (s-string :s-string)
     (pads :pads :length (pad-4 (+ 1
				   length-of-string))))
  :opcode *clx-xim-open*
  :size-packet
  (align-s-4 (+ 1
		(length s-string))
	     NIL))

(defmethod obj-to-data :before ((frame clx-xim-open-fr))
  (setf (length-of-string frame) (length (s-string frame))))

(define-packet clx-im-ximattr-fr
    ((attribute-id :u2)
     (type-of-value :u2)
     (length-of-im-attribute :u2)
     (im-attribute :s-string :length length-of-im-attribute)
     (pad :pads :length (pad-4 (+ 6
				     length-of-im-attribute))))
  :size-packet (align-s-4 (+ 6
			     length-of-im-attribute)
			  NIL))

(define-packet clx-im-xicattr-fr
    ((attribute-id :u2)
     (type-of-value :u2)
     (length-of-ic-attribute :u2)
     (ic-attribute :s-string :length length-of-ic-attribute)
     (pad :pads :length (pad-4 (+ 6
				  length-of-ic-attribute))))
  :size-packet (align-s-4 (+ 6
			     length-of-ic-attribute)
			  NIL))

(define-packet clx-xim-set-ic-focus-fr
    ((input-method-id :u2)
     (input-context-id :u2))
  :size-packet 4
  :opcode *clx-xim-set-ic-focus*)

(define-packet clx-im-xicattribute-fr
    ((attribute-id :u2)
     (value-length :u2)
     (value :bytes)
     (pad :pads :length (pad-4 value-length)))
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
     (pad-0 :pads :length (pad-4 (+ 2 info-length)))

     (extension-major-opcode :u1)
     (extension-minor-opcode :u1)
     (name-length :u2)
     (ext-name :s-string :length name-length)
     (pad-1 :pads :length (pad-4 name-length))
     )
  :size-packet (align-s-4 (+ 6 info-length name-length)
			  NIL))

(define-packet clx-im-ext-fr
    ((extension-major-opcode :u1)
     (extension-minor-opcode :u1)
     (name-length :u2)
     (ext-name :s-string :length name-length)
     (pad-1 :pads :length (pad-4 name-length)))
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

(define-packet clx-im-ximattribute-fr
    ((attribute-id :u2)
     (value-length :u2)
     (value :s-string))
  :size-packet (+ 4 (s-strings-bytes value)))

(define-packet clx-im-create-ic-fr
    ((input-method-id :u2)
     (size :u2)
     (items :clx-im-xicattribute-fr))
  :size-packet (+ 4 (let ((result 0))
		      (dolist (item items)
			(=+ result (size-packet item)))
		      result))
  :opcode *clx-xim-create-ic*)

(defmethod obj-to-data :before ((frame clx-im-create-ic-fr))
  (let ((result 0))
    (dolist (item (items frame))
      (=+ result (size-packet item)))
    (setf (size frame) result)))

(define-packet clx-im-create-ic-reply-fr
    ((input-method-id :u2)
     (input-context-id :u2))
  :size-packet 4)


(define-packet clx-im-set-event-mask-fr
    ((input-method-id :u2)
     (input-context-id :u2)
     (forward-event-mask :u4)
     (synchronous-event-mask :u4))
  :size-packet 12)

(defun clx-im-ic-attr-size (type)
  (cond ((or (eq type *ximtype-card32*)
	     (eq type *ximtype-window*)
	     (eq type *ximtype-xpoint*))
	 4)
	((eq type *ximtype-xrectangle*)
	 8)))


(defun clx-im-get-ic-value (pos type)
  (cond ((eq *ximtype-card32* type)
	 (data-to-byte pos :u4))

	((eq *ximtype-window* type)
	 (data-to-byte (window-id pos) :u4))

	((eq *ximtype-xpoint* type)
	 (obj-to-data (make-instance 'clx-xim::clx-im-xpoint-fr
				     :x (first pos)
				     :y (second pos))))
	((eq *ximtype-xrectangle* type )
	 (obj-to-data (make-instance 'clx-xim::clx-im-xrectangle-fr
				     :x (first pos)
				     :y (second pos)
				     :width (third pos)
				     :height (fourth pos))))))

(define-packet clx-im-forward-event-fr
    ((input-method-id :u2)
     (input-context-id :u2)
     (flag :u2)
     (sequence-number :u2)))

(define-packet clx-im-key-press-event-fr
    ((response-type :u1)
     (code :u1)
     (x-sequence :u2)
     (x-time :u4)
     (root :u4)
     (event :u4)
     (child :u4)
     (root-x :u2)
     (root-y :u2)
     (event-x :u2)
     (event-y :u2)
     (state :u2)
     (same-screen :u1)
     (pad :pads :length 1)))


(define-packet clx-im-sync-reply-fr
    ((input-method-id :u2)
     (input-context-id :u2))
  :size-packet 4
  :opcode *clx-xim-sync-reply*)

(define-packet clx-im-ximtriggerkey-fr
    ((key-sym :u4)
     (modifier :u4)
     (modifier-mask :u4))
  :size-packet 12)

(define-packet clx-im-register-triggerkeys-fr
    ((input-method-id :u2)
     (pad :u2)
     (on-key-length :u4)
     (on-key :clx-im-ximtriggerkey-fr :bytes on-key-length)
     (off-key-length :u4)
     (off-key :clx-im-ximtriggerkey-fr :bytes off-key-length)))
