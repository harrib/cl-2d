(in-package :cl-2d)

;;;;  cl-2d provides some extra contexts and functions to augment
;;;;  cl-cairo2.

(defun xlib-image-context-to-png (context filename)
  "Write the contents of an xlib-image-context to a PNG file."
  (surface-write-to-png (get-target context) filename))

(defclass png-context (context)
  ((surface :initarg :surface
    :documentation "the image surface")
   (filename :initarg :filename
    :documentation "file we write to upon closing"))
  (:documentation "A png context is a surface which writed out the
  image to a file when it is closed by destroy."))

(defun create-png-context (width height filename &key 
			  (format 'format-rgb24) (background-color
			  +white+))
  "Create a png context (essentially a Cairo image surface that is
written into a file when closed, with the given background color."
  (let* ((surface (create-image-surface format width height))
	 (context (create-context surface))
	 (png-context (with-slots (width height pixel-based-p) context
			(make-instance 'png-context
				       :pointer (slot-value context 'pointer)
				       :width width
				       :height height
				       :pixel-based-p pixel-based-p ; t
				       :surface surface
				       :filename filename))))
    (set-source-color background-color png-context)
    (paint png-context)
    png-context))

(defmethod destroy ((png-context png-context))
  (with-slots (filename surface) png-context
    (surface-write-to-png surface filename)
    (call-next-method)))
