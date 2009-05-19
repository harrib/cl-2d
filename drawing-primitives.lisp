(in-package :cl-2d)

(defun snap-to-integer (x y context)
  "Snap x and y to integer values in device coordinates."
  (multiple-value-bind (x y) (user-to-device x y context)
    (device-to-user (round x) (round y) context)))

(defmacro snap-pair-conditionally ((x y context snap-p
				      &optional (snap-function
				      'snap-to-integer))
				   &body body)
  (assert (and (symbolp x) (symbolp y)))
  `(multiple-value-bind (,x ,y) (if ,snap-p
				  (,snap-function ,x ,y ,context)
				  (values ,x ,y))
     ,@body))

