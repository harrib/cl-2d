(in-package :cl-2d)

;;;;  snapping
;;;;  
;;;;  Snapping to integers and integers+0.5 makes sure that lines and
;;;;  blocks look nice on pixel-based surfaces.  Use only when
;;;;  necessary.

(declaim (inline snap))

(defun snap (value mode)
  "Snapping has 3 modes:
   :none    value is passed through
   :int     value is rounded to the nearest integer
   :half    value is rounded to the nearest integer+0.5

On borders (int+0.5 for :int, int for :half) they round up."
  (ecase mode
    (:none value)
    (:int (round value))
    (:half (+ (floor value) 0.5))))

(defun snap* (value mode context)
  "Snap based on whether context is pixel-based."
  (if (pixel-based-p context)
      (snap value mode)
      value))

;;;;  primitives
;;;;
;;;;  Primitives are functions that use only cairo directly, for
;;;;  drawing on a context or setting up paths, etc.

(defun segment (context x-start y-start x-end y-end)
  "Draw a line from (x-start,y-start) to (x-end,y-end)."
  (move-to context x-start y-start)
  (line-to context x-end y-end)
  (stroke context))

(defun filled-rectangle (context color left top right bottom)
  "Fill the rectangle defined by the coordinates with color."
  (rectangle context left top (- right left) (- bottom top))
  (set-source-color context color)
  (fill-path context))

(defun circle-path (context x y radius)
  "Set up a path for a circle centered at (x,y) with given radius."
  (move-to context (+ x radius) y)
  (arc context x y radius 0 (twice pi)))

(defun filled-circle (x y radius color context)
  "Draw a filled circle with the given color, center (x,y) and radius."
  (circle-path context x y radius)
  (set-source-color context color)
  (fill-path context))

;;;;  text handling
;;;;
;;;;  Text handling relies on the ability of Cairo to measure the
;;;;  extents of a text, which are then aligned properly.

(defclass text-with-extents ()
  ((text :initarg :text)
   (x-bearing :initarg :x-bearing)
   (y-bearing :initarg :y-bearing)
   (width :initarg :width)
   (height :initarg :height)
   (x-advance :initarg :x-advance)
   (y-advance :initarg :y-advance)))

(defun add-text-extents (context text)
  (multiple-value-bind (x-bearing y-bearing width height x-advance y-advance)
      (text-extents context text)
    (make-instance 'text-with-extents
     :text text
     :x-bearing x-bearing
     :y-bearing y-bearing
     :width width
     :height height
     :x-advance x-advance
     :y-advance y-advance)))

;;;; not used now
;; (defun measure-labels (context labels)
;;   "Measure the extents of the strings in labels, return 3 values:
;;   the list of extents, the largest width and the largest height."
;;   (iter
;;     (for x in labels)
;;     (for (values x-bearing y-bearing width height) 
;;      := (text-extents context x))
;;     (collect (list x-bearing y-bearing width height) 
;;      :into extents)
;;     (finding width maximizing width :into max-width)
;;     (finding height maximizing height :into max-height)
;;     (finally
;;      (return (values extents max-width max-height)))))

(defun capital-letter-height (context)
  "Return the height of a typical capital letter, currently H."
  (multiple-value-bind (x-bearing y-bearing width height)
      (text-extents context "H")
    (declare (ignore x-bearing y-bearing width))
    height))

(defgeneric aligned-text (context x y text &key x-align y-align angle))
  
(defmethod aligned-text (context x y (text-with-extents text-with-extents)
			 &key (x-align 0.5) (y-align 0.5) (angle 0))
  "Show text-with-extents aligned relative to (x,y).  Return width and
height."
  (with-slots (text x-bearing y-bearing width height) text-with-extents
    (move-to context x y)
    (let ((trans-matrix (get-trans-matrix context))
	  (x-rel (+ x-bearing (* width x-align)))
	  (y-rel (+ y-bearing (* height y-align))))
      ;; show text
      (move-to context x y)
      (rotate context angle)
      (rel-move-to context (- 0 x-rel) (- 0 y-rel))
      (show-text context text)
      (set-trans-matrix context trans-matrix))
    (values width height)))

(defmethod aligned-text-rectangle (context x y text-with-extents fill-color
				   &key (x-align 0.5) (y-align 0.5) (angle 0)
				   (padding 2))
  "Fill a rectangle corresponding to text-with-extents and the given
alignment with fill-color."
  (with-slots (text x-bearing y-bearing width height) text-with-extents
    (move-to context x y)
    (let ((trans-matrix (get-trans-matrix context))
	  (x-rel (+ x-bearing (* width x-align) padding))
	  (y-rel (- (+ y-bearing (* height y-align)) padding))
	  (width (+ (* 2 padding) width))
	  (height (+ (* 2 padding) height)))
      (rotate context angle)
      (rel-move-to context (- 0 x-rel) (- 0 y-rel))
      (rel-line-to context width 0d0)
      (rel-line-to context 0d0 (- height))
      (rel-line-to context (- width) 0d0)
      (close-path context)
      (set-source-color context fill-color)
      (fill-path context)
      (set-trans-matrix context trans-matrix))
    (values width height)))

(defmethod aligned-text (context x y (text string)
			 &key (x-align 0.5) (y-align 0.5) (angle 0))
  "Show text aligned relative to (x,y).  Return width and height."
  (aligned-text context x y (add-text-extents context text)
   :x-align x-align :y-align y-align :angle angle))

;;;; auxiliary functions
;;;;

(defun calculate-function (function domain number-of-points
			   ignorable-conditions)
  "Calculate function at number-of-points points on domain.  If a
member of ignorable-conditions is encountered, the condition is
handled and the value is nil.  Return (values xs fxs)."
  (assert (>= number-of-points 2))
  (let ((xs (num-sequence :from (left domain)
			  :to (right domain)
			  :length number-of-points))
	(fxs (make-array (1+ number-of-points)))
	(caught-condition-p nil))
    (iter
      (for i :from 0)
      (for x :in-vector xs)
      (for fx := (handler-case (funcall function x)
		   (t (condition)
		     (if (member condition ignorable-conditions
				 :test (function typep))
			 nil
			 (error condition)))))
      (setf (aref fxs i) fx)
      (unless fx
	(setf caught-condition-p t)))
    (values xs fxs)))
