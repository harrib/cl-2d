(in-package :cl-2d)

(declaim (optimize (debug 3)))

;;;;  Axis
;;;;
;;;;  Axis creation is the most painful mess in plotting.  The
;;;;  following is some leftover from an earlier incarnation of cl-2d,
;;;;  which I am going to rework, one of these days.  Currently the
;;;;  code qualifies for The Evil Spaghetti Award.
;;;;
;;;;  How it should work, ideally:
;;;;
;;;;  - axis plotting should be based on mapping (linear, log)
;;;;  - better guessing on number and placement of labels with an
;;;;    adaptive algorithm
;;;;  - but at the same time should allow arbitrary ticks _and_ labels
;;;;    (also, labels could be empty too)
;;;;  IDEA: user passes a function/class, which expands on demand.

(defclass axis () 
  ((interval :initarg :interval :documentation "interval")
   (positions :initarg :positions :documentation "positions of tickmarks")
   (labels :initarg :labels :documentation "labels of tickmarks")
   (title :initarg :title :documentation "axis title")))

(defclass standard-axis ()
  ((interval :initarg :interval :documentation "interval")
   (title :initarg :title :documentation "axis title")
   (max-exponent :initarg :max-exponent :initform 6)
   (min-exponent :initarg :min-exponent :initform -6)
   (engineering-exponent-p :initarg :engineering-exponent :initform t)   
   (initial-n :initarg :initial-n :initform 10)
   (density :initarg :density :initform 0.9)
   (spacing :initarg :spacing :initform 2 
	    :documentation "spacing between labels")
   (round-to :initarg :round-to :initform '(1 2 5)))
  (:documentation "A standard axis is a set of parameter that
flexibly generate an axis when the frame is known."))

(defun format-axis-positions (positions exponent 
			      &key 
			      (max-exponent 6)
			      (min-exponent -6)
			      (engineering-exponent-p t))
  "Format axis positions to make labels.  If the exponent is above
max-exponent or below min-exponent, use scientific notation.  If
engineering-exponent-p, use an exponent which is a multiple of 3 and is
smaller than or equal to the original exponent.

Only for use with positions and exponent generated by
pretty-axis-positions."
  (assert (<= min-exponent 0 max-exponent))
  (multiple-value-bind (significant-digits exponent)
      (cond
	((and (<= 0 exponent) (< exponent max-exponent))
	 ;; integer, no exponent
	 (values 0 nil))
	((< min-exponent exponent 0)
	 ;; fraction, no exponent
	 (values (- exponent) nil))
	(t
	 ;; exponent, may be corrected downwards, integer mantissa
	 (let ((correction (if engineering-exponent-p
			       (mod exponent 3)
			       0)))
	   (values 0 (- exponent correction)))))
;;    (format t "significant digits ~a ~%" significant-digits)
    (let ((divisor (if exponent (expt 10 exponent) 1)))
      (iter
	(for position :in-vector positions)
	(collecting
	  (let* ((mantissa (/ position divisor))
		 (mantissa-string (if (plusp significant-digits)
				      (format nil "~,v,,f" significant-digits
					      mantissa)
				      (format nil "~d" (truncate mantissa)))))
	    (if exponent
		(format nil "~ae~d" mantissa-string exponent)
		mantissa-string)))))))
  
(defun fit-axis-labels (lower upper max-size
			&key 
			(initial-n 10) (spacing 2)
			(max-exponent 6) (min-exponent -6)
			(engineering-exponent-p t) (round-to '(1 2 5)))
  "Try to make an axis between lower and upper so that the total
number of characters in labels + inter-label spacing (spacing) is
smaller than width.  Starts with initial-n labels and decreases the
number of labels until they fit.

For max-exponent, min-exponent and engineering-exponent-p, see
format-axis-positions.  For round-to, see pretty-axis-positions."
  (assert (plusp initial-n))
  (iter
    (for n :from initial-n :downto 1)
    (bind (((values positions exponent)
	    (pretty-axis-positions lower upper n :round-to round-to))
	   (labels (format-axis-positions positions exponent
					  :max-exponent max-exponent
					  :min-exponent min-exponent
					  :engineering-exponent-p
					  engineering-exponent-p))
	   (size (+ (reduce #'+ labels :key #'length)
		  (* (1- (length positions)) spacing))))
      (when (or (<= size max-size) (= 1 n))
;; 	(format t "fitting axis labels: size=~a max-size=~a n=~a~%"
;; 		size max-size n)
;; 	(format t "labels: ~a" labels)
	(return-from fit-axis-labels
	  (values positions labels))))))

(defun expand-standard-axis (standard-axis max-size char-width char-height
			     perpendicular-labels-p)
  "Expand a standard axis into a frame of given height.  Return an
axis.  max-size is the size to be filled, char-width and char-height
are maximum character widths/heights (all 3 in the same units)."
  (with-slots (interval title max-exponent min-exponent
		     engineering-exponent-p initial-n density spacing
		     round-to) standard-axis
    (let ((left (left interval))
	  (right (right interval)))
      (if (= left right)
	  (make-instance 
	   'axis :interval interval :positions (vector left)
	   :labels (list (format nil "~f" left)) :title title)
	  (bind ((effective-size (* density max-size))
		 ((values positions labels)
		  (if perpendicular-labels-p
		      ;; perpendicular labels, can calculate total number
		      (bind (((values positions exponent)
			      (pretty-axis-positions left right 
			       (max 1
				    (floor
				     (/ effective-size 
					(* char-height (1+ spacing)))))
			       :round-to round-to))
			     (labels (format-axis-positions 
				      positions exponent
				      :max-exponent max-exponent
				      :min-exponent min-exponent
				      :engineering-exponent-p
				      engineering-exponent-p)))
			(values positions labels))
		      ;; parallel labels, will fit axis by trial and error
		      (fit-axis-labels left right (/ effective-size char-width)
				       :initial-n initial-n :spacing spacing
				       :max-exponent max-exponent
				       :min-exponent min-exponent
				       :engineering-exponent-p engineering-exponent-p
				       :round-to round-to))))
	    (make-instance
	     'axis :interval interval :positions positions
	     :labels labels
	     :title title))))))

(defun calculate-label-alignment (label-orientation)
  "Calculate angle, x-align and y-align for a given label orientation."
  (ecase label-orientation
    (parallel (values (/ pi -2) 0.5 0))
    (parallel-opposite (values (/ pi 2) 0.5 1))
    (perpendicular (values 0 0 0.5))
    (perpendicular-opposite (values pi 1 0.5))))

(defun axis-primitive (context width height axis axis-style
		       label-orientation reverse-title-p)
  "This is a generalized axis drawing function, not to be called
directly in normal cases.  It draws a vertical right axis in
context to fill width and height, starting from (0,0).
label-orientation can be parallel, parallel-opposite,
perpendicular, perpendicular-opposite.  If reverse-title-p is
true, the title will be rotated by 180 degrees.  The title is
always parallel to the axis.

Axis can be a standard axis, in that case, it is expanded.

Suggested use: set up a rotation and translation using cairo to
position this axis in the appropriate place.  Do not use scaling,
it will mess up line and font widths."
  (if (typep axis 'standard-axis)
      (bind (((values nil nil char-width char-height) (text-extents context "0")))
	(setf axis
	      (expand-standard-axis
	       axis height char-width char-height
	       (member label-orientation
		       '(perpendicular perpendicular-opposite))))))
  (with-slots (interval positions labels title) axis
    (with-slots (axis-padding font-style line-style tick-length tick-padding
			      title-padding) axis-style
      (with-sync-lock (context)
	;; axis line
	(set-style context line-style)
	(set-style context font-style)
	(segment context axis-padding 0 axis-padding height)
	;; axis tickmarks and labels
	(bind (((values interval title-y-align title-angle) 
		(if reverse-title-p
		    (values (flip-interval interval) 0 (/ pi 2))
		    (values interval 1 (/ pi -2))))
	       (mapping (make-instance 'linear-mapping
				       :domain interval
				       :range (make-interval height 0) :snap-p nil))
	       ((values angle x-align y-align)
		(calculate-label-alignment label-orientation))
	       (tick-start axis-padding)
	       (tick-end (+ tick-start tick-length))
	       (label-x (+ tick-end tick-padding)))
	  (iter
	    (for label in labels)
	    (for position :in-vector positions)
	    (for y := (map-coordinate mapping position))
	    (segment context tick-start y tick-end y)
	    (aligned-text context label-x y label
			  :x-align x-align :y-align y-align
			  :angle angle))
	  ;; draw axis title
	  (aligned-text context (- width title-padding) (/ height 2) title
			:y-align title-y-align
			:angle title-angle))))))

(defun right-axis (frame axis &optional (axis-style *default-axis-style*))
  "Draw axis as a right axis in frame with given style."
  (with-slots (horizontal-interval vertical-interval context) frame
    (translate context (left horizontal-interval) (right vertical-interval))
    (axis-primitive context (width frame) (height frame) axis axis-style
		    (ecase (label-orientation axis-style)
		      ((horizontal perpendicular) 'perpendicular)
		      ((vertical parallel) 'parallel))
		    nil)
    (reset-trans-matrix context)))
      
(defun left-axis (frame axis &optional (axis-style *default-axis-style*))
  "Draw axis as a left axis in frame with given style."
  (with-slots (horizontal-interval vertical-interval context) frame
    (translate context (right horizontal-interval) (left vertical-interval))
    (rotate context pi)
    (axis-primitive context (width frame) (height frame) axis axis-style
		     (ecase (label-orientation axis-style)
		       ((horizontal perpendicular) 'perpendicular-opposite)
		       ((vertical parallel) 'parallel-opposite))
		     t)
    (reset-trans-matrix context)))

(defun top-axis (frame axis &optional (axis-style *default-axis-style*))
  "Draw axis as a top axis in frame with given style."
  (with-slots (horizontal-interval vertical-interval  context) frame
    (translate context (left horizontal-interval) (left vertical-interval))
    (rotate context (/ pi -2))
    (axis-primitive context (height frame) (width frame) axis axis-style
		    (ecase (label-orientation axis-style)
		      ((horizontal parallel) 'parallel-opposite)
		      ((vertical perpendicular) 'perpendicular-opposite))
		    t)
    (reset-trans-matrix context)))

(defun bottom-axis (frame axis &optional (axis-style *default-axis-style*))
  "Draw axis as a bottom axis in frame with given style."
  (with-slots (horizontal-interval vertical-interval context) frame
    (translate context (right horizontal-interval) (right vertical-interval))
    (rotate context (/ pi 2))
    (axis-primitive context (height frame) (width frame) axis axis-style
		    (ecase (label-orientation axis-style)
		      ((horizontal parallel) 'parallel)
		      ((vertical perpendicular) 'perpendicular))
		    nil)
    (reset-trans-matrix context)))
