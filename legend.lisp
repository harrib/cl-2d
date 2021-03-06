(in-package :cl-2d)

(defun horizontal-legend (frame style-text-pairs &optional
			       (horizontal-legend-style
				*default-horizontal-legend-style*))
  "Using a flat list of styles and texts, draw a legend in frame."
  (with-slots (horizontal-interval vertical-interval context) frame
    (with-slots (font-style left-padding style-left-padding
			    style-text-padding text-right-padding
			    line-length) horizontal-legend-style
      (set-style context font-style)
      (let* ((vertical-center (snap* (interval-midpoint vertical-interval)
				     :half
				     context))
	     (text-top (- vertical-center 
			  (half (capital-letter-height context))))
	     (horizontal-position (+ (left horizontal-interval) left-padding)))
	(iter
	  (generate elt :in style-text-pairs)
	  (for style := (next elt))
	  (for text := (next elt))
	  ;; draw the style
	  (incf horizontal-position style-left-padding)
	  (etypecase style
	    (line-style
	     (set-style context style)
	     (segment context horizontal-position vertical-center
		      (+ horizontal-position line-length) vertical-center)
	     (incf horizontal-position (+ line-length style-text-padding))))
	  ;; draw the text
	  (let ((width (aligned-text context horizontal-position
				     text-top text
				     :x-align 0 :y-align 0)))
	    (incf horizontal-position (+ width text-right-padding)))))))
  (values))

(defun image-legend (frame color-mapping &key
		     (image-legend-style *default-image-legend-style*)
		     (z-title ""))
  "Draw an image legend in frame, with given color-mapping and style."
  (with-slots (width number-of-colors padding axis-label-distance)
      image-legend-style
    (bind ((internal-frame (pad-frame frame padding))
	   (#(gradient-frame axis-frame) 
	     (split-frame-horizontally internal-frame width (spacer))))
      (with-slots (domain color-function) color-mapping
	(with-slots ((lower left) (upper right)) domain
	  (if (zero-interval-p domain)
	      ;; homogenous rectangle for denegerate 
	      (fill-with-color gradient-frame
			       (funcall color-function lower))
	      ;; draw color gradient
	      (with-slots (horizontal-interval vertical-interval context)
		  gradient-frame
		(with-slots ((left-edge left) (right-edge right))
		    horizontal-interval
		  (with-sync-lock (context)
		    (let ((mapping (make-instance 'linear-mapping
						  :domain domain
						  :range vertical-interval)))
		      (iter
			(for i from 0 below number-of-colors)
			(for left := (convex-combination 
				      lower upper
				      (/ i number-of-colors)))
			(for right := (convex-combination 
				       lower upper
				       (/ (1+ i) number-of-colors)))
			(filled-rectangle
			 context
			 (funcall color-function 
				  (/ (+ left right) 2))
			 left-edge (map-coordinate mapping left :int)
			 right-edge (map-coordinate mapping right :int))))))))
	  ;; draw axis
	  (right-axis axis-frame 
		      (make-instance 'standard-axis
				     :interval (make-interval lower upper)
				     :title z-title)
		      (slot-value image-legend-style 'axis-style)))))))
