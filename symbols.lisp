(in-package :cl-2d)

;;;;  functions for plotting symbols (circles, dots, rectangles, etc)
;;;;
;;;;  The general syntax is
;;;;
;;;;  (symbol-drawing-function x y size color context)
;;;;
;;;;  where x and y are the middle of the symbol, size is the length
;;;;  of an edge othe square that would contain the symbol, which is
;;;;  drawn with color in context.

(defun symbol-filled-circle (context x y size color)
  "Filled circle symbol."
  (let ((radius (half size)))
    (filled-circle context x y radius color)))

(defun symbol-filled-square (context x y size color)
  "Filled square symbol."
  (let ((halfsize (half size)))
    (rectangle context (- x halfsize) (- y halfsize) size size)
    (set-source-color context color)
    (fill-path context)))

(defun symbol-hollow-circle (context x y size color)
  "Hollow circle symbol."
  (let ((radius (half size)))
    (circle-path context x y radius)
    (set-source-color context color)
    (stroke context)))

(defun symbol-hollow-square (context x y size color)
  "Hollow square symbol."
  (let ((halfsize (half size)))
    (rectangle context (- x halfsize) (- y halfsize) size size)
    (set-source-color context color)
    (stroke context)))

;;;;
;;;;  functions that assign a size or a color to a particular weight
;;;;
;;;;  All of them take one argument and return an appropriate color or
;;;;  size.

(defun proportional-size (x)
  "Return a function that multiplies x by the square root of its argument."
  (lambda (s)
    (* x (sqrt s))))
