;;;; chess.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defconstant +number-of-files+ 8)

(defconstant +number-of-ranks+ 8)

(defconstant +initial-board+
  #2A((b-r b-n b-b b-q b-k b-b b-n b-r)
      (b-p b-p b-p b-p b-p b-p b-p b-p)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (w-p w-p w-p w-p w-p w-p w-p w-p)
      (w-r w-n w-b w-q w-k w-b w-n w-r))
  "Two-dimensional array of piece symbols in the form [bw]-[prnbqk]")

(defun new-board ()
  "Evaluates to a fresh 2D array with initial the chess position"
  (let ((board (make-array (list +number-of-files+
                                 +number-of-ranks+))))
    (dotimes (index (array-total-size board))
      (setf (row-major-aref board index)
            (row-major-aref +initial-board+ index)))
    board))

(defun get-square (board file rank)
  "Accesses a chess board x/y location"
  (aref board rank file))

(defun (setf get-square) (value board file rank)
  "Sets a chess board x/y location"
  (setf (aref board rank file) value))
