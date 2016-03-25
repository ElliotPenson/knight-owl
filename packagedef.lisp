;;;; packagedef.lisp
;;;; Author: Elliot Penson

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defpackage :knight-owl
  (:use :common-lisp :cl-ppcre)
  (:export :new-board :duplicate-board :print-board :get-square
           :make-move :valid-move-p :check-p :checkmate-p :+all-moves+
           :pgn-parse))
