;;;; packagedef.lisp
;;;; Author: Elliot Penson

(ql:quickload "cl-ppcre")

(defpackage :knight-owl
  (:use :common-lisp :cl-ppcre)
  (:export :new-board :duplicate-board :print-board :get-square
           :make-move :valid-move-p :check-p :checkmate-p :+all-moves+
           :pgn-parse))
