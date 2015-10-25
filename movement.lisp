;;;; movement.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defconstant +rook-moves+
  '((0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7)
    (0 -1) (0 -2) (0 -3) (0 -4) (0 -5) (0 -6) (0 -7)
    (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0)
    (-1 0) (-2 0) (-3 0) (-4 0) (-5 0) (-6 0) (-7 0))
  "list of (file rank) scales for rooks")

(defconstant +knight-moves+
  '((-1 -2) (1 -2) (-2 -1) (2 -1)
    (-2 1) (2 1) (-1 2) (1 2))
  "list of (file rank) scales for knights")

(defconstant +bishop-moves+
  '((-1 -1) (-2 -2) (-3 -3) (-4 -4) (-5 -5) (-6 -6) (-7 -7)
    (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7)
    (1 -1) (2 -2) (3 -3) (4 -4) (5 -5) (6 -6) (7 -7)
    (-1 1) (-2 2) (-3 3) (-4 4) (-5 5) (-6 6) (-7 7))
  "list of (file rank) scales for bishops")

(defconstant +queen-moves+
  (append +rook-moves+ +bishop-moves+)
  "list of (file rank) scales for queens")

(defconstant +king-moves+
  '((-1 -1) (0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0))
  "list of (file rank) scales for kings")

(defun pawn-moves (whitep capturep final-rank)
  "Provides a list of (file rank) scales for pawns. Since a pawn's
   potential moves depend on color and position, a function is
   required."
  ;; TODO en passant
  (if whitep
      (if capturep
          '((1 1) (-1 1))
          (if (= final-rank 4)
              '((0 1) (0 2))
              '((0 1))))
      (if capturep
          '((-1 -1) (1 -1))
          (if (= final-rank 3)
              '((0 -1) (0 -2))
              '((0 -1))))))

