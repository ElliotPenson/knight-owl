;;;; movement.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defgeneric piece-moves (piece-char whitep capturep final-rank)
  (:documentation "Evaluates to a list of (file rank) scales"))

(defmethod piece-moves ((piece-char (eql #\R)) whitep capturep final-rank)
  "Provides all possible rook moves"
  '((0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7)
    (0 -1) (0 -2) (0 -3) (0 -4) (0 -5) (0 -6) (0 -7)
    (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0)
    (-1 0) (-2 0) (-3 0) (-4 0) (-5 0) (-6 0) (-7 0)))

(defmethod piece-moves ((piece-char (eql #\N)) whitep capturep final-rank)
  "Provides all possible knight moves"
  '((-1 -2) (1 -2) (-2 -1) (2 -1)
    (-2 1) (2 1) (-1 2) (1 2)))

(defmethod piece-moves ((piece-char (eql #\B)) whitep capturep final-rank)
  "Provides all possible bishop moves"
  '((-1 -1) (-2 -2) (-3 -3) (-4 -4) (-5 -5) (-6 -6) (-7 -7)
    (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7)
    (1 -1) (2 -2) (3 -3) (4 -4) (5 -5) (6 -6) (7 -7)
    (-1 1) (-2 2) (-3 3) (-4 4) (-5 5) (-6 6) (-7 7)))

(defmethod piece-moves ((piece-char (eql #\Q)) whitep capturep final-rank)
  "Provides all possible queen moves"
  (append (piece-moves #\R whitep capturep final-rank)
          (piece-moves #\B whitep capturep final-rank)))

(defmethod piece-moves ((piece-char (eql #\K)) whitep capturep final-rank)
  "Provides all possible king moves"
  '((-1 -1) (0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0)))

(defmethod piece-moves ((piece-char character) whitep capturep final-rank)
  "Provides all possible pawn moves (fall through case)"
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

(defun can-jump-p (piece-letter)
  "Determines if a piece can move without concern for pieces on its path"
  (char-equal piece-letter #\N))

(defun move-origin (final-file final-rank pre-board piece-letter whitep
                    capturep &key origin-file origin-rank)
  "Selects from a piece's possible moves the starting location based on a
   destination. The pre-board variable gives the layout before the move."
  (find-if (lambda (possible-move)
             (destructuring-bind (file-scale rank-scale) possible-move
               (let ((candidate-file (+ final-file file-scale))
                     (candidate-rank (+ final-rank rank-scale)))
                 (and (in-board-p candidate-file candidate-rank)
                      (eq (get-square pre-board candidate-file candidate-rank)
                          (piece-char->symbol piece-letter whitep))
                      (or (can-jump-p piece-letter)
                          (not (pieces-between-p pre-board final-file final-rank
                                                 candidate-file candidate-rank)))
                      (or (null origin-file) (= origin-file candidate-file))
                      (or (null origin-rank) (= origin-rank candidate-rank))))))
           (piece-moves piece-letter whitep capturep final-rank)))
