;;;; movement.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defconstant +algebraic-move-re+
  "([RNBQK])?([a-h])?([1-8])?(x)?([a-h])([1-8])(=[RNBQK])?([#+])?"
  "A regular expression that describes chess movement in standard
   algebraic notation")

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

(defun can-jump-p (piece-char)
  "Determines if a piece can move without concern for pieces on its path"
  (char-equal piece-char #\N))

(defun move-origin (final-file final-rank pre-board piece-letter whitep
                    capturep &key origin-file origin-rank)
  "Selects from a piece's possible moves the starting location based on a
   destination. The pre-board variable gives the layout before the move."
  (loop for (file-scale rank-scale)
     in (piece-moves piece-letter whitep capturep final-rank)
     as candidate-file = (+ final-file file-scale)
     as candidate-rank = (+ final-rank rank-scale)
     when (and (in-board-p candidate-file candidate-rank)
               (eq (get-square pre-board candidate-file candidate-rank)
                   (piece-char->symbol piece-letter whitep))
               (or (can-jump-p piece-letter)
                   (not (pieces-between-p pre-board final-file final-rank
                                          candidate-file candidate-rank)))
               (or (null origin-file) (= origin-file candidate-file))
               (or (null origin-rank) (= origin-rank candidate-rank)))
     return (list candidate-file candidate-rank)))

(defun destructure-move (string-move)
  "Does the dirty work of parsing a move in algebraic notation. Evaluates to a
   list of (piece origin-file origin-rank capturep final-file final-rank
   promotion and check)"
  (flet ((string->char (string)
           (char string 0)))
    (register-groups-bind (piece origin-file origin-rank capturep
                                 final-file final-rank promotion check)
        (+algebraic-move-re+ string-move)
      (list (if piece (string->char piece) #\P)
            (when origin-file
              (file->index (string->char origin-file)))
            (when origin-rank
              (rank->index (string->char origin-rank)))
            capturep
            (file->index (string->char final-file))
            (rank->index (string->char final-rank))
            (when promotion
              ;; choose piece part of '=piece'
              (char promotion 1))
            check))))

(defun can-promote-p (piece rank whitep)
  (let ((final-rank (if whitep 0 +number-of-ranks+)))
    (and (char-equal piece #\P)
         (= rank final-rank))))

(defun valid-move-p (move board whitep)
  "Determines if a move can be performed on a given board."
  ;; TODO: castling, check/checkmate
  (destructuring-bind (piece origin-rank origin-file capturep
                             final-file final-rank promotion check)
      (destructure-move move)
    (declare (ignore check))
    (let ((origin (move-origin final-file final-rank board piece
                               whitep capturep
                               :origin-file origin-file
                               :origin-rank origin-rank))
          (destination-square (get-square board final-file final-rank)))
      (and origin
           (if capturep
               destination-square
               (null destination-square))
           (or (null promotion)
               (can-promote-p piece final-rank whitep))))))

(defun make-move (move board whitep)
  "Destructively modifies the board for the given move. The move parameter
   should be a string in algebraic chess notation."
  ;; TODO: castling, check/checkmate
  (destructuring-bind (piece origin-rank origin-file capturep
                             final-file final-rank promotion check)
      (destructure-move move)
    (declare (ignore check))
    (destructuring-bind (origin-file origin-rank)
        (move-origin final-file final-rank board piece whitep capturep
                     :origin-file origin-file :origin-rank origin-rank)
      (setf (get-square board final-file final-rank)
            (if promotion
                (piece-char->symbol promotion whitep)
                (get-square board origin-file origin-rank))
            (get-square board origin-file origin-rank)
            nil)))
  board)
