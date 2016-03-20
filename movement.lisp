;;;; movement.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defconstant +algebraic-move-re+
  "([RNBQK])?([a-h])?([1-8])?(x)?([a-h])([1-8])(=[RNBQK])?([#+])?"
  "A regular expression that describes chess movement in standard
   algebraic notation")

(defconstant +piece-chars+ '(#\R #\N #\B #\Q #\K #\P))

(defconstant +all-moves+
  (let ((files '("a" "b" "c" "d" "e" "f" "g" "h"))
        (ranks '("1" "2" "3" "4" "5" "6" "7" "8"))
        (capture-sign '("" "x"))
        (check-sign '("" "#" "+")))
    (mapcar (lambda (string-list)
              (format nil "~{~a~}" string-list))
            (append (product '("R" "N" "B" "Q" "K")
                             capture-sign files ranks check-sign)
                    (product files ranks capture-sign
                             '("" "=R" "=N" "=B" "=Q" "=K")
                             check-sign)))))
                 ;; (product '("0-0" "0-0-0") check-sign)

(defun product (&rest lists)
  "Calculates the Cartesian product of the input lists."
  (when lists
    (loop for head in (first lists)
       for tails = (apply #'product (rest lists))
       if tails append (mapcar (lambda (tail) (cons head tail)) tails)
       else collect (list head))))

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
  "True for a knight, false for any other piece."
  (char-equal piece-char #\N))

(defun can-promote-p (piece rank whitep)
  "Decides if a given piece may be promoted based on its type and position."
  (let ((final-rank (if whitep 0 +number-of-ranks+)))
    (and (char-equal piece #\P)
         (= rank final-rank))))

(defun being-attacked-p (file rank board white-attacker-p)
  "Searches for any pieces that could capture a certain square."
  (some (lambda (piece-char)
          (move-origin file rank board piece-char white-attacker-p t))
        +piece-chars+))

(defun check-p (board whitep)
  "Determines if the king is being attacked."
  (destructuring-bind (king-file king-rank)
      (piece-location (if whitep 'w-k 'b-k) board)
    (being-attacked-p king-file king-rank board (not whitep))))

(defun checkmate-p (board whitep)
  "Determines if the king is both being attacked and cannot escape."
  (and (check-p board whitep)
       (notany (lambda (move)
                 (valid-move-p move board whitep))
               +all-moves+)))

(defun move-origin (final-file final-rank pre-board piece-char whitep
                    capturep &key origin-file origin-rank)
  "Selects from a piece's possible moves the starting location based on a
   destination. The pre-board variable gives the layout before the move."
  (loop for (file-scale rank-scale)
     in (piece-moves piece-char whitep capturep final-rank)
     as candidate-file = (+ final-file file-scale)
     as candidate-rank = (+ final-rank rank-scale)
     when (and (in-board-p candidate-file candidate-rank)
               (eq (get-square pre-board candidate-file candidate-rank)
                   (piece-char->symbol piece-char whitep))
               (or (can-jump-p piece-char)
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
        (+algebraic-move-re+ (string-trim (append +whitespace-chars+
                                                  '(#\! #\?))
                                          string-move))
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

(defun valid-check-sign-p (check-sign move board whitep)
  "Finds out if the check sign (empty string, hash, or plus) matches the
   the board's state after a move is performed."
  (let ((future-board (make-move move (duplicate-board board) whitep)))
    (and (not (check-p future-board whitep))
         (or (and (not check-sign)
                  (not (check-p future-board (not whitep))))
             (and (string= check-sign "+")
                  (check-p future-board (not whitep)))
             (and (string= check-sign "#")
                  (checkmate-p future-board (not whitep)))))))

(defun valid-move-p (move board whitep)
  "Determines if a move can be performed on a given board."
  (when (destructure-move move)
    (destructuring-bind (piece origin-rank origin-file capturep
                               final-file final-rank promotion check)
        (destructure-move move)
      (let ((origin (move-origin final-file final-rank board piece
                                 whitep capturep
                                 :origin-file origin-file
                                 :origin-rank origin-rank))
            (destination-square (get-square board final-file final-rank)))
        (and origin
             (if capturep
                 (if whitep
                     (black-piece-p destination-square)
                     (white-piece-p destination-square))
                 (null destination-square))
             (or (null promotion)
                 (can-promote-p piece final-rank whitep))
             (valid-check-sign-p check move board whitep))))))

(defun move-from-to (board initial-file initial-rank final-file final-rank
                     &key new-piece)
  "Advances a single piece given an origin and a destination. The new-piece
   keyword parameter may be used to redefine a piece (promotion)."
  (setf (get-square board final-file final-rank)
        (if new-piece
            new-piece
            (get-square board initial-file initial-rank))
        (get-square board initial-file initial-rank)
        nil))

(defun make-move (move board whitep)
  "Destructively modifies the board for the given move. The move parameter
   should be a string in algebraic chess notation. PRECONDITION: The move is
   valid on the given board."
  (destructuring-bind (piece origin-rank origin-file capturep
                             final-file final-rank promotion check)
      (destructure-move move)
    (declare (ignore check))
    (destructuring-bind (origin-file origin-rank)
        (move-origin final-file final-rank board piece whitep capturep
                     :origin-file origin-file :origin-rank origin-rank)
      (move-from-to board origin-file origin-rank final-file final-rank
                    :new-piece (when promotion
                                 (piece-char->symbol promotion whitep)))))
  board)
