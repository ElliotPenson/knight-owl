;;;; movement.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defconstant +algebraic-move-re+
  "([RNBQK])?([a-h])?([1-8])?(x)?([a-h])([1-8])(=[RNBQK])?([#+])?"
  "A regular expression that describes chess movement in standard
   algebraic notation.")

(defconstant +algebraic-castle-re+
  "[0O]-[0O](-[0O])?([#+])?"
  "A regular expression that describes castling notation.")

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
                             check-sign)
                    (product '("0-0" "0-0-0") check-sign))))
  "A list of all possible string algebraic notation moves.")

(defun product (&rest lists)
  "Calculate the Cartesian product of the input lists."
  (when lists
    (loop for head in (first lists)
       for tails = (apply #'product (rest lists))
       if tails append (mapcar (lambda (tail) (cons head tail)) tails)
       else collect (list head))))

(defgeneric piece-moves (piece-char whitep capturep final-rank)
  (:documentation "Evaluate to a list of (file rank) scales."))

(defmethod piece-moves ((piece-char (eql #\R)) whitep capturep final-rank)
  "Provide all possible rook moves."
  '((0 1) (0 2) (0 3) (0 4) (0 5) (0 6) (0 7)
    (0 -1) (0 -2) (0 -3) (0 -4) (0 -5) (0 -6) (0 -7)
    (1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (7 0)
    (-1 0) (-2 0) (-3 0) (-4 0) (-5 0) (-6 0) (-7 0)))

(defmethod piece-moves ((piece-char (eql #\N)) whitep capturep final-rank)
  "Provide all possible knight moves."
  '((-1 -2) (1 -2) (-2 -1) (2 -1)
    (-2 1) (2 1) (-1 2) (1 2)))

(defmethod piece-moves ((piece-char (eql #\B)) whitep capturep final-rank)
  "Provide all possible bishop moves."
  '((-1 -1) (-2 -2) (-3 -3) (-4 -4) (-5 -5) (-6 -6) (-7 -7)
    (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7)
    (1 -1) (2 -2) (3 -3) (4 -4) (5 -5) (6 -6) (7 -7)
    (-1 1) (-2 2) (-3 3) (-4 4) (-5 5) (-6 6) (-7 7)))

(defmethod piece-moves ((piece-char (eql #\Q)) whitep capturep final-rank)
  "Provide all possible queen moves."
  (append (piece-moves #\R whitep capturep final-rank)
          (piece-moves #\B whitep capturep final-rank)))

(defmethod piece-moves ((piece-char (eql #\K)) whitep capturep final-rank)
  "Provide all possible king moves."
  '((-1 -1) (0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0)))

(defmethod piece-moves ((piece-char character) whitep capturep final-rank)
  "Provide all possible pawn moves (fall through case)."
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
  "Decide if a given piece may be promoted based on its type and position."
  (let ((final-rank (if whitep 0 +number-of-ranks+)))
    (and (char-equal piece #\P)
         (= rank final-rank))))

(defun being-attacked-p (file rank board white-attacker-p)
  "Search for any pieces that could capture a certain square."
  (some (lambda (piece-char)
          (move-origin file rank board piece-char white-attacker-p t))
        +piece-chars+))

(defun check-p (board whitep)
  "Determine if the king is being attacked."
  (destructuring-bind (king-file king-rank)
      (piece-location (if whitep 'w-k 'b-k) board)
    (being-attacked-p king-file king-rank board (not whitep))))

(defun checkmate-p (board whitep)
  "Determine if the king is both being attacked and cannot escape."
  (and (check-p board whitep)
       (notany (lambda (move)
                 (valid-move-p move board whitep))
               +all-moves+)))

(defun move-origin (final-file final-rank pre-board piece-char whitep
                    capturep &key origin-file origin-rank)
  "Select from a piece's possible moves the starting location based on a
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
  "Do the dirty work of parsing a move in algebraic notation. Evaluate to a
   list of (piece origin-file origin-rank capturep final-file final-rank
   promotion and check)."
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
  "Find out if the check sign (empty string, hash, or plus) matches the
   board's state after a move is performed."
  (let ((future-board (make-move move (duplicate-board board) whitep)))
    (and (not (check-p future-board whitep))
         (or (and (not check-sign)
                  (not (check-p future-board (not whitep))))
             (and (string= check-sign "+")
                  (check-p future-board (not whitep)))
             (and (string= check-sign "#")
                  (checkmate-p future-board (not whitep)))))))

(defun valid-castle-move-p (move board whitep)
  "Determine if the given castling move (0-0, 0-0-0, 0-0+, etc) is legal."
  (register-groups-bind (queensidep check)
      (+algebraic-castle-re+ move)
    (let ((rank (if whitep 7 0))
          (king-piece (if whitep 'w-k 'b-k))
          (rook-piece (if whitep 'w-r 'b-r))
          (initial-king-file 4)
          (initial-rook-file (if queensidep 0 7)))
      (and (eq (get-square board initial-king-file rank) king-piece)
           (eq (get-square board initial-rook-file rank) rook-piece)
           (not (pieces-between-p board initial-king-file rank
                                  initial-rook-file rank))
           (valid-check-sign-p check move board whitep)))))

(defun valid-single-piece-move-p (move board whitep)
  "Determine if the shift of one chess piece is legal on the given board."
  (when (destructure-move move)
    (destructuring-bind (piece origin-file origin-rank capturep
                               final-file final-rank promotion check)
        (destructure-move move)
      (let ((origin (move-origin final-file final-rank board piece
                                 whitep capturep
                                 :origin-file origin-file
                                 :origin-rank origin-rank))
            (destination-square (get-square board final-file final-rank)))
        (and origin ; i.e. a piece exists in the right location
             (if capturep
                 (if whitep
                     (black-piece-p destination-square)
                     (white-piece-p destination-square))
                 (null destination-square))
             (or (null promotion)
                 (can-promote-p piece final-rank whitep))
             (valid-check-sign-p check move board whitep))))))

(defun valid-move-p (move board whitep)
  "Determine if a move can legally be performed on a given board."
  (if (castling-move-p move)
      (valid-castle-move-p move board whitep)
      (valid-single-piece-move-p move board whitep)))

(defun move-from-to (board initial-file initial-rank final-file final-rank
                     &key new-piece)
  "Advance a single piece given an origin and a destination. The new-piece
   keyword parameter may be used to redefine a piece (promotion)."
  (setf (get-square board final-file final-rank)
        (if new-piece
            new-piece
            (get-square board initial-file initial-rank))
        (get-square board initial-file initial-rank)
        nil))

(defun castling-move-p (move)
  "Determine if a action in algebraic chess notation represents a castle
   or a simple, single piece move."
  (char= (char move 0) #\0))

(defun queenside-castle (board whitep)
  "Destructively modify the board to apply a queenside castle. The whitep
   parameter indicates which player is performing the move."
  (let ((rank (if whitep 7 0))
        (initial-king-file 4) (initial-rook-file 0)
        (final-king-file 2) (final-rook-file 3))
    (move-from-to board initial-king-file rank final-king-file rank)
    (move-from-to board initial-rook-file rank final-rook-file rank)))

(defun kingside-castle (board whitep)
  "Destructively modify the board to apply a kingside castle. The whitep
   parameter indicates which player is performing the move."
  (let ((rank (if whitep 7 0))
        (initial-king-file 4) (initial-rook-file 7)
        (final-king-file 6) (final-rook-file 5))
    (move-from-to board initial-king-file rank final-king-file rank)
    (move-from-to board initial-rook-file rank final-rook-file rank)))

(defun make-castle-move (move board whitep)
  "Simultaneously move the king and a rook, switching their sides. The
   move parameter must represent a castle (0-0, 0-0-0, 0-0+, etc)."
  (register-groups-bind (queensidep check)
      (+algebraic-castle-re+ move)
    (declare (ignore check))
    (if queensidep
        (queenside-castle board whitep)
        (kingside-castle board whitep)))
  board)

(defun make-one-piece-move (move board whitep)
  "Destructively shift one piece on a board. The move parameter must
   represent the movement of a single piece (e.g. e4, Qxd5)."
  (destructuring-bind (piece origin-file origin-rank capturep
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

(defun make-move (move board whitep)
  "Destructively modify the board for the given move. The move parameter
   should be a string in algebraic chess notation. PRECONDITION: The move
   is valid on the given board."
  (if (castling-move-p move)
      (make-castle-move move board whitep)
      (make-one-piece-move move board whitep)))
