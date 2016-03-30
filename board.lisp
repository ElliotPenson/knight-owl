;;;; board.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defconstant +number-of-files+ 8)

(defconstant +number-of-ranks+ 8)

(defparameter *initial-board*
  #2A((b-r b-n b-b b-q b-k b-b b-n b-r)
      (b-p b-p b-p b-p b-p b-p b-p b-p)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (nil nil nil nil nil nil nil nil)
      (w-p w-p w-p w-p w-p w-p w-p w-p)
      (w-r w-n w-b w-q w-k w-b w-n w-r))
  "Two-dimensional array of piece symbols in the form [bw]-[prnbqk].")

(defun new-board ()
  "Evaluate to a fresh 2D array with initial the chess position."
  (duplicate-board *initial-board*))

(defun duplicate-board (board)
  "Create a new two-dimensional array identical to the input board."
  (let ((new-board (make-array (list +number-of-files+
                                     +number-of-ranks+))))
    (dotimes (index (array-total-size board))
      (setf (row-major-aref new-board index)
            (row-major-aref board index)))
    new-board))

(defun get-square (board file rank)
  "Access a chess board x/y location."
  (aref board rank file))

(defun (setf get-square) (value board file rank)
  "Set a chess board x/y location."
  (setf (aref board rank file) value))

(defun in-board-p (file rank)
  "Determine if the given position is within the bounds of a chess board."
  (and (not (minusp file))
       (not (minusp rank))
       (< file +number-of-files+)
       (< rank +number-of-ranks+)))

(defun enclosed-by (start end)
  "Produce a list of integers. Both start and end are exclusive."
  (let ((advance-fn (if (< start end) #'1+ #'1-)))
    (unless (= (funcall advance-fn start) end)
      (cons (funcall advance-fn start)
            (enclosed-by (funcall advance-fn start) end)))))

(defun pieces-between-p (board file1 rank1 file2 rank2)
  "Decide if there's a clear path from one position to the next."
  (flet ((takenp (file rank)
           (get-square board file rank)))
    (cond ((= (abs (- file1 file2)) ; diagonal
              (abs (- rank1 rank2)))
           (loop for file in (enclosed-by file1 file2)
              for rank in (enclosed-by rank1 rank2)
              thereis (takenp file rank)))
          ((= file1 file2)          ; same file
           (loop for rank in (enclosed-by rank1 rank2)
              thereis (takenp file1 rank)))
          ((= rank1 rank2)          ; same rank
           (loop for file in (enclosed-by file1 file2)
              thereis (takenp file rank1))))))

(defun file->index (file-char)
  "Convert an algebraic chess notation file into an x-position."
  (ecase file-char
    ((#\a #\A) 0) ((#\b #\B) 1) ((#\c #\C) 2) ((#\d #\D) 3)
    ((#\e #\E) 4) ((#\f #\F) 5) ((#\g #\G) 6) ((#\h #\H) 7)))

(defun rank->index (rank-char)
  "Convert an algebraic chess notation rank into a y-position."
  (- +number-of-ranks+ (digit-char-p rank-char)))

(defun piece-char->symbol (piece-char whitep)
  "Evaluate to the symbol piece name (e.g. 'w-r) that represents the given
   piece character (e.g. #\R)."
  (intern (cond ((member piece-char '(#\R #\N #\B #\Q #\K))
                 (format nil "~:[B~;W~]-~a" whitep piece-char))
                (whitep "W-P") (t "B-P"))
          (find-package :knight-owl)))

(defun piece-location (piece-symbol board)
  "Provide the first discovered (file rank) of the given piece. Evaluate
   to nil if none can be found"
  (dotimes (file +number-of-files+)
    (dotimes (rank +number-of-ranks+)
      (when (eql (get-square board file rank)
                 piece-symbol)
        (return-from piece-location
          (list file rank))))))

(defun white-piece-p (piece-symbol)
  (member piece-symbol '(w-p w-r w-n w-b w-q w-k)))

(defun black-piece-p (piece-symbol)
  (member piece-symbol '(b-p b-r b-n b-b b-q b-k)))

(defun piece-symbol->unicode (symbol)
  "Convert a symbol piece name (e.g. 'w-r) into a unicode string."
  (case symbol
    (b-p #\BLACK_CHESS_PAWN)
    (b-r #\BLACK_CHESS_ROOK)
    (b-n #\BLACK_CHESS_KNIGHT)
    (b-b #\BLACK_CHESS_BISHOP)
    (b-q #\BLACK_CHESS_QUEEN)
    (b-k #\BLACK_CHESS_KING)
    (w-p #\WHITE_CHESS_PAWN)
    (w-r #\WHITE_CHESS_ROOK)
    (w-n #\WHITE_CHESS_KNIGHT)
    (w-b #\WHITE_CHESS_BISHOP)
    (w-q #\WHITE_CHESS_QUEEN)
    (w-k #\WHITE_CHESS_KING)
    (otherwise #\LOW_LINE)))

(defun print-board (board &key (stream t))
  "Write a 2D chess board array to a stream in a pretty fashion."
  (dotimes (rank +number-of-ranks+)
    (format stream "~a " (- +number-of-ranks+ rank))
    (dotimes (file +number-of-files+)
      (let ((piece (get-square board file rank)))
        (format stream "|~a|" (piece-symbol->unicode piece))))
    (fresh-line))
  (format stream " ~{  ~a~}~%" '("A" "B" "C" "D" "E" "F" "G" "H")))
