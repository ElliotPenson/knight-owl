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
  "Two-dimensional array of piece symbols in the form [bw]-[prnbqk]")

(defun new-board ()
  "Evaluates to a fresh 2D array with initial the chess position"
  (let ((board (make-array (list +number-of-files+
                                 +number-of-ranks+))))
    (dotimes (index (array-total-size board))
      (setf (row-major-aref board index)
            (row-major-aref *initial-board* index)))
    board))

(defun get-square (board file rank)
  "Accesses a chessboard x/y location"
  (aref board rank file))

(defun (setf get-square) (value board file rank)
  "Sets a chessboard x/y location"
  (setf (aref board rank file) value))

(defun in-board-p (file rank)
  "Determines if the given position is within the bounds of a chess board"
  (and (not (minusp file))
       (not (minusp rank))
       (< file +number-of-files+)
       (< rank +number-of-ranks+)))

(defun enclosed-by (start end)
  "Produces a list of integers. Both start and end are exclusive"
  (let ((advance-fn (if (< start end) #'1+ #'1-)))
    (unless (= (funcall advance-fn start) end)
      (cons (funcall advance-fn start)
            (enclosed-by (funcall advance-fn start) end)))))

(defun pieces-between-p (board file1 rank1 file2 rank2)
  "Decides if there's a clear path from one position to the next"
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
  "Converts an algebraic chess notation file into an x-position"
  (ecase file-char
    (#\a 0) (#\b 1) (#\c 2) (#\d 3) (#\e 4) (#\f 5) (#\g 6) (#\h 7)))

(defun rank->index (rank-char)
  "Converts an algebraic chess notation rank into a y-position"
  (- +number-of-ranks+ (digit-char-p rank-char)))

(defun piece-char->symbol (piece-char whitep)
  "Evaluates to the symbol piece name (e.g. 'w-r) that represents the given
   piece character (e.g. #\R)"
  (cond ((member piece-char '(#\R #\N #\B #\Q #\K))
         (intern (format nil "~:[B~;W~]-~a" whitep piece-char)))
        (whitep 'w-p) (t 'b-p)))

(defun piece-symbol->unicode (symbol)
  "Converts a symbol piece name (e.g. 'w-r) into a unicode string."
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
  "Write a 2D chessboard array to a stream in a pretty fashion"
  (dotimes (rank +number-of-ranks+)
    (format stream "~a " (- +number-of-ranks+ rank))
    (dotimes (file +number-of-files+)
      (let ((piece (get-square board file rank)))
        (format stream "|~a|" (piece-symbol->unicode piece))))
    (fresh-line))
  (format stream " ~{  ~a~}~%" '("A" "B" "C" "D" "E" "F" "G" "H")))
