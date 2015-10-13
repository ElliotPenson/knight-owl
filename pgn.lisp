;;;; pgn.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defconstant +whitespace-chars+
  '(#\tab #\newline #\linefeed
    #\page #\return #\space))

(defun next-token (stream)
  "Provides the stream's next whitespace delimited token"
  (with-output-to-string (string-out)
    (flet ((read-to (chars)
             (loop for char = (read-char stream nil)
                while (and char (not (member char chars)))
                do (write-char char string-out))))
      (cond ((char= (peek-char t stream nil) #\")
             (read-char stream nil) ; skip first quote
             (read-to '(#\")))
            (t (read-to +whitespace-chars+))))))

(defun tag-parse (stream)
  "Converts PGN tag pairs into an alist"
  (loop for peek = (peek-char t stream nil)
     while (char= peek #\[)
     do (read-char stream nil)   ; skip [
     collect (cons (next-token stream)
                   (next-token stream))
     do (read-char stream nil))) ; skip ]

(defun movetext-parse (stream)
  "Reads moves in algebraic notation into a list"
  ;; Note: doesn't handle {} comments
  (loop for peek = (peek-char t stream nil)
     until (or (null peek)       ; eof
               (char= peek #\[)) ; tag (new game)
     if (char= peek #\;)
     do (read-line stream nil)
     else if (digit-char-p peek)
     do (next-token stream) ; skip numbers
     else collect (next-token stream)))

(defun pgn-parse (stream)
  "Parses a single chess game depicted in portable game notation"
  (list (tag-parse stream)
        (movetext-parse stream)))
