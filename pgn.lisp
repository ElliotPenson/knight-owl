;;;; pgn.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(defconstant +whitespace-chars+
  '(#\tab #\newline #\linefeed
    #\page #\return #\space))

(defun next-token (stream)
  "Provide the stream's next whitespace delimited token."
  (with-output-to-string (string-out)
    (flet ((read-to (chars)
             (loop for char = (read-char stream nil)
                while (and char (not (member char chars)))
                do (write-char char string-out))))
      (cond ((char= (peek-char t stream nil) #\")
             (read-char stream nil) ; skip first quote
             (read-to '(#\")))
            (t (read-to +whitespace-chars+))))))

(defun remove-comment (stream)
  "Clear a line in the stream if it begins with a semicolon or a section of
   text if it's surrounded with curly brackets."
  (let* ((peek (peek-char t stream nil))
         (close-char (ecase peek
                       (#\; #\newline)
                       (#\{ #\}))))
    (loop for char = (read-char stream nil)
       until (char= char close-char))))

(defun tag-parse (stream)
  "Convert PGN tag pairs into an alist."
  (loop for peek = (peek-char t stream nil)
     while (char= peek #\[)
     do (read-char stream nil)   ; skip [
     collect (cons (next-token stream)
                   (next-token stream))
     do (read-char stream nil))) ; skip ]

(defun movetext-parse (stream)
  "Read moves in algebraic notation into a list."
  (loop for peek = (peek-char t stream nil)
     until (or (null peek)       ; eof
               (char= peek #\[)) ; tag (new game)
     if (member peek '(#\; #\{)) ; comment
     do (remove-comment stream)
     else if (digit-char-p peek) ; move number
     do (next-token stream)
     else collect (next-token stream)))

(defun pgn-parse (stream)
  "Parse a single chess game depicted in portable game notation."
  (when (peek-char t stream nil nil)
    (list (tag-parse stream)
          (movetext-parse stream))))
