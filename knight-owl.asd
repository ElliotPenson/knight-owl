;;;; knight-owl.asd

(defsystem :knight-owl
    :description "A chess library written in lisp."
    :author "Elliot Penson <elliotpenson@gmail.com"
    :licence "MIT License"
    :depends-on (:cl-ppcre)
    :components ((:file "packagedef")
                 (:file "pgn" :depends-on ("packagedef"))
                 (:file "board" :depends-on ("packagedef"))
                 (:file "movement" :depends-on ("pgn" "board"))))
