;;;; unit-tests.lisp
;;;; Author: Elliot Penson

(in-package :knight-owl)

(ql:quickload "fiveam")
(use-package :fiveam)

(def-suite knight-owl-suite)
(in-suite knight-owl-suite)

(test file->index
  (is (= (file->index #\a) 0))
  (is (= (file->index #\b) 1))
  (is (= (file->index #\c) 2))
  (is (= (file->index #\d) 3))
  (is (= (file->index #\e) 4))
  (is (= (file->index #\f) 5))
  (is (= (file->index #\g) 6))
  (is (= (file->index #\h) 7)))

(test rank->index
  (is (= (rank->index #\1) 7))
  (is (= (rank->index #\2) 6))
  (is (= (rank->index #\3) 5))
  (is (= (rank->index #\4) 4))
  (is (= (rank->index #\5) 3))
  (is (= (rank->index #\6) 2))
  (is (= (rank->index #\7) 1))
  (is (= (rank->index #\8) 0)))

(test enclosed-by
  (is (equal (enclosed-by 1 5)
             '(2 3 4)))
  (is (equal (enclosed-by 0 5)
             '(1 2 3 4)))
  (is (equal (enclosed-by -5 5)
             '(-4 -3 -2 -1 0 1 2 3 4)))
  (is (equal (enclosed-by -5 0)
             '(-4 -3 -2 -1)))
  (is (equalp (enclosed-by -5 -1)
              '(-4 -3 -2)))
  (is (equalp (enclosed-by 5 1)
              '(4 3 2)))
  (is (equalp (enclosed-by 5 0)
              '(4 3 2 1)))
  (is (equalp (enclosed-by 5 -5)
              '(4 3 2 1 0 -1 -2 -3 -4)))
  (is (equalp (enclosed-by 0 -5)
              '(-1 -2 -3 -4)))
  (is (equalp (enclosed-by -1 -5)
              '(-2 -3 -4))))
