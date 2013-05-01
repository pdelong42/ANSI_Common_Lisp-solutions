#!/usr/bin/clisp

(load "util.lisp")

; 4.1

(defun quarter-turn
   (x)
   (let*
      (
         (d (array-dimensions x))
         (m (first            d))
         (n (second           d))
         (y (make-array (list n m)))
      )
      (do
         (
            (j 0 (+ j 1))
            (l 0 (+ l 1))
         )
         (  (>= j n))
         (do
            (
               (i 0 (+ i 1))
               (k (- m 1) (- k 1))
            )
            (  (>= i m))
            (setf
               (aref y l k)
               (aref x i j)
            )
         )
      )
      y
   )
)

(PrintExercise
   "Exercise 4.1 - 2x2"
   '(quarter-turn #2a((a b) (c d)))
   #2a((c a) (d b))
)

(PrintExercise
   "Exercise 4.1 - 3x3"
   '(quarter-turn #2a((a b c) (d e f) (g h i)))
   #2a((g d a) (h e b) (i f c))
)

(PrintExercise
   "Exercise 4.1 - 3x2"
   '(quarter-turn #2a((a b) (c d) (e f)))
   #2a((e c a) (f d b))
)

(PrintExercise
   "Exercise 4.1 - 1x6"
   '(quarter-turn #2a((a b c d e f)))
   #2a((a) (b) (c) (d) (e) (f))
)

(PrintExercise
   "Exercise 4.1 - 6x1"
   '(quarter-turn #2a((f) (e) (d) (c) (b) (a)))
   #2a((a b c d e f))
)

; ganked this from some guy on the net, and tweaked it to suit my style;
; also fixed some potential bugs;

(defun quarter-turn-square
   (x) 
   (let*
      (
         (d (array-dimensions x))
         (n (car              d))
         (y (make-array       d))
      )
      (do
         ((i 0 (+ i 1)))
         ((>= i n))
         (do
            ((j 0 (+ j 1)))
            ((>= j n))
            (setf
               (aref y i j)
               (aref x (- n j 1) i)
            )
         )
      )
      y
   )
)

(PrintExercise
   "Exercise 4.1 - 2x2"
   '(quarter-turn-square #2a((a b) (c d)))
   #2a((c a) (d b))
)

(PrintExercise
   "Exercise 4.1 - 3x3"
   '(quarter-turn-square #2a((a b c) (d e f) (g h i)))
   #2a((g d a) (h e b) (i f c))
)
