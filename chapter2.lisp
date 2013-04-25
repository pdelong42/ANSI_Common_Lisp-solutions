#!/usr/bin/clisp

(load "util.lisp")

; 2.1a

; The sub-expressions, (- 5 1) and (+ 3 7), are each evaluated first (their
; results probably pushed onto a stack as each is evaluated in-turn), and then
; the larger expression is evaluated (probably by popping the previous results
; off of the aforementioned stack).

(PrintExercise
   "Exercise 2.1a"
   '(+ (- 5 1) (+ 3 7))
   14
)

; 2.1b

; The sub-expression, (+ 2 3), is evauated first, and its result is passed as
; an argument to the outer expression, which creates a list containing it.

(PrintExercise
   "Exercise 2.1b"
   '(list 1 (+ 2 3))
   '(1 5)
)

; 2.1c

; (write description)

(PrintExercise
   "Exercise 2.1c"
   '(if (listp 1) (+ 1 2) (+ 3 4))
   7
)

; 2.1d

; (write description)

(PrintExercise
   "Exercise 2.1d"
   '(list (and (listp 3) t) (+ 1 2))
   '(nil 3)
)

; 2.8a

(defun rdots
   (x)
   (if
      (> x 0)
      (or
         (format t ".")
         (rdots (- x 1))
      )
   )
)

(PrintExercise
   "Exercise 2.8a - recursive"
   '(rdots 5)
   "....."
   t
)

(defun idots
   (x)
   (do
      ((i 1 (+ i 1)))
      ((> i x) 'done)
      (format t ".")
   )
)

(PrintExercise
   "Exercise 2.8a - iterative"
   '(idots 5)
   "....."
   t
)

; 2.8b

(defun rcount
   (x y)
   (unless x (return-from rcount 0))
   (+
      (if (eql y (car x)) 1 0)
      (rcount (cdr x) y)
   )
)

(PrintExercise
   "Exercise 2.8b - recursive"
   '(rcount '(a b c d e a b c a) 'a)
   3
)

(defun icount
   (x y)
   (let
      (  (n 0))
      (do
         ((z x (cdr z)))
         ((null z) 'done)
         (if
            (eql y (car z))
            (setf n (+ n 1))
         )
      )
      n
   )
)

(PrintExercise
   "Exercise 2.8b - iterative"
   '(icount '(a b c d e a b c a) 'a)
   3
)
