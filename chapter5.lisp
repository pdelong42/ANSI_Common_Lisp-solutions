#!/usr/bin/clisp

(load "util.lisp")

; 5.1

(PrintExercise
   "Exercise 5.1a - original, with \"let\""
   ''(let
      (  (x (car y)))
      (cons x x)
   )
)

(PrintExercise
   "Exercise 5.1a - translation, sans \"let\""
   ''(funcall
      (lambda
         (x)
         (cons x x)
      )
      (car y)
   )
)

(PrintExercise
   "Exercise 5.1b - original, with \"let\""
   ''(let*
      (
         (w (car x))
         (y (+ w z))
      )
      (cons w y)
   )
)

(PrintExercise
   "Exercise 5.1b - translation #1, sans \"let\""
   ''(funcall
      (lambda
         (w)
         (funcall
            (lambda
               (y)
               (cons w y)
            )
            (+ w z)
         )
      )
      (car x)
   )
)

(PrintExercise
   "Exercise 5.1b - translation #2, sans \"let\""
   ''(funcall
      (lambda
         (w y)
         (cons w y)
      )
      (car x)
      (+ w z)
   )
)

; 5.2

(defun mystery
   (x y)
   (unless y (return-from mystery))
   (if
      (eql x (car y))
      (return-from mystery 0)
   )
   (let
      (  (z (mystery x (cdr y))))
      (and z (+ z 1))
   )
)

(defun my-mystery
   (x y)
   (cond
      (  (null y) nil)
      ((eql x (car y)) 0)
      (t
         (let
            (  (z (mystery x (cdr y))))
            (and z (+ z 1))
         )
      )
   )
)

(PrintExercise
   "Exercise 5.2"
   '(my-mystery 4 '(1 2 3 4 5 4 3 2 1))
   (mystery 4 '(1 2 3 4 5 4 3 2 1))
)

; 5.3

(defun restricted-square
   (x)
   (if
      (and
         (typep x 'integer)
         (or
            (> x 5)
            (<= x 0)
         )
      )
      (* x x)
      x
   )
)

(PrintExercise
   "Exercise 5.3"
   '(mapcar #'restricted-square '(-5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9))
   '(25 16 9 4 1 0 1 2 3 4 5 36 49 64 81)
)
