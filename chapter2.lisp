#!/usr/bin/clisp

; I cheated here and used "let*" and "return-from" before they were introduced.

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

; Since "1" is not a list, by itself, the "else" clause of the "if-then-else"
; construct is taken, and the numbers "3" and "4" are added and the result
; "7" is returned.

(PrintExercise
   "Exercise 2.1c"
   '(if (listp 1) (+ 1 2) (+ 3 4))
   7
)

; 2.1d

; Since "3" is not a list, by itself, the enclosing "and" evaluates as "nil"
; (presumably to be pushed on the stack); next the numbers "1" and "2" are
; added together for a result of "3"; the larger expression then creates a list
; from this value and the previous one it pops off the stack.

(PrintExercise
   "Exercise 2.1d"
   '(list (and (listp 3) t) (+ 1 2))
   '(nil 3)
)

; 2.2

(PrintExercise
   "Exercise 2.2"
   '(cons 'a (cons 'b (cons 'c nil)))
   '(a b c)
)

(PrintExercise
   "Exercise 2.2"
   '(cons 'a (cons 'b '(c)))
   '(a b c)
)

(PrintExercise
   "Exercise 2.2"
   '(cons 'a '(b c))
   '(a b c)
)

; 2.3

(defun yonban-me
   (x)
   (car (cdr (cdr (cdr x))))
)

(PrintExercise
   "Exercise 2.3"
   '(yonban-me '(1st 2nd 3rd 4th 5th 6th))
   '4th
)

; 2.4

(defun greater
   (x y)
   (if (> x y) x y)
)

(PrintExercise
   "Exercise 2.4"
   '(greater 100 -100)
   100
)

; 2.5a

; The following function expects a list as its input, and returns true if it
; contains any null entries, false if there are none.

(defun enigma
   (x)
   (unless x (return-from enigma))
   (unless
      (car x)
      (return-from enigma t)
   )
   (enigma (cdr x))
)

(PrintExercise
   "Exercise 2.5a"
   '(enigma '(a nil c))
   t
)

; PrintExercise has an interesting bug I didn't catch earlier: you can't use
; NIL as an expected output, since that's interpreted as the desire to not
; print expected output.  This needs to be addressed sooner or later.

; 2.5b

; The following function expects an element and a list as its input, and
; returns the position of the first occurence of the element within the list.

(defun mystery
   (x y)
   (unless y (return-from mystery))
   (if
      (eql x (car y))
      (return-from mystery 0)
   )
   (let
      (  (z (mystery x (cdr y))))
      (if z (+ z 1))
   )
)

(PrintExercise
   "Exercise 2.5b"
   '(mystery 'c '(a b a c a b))
   3
)

; 2.6a

(PrintExercise
   "Exercise 2.6a"
   '(car (car (cdr '(a (b c) d))))
   'b
)

; 2.6b

(PrintExercise
   "Exercise 2.6b"
   '(or 13 (/ 1 0))
   13
)

; 2.6c

(PrintExercise
   "Exercise 2.6c"
   '(apply #'list 1 nil)
   '(1)
)

; 2.7

(defun contains-list?
   (lst)
   (unless lst (return-from contains-list?))
   (if
      (listp (car lst))
      (return-from contains-list? t)
   )
   (contains-list? (cdr lst))
)

(PrintExercise
   "Exercise 2.7"
   '(contains-list? '(a (b c) d))
   t
)

; 2.8a

(defun rdots
   (x)
   (unless
      (> x 0)
      (return-from rdots)
   )
   (format t ".")
   (rdots (- x 1))
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

(defun icount ; I count the candles on the shelf...
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

; 2.9a

; The original function seemed to behave as though 'remove' has side-effects,
; which it does not.

(defun summit
   (lst)
   (apply
      (function +)
      (remove nil lst)
   )
)

(PrintExercise
   "Exercise 2.9a"
   '(summit '(1 nil 3 nil 5))
   9
)

; 2.9b

; The original function definition had no recursion termination condition, no
; base case.

(defun summit
   (lst)
   (unless lst (return-from summit 0))
   (+
      (summit (cdr lst))
      (let
      (  (x (car lst)))
         (if (null x) 0 x)
      )
   )
)

(PrintExercise
   "Exercise 2.9b"
   '(summit '(2 nil 4 nil 6))
   12
)
