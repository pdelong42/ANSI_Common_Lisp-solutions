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

; 4.2

; NOTE: What is not made clear, in the definition of "reduce" from the book,
; is that there is an implied elipsis in each of the "equivalent to"
; expressions.  That is, the expressions are not fixed at three variables, like
; the examples seem to imply, but can be expanded to an indefinite number of
; variables (e.g., 'd', 'e', 'f', etc.)

(defun my-copy-list
   (x)
   (reduce (function cons) x :from-end t :initial-value nil)
)

(defun my-reverse
   (x)
   (reduce
      (lambda
         (u v)
         (cons v u)
      )
      x
      :initial-value nil
   )
)

(PrintExercise
   "Exercise 4.2a - preliminary"
   '(setf list-test '(u v w x y z))
)

(PrintExercise
   "Exercise 4.2a"
   '(my-copy-list list-test)
   (copy-list list-test)
)

(PrintExercise
   "Exercise 4.2a - extra assurance"
   '(eql list-test (my-copy-list list-test))
   "NIL"
)

(PrintExercise
   "Exercise 4.2b"
   '(my-reverse list-test)
   (reverse list-test)
)

; 4.3

(defun print-trinode
   (n s d)
   (format s "(elt: ~A l: ~A m: ~A r: ~A)"
      (trinode-elt n)
      (trinode-l   n)
      (trinode-m   n)
      (trinode-r   n)
   )
)

(defstruct (trinode (:print-function print-trinode)) elt l m r)

(defun tst-copy
   (tst)
   (unless tst (return-from tst-copy))
   (make-trinode
      :elt           (trinode-elt tst)
      :l   (tst-copy (trinode-l   tst))
      :m   (tst-copy (trinode-m   tst))
      :r   (tst-copy (trinode-r   tst))
   )
)

(defun tst-find
   (obj tst)
   (unless tst (return-from tst-find))
   (or
      (eql      obj (trinode-elt tst))
      (tst-find obj (trinode-l   tst))
      (tst-find obj (trinode-m   tst))
      (tst-find obj (trinode-r   tst))
   )
)

(PrintExercise
   "Exercise 4.3a - preliminary"
   '(setf trinode-test
      (make-trinode
                          :elt 'ABC
         :l (make-trinode :elt 'DEF)
         :m (make-trinode :elt 'GHI)
         :r (make-trinode :elt 'JKL)
      )
   )
   "N/A"
)

(PrintExercise
   "Exercise 4.3a"
   '(tst-copy trinode-test)
   trinode-test
)

(PrintExercise
   "Exercise 4.3a - sanity check"
   '(eql trinode-test trinode-test)
   t
)

(PrintExercise
   "Exercise 4.3a - extra assurance"
   '(eql trinode-test (tst-copy trinode-test))
   "NIL"
)

(PrintExercise
   "Exercise 4.3a - extra extra assurance"
   '(eql
      (trinode-r           trinode-test)
      (trinode-r (tst-copy trinode-test))
   )
   "NIL"
)

(PrintExercise
   "Exercise 4.3b"
   '(tst-find
      trinode-test
      (make-trinode
                          :elt 'MNO
         :l (make-trinode :elt 'PQR)
         :m (make-trinode :elt trinode-test)
         :r (make-trinode :elt 'STU)
      )
   )
   t
)
