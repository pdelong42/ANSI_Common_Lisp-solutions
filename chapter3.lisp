#!/usr/bin/clisp

; I cheated here and used "let*" and "return-from" before they were introduced.

(load "util.lisp")

; 3.1

; See the accompanying image file(s) for the diagrames called-for by Exercise
; 3.1; the "cons" statements below are intended to aid in making those
; diagrams.

(PrintExercise
   "Exercise 3.1a"
   '(cons 'a (cons 'b (cons (cons 'c (cons 'd nil)) nil)))
   '(a b (c d))
)

(PrintExercise
   "Exercise 3.1b"
   '(cons 'a (cons (cons 'b (cons (cons 'c (cons (cons 'd nil) nil)) nil)) nil))
   '(a (b (c (d))))
)

(PrintExercise
   "Exercise 3.1c"
   '(cons
       (cons
          (cons 'a (cons 'b nil))
          (cons 'c nil)
       )
       (cons 'd nil)
    )
   '(((a b) c) d)
)

(PrintExercise
   "Exercise 3.1d"
   '(cons 'a (cons (cons 'b 'c) 'd))
   '(a (b . c) . d)
)

; 3.2

(defun new-union
   (x y)
   (unless y (return-from new-union x))
   (let
      (  (v (car y))
         (w (cdr y))
      )
      (new-union
         (if
            (member v x)
            x
            (append x (list v))
         )
         w
      )
   )
)

(PrintExercise
   "Exercise 3.2"
   '(new-union '(a b c) '(b a d))
   '(a b c d)
)

; 3.3

(defun occurrences
   (x)
   (unless x (return-from occurrences))
   (let*
      (  (u (car x))
         (v (reverse (occurrences (cdr x))))
         (w (assoc u v))
      )
      (unless w (return-from occurrences (cons (cons u 1) v)))
      (cons
         (cons
            (car w)
            (+ 1 (cdr w))
         )
         (remove w v)
      )
   )
)

(PrintExercise
   "Exercise 3.3"
   '(occurrences '(a b a d a c d c a))
   '((a . 4) (c . 2) (d . 2) (b . 1))
)

; 3.4

; This returns "nil" because "(a)" refers to two different lists - the lists
; are distinct, even though they have the same elements and appear identical.

(PrintExercise
   "Exercise 3.4"
   '(member '(a) '((a) (b)))
)

; 3.5a

; this first solution is based on one I found while looking on the Internet for
; a solution, to see how well I solved the exercise - I thought this one was
; more elegant than mine, so I adapted it to my style;

(defun rpos++
   (x)
   (rposn++ x 0)
)

(defun rposn++
   (x n)
   (if
      (null x)
      (return-from rposn++)
   )
   (cons
      (+ (car x) n)
      (rposn++
         (cdr x)
         (+ n 1)
      )
   )  
)  

(PrintExercise
   "Exercise 3.5a (not my solution)"
   '(rpos++ '(7 5 1 4))
   '(7 6 3 7)
)

(defun rpos+
   (x)
   (let
      (  (n (length x)))
      (rposn+ x n n)
   )
)

(defun rposn+
   (x n m)
   (unless x (return-from rposn+))
   (cons
      (+
         (car x)
         (- m n)
      )
      (rposn+
         (cdr x)
         (- n 1)
         m
      )
   )
)

(PrintExercise
   "Exercise 3.5a (my solution)"
   '(rpos+ '(7 5 1 4))
   '(7 6 3 7)
)

; 3.5b

(defun ipos+
   (x)
   (let 
      (  (n 0)
         (y nil)
      )
      (dolist
         (w x)
         (push (+ w n) y)
         (setf n (+ n 1))
      )
      (reverse y)
   )
)

(PrintExercise
   "Exercise 3.5b"
   '(ipos+ '(7 5 1 4))
   '(7 6 3 7)
)

; 3.5c

(defun mpos+
   (x)
   (let
      (  (n -1))
      (mapcar
         (lambda
            (y)
            (setf n (+ n 1))
            (+ y n)
         )
         x
      )
   )
)

(PrintExercise
   "Exercise 3.5c"
   '(mpos+ '(7 5 1 4))
   '(7 6 3 7)
)

; 3.5 "extra credit" - by recursion + mapcar

(defun rmpos+
   (x)
   (unless x (return-from rmpos+))
   (cons
      (car x)
      (mapcar
         (lambda
            (y)
            (+ y 1)
         )
         (rmpos+ (cdr x))
      )
   )
)

(PrintExercise
   "Exercise 3.5 \"extra credit\""
   '(rmpos+ '(7 5 1 4))
   '(7 6 3 7)
)

; 3.6a

(defun gov_cons
   (x y)
   (cons y x)
)

(PrintExercise
   "Exercise 3.6a"
   '(gov_cons 'a 'b)
   '(b . a)
)

; 3.6b

; note that this is defined the same way "list" would be, with the exception of
; the usage of "gov_cons"; it would probably be more appopriate to swap the
; "head" and "tail" names, considering the intent of the exercise;

(defun gov_list
   (&rest args)
   (let
      (  (head (car args))
         (tail (cdr args))
      )
      (unless tail (return-from gov_list (gov_cons head nil)))
      (gov_cons head (apply (function gov_list) tail))
   )
)

(PrintExercise
   "Exercise 3.6b"
   '(gov_list 'a 'b 'c)
   '(((nil . c) . b) . a)
)

; 3.6c

(defun gov_length
   (x)
   (unless x (return-from gov_length 0))
   (+ 1 (gov_length (car x)))
)

(PrintExercise
   "Exercise 3.6c"
   '(gov_length (gov_list 'a 'b 'c))
   3
)

; 3.6d

(defun gov_member
   (x y)
   (unless y (return-from gov_member))
   (if
      (eql x (cdr y))
      y
      (gov_member x (car y))
   )
)

(PrintExercise
   "Exercise 3.6d"
   '(gov_member 'b (gov_list 'a 'b 'c))
   (gov_list 'b 'c)
)

; 3.7

; Modify the program in Figure 3.6 to use fewer cons cells. (Hint: Use dotted
; lists.) [see function definitions for: compress, compr, n-elts]

(defun compress
   (x)
   (unless
      (consp x)
      (return-from compress x)
   )
   (compr
      (car x)
      1
      (cdr x)
   )
)

(defun compr
   (elt n lst)
   (unless lst (return-from compr (list (n-elts elt n))))
   (let
      (  (next (car lst)))
      (if
         (eql next elt)
         (compr elt
            (+ n 1)
            (cdr lst)
         )
         (cons
            (n-elts elt n)
            (compr next 1 (cdr lst))
         )
      )
   )
)

(defun n-elts
   (elt n)
   (if
      (> n 1)
      (list n elt)
      elt
   )
)

(defun uncompress
   (lst)
   (unless lst (return-from uncompress))
   (let
      (  (elt (car lst))
         (rest (uncompress (cdr lst)))
      )
      (if
         (consp elt)
         (append (apply (function list-of) elt) rest)
            ;(funcall (function list-of) (car elt) (cdr elt))
         (cons elt rest)
      )
   )
)

(defun list-of
   (n elt)
   (unless
      (zerop n)
      (cons elt (list-of (- n 1) elt))
   )
)

(PrintExercise
   "Exercise 3.7 - n-elts"
   '(n-elts 'x 5)
   '(5 x)
)

(PrintExercise
   "Exercise 3.7 - compress"
   '(compress '(1 1 1 0 1 0 0 0 0 1))
   '((3 1) 0 1 (4 0) 1)
)

(PrintExercise
   "Exercise 3.7 - list-of"
   '(list-of 3 'ho)
   '(ho ho ho)
)

(PrintExercise
   "Exercise 3.7 - uncompress"
   '(uncompress '((3 1) 0 1 (4 0) 1))
   '(1 1 1 0 1 0 0 0 0 1)
)
