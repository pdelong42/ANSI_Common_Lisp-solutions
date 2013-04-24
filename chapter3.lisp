#!/usr/bin/clisp

; I cheated here and used "let*" and "return-from" before they were introduced.

(defun PrintExercise
   (heading &optional code output prints?)
   (format t "~A:~%" heading)
   (unless code (return-from PrintExercise (format t "TBD~%~%")))
   (format t "~A~%" code)
   (format t "actual:   ")
   (if prints?
      (eval code)
      (format t "~A" (eval code))
   )
   (terpri)
   (if output (format t "expected: ~A~%" output))
   (terpri)
)

; 3.1

(PrintExercise "Exercise 3.1")

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

(PrintExercise "Exercise 3.4")

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
   (if
      (null x)
      (return-from rposn+)
   )
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
   (if  
      (null x)
      (return-from rmpos+)
   )
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

; ToDo: think of a better way to test this, with actual output

(PrintExercise
   "Exercise 3.6a"
   '(function gov_cons)
)

; 3.6b

