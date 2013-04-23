#!/usr/bin/clisp

; I cheated here and used "let*" and "return-from" before they were introduced.

(defun PrintExercise
   (heading &optional code output)
   (format t "~A:~%" heading)
   (unless code (return-from PrintExercise (format t "TBD~%~%")))
   (format t "~A~%" code)
   (format t "actual:   ~A~%" (eval code))
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
