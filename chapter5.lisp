#!/usr/bin/clisp

(load "util.lisp")

; code from Figures 5.1 and 5.2

;(defconstant month
;   (reverse
;      (maplist
;         (lambda
;            (x)
;            (apply (function +) x)
;         )
;         (reverse
;            '(0 31 28 31 30 31 30 31 31 30 31 30 31)
;         )
;      )
;   )
;)

(defconstant month
   #(0 31 59 90 120 151 181 212 243 273 304 334 365)
)

(defconstant yzero 2000)

(defun leap?
   (y)
   (unless
      (zerop (mod y 4))
      (return-from leap?)
   )
   (unless
      (zerop (mod y 100))
      (return-from leap? t)
   )
   (zerop (mod y 400))
)

(PrintExercise
   "Figures 5.1 and 5.2 - test of \"leap?\""
   '(mapcar #'leap? '(1904 1900 1600))
   '(t nil t)
)

(defun date->num
   (d m y)
   (+
      (- d 1)
      (month-num m y)
      (year-num y)
   )
)

(defun month-num
   (m y)
   (+
      (svref month (- m 1))
      (if
         (and
            (> m 2)
            (leap? y)
         )
         1
         0
      )
   )
)

(defun year-num
   (y)
   (let
      (  (d 0))
      (if
         (>= y yzero)
         (dotimes
            (i
               (- y yzero)
               d
            )
            (incf d (year-days (+ yzero i)))
         )
         (dotimes
            (i
               (- yzero y)
               (- d)
            )
            (incf d (year-days (+ y i)))
         )
      )
   )
)

(defun year-days
   (y)
   (if (leap? y) 366 365)
)

(defun num->date
   (n)
   (multiple-value-bind
      (y left)
      (num-year n)
      (multiple-value-bind
         (m d)
         (num-month left y)
         (values d m y)
      )
   )
)

(defun num-year
   (n)
   (if
      (< n 0)
      (do*
         (
            (y
               (- yzero 1)
               (- y 1)
            )
            (d
               (-   (year-days y))
               (- d (year-days y))
            )
         )
         (
            (<= d n)
            (values y (- n d))
         )
      )
      (do*
         (
            (y yzero (+ y 1))
            (prev 0 d)
         )
         (
            (> d n)
            (values y (- n prev))
         )
      )
   )
)


(defun num-month
   (n y)
   (unless
      (leap? y)
      (return-from num-month (nmon n))
   )
   (cond
      (
         (= n 59)
         (values 2 29)
      )
      (
         (> n 59)
         (nmon (- n 1))
      )
      (t (nmon n))
   )
)

(defun nmon
   (n)
   (let
      (  (m (position n month :test #'<)))
      (values m (+ 1 (- n (svref month (- m 1)))))
   )
)

(defun date+
   (d m y n)
   (num->date (+ (date->num d m y) n))
)

(PrintExercise
   "Figures 5.1 and 5.2 - test of \"date+\""
   '(multiple-value-list (date+ 17 12 1997 60))
   '(15 2 1998)
)

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

; 5.4

(defun my-month-num
   (m y)
   (if
      (< m 0)
      (return-from my-month-num 0)
   )
   (+
      (my-month-num (- m 1) y)
      (case m
         ((0 2 4 6 7 9 11) 31)
         ((3 5 8 10) 30)
         (t (if (leap? y) 29 28))
      )
   )
)

(dolist
   (y '(2000 2001))
   (dotimes
      (i 13)
      (PrintExercise
         (format t "Exercise 5.4 - year=~A month=~A~%" y i)
         `(my-month-num (- ,i 1) ,y)
         (month-num (+ i 1) y)
      )
   )
)
