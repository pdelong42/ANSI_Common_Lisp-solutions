#!/usr/bin/clisp

(load "util.lisp")

; ToDo: make this definition of "month" workable as a replacement for the
; existing one.

;(defconstant month
;   (reverse
;      (maplist
;         (lambda
;            (x)
;            (apply (function +) x)
;         )
;         (reverse '(0 31 28 31 30 31 30 31 31 30 31 30 31))
;      )
;   )
;)

; code from Figures 5.1 and 5.2

(defconstant month #(0 31 59 90 120 151 181 212 243 273 304 334 365))

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
      (  (= n 59)
         (values 2 29)
      )
      (  (> n 59)
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

; 5.5

(defun precedes-iterative
   (x v)
   (do*
      (  (chars)
         (i 0 (+ i 1))
         (curr nil (aref v i))
      )
      (
         (> i (- (length v) 2))
         (sort chars (function char<))
      )
      (and
         (eql x (aref v (+ i 1)))
         (not (member curr chars))
         (push curr chars)
      )
   )
)

(PrintExercise
   "Exercise 5.5 - iterative"
   '(precedes-iterative #\a "abracadabra")
   '(#\c #\d #\r)
)

(defun precedes-recursive
   (x v)
   (unless
      (< 1 (length v))
      (return-from precedes-recursive)
   )
   (let*
      (
         (curr (aref   v 0))
         (rest (subseq v 1))
         (chars (precedes-recursive x rest))
      )
      (and
         (eql x (aref v 1))
         (not (member curr chars))
         (push curr chars)
      )
      (sort chars (function char<))
   )
)

(PrintExercise
   "Exercise 5.5 - recursive"
   '(precedes-recursive #\a "abracadabra")
   '(#\c #\d #\r)
)

(defun precedes-alternative
   (x v &optional prev chars)
   (unless
      (< 0 (length v))
      (return-from precedes-alternative chars)
   )
   (sort
      (precedes-alternative
         x
         (subseq v 1)
         (aref   v 0)
         (if
            (and
               prev
               (eql x (aref v 0))
               (not (member prev chars))
            )
            (cons prev chars)
            chars
         )
      )
      (function char<)
   )
)

(PrintExercise
   "Exercise 5.5 - alternative"
   '(precedes-alternative #\a "abracadabra")
   '(#\c #\d #\r)
)

; 5.6

(defun intersperse-iterative
   (obj lst)
   (let
      (  (ret (cons (car lst) nil)))
      (dolist
         (cur (cdr lst))
         (setf ret (cons cur (cons obj ret)))
      )
      (reverse ret)
   )
)

(PrintExercise
   "Exercise 5.6 - iterative"
   '(intersperse-iterative '- '(a b c d))
   '(a - b - c - d)
)

; someone else's solution (modified) which I like better

(defun intersperse-alternative
   (obj lst)
   (let
      (  (ret))
      (dolist
         (cur lst)
         (push cur ret)
         (push obj ret)
      )
      (reverse (cdr ret))
   )
)

(PrintExercise
   "Exercise 5.6 - alternative"
   '(intersperse-alternative '- '(a b c d))
   '(a - b - c - d)
)

(defun intersperse-recursive
   (obj lst)
   (let
      (  (x (car lst))
         (y (cdr lst))
      )
      (cons x (if y (cons obj (intersperse-recursive obj y))))
   )
)

(PrintExercise
   "Exercise 5.6 - recursive"
   '(intersperse-recursive '- '(a b c d))
   '(a - b - c - d)
)

; 5.7

; Note from Paul: the solutions I found online did not assume that the absolute
; value of the difference should be "1", like I did below.  But either way, the
; logic is mostly identical (just drop the wrapping call to "abs").

(defun unitdiff-recursive
   (lst)
   (let
      (  (the1st  (car lst))
         (therest (cdr lst))
      )
      (unless therest (return-from unitdiff-recursive t))
      (unless
         (unitdiff-recursive (cdr therest))
         (return-from unitdiff-recursive)
      )
      (= 1 (abs (- the1st (car therest))))
   )
)

(defun unitdiff-iterative
   (lst)
   (let
      (  (obj (car lst)))
      (dolist
         (x (cdr lst))
         (unless
            (= 1 (abs (- obj x)))
            (return-from unitdiff-iterative)
         )
         (setf obj x)
      )
      (return-from unitdiff-iterative t)
   )
)

(defun unitdiff-mapreturn
   (lst)
   (let
      (  (obj (car lst)))
      (mapc
         (lambda
            (x)
            (unless
               (= 1 (abs (- obj x)))
               (return-from unitdiff-mapreturn)
            )
            (setf obj x)
         )
         (cdr lst)
      )
      (return-from unitdiff-mapreturn t)
   )
)

; this answer is from Ervin Dede

(defun diff-is-always-one
   (lst)
   (listp
      (mapc
         (lambda
            (x y)
            (unless
               (= 1 (abs (- y x)))
               (return-from diff-is-always-one)
            )
         )
         lst
         (cdr lst)
      )
   )
)

; There's a better way to do this, I'm sure.  But until I work-out the details,
; it's better to have something rather than nothing.

(dolist
   (f
     '(  unitdiff-recursive
         unitdiff-iterative
         unitdiff-mapreturn
         diff-is-always-one
      )
   )
   (dolist
      (x
        '(  (t     )
            (t     30)
            (t     1 2 3 2 1)
            ("NIL" 1 3 2 5 4)
            (t     5000 5001 5000)
            (t     29 28 27)
            (t     -5 -6 -7 -6)
            ("NIL" 50 3 666 98 5)
         )
      )
      (let
         (  (val (car x))
            (lst (cdr x))
         )
         (PrintExercise "Exercise 5.7" `(,f ',lst) val)
      )
   )
)

; 5.8

(defun min-and-max
   (vec)
   (if
      (eql 0 (length vec))
      (return-from min-and-max)
   )
   (let
      (  (head (aref vec 0)))
      (multiple-value-bind
         (minv maxv)
         (min-and-max (subseq vec 1))
         (values
            (cond
               (  (xor head minv))
               (  (and head minv)
                  (if (< head minv) head minv)
               )
            )
            (cond
               (  (xor head maxv))
               (  (and head maxv)
                  (if (> head maxv) head maxv)
               )
            )
         )
      )
   )
)

(PrintExercise
   "Exercise 5.8"
   '(multiple-value-bind
      (min max)
      (min-and-max #(5 3 1 0 -5 11 20 2 nil))
      (list min max)
   )
   '(-5 20)
)

; 5.9

; The program in Figure 3.12 continues to search as the first complete path
; works its way through the queue.  In broad searches this would be a problem.

(defun bfs
   (end queue net)
   (unless queue (return-from bfs))
   (let*
      (  (path (car queue))
         (node (car path))
      )
      (if
         (eql node end)
         (throw 'abort (reverse path))
      )
      (bfs
         end
         (append
            (cdr queue)
            (mapcar
               (lambda
                  (n)
                  (cons n path)
               )
               (cdr (assoc node net))
            )
         )
         net
      )
   )
)  

(defun shortest-path-catch
   (start end net)
   (catch 'abort (bfs end (list (list start)) net))
)

(PrintExercise
   "Exercise 5.9a"
   '(shortest-path-catch 'a 'd '((a b c) (b c) (c d)))
   '(a c d)
)

(defun shortest-path-nocatch
   (start finish net)
   (labels
      (  (bfs
            (end queue)
            (unless queue (return))
            (let*
               (
                  (path (car queue))
                  (node (car path))
               )
               (if
                  (eql node end)
                  (return-from shortest-path-nocatch (reverse path))
               )
               (bfs
                  end
                  (append
                     (cdr queue)
                     (mapcar
                        (lambda
                           (n)
                           (cons n path)
                        )
                        (cdr (assoc node net))
                     )
                  )
               )
            )
         )
      )
      (bfs finish (list (list start)))
   )
)

(PrintExercise
   "Exercise 5.9b"
   '(shortest-path-nocatch 'a 'd '((a b c) (b c) (c d)))
   '(a c d)
)

; I cheated - "labels" isn't taught until chapter 6

; UPDATE: oh, I guess I wasn't cheating after all - my original version of
; "bfs" fulfills the requirements of ex5.9b (I used a "return-from" statement
; because I thought it improved the code.
