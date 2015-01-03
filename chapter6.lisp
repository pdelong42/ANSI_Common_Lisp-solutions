#!/usr/bin/clisp

(load "util.lisp")

(defun single?
   (lst)
   (and
      (consp lst)
      (null (cdr lst))  )  )

(defun append1
   (lst obj)
   (append lst (list obj))  )

(defun map-int
   (fn n)
   (let
      (  (acc))
      (dotimes
         (i n)
         (push (funcall fn i) acc)  )
      (nreverse acc)  )  )

(defun filter
   (fn lst)
   (let
      (  (acc))
      (dolist
         (x lst)
         (let
            (  (val (funcall fn x)))
            (if val (push val acc))  )  )
      (nreverse acc)  )  )

; try implementing this recursively

(defun most
   (fn lst)
   (unless lst (return-from most (values nil nil)))
   (let*
      (  (wins (car lst))
         (max (funcall fn wins))  )
      (dolist
         (obj (cdr lst))
         (let
            (  (score (funcall fn obj)))
            (when
               (> score max)
               (setf wins obj max score)  )  )  )
      (values wins max)  )  )

(defun make-adder
   (n)
   (lambda
      (x)
      (+ x n)  )  )


;(let
;   (  (counter 0))
;   (defun reset nil (setf counter 0))
;   (defun stamp nil (setf counter (+ counter 1)))  )

; ToDo: try implementing this recursively

(defun compose
   (&rest fns)
   (destructuring-bind
      (fn1 . rest)
      (reverse fns)
      (lambda
         (&rest args)
         (reduce
            (lambda
               (v f)
               (funcall f v)  )
            rest
            :initial-value
            (apply fn1 args)  )  )  )  )

(defun disjoin
   (fn &rest fns)
   (unless fns (return-from disjoin fn))
   (lambda
      (&rest args)
      (or
         (apply fn args)
         (apply (apply (function disjoin) fns) args)  )  )  )

(defun conjoin
   (fn &rest fns)
   (unless fns (return-from conjoin fn))
   (lambda
      (&rest args)
      (and
         (apply fn args)
         (apply (apply (function conjoin) fns) args)  )  )  )

(defun curry
   (fn &rest args)
   (lambda
      (&rest args2)
      (apply fn (append args args2))  )  )

(defun rcurry
   (fn &rest args)
   (lambda
      (&rest args2)
      (apply fn (append args2 args))  )  )

(defun always
   (x)
   (lambda (&rest args) x)  )

(defun fib
   (n)
   (if
      (<= n 1)
      1
      (+
         (fib (- n 1))
         (fib (- n 2))  )  )  )

(defun fib-iterative
   (n)
   (do
      (  (i  n (- i 1))
         (f1 1 (+ f1 f2))
         (f2 1 f1)  )
      (  (<= i 1) f1)  )  )

; 6.1

(defun constituent
   (c)
   (and
      (graphic-char-p c)
      (not (char= c #\  ))  )  )

(defun tokens
   (  str
      &key
      (test (function constituent))
      (start 0)  )
   (let
      (  (p1 (position-if test str :start start)))
      (unless p1 (return-from tokens))
      (let
         (  (p2
               (position-if
                  (lambda
                     (c)
                     (not (funcall test c))  )
                  str
                  :start p1  )  )  )
         (cons
            (subseq str p1 p2)
            (if p2 (tokens str :test test :start p2))  )  )  )  )

(dolist
   (args
      '(
         ("(ab cde f)"       "ab12 3cde.f"    :test (function alpha-char-p) :start 0) ; try a non-default test function
         ("(ab12 3cde.f gh)" "ab12 3cde.f gh" :test (function constituent)  :start 0) ; test defaults explicitly
         ("(ab12 3cde.f)"    "ab12 3cde.f"    :test (function constituent)  :start 0) ; test fewer fields? (not sure what my thoughts were here)
         ("(ab cde f gh)"    "ab12 3cde.f gh" :test (function alpha-char-p) :start 0) ; again, not sure what my thoughts were here
         ("(ab12 3cde.f)"    "ab12 3cde.f")                                           ; testing with defaults only
         ("(ab12 3cde.f gh)" "ab12 3cde.f gh")
      )
   )
   (PrintExercise
      "Exercise 6.1"
      `(tokens ,@(cdr args))
      (car args)  )  )

; 6.2

(defun bin-search
   (  obj
      vec
      &key
      (key (function identity))
      (test (function <))
      (start 0)
      (end (length vec))  )
   (let
      (  (len (length vec)))
      (if
         (zerop len)
         (return-from bin-search)  )
      (finder
         obj
         vec
         start
         (if
            (and end (< end len))
            end
            (- len 1)  )
         test
         key  )  )  )

(defun finder
   (obj vec start end test key)
   ;(format t "DEBUG: start:~A end:~A vec:~A~%" start end (subseq vec start (+ end 1)))
   (if
      (< end start)
      (return-from finder)  )
   (if
      (= end start)
      (return-from finder
         (unless
            (funcall test obj (funcall key (aref vec start)))
            (aref vec start)  )  )  )
   (let*
      (  (mid (+ start (round (/ (- end start) 2))))
         (tmp (aref vec mid))
         (tst (funcall key tmp))  )
      (if
         (eql obj tst)
         (return-from finder tmp)  )
      (if
         (funcall test obj tst)
         (return-from finder (finder obj vec start (- mid 1) test key))  )
      (finder obj vec (+ mid 1) end test key)  )  )

(labels
   (  (foo
         (x y)
         (unless x (return-from foo))
         (PrintExercise
            "Exercise 6.2"
            `(bin-search ,(car x) #(0 1 2 3 4 5 6 7 8 9))
            (car y)  )
         (foo
            (cdr x)
            (cdr y)  )  )  )
   (foo
     '(   -1 0 1 2 3 4 5 6 7 8 9 10)
     '("NIL" 0 1 2 3 4 5 6 7 8 9  9)  )  )

(dolist
   (args
      '(
         ("NIL" 3 #(4 5 6 7 8 9))
         (d 'd #(a b c d e f g h i j) :test (function string<))
         ((3 c)  3 #((1 a) (2 b) (3 c) (4 d) (5 e) (6 f) (7 g) (8 h) (9 i) (10 j)) :key (function first))
         ((c 3)  3 #((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10)) :key (function second))
         ((d 4) 'd #((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10)) :key (function car) :test (function string<))
         ("NIL" 3 #(0 1 2 3 4 5 6 7 8 9) :start 4)
         ("NIL" 3 #(0 1 2 3 4 5 6 7 8 9) :start 4 :test (function >))
         ("NIL" 3 #(0 1 2 3 4 5 6 7 8 9) :end   1 :test (function >))  )  )
   (PrintExercise
      "Exercise 6.2"
      `(bin-search ,@(cdr args))
      (car args)  )  )

; 6.3

(defun nargs
   (&rest  args)
   (length args)  )

(dolist
   (args
      '(
         (0)
         (1 (1))
         (2 (1 10))
         (0 a)  )  )
   (PrintExercise
      "Exercise 6.3"
      `(nargs ,@(car (cdr args)))
      (car args)  )  )

; 6.4

(defun winners
   (fn lst)
   (unless lst (return-from winners (values nil nil)))
   (let*
      (  (winner (car lst))
         (runner-up)
         (max (funcall fn winner))  )
      (dolist
         (obj lst)
         (let
            (  (score (funcall fn obj)))
            (when
               (> score max)
               (setf runner-up winner winner obj max score)  )  )  )
      (values winner runner-up)  )  )

(PrintExercise
   "Exercise 6.4"
   '(multiple-value-bind
      (first second)
      (winners #'identity '(1 2 3 2 2 3 3 3 4 1 1 2 2))
      (format nil "test of \"winners\": higest scoring = ~A; runner-up = ~A" first second)  )
   "test of \"winners\": higest scoring = 4; runner-up = 3"  )

; 6.5

(defun filter
   (fn lst)
   (let
      (  (acc))
      (dolist
         (x lst)
         (let
            (  (val (funcall fn x)))
            (if val (push val acc))  )  )
      (nreverse acc)  )  )

(defun my-remove-if
   (fn lst)
   (filter
      (lambda
         (x)
         (unless (funcall fn x) x)  )
      lst  )  )

(PrintExercise
   "Exercise 6.5"
   '(my-remove-if (lambda (x) (evenp x)) '(1 2 3 4 5 6 7))
   '(1 3 5 7)  )

; 6.6

(let
   (  (x))
   (defun greatest
      (y)
      (unless
         (and x (> x y))
         (setf x y)  )
      x  )  )

(labels
   (  (foo
         (x y)
         (unless x (return-from foo))
         (PrintExercise
            "Exercise 6.6"
            `(greatest ,(car x))
            (car y)  )
         (foo
            (cdr x)
            (cdr y)  )  )  )
   (foo
      '(0 3 0 5 3)
      '(0 3 3 5 5)  )  )

; 6.7

(let
   (  (x))
   (defun greater
      (y)
      (unless
         x
         (setf x y)
         (return-from greater)  )
      (unless
         (< x y)
         (return-from greater)  )
      (setf x y)
      t  )  )

(labels
   (  (foo
         (x y)
         (unless x (return-from foo))
         (PrintExercise
            "Exercise 6.7"
            `(greater ,(car x))
            (car y)  )
         (foo
            (cdr x)
            (cdr y)  )  )  )
   (foo
      '(0 3 3 0)
      '("NIL" t "NIL" "NIL")  )  )

; 6.8

(defun expensive
   (x)
   (+ x 100)  )

(let
   (  (memo (make-hash-table)))
   (defun memo nil memo)
   (defun frugal
      (x)
      (multiple-value-bind
         (value exists)
         (gethash x memo)
         (if exists (return-from frugal (values value exists)))
         (values
            (setf
               (gethash x memo)
               (expensive x)  )
            exists  )  )  )  )

(dolist
   (x
      '(
         ((105 nil) (expensive 5))
         ((105 nil)    (frugal 5))
         ((105   t)    (frugal 5))
         ((103 nil)    (frugal 3))
         ((103   t)    (frugal 3))  )  )
   (PrintExercise
      "Exercise 6.8"
      `(multiple-value-bind
         (value exists)
         ,(car (cdr x))
         (list value exists)  )
      (car x)  )  )

; 6.9

(defun straighten
   (list)
   (let
      (  (x (car (last list))))
      (if
         (consp x)
         (append (butlast list) x)
         list  )  )  )

(labels
   (  (foo
         (x y)
         (unless x (return-from foo))
         (PrintExercise
            "Exercise 6.9 - preliminary"
            `(straighten ',(car x))
            (car y)  )
         (foo
            (cdr x)
            (cdr y)  )  )  )
   (foo
      '(
         (1 2 3 4 5)
         (1 2 3 nil)
         (1 2 3 (4))
         (1 2 3 (4 5))
         (1 2 3 (4 5 6))
         (1 2 3 (4 5 6 (7 8 9)))  )
      '(
         (1 2 3 4 5)
         (1 2 3 nil)
         (1 2 3 4)
         (1 2 3 4 5)
         (1 2 3 4 5 6)
         (1 2 3 4 5 6 (7 8 9))  )  )  )

(defun my-apply
   (fn &rest args)
   (let
      (  (*print-base* 8))
      (apply fn (straighten args))  )  )

(dolist
   (x
      '(
         (   apply 20 21 22 30 "40 ")
         (my-apply 24 25 26 36 "50 ")
         (   apply 20 21 22 30 "40 ")  )  )
   (PrintExercise
      "Exercise 6.9"
      `(
         ,(car x)
         (lambda
            (&rest tmp)
            (format nil "(~{~A ~})" tmp)  )
         '(20 21 22 30 40)  )
      (cdr x)  )  )
