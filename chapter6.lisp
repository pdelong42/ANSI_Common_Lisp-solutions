#!/usr/bin/clisp

(load "util.lisp")

; 6.1

(defun constituent
   (c)
   (and
      (graphic-char-p c)
      (not (char= c #\  ))
   )
)

(defun tokens
   (  str
      &key
      (test (function constituent))
      (start 0)
   )
   (let
      (  (p1 (position-if test str :start start)))
      (unless p1 (return-from tokens))
      (let
         (  (p2
               (position-if
                  (lambda
                     (c)
                     (not (funcall test c))
                  )
                  str
                  :start p1
               )
            )
         )
         (cons
            (subseq str p1 p2)
            (if p2 (tokens str :test test :start p2))
         )
      )
   )
)

(PrintExercise
   "Exercise 6.1 - try a non-default test function"
   '(tokens "ab12 3cde.f" :test (function alpha-char-p) :start 0)
   "(ab cde f)"
)

(PrintExercise
   "Exercise 6.1 - test defaults explicitly"
   '(tokens "ab12 3cde.f gh" :test (function constituent) :start 0)
   "(ab12 3cde.f gh)"
)

(PrintExercise
   "Exercise 6.1 - test fewer fields? (not sure what my thoughts were here)"
   '(tokens "ab12 3cde.f" :test (function constituent) :start 0)
   "(ab12 3cde.f)"
)

(PrintExercise
   "Exercise 6.1 - again, not sure what my thoughts were here"
   '(tokens "ab12 3cde.f gh" :test (function alpha-char-p) :start 0)
   "(ab cde f gh)"
)

(PrintExercise
   "Exercise 6.1 - testing with defaults only"
   '(tokens "ab12 3cde.f")
   "(ab12 3cde.f)"
)

(PrintExercise
   "Exercise 6.1"
   '(tokens "ab12 3cde.f gh")
   "(ab12 3cde.f gh)"
)

; 6.2

(defun bin-search
   (  obj
      vec
      &key
      (key (function identity))
      (test (function <))
      (start 0)
      (end (length vec))
   )
   (let
      (  (len (length vec)))
      (if
         (zerop len)
         (return-from bin-search)
      )
      (finder
         obj
         vec
         start
         (if
            (and end (< end len))
            end
            (- len 1)
         )
         test
         key
      )
   )
)

(defun finder
   (obj vec start end test key)
   ;(format t "DEBUG: start:~A end:~A vec:~A~%" start end (subseq vec start (+ end 1)))
   (if
      (< end start)
      (return-from finder)
   )
   (if
      (= end start)
      (return-from finder
         (unless
            (funcall test obj (funcall key (aref vec start)))
            (aref vec start)
         )
      )
   )
   (let*
      (  (mid (+ start (round (/ (- end start) 2))))
         (tmp (aref vec mid))
         (tst (funcall key tmp))
      )
      (if
         (eql obj tst)
         (return-from finder tmp)
      )
      (if
         (funcall test obj tst)
         (return-from finder (finder obj vec start (- mid 1) test key))
      )
      (finder obj vec (+ mid 1) end test key)
   )
)

(labels
   (  (foo
         (x y)
         (unless x (return-from foo))
         (PrintExercise
            "Exercise 6.2"
            `(bin-search ,(car x) #(0 1 2 3 4 5 6 7 8 9))
            (car y)
         )
         (foo
            (cdr x)
            (cdr y)
         )
      )
   )
   (foo
     '(   -1 0 1 2 3 4 5 6 7 8 9 10)
     '("NIL" 0 1 2 3 4 5 6 7 8 9  9)
   )
)

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
         ("NIL" 3 #(0 1 2 3 4 5 6 7 8 9) :end   1 :test (function >))
      )
   )
   (PrintExercise
      "Exercise 6.2"
      `(bin-search ,@(cdr args))
      (car args)
   )
)

; 6.3

(defun nargs
   (&rest  args)
   (length args)
)

(dolist
   (args
      '(
         (0)
         (1 (1))
         (2 (1 10))
         (0 a)
      )
   )
   (PrintExercise
      "Exercise 6.3"
      `(nargs ,@(car (cdr args)))
      (car args)
   )
)

; 6.4

(defun winners
   (fn lst)
   (unless lst (return-from winners (values nil nil)))
   (let*
      (  (winner (car lst))
         (runner-up)
         (max (funcall fn winner))
      )
      (dolist
         (obj lst)
         (let
            (  (score (funcall fn obj)))
            (when
               (> score max)
               (setf runner-up winner winner obj max score)
            )
         )
      )
      (values winner runner-up)
   )
)

(PrintExercise
   "Exercise 6.4"
   '(multiple-value-bind
      (first second)
      (winners #'identity '(1 2 3 2 2 3 3 3 4 1 1 2 2))
      (format nil "test of \"winners\": higest scoring = ~A; runner-up = ~A" first second)
   )
   "test of \"winners\": higest scoring = 4; runner-up = 3"
)
