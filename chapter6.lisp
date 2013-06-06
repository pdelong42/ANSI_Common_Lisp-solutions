#!/usr/bin/clisp

(load "util.lisp")

; 6.1

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

(defun constituent
   (c)
   (and
      (graphic-char-p c)
      (not (char= c #\  ))
   )
)

(PrintExercise
   "Exercise 6.1 - split by alphabetic characters"
   '(tokens "ab12 3cde.f" :test (function alpha-char-p) :start 0)
   "(ab cde f)"
)

(PrintExercise
   "Exercise 6.1 - test default explicitly"
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

;(PrintExercise
;   "Exercise 6."
;)

; ToDo:
; - rework the above test statements so that it is a do-loop like the one
;   below.
