#!/usr/bin/clisp

; 2.8a

(defun rdots (x)
   (if
      (> x 0)
      (or (format t ".") (rdots (- x 1)))
      (format t "~%")
   )
)

(defun idots (x)
   (do
      ((i 1 (+ i 1)))
      ((> i x) 'done)
      (format t ".")
   )
   (format t "~%")
)

(rdots 5)
(idots 5)

(setf foo '(a b c d e a b c a))

;(format t "~A" foo)

; 2.8b

(defun rcount (x y)
   (if
      (not x)
      (return-from rcount 0)
   )
   (+
      (if (eql y (car x)) 1 0)
      (rcount (cdr x) y)
   )
)

(defun icount (x y)
   (let ((n 0))
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

(format t "~A~%" (rcount foo 'a))
(format t "~A~%" (icount foo 'a))
