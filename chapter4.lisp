#!/usr/bin/clisp

(load "util.lisp")

; Section 4.7 Example: BST

(defun print-node
   (n s d)
   (format s "(elt: ~A l: ~A r: ~A)"
      (node-elt n)
      (node-l   n)
      (node-r   n)
   )
)

(defstruct (node (:print-function print-node)) elt l r)

(defun bst-copy
   (bst)
   (unless bst (return-from bst-copy))
   (make-node
      :elt           (node-elt bst)
      :l   (bst-copy (node-l   bst))
      :r   (bst-copy (node-r   bst))
   )
)

(defun bst-insert
   (obj bst <)
   (unless bst (return-from bst-insert (make-node :elt obj)))
   (let
      (  (elt (node-elt bst)))
      (if
         (eql obj elt)
         (return-from bst-insert bst)
      )
      (if
         (funcall < obj elt)
         (make-node
            :elt elt
            :l   (bst-insert obj (node-l bst) <)
            :r                   (node-r bst)
         )
         (make-node
            :elt elt
            :r   (bst-insert obj (node-r bst) <)
            :l                   (node-l bst)
         )
      )
   )
)

(defun bst-find
   (obj bst <)
   (unless bst (return-from bst-find))
   (let
      (  (elt (node-elt bst)))
      (if
         (eql obj elt)
         (return-from bst-find bst)
      )
      (if
         (funcall < obj elt)
         (bst-find obj (node-l bst) <)
         (bst-find obj (node-r bst) <)
      )
   )
)

(defun bst-min
   (bst)
   (and bst (or (bst-min (node-l bst)) bst))
)

(defun bst-max
   (bst)
   (and bst (or (bst-max (node-r bst)) bst))
)

(defun bst-traverse
   (fn bst)
   (when bst
      (bst-traverse fn (node-l   bst))
      (funcall      fn (node-elt bst))
      (bst-traverse fn (node-r   bst))
   )
)

(defun bst-remove
   (obj bst <)
   (unless bst (return-from bst-remove))
   (let
      (  (elt (node-elt bst))
         (l   (node-l   bst))
         (r   (node-r   bst))
      )
      (unless
         (eql obj elt)
         (return-from
            bst-remove
            (if
               (funcall < obj elt)
               (make-node
                  :elt                 elt
                  :l   (bst-remove obj l <)
                  :r                   r
               )
               (make-node
                  :elt                 elt
                  :r   (bst-remove obj r <)
                  :l                   l
               )
            )
         )
      )
      (unless l (return-from bst-remove r))
      (unless r (return-from bst-remove l))
      (return-from
         bst-remove
         (if
            (zerop (random 2))
            (make-node
               :elt (node-elt (bst-max l))
               :l      (bst-remove-max l)
               :r                      r
            )
            (make-node
               :elt (node-elt (bst-min r))
               :r      (bst-remove-min r)
               :l                      l
            )
         )
      )
   )
)

(defun bst-remove-min
   (bst)
   (if
      (node-l bst)
      (make-node
         :elt                 (node-elt bst)
         :l   (bst-remove-min (node-l   bst))
         :r                   (node-r   bst)
      )
      (node-r bst)
   )
)

(defun bst-remove-max
   (bst)
   (if
      (node-r bst)
      (make-node
         :elt                 (node-elt bst)
         :l                   (node-l   bst)
         :r   (bst-remove-max (node-r   bst))
      )
      (node-l bst)
   )
)

; The BST code below this line is the broken variety - delete it when you are
; satisfied it no longer serves any purpose.

(defun broken-bst-remove
   (obj bst <)
   (unless bst (return-from bst-remove))
   (let
      (  (elt (node-elt bst)))
      (if
         (eql obj elt)
         (return-from bst-remove (percolate bst))
      )
      (if
         (funcall < obj elt)
         (make-node
            :elt                         elt
            :l   (bst-remove obj (node-l bst) <)
            :r                   (node-r bst)
         )
         (make-node
            :elt                         elt
            :r   (bst-remove obj (node-r bst) <)
            :l                   (node-l bst)
         )
      )
   )
)

(defun percolate
   (bst)
   (unless
      (node-l bst)
      (if
         (node-r bst)
         (return-from percolate (rperc bst))
         (return-from percolate)
      )
   )
   (unless 
      (node-r bst)
      (return-from percolate (lperc bst))
   )
   (if
      (zerop (random 2))
      (lperc bst)
      (rperc bst)
   )
)

(defun rperc
   (bst)
   (make-node
      :elt (node-elt (node-r bst))
      :l             (node-l bst)
      :r  (percolate (node-r bst))
   )
)

(defun lperc
   (bst)
   (make-node
      :elt (node-elt (node-l bst))
      :l  (percolate (node-l bst))
      :r             (node-r bst)
   )
)

; Gradually re-enable these tests (in the new framework) as they are needed.

(PrintExercise
   "Section 4.7 example: BST - initialization"
   '(progn
      (setf nums nil)
      (dolist
         (x '(5 8 4 2 1 9 6 7 3))
         (setf nums (bst-insert x nums (function <)))
      )
      nums
   )
   "N/A"
)

(PrintExercise
   "Section 4.7 example: BST - test of bst-copy and bst-traverse"
   '(bst-traverse #'princ (bst-copy nums))
   "123456789NIL"
)

(PrintExercise
   "Section 4.7 example: BST - test of null bst-find"
   '(bst-find 12 nums (function <))
   "NIL"
)

(PrintExercise
   "Section 4.7 example: BST - test of non-null bst-find"
   '(bst-find 4 nums (function <))
   "(elt: 4 l: (elt: 2 l: (elt: 1 l: NIL r: NIL) r: (elt: 3 l: NIL r: NIL)) r: NIL)"
)

(PrintExercise
   "Section 4.7 example: BST - test of non-null bst-min"
   '(bst-min nums)
   "TBD"
)

(PrintExercise
   "Section 4.7 example: BST - test of non-null bst-max"
   '(bst-max nums)
   "TBD"
)

;(format t "(setf nums (bst-remove 2 nums (function <)))~%")
;(format t "expected: #<5>~%")
;(format t "actual:   ~A~%~%" (setf nums (bst-remove 2 nums (function <))))

;(format t "(bst-find 2 nums (function <))~%")
;(format t "expected: NIL~%")
;(format t "actual:   ~A~%~%" (bst-find 2 nums (function <)))

;(format t "(bst-traverse (function princ) nums)~%")
;(format t "expected: 13456789~%")
;(format t "actual:   ")
;(bst-traverse (function princ) nums)
;(format t "~%")

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
   "Exercise 4.2a - initialization"
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
   "Exercise 4.3a - initialization"
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

; 4.4

(defun bst-reverse
   (bst)
   (unless bst (return-from bst-reverse))
   (append
      (bst-reverse (node-r   bst))
      (list        (node-elt bst))
      (bst-reverse (node-l   bst))
   )
)

(PrintExercise
   "Exercise 4.4"
   '(bst-reverse nums)
   '(9 8 7 6 5 4 3 2 1)
)

; 4.5

; The definition of "bst-adjoin" is identical to the functionality of the
; already written "bst-insert".

(PrintExercise
   "Exercise 4.5 - skipped (functionality identical to bst-insert)"
)

; 4.6

(defun ass-to-hash
   (ass)
   (unless ass (return-from ass-to-hash (make-hash-table)))
   (let
      (
         (curr              (car ass) )
         (rest (ass-to-hash (cdr ass)))
      )
      (setf
         (gethash (car curr) rest)
         (         cdr curr      )
      )
      rest
   )
)

(defun ass-to-hash-iterative
   (ass)
   (let
      (  (hash (make-hash-table)))
      (dolist (curr (reverse ass))
         (setf
            (gethash (car curr) hash)
            (         cdr curr      )
         )
      )
      hash
   )
)

(defun hash-to-ass
   (hash)
   (let
      (ass)
      (maphash
         (lambda
            (k v)
            (push (cons k v) ass)
         )
         hash
      )
      (reverse ass)
   )
)

(PrintExercise
   "Exercise 4.6 - initialization"
   '(setf testass '((foo . 1) (bar . 2)))
   '((foo . 1) (bar . 2))
)

(PrintExercise
   "Exercise 4.6a - recursive"
   '(ass-to-hash testass)
   #S(hash-table :test fasthash-eql (bar . 2) (foo . 1))
)

(PrintExercise
   "Exercise 4.6a - iterative"
   '(ass-to-hash-iterative testass)
   #S(hash-table :test fasthash-eql (bar . 2) (foo . 1))
)

(PrintExercise
   "Exercise 4.6b"
   '(hash-to-ass (ass-to-hash testass))
   testass
)
