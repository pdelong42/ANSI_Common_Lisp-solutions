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
