(defun binpow (a n)
  (declare (optimize (speed 3) (safety 0)))
  (labels ((f (a n mul)
             (declare (optimize (speed 3) (safety 0)))
             (cond ((= n 0) a)
                   ((oddp n) (f (* a mul) (ash n -1) (* mul mul)))
                   (t (f a (ash n -1) (* mul mul))))))
    (f 1 n a)))

(defun solve (n)
  (declare (optimize (speed 3) (safety 0)))
  (let ((a (binpow 2 n))
        (ans 0))
    (loop until (zerop a)
          do (incf ans (mod a 10))
             (setf a (floor a 10)))
    ans))

(loop repeat (read)
      do (format t "~A~%" (solve (read))))
