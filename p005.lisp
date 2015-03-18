(defun solve (n)
  (let ((res 1))
    (loop for i from 1 to n do
      (setf res (* res (/ i (gcd res i)))))
    res))

(loop repeat (read) do
      (format t "~A~%" (solve (read))))
