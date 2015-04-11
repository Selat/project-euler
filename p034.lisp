(defun fact (n)
  (labels ((f (n r)
             (if (<= n 1) r
                 (f (1- n) (* n r)))))
    (f n 1)))

(defun digits-fact (n)
  (let ((res 0))
    (loop while (> n 0) do
      (incf res (fact (mod n 10)))
      (setf n (floor n 10)))
    res))

(defun solve (n)
  (loop for i from 10 to n
        for s = (digits-fact i)
        sum (if (zerop (mod s i)) i 0)))

(format t "~A~%" (solve (read)))
