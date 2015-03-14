(defun sum (n a)
  (let ((b (floor (1- n) a)))
    (/ (* b (1+ b) a) 2)))

(defun solve (n)
  (+ (sum n 3) (sum n 5)
     (- (sum n 15))))

(loop repeat (read) do
      (format t "~A~%" (solve (read))))
