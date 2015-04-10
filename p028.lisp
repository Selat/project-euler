(defconstant &mod& 1000000007)

(defun solve (n)
  (let ((m (floor n 2)))
    (mod (+ 1
            (* 4 (1- (/ (* (1+ m) (1+ (* 2 m)) (+ (* 2 m) 3)) 3)))
            (* -6 m (1+ m)))
         &mod&)))

(loop repeat (read) do
  (format t "~A~%" (solve (read))))
