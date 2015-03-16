(defun solve (n)
  (let ((res 1)
        (end (ceiling (sqrt n))))
    (loop for i from 2 to end
          do (if (= (mod n i) 0)
                 (progn (setf res i)
                        (loop while (= (mod n i) 0)
                              do    (setf n (/ n i))))))
    (max res n)))

(loop repeat (read) do
      (format t "~a~%" (solve (read))))
