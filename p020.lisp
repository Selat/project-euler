(defun solve (n)
  (let ((fact 1)
        (ans 0))
    (loop for i from 1 to n do
      (setf fact (* fact i)))
    (loop while (> fact 0) do
      (incf ans (mod fact 10))
      (setf fact (floor fact 10)))
    ans))

(loop repeat (read) do
  (format t "~A~%" (solve (read))))
