(defun get-fibonacci-id (n)
  (ceiling (/ (+ (/ (log 5.0l0) 2.0l0)
                 (* (log 10.0l0) (1- n)))
              (log 1.6180339887498948482l0))))

(loop repeat (read) do
  (format t "~A~%" (get-fibonacci-id (read))))
