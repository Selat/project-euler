;; The solution uses following formula
;; 1^2 + ... + n^2 = n * (n + 1) * (2n + 1) / 6
;; (n * (n + 1) / 2)^2 - n * (n + 1) * (2n + 1) / 6 = (n - 1) * n * (n + 1) * (3n + 2) / 12

(defun solve (n)
  (floor (* (1- n) n (1+ n) (+ (* 3 n) 2)) 12))

(loop repeat (read) do
      (format t "~A~%" (solve (read))))
