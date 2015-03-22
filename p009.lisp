;; This solution uses the following statement:
;; Any Pythagorean triplet can be represented in the following way:
;; a = d * (m * m - n * n)
;; b = d * 2 * m * n
;; c = d * (m * m + n * n)
;; where d, n, m are some natural numbers.
;; Therefore a + b + c = N = 2 * m * (m + n) * d

(defun get-sides-mul (m n d)
  (* (- (* m m) (* n n))
     (* 2 m n)
     (+ (* m m) (* n n))
     d d d))

(defun get-res (m total)
  (let ((res -1))
    (loop for n from 1 to (min (- total m) (1- m))
          do (when (zerop (mod total (+ n m)))
               (setf res (max res (get-sides-mul m n (/ total (+ n m)))))))
    res))

(defun solve (n)
  (if (= (mod n 2) 1) -1
      (let ((res -1)
            (max-m (floor (sqrt (/ n 2)))))
        (loop for m from 1 to max-m
              do (when (zerop (mod (/ n 2) m))
                   (setf res (max res (get-res m (/ n 2 m))))))
        res)))

(loop repeat (read)
      do (format t "~A~%" (solve (read))))

;; Just to check correctness of fast solution
(defun brute-force (n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum n))
  (let ((res -1))
    (loop for a from 1 to (floor n 3) do
      (loop for b from 1 to (floor n 2)
            for c = (- n a b)
            do (when (= (* c c) (+ (* a a) (* b b)))
                 (setf res (max res (* a b c))))))
    res))
