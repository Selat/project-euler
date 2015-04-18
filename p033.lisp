(defun simplify-frac (num denom)
  (let ((g (gcd num denom)))
    (values (/ num g) (/ denom g))))

(defun good-fraction-p (num denom)
  (and (not (zerop (mod num 10)))
       (not (zerop (mod denom 10)))
       (= (mod num 10) (floor denom 10))
       (not (= num denom))
       (= (/ num denom)
          (/ (floor num 10) (mod denom 10)))))

(defun find-fractions ()
  (let ((res nil))
    (loop for i from 10 to 99 do
      (loop for j from 10 to 99 do
        (when (good-fraction-p i j)
          (setf res (cons (/ i j) res)))))
    res))

(defun solve ()
  (reduce #'* (find-fractions)))
