(defconstant &maxn& 21000)
(defconstant &maxprime& 1000)
(defvar *primes* (make-array &maxprime& :element-type 'fixnum))
(defvar *possible-to-represent* (make-hash-table))

(defun fill-primes (n)
  (let ((used (make-array (1+ (ceiling n 2)) :element-type 'bit :initial-element 0))
        (cur-prime-id 0))
    (setf (aref *primes* 0) 2)
    (loop for i from 1 below (ceiling n 2)
          for num = (1+ (* 2 i))
          do (when (= (aref used i) 0)
               (setf (aref *primes* (incf cur-prime-id)) num)
               (loop for j from num to n by num
                     for id = (floor j 2)
                     do (when (= (mod j 2) 1) (setf (aref used id) 1)))))))


(defun sum-divisors (n)
  (let ((res 1)
        (curn n))
    (- (* (loop named outer for p across *primes*
                when (> (* p p) n) return res
                  do (when (zerop (mod n p))
                       (let ((num 0)
                             (tmp p))
                         (loop while (zerop (mod curn tmp))
                               do (incf num)
                                  (setf tmp (* tmp p)))
                         (setf curn (/ curn (/ tmp p)))
                         (setf res (* res (/ (1- tmp) (1- p))))
                         (if (= curn 1) (return-from outer res)))))
          (if (> curn 1) (1+ curn) 1))
       n)))

(defun find-abundant-numbers (n)
  (let ((res nil))
    (loop for i from 1 to n do
      (if (> (sum-divisors i) i)
          (push i res)))
    (make-array (length res) :element-type 'fixnum :initial-contents (reverse res))))

(defun fill-nums (n)
  (let ((nums (find-abundant-numbers n)))
    (loop for i from 0 below (length nums)
          for a = (aref nums i) do
            (loop for j from i until (> (+ a (aref nums j)) n) do
              (setf (gethash (+ a (aref nums j)) *possible-to-represent*) t)))))

(defun solve (n)
  (if (<= n &maxn&)
      (gethash n *possible-to-represent*)
      t))

(fill-primes &maxprime&)
(fill-nums &maxn&)

(loop repeat (read) do
  (format t "~A~%" (if (solve (read))
                       "YES"
                       "NO")))
