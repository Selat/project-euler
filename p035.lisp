(defconstant &maxprime& 1000000)
(defconstant &primes-num& 100000)
(defvar *primes* (make-array (* 2 &primes-num&) :element-type 'fixnum))
(defvar *primes-h* (make-hash-table))
(defconstant &maxn& 1000000)

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

(defun fill-primes-hashes ()
  (loop for i from 0 until (zerop (aref *primes* i)) do
    (setf (gethash (aref *primes* i) *primes-h*) t)))

(defun prime-p (n)
  (gethash n *primes-h*))

;; Return maximum power k of p such that p^k <= n
(defun get-max-pow (n p)
  (let ((max-pow 1))
    (loop until (> (* p max-pow) n) do
      (setf max-pow (* p max-pow)))
    max-pow))

(defun rotate (n)
  (let ((max-pow (get-max-pow n 10)))
    (+ (floor n max-pow) (* 10 (mod n max-pow)))))

(defun circular-prime-p (n)
  (let ((start-pow (get-max-pow n 10))
        (cur n))
    (loop when (not (prime-p cur)) return nil
            do (setf cur (rotate cur))
          when (< (get-max-pow cur 10) start-pow) return nil
            when (= cur n) return t)))

(defun solve (n)
  (let ((res nil))
    (loop for i from 1 to n
          count (circular-prime-p i))))

(fill-primes &maxprime&)
(fill-primes-hashes)
