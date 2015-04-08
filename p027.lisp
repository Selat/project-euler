(defconstant &maxprime& 3000)
(defconstant &primes-num& 320)
(defvar *primes* (make-array (* 2 &primes-num&) :element-type 'fixnum))

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

(defun is-prime (n)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum n)
           (type (simple-array fixnum) *primes*))
  (labels ((get-id (l r)
             (declare (type unsigned-byte l)
                      (type unsigned-byte r))
             (if (>= l r) (aref *primes* l)
                 (let ((m (ceiling (+ l r) 2)))
                   (if (< n (aref *primes* m))
                       (get-id l (1- m))
                       (get-id m r))))))
    (= (get-id 0 (1- &primes-num&)) n)))

(defun count-primes-num (a b)
  (loop for i from 0 to 100
        when (not (is-prime (+ (* i i) (* a i) b))) return i))

(defun find-a (n b)
  (let ((nodd (if (evenp n) (1- n) n))
        (neven (if (oddp n) (1- n) n))
        (max 0)
        (max-id 0))
  (if (evenp b)
      (loop for i from (- neven) to neven by 2
            for cur = (count-primes-num i b) do
              (when (> cur max) (setf max cur) (setf max-id i)))
      (loop for i from (- nodd) to nodd by 2
            for cur = (count-primes-num i b) do
              (when (> cur max) (setf max cur) (setf max-id i))))
    (values max max-id)))

(defun solve (n)
  (let ((max 0)
        (max-a 0)
        (max-b 0))
    (setf n (min n 1700))
    (loop for i from 0 below &primes-num&
          for p = (aref *primes* i)
          until (> p n) do
            (multiple-value-bind (cur id) (find-a n p)
              (when (> cur max) (setf max cur)
                    (setf max-a id)
                    (setf max-b p)))
            (multiple-value-bind (cur id) (find-a n (- p))
              (when (> cur max) (setf max cur)
                    (setf max-a id)
                    (setf max-b (- p)))))
    (values max-a max-b)))

(fill-primes &maxprime&)

(multiple-value-bind (a b) (solve (read)) (format t "~A ~A~%" a b))
