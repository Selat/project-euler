(defconstant &maxn& 100000)
(defconstant &mod& 1000000007)
(defconstant &coins-num& 8)
(defvar *dp* (make-array (1+ &maxn&) :element-type 'fixnum))
(defvar *coins* (make-array &coins-num& :initial-contents '(1 2 5 10 20 50 100 200)))

(defun solve (n)
  (setf (aref *dp* 0) 1)
  (loop for i from 1 to n do
    (setf (aref *dp* i) 0))
  (loop for i from 0 below &coins-num&
        for coin-tenor = (aref *coins* i) do
          (loop for j from coin-tenor to n do
            (incf (aref *dp* j) (aref *dp* (- j coin-tenor)))
            (setf (aref *dp* j) (mod (aref *dp* j) &mod&)))))

(defun get-answer (n)
  (aref *dp* n))

(solve &maxn&)
(loop repeat (read) do
  (format t "~A~%" (get-answer (read))))
