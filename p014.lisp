(defconstant &maxn& 5000000)
(defvar *cache* (make-array (1+ &maxn&) :element-type 'fixnum))
(defvar *max* (make-array (1+ &maxn&) :element-type 'fixnum))

(defun precalc-table (n)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) *cache*)
           (type fixnum n))
  (setf (aref *cache* 1) 1)
  (loop for i from 2 to n do
    (let ((j i)
          (len 0))
      (loop until (< j i)
            do (if (evenp j)
                   (setf j (/ j 2))
                   (setf j (+ (* 3 j) 1)))
               (incf len))
      (setf (aref *cache* i) (+ len (aref *cache* j))))))

(defun precalc-max-table (n)
  (setf (aref *max* 1) (aref *cache* 1))
  (loop for i from 2 to n do
    (setf (aref *max* i) (if (>= (aref *cache* i)
                                (aref *cache* (aref *max* (1- i))))
                             i
                             (aref *max* (1- i))))))

(precalc-table &maxn&)
(precalc-max-table &maxn&)

(loop repeat (read) do
  (format t "~A~%" (aref *max* (read))))
