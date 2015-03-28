(defconstant &maxn& 1000)
(defconstant &mod& 1000000007)
(defvar *fact* (make-array (1+ &maxn&) :element-type 'fixnum))

(defun fill-fact (n)
  (let ((cur 1))
    (loop for i from 1 to n do
      (setf cur (mod (* cur i) &mod&))
      (setf (aref *fact* i) cur))))

(defun binpow (a n)
  (labels ((f (a n mul)
             (cond ((= n 0) a)
                   ((oddp n) (f (mod (* a mul) &mod&) (ash n -1) (mod (* mul mul) &mod&)))
                   (t (f a (ash n -1) (mod (* mul mul) &mod&))))))
    (f 1 n a)))

(defun fact (n)
  (aref *fact* n))

(defun solve (n m)
  (mod (* (fact (+ n m))
          (binpow (fact n) (- &mod& 2))
          (binpow (fact m) (- &mod& 2)))
       &mod&))

(fill-fact &maxn&)

(loop repeat (read)
      do (format t "~A~%" (solve (read) (read))))
