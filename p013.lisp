(defconstant &maxn& 100)

(defun read-num ()
  (let ((s (read-line))
        (res (make-array 50 :element-type 'fixnum :initial-element 0)))
    (loop for i from 0 below (length s)
          do (setf (aref res i) (digit-char-p (aref s (- (length s) i 1)))))
    res))

(defmacro sum-range (a b l)
  `(loop for i from ,a below ,b
         do (multiple-value-bind (q r)
                (floor (+ ,@(loop for x in l
                                  collect (list 'aref x 'i)))
                       10)
              (setf (aref res i) r)
              (setf (aref res (1+ i)) q))))

(defun sum-numbers (a b)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) a)
           (type (simple-array fixnum) b))
  (let* ((len (min (length a) (length b)))
         (res (make-array (1+ (max (length a) (length b))) :element-type 'fixnum :initial-element 0)))
    (sum-range 0 len (res a b))
    (if (< len (length a))
        (sum-range len (length a) (res a))
        (sum-range len (length b) (res b)))
    res))

(defun copy-array (a b)
  (loop for i from 0 below (min (length a) (length b))
        do (setf (aref a i) (aref b i))))

(defun solve (n)
  (let ((res (make-array &maxn& :element-type 'fixnum))
        (first (read-num)))
    (copy-array res first)
    (loop repeat (1- n)
          do (copy-array res (sum-numbers res (read-num))))
    (loop for i from (1- &maxn&) downto 0
          when (not (zerop (aref res i)))
            return (reverse (coerce (subseq res (- i 9) (1+ i)) 'list)))))


(format t "~{~A~}~%" (solve (read)))
