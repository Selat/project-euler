(defconstant &maxn& 10000)
(defvar *ans* (make-array (1+ &maxn&) :element-type 'fixnum))

(defun get-period-length (n)
  (let ((r (make-hash-table))
        (val 1)
        (pos 0))
    (loop until (or (gethash val r) (zerop val)) do
      (setf (gethash val r) pos
            val (mod (* val 10) n))
      (incf pos))
    (if (zerop val) 0 (- pos (gethash val r)))))

(defun fill-answers (n)
  (let ((maxl 0)
        (maxid 0))
    (loop for i from 1 to n
          for len = (get-period-length i) do
            (when (> len maxl)
              (setf maxl len maxid i))
      (setf (aref *ans* i) maxid))))

(defun solve (n)
  (aref *ans* (1- n)))

(fill-answers &maxn&)

(loop repeat (read) do
  (format t "~A~%" (solve (read))))
