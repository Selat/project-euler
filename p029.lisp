(defun get-distinct-num (k n)
  (let ((h (make-hash-table)))
    (loop for i from 1 to k do
      (loop for j from 2 to n do
        (setf (gethash (* i j) h) t)))
    (hash-table-count h)))

(defun solve (n)
  (let ((used (make-array (1+ n) :initial-element nil))
        (ans 0))
    (loop for i from 2 to n do
      (when (not (aref used i))
        (let ((k 0)
              (c i))
          (loop until (> c n) do
            (incf k)
            (setf (aref used c) t)
            (setf c (* c i)))
          (incf ans (get-distinct-num k n)))))
    ans))

(format t "~A~%" (solve (read)))
