(defun add-digits (n digits)
  (loop for r = (mod n 10)
        when (zerop n) return t
          when (zerop r) return nil
            when (gethash r digits) return nil
              do (setf (gethash r digits) t)
                 (setf n (floor n 10))))

(defun is-multiply (n k)
  (let ((digits (make-hash-table)))
    (loop for i from 1 to k
          when (and (= (hash-table-count digits) k)
                    (not (gethash (1+ k) digits))) return t
            when (not (add-digits (* i n) digits)) return nil)))

(defun solve (n k)
  (let ((res nil))
    (loop for i from 1 to n do
      (if (is-multiply i k)
          (setf res (cons i res))))
    (reverse res)))

;; Form a pandigital number of form 1 * n . 2 * n ...
(defun form-number (n k)
  (let ((res (make-string 0))
        (digits (make-hash-table)))
    (loop for i from 1 to k until (not (add-digits (* n i) digits)) do
      (setf res (concatenate 'string res (write-to-string (* n i)))))
    (parse-integer res)))

(format t "~{~A~%~}" (solve (read) (read)))
