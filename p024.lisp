(defconstant &alphabet& "abcdefghijklmnopqrstuvwxyz")
(defvar *string* "abcdefghijklm")

(defun factorial (n)
  (let ((res 1))
    (loop for i from 1 to n do
      (setf res (* res i)))
    res))

(defun get-letter-by-id (id)
  (aref &alphabet& id))

(defun get-unused-id (used id)
  (let ((curid (1+ id)))
    (loop for i from 0 below (length used) do
            (if (not (aref used i)) (decf curid))
          when (zerop curid) return i)))

(defun get-permutation (str n)
  (let ((res str)
        (f (factorial (length str)))
        (used (make-array (length str) :element-type 'boolean :initial-element nil))
        (curid))
    (loop for i from 0 below (length str) do
      (setf f (/ f (- (length str) i)))
      (setf curid (get-unused-id used (floor (max (1- n) 0) f)))
      (setf (aref used curid) t)
      (setf (aref res i) (get-letter-by-id curid))
      (setf n (- n (* f (floor (max (1- n) 0) f)))))
    res))

(loop repeat (read) do
  (format t "~A~%" (get-permutation *string* (read))))
