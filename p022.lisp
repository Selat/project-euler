(defconstant &alphabet& "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar *strings* (make-hash-table :test #'equal))

(defun get-char-alphabet-pos (c)
  (1+ (position c &alphabet&)))

(defun get-string-alphabet-cost (s)
  (loop for c across s sum (get-char-alphabet-pos c)))

(defun read-data ()
  (let ((a (make-array (read) :element-type 'string)))
    (loop for i from 0 below (length a) do
      (setf (aref a i) (read-line)))
    a))

(defun prepare-data ()
  (let ((a (sort (read-data) #'string<)))
    (loop for i from 0 below (length a)
          for s = (aref a i) do
            (setf (gethash s *strings*)
                  (* (1+ i) (get-string-alphabet-cost s))))))

(defun solve (s)
  (gethash s *strings*))

(prepare-data)
(loop repeat (read) do
  (format t "~A~%" (solve (read-line))))
