;; Just bruteforce precalc

(defvar *digit-pows* (make-hash-table))
(defvar *nums* (make-hash-table))
(defconstant &maxn& 9999999)

(defun get-digits (a)
  (if (> a 0) (cons (mod a 10) (get-digits (floor a 10)))))

(defun fill-digit-pows (n)
  (clrhash *digit-pows*)
  (loop for i from 0 to 9 do
    (setf (gethash i *digit-pows*)
          (pow i n))))

(defun pow (a n)
  (let ((res 1))
    (loop for i from 1 to n do
      (setf res (* res a)))
    res))

(defun sum-digits (a)
  (reduce #'+ (mapcar (lambda (a) (gethash a *digit-pows*))
                      (get-digits a))))

(defun prefill-nums ()
  (let ((res (make-hash-table)))
    (loop for len from 3 to 6
          for curres = nil
          do
      (fill-digit-pows len)
      (loop for i from 1 to &maxn& do
        (if (= (sum-digits i) i)
            (setf curres (cons i curres))))
      (setf (gethash len res) curres))
    res))

(defun fill-hash-nums ()
  (setf (gethash 3 *nums*) '(407 371 370 153))
  (setf (gethash 4 *nums*) '(9474 8208 1634))
  (setf (gethash 5 *nums*) '(194979 93084 92727 54748 4151 4150))
  (setf (gethash 6 *nums*) '(548834)))

(defun solve (n)
  (reduce #'+ (gethash n *nums*)))

(fill-hash-nums)
(format t "~A~%" (solve (read)))
