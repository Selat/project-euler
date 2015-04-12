(defun get-digits (a)
  (if (> a 0) (cons (mod a 10) (get-digits (floor a 10)))))

(defun find-max (a)
  (loop for i in a maximize i))

(defun is-pandigital (n a b c)
  (let ((da (get-digits a))
        (db (get-digits b))
        (dc (get-digits c)))
    (and (null (intersection da db))
         (null (intersection da dc))
         (null (intersection db dc))
         (= (find-max (append da db dc)) n)
         (= (length (append da db dc)) n))))

(defun pow (a n)
  (let ((res 1))
    (loop for i from 1 to n do
      (setf res (* res a)))
    res))

(defun solve (n)
  (loop for i from 1 to (pow 10 (ceiling n 2)) sum
    (loop for j from i to (pow 10 (ceiling n 2)) sum
                                                 (if (is-pandigital n i j (* i j)) (progn ;;(format t "~A ~A~%" i j)
                                                                                     (* i j)) 0))))

(print (solve (read)))
