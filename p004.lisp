(defvar *palindromes* nil)

(defun reverse-digits (n)
  (labels ((reverse-d (n v)
             (if (zerop n) v
                 (multiple-value-bind (q r)
                     (floor n 10)
                   (reverse-d q (+ (* v 10) r))))))
    (reverse-d n 0)))

(defun get-palindrome (n pow)
  (+ (* n pow) (reverse-digits n)))

(defun can-represent-p (n pow)
  (loop for i from (floor pow 10) below pow
        for x = (floor n i)
        when (and (= (mod n i) 0)
                  (>= x i)
                  (< x pow))
          return t))

(defun fill-palindromes (pow)
  (loop for i from (floor pow 10) below pow
        for pal = (get-palindrome i pow)
        do (if (can-represent-p pal pow)
               (push pal *palindromes*))))

(defun get-max-palindrome (n)
  (loop for i in *palindromes*
        when (< i n) return i))

(fill-palindromes 1000)

(loop repeat (read)
      do (format t "~A~%" (get-max-palindrome (read))))
