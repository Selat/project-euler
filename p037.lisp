(defconstant &maxn& 1000000)

(defun gen-number (digits-list)
  (let ((i 1))
    (loop for d in (reverse digits-list)
          sum (* i d)
          do (setf i (* i 10)))))

;; Return maximum power k of p such that p^k <= n
(defun get-max-pow (n p)
  (let ((max-pow 1))
    (loop until (> (* p max-pow) n) do
      (setf max-pow (* p max-pow)))
    max-pow))

(defun prime-p (n)
  (if (= n 1) nil
      (loop for i from 2
            when (> (* i i) n) return t
              when (zerop (mod n i)) return nil)))

(defun check-right-prime (n)
  (loop when (zerop n) return t
        when (not (prime-p n)) return nil
          do (setf n (floor n 10))))

(defun get-primes (suffix)
  (let ((res nil))
    (loop for i from 1 to 9
          for cur = (cons i suffix) do
            (when (check-right-prime (gen-number cur))
              (setf res (cons cur res))))
    res))

(defun get-next-suffixes (suffixes)
  (loop for s in suffixes append
        (get-primes s)))

(defun fill-primes (n)
  (let* ((cur '((2) (3) (5) (7)))
         (res nil)
         (max-pow (get-max-pow n 10)))
    (loop until (or (> (length (car cur)) max-pow) (null cur)) do
      (setf cur (get-next-suffixes cur))
      (setf res (append (mapcar #'gen-number cur) res)))
    (reverse res)))

(defun solve (n primes)
  (loop for x in primes until (>= x n)
        sum x))

(format t "~A~%" (solve (read) (fill-primes &maxn&)))
