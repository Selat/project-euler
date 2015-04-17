(defun split-digits (n base)
  (let ((res nil))
    (loop until (zerop n) do
      (setf res (cons (mod n base) res))
      (setf n (floor n base)))
    res))

(defun palindrome-p (digits)
  (equal digits (reverse digits)))

(defun gen-number (digits-list)
  (let ((i 1))
    (loop for d in digits-list
          sum (* i d)
          do (setf i (* i 10)))))

(defun pow (a n)
  (let ((res 1))
    (loop for i from 1 to n do
      (setf res (* res a)))
    res))

;; Brute force method - just check condition from statement for each number from 1 to n.
;; O(n)
(defun solve (n k)
  (loop for i from 1 to n
        sum (if (and (palindrome-p (split-digits i 10))
                     (palindrome-p (split-digits i k)))
                i 0)))

;; Generate just first half of all possible palindromes and then check
;; whether it's palindrome in base k.
;; O(sqrt(n))
(defun solve-fast (n k)
  (loop for i from 1 below (pow 10 (ceiling (length (split-digits n 10)) 2))
        for dl = (split-digits i 10)
        for n1 = (gen-number (append dl (reverse dl)))
        for n2 = (gen-number (append dl (cdr (reverse dl))))
        sum (+ (if (and (<= n1 n)
                        (palindrome-p (split-digits n1 10))
                        (palindrome-p (split-digits n1 k)))
                   n1 0)
               (if (and (<= n2 n)
                        (palindrome-p (split-digits n2 10))
                        (palindrome-p (split-digits n2 k)))
                   n2 0))))

(format t "~A~%" (solve-fast (read) (read)))
