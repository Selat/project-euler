;; This file contanins some usefull functions for working with dates.

(defvar *months* (make-array 12 :element-type 'fixnum
                                :initial-contents '(31 28 31 30
                                                    31 30 31 31
                                                    30 31 30 31)))

(defun leap-year-p (y)
  (and (zerop (mod y 4))
       (or (not (zerop (mod y 100)))
           (zerop (mod y 400)))))

(defun get-month-length (year month)
  (if (and (leap-year-p year) (= month 2))
      (1+ (aref *months* (1- month)))
      (aref *months* (1- month))))

(defun get-before-year-days-num (y)
  (setf y (- y 1900))
  (+ (* y 365)
     ;; This is one extra day for each leap year
     (floor (1- y) 4)
     ;; Centuries are not a leap years
     (- (floor (1- y) 100))
     ;; But centuries that are divisible by 400 are leap years
     ;; We add 299 to y because our start year is 1900 which has
     ;; remainder 300 after division by 400.
     (floor (+ y 299) 400)))

(defun get-before-month-days-num (year month)
  (loop for i from 1 below month sum (get-month-length year i)))

(defun get-before-day-days-num (day)
  (1- day))

(defun get-day-number (year month day)
  (+ (get-before-year-days-num year)
     (get-before-month-days-num year month)
     (get-before-day-days-num day)))

(defun get-week-day (year month day)
  (1+ (mod (get-day-number year month day) 7)))

(defun sunday-p (year month day)
  (= (get-week-day year month day) 7))

(defun solve (year1 month1 day1 year2 month2 day2)
  (when (> day1 1) (setf month1 (1+ month1)))
  (if (= year1 year2)
      (loop for m from month1 to month2
            count (sunday-p year1 m 1))
      (+ (loop for m from month1 to 12
               count (sunday-p year1 m 1))
         (loop for y from (1+ year1) below year2
               sum (loop for m from 1 to 12
                         count (sunday-p y m 1)))
         (loop for m from 1 to month2
               count (sunday-p year2 m 1)))))

(loop repeat (read) do
  (format t "~A~%" (solve (read) (read) (read)
                          (read) (read) (read))))
