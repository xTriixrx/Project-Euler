;; 2520 is the smallest number that can be divided by each of the numbers from
;; 1 to 10 without any remainder. What is the smallest positive number that is
;; evenly divisible by all of the numbers from 1 to 20?

(defun smallest-common-lcm (i n)
  (let ((ans 1))
    ;; loop until i > n
    (do () ((> i n))
      ;; LCM(ans, i) = ans * i / gcd(ans, i)
      (setf ans (/ (* ans i) (gcd ans i)))
      (incf i))
    ans))
