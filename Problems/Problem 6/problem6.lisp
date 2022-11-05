;; The sum of the squares of the first ten natural numbers is,
;; 1^2 + 2^2 + ... + 10^2 = 385
;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + ... + 10)^2 = 55^2 = 3025
;; Hence the difference between the sum of the squares of the first ten natural
;; numbers and the square of the sum is,
;; 3025 − 385 = 2640.
;; Find the difference between the sum of the squares of the first one hundred
;; natural numbers and the square of the sum.

(defun sum-of-squares (n)
  (let ((ans 0))
    ;; initialize i and stop once i > n
    (do ((i 1)) ((> i n))
      ;; ans += i^2
      (setf ans (+ ans (expt i 2)))
      (incf i))
    ans))

(defun square-of-sum (n)
  (let ((ans 0))
    ;; initialize i and stop once i > n
    (do ((i 1)) ((> i n))
      ;; ans += i
      (setf ans (+ ans i))
      (incf i))
    ;; return ans^2
    (expt ans 2)))

(defun sum-square-difference (n)
  ;; Take difference of sums
  (- (square-of-sum n) (sum-of-squares n)))
