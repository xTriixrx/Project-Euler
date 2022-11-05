;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;; a^2 + b^2 = c^2
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

;; Using square sum relation of pythagorean triplets to write numbers in terms
;; of m and n:
;; a = m^2 - n^2
;; b = 2 * m * n
;; c = m^2 + n^2
;; because,
;; a^2 = m^4 + n^4 - 2 * m^2 * n^2
;; b^2 = 4 * m^2 * n^2
;; c^2 = m^4 + n^4 + 2 * m^2 * n^2

;; To find a*b*c using pythagorean-triplets-equals, use reduce.
;; (reduce '* (pythagorean-triplets-equals 1000))

(defun pythagorean-triplets-equals (sum)
  (let ((a 0) (b 0) (c 0) (m 2) (triplet '()))
    ;; Stop loop once triplet with matching sum is found or if m > sum
    (do () ((or (not (eql triplet '())) (> m sum)))
      ;; Iterate n up to m
      (do ((n 1)) ((>= n m))
        ;; Generate a, b, and c using square sum relation
        (setf a (- (* m m) (* n n)))
        (setf b (* 2 (* m n)))
        (setf c (+ (* m m) (* n n)))
        ;; If a + b + c matches sum, set triplet as list to be returned
        (if (eql (+ a (+ b c)) sum)
          (setf triplet (cons a (cons b (cons c nil)))))
        (incf n))
      (incf m))
    triplet))
