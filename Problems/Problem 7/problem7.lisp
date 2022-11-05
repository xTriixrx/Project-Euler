;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can
;; see that the 6th prime is 13.

;; What is the 10001st prime number?

(defun sieve-of-eratosthenes (n)
  ;; Create array of N+1 size where each value is set as t
  (let ((primes (make-array (+ n 1) :initial-element t)))
    ;; Iterate over factor up until p * p > n and
    ;; set each non prime index to nil
    (do ((p 2)) ((> (* p p) n))
      (if (eql (aref primes p) t)
        (do ((i (* p p))) ((> i n))
          (setf (aref primes i) nil)
          (setf i (+ i p))))
      (incf p))
    ;; Set each remaining 't' value to the index it represents
    (do () ((< n 0))
      (if (eql (aref primes n) t)
        (setf (aref primes n) n))
      (decf n))
    primes))

(defun nth-prime (n)
  (let ((ans 0) (primes 0))
    ;; iterate over i until sieves algorithm returns enough primes
    (do ((i 2)) ((not (eql ans 0)))
      (setf primes (remove nil (sieve-of-eratosthenes (* n i))))
      ;; If enough primes exist, find the nth prime which exists in n + 1.
      (if (> (length primes) (+ n 1))
        (setf ans (aref primes (+ n 1))))
      (incf i))
    ans))
