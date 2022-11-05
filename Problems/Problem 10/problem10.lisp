;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;; Find the sum of all the primes below two million.

;; Use (- (reduce '+ (remove nil (sieve-of-eratosthenes 2000000))) 1)

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
