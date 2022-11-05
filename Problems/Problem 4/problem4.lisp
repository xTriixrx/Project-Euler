;; A palindromic number reads the same both ways. The largest palindrome
;; made from the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.

(defun is-palindrome (x)
  (let ((result t)
        (num (write-to-string x)))
    ;; Initialize i & j to be start and end index of string, respectively.
    (do ((i 0) (j (- (length num) 1)))
      ;; Terminating condition being if either result being false or i > j;
      ;; Meaning that either some characters were not eql or every char was checked.
      ((or (not result) (not (< i j))))
      ;; Set result to be t or nil if chars are equal or not respectively.
      (setf result (eql (char num i) (char num j)))
      (incf i)
      (decf j))
    result))

(defun largest-palindrome (x)
  ;; Set local bounds and answer store
  (let ((l-bound (expt 10 (- x 1)))
        (h-bound (- (expt 10 x) 1))
        (answer 0))
    ;; Set outter loop outer iterator to high bound
    (do ((outer h-bound))
      ;; End outer loop once outer is less than low bound
      ((< outer l-bound))
      ;; Set inner loop inner iterator to outer
      (do ((inner outer) (break-inner nil))
        ;; End inner loop once inner is less than low bound or break-inner is t.
        ((or (< inner l-bound) break-inner))
        ;; Get current num and if palindrome and larger than current answer,
        ;; save as new answer, set break-inner to t and if inner is greater than
        ;; low bound, set low bound to inner.
        (let ((num (* outer inner)))
          (if (and (is-palindrome num) (> num answer))
            (progn
             (setf answer num)
             (setf break-inner t)
             (if (> inner l-bound) (setf l-bound inner)))))
        (decf inner))
      (decf outer))
    answer))
