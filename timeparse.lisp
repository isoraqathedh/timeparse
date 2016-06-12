;;;; timeparse.lisp

(in-package #:timeparse)

(defun match-entire-target (needle haystack start)
  "Determine if NEEDLE is entirely in HAYSTACK at the position START."
  (string= needle haystack :start2 start :end2 (min (length haystack)
                                                    (+ start (length needle)))))

(defun looking-for (needles haystack start)
  "See if the string immediately at point is part of a list of needles.
Returns position of the needle found in NEEDLES."
  (loop for needle across needles
        for j from 0
        when (and (not (string= needle ""))
                  (match-entire-target needle haystack start))
        return j))

(defun read-number (digit-count haystack start &optional (padchar #\0))
  "Read a number with at least DIGIT-COUNT digits."
  (let ((position-after-padchars
          (position-if (lambda (thing) (char/= thing padchar)) haystack
                       :start start)))
    (multiple-value-bind (number digits-used)
        ;; Possible problem point: can sometimes take in more than specified.
        ;; This might be expected sometimes but not always.
        (parse-integer haystack :start position-after-padchars
                                :junk-allowed t)
      (when (< (- digits-used start) digit-count)
        (error "Not enough digits to make a ~r-digit number." digit-count))
      (list number (- digits-used start)))))


