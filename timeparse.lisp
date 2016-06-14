;;;; timeparse.lisp

(in-package #:timeparse)

(defvar +paddable-keywords+
  '(:year :month :day :weekday :hour :minute :second :msec :usec :nsec
    :iso-week-year :iso-week-number :iso-week-day)
  "These symbols are allowed to be in a padding list.")

(defun paddable-keywords-p (thing)
  (find thing +paddable-keywords+))

(deftype paddable-keyword ()
  '(satisfies paddable-keywords-p))

(defun match-entire-target (haystack needle start)
  "Determine if NEEDLE is entirely in HAYSTACK at the position START."
  (let ((match (string= needle haystack
                         :start2 start
                         :end2 (min (length haystack)
                                    (+ start (length needle))))))
    (when match
      (list match (length needle)))))

(defun looking-for (haystack needles start)
  "See if the string immediately at point is part of a list of needles.
Returns position of the needle found in NEEDLES."
  (loop for needle across needles
        for j from 0
        when (and (not (string= needle ""))
                  (match-entire-target needle haystack start))
        return (list j (length needle))))

(defun read-number (haystack digit-count start &optional (padchar #\0))
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

(defun match-fragment (haystack fragment start)
  (etypecase fragment
    (character (match-entire-target haystack (string fragment) start))))
