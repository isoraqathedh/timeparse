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

(defun match-multiple-targets (haystack needles start)
  "See if the string immediately at point is part of a list of needles.
Returns position of the needle found in NEEDLES."
  (loop for needle across needles
        for j from 0
        when (and (not (string= needle ""))
                  (match-entire-target haystack needle start))
        return (list j (length needle))))

(defun match-number (haystack start &key (digit-count 1 digit-count-supplied-p)
                                         (min-digit-count 1) max-digit-count
                                         (padchar #\0))
  "Read a number in HAYSTACK.

Start searching at START.
The scanned number has between DIGIT-COUNT-MIN and DIGIT-COUNT-MAX digits,
or exactly DIGIT-COUNT digits if that is supplied.
The string can be preceded by any number of PADCHARs
that also count toward the maximum."
  (let ((position-after-padchars
          ;; skip past the padding characters.
          (position-if (lambda (thing) (char/= thing padchar)) haystack
                       :start start
                       :end (+ start (cond (digit-count-supplied-p digit-count)
                                           (max-digit-count max-digit-count)
                                           (t 0))))))
    (if digit-count-supplied-p
        (multiple-value-list
         (parse-integer haystack :start position-after-padchars
                                 :end (+ position-after-padchars digit-count)))
        (multiple-value-bind (number digits-used)
            (parse-integer haystack :start position-after-padchars
                                    :end (when max-digit-count
                                           (+ position-after-padchars
                                              max-digit-count))
                                    ;; We can end parsing after we hit any junk.
                                    :junk-allowed t)
          (format t "Got number ~d, using ~r digits~%" number digits-used)
          (cond ((< (- digits-used position-after-padchars) min-digit-count)
                 (error "Not enough digits to make a ~r-digit number."
                        digit-count))
                ((> (- digits-used position-after-padchars) max-digit-count)
                 (error "Too many digits to make a ~r-digit number."
                        digit-count))
                (t (list number (- digits-used start))))))))

(defun match-fragment (haystack fragment start)
  (etypecase fragment
    (character (match-entire-target haystack (string fragment) start))
    (string (match-entire-target haystack fragment start))
    (list (destructuring-bind (spec pad &optional (pad-char #\0)) fragment
            (declare (ignorable spec))
            (read-number haystack pad start pad-char)))
    (paddable-keyword (read-number haystack 1 start))
    (keyword (case fragment
               (:long-month (looking-for haystack +month-names+ start))
               (:short-month (looking-for haystack +short-month-names+ start))
               (:long-weekday (looking-for haystack +day-names+ start))
               (:short-weekday (looking-for haystack +short-day-names+ start))
               (:ampm (looking-for haystack #("am" "pm") start))))))
