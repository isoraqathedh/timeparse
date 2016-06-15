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

(define-condition match-failure (error) ()
  (:documentation "Generic matching error."))

(define-condition match-fallthrough-error (match-failure)
  ((needle :initarg :needles
           :reader needles)
   (haystacks :initarg :haystack
              :reader haystack))
  (:documentation "Error raised when trying to match needle(s) but fail.")
  (:report (lambda (condition stream)
             (format stream "Match failed: wanted ~:[~;one of ~]~s, got ~s"
                     (typep (needles condition) '(or list (vector string)))
                     (needles condition)
                     (haystacks condition)))))

(define-condition match-number-error (match-failure)
  ((min-digits :initarg :min-digits
               :initform 0
               :reader min-digits)
   (max-digits :initarg :max-digits
               :initform nil
               :reader max-digits)
   (parsed-number :initarg :parsed-number
                  :reader parsed-number))
  (:documentation
   "Error raised when the number of digits parsed is out of bounds.")
  (:report (lambda (condition stream)
             (format stream
                     "Match failed: parsed number ~a has ~a digit~:p, ~
                      not ~a ~:[or more~;- ~:*~a~]."
                     (parsed-number condition)
                     (length (format nil "~a" (parsed-number condition)))
                     (min-digits condition)
                     (max-digits condition)))))

(defun match-entire-target (haystack needle start)
  "Determine if NEEDLE is entirely in HAYSTACK at the position START."
  (let ((match (string= needle haystack
                        :start2 start
                        :end2 (min (length haystack)
                                   (+ start (length needle))))))
    (if match
        (list match (length needle))
        (error 'match-fallthrough-error :needles needle :haystack haystack))))

(defun match-multiple-targets (haystack needles start)
  "See if the string immediately at point is part of a list of needles.
Returns position of the needle found in NEEDLES."
  (loop for needle across needles
        for j from 0
        when (and (not (string= needle ""))
                  (handler-case (match-entire-target haystack needle start)
                    (match-fallthrough-error nil)))
        return (list j (length needle))
        finally (error 'match-fallthrough-error :needles needles
                                                :haystack haystack)))

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
          (or (position-if (lambda (thing) (char/= thing padchar)) haystack
                           :start start
                           :end (+ start
                                   (cond (digit-count-supplied-p digit-count)
                                         (max-digit-count max-digit-count)
                                         (t 0))))
              start)))
    (if digit-count-supplied-p
        (multiple-value-bind (number digits-used)
            (parse-integer haystack
                           :start position-after-padchars
                           :end (+ position-after-padchars digit-count))
          (list number (- digits-used start)))
        (multiple-value-bind (number digits-used)
            (parse-integer haystack :start position-after-padchars
                                    :end (when max-digit-count
                                           (+ position-after-padchars
                                              max-digit-count))
                                    ;; We can end parsing after we hit any junk.
                                    :junk-allowed t)
          (if (<= min-digit-count (- digits-used start) (or max-digit-count most-positive-fixnum))
              (list number (- digits-used start))
              (error 'match-number-error :parsed-number number
                                         :min-digits min-digit-count
                                         :max-digits max-digit-count))))))

(defun match-fragment (haystack fragment start)
  (etypecase fragment
    (character (match-entire-target haystack (string fragment) start))
    (string (match-entire-target haystack fragment start))
    (list (destructuring-bind (spec pad &optional (pad-char #\0)) fragment
            (declare (ignorable spec))
            (match-number haystack start :min-digit-count pad :padchar pad-char)))
    (paddable-keyword (match-number haystack start))
    (keyword (case fragment
               (:long-month
                (match-multiple-targets haystack +month-names+ start))
               (:short-month
                (match-multiple-targets haystack +short-month-names+ start))
               (:long-weekday
                (match-multiple-targets haystack +day-names+ start))
               (:short-weekday
                (match-multiple-targets haystack +short-day-names+ start))
               (:ampm
                (match-multiple-targets haystack #("am" "pm") start))
               (:ordinal-day
                (let* ((number-component
                         (match-number haystack start :max-digit-count 2))
                       (ordinal-component
                         (match-multiple-targets
                          haystack
                          #("st" "nd" "rd" "th")
                          (+ start (second number-component)))))
                  (list (first number-component)
                        (+ (second ordinal-component)
                           (second number-component)))))
               (:hour12 (match-number haystack start :max-digit-count 2))
               ((:gmt-offset :gmt-offset-or-z :gmt-offset-hhmm)
                (let (characters-parsed offset-hour offset-minute met-colon-p)
                  ;; Deal with Z first:
                  (when (handler-case (match-entire-target haystack "Z" start)
                          (match-fallthrough-error nil))
                    (return-from match-fragment (list "Z" 1)))
                  ;; Match the hour:
                  (destructuring-bind (match count)
                      (match-number haystack start :digit-count 2)
                    (setf offset-hour match)
                    (incf characters-parsed count))
                  ;; Match the colon
                  (when (handler-case (match-entire-target haystack ":"
                                                           (+ start characters-parsed))
                          (match-fallthrough-error nil))
                    (incf characters-parsed)
                    (setf met-colon-p t))
                  ;; Match the minute
                  (destructuring-bind (match count)
                      (match-number haystack (+ start characters-parsed)
                                    :digit-count 2)
                    (setf offset-minute match)
                    (incf characters-parsed count))
                  (list (list offset-hour offset-minute met-colon-p)
                        characters-parsed)))
               ((:timezone :minimal-weekday)
                (error "Option ~a not implemented" fragment))))))

(defun parse-timestring-into-list (timestring format)
  "Reads TIMESTRING into its FORMAT components."
  (loop with out = ()
        with point = 0
        for fragment in format
        for (parse advance) = (match-fragment timestring fragment point)
        do (incf point advance)
           (typecase fragment
               ((member :gmt-offset :gmt-offset-hhmm :gmt-offset-or-z)
                (cond ((listp parse)
                       (setf (getf out :offset-hour (first parse))
                             (getf out :offset-minute (second parse))))
                      ((string-equal parse "Z")
                       (setf (getf out :offset-hour 0)
                             (getf out :offset-minute 0)))))
               (keyword
                (setf (getf out fragment) parse))
               ((or character string))
               (list
                (setf (getf out (first fragment)) parse)))
        finally (return (list out point))))
