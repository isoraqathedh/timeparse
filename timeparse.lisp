;;;; timeparse.lisp

(in-package #:timeparse)

(defvar +listable-keywords+
  '(:year :month :day :weekday :hour :minute :second :msec :usec :nsec
    :iso-week-year :iso-week-number :iso-week-day)
  "A list of keywords that can be in a list that represents padding.")

(define-condition no-padding-allowed (error)
  ((offending-symbol :initarg :offending-symbol
                     :reader offending-symbol))
  (:documentation "Raise this error when an option is in a padding list
where it should not be.")
  (:report (lambda (condition stream)
             (format stream "No padding list is allowed for the specifier ~a."
                     (offending-symbol condition)))))

(defun date-max-space (fragment)
  "Determine the maximum character width that a time unit can take up.
Padding is not considered.
If there is no limit, then return nil."
  (ecase fragment
    (:year nil) (:month 2)  (:day 2) (:weekday 1)
    (:hour 2)   (:minute 2) (:second 2)
    (:msec 3)   (:usec 6)   (:nseq 9)
    (:iso-week-year nil)
    (:iso-week-number 2)
    (:iso-week-day 1)))

(defun create-matcher-fragment (fragment)
  "Create a corresponding matcher fragment for a matcher fragment.

A matcher fragment is a bit of S-expression
that can be joined together to eventually be passed through to `CL-PPCRE:SCAN'."
  (cond ((or (stringp fragment)
             (characterp fragment)) fragment)
        ((listp fragment)
         (destructuring-bind (spec &optional (pad-length 1) (pad-char #\0))
             fragment
           (if (member spec +listable-keywords+)
               `(:sequence
                 (:non-greedy-repetition 0 ,(1- pad-length) ,pad-char)
                 (:register (:greedy-repetition 1 ,(date-max-space spec)
                                                :digit-class)))
               (error 'no-padding-allowed :offending-symbol spec))))
        ((member fragment +listable-keywords+)
         `(:register (:greedy-repetition 1 ,(date-max-space fragment) :digit-class)))
        ((keywordp fragment)
         (ecase fragment
           (:long-month
            `(:register (:alternation ,@(cdr (coerce +month-names+ 'list)))))
           (:short-month
            `(:register (:alternation ,@(cdr
                                         (coerce +short-month-names+ 'list)))))
           (:long-weekday
            `(:register (:alternation ,@(coerce +day-names+ 'list))))
           (:short-weekday
            `(:register (:alternation ,@(coerce +short-day-names+ 'list))))
           (:hour12
            `(:register (:alternation ,@(loop for i from 1 to 12
                                              collect (princ-to-string i)))))
           (:ampm
            `(:register (:alternation "am" "pm")))
           ((:gmt-offset :gmt-offset-or-Z :gmt-offset-hhmm)
            `(:register
              (:alternation
               "Z"
               (:sequence
                (:alternation #\+ #\-) :digit-class :digit-class
                (:greedy-repetition 0 1 #\:) :digit-class :digit-class))))
           ((:timezone
             `(:register (:greedy-repetition 1 nil (:RANGE #\A #\Z)))))))))

(defun timestring-format->ppcre-parse-tree (timestring-format &optional anchored)
  "Creates a CL-PPCRE parse tree that matches all time strings
that the format would produce.

May match something that may not be possible."
  (concatenate 'list
               '(:sequence)
               (if anchored '(:start-anchor))
               (mapcar #'create-matcher-fragment timestring-format)
               (if anchored '(:end-anchor))))
