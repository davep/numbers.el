;;; numbers.el --- Display information and trivia about numbers
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: games, trivia, maths, numbers
;; URL: https://github.com/davep/numbers.el

;;; Commentary:
;;
;; numbers.el is a little wrapper around http://numbersapi.com/ that can be
;; used to display maths information and trivia about numbers.

(require 'thingatpt)

;;; Code:

(defconst numbers-math-api-url "http://numbersapi.com/%d/math"
  "URL for getting maths information about a number.")

(defconst numbers-trivia-api-url "http://numbersapi.com/%d"
  "URL for getting trivia about a number.")

(defconst numbers-user-agent "numbers.el"
  "User agent to send when requesting number information.")

(defun numbers-get (url number)
  "Visit URL getting information about NUMBER."
  (let* ((url-request-extra-headers `(("User-Agent" . ,numbers-user-agent)))
         (buffer (url-retrieve-synchronously (format url number) t t)))
    (when buffer
      (with-current-buffer buffer
        (setf (point) (point-min))
        (when (search-forward-regexp "^$" nil t)
          (buffer-substring (1+ (point)) (point-max)))))))

(defun numbers-get-math (number)
  "Get some maths information about NUMBER."
  (numbers-get numbers-math-api-url number))

(defun numbers-get-trivia (number)
  "Get some trivia about NUMBER."
  (numbers-get numbers-trivia-api-url number))

(defun numbers-reader ()
  "Get a number from the user.

If `current-prefix-arg' is non-nil its value is returned,
otherwise a number is read form the minibuffer, using any number
`thing-at-point' could find at `point' as the default."
  (or current-prefix-arg (read-number "Number: " (thing-at-point 'number))))

;;;###autoload
(defun numbers-math (number)
  "Display some maths information about NUMBER."
  (interactive (list (numbers-reader)))
  (message "%s" (numbers-get-math number)))

;;;###autoload
(defun numbers-trivia (number)
  "Display some trivia about NUMBER."
  (interactive (list (numbers-reader)))
  (message "%s" (numbers-get-trivia number)))

(provide 'numbers)

;;; numbers.el ends here
