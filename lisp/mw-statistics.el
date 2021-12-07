;;; mw-statistics --- Provide some basic statistics functions to operate on sequences.
;;; Commentary:

;;; Code:

(defun mean(number-list)
  "Provide the average of the elements of NUMBER-LIST."
  (let ((N (length number-list)))
    (apply '+ (mapcar (lambda (num)
                        (/ num (float N))) number-list))))


(defun standard-deviation(number-list)
  "Calculate the deviation from the mean over a population provided in NUMBER-LIST."
  (let ((µ (mean number-list)))
    (sqrt (sum--of-diff-sq number-list µ))))


(defun sum--of-diff-sq(lst mu)
  "Calculate sum of deviation from the mean, MU, divided by total items in LST."
  (let ((N (length lst)))
    (defun recurse (lst)
      (if (null (car lst))
          0
        (+ (/ (expt (- (car lst) mu) 2) N) (recurse (cdr lst)))))

    (recurse lst)))

(provide 'mw-statistics)
;;; mw-statistics.el ends here
