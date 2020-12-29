;;; c2-rowing.el --- Record rowing session data into an org-mode table.
;;;
;;; Commentary:
;;;
;;; 
;;; Code:

(require 'org-table)
(require 'ert)

;;;###autoload
(defun c2-rowing/enter-data()
  "Prompt for data from a rowing session to be entered into a table.

This data will be entered into a table row at point.  The data
entered will also be used to calculate an adjusted caloric
expenditure.

This function will ask for the following data:
   * Date of the session
   * Time session was initiated
   * Duration of the session, in minutes
   * Distance rowed, in meters
   * Weight of the rower at time of session, in pounds
   * Raw caloric expenditure per hour reported by the rowing computer
   * Strokes per minute

This function will calculate the following data:
   * Adjusted calories per hour (adjusted for weight)
   * Adjusted caloric expenditure for this workout

The formulas used to calculate calorie expenditure are as follows:
    1. True Calories/hour burned = Calories on the PM - 300 + (1.714 * weight)
    2. Calorie burn for your workout = (True Calories/hour burned * duration in seconds)/3600

Formulas obtained from the Concept2 website:
   https://www.concept2.com/indoor-rowers/training/calculators/calorie-calculator"
  (interactive)
  (let* ((date (read-string "Enter date of session: "))
         (time (read-string "Start time: "))
         (duration (read-number "Duration (minutes): ")) ; convert from minutes to seconds
         (dist (read-string "Distance (m): "))
         (wt (read-number "Weight (lb): "))
         (raw-cal (read-number "Avg Calories/Hour: "))
         (strokes (read-string "Strokes/minute: "))
         (true-calories (true--calories-per-hour raw-cal wt))
         (cal-burn (total--cals-burned true-calories duration)))
    (insert date) ; prime the loop
    (dolist (datum (list time (number-to-string duration) dist (number-to-string wt) (number-to-string raw-cal) strokes (number-to-string true-calories) (format "%0.2f" cal-burn)))
      (org-table-next-field)
      (insert datum))
    (org-table-next-field)))


(defun true--calories-per-hour (raw-cals-per-hour wt)
  "Calculate calories/hour adjusted for weight.

This calculation relies on the average calories/hour,
RAW-CALS-PER-HOUR, and the weight of the rower, WT, at the time
of workout."
  (+ (- raw-cals-per-hour 300) (* 1.714 wt)))


(defun total--cals-burned (true-cals-per-hour duration)
  "Calculate total calories burned for a single workout.

This calculation uses adjusted calories/hour, TRUE-CALS-PER-HOUR, and
the DURATION of the workout."
    (/ (* true-cals-per-hour (* 60 duration)) 3600))


;;; Tests
(ert-deftest true-cals-test()
  (should (equal 715.79 (true--calories-per-hour 613 235)))
  (should (equal 655.79 (true--calories-per-hour 553 235)))

  (should (equal 838.36 (true--calories-per-hour 727 240))))

(ert-deftest total-cals-test()
  (should (equal 536.8425 (total--cals-burned 715.79 45)))
  (should (equal 327.895 (total--cals-burned 655.79 30))))

(provide 'c2-rowing)
;;; c2-rowing.el ends here
