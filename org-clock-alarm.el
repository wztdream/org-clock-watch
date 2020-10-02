;; Author: zongtao wang <wztdream@163.com>
;; Package-Requires: ((org-clock))

;;; Code:

(require 'org)
(require 'org-clock)
(require 'notifications)


(defvar org-clock-alarm-timmer nil)
(defvar org-clock-alarm-on-p nil
  "alarm set or not")

(defcustom org-clock-alarm-overtime-notify-sound (when load-file-name
                                      (concat (file-name-directory load-file-name)
                                              "resources/why-not-a-comfortable-rest.wav"))
  "The path to a sound file that´s to be played when a pomodoro is started."
  :group 'org-clock-alarm
  :type 'file)

(defcustom org-clock-alarm-overtime-icon (when load-file-name
                                        (concat (file-name-directory load-file-name)
                                                "resources/beach.svg"))
  "The path to a sound file that´s to be played when a pomodoro was finished."
  :group 'org-clock-alarm
  :type 'file)

(defvar org-clock-alarm-threshold (* 40 60)
  "over this seconds, will show over time notify")

(defcustom org-clock-alarm-overtime-notify-interval 180
  "over this seconds, will show over time notify"
  :group 'org-clock-alarm
  :type 'integer)

(defun org-clock-alarm-overtime-action (id key)
  (cond
   ((equal key "ok")
    (org-clock-alarm))
   ((equal key "latter")
    (org-clock-alarm)
    (shell-command "wmctrl -x -a Emacs")
    (org-clock-alarm (* 60 (read-number "Min: " 10)) t)))
  )

(defun org-clock-alarm-overtime-notify()
  "show alarm when over time"
  (let ((overred-time (- (org-time-convert-to-integer (org-time-since org-clock-start-time)) org-clock-alarm-threshold)))
   (when (and
         (>= overred-time 0)
         (zerop (mod overred-time org-clock-alarm-overtime-notify-interval))
         (time-less-p (org-x11-idle-seconds) '(0 180 0 0))) ;; only alarm once if idle
    (notifications-notify
     :title "OVERTIME"
     :urgency 'critical
     :body (format "over time <b> +%s min</b>" (floor overred-time 60))
     :actions '("ok" "why not?" "latter" "need more time")
     :on-action 'org-clock-alarm-overtime-action
     :app-icon org-clock-alarm-overtime-icon
     :sound-file org-clock-alarm-overtime-notify-sound
     :timeout 3000
     ))))

;;;###autoload
(defun org-clock-alarm (&optional min continue-last)
  "turn on/off org clock alarm, MIN is the alarm threshold, CONTINUE-LAST is
is Boolean, if non-nil will clock in last task
"
  (interactive)
  (if org-clock-alarm-on-p
      ;; if on then turn off
      (progn
        (org-clock-out)
        (when org-clock-alarm-timmer
          (setq org-clock-alarm-timmer (cancel-timer org-clock-alarm-timmer)))
        (setq org-clock-alarm-on-p nil))

    ;; if off then turn on
    (progn
      (if min
          (setq org-clock-alarm-threshold min)
          (setq org-clock-alarm-threshold (* 60 (read-number "Threshold in Min: " 40))))
      (if continue-last
          (org-clock-in-last)
          (org-clock-in))
      (unless org-clock-alarm-timmer
        (setq org-clock-alarm-timmer (run-with-timer t 1 'org-clock-alarm-overtime-notify)))
      (setq org-clock-alarm-on-p t)))
  )

(provide 'org-clock-alarm)
