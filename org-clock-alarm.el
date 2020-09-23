;; Author: zongtao wang <wztdream@163.com>
;; Package-Requires: ((org-clock))

;;; Code:

(require 'org)
(require 'org-clock)
(require 'notifications)


(defvar org-clock-alarm-timmer nil)
(defvar org-clock-alarm-state :off
  "alarm set or not")

(defcustom org-clock-alarm-overtime-notify-sound (when load-file-name
                                      (concat (file-name-directory load-file-name)
                                              "resources/why-not-a-comfortable-rest.wav"))
  "The path to a sound file that´s to be played when a pomodoro is started."
  :group 'org-clock-alarm
  :type 'file)

(defcustom org-clock-alarm-overtime-icon (when load-file-name
                                        (concat (file-name-directory load-file-name)
                                                "resources/beach.SVN"))
  "The path to a sound file that´s to be played when a pomodoro was finished."
  :group 'org-clock-alarm
  :type 'file)

(defcustom org-clock-alarm-threshold 40
  "over this minutes, will show over time notify"
  :group 'org-clock-alarm
  :type 'integer)

(defun org-clock-overtime-action (id key)
  (when (equal key "ok")
    (org-clock-alarm-stop)
    )

(defun org-clock-overtime-notify()
  "show alarm when over time"
  (let ((overred-time (- (org-clock-get-clocked-time) org-clock-alarm-threshold)))
   (when (and
         (>= overred-time 0)
         (time-less-p (org-x11-idle-seconds) '(0 180 0 0))) ;; only alarm once if idle
    (notifications-notify
     :title "OVERTIME"
     :body (format "over time <b>+%s</b>" overred-time)
     :actions '("ok" "why not?")
     :on-action 'org-clock-alarm-overtime-action
     :app-icon org-clock-alarm-overtime-icon
     :sound-file org-clock-alarm-overtime-notify-sound
     ))))

(defun org-clock-alarm-start ()
  "turn on org clock alarm"
  (interactive)
  (org-clock-in)
  (unless org-clock-alarm-timmer
    (setq org-clock-alarm-timmer (run-with-timer t 10 'org-clock-overtime-notify))
    )
  (setq org-clock-alarm-state :on)
  )
(defun org-clock-alarm-stop ()
  "stop org clock alarm"
  (interactive)
  (org-clock-out)
  (when org-clock-alarm-timmer
    (setq org-clock-alarm-timmer (cancel-timer org-clock-alarm-timmer)))
  (setq org-clock-alarm-state :off)
  )

(provide 'org-clock-alarm)
