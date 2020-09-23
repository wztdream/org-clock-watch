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
                                                "resources/beach.SVN"))
  "The path to a sound file that´s to be played when a pomodoro was finished."
  :group 'org-clock-alarm
  :type 'file)

(defcustom org-clock-alarm-threshold 40
  "over this minutes, will show over time notify"
  :group 'org-clock-alarm
  :type 'integer)

(defcustom org-clock-alarm-overtime-notify-interval 3
  "over this minutes, will show over time notify"
  :group 'org-clock-alarm
  :type 'integer)

(defun org-clock-alarm-overtime-action (id key)
  (when (equal key "ok")
    (org-clock-alarm))
  )

(defun org-clock-alarm-overtime-notify()
  "show alarm when over time"
  (let ((overred-time (- (org-clock-get-clocked-time) org-clock-alarm-threshold)))
   (when (and
         (>= overred-time 0)
         (zerop (mod overred-time org-clock-alarm-overtime-notify-interval))
         (time-less-p (org-x11-idle-seconds) '(0 180 0 0))) ;; only alarm once if idle
    (notifications-notify
     :title "OVERTIME"
     :body (format "over time <b>+%s</b>" overred-time)
     :actions '("ok" "why not?")
     :on-action 'org-clock-alarm-overtime-action
     :app-icon org-clock-alarm-overtime-icon
     :sound-file org-clock-alarm-overtime-notify-sound
     ))))

;;;###autoload
(defun org-clock-alarm ()
  "turn on/off org clock alarm"
  (interactive)
  (if org-clock-alarm-on-p
     ;; if on then turn off
    (progn
      (org-clock-out)
      (when org-clock-alarm-timmer
        (setq org-clock-alarm-timmer (cancel-timer org-clock-alarm-timmer)))
      (setq org-clock-alarm-state nil)))

  ;; if off then turn on
  (progn
    (org-clock-in)
    (unless org-clock-alarm-timmer
      (setq org-clock-alarm-timmer (run-with-timer t 1 'org-clock-alarm-overtime-notify)))
    (setq org-clock-alarm-state t))
  )

(provide 'org-clock-alarm)
