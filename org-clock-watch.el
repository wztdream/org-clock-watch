;; Author: zongtao wang <wztdream@163.com>
;; Package-Requires: ((org-clock))

;;; Code:

(require 'org)
(require 'org-clock)
(require 'notifications)


(defvar org-clock-watch-on-p nil
  "alarm set or not")

(defvar org-clock-watch-postponed-time 0
  "accumulated postponed time"
  )
(defvar org-clock-watch-overred-time 0
"over time value")

(defvar org-clock-watch-work-plan-file-path nil
  "The the work plan org file path .")

(defvar org-clock-watch-set-clock-notify-interval (* 60 3)
  "time interval to notify user set clock"
  )

(defvar org-clock-watch-set-clock-notify-passed-time 0
  "total time (sec) pass since first notify"
  )

(defvar org-clock-watch-threshold nil
  "over this seconds, will show over time notify")

(defcustom org-clock-watch-notify-to-set-clock-sound (when load-file-name
                                                   (concat (file-name-directory load-file-name)
                                                           "resources/why-not-clock-in.wav"))
  "The path to a sound file that´s to be played when found no clock is running."
  :group 'org-clock-watch
  :type 'file)
(defcustom org-clock-watch-overtime-notify-sound (when load-file-name
                                      (concat (file-name-directory load-file-name)
                                              "resources/why-not-a-comfortable-rest.wav"))
  "The path to a sound file that´s to be played when overtime."
  :group 'org-clock-watch
  :type 'file)

(defcustom org-clock-watch-overtime-icon (when load-file-name
                                        (concat (file-name-directory load-file-name)
                                                "resources/beach.svg"))
  "The path to a icon file that´s to be show when overtime."
  :group 'org-clock-watch
  :type 'file)

(defcustom org-clock-watch-overtime-notify-interval 180
  "over this seconds, will show over time notify"
  :group 'org-clock-watch
  :type 'integer)

(defun org-clock-watch-goto-work-plan()
  (shell-command "wmctrl -x -a Emacs")
  (find-file org-clock-watch-work-plan-file-path))

(defun org-clock-watch-overtime-action (id key)
  (cond
   ((equal key "ok")
    (org-clock-out))
   ((equal key "5min")
    (setq org-clock-watch-postponed-time (+ org-clock-watch-overred-time  (* 60 5)))
    )
   ((equal key "latter")
    (shell-command "wmctrl -x -a Emacs")
    (setq org-clock-watch-postponed-time (+ org-clock-watch-overred-time (* 60 (read-number "Threshold in Min: " 10))))
    ))
  )

;;;###autoload
(defun org-clock-watcher()
  "To watch org-clock status, if `org-clocking-p' is t and not set org-clock-watch,
then set org-clock-watch, if `org-clocking-p' is nil then notify to set org-clock,
you need to run this function as a timer, in you init file
"
  ;; only watch when not idle
  (when (time-less-p (org-x11-idle-seconds) '(0 120 0 0))
   (cond
   ;; org-clock is running then watch it
   ((org-clocking-p)
    (unless org-clock-watch-on-p
      (setq org-clock-watch-on-p t
      org-clock-watch-threshold (* 60 (read-number "Threshold in Min: " 40))
      org-clock-watch-set-clock-notify-passed-time 0))
    (setq org-clock-watch-overred-time (- (org-time-convert-to-integer (org-time-since org-clock-start-time)) org-clock-watch-threshold))
    (when (and
           (>= (- org-clock-watch-overred-time org-clock-watch-postponed-time) 0)
           (zerop (mod org-clock-watch-overred-time org-clock-watch-overtime-notify-interval)))
      (notifications-notify
       :title org-clock-current-task
       :urgency 'critical
       :body (format "over time <b> +%s min</b>" (floor overred-time 60))
       :actions '("ok" "why not?" "5min" "5min" "latter" "more time")
       :on-action 'org-clock-watch-overtime-action
       :app-icon org-clock-watch-overtime-icon
       :sound-file org-clock-watch-overtime-notify-sound
       :timeout 3000
       )))
   ;; org-clock is not running, then notify to clock in a task
   ((not (org-clocking-p))
    (setq org-clock-watch-set-clock-notify-passed-time (1+ org-clock-watch-set-clock-notify-passed-time))
    (if org-clock-watch-on-p
        ;; set initial value
        (setq org-clock-watch-postponed-time 0
              org-clock-watch-threshold nil
              org-clock-watch-on-p nil)
      (when (zerop (mod org-clock-watch-set-clock-notify-passed-time org-clock-watch-set-clock-notify-interval))
        (notifications-notify
         :title "Set a clock?"
         :urgency 'critical
         :sound-file org-clock-watch-notify-to-set-clock-sound
         :app-icon org-pomodoro-no-set-me-icon
         :timeout 3000
         )
        (run-at-time "3 sec" nil 'org-clock-watch-goto-work-plan))
      )))))

(provide 'org-clock-watch)
