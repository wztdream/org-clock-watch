;; Author: zongtao wang <wztdream@163.com>
;; Package-Requires: ((org) (org-clock) (notifications))

;;; Code:

(require 'org)
(require 'org-clock)
(require 'notifications)

(defvar org-clock-watch-timer nil "the timer that runs org-clock-watcher")

(defvar org-clock-watch-postponed-time 0 "accumulated postponed time")
(defvar org-clock-watch-overred-time 0 "over time value")

(defvar org-clock-watch-work-plan-file-path
  nil "The the work plan org file path .")


(defvar org-clock-watch-total-on-time 0 "total time (sec) since turn on watcher")

(defvar org-clock-watch-open-org-agenda-func
  nil "the function to open org-agenda, it should be a wrapper of org-agenda
for example:
(defun my/open-org-agenda-clock ()
  (interactive)
  (org-agenda nil SOME-LETTER))

You can set `org-agenda-custom-commands' with SOME-LETTER
")

(defvar org-clock-watch-timer-file-path nil
  "the file path for timer, which is an org file path")
(defvar org-clock-watch-timer-id nil "the id of the heading, which is a string")

(defcustom org-clock-watch-micro-rest-p t
  "non-nil means will send micro rest alarm, nil will disable it"
  :group 'org-clock-watch
  :type 'boolean)

(defcustom org-clock-watch-clock-in-sound (when load-file-name
                                            (concat (file-name-directory load-file-name)
                                                    "resources/why-not-clock-in.wav"))
  "The path to a sound file that´s to be played when found no clock is running."
  :group 'org-clock-watch
  :type 'file)

(defcustom org-clock-watch-effort-sound (when load-file-name
                                          (concat (file-name-directory load-file-name)
                                                  "resources/why-not-set-an-effort.wav"))
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

(defcustom org-clock-watch-no-set-me-icon (when load-file-name
                                            (concat (file-name-directory load-file-name)
                                                    "resources/tomato.svg"))
  "The path to a icon file that´s to be show when overtime."
  :group 'org-clock-watch
  :type 'file)

(defcustom org-clock-watch-micro-rest-sound (when load-file-name
                                              (concat (file-name-directory load-file-name)
                                                      "resources/ding.wav"))
  "The path to a sound file that´s to be played when need micro rest."
  :group 'org-clock-watch
  :type 'file)

(defcustom org-clock-watch-overtime-notify-interval
  180 "over this seconds, will show over time notify"
  :group 'org-clock-watch
  :type 'integer)

(defcustom org-clock-watch-micro-rest-interval
  180 "micro rest means a very short time interval and a very small rest
such as stretch your body, shake your head every 3 min
" :group 'org-clock-watch
  :type 'integer)

(defcustom org-clock-watch-effort-notify-interval
  60 "interval in sec to notify set effort"
  :group 'org-clock-watch
  :type 'integer)

(defcustom org-clock-watch-clock-in-notify-interval
  180 "time interval to notify user set clock"
  :group 'org-clock-watch
  :type 'integer)
(defun org-clock-watch-start-heading-clock (id file effort)
  "Start clock programmatically for heading with ID in FILE, and set effort to EFFORT."
  (if-let (marker (org-id-find-id-in-file id file t))
      (save-current-buffer (save-excursion
                             (set-buffer (marker-buffer marker))
                             (goto-char (marker-position marker))
                             (org-clock-in)
                             (when effort
                               (org-set-effort nil effort))))
    (warn "Clock not started (Could not find ID '%s' in file '%s')"
          id file)))

(defun org-clock-watch-initilize ()
  "initialize variables"
  (setq org-clock-watch-postponed-time 0 org-clock-watch-total-on-time
        0))

(defun org-clock-watch-goto-work-plan ()
  "open work plan org file"
  (interactive)
  (shell-command "wmctrl -x -a Emacs")
  (find-file org-clock-watch-work-plan-file-path))

(defun org-clock-watch-clock-in-action (id key)
  (let (effort)
    (if (equal key "task")
        (funcall org-clock-watch-open-org-agenda-func)
      ;;else, get the effort
      (cond
       ((equal key "manual")
        (shell-command "wmctrl -x -a Emacs")
        (setq effort (read-string "effort:" nil nil "00:60")))
       ((equal key "30min")
        (setq effort "00:30"))
       ((equal key "45min")
        (setq effort "00:45"))
       ((equal key "60min")
        (setq effort "01:00"))
       ((equal key "90min")
        (setq effort "01:30"))
       ((equal key "120min")
        (setq effort "02:00")))
      ;; start clock and set effort
      (org-clock-watch-start-heading-clock org-clock-watch-timer-id
                                           org-clock-watch-timer-file-path effort))))

(defun org-clock-watch-clock-in-close (id reason)
  ;; start clock and set effort
  (when (equal reason 'expired)
    (message "inner: id %s reason %s" id reason)
    (org-clock-watch-start-heading-clock org-clock-watch-timer-id
                                         org-clock-watch-timer-file-path nil)))

(defun org-clock-watch-overtime-action (id key)
  (cond
   ((equal key "ok")
    (org-clock-out))
   ((equal key "5min")
    (setq org-clock-watch-postponed-time (+ org-clock-watch-overred-time
                                            (* 60 5))))
   ((equal key "10min")
    (setq org-clock-watch-postponed-time (+ org-clock-watch-overred-time
                                            (* 60 10))))
   ((equal key "20min")
    (setq org-clock-watch-postponed-time (+ org-clock-watch-overred-time
                                            (* 60 20))))
   ((equal key "30min")
    (setq org-clock-watch-postponed-time (+ org-clock-watch-overred-time
                                            (* 60 30))))))

(defun org-clock-watcher ()
  "To watch org-clock status, if `org-clocking-p' is t and not set org-clock-watch,
then set org-clock-watch, if `org-clocking-p' is nil then notify to set org-clock,
you need to run this function as a timer, in you init file
"
  ;; tic-toc
  (setq org-clock-watch-total-on-time (1+ org-clock-watch-total-on-time))
  ;; only alarm when not idle
  (when (time-less-p (org-x11-idle-seconds)
                     '(0 120 0 0))
    (if (org-clocking-p)
        ;; org-clock is running
        (progn
          ;; when not set effort, then periodically notify user to set effort
          (when (and (or (null org-clock-effort)
                         (equal org-clock-effort ""))
                     (zerop (mod org-clock-watch-total-on-time org-clock-watch-effort-notify-interval)))
            (notifications-notify :title "Set an effort?"
                                  :urgency 'normal
                                  :app-icon org-clock-watch-no-set-me-icon
                                  :timeout 10000)
            (call-process "aplay" nil nil nil org-clock-watch-effort-sound)
            (run-at-time nil
                         nil
                         (lambda nil
                           (shell-command "wmctrl -x -a Emacs")
                           (org-set-effort))))
          ;; update over time
          (setq org-clock-watch-overred-time (- (org-time-convert-to-integer (org-time-since org-clock-start-time))
                                                (* 60
                                                   (org-duration-to-minutes org-clock-effort))))
          ;; overtime alarm
          (when (and (> org-clock-watch-overred-time org-clock-watch-postponed-time)
                     (zerop (mod org-clock-watch-overred-time org-clock-watch-overtime-notify-interval)))
            (notifications-notify :title org-clock-current-task
                                  :urgency 'normal
                                  :body (format "over time <b> +%s min</b>"
                                                (floor org-clock-watch-overred-time 60)):actions'("ok" "why not?" "5min" "5min" "10min" "10min"
                                                "20min" "20min" "30min" "30min")
                                  :on-action 'org-clock-watch-overtime-action
                                  :app-icon org-clock-watch-overtime-icon
                                  :timeout 10000)
            (call-process "aplay" nil nil nil org-clock-watch-overtime-notify-sound)))
      ;; actions to take when org-clock is not running
      ;; notify to clock in
      (when (zerop (mod org-clock-watch-total-on-time org-clock-watch-clock-in-notify-interval))
        (notifications-notify :title "clock in?"
                              :urgency 'normal
                              :app-icon org-clock-watch-no-set-me-icon
                              :actions '("manual" "manual" "task" "task" "30min" "30min"
                                         "45min" "45min" "60mim" "60min" "90min" "90mim"
                                         "120min" "120min"):on-action'org-clock-watch-clock-in-action
                              :on-close 'org-clock-watch-clock-in-close
                              :timeout 10000)
        (call-process "aplay" nil nil nil org-clock-watch-clock-in-sound)))
    ;; periodically sent micro rest alarm when system not idle
    (when (and org-clock-watch-micro-rest-p
               (zerop (mod org-clock-watch-total-on-time org-clock-watch-micro-rest-interval)))
      (call-process "aplay" nil nil nil org-clock-watch-micro-rest-sound))))


;;;###autoload
(defun org-clock-watch-toggle (&optional on-off)
  "start/stop the timer that runs org-clock-watcher
ON-OFF `C-u' or 'on means turn on, `C-u C-u' or 'off means turn off, `nil' means toggle
"
  (interactive "P")
  (cond
   ((null on-off)
    (if org-clock-watch-timer
        (setq org-clock-watch-timer (cancel-timer org-clock-watch-timer))
      (setq org-clock-watch-timer (run-with-timer 5 1 'org-clock-watcher))
      (org-clock-watch-initilize)))
   ((or (equal on-off 'on)
        (equal on-off '(4)))
    (unless org-clock-watch-timer
      (setq org-clock-watch-timer (run-with-timer 5 1 'org-clock-watcher))
      (org-clock-watch-initilize)))
   ((or (equal on-off 'off)
        (equal on-off '(16)))
    (when org-clock-watch-timer
      (setq org-clock-watch-timer (cancel-timer org-clock-watch-timer)))))
  (if org-clock-watch-timer
      (message "org-clock-watcher started")
    (message "org-clock-watcher stopped")))

(defun org-clock-watch-status ()
  "get the status of watcher"
  (interactive)
  (if org-clock-watch-timer
      (message "org-clock-watcher is running")
    (message "org-clock-watcher is stopped")))

;; add hook to org-clock-out-hook, to initialize watch
(add-hook 'org-clock-out-hook 'org-clock-watch-initilize)
(provide 'org-clock-watch)

;;code end here
