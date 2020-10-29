# org-clock-watch

## Introduction

Add alarm feature for org-clock, there are mainly three goals for this package:

1. record work time in order to analysis latter on

2. alarm you to take a rest before you are worn out

3. balance your time on projects

## Features

**added by org-clock-watch:**

1. if system not idle and you forgot to clock in or set effort then alarm you to set them

2. if system idle over threshold, then stop alarm until you are back

3. if over effort threshold, then periodically notify you to take a rest by sound and pop up window

4. if you need more time to work, just click the button "5 min" on pop up window, it will postpone the alarm

**org-clock already have:**

1. analysis time distribution spend on projects

2. interactively resolve clock in time if system idle over threshold

3. show current alarm and time in mode line

## Installation

1. use [quelpa](https://github.com/quelpa/quelpa) to install, you only need to eval below code in emacs, see details on quelpa repo

   gitee:

   `(quelpa `(org-clock-watch :fetcher git :url "https://gitee.com/zongtao_wang/org-clock-watch.git" :branch "master" :files (:defaults "resources")))` 

     github:

     `(quelpa `(org-clock-watch :fetcher git :url "https://github.com/wztdream/org-clock-watch.git" :branch "master" :files (:defaults "resources")))` 
  

2. set your work plan file path, it should be an org file, org-clock-watch will open it and let you clock in some task in it

    `setq org-clock-watch-work-plan-file-path "/path/to/your/work/plan/org/file"`

3. add below code in you init file to start the watcher

   `(org-clock-watch-toggle 'on)`

4. use system idle time

    If you want to use system idle time not emacs idle time, you need to install `xprintidle`. 
    
    If you are using ubuntu, it is easy:

       1. `sudo apt install xprintidle`

       2. `M-x customize-varialbe org-clock-x11idle-program-name` and set it to xprintidle

    For other system please check [emacs manual](https://www.gnu.org/software/emacs/manual/html_node/org/Resolving-idle-time.html)

    > On computers using macOS, idleness is based on actual user idleness, not just Emacs’ idle time. For X11, you can install a utility program ‘x11idle.c’, available in the ‘contrib/scripts/’ directory of the Org Git distribution, or install the xprintidle package and set it to the variable org-clock-x11idle-program-name if you are running Debian, to get the same general treatment of idleness. On other systems, idle time refers to Emacs idle time only.

5. key binding

   If you like hydra key, reference below setting:

```
(defhydra hydra-org-clock (:color pink :hint nil)
"
org-clock hydra key

clock                             ^^^^effort             ^^watcher
-------------------------------^^^^^^^---------------------------------
[_i_]  clock in     [_c_]  cancel     [_e_] set effort     [_t_] toggle
[_L_]  clock last   [_o_]  clock out  [_E_] reset effort   [_s_] start
[_r_]  resolve                                         ^^^^[_S_] stop
[_g_]  goto                                            ^^^^[_w_] status
[_J_]  jump2current                                    ^^^^[_O_] open plan

[_q_] cancel
"
      ("i" org-clock-in)
      ("o" org-clock-out :exit t)
      ("r" org-resolve-clocks :exit t)
      ("g" org-clock-goto :exit t)
      ("J" spacemacs/org-clock-jump-to-current-clock :exit t)
      ("c" org-clock-cancel :exit t)
      ("L" org-clock-in-last)
      ("e" org-set-effort :exit t)
      ("E" org-clock-modify-effort-estimate :exit t)
      ("t" org-clock-watch-toggle :exit t)
      ("s" (org-clock-watch-toggle 'on) :exit t)
      ("S" (org-clock-watch-toggle 'off) :exit t)
      ("w" (org-clock-watch-status))
      ("O" org-clock-watch-goto-work-plan)
      ("q" nil :color blue))

(global-set-key (kbd "C-c .") 'hydra-org-clock/body)
```

## Work flow

1. open emacs

2. do things you like, not necessarily in emacs

3. forgot to clock in? org-clock-watch pop up notify window, play sound to alarm you, you can clock in by `org-clock-in`

4. forgot to set effort? org-clock-watch popup window, play sound to alarm and focus to current clock in task, you can set effort by `org-set-effort`, org clock treat effort as overtime threshold

5. of cause you can manually open work plan file and clock in and set effort, org-clock-watch can handle this scenario

6. if overtime, alarm you periodically until you clock out `org-clock-out`

7. you need 5 more min to finish the work, click the button "5 min" on pop up window, the alarm will postpone 5 min

8. you can reset the effort by `org-clock-modify-effort-estimate`

9. you walk way for a while, it will stop alarm until you came back, then org-clock may guide you to resolve the clock in time, reduce 10 min for example

10. one week latter, you want to see where is your time spend. Open work plan and run `org-clock-report`, there will be a report table shown up

So, you only need to run org-clock command, others are all auto

## Usage

I think you can figure out how to use it by work flow and the hydra key, anyway here are the main commands:

1. start clock in. `org-clock-in`

2. set over time threshold. `org-set-effort`

3. stop clock. `org-clock-out`

4. reset over time threshold. `org-clock-modify-effort-estimate`

5. check org-clock-watch status. `org-clock-watch-status`

6. close org-clock-watch. `C-u C-u org-clock-watch-toggle`

7. open org-clock-watch. `C-u org-clock-watch-toggle`

8. toggle org-clock-watch. `org-clock-watch-toggle`

9. open work plan file. `org-clock-watch-goto-work-plan`

you can customize the icon, sound and alarm interval by `M-x custom-group org-clock-watch`

## Philosophy

1. keep it simple for simple task

2. org-clock is powerful, org-clock-watch only watch it, so you can use all the features of org-clock

Enjoy it!
