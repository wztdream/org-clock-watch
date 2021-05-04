# org-clock-watch

## 介绍

为org-clock 添加提醒功能,目的有三个:

1. 记录工作时间,方便统计在每个工作项目上花的时间

2. 防止过渡劳累工作,周期性提醒休息

3. 在各个项目之间合理分配时间

## 功能

**org-clock-watch添加的功能有:**

1. 如果计算机没有idle, 并且没有设置计时提醒,则弹出提醒,并播放声音提醒添加计时提醒

2. 如果计算机idel超过一定时间,则停止提醒,直到计算机不idle

3. 如果超时则周期性提醒休息,并播放声音

4. 如果需要延时,点击相应按钮,启动延时计时

**org-clock 已有功能:**

1. 统计一段时间内每个任务所花费的时间

2. 计算机idle超过一段时间,互动式恢复计时,如减去idel时间,保留指定时间等等

3. 开启org-clock 在modeline显示任务功能,则可显示当前任务

## 安装教程

1. 使用 [quelpa](https://github.com/quelpa/quelpa) 安装本包, 只需在emacs中eval下面的代码, quela会记忆安装的包,重启emacs并不需要重新安装包,详见quelpa repo

   gitee:

   `(quelpa `(org-clock-watch :fetcher git :url "https://gitee.com/zongtao_wang/org-clock-watch.git" :branch "master" :files (:defaults "resources")))` 

   github:

   `(quelpa `(org-clock-watch :fetcher git :url "https://github.com/wztdream/org-clock-watch.git" :branch "master" :files (:defaults "resources")))` 

2. 设置work plan文件路径,如果你没有设置计时,它会打开这个文件,让你选择工作项目并计时

    `setq org-clock-watch-work-plan-file-path "/path/to/your/work/plan/org/file"`

3. 启动watcher, 在你的init文件中添加:

   `(org-clock-watch-toggle 'on)`

4. 使用系统idle时间

    如果想要使用电脑的idle时间(而不是emacs的idle 时间) 需要安装 `xprintidle`
    
    如果是ubuntu系统:
    
       1. `sudo apt install xprintidle`

       2. `M-x customize-varialbe org-clock-x11idle-program-name` 把设置改成 xprintidle 即可

    其他系统请参考[emacs 手册](https://www.gnu.org/software/emacs/manual/html_node/org/Resolving-idle-time.html)

    > On computers using macOS, idleness is based on actual user idleness, not just Emacs’ idle time. For X11, you can install a utility program ‘x11idle.c’, available in the ‘contrib/scripts/’ directory of the Org Git distribution, or install the xprintidle package and set it to the variable org-clock-x11idle-program-name if you are running Debian, to get the same general treatment of idleness. On other systems, idle time refers to Emacs idle time only.

5. 快捷键

主要是设置org-clock的快捷键, 如果你喜欢hydra可以参考下面的设置

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

## 工作流

1. 打开emacs

2. 开始做你想做的事情 (不限于emacs)

3. 忘记计时了,org-clock-watch弹出提醒,播放声音,打开工作计划文件, 你可以用`org-clock-in` 来计时

4. 忘记设置提醒时长了, org-clock-watch弹出提醒,播放声音,定位到计时项目, 你可以用 `org-set-effort` 来设置提醒时长

5. 如果你愿意也可以直接打开工作目录并对某个工作计时, 并设置effort, org-clock-watch 能够识别这些情景

6. 超过时长则周期性弹出提醒,播放声音,直到你结束计时 `org-clock-out`

7. 你发现时间不够,需要再多工作5分钟,点击提醒框中的 "5 min",提醒自动延后5 分钟

8. 你发现设置的effort 不合理, 可以用 `org-clock-modify-effort-estimate` 重新设置effort

9. 你临时有事离开电脑一会儿, 该包会自动识别, 停止提醒,直到你回来. org-clock 弹出恢复窗口,引导你恢复计时,比如从计时中扣除10分钟等等

10. 一周以后,你想看看这一周你都做了什么, 打开工作计划文件, 运行 `org-clock-report` 则以表格方式给出花费时间分布.

总之, 你只需要运行org-clock 的计时命令,剩下的不用你管.

## 使用方法

可以参考工作流和hydra key 的设定, 主要命令有:

1. 开始计时. 打开工作文件 `org-clock-in`

2. 设置提醒时长. `org-set-effort`

3. 结束计时. `org-clock-out`

4. 修改提醒时长 `org-clock-modify-effort-estimate`

5. 查看org-clock-watch 状态 `org-clock-watch-status`

6. 关闭 org-clock-watch `C-u C-u org-clock-watch-toggle`

7. 开启 org-clock-watch `C-u org-clock-watch-toggle`

8. Toggle org-clock-watch `org-clock-watch-toggle`

9. 打开工作计划文件 `org-clock-watch-goto-work-plan`

org-clock-watch 使用的icon, 播放的声音, 提醒周期都是可以定制的:

`M-x custom-group org-clock-watch`

## 哲学

1. 简单的任务就让它保持简单

2. org-clock 非常强大,这个包只"监视"org-clock的状态,并发出提醒, 所以你可以使用org-clock的全部功能.

Enjoy it!
