# org-clock-watch

#### 介绍
为org-clock 添加提醒功能,目的有两个:

1. 记录工作时间,方便统计在每个工作项目上花的时间
2. 防止过渡劳累工作,周期性提醒休息

#### 功能
**org-clock-watch添加的功能有:**

1. 如果计算机没有idle, 并且没有设置计时提醒,则弹出提醒,并播放声音提醒添加计时提醒
2. 如果计算机idel超过一定时间,则停止提醒,直到计算机不idle
3. 如果超时则周期性提醒休息,并播放声音
4. 如果需要延时,点击相应按钮,启动延时计时

**org-clock 已有功能:**

1. 统计一段时间内每个任务所花费的时间
2. 计算机idle超过一段时间,互动式恢复计时,如减去idel时间,保留指定时间等等
3. 开启org-clock 在modeline显示任务功能,则可显示当前任务



#### 安装教程

1.  emacs

    1. 使用 [quelpa](https://github.com/quelpa/quelpa) 安装本包, 只需在emacs中eval下面的代码, quela会记忆安装的包,重启emacs并不需要重新安装包,详见quelpa repo

      `(quelpa `(org-clock-watch :fetcher git :url "https://gitee.com/zongtao_wang/org-clock-watch.git" :branch "master" :files (:defaults "resources")))` 
    
    2. 设置work plan文件路径,如果你没有设置计时,它会打开这个文件,让你选择工作项目并计时

        `setq org-clock-watch-work-plan-file-path "/path/to/your/work/plan/org/file"`
        
    3. 启动watcher, 在你的init文件中添加:

       `(run-with-timer 1 1 'org-clock-watcher)`

2.  spacemacs

    1. 在your-layer中添加包 

       (org-clock-watch :location (recipe :fetcher git :url "git@gitee.com:zongtao_wang/org-clock-watch.git" :branch "master" :files (:defaults "resources")))

    2. 配置包

       ```
       (defun your-layer/init-org-clock-watch()
           (use-package org-clock-watch
               :defer t
               :init
               (run-with-timer 1 1 'org-clock-watcher)
           ))

       ```
    3. 在init file 的dotspacemacs-configuration-layers中添加 your-layer

     `(your-layer :variables org-clock-watch-work-plan-file-path "/file/path/to/your/work/plan/org/file")`
3. 快捷键

主要是设置org-clock的快捷键, 如果你喜欢hydra可以参考下面的设置

```
(defhydra hydra-org-clock (:color pink :hint nil)
"
org-clock hydra key

clock                                     ^^^^ effort
───────────────────────────────────────── ^^^^ ───────────────────────
[_i_]  clock in         [_c_]  cancel          [_e_] set effort
[_o_]  clock out        [_l_]  clock last      [_E_] reset effort
[_r_]  resolve
[_g_]  goto
[_j_]  jump to current

[_q_] cancel
"
      ("i" org-clock-in)
      ("o" org-clock-out :exit t)
      ("r" org-resolve-clocks :exit t)
      ("g" org-clock-goto :exit t)
      ("j" spacemacs/org-clock-jump-to-current-clock :exit t)
      ("c" org-clock-cancel :exit t)
      ("l" org-clock-in-last)
      ("e" org-set-effort :exit t)
      ("E" org-clock-modify-effort-estimate :exit t)
      ("q" nil :color blue))

(global-set-key (kbd "C-c .") 'hydra-org-clock/boday)
```

#### 工作流

1. 打开emacs

2. 开始做你想做的事情

3. 忘记计时了,org-clock-watch弹出提醒,播放声音,打开工作计划文件, 你可以用`org-clock-in` 来计时

4. 忘记设置提醒时长了, org-clock-watch弹出提醒,播放声音,定位到计时项目, 你可以用 `org-set-effort` 来设置提醒时长

5. 如果你愿意也可以直接打开工作目录并对某个工作计时, 并设置effort, org-clock-watch 能够识别这些情景

6. 超过时长则周期性弹出提醒,播放声音,直到你结束计时 `org-clock-out`

7. 你发现时间不够,需要再多工作5分钟,点击提醒框中的"5 min",提醒自动延后5 分钟

8. 你发现设置的effort 不合理, 可以用 `org-clock-modify-effort-estimatek` 重新设置effort

9. 你临时有事离开电脑一会儿, 该包会自动识别, 停止提醒,直到你回来. org-clock 弹出恢复窗口,引导你恢复计时,比如从计时中扣除10分钟等等

10. 一周以后,你想看看这一周你都做了什么, 打开工作计划文件, 运行 `org-clock-report` 则以表格方式给出花费时间分布.

总之, 你只需要运行org-clock 的计时命令,剩下的不用你管.


#### 哲学

1. 简单的任务就让它保持简单

2. org-clock 非常强大,这个包只"监视"org-clock的状态,并发出提醒, 所以你可以使用org-clock的全部功能.
