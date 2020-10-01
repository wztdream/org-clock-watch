# org-clock-alarm

#### 介绍
为org-clock 添加提醒功能,目的有两个:
1. 记录工作时间,方便统计在每个工作项目上花的时间
2. 防止过渡劳累工作,周期性提醒休息

功能有:
1. 如果计算机没有idle, 并且没有设置计时提醒,则弹出提醒,并播放声音提醒添加计时提醒
2. 如果计算机idel超过一定时间,则停止提醒,直到计算机不idle
3. 如果超时则周期性提醒休息
4. 如果需要延时,点击相应按钮,启动延时计时
5. 开启org-clock 在modeline显示任务功能,则可显示当前任务


#### 安装教程

1.  emacs 自行查找安装gitee包的方法,并安装org-clock-alarm
2.  设置好自己的工作目录org文件

#### 使用说明

1.  基本使用只需执行 org-clock-alarm 命令, 它会自动设置计时或者开始计时

#### 哲学
简单的任务就让它保持简单

org-clock 非常强大,这个包只"监视"org-clock的状态,并发出提醒, 所以你可以使用org-clock的全部功能.
#### 特技

1.  使用 Readme\_XXX.md 来支持不同的语言，例如 Readme\_en.md, Readme\_zh.md
2.  Gitee 官方博客 [blog.gitee.com](https://blog.gitee.com)
3.  你可以 [https://gitee.com/explore](https://gitee.com/explore) 这个地址来了解 Gitee 上的优秀开源项目
4.  [GVP](https://gitee.com/gvp) 全称是 Gitee 最有价值开源项目，是综合评定出的优秀开源项目
5.  Gitee 官方提供的使用手册 [https://gitee.com/help](https://gitee.com/help)
6.  Gitee 封面人物是一档用来展示 Gitee 会员风采的栏目 [https://gitee.com/gitee-stars/](https://gitee.com/gitee-stars/)
