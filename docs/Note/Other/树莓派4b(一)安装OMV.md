# 树莓派 4b(一)安装 OMV

## 查看设备版本

```bash
# 查看设备型号
$ cat /proc/device-tree/model
Raspberry Pi 4 Model B Rev 1.2

# 查看系统信息
$ lsb_release -a
No LSB modules are available.
Distributor ID: Raspbian
Description:    Raspbian GNU/Linux 10 (buster)
Release:        10
Codename:       buster

# 查看cpu & gpu温度
$ vcgencmd measure_temp
temp=28.2'C

# 查看cpu当前频率
$ sudo cpufreq-info -w -m

# 查看cpu信息
$ lscpu
显示很多就不列出来了

# 查看内存
$ free -h
              total        used        free      shared  buff/cache   available
Mem:          3.7Gi       265Mi       1.9Gi        75Mi       1.6Gi       3.2Gi
Swap:          99Mi          0B        99Mi

# 查看硬盘
$ lsblk
NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINT
mmcblk0     179:0    0  29.3G  0 disk
├─mmcblk0p1 179:1    0   256M  0 part /boot
└─mmcblk0p2 179:2    0    29G  0 part /

# 查看网络
$ ifconfig

# 查看usb设备
lsusb
# 查看其它硬件
lsmod
```

设备信息：Raspberry Pi 4b 4G v1.2

## 安装系统

[下载地址](https://www.raspberrypi.org/software/operating-systems/#raspberry-pi-os-32-bit) 推荐下载 **Raspberry Pi OS with desktop** 下面是安装后的默认值。

```bash
设备名：raspberrypi
账户：pi
密码：raspberry
```

然后使用 Rufus 软件写入到 TF 卡中。

TF 卡根目录创建文件名为 `ssh` 的空文件，不要有后缀。可以自动打开 ssh。

然后装上 tf 卡，连上网线，显示器，键盘，鼠标，通电开机。

## 设置系统

一开始选中文就完事了，然后一路 next，完毕后重启 `sudo shutdown -r now` 。

然后系统就是中文了。

### 设置 VNC 和 SSH

左上角 > 首选项 > Raspberry Pi Configuration > Interfaces

将 SSH 和 VNC 选到 Enable。

然后重启 `sudo shutdown -r now` 。

#### Cannot currently show the desktop 问题修复

VNC 显示 Cannot currently show the desktop 则进行如下设置

```
sudo raspi-config
选择 【2 Display Options】
选择 【D1 Resolution】
选择 【只要不选第一个就行，我这里选的1440x900 60Hz 16:10】
```

### 设置网络

右上角设置网络，**隐藏网络是无法连接的** 。反正我用的有线网络无所谓。

打开终端输入 `ifconfig` 查看下你的 IP 地址，我这里是 `192.168.199.2` 后面都会用这个地址。

### 设置清华的源

```bash
# 编辑 `/etc/apt/sources.list` 文件，删除原文件所有内容，用以下内容取代：
deb http://mirrors.tuna.tsinghua.edu.cn/raspbian/raspbian/ buster main non-free contrib rpi
deb-src http://mirrors.tuna.tsinghua.edu.cn/raspbian/raspbian/ buster main non-free contrib rpi

# 编辑 `/etc/apt/sources.list.d/raspi.list` 文件，删除原文件所有内容，用以下内容取代：
deb http://mirrors.tuna.tsinghua.edu.cn/raspberrypi/ buster main ui

# 执行下面命令更新软件
sudo apt update
sudo apt upgrade -y
sudo rm -f /etc/systemd/network/99-default.link

# 重启
sudo reboot
```

## 安装 OMV

```
wget -O - https://github.com/OpenMediaVault-Plugin-Developers/installScript/raw/master/install | sudo bash
```

如果连接失败，请设置下代理。

```bash
sudo raspi-config
选择 【6 Advanced Options】
选择 【A5 Network Proxy Setting】
选择 【P1 All】
输入你的代理，我这里是【http://192.168.199.250:7777】
然后会自动重启。
```

完毕后再次执行上面的命令。

安装完毕后打开浏览器输入你树莓派的 ip 地址进入 web 管理页面

```bash
账号：admin
密码：openmediavault
```

建议开启如下服务：

```
FTP	这个什么都能连
NFS	这个Apple系和Linux连接较快
SMB/CIFS	这个主要给Windows连接
```

### 共享文件夹 设置

如果无法选择，请在 OMV 中将硬盘里面先卸载，再挂载即可。

在**共享文件夹**中为每个目录的**ACL**增加 **读/写/执行**并**应用到文件与子文件夹**然后应用即可。

### 服务设置

FTP：开启匿名（内网用方便）

NFS：无法设置匿名。

SMB/CIFS：设置公开 **允许访客** （内网用方便）

### flashmemory

这个插件一定要装，要不 U 盘或者 tf 卡很容易就挂了。

### 最后

```bash
# 删除代理
sudo raspi-config
选择 【6 Advanced Options】
选择 【A5 Network Proxy Setting】
选择 【P1 All】
设置为空【】
然后会自动重启。
```

### OMV 中设置休眠

```bash
高级电源管理：1-待机状态时最低功耗(磁头停转)
自动声音管理：关闭
停转时间：10分钟
写入缓存：关闭(硬盘支持的话可以开启)
```

## 性能

电脑 -> 树莓派 -> USB3.0 移动硬盘 写入数据：约 30MB/s

USB3.0 移动硬盘 -> 树莓派 -> 电脑 读取数据：约 90MB/s
