# Win Linux 双系统安装指南

## 环境说明

硬件：一块 240G NVMe，一块 240G SSD，一块 2T 的 HDD。

系统：Linux Mint 18.2，Windows 10 Enterprise Version 1703 Update June 2017

分配：由于工作原因，我的主系统为 Linux Mint 所以我的分配方式如下：

Mint 安装在 NVMe 上，Win10 安装在 SSD 上。

HDD 分成两个 1T 的盘一个 NTFS 一个 Ext4。

安装顺序：先装 Win10，再装 Mint

## 安装 Win10

进入系统安装后，首先在 NVMe 上新建分区，会提示需要自动建立几个分区。点继续。

之后再顺次将 SSD 和 HDD 都建立分区。最后分区如下。

NVMe：MSR 分区(16M)，恢复分区(450M)，EFI 分区(100M)，一个主分区

SSD：MSR 分区(16M)，主分区

HDD：MSR 分区(16M)，主分区

建立好之后，选择 SSD 主分区，继续安装。

进入系统后，进磁盘管理，将 HDD 上的分区删除，再建立一个 1T 的分区格式化。

## 安装 Mint

进入系统安装后，选择自定义分区。建立主分区，不要建立逻辑分区！

在 NVMe 上将 200G 的分区删除，然后建立一个 2G 的分区挂载 Swap，再建立一个最大分区挂载/。

这里 Swap 分区可以和内存一样大，如果内存较大则可以只建立很小的 swap 即可。

可以将 HDD 挂载到你喜欢的目录，我这里直接挂载到/data 目录。

**安装引导区 记得选 NVMe 上的引导区 好像是 EFI 分区**

## 其他说明和调整

- 硬盘设置问题，所有硬盘必须使用 GTP 分区。否则会有读取不正常状态，默认为 MBR 分区方式。

- 文件系统问题，Win10 使用 NTFS，Mint 必须使用主分区模式的 Ext4。否则无法相互读写。

- Mint 驱动问题，直接在驱动管理器中选择新的驱动即可。

- 时间问题，双系统切换导致时间不正确。解决办法：调整 Win 的注册表，使其使用硬件时间为 UTC 时间。

  ```
  Reg add HKLM\SYSTEM\CurrentControlSet\Control\TimeZoneInformation /v RealTimeIsUniversal /t REG_DWORD /d 1
  ```

- Mint 读写 NTFS 文件，必须关闭 Win 的快速启动，否则提示权限问题无法读写。并使用 ntfs-3g 挂载 ntfs 盘。

挂载分区

```bash
mount -t ntfs /dev/sdb2 /mnt/D -o iocharset=utf8,umask=0
```

卸载分区

```bash
unmount /dev/sdb2
```

启动自动挂载
打开/etc/fstab 文件，在文件末尾加上

```bash
/dev/sdb2 /mnt/D ntfs utf8,umask=0
```

- Win 读写 Ext4 文件，需要安装 Ext2Fsd 软件。
