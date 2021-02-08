# Python 环境搭建

### 1. 下载并安装

[下载页面](https://www.python.org/downloads/windows/) 下载对应的版本。**默认下载为 x86，请手动选择 x64 版本**

安装时：

- 勾选 `Add Python 3.x to PATH` 。
- 并选择自定义安装位置。
- 勾选 `Install for all users` 。
- 选择需要的安装位置。
- 点击安装。

设置镜像【有些时候镜像会有问题，则需要清除对应文件。】：

`sudo vi ~/.pip/pip.conf` 中添加或修改，Windows 中为用户目录中创建 `pip/pip.ini` 文件。

```
[global]
index-url = https://mirrors.aliyun.com/pypi/simple/

[install]
trusted-host=mirrors.aliyun.com
```

### 2. 安装常用库

更新 `pip` 执行： `c:\tools\python38\python.exe -m pip install --upgrade pip`

```bash
virtualenv
wheel
yapf
```

### 3. 使用 virtualenv

进入项目的根目录，执行 `virtualenv venv` 创建虚拟环境。

mac 脚本：`/path/venv.sh`

```bash
echo 进入Machine Learning虚拟环境目录
cd /Users/jasper/vpythons/ml
echo 进入虚拟环境
source bin/activate
echo 目录地址：/Users/jasper/vpythons/ml
```

mac 执行：`. /venv.sh`

---

Linux 脚本：`/path/venv.sh` ，末尾需要加入`$SHELL` 否则执行完毕后会回到起始状态。

```bash
echo 进入Machine Learning虚拟环境目录
cd /Users/jasper/vpythons/ml
echo 进入虚拟环境
source bin/activate
echo 目录地址：/Users/jasper/vpythons/ml
$SHELL
```

Linux 执行：`./venv.sh`

---

windows CMD 脚本：`C:\path\venv.bat`

```bash
cd C:\vpythons\ml
.\Scripts\activate
:: @cmd /k
```

windows CMD 执行：`.\venv.bat` 或 `venv.bat`

---

windows Powershell 脚本：`C:\path\venv.ps1`

```bash
cd C:\vpythons\ml
.\Scripts\activate
:: @cmd /k
```

windows Powershell 执行：`.\venv.ps1` 或 `.\\venv.ps1`
