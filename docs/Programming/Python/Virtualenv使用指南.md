# Virtualenv 使用指南

- Mac 中建议先安装 Python3 `brew install python3` 并 `brew linkapps python3`

### 1. 安装 Virtualenv

```bash
pip install --user virtualenv #不加--user安装到全局，加了安装到个人
virtualenv --version
```

### 2. 创建一个虚拟环境

```bash
cd my_project_dir
virtualenv venv # 创建系统Python版本的环境
```

转到`my_project_dir` 目录，创建一个虚拟环境，虚拟环境目录名为 `venv` ，这里 `venv` 可指定，不指定则直接放在当前目录。

还可以创建其他版本的虚拟环境（本地必须安装了指定的版本）

```bash
virtualenv -p python3 venv # 创建Python3的环境
```

### 3. 进入/退出虚拟环境

Windows PowerShell 中需先执行 `Set-ExecutionPolicy -ExecutionPolicy RemoteSigned` 修改权限。

```bash
source venv/bin/activate	# 进入环境 Linux 或 MacOS
venv/Scripts/activate		# 进入环境 Windows CMD
.\venv\Scripts\activate		# 进入环境 Windows PowerShell

deactivate					# 退出环境
```

### 4. 运行脚本

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
