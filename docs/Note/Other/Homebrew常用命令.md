# Homebrew 常用命令

### 1. 安装

地址：https://brew.sh/index_zh-cn

安装脚本

```bash
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

### 2. 常用

```bash
# 安装
brew install 软件名

# 搜索
brew search 软件名

# 卸载
brew uninstall 软件名

# 更新brew 和 软件包信息
brew update

# 更新指定包，不写软件名则更新所有。
brew upgrade [软件名]

# 已安装列表
brew list

# 查看软件信息
brew info 软件名

# 打开软件官网
brew home 软件名

# 查看需要更新的软件
brew outdated

# 显示依赖，默认显示所有
brew deps --tree [软件名]

# 清理旧版本
brew cleanup

```

### 3. 可替换 Aliyun 的 mirrors

`https://opsx.alibaba.com/mirror`
