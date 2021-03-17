# Mac 程序无法运行

## Mac 文件损坏处理

原因：系统偏好设置->安全性与饮食->通用->允许从以下位置下载的应用 需要设置为 任何来源 。新系统中隐藏了该选项。

解决方法：打开 Mac 终端，输入以下代码回车。

```bash
sudo spctl --master-disable
```

## 应用程序不能打开

原因：执行程序丢失了执行权限。

解决方法：右键对应的执行文件->显示包内容->进入到 Contents/MacOS 文件夹中，这里会看到一个文件，给该文件添加执行权限即可。打开 终端 输入
`chmod +x `将文件拖入然后回车。

# 例如：

```bash
chmod +x /Applications/Xcode.app/Contents/MacOS/Xcode
```
