# Excel 第一次打开空白的问题

由于不知道的原因，导致打开 Excel 文件，第一次都会是空白页，需要再次打开才能正常打开文件。

创建一个 `修复Excel打开空白.reg` 文件，内容如下：

```
Windows Registry Editor Version 5.00

[HKEY_CLASSES_ROOT\Excel.Sheet.12\shell\Open\command]
@="\"C:\\Program Files\\Microsoft Office\\Root\\Office16\\EXCEL.EXE\" \"%1\""

[HKEY_CLASSES_ROOT\Excel.Sheet.8\shell\Open\command]
@="\"C:\\Program Files\\Microsoft Office\\Root\\Office16\\EXCEL.EXE\" \"%1\""

```

出现该问题时双击该注册表文件即可。
