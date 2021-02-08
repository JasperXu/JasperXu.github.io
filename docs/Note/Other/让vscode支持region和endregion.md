# 让 vs code 支持 region 和 endregion

在 `Microsoft VS Code\resources\app\extensions/XXX/language-configuration.json` 文件的最后一个 `}` 前加入如下内容：

```json
,
	"folding": {
		"offSide": true,
		"markers": {
			"start": "^\\s*//\\s*#?region",
			"end": "^\\s*//\\s*#?endregion"
		}
	}
```

上面的 `XXX` 为你要使用的语言。

这里的示例语言是使用 `//` 作为注释的。

开始部分使用`//region`或`//#region` ，结束部分使用`//endregion`或`//#endregion` 。

如果使用语言不是使用 `//` 作为注释，则替换为对应的注释即可。
