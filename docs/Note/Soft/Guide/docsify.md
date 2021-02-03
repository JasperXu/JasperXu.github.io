# docsify 指南

## 1. 基本使用

什么安装创建什么的去看官网吧，这个写的很详细了。[官网文档](https://docsify.js.org/#/zh-cn/)

下面是些不一样的东西。

## 2. 推荐插件

- [docsify-themeable](https://jhildenbiddle.github.io/docsify-themeable/#/introduction) 一个应用很广泛的样式，提供浅色模式和深色模式。
- [emoji](https://docsify.js.org/#/zh-cn/plugins?id=emoji) 提供 emoji 支持，需要应用 docsify 中自带的 plugins 中的对应 js 文件。
- [zoom-image](https://docsify.js.org/#/zh-cn/plugins?id=%e5%9b%be%e7%89%87%e7%bc%a9%e6%94%be-zoom-image) 提供图片的一些显示功能，需要应用 docsify 中自带的 plugins 中的对应 js 文件。
- [gitalk](https://docsify.js.org/#/zh-cn/plugins?id=gitalk) 一个页面评论插件，需要配合 docsify 中自带的 plugins 中的对应 js 文件。 **这个文件需要调整！！！**
- [MathJax](https://www.mathjax.org/) 用于页面显示数学公式的玩意。
- [prism](https://docsify.js.org/#/zh-cn/language-highlight) 代码高亮。一些语言需要单独应用对应的 js 文件。
- [docsify-fontawesome](https://github.com/erickjx/docsify-fontawesome#readme) 提供很多小图标的玩意。
- [docsify-pagination](https://github.com/imyelo/docsify-pagination#readme) 提供上一页和下一页的功能。
- [docsify-tabs](https://jhildenbiddle.github.io/docsify-tabs/#/) 提供 Tabs 的功能。
- [docsify-edit-on-github](https://github.com/njleonzhang/docsify-edit-on-github#readme) 在页面上提供一个连接，如果你有权限的话可以直接跳转到对应的 Github 页面进行页面编辑。
- [docsify-jx-toc](https://github.com/jasperxu/docsify-jx-toc#readme) 我自己写的提供页面内目录和回到顶部按钮的功能。
- [docsify-jx-themes-manage](https://github.com/jasperxu/jasperxu.github.io/tree/main/docs/lib/docsify-jx-themes-manage/dist) 我自己写的，在回到顶部按钮下面显示两个按钮，用来切换浅色模式和深色模式的，需要的可以直接在我的代码库里面找。功能很弱就没发布了。

## 3. 插件调整

### 3.1. docsify-themeable

他同时提供了一些特殊功能 [点击查看](https://jhildenbiddle.github.io/docsify-themeable/#/markdown)

#### 3.1.1. 设置封面的链接样式

只会给最后一个 `<p>` 中的 `<a>` 标签加特殊的样式。 如果希望和我[首页](/)一样的每个标签样式都一样需要进行如下调整。
需要自己到样式表中找到 `.cover` 下的所有样式

- 将 `p:last-child` 替换为 `p`
- 将 `a:first-child` 替换为 `a`
- 将 `a:last-child` 替换为 `a`

#### 3.1.2. 设置封面宽度

默认封面中内容宽度很窄，这里加了一倍的宽度。将对应的 css 中的如下内容进行调整。

将 `--cover-max-width: 40em;` 替换为 `--cover-max-width: 80em;`

### 3.2. gitalk

按官网的设置，貌似创建出来的聊天页面会有问题。这里需要将 docsify 下载到本地来引用。

修改 `docsify/lib/plugins/gitalk.js` 文件。

```js
(function () {
  /* eslint-disable no-unused-vars */
  function install(hook) {
    var dom = Docsify.dom;

    hook.mounted(function (_) {
      var div = dom.create("div");
      div.id = "gitalk-container";
      var main = dom.getNode("#main");
      div.style = "max-width: " + main.clientWidth + "px; margin: 0 auto 20px; padding: 0px 45px;";
      dom.appendTo(dom.find(".content"), div);
    });

    hook.doneEach(function (_) {
      var el = document.getElementById("gitalk-container");
      if (el != null) {
        // 去掉CoverPage的加载
        while (el.hasChildNodes()) {
          el.removeChild(el.firstChild);
        }

        // 合并gitalk群组。
        var getID = function () {
          var idstring = location.href.substring(location.origin.length + 2);
          if (idstring.startsWith("/Note/Soft/Recommend")) return "/Note/Soft/Recommend";
          if (idstring.startsWith("/Programming/Golang/GORM/")) return "/Programming/Golang/GORM/";
          return idstring;
        };

        const gitalk = new Gitalk({
          clientID: "f4138bcafcce8b3e2a62",
          clientSecret: "bf3da43a9e862fca8252bf83491d103021eec268",
          repo: "jasperxu.github.io",
          owner: "jasperxu",
          admin: ["jasperxu"],
          id: getID(),
          // facebook-like distraction free mode
          distractionFreeMode: false,
        });

        // eslint-disable-next-line
        gitalk.render("gitalk-container");
      }
    });
  }

  $docsify.plugins = [].concat(install, $docsify.plugins);
})();
```

1. 修正了在 **CoverPage** 页面会出现错误的 bug。

2. 修正了页面过小时显示过宽的问题。

3. 会自动为每一页提供一个聊天组。

4. 可以手动合并一些页面链接，让其使用同一个聊天组。

比如我这里将`/Note/Soft/Recommend` 开头的所有页面都设置到 `/Note/Soft/Recommend` 这同一个聊天中。

再比如我这里将 `/Programming/Golang/GORM` 开头的所有页面都设置到 `/Programming/Golang/GORM` 这同一个聊天中。

### 3.3. docsify-edit-on-github

调整样式，并为每一页在页面顶部提供一个修订时间的显示。修改插件如下：

```js
(function (win) {
  function isFunction(functionToCheck) {
    return functionToCheck && {}.toString.call(functionToCheck) === "[object Function]";
  }

  win.EditOnGithubPlugin = {};

  function create(docBase, docEditBase, title) {
    title = title || "Edit on github";
    docEditBase = docEditBase || docBase.replace(/\/blob\//, "/edit/");

    function editDoc(event, vm) {
      var docName = vm.route.file;

      if (docName) {
        var editLink = docEditBase + docName;
        window.open(editLink);
        event.preventDefault();
        return false;
      } else {
        return true;
      }
    }

    win.EditOnGithubPlugin.editDoc = editDoc;

    function generateHeader(title) {
      return (header = [
        '<div style="overflow: auto">',
        "<p>",
        '修订时间：<i class="far fa-calendar-alt"></i> {docsify-updated} ',
        '<a style="float: right;text-decoration: underline; cursor: pointer"',
        'onclick="EditOnGithubPlugin.onClick(event)">',
        title,
        "</a></p>",
        "</div>\n\n",
      ].join(""));
    }

    return function (hook, vm) {
      win.EditOnGithubPlugin.onClick = function (event) {
        EditOnGithubPlugin.editDoc(event, vm);
      };
      if (isFunction(title)) {
        hook.beforeEach(function (html) {
          return generateHeader(title(vm.route.file)) + html;
        });
      } else {
        var header = generateHeader(title);
        hook.beforeEach(function (html) {
          return header + html;
        });
      }
    };
  }

  win.EditOnGithubPlugin.create = create;
})(window);
```

## 4. 正常使用

```bash
# 进入放置文档的目录
cd docs

# 启动服务
docsify serve
```
