# npm 上传自己的包

## 1. 新建项目

```bash
npm init
```

中间会让你填写一些东西，自己填就行了。

## 2. 登陆 npm

```bash
# 设置到原始的npm源，如果你改了国内源就需要这个
npm config set registry http://registry.npmjs.org
# 登陆
npm login
Username:这里填你的npm用户名
Password：这里填你的npm密码
Email：(this IS public)这里填你的邮箱
Logged xxxxxxxxxxxxx
```

## 3. 发布

```bash
npm publish
```

## 4. 维护

下次使用 publish 必须要修改版本号。

X.Y.Z 分别代表主版本号、次版本号和补丁版本号。

- Z：只修改了 bug
- Y：新增了功能，但是向下兼容。
- X：大改动，且不向下兼容。

删除和撤销已发布的包

```bash
npm unpublish 包名			//撤销已发布的包 
npm unpublish 包名 --force	//强制撤销
npm unpublish 包名@1.0.0	//可以撤销指定版本 
```

## 5. 通过 CDN 访问

### jsdelivr

```
https://cdn.jsdelivr.net/npm/包名@1.0.0/dist/文件名.min.js
```

### unpkg

```
https://unpkg.com/npm/包名@1.0.0/dist/文件名.min.js
```
