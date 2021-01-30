# 8. 更新日志

## 8.1. v1.0

### 8.1.1. 破坏性变更

- `gorm.Open`返回类型为`*gorm.DB`而不是`gorm.DB`
- 更新只会更新更改的字段

大多数应用程序不会受到影响，只有当您更改回调中的更新值（如`BeforeSave`，`BeforeUpdate`）时，应该使用`scope.SetColumn`，例如：

```go
func (user *User) BeforeUpdate(scope *gorm.Scope) {
  if pw, err := bcrypt.GenerateFromPassword(user.Password, 0); err == nil {
    scope.SetColumn("EncryptedPassword", pw)
    // user.EncryptedPassword = pw  // 不工作，更新时不会包括EncryptedPassword字段
  }
}
```

- 软删除的默认查询作用域只会检查`deleted_at IS NULL`

之前它会检查 deleted_at 小于 0001-01-02 也排除空白时间，如：

```go
SELECT * FROM users WHERE deleted_at IS NULL OR deleted_at <= '0001-01-02'
```

但是没有必要，如果你使用`*time.Time`作为模型的`DeletedAt`，它已经被`gorm.Model`使用了，所以 SQL 就足够了

```go
SELECT * FROM users WHERE deleted_at IS NULL
```

所以如果你使用`gorm.Model`，那么你是好的，没有什么需要改变，只要确保所有记录的空白时间为`deleted_at`设置为`NULL`，示例迁移脚本：

```go
import (
    "github.com/jinzhu/now"
)

func main() {
  var models = []interface{}{&User{}, &Image{}}
  for _, model := range models {
    db.Unscoped().Model(model).Where("deleted_at < ?", now.MustParse("0001-01-02")).Update("deleted_at", gorm.Expr("NULL"))
  }
}
```

- 新的 ToDBName 逻辑

在 GORM 将 struct，Field 的名称转换为 db 名称之前，只有那些来自[golint](https://github.com/golang/lint/blob/master/lint.go#L702)的常见初始化（如`HTTP`，`URI`）是特殊处理的。

所以字段`HTTP`的数据库名称将是`http`而不是`h_t_t_p`，但是一些其他的初始化，如`SKU`不在 golint，它的数据库名称将是`s_k_u`，这看起来很丑陋，这个版本固定这个，任何大写的初始化应该正确转换。

- 错误`RecordNotFound`已重命名为`ErrRecordNotFound`
- `mssql`驱动程序已从默认驱动程序中删除，导入它用`import _ "github.com/jinzhu/gorm/dialects/mssql"`
- `Hstore`已移至`github.com/jinzhu/gorm/dialects/postgres`
