# go 执行 sql 语句

## 〇、一次执行

```go
result, err := db.Exec("INSERT INTO `table`(`field1`, `field2`) VALUES (?, ?);", item.field1, item.field2)
if err != nil {
    return err
}
result.LastInsertId() // 自增列可获取，返回值为(int64, error)
result.RowsAffected() // 影响行数，返回值为(int64, error)
```

## 一、预声明，重复执行执行

```go
// 声明部分
stmt, err := db.Prepare("INSERT INTO `table`(`field1`, `field2`) VALUES (?, ?);")
if err != nil {
    return err
}
defer stmt.Close()
// 执行部分，result常用_替代
result, err = stmt.Exec(item.field1, item.field2)
if err != nil {
    return err
}
result.LastInsertId() // 自增列可获取，返回值为(int64, error)
result.RowsAffected() // 影响行数，返回值为(int64, error)
```

## 二、查询返回一行

```go
item := models.Table{}
tmpBool1 := []uint8{} // 用于接受数据库中的bit类型然后再转换成bool

row := db.QueryRow("SELECT `UUID`, `IsAdmin` FROM `table` WHERE  `UUID`=?;", ID)
err := row.Scan(&item.UUID, &tmpBool1)
if err != nil {
    return nil
}
item.IsAdmin = tools.Bit2Bool(tmpBool1)
```

转化 bit 类型到 Bool

```go
// Bit2Bool 把数据库中的bit类型取出为[]uint8后转化为bool
func Bit2Bool(value []uint8) bool {
    if len(value) == 0 || value[len(value)-1] == 0 {
        return false
    } else {
        return true
    }
}
```

## 三、查询多行

```go
var items []interface{} = make([]interface{}, 0)
rows, err := db.Query("SELECT `UUID`, `IsAdmin` FROM `table` WHERE 1;")
if err != nil {
    return err
}
for rows.Next() {
    item := models.Table{}
    tmpBool1 := []uint8{} // 用于接受数据库中的bit类型然后再转换成bool

    err = rows.Scan(&item.UUID, &tmpBool1)
    if err != nil {
        return err
    }

    item.IsAdmin = tools.Bit2Bool(tmpBool1)

    items = append(items, item)
}
```
