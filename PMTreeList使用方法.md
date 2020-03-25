# PMTreeList中常用的方法

```pascal
// 1. PMTreeList的每个列都有一个FieldName,它不同于每个Column的Name, 可以为每个fieldname赋值
TPMTreeListColumn(ZbclTree.Columns[i]).FieldName := cst_Zbcl_Bm;
// 2. 根据FieldName判断Column是否存在
if ZbclTree.GetColumnByFieldName(cst_Zbcl_djsx) <> nil then

```

