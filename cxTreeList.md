## 行节点和列节点

```pascal
cxTreelistNode: //行节点
cxTreelistColumn: //列节点
```

## 添加新的节点

```pascal
//添加新的行节点
var
  ANewNode: TcxTreeListNode;
  i: integer;
begin
  ANewNode := lst.add;
  ANewNode.ValueCount; //节点的列的个数
  for i := 0 to ANewNode.valueCount - 1 do
  	ANewNode.values[i] := IntToStr(i);
end;

//给选中节点添加子节点
var
  ANewNode: TcxTreeListNode;
  i: integer;
begin
  ANewNode := lst1.FocusedNode.AddChild;
  for i := 0 to ANewNode.valueCount - 1 do
  	ANewNode.values[i] := IntToStr(i);
end;

//获得该行节点的下一个节点
lst1.FocusedNode.GetNext; //这个不是获得同一级的下一个节点，而是获得界面上的下一个节点。

//获得同一级的下一个节点
lst1.FocusedNode.GetNextSibling;//这里的同一级是指的在

//删除节点
lst1.FocusedNode.Delete;//会删除该节点和该节点的子节点

//删除子节点
lst.FocusedNode.DeleteChildren; //会删除该节点的所有子节点

//添加新的列
var
  AColumn: TcxTreeListColumn;
begin
  AColumn := lst1.CreateColumn;
  AColumn.caption.text := '新列'；
  AColumn.caption.AlignHorz := taCenter;
  AColumn.Tag := 1;
  AColumn.Name := 'NewColumn';
end;

//删除列
lst1.FocusedColumn.Destroy;
```

## 各个项目的值

```pascal
ANewNode.ValueCount //新的节点的列的数量
Col.ItemIndex  //该列的索引是多少
```



## 事件的执行顺序

​		**在获取焦点和选取节点的时候，首先是获取焦点，然后是选中节点。**

