# 数组类

* 数组对象中元素的大小怎么确定，什么时候确定

> 数组是相同大小元素的数据组合，所以数组类中数组元素的大小可以在构造函数赋值。

* 数组对象中元素个数有没有限制，数组中元素最大多少个，数组能够动态分配内存吗？不仅仅是能够增加内存，还要减少内存。在增加或减少内存的时候能不能快速分配内存，不需要移动元素实现数据的插入或删除。数组能不能自身实现越界检查，当数组越界的时候能够自动提醒错误，不是程序崩溃，也不用人为的去控制。

> 

* 

# TList

* 删除TList中元素的时候要注意什么问题

> 删除TList后，TList中的内容都没有得到释放，这样有一个好处就是当有多个TList中有同一个对象时，那么就不会导致释放掉一个TList后另外一个TList中的对象就不能使用了。
>
> 当循环删除TList中的内容时，如果从0到n的顺序删除，那么就会出现一个内部数据移动的问题，例如删掉第0个之后，那么原来的第1个编程第0个，而这时i的值为1。所以删除TList中数据的时候要倒序删除

```pascal
// 当删除某些内容时
for i := Pred(MyList.Count) to 0 do
begin
  if SomeConditionApplies(i) then  // 如果满足删除条件的话
  begin
    TObject(MyList[i]).Free;
    MyList.Delete(i);
  end;
end;
// 如果要全部删除时
for i := 0 to Pred(MyList.Count) do
  TObject(MyList[i]).Free;
MyList.Clear;
```



