# RTTI

* ***现在对typedata的理解可能还有问题。***
* 运行时类型信息是指在运行时保存和检索对象和数据类型信息的手段，这个有两个概念，一个是保存，另外一个是检索。保存是指保存一个对象或者是一个变量的信息，其中包括这个对象或者变量的数据类型和值。检索就是查找一个对象或者变量它究竟是什么。
* 例如，一个用其他类（TButton） 创建的 AObject： TObject，那么RTTI就要知道这个AObject不是一个TObject，而是一个TButton。当需要用到类型信息的时候就要能够得到这个信息。
* 在查询RTTI 的时候最主要的就是类型信息结构体TTypeInfo 和类型数据结构体 TTypeData。这正好对应了上面提到的对象或变量的数据类型和值。

## 常用函数

* GetEnumName

```pascal
// 当你希望知道你获得的变量是什么类型的时候，可以使用该方法
GetEnumName(TypeInfo: PTypeInfo; Value: Integer);  // 可以遍历任意枚举类型，并获取其名称

type
  TEnum = (fsnormal,fseasy);
var
  a: TEnum;
begin
  a := fsnormal;  // 这里是显示说明了，如果a的值时通过参数传进来的，那么不能知道a是哪一个。
  GetEnumName(TypeInfo(TEnum),ord(a)); // 获得 a 这个变量是 TEnum 中的哪个。
end;

// 这个 AObject 可能是用TButton 创建的一个对象
// ClassInfo 返回一个指针，这个指针是Pointer，也就是无类型的指针，实际上这个指针是指向TTypeInfo类型的指针
var
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  begin
    TypeInfo := PTypeInfo(AObject.ClassInfo()); 
    TypeData := GetTypeData(TypeInfo);  // typInfo.GetTypeData
  end;
```

在 TTypeInfo 中没有 TypeData 字段， 那么如何获得具体的类型数据呢？ 

上面的问题要从 TObject 说起

```pascal
TObject = class
  Class function ClassName: shortString; // 返回类类型名称
  Class function ClassParent: TClass; // 判断某个类的父类
  class function ClassInfo: Pointer;   // 这个指针本身是一个指向 PTypeInfo 类型变量的指针，返回类类型信息
  class function InstanceSize: Longint;
  class function InheritsFrom(AClass: TClass): Boolean; // 判断某个类是否由其他类派生而来。
end;
```

上面的函数主要与RTTI有关。

对于那些不是类类型的数据类型，则可以使用定义于System中的 TypeInfo 函数获取类型信息， 不过没有出现在源码中

至于具体的类型数据，则必须调用全局函数 GetTypeData 获得

```pascal
function GetTypeData(TypeData: PTypeInfo): PTypeData;
```



