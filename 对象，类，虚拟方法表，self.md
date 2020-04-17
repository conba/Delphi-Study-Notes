# 对象，类，虚拟方法表，self

## 什么是对象

```pascal
// 对象是用类创建的实例，对象本身在堆中存储，对象名称是一个指向对象存储空间的一个指针。
// 注意，对象之间是不共享数据的，而类中的方法对每个对象而言是共享的。
// self分为两种，第一是在类方法中使用的self，这个时候self指的是虚拟方法表的基地址，也就是起始地址，；第二是在普通方法中使用self，这个时候self就是对象名称的一个别名，其实就是对象本身。
// 对象名称或者说self存储的是对象本身的基地址（起始地址，现在我是这样认为的，可能不对，后面了解后会修改），每个对象中的内容包括VMT和该对象的成员字段，首先保存的就是虚拟方法表的地址（注意，不是虚拟方法表的基地址，而是虚拟方法表的地址，也就是说它指向虚拟方法表）。

  TMyClass = class
    FA: Byte;
    FB: array [0..3] of Double;
  protected
    FC: Boolean;
    FD: string;
  private
    FE: Integer;
    procedure F0();
    function G0(): Integer;
    procedure H0(var P: Pointer);

    function GetSelfAddress: string;
    class function GetClassSelfAddress: string;
    function GetVMTAddress: string;
  end;

  TProcA = procedure () of object;
  TFuncB = function (): Integer of object;
  TProcC = procedure (var P: Pointer) of object;
  
// 获取类方法中self这个指针保存（指向）的地址
class function TMyClass.GetClassSelfAddress: string;
var
  P: Pointer;
begin
  P := Pointer(Self);
  Result := IntToHex(Integer(P), 8);
end;

// 获取普通方法中self这个指针保存（指向）的地址
function TMyClass.GetSelfAddress: string;
var
  P: Pointer;
begin
  P := Pointer(Self);
  Result := IntToHex(Integer(P), 8);
end;

// 获取虚拟方法表的地址
function TMyClass.GetVMTAddress: string;
var
  P: Pointer;
begin
  P := Pointer(Self);
  P := Pointer(P^);
  Result := IntToHex(Integer(P), 8);
end;

// 查看各个地址信息
procedure TForm13.btn1Click(Sender: TObject);
var
  MyClass: TMyClass;
  Str: string;
  PMyClass: Pointer;
  P: Pointer;
begin
  Self.mmo1.Lines.Clear;
  Str := '类方法的Self指向地址 ' + TMyClass.GetClassSelfAddress;
  mmo1.Lines.Add(Str);  // 00523A60

  PMyClass := @MyClass;
  Str := 'MyClass这个对象指针本身的地址 ' + IntToHex(Integer(PMyClass), 8);
  mmo1.Lines.Add(Str);  // 0018F53C

  MyClass := TMyClass.Create;
  Str := '普通方法中self指向的地址 ' + MyClass.GetSelfAddress;
  mmo1.Lines.Add(Str);  // 0247DE00

  PMyClass := Pointer(MyClass);
  Str := 'MyClass这个对象指针指向的地址 ' + IntToHex(Integer(PMyClass), 8);
  mmo1.Lines.Add(Str);  // 0247DE00

  Str := 'MyClass这个对象虚拟方法表的基地址' + MyClass.GetVMTAddress;
  mmo1.Lines.Add(str);  // 00523A60
  
  P := @MyClass.FA;
  Str := 'MyClass中FA字段的地址 ' + IntToHex(Integer(P), 8);
  mmo1.Lines.Add(Str);  // 023FDE04

  P := @MyClass.FB;
  Str := 'MyClass中FB字段的地址 ' + IntToHex(Integer(P), 8);
  mmo1.Lines.Add(Str);  // 023FDE08

  P := @MyClass.FC;
  Str := 'MyClass中FC字段的地址 ' + IntToHex(Integer(P), 8);
  mmo1.Lines.Add(Str);  // 023FDE28

  P := @MyClass.FD;
  Str := 'MyClass中FD字段的地址 ' + IntToHex(Integer(P), 8);
  mmo1.Lines.Add(Str);  // 023FDE2C

  ProcA := MyClass.F0;
  Str := 'TMyClass中F0的地址 ' + IntToHex(Integer(@ProcA), 8);
  mmo1.Lines.Add(Str);  // 00524044

  FuncB := MyClass.G0;
  Str := 'TMyClass中G0的地址 ' + IntToHex(Integer(@FuncB), 8);
  mmo1.Lines.Add(Str);  // 00524050

  ProcC := MyClass.H0;
  Str := 'TMyClass中H0的地址 ' + IntToHex(Integer(@ProcC), 8);
  mmo1.Lines.Add(Str); // 005240E0

  MyClass.Free;
end;
```

## 类成员的可见性和类限定

```pascal
// 首先，这是两个不同的概念。
// 类成员的可见性是对外部来说的，类成员中只有声明在public和published域的成员对外才是可见的，
// 类限定这个概念来源于继承。例如，父类的private域中的成员子类能否调用，是否有权限调用。
```



