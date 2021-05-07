# RTTI

* ***现在对typedata的理解可能还有问题。***
* RTTI包含的内容非常多，主要目的是，让程序在运行时也能知道数据类型以及类的信息，这些信息十分详细的保存了数据类型以及类的所有细节，几乎包括我们可以在程序代码中得知的信息。
* 运行时类型信息是指在运行时保存和检索对象和数据类型信息的手段，这个有两个概念，一个是保存，另外一个是检索。保存是指保存一个对象或者是一个变量的信息，其中包括这个对象或者变量的数据类型和值。检索就是查找一个对象或者变量它究竟是什么。
* 例如，一个用其他类（TButton） 创建的 AObject： TObject，那么RTTI就要知道这个AObject不是一个TObject，而是一个TButton。当需要用到类型信息的时候就要能够得到这个信息。
* 在查询RTTI 的时候最主要的就是类型信息结构体TTypeInfo 和类型数据结构体 TTypeData。这正好对应了上面提到的对象或变量的数据类型和值。

## 举个例子

```pascal
// 枚举类型
TFormStyle = (fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop);
// 在代码中我们可以随意使用fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop来标识TFormStyle类型的变量的实际值。不过在程序实际运行的时候，左右的值视为整数来处理（作为整数更快，更节省内存），所以只是知道值，无法取得这些值的名称。简单来说，没有办法获得fsNormal这个字符串
// 不过有了RTTI的支持，就可以将这些信息保存到可执行文件中，那么这样就能够获得TFormStyle类型变量的名称。
begin
  showMessage(GetEnumName(TypeInfo(TFormStyle), order(fsNormal)));
end;
// TypeInfo是TypInfo单元提供的函数，用来获取类型的RTTI指针，用途和对象的TObject.ClassInfo一样
```



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
  Class function ClassNameIs(const Name: string): Boolean; 
  // 以上3个方法提供类名称以及父类信息，是通过虚拟方法表得到的。
  class function ClassInfo: Pointer;   // 这个指针本身是一个指向 PTypeInfo 类型变量的指针，返回类类型信息，也就是此类的RTTI，普通类型使用TypeInfo函数。
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

## 常用函数

```pascal
 GetTypeData 函数
 GetPropInfo 函数
 FindPropInfo 函数
 GetPropInfos 函数
 SortPropList 函数
 GetPropList 函数
------------------------------------------------------
 GetObjectPropClass 函数
 PropType / PropIsType 函数
 IsPublishedProp 函数
 IsStoredProp 函数
 FreeAndNilProperties 函数
 SetToString / StringToSet 函数
 GetEnumName / GetEnumValue / GetEnumNameValue 函数
------------------------------------------------------
 GetOrdProp 函数详解
 SetOrdProp 函数
 GetEnumProp / SetEnumProp 函数
 GetSetProp / SetSetProp 函数
 GetObjectProp / SetObjectProp 函数
 GetStrProp / SetStrProp 函数
 GetFloatProp / SetFloatProp 函数
 GetPropValue / SetPropValue 函数
 TPublishableVariantType class
------------------------------------------------------
 RegisterClass / FindClass 系列函数 (Classes.pas)
 IdentToInt / IntToIdent 系列函数 (Classes.pas)
 
GetTypeData 函数
===============================================================================
GetTypeData 函数根据 TTypeInfo 指针获得 TTypeData 的地址。

function GetTypeData(TypeInfo: PTypeInfo): PTypeData;
asm
       XOR    EDX,EDX                          ; EDX 清零
       MOV    DL,[EAX].TTypeInfo.Name.Byte[0]  ; 获得 Name 字符串长度
       LEA    EAX,[EAX].TTypeInfo.Name[EDX+1]  ; 获得 TTypeData 的地址
end;

===============================================================================
 GetPropInfo 函数
===============================================================================
GetPropInfo 函数用于获得属性的 RTTI 指针 PPropInfo。它有四种重载形式，后面三种重载的实现都是调用第一种形式。AKinds 参数用于限制属性的类型，如果得到的 PPropInfo 不属于指定的类型，则返回 nil。

  function GetPropInfo(TypeInfo: PTypeInfo; const PropName: string): PPropInfo;

  function GetPropInfo(Instance: TObject; const PropName: string;
    AKinds: TTypeKinds = []): PPropInfo;
  function GetPropInfo(AClass: TClass; const PropName: string;
    AKinds: TTypeKinds = []): PPropInfo;
  function GetPropInfo(TypeInfo: PTypeInfo; const PropName: string;
    AKinds: TTypeKinds): PPropInfo;

===============================================================================
 FindPropInfo 函数
===============================================================================
FindPropInfo 函数根据属性名称获得属性的 RTTI 指针，它只是在 GetPropInfo 函数的基础上加上了错误检查功能，如果没有属性 RTTI 信息，则触发 EPropertyError 异常。

function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo;
function FindPropInfo(AClass: TClass; const PropName: string): PPropInfo;

===============================================================================
 GetPropInfos 函数
===============================================================================
GetPropInfos 函数的功能是把一个类(class)所有属性 RTTI 指针 PPropInfo 填充至传入的参数 PPropList 数组中。

注意：这个函数不负责分配该数组的内容，使用前必须根据属性的数量分配足够的空间。该数组结束后必须清除分配的内容。

  procedure GetPropInfos(TypeInfo: PTypeInfo; PropList: PPropList);

注：使用 GetPropList 实现相同的功能更方便。

===============================================================================
 SortPropList 函数
===============================================================================
SortPropList 可以对 GetPropInfos 函数填充的属性信息指针数组按属性名称排序。

  procedure SortPropList(PropList: PPropList; PropCount: Integer);

在 VCL 中 SortPropList 只被 GetPropList 函数使用。

===============================================================================
 GetPropList 函数
===============================================================================
GetPropList 函数同 GetPropInfos 一样，填充 PPropList 数组。GetPropList 实际上是调用 GetPropInfos 进行填充工作，最后返回已填充的属性的数量。

  function GetPropList(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds;
    PropList: PPropList; SortList: Boolean): Integer;

  function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
  function GetPropList(AObject: TObject; out PropList: PPropList): Integer;

注意：GetPropList 的内存分配有点混乱，上面第一个 GetPropList 必须自己分配 PPrpList 数组的内存，后面二个 GetPropList 会自动分配 PPropList 数组的内存。造成这种情况的原因是：第一个 GetPropList 可以设置 TypeKinds 参数限制只返回指定类型的属性，这样就不能直接得到可能返回的属性数量。TypeKinds 参数可以设置为 tkAny，表示返回所有数据类型的属性。

第一个 GetPropList 函数可以设置 SortList 参数对属性名称进行排序。它实际上是调用第二个 GetPropList 并调用 SortPropList 函数执行排序。

注意：PPropList 不再使用的时候，要记得使用 FreeMem 函数清除数组内存(根据返回值是否大于1)。

===============================================================================
 GetObjectPropClass 函数
===============================================================================
GetObjectPropClass 函数用于返回对象类型的属性所属的类(class)。

  function GetObjectPropClass(Instance: TObject; PropInfo: PPropInfo): TClass;
  function GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
  function GetObjectPropClass(PropInfo: PPropInfo): TClass;

这个函数被 SetObjectProp 函数使用，用于参数检验。

===============================================================================
 PropType / PropIsType 函数
===============================================================================
PropType 函数用于获得属性的数据类型。

  function PropType(Instance: TObject; const PropName: string): TTypeKind;
  function PropType(AClass: TClass; const PropName: string): TTypeKind;

PropIsType 判断属性是否属于某种数据类型。它调用 PropType 实现功能。

  function PropIsType(Instance: TObject; const PropName: string;
    TypeKind: TTypeKind): Boolean;
  function PropIsType(AClass: TClass; const PropName: string;
    TypeKind: TTypeKind): Boolean;

===============================================================================
 IsPublishedProp 函数
===============================================================================
IsPublishedProp 函数用于判断属性是否是 published 属性，它通过检查该属性 RTTI 指针是否等于 nil 来实现功能。

  function IsPublishedProp(Instance: TObject; const PropName: string): Boolean;
  function IsPublishedProp(AClass: TClass; const PropName: string): Boolean;

IsPublishedProp 函数没有被 VCL 使用。

===============================================================================
 IsStoredProp 函数
===============================================================================
IsStoredProp 函数使用属性信息中的 TPropInfo.StoredProp 函数指针来调用属性定义时用 stored 关键字定义的函数的结果。

这个函数被用于 Delphi 持续机制，TWriter.WriteProperties 方法调用 IsStoredProp 判断是否需要把该属性的值写入流中。

  function IsStoredProp(Instance: TObject; PropInfo: PPropInfo): Boolean;
  function IsStoredProp(Instance: TObject; const PropName: string): Boolean;

===============================================================================
 FreeAndNilProperties 函数
===============================================================================
FreeAndNilProperties 函数用于清除一个对象的所有 published 的对象类型的属性的对象。这个函数调用 GetObjectProp 执行获得对象属性的对象句柄，并调用对象的 Free 方法清除这个对象，然后调用 SetObjectProp 设置该属性为 nil。

  procedure FreeAndNilProperties(AObject: TObject);

我不知道这个函数能用在哪里，至少 VCL 中没有使用这个函数。

===============================================================================
 SetToString / StringToSet 函数
===============================================================================
SetToString 和 StringToSet 是两个 RTTI 辅助函数，它们把集合值转换为字符串，或者把字符串转换为集合值。

  function SetToString(PropInfo: PPropInfo; Value: Integer;
    Brackets: Boolean = False): string;

  function StringToSet(PropInfo: PPropInfo; const Value: string): Integer;

注意：这里的集合值最多只能包含 32 个元素(4 bytes)，这是集合 RTTI 的限制。

===============================================================================
 GetEnumName / GetEnumValue / GetEnumNameValue 函数
===============================================================================
GetEnumName 函数根据枚举整数值返回枚举字符串。它可以返回以下三种枚举名称：

  Integer：直接返回 IntToStr(Integer)
  Boolean：返回 True/False
  Enum   ：返回 TTypeData^.NameList 中存储的枚举名称

  function GetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;

GetEnumValue 函数根据枚举字符串返回枚举整数值。它与 GetEnumName 类似，可以返回三种枚举的整数值，但对于 Enum 类型，它调用了 GetEnumNameValue 函数。

  function GetEnumValue(TypeInfo: PTypeInfo; const Name: string): Integer;

GetEnumNameValue 函数与 GetEnumValue 函数功能差不多，但它是个汇编函数，只能返回纯枚举类型的值。其工作原理也是匹配 TTypeData^.NameList 值。

  function GetEnumNameValue(TypeInfo: PTypeInfo; const Name: string): Integer;

注意：GetEnumNameValue 隐藏在 Implementation 段，不能直接使用，它是为 GetEnumValue 函数服务的。

===============================================================================
 GetOrdProp 函数详解
===============================================================================
GetOrdProp 是 Delphi RTTI 中使用频繁的函数。GetOrdProp 根据对象句柄和对象属性的 TPropInfo 指针获得对象的属性值。它的返回值是 Longint，需要强制转换成相应的属性类型才能使用。

  function GetOrdProp(Instance: TObject; PropInfo: PPropInfo): Longint;

GetOrdProp 调用 TPropInfo.GetProc 函数指针得到属性的返回值。它的工作过程是：

  如果该属性的类型是 class 类型，那么返回值是 4 个字节(对象句柄)。
    否则通过 TTypeData.OrdType 得到返回值的类型，存储在 BL 中。
    { TOrdType = (otSByte, otUByte, otSWord, otUWord, otSLong, otULong); }
  检查 TPropInfo.GetProc 的第一个字节(注意是 GetProc 指针的第一个字节)：
    如果 GetProc[0] = FF，说明GetProc是fieldoffset；如果GetProc[0]=FE，说明 GetProc 是 virtual method offset；
    如果 GetProc[0] < $FE，说明 GetProc 是 static method；
  然后根据不同的 GetProc 类型解析后，调用 GetProc。
  根据 BL 中存储的类型符号信息修正返回值(EAX)的符号信息。
  根据 BL 中存储的类型的大小裁剪返回值 EAX 为 EAX/AX/AL。
  EAX(AX/AL) 即是返回的属性值。

GetOrdProp 的汇编代码及注释如下：

function GetOrdProp(Instance: TObject; PropInfo: PPropInfo): Longint;
asm
       PUSH   EBX
       PUSH   EDI
       MOV    EDI,[EDX].TPropInfo.PropType       ; EDI <- PPTypeInfo
       MOV    EDI,[EDI]                          ; EDI <- PTypeInfo
       MOV    BL,otSLong                         ; BL  <- otSLong
       CMP    [EDI].TTypeInfo.Kind,tkClass       ; if Prop is Class
       JE     @@isClass                          ; jmp @@isClass
       XOR    ECX,ECX                            ; ECX <- 0
       MOV    CL,[EDI].TTypeInfo.Name.Byte[0]    ; CL  <- Name StrLength
       MOV    BL,[EDI].TTypeInfo.Name[ECX+1].TTypeData.OrdType
                                                   ; BL  <- Prop OrdType
@@isClass:
       MOV    ECX,[EDX].TPropInfo.GetProc        ; ECX <- GetProc Addr
       CMP    [EDX].TPropInfo.GetProc.Byte[3],FE;cmpHiByte(GetProc),FE
       MOV    EDX,[EDX].TPropInfo.Index          ; EDX <- Prop Index
       JB     @@isStaticMethod                   ; if below FEJA@@isField;ifisFF

       {      the GetProc is a virtual method }   ; if is FEMOVSXECX,CXsignextendslotoffsADDECX,[EAX]vmt+slotoffsCALLdwordptr[ECX]callvmt[slot]JMP@@final@@isStaticMethod:CALLECX;callGetProcdirectlyJMP@@final@@isField:ANDECX,00FFFFFF       ; clear HiByte(GetProc)
       ADD    ECX,EAX             ; ECX <- Field Addr
       MOV    AL,[ECX]            ; AL  <- Field Addr[0]
       CMP    BL,otSWord          ; if OrdType < otSWord
       JB     @@final             ; Exit
       MOV    AX,[ECX]            ; else AX <- Field[0..1]
       CMP    BL,otSLong          ; if OrdType < otSLong
       JB     @@final             ; Exit
       MOV    EAX,[ECX]           ; else EAX <- Field[0..3]
@@final:
       CMP    BL,otSLong          ; if OrdType >= otSLong
       JAE    @@exit              ; Exit
       CMP    BL,otSWord          ; if OrdType >= otSWord
       JAE    @@word              ; jmp @@word
       CMP    BL,otSByte          ; if OrdType = otSByte
       MOVSX  EAX,AL              ; AL <- Sign(EAX)
       JE     @@exit              ; Exit
       AND    EAX,FF;clearHiWord(EAX)JMP@@exit;Exit@@word:MOVSXEAX,AX;AX<=Sign(EAX)JE@@exit;ifOrdType=otSWordthenExitANDEAX,FFFF           ; clear HiWord(EAX)
@@exit:
       POP    EDI
       POP    EBX
end;

TypInfo.pas 中重载了 GetOrdProp 函数，将 PPropInfo 参数替换为 PropName，方便程序员调用，它其实也是调用了上面介绍的 GetOrdProp 函数。

function GetOrdProp(Instance: TObject; const PropName: string): Longint;
begin
  Result := GetOrdProp(Instance, FindPropInfo(Instance, PropName));
end;

下面是使用 GetOrdProp 的例子：

  Self.Width := Self.Width - GetOrdProp(Self, 'Height');

上面的语句相当于：

  Self.Width := Self.Width - Self.Height;

* 后文介绍的 Get___Prop 系列函数或者调用本函数，或者它的实现方法与本函数类似。

===============================================================================
 SetOrdProp 函数
===============================================================================
SetOrdProp 函数是 GetOrdProp 的逆过程，它调用 TPropInfo.SetProc 函数指针设置对象的属性值。SetProc 指针的第一个字节的意义同 GetProc 一样，也是表示该 SetProc 是字段偏移、虚方法偏移和静态方法。

  procedure SetOrdProp(Instance: TObject; PropInfo: PPropInfo; Value: Longint);

SetOrdProc 也根据属性名称重载了：

  procedure SetOrdProp(Instance: TObject; const PropName: string; Value: Longint);

由于 SetOrdProp 的汇编代码与 GetOrdProp 的几乎一样，在此就不再列出。作为练习，试用一下：

  SetOrdProp(Self, 'Height', Self.Height + 10);

该语句的功能相当于：

  Self.Height := Self.Height + 10;

* 后文介绍的 Set___Prop 系列函数或者调用本函数，或者它的实现方法与本函数类似。

===============================================================================
 GetEnumProp / SetEnumProp 函数
===============================================================================
GetEnumProp 函数获取枚举类型属性的枚举字符串，它调用 GetEnumName 转换 GetOrdProp 的返回值。

  function GetEnumProp(Instance: TObject; PropInfo: PPropInfo): string;
  function GetEnumProp(Instance: TObject; const PropName: string): string;

SetEnumProp 函数使用枚举字符串设置枚举类型属性值，它调用 GetEnumValue 转换枚举字符串后再调用 SetOrdProp 设置属性值。

  procedure SetEnumProp(Instance: TObject; PropInfo: PPropInfo;
    const Value: string);
  procedure SetEnumProp(Instance: TObject; const PropName: string;
    const Value: string);

===============================================================================
 GetSetProp / SetSetProp 函数
===============================================================================
GetSetProp 函数用于获取集合类型属性的字符串值，它也是调用 GetOrdProp 获得属性值，然后调用 SetToString 函数把数值转换成字符串。

注意：GetOrdProp 函数返回值是 Integer，那么它是如何表示可以存储 256 个元素的集合类型呢？答案是：如果是 published 集合属性，那么该集合最大只能是 4 个字节，也就是最多只能存储 32 个元素。

  function GetSetProp(Instance: TObject; PropInfo: PPropInfo;
    Brackets: Boolean): string;
  function GetSetProp(Instance: TObject; const PropName: string;
    Brackets: Boolean = False): string;

SetSetProp 函数用于通过字符串设置集合类型属性的值。它先调用 StringToSet 函数把字符串转换为整数值，然后使用 SetOrdProp 函数设置属性值。

  procedure SetSetProp(Instance: TObject; PropInfo: PPropInfo;
    const Value: string);
  procedure SetSetProp(Instance: TObject; const PropName: string;
    const Value: string);

试验：  SetSetProp(Self, 'BorderIcons', '[biSystemMenu]');


===============================================================================
 GetObjectProp / SetObjectProp 函数
===============================================================================
对象实际上是指针，也就是整数值，所以 GetObjectProp 直接调用 GetOrdProp 就可以了。

MinClass 参数指定得到的 Object 必须属于某个 class ，如果不是则返回 nil 。

  function GetObjectProp(Instance: TObject; PropInfo: PPropInfo;
    MinClass: TClass = nil): TObject;
  function GetObjectProp(Instance: TObject; const PropName: string;
    MinClass: TClass = nil): TObject;

SetObjectProp 用于设置属性的对象句柄。ValidateClass 参数表示是否需要检查传入的对象类型与属性信息的类信息是否兼容。

  procedure SetObjectProp(Instance: TObject; PropInfo: PPropInfo;
    Value: TObject; ValidateClass: Boolean = True);
  procedure SetObjectProp(Instance: TObject; const PropName: string;
    Value: TObject);

例子：
  var
    MyFont: TFont;
  begin
    MyFont := TFont.Create;
   MyFont.Height :=  20;
   SetObjectProp(Self, 'Font', MyFont);
  end;

===============================================================================
 GetStrProp / SetStrProp 函数
===============================================================================
GetStrProp 函数用于获得字符串类型的属性值。

  function GetStrProp(Instance: TObject; PropInfo: PPropInfo): string;
  function GetStrProp(Instance: TObject; const PropName: string): string;

由于 Delphi 支持三种类型的字符串，GetStrProp 根据字符串的类型，分别调用三个获得字符串属性值的函数：

  case PropInfo^.PropType^.Kind of
    tkString: GetShortStrPropAsLongStr(Instance, PropInfo, Result);
    tkLString: GetLongStrProp(Instance, PropInfo, Result);
    tkWString: GetWideStrPropAsLongStr(Instance, PropInfo, Result);
  end;

其中 GetShortStrPropAsLongStr 又调用了 GetShortStrProp；GetWideStrPropAsLongStr 又调用了 GetWideStrProp，进行字符串间的类型转换。

SetStrProp 函数用于设置字符串类型的属性值。它的实现方法与 GetStrProp 类似。

  procedure SetStrProp(Instance: TObject; PropInfo: PPropInfo;
    const Value: string);
  procedure SetStrProp(Instance: TObject; const PropName: string;
    const Value: string);

===============================================================================
 GetFloatProp / SetFloatProp 函数
===============================================================================
GetFloatProp 用于获得浮点型属性值。它将 Single(4 bytes)、Double(8 bytes)、Comp(8 bytes)、Currency(8 bytes) 类型的浮点数属性转换为 Extented(10 bytes) 类型返回。

  function GetFloatProp(Instance: TObject; PropInfo: PPropInfo): Extended;
  function GetFloatProp(Instance: TObject; const PropName: string): Extended;

SetFloatProp 用于设置浮点型属性值。它的实现方法与 GetFloatProp 类似。

  procedure SetFloatProp(Instance: TObject; PropInfo: PPropInfo;
    const Value: Extended);
  procedure SetFloatProp(Instance: TObject; const PropName: string;
    const Value: Extended);

===============================================================================
 GetVariantProp / SetVariantProp
===============================================================================
GetVariantProp 函数用于获得 Variant 类型的属性值。

  function GetVariantProp(Instance: TObject; PropInfo: PPropInfo): Variant;
  function GetVariantProp(Instance: TObject; const PropName: string): Variant;

SetVariantProp 函数用于设置 Variant 类型的属性值。

  procedure SetVariantProp(Instance: TObject; PropInfo: PPropInfo;
    const Value: Variant);
  procedure SetVariantProp(Instance: TObject; const PropName: string;
    const Value: Variant);

===============================================================================
 GetMethodProp / SetMethodProp
===============================================================================
GetMethodProp 函数用于获得 Method 类型的属性值。

  function GetMethodProp(Instance: TObject; PropInfo: PPropInfo): TMethod; 
  function GetMethodProp(Instance: TObject; const PropName: string): TMethod;

SetMethodProp 函数用于设置 Method 类型的属性值。

  procedure SetMethodProp(Instance: TObject; const PropName: string;
    const Value: TMethod);
  procedure SetMethodProp(Instance: TObject; PropInfo: PPropInfo;
    const Value: TMethod);

===============================================================================
 GetInt64Prop / SetInt64Prop
===============================================================================
SetInt64Prop 函数用于设置 Int64 类型的属性值。不同于一般整数用 EAX 返回，Int64 类型的返回值由 EDX:EAX 返回，所以有必要单独定义 Int64 的获取和设置方法。

  function GetInt64Prop(Instance: TObject; PropInfo: PPropInfo): Int64;
  function GetInt64Prop(Instance: TObject; const PropName: string): Int64;

SetInt64Prop 函数用于设置 Int64 类型的属性值。

  procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo;
    const Value: Int64);
  procedure SetInt64Prop(Instance: TObject; const PropName: string;
    const Value: Int64);

===============================================================================
 GetInterfaceProp / SetInterfaceProp 函数
===============================================================================
GetInterfaceProp 函数用于获得 Interface 类型的属性值。

  function GetInterfaceProp(Instance: TObject; PropInfo: PPropInfo): IInterface;
  function GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;

SetInterfaceProp 函数用于设置 Interface 类型的属性值。

  procedure SetInterfaceProp(Instance: TObject; PropInfo: PPropInfo;
    const Value: IInterface); 
  procedure SetInterfaceProp(Instance: TObject; const PropName: string;
    const Value: IInterface);

* 不太熟悉 Interface，以后再看实现过程。

===============================================================================
 GetPropValue / SetPropValue 函数
===============================================================================
GetPropValue 函数用于获得任何类型的属性值，它返回 Variant 类型。

注意，这个函数没有重载函数，只能使用属性名称字符串为参数。

GetPropValue 先调用 GetPropInfo 函数获得属性的类型，然后根据属性的数据类型选择调用以上介绍的 GetOrdProp、GetEnumProp、GetSetProp、GetStrProp 等函数实现具体的功能。

GetPropValue 的参数 PreferStrings 如果设置为 True，那么对于枚举、集合类型，将返回字符串值，否则返回整数值。GetPropValue 还可以返回动态数组类型的属性值。(目前对动态数组不太熟悉，先记下来。)

  function GetPropValue(Instance: TObject; const PropName: string;
   PreferStrings: Boolean): Variant;

SetPropValue 函数用于设置任何类型的属性值。SetPropValue 的实现与 GetPropValue 类似。并且 SetPropValue 内部分析 Value 参数是否是字符串来设置枚举和集合类型的属性，所以不需要 PreferStrings 参数。SetPropValue 也可以设置动态数组属性，它使用了 SetOrdProp 函数实现这一功能，看来动态数组在内存中的表现是一个指针。

  procedure SetPropValue(Instance: TObject; const PropName: string;
    const Value: Variant);

===============================================================================
 TPublishableVariantType class
===============================================================================
在 TypInfo.pas 的代码注释中说 TPublishableVariantType 是用来代替 TCustomVariantType 以便更容易在 RTTI 中使用自定义的 Variant 类型。

* 现在对这两个类型都不太了解，先记在这里以后再学。

===============================================================================
 RegisterClass / FindClass 系列函数 (Classes.pas)
===============================================================================
Delphi 提供了一种机制，可以使用类(class)的名称获得类(class VMTptr)。缺省情况下这些类必须是从 TPersistent 类继承下来的。使用这项功能之前必须在先把类信息注册到全局对象 RegGroup 中。

RegisterClass 函数用于注册类信息至 RegGroup 中，注意该函数名称和 Win32 API 中注册窗口类的函数同名。如果类已经被注册过了，RegisterClass 将直接返回。如果有一个不同的类以相同的名称注册了，RegisterClass 将触发异常(EFilerError)。

  procedure RegisterClass(AClass: TPersistentClass);

RegisterClasses 函数可以方便地注册一批类：

  procedure RegisterClasses(AClasses: array of TPersistentClass);

RegisterClassAlias 函数可以为类以其它的名称注册，以避免名称冲突。

  procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);

GetClass 函数根据类名称字符串获得类(class)，如果没找到，将返回 nil：

  function GetClass(const AClassName: string): TPersistentClass;

FindClass 函数包装了 GetClass，不同的是如果没找到该类，则触发异常(EClassNotFound)：

  function FindClass(const ClassName: string): TPersistentClass;

UnRegisterClass 系列函数执行 RegisterClass 相反的工作：

  procedure UnRegisterClass(AClass: TPersistentClass);
  procedure UnRegisterClasses(AClasses: array of TPersistentClass);
  procedure UnRegisterModuleClasses(Module: HMODULE);

缺省的 RegGroup 用于组织从 TPersistent 继承下来的类，下面五个函数可以设置自己的 RegGroup：

  procedure StartClassGroup(AClass: TPersistentClass);
  procedure GroupDescendentsWith(AClass, AClassGroup: TPersistentClass);
  function ActivateClassGroup(AClass: TPersistentClass): TPersistentClass;
  function ClassGroupOf(AClass: TPersistentClass): TPersistentClass; overload;
  function ClassGroupOf(Instance: TPersistent): TPersistentClass; overload;

===============================================================================
 IdentToInt / IntToIdent 系列函数 (Classes.pas)
===============================================================================
IdentToInt 和 IntToIdent 函数用于实现字符串值和数值之间的转换。它的原理很简单，就是通过数组一一映射查找。不过一般不用直接使用这两个函数，而是使用 Delphi 中已经包装好的函数。这些函数的返回值都是 Boolean，表示转换是否成功。

  function IdentToInt(const Ident: string; var Int: Longint;
    const Map: array of TIdentMapEntry): Boolean;
  function IntToIdent(Int: Longint; var Ident: string;
    const Map: array of TIdentMapEntry): Boolean;

  { Graphics.pas }
  function CharsetToIdent(Charset: Longint; var Ident: string): Boolean;
  function IdentToCharset(const Ident: string; var Charset: Longint): Boolean;

  function ColorToIdent(Color: Longint; var Ident: string): Boolean;
  function IdentToColor(const Ident: string; var Color: Longint): Boolean;

  { Controls.pas }
  function CursorToIdent(Cursor: Longint; var Ident: string): Boolean;
  function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean;

例子：
  var
    NewColor: Integer;
  begin
    if IdentToColor('clWindow', NewColor) then
     Self.Color := NewColor;
  end;
  
===============================================================================
 结束
===============================================================================
```

