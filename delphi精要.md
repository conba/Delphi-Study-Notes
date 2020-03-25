1. 无类型参数前面必须加上Const、Out或Var前缀；无类型参数不能指定默认值。
2. 在使用无类型参数的时候必须对参数进行显示，这种限制一般在过程的实现中完成的，在运行时检查参数值的实际类型。
3. 

# 4 VCL入门

## 4.2 组件与控件的概念

1. VCL是可视化组件库的简称（Visual Component Library）。
2. 一个组件就是类。该类按照面向对象编程的原理封转了一系列功能。在程序中，我们可以创建类的实例，即对象。
3. 其次，在delphi的组件面板，有很多分页，如“Standard”、“Additional”、“System”等。每个组件页下有很多组件，可以拖入窗体或者数据模块进行设计。因此，我们说组件在程序设计时时可以进行可视化设计的。
4. 组件的概念就是：可以在程序设计时进行可视化设计的类。在delphi中，一个类被注册后，就会出现在组件页，从而成为一个组件。
5. 控件是组件的一种。这是根据组件在程序运行时的可视性来划分的，在程序运行时可见的组件就是控件。

## 4.3 使用VCL

* 使用VCL，就是使用它提供的类，组件。可以通过两种途径使用VCL的类，组件。
  1. 设计时加入组件进行属性和事件设计，然后就可以运行程序了。这种是通过静态的方式使用组件。
  2. 用代码创建类的实例（对象），然后使用它提供的功能，最后销毁它。这可以看作是通过动态方式使用组件。

# 5 VCL精要

![VCL架构](\VCL架构.png)

* TObject（单元：System，主要定义了四类功能的虚方法）

  1. 对象的构造和析构函数
  2. 返回运行时类型信息。虽然TObject提供了此功能，但是它隐藏了，而在子类TPersistent中公开

  ```pascal
  class function ClassName: shortString; {返回类或者对象的类型名}
  function ClassType: TClass;            {返回对象的类型(即类引用)}
  Class function ClassParent: TClass;    {返回类或者对象的父类类型}
  Class function ClassInfo: Pointer;     {返回类或者对象的运行时类型信息表的地址}
  ```

  3. 支持消息处理。由方法Dispatch和DefaultHandler提供。
  4. 支持接口实现。由方法GetInterface和类方法GetInterfaceEntry、GetInterfaceTable提供。

* TPersistent（单元：Class，抽象类）

  1. 对象的相互复制。AssignTo和Assign这两个虚方法提供，他们都要子类具体实现。
  2. 在流里读写属性的能力。
  3. 提供了RTTI的能力。

  ```pascal
  TPersistent = class(TObject)
  ```

* TComponent（单元：Classes，抽象类）

  1. 注册后可以出现在组件页；设计时可见、可以管理； 运行时不可见。
  2. 可以拥有别的对象而成为其他对象的拥有者（Owner）。
  3. 加强了流读写能力。
  4. 可以转化为ActiveX空间和别的COM类。

  TComponent 是抽象类，不要直接创建其实例。如果你需要开发运行时不可见组件，可以从TComponent 继承，否则可以从TWinControl 或其子类继承。

* TControl（Controls）

  TControl是控件类。所谓控件，时运行时可见的组件。VCL所有控件都是TControl的直接或间接子类。

* TWinControl(（Controls）

  TWinControl时所有窗口类控件的祖先类。窗口控件有以下特点：

  1. 可以有输入焦点。
  2. 可以接受键盘输入
  3. 可以作为其他控件的容器
  4. 有句柄（Handle）属性。

  TWinControl 定义了窗口空间的共同属性、方法、事件。对于不同类型的窗口空间，VCL定义类TCustomComBox、TCustomEdit等多个i类。大部分窗口控件都不是直接从TWinControl 而是从其子类派生。TWinControl的窗口图像是通过调用Windows底层方法内部完成的，而不能自定义绘制图形图像。

* TGraphicControl（Controls）

  TGraphicControl是所有非窗口类控件的祖先类。

  非窗口空间也有四个特点，而这些特点和TWinControl的四个特点相反，所以它是轻量级控件，资源消耗比TWinControl 少很多。

  TGraphicControl 增加了非常重要的Canvas（画布，TCanvas类型）属性，从而提供在控件表面自定义绘制图形图像的能力；增加了Paint 方法类响应WM_PAINT 消息，实现自定义绘制。如在TSpeedButton上可以绘制一个图像。

  特别指出：TCustomControl从TWinControl继承，是窗口类。但是也具有非窗口类的特点：具有Canvas 属性和Paint 方法。

## 5.1.2 构造和析构的内幕

* TObject 中只声明了Create 方法和 Destroy 方法。没有实现这两个方法。事实上编译器会在这两个函数调用之前插入_ClassCreate 和 _ClassDestroy。也就是说，Create 和Destroy 只是在对象已经构造和析构前初始化和反初始化对象成员。

* 在VCL组件开发是，常常有如下代码

  ```pascal
  type
    TDemoClass = class(TComponent)
     private
       OneObject: TOneClass;
     public
       Constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;
    end;
    
    constructor TDemoClass.Create(AOwner: TComponent);
    begin
      inherited Create(AOwner);
      // 或者直接写‘inherited；’。当要继承的一个方法已被重载时，必须书写完整，不然编译器不知道你要继承那个方法。这行是继承父类的Create代码。
      OneObject := TOneClass.Create(Self);
    end;
    
    destructor TDemoClass.Destory;
    begin
      OneObject.Free;
      // 或者FreeAndNil(OneObject);
      inherited Destroy;
    end;
  ```

  TComponent 中重新声明了Create方法

  ```pascal
  constructor Create(AOwner: TComponent); virtual;
  ```

  在TComponent及其子类中，已经不在可能使用无参数的TObject.Create了。

  另外，在整个构造和析构过程中还调用了TObject两个虚方法：AfterConstruction 和 BeforeDestruction。在子类中覆盖他们之后，可以做一些发夹工作。比如：在TCustomForm(TForm的父类)中用来触发OnCreate和OnDestroy：

  ```pascal
  type
    TCustomForm = class(TScrollingWinControl)
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
    end;
    
    procedure TCustomForm.AfterConstruction;
    begin
      if not OldCreateOrder then DoCreate;
      // 然后在DoCreate中调用FOnCreate, 从而触发事件OnCreate.
    end;
    
    procedure TCustomForm.BeforeDestruction;
    begin
      if not OldCreateOrder then DoDestroy;
      //然后在DoDestroy中调用FOnDestroy, 从而触发时间OnDestroy.
    end;
  ```

  总上所述，一个对象总的构造和析构过程如下：

  ```pascal
  _ClassCreate -> Create -> AfterConstruction(->DoCreate/ OnCreate) 
  -> 使用对象 -> 
  BeforeDestruction -> (DoDestroy/ OnDestroy ->) Destroy -> _ClassDestroy.
  ```

## 5.1.3 虚拟方法表和动态方法

* 首先明确本书中的两个概念：虚方法和虚拟方法。

  ***虚方法***指的是用***virtual***和***dynamic***两个关键字来修饰的方法。***虚拟方法***是用***virtual***来修饰的方法，***动态方法***是用***dynamic***来修饰的方法。

* VMT是***虚拟方法表***，虚拟方法表被分成了一个个的大小为4个字节的指针。

* ![VMT结构图](\VMT结构图.png)

* 从上图可以看出，一个对象实际上是一个指针，该指针指向对象的实际数据区，也被称为对象数据区。那么对象的字段，方法，属性和事件这些对象数据在“对象数据区”中怎么组织的呢？

  1. 头4个字节存放一个指针，该指针指向另一个地址区域。
  2. 区域小区域分别存储对象的各种数据成员（即字段，不包括方法。方法的入口被定义在另一个内存表中，虚拟方法表中有一个指针指向该表）

  头4个指针指向虚拟方法表，虚拟方法表有被分成了很多个大小为4个字节的指针，虚拟方法表中正偏移的区域的每个指针对应一个虚拟方法的入口地址。虚拟方法表中负偏移的区域的每个指针用来存放字段，属性值和所有非虚方法的入口地址。（好像所有非虚方法也有一张表来存储）。

* 虚拟方法表的结构

  1. VMT还可以细分成两个区域，即基础信息区和用户定义虚拟方法区。
  2. VMT对应于类而不是对象。

* VMT的负偏移区是基础信息区（-76~0），存储了基础性数据，如实例大小，基础性数据的指针，如接口表，运行时类型信息表，字段表，方法表，类名和父类虚拟方法表等和所有基础性虚拟方法，这些方法都是在类TObject中定义的，如：AfterConstruction，BeforeDestruction，DefaultHandler，Destroy的指针。所以基础信息区并不全是指针列表。这个区域所存放的数据和指针主要是用来帮助实现对象的构造和析构，运行时类型信息存取，字段和方法解析等。基础信息区的大小是固定的。

* VMT的正偏移区（从0开始）是用户定义的虚拟方法（即所有非TObject定义的虚拟方法）所在区域，每4个字节存储一个用户定义的虚拟方法指针。这些虚拟方法不管是在本类中定义的。还包括从TObject一直到本类的所有中间类定义的所有虚拟方法。因此，这个区域的大小是由虚拟方法的多少决定的。

* 不同的类总是具有独立的VMT，也就是说，VMT是根据类的定义来生成的，跟类实例没有关系。

* 类引用类型的定义

  ```pascal
  function TObject.ClassType: TClass;
  begin
    Pointer(Result) := PPointer(Self)^;
  end;
  ```

  这里为什么将Result强制转化成Pointer类型？

  PPointer(Self) 是将Result 强制转化成指向指针的指针类型，PPointer 类型变量中的内容一定是一个指针。也就是Pointer类型，Pointer变量中存的是无类型变量的地址，他和PInteger，PBoolean不同。PInteger 类型的指针变量存的一定是一个Integer类型变量的地址，因此，必须要把Result 转化成一个指向无类型变量的指针才能够赋值，不然会出错。

* 可见，类引用实际上就是指向VMT的指针。也就是说，类引用和VMT有唯一的对应关系。

* 类的VMT有编译器产生，就等着一些指针区指向他。

* 如果TParent定义了一个虚方法F，并且实现了部分功能，然后TChild派生于TParent，并覆盖了虚拟方法F，并做了附加功能实现。这个时候编译器在编译的时候这两个类会各自产生一个VMT，每个VMT中都会有各自的F指针，互不相关。

## 5.1.4 TObject 如何使用虚拟方法表

* 虚拟方法表的分布图

  ![虚拟方法表](\虚拟方法表.png)

  ![](\虚拟方法表1.png)

  ![](\虚拟方法表2.png)

## 5.2 VCL消息机制

* Windows是一个基于消息的操作系统。当运行程序的时候，程序中每个有窗口的控件（TWinControl，也就是有句柄的控件）都会在Windows中注册一个窗口过程，这个过程在VCL中叫做MainWndProc。Windows消息到达MainWndProc后，由MainWndProc调用一系列方法来处理这个消息。

  VCL消息机制的整个流程如下：

  ```pascal
  Windows -> Delphi Application -> TWinControl -> MainWndProc —> WndProc -> Dispatch -> Handler
  ```

  

