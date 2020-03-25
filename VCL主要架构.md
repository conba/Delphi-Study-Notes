# 深入浅出VCL

![](\VCL主要架构.png)
1. TObject （定义他的单元是System）

   主要定义了四类功能的虚方法：

   （1） 对象构造函数和析构函数

   （2）返回运行时类型信息。

   ```pascal
   class function ClassName: ShortString;{返回类或者对象的类型名}
   function ClassType: TClass;{返回对象的类型（即类引用）}
   class function ClassParent：TClass;{返回类或者对象的父类类型}
   class function ClassInfo: Pointer;{返回类或者对象的运行时类型信息表的地址}
   ```

   （3）支持消息处理。有方法Dispatch和DefaultHandler提供。

   （4）支持接口实现。有方法GetInterface和类方法GetInterfaceEntry、GetInterfaceTabel提供。

2. TPersistent（Classes，抽象类）

   主要有两类功能：

   （1）对象相互复制。AssignTo 和Assign这两个虚方法提供，他们都要有子类具体实现。

   （2）在流里读写属性的能力，凡是从TPersistent继承下来的TComponent使得所有的对象都有进行流操作的能力。

   **流是一个以二进制数据形式封装在存储介质（例如内存或磁盘文件）中的对象，因为delphi窗体文件是通过流实现的，直接从TPersistent继承下来的TComponent使得所有的控件具有生成Form文件的能力** 。

   TPersistent是抽象类，不要直接创建其实例。

   ```pascal
   TPersistent = class(TObject)
   ```

   RTTI（Run-Time Type Information）,运行时类型信息，也就是说在运行期获得数据类型或类的信息。

3. TComponent（Classes，抽象类）

   TComponent具有四类主要功能：

   （1）注册后可以出现在组件页；设计时可见、可以管理；运行时不可见。

   （2）可以拥有别的对象而成为其他对象的拥有者（Owner）。

   （3）加强了流读写功能。

   （4）可以转化为ActiveX控件和别的COM类。

   （5）是第一个可以被用来创建新组件的基类，非可视化组件也是从TComponent继承下来。

   TComponent是抽象类，不要直接创建其实例。如果开发运行时不可见控件，可以从TComponent继承，否则可以从TWinControl或其子类继承。

4. TControl（Controls）

   TControl是控件类，所谓控件，是运行时可见的组件。VCL所有控件都是TControl的直接或间接子类。

   两种基本的可视化空间：graphic controls 和 windowed controls，他们的代表分别是TGraphicControl类和TWinControl类。他们之间主要的区别在于TGraphicControl继承下来的控件没有窗口句柄，因而也没有输入焦点。

   窗口控件进一步分可以分为两类。一类是直接从TWinControl继承下来的通过Windows内部控件实现的控件，他能够自己自动实现重绘功能。而另一类TCustomControl类则指那些需要窗口句柄控件但未封装提供重绘功能的Windows控件。

5. TWinControl（Controls）

   TWinControl是所有控件类控件的祖先类。窗口控件有一下特点：

   （1）可以有输入焦点。

   （2）可以接收键盘输入。

   （3）可以作为其他控件的容器。

   （4）有句柄（Handle）属性。
   
6. 可以将VCL分为三个主要区：组件区，通用对象区和异常区。组件区都是从TComponent继承而来的。有一些没有继承TComponent，这些非组件类分布在VCL的通用对象区和异常区。

   这些非组件类主要由两个好处，一是非组件类可以定义组件属性的数据类型，如图像组件（TGraphic对象）的Picture属性或列表框（TString对象）的Items属性。这些类一般继承自TPersistent，所以时流式的，可以由子属性甚至事件；二是可以直接使用。在用户编写的delphi代码中，可以分配和处理这些类的对象。

   ![](.\delphi主要基类及其派生类的关系.png)

### 可视化控件和非可视化空间的区别

​	是否有窗口句柄。

**查看事件的调用者**

```pascal
procedure TForm1.OnClick(Sender: TObject);
var
  Str: string;
begin
  Str := '';
  Str := Sender.ClassName;  //引发事件对象所属的类
  Str := Sender.classParent.ClassName；//父类名
  Str := IntToStr(Sender.InstanceSize);//实际字节数
end;
```

**获得某组件的祖先类的类型和祖先类的相关属性**

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  ClassRef: TClass;
begin
  ClassRef := Sender.ClassType;
  while ClassRef <> nil do
  begin
    ListBox1.Items.add(ClassRef.ClassName);
    ClassRef := ClassRef.ClassParent;
  end;
end;
```

## TPersistent: 持久化对象、

​		该类时Delphi可视化编程的基础。该类实现了对象公布（published）属性的存取，即在该类及派生类中声明的publish的属性，方法，事件等可在设计期时显示在Object Inspector窗中。能在Object Inspector中对对象的publishe属性进行设计期的设计，并可将设置的值存到窗体或者数据模块的DFM文件中。

```pascal
{$M+}
TPersistent = class(TObject)
private
  procedure AssignError(Source: TPersistent);
protected
  procedure AssignTo(Dest: TPersistent); virtual;
  procedure DefineProperties(Filer: TFiler); virtual;
  function GetOwner: TPersistent; dynamic;
public
  destructor Destroy; override;
  procedure Assign(Source: TPersistent); virtual;
  function GetNamePath: string; dynamic;
  {$M-}
```

TPersistent类的声明只有七个方法，实际上完成类的属性，方法，事件的存取工作的代码并没有定义在TPersistent类中，而是由Delphi另行定义，如TReader和TWriter等对象，这些对象都是以TPersistent为服务对象的。

从TPersistent类的声明看，他与TObject的声明方式和结构基本相同，但是TObject却没有published属性，方法和事件的设计期的存取功能呢？因为TPersistent的声明中有**M** 编译开关，在**{$M+}** 和**{$M-}** 间声明时，程序编译器会为类生成与**RTTI（Runtime Type Infomation）** 相关的代码来完成类的published的属性、方法和时间的存储工作。并且该类的子类的published属性、方法和事件也具有存取特性。如果一个类或其祖先类都没有在**{$M+}** 和**{$M-}** 声明，则该类不能有published的属性、事件、和方法。

一般而言，TPersistent 的Assign方法比较常用，他主要完成两个对象的复制。对于两个对象，如

```pascal
var
  Obj1, Obj2: TFont;
begin
  Obj1 := Obj2;
end;
```





