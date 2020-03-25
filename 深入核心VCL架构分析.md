# 第一章

## 事件/消息模型

```pascal
// 消息的定义
MyMessage = packed record
  MessageID: Longint;  //发生事件的消息
  wParam: Longint;  // 辅助信息，例如鼠标的位置或者用户输入的字符
  lParam: Longint; // 同上
  pt: TPoint;  // 存储鼠标的全域坐标
end;
```

1. 应用程序由很多窗口，如何找到该消息对应的窗口。
2. 找到了窗口之后怎样将该消息分派给应用程序或窗口。
3. 当分派完成后，窗口怎样处理该消息。

```pascal
MyMessage = packed record
  hwnd: HWND;  // 代表窗口的独特识别值
  message: UINT;
  wParam: WPARAM;
  lParam: LPARAM;
  time: DWORD;
  pt: TPoint;
end;
```

​		这个时候事件/消息模型已经相当完备了，这个时候还有一个问题，就是当用户快速的在应用程序中点击了数个位置不同的鼠标键，那么会产生好几个事件，因此，需要为应用程序来暂存消息。我们只需要为每一个应用程序建立一个消息队列（Message Queue），当事件发生时执行环境就把代表它的消息分派到消息队列中，等待应用程序从其中取出并处理。

​		这样第一个问题就解决了，下面解决第二和第三个问题。

​		首先执行环境如何为应用程序创建窗口，那么我们要获得下面的信息。

* 窗口的位置和大小。

* 窗口的格式，使用的颜色以及使用的光标种类。

* 窗口使用的菜单以及其他的资源。

* 当这个窗口发生事件时，能处理窗口消息的函数地址。	

​		当有了上面的信息之后，执行环境就可以为应用程序创建窗口了，那么应用程序如何给应用程序提供这些信息呢？

```pascal
MyWindowClassInfo = packed record
  style: UINT;  // 窗口的格式
  iWidth: Integer;
  iHeight: Integer;
  lpfnWndProc: Pointer;  // 函数指针
  hIcon: HICON;
  hCursor: HCURSOR;  // 窗口使用的光标种类
  hbrBackground: HBRUSH;
  lpszMenuName: PAnsiChar;
  lpszClassName: PAnsiChar; // 类名称
  hIconSm: HICON;
end;
```

​		当然，由于应用程序可以创建多个窗口，因此它可以提供多个窗口信息数据结构，他为每一种窗口提供一个MyWindowClassInfo数据结构，在分别向执行环境注册每一种窗口的MyWindowClassInfo信息，那么当它需要创建特定的窗口种类时，只要提供欲创建窗口的类名称，执行环境再根据类名称找到相对应的MyWindowClassInfo数据结构，最后根据MyWindowClassInfo之中的信息来创建窗口。因此在MyWindowClassInfo数据结构中LpszClassName字段代表的就是窗口的类名称。

​		下面讨论剩下的两个问题：1. 找到了窗口之后如何发给这个窗口； 2. 窗口接受到消息之后怎么处理消息

​		要让窗口处理消息最简单的方法就是让执行环境调用窗口提供的函数并传递消息给此函数来处理。但如何让窗口提供函数给执行环境，执行环境才能够知道在消息产生后调用什么函数呢？应用程序只需要把一个能够处理消息的函数的地址指定给MyWindowClassInfo中特定的字段给执行环境就可以了，所以lpfnWndProc字段就是一个Pointer类型的字段，代表应用程序可以吧任何的函数地址指定给此字段来代表可以处理此窗口消息的函数。由于这个函数会由执行环境调用，因此这种函数也被成为回调函数。

​		由于执行环境在调用应用程序提供的回调函数时必须传递和消息相关的信息给此函数，因此执行环境必须定义此函数的原型（Prototype）, 意即执行环境必须定义此回调函数接受的参数格式，回传数值以及调用惯例等，如此一来执行环境才能够正确传递必要的参数给会掉函数。例如，执行环境可以定义回调函数必须要有如下原型

```pascal
function WindowProc(Window: HWnd; AMessage: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall; export;
```

## 撰写Windows应用程序的步骤

* 定义窗口类的内容，以决定窗口的创建格式以及回调函数

* 注册窗口类

* 创建窗口

* 进入窗口消息处理循环以便让回调函数处理消息窗口，知道应用程序结束位置。

  ​	下面的object Pascal 程序即遵照了上面的步骤来开发原生Windows应用程序

  ```pascal
  program ObjectPascalWinHello;
  
  uses 
    Windows, Messages; SysUtils;
  const
    AppName = 'ObjectPascalHello';
  function WindowProc(Window: HWnd; AMessage: UINT; WParam: WPARAM; 
                  LParam: LPARAM): LRESULT; stdcall; export;
  var
    dc: hdc;
    ps: TPaintStruce;
    r: TRect;
  begin
    WindowProc := 0;
    case AMessage of
      WM_
  end;
  ```


# 第二章  VCL的诞生和设计原理

## 设计目标

1. 采用单继承模式
2. VCL FrameWork必须不限于16位或32位平台
3. 必须提供开放的组件架构，以允许程序员开发自定义空间
4. 必须进化成可在设计时期即提供功能的Framework
5. 必须使用PME（Property-Event-Method）模型
6. 必须使用面向对象计数来设计和实现
7. 必须完善的封装和分派窗口消息

## VCL对象声明的成型

​		由于VCL使用面向对象技术设计和实现，所以第一步要提供VCL对象声明周期（Object Lifecyle）以及VCL对象管理（Object Management）的能力。一旦有了这个基础的对象能力，其他的VCL Framework就可以依据此基础服务来派生出其他的服务。

​		基本的对象管理服务至少包括以下的服务

1. 对象的创建和初始化
2. 对象方法的分配
3. 对象的消灭

## Object Pascal 的对象模型

```pascal
TObject = class
  constructor Create;
  destructor Destroy; virtual;
end;
```

```pascal
TMyObject = class(TObject)
  destructor Destroy; override;
end;

Obj := TMyClass.Create;
```

​		当运行上面那句话的时候，到底发生了什么呢？其实发生了很多事情，包括分配内存，设定字段变量数据结构以及设定执行框架等工作，因此，上面的代码也可以分解成如下的代码

```pascal
TMyObject.AllocateMemory;
TMyObject.InitializeSpecialFields;
Obj := TMyObject.SetupExecFrame;
```

​		创建对象的第一步是分配内存，Object Pascal会使用内建的内存管理器来为对象分配内存

```pascal
PMemoryManager = ^TMemoryManager;
TMemoryManager = record
  GetMen: function(Size: Integer): Pointer;
  FreeMem: function(P: Pointer): Integer;
  ReallocMen: function(P: Pointer; Size: Integer): Pointer;
end;
```

​		在分配了对象原始内存之后，Object Pascal 的对象模型会先初始化所有的内存内容为0： 

```pascal
FillChar(Instance^, InstanceSize, 0);
```

​		之后会对一些特别的字段进行初始化，例如虚拟方法表，接口的引用计数，动态数组初始化内存块。

## 从原始内存到对象雏形

​		把对象模型分配的内存内容变成活生生的存在于内存中的对象。这需要为对象设定正确的VMT并串联起正确的继承架构。

## Object Pascal对象服务

1. 对象创建服务
2. 对象释放服务
3. 对象识别服务：提供对象判断，识别的机制
4. 对象信息服务：提供程序代码存取对象信息的服务
5. 对象消息分派服务：提供Object Pascal分派消息的服务，和VCL封装窗口消息有密切关系。