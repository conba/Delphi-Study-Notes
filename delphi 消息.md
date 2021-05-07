# 消息

## 消息的定义

1. windows中消息的定义

   ```pascal
   PMsg = ^TMsg
   tabMSG = packed record
     hwnd: HWND;  // 这个是窗口句柄，真正由句柄的类是从TWinControl继承下来的。
     message: UINT;
     wParam: WPARAM;
     lParam: LPARAM;
     time: DWORD;
     pt: Pointer;
   end;
   TMSG = tagMSG;
   MSG = tagMSG;
   
   // TMsg 是根据windows定义的消息类型使用delphi翻译过来的
   ```

2. 上面的消息在delphi中不太方便，然后delphi中自己定义了新的消息类型

   ```pascal
   PMessage = ^TMessage;
   TMessage = packed record
     Msg: Cardinal;
     case integer of
     0:(
     	WParam: Longint;
     	LParam: Longint;
     	Result: Longint;
     );
     1:(
     	WParamLo: Word;
     	WParamHi: Word;
     	LParamLo: Word;
     	LParamHi: Word;
     	ResultLo: Word;
     	ResultHi: Word;
     );
   end;
   
   // TMessage可以保存任何消息，然后delphi还定义了针对不同消息的多种记录类型。例如键盘消息记录TWMKey, 鼠标消息记录TWMMouse, 命令消息记录TWMCommand;
   // 最开始就有点奇怪，窗口的句柄去哪了？（现在个人认为这是在vcl中使用的消息，句柄在WinControl.）
   ```

## 消息处理

* 从操作系统实现上来讲，Windows会根据当前发生的事情创建一条消息，并将其放到应用程序消息队列的末尾，应用程序从消息队列中获取消息并分派给指定的窗口或组件。每个窗口都定义了所谓的**窗口过程**，该过程负责接受并响应消息，再将结果返回给操作系统。

* Windows中消息的产生时间是不确定的，应用程序只有在接受到消息后才进行特殊处理，没有接受到消息时执行自己的既定任务或者什么都不干。

* 实现消息的处理方法一般是消息循环。

  ```pascal
  // 处理消息
  function TApplication.ProcessMessage(var Msg: TMsg): Boolean;
    var
      Handled: Boolean;
  begin
    Result := False;
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      if Msg.Message <> WM_QUIT then
      begin
        Handled := False;
        if Assigned(FOnMessage) then  // 这里
        	FOnMessage(Msg, Handled);  // 如果Handled返回为true，则说明消息已经处理完成，就不走DispatchMessageW(Msg)
        if not IsHint(Msg) and not Handle and not IsMDIMsg(Msg) and 
          not IsKeyMsg(Msg) and not IsDlgMsg(Msg) then
        begin
          TranslateMessage(Msg);
          if Unicode then
            DispatchMessageW(Msg)
          else
            DispatchMessageA(Msg);
        end;
      end
      else
        FTerminate := True;
    end;
  end;
  
  // 消息循环
  procedure TApplication.ProcessMessages;
  var
    Msg: TMsg;
  begin
    while ProcessMessage(Msg) do {loop};
  end;
  
  // 对Application 的 FOnMessage 赋值
  procedure TForm1.MessageProc(var Msg: TMsg; var Handled: Boolean);
  begin
    if Msg.message = JM_DATA then
    begin
      Memo.Lines.Add('Application.OnMessage has processed JM_DATA.');
      Handled := False;
  //    Handled := True;
    end;
  end;
  
  // 在Create是对OnMessage进行赋值
  procedure TForm1.FormCreate(Sender: TObject);
  begin
    Application.OnMessage := Self.MessageProc; // 这样就
  end;
  ```
  
  

## 消息的分类

* Windows消息可以分为4大类：Windows标准消息，通知消息，命令消息和用户自定义消息

## 消息的发送

* 在VCl中，一般有三种方式发送消息

  1. Sendmessage，PostMessage和PostThreadMessage
  2. TControl的Perform
  3. TWinControl的Broadcast

  ```pascal
  type
    TControl = class(TComponent)
    public
      // 一般用来向自己发送消息
    	function Perform(Msg: Cardinal; WParam, LParam: Longint): Longint;
    end;
    
    TWinControl = class(TControl)
    public
    	procedure Broadcast(var Message);
    end;
  
  function PostMessage(hWnd: HWND; Msg: Cardinal; WParam, LParam: Longint): LongBool;
  function PostThreadMessage(idThread: Cardinal; Msg: Cardinal; WParam, LParam: Longint): LongBool;
  function SendMessage(hWnd: HWND; Msg: Cardinal; WParam, LParam: Longint): Longint;
  
  // 前两种方式中已经明确了消息要发送的窗口，下面三种方式可以指定窗口发送消息。
  // postMessage会将消息投递到创建由hWnd参数指定的窗口的线程消息队列中，该函数立即返回而不等待接受消息的线程响应完毕。
  // sendMessage会将消息发送到有hWnd参数指定的窗口，并且在该窗口没有处理完毕该消息是不会返回。
  // 从原理上来说，PostMessage会进消息队列，而sendMessage不会进消息队列。
  ```

## VCL处理消息的流程

```pascal
// Application 中 FOnMessage 的定义
type
  TMessagEvent = procedure(var Msg: TMsg; var Handled: Boolean);
TApplication = class(TComponent)  
private
  FOnMessage: TMessageEvent;
public
  property OnMessage: TMessageEvent FOnMessage write FOnMessage;
end;

// FOnMessage处理完消息后如果要继续处理消息（Handled = False）,那么由DispatchMessage将消息派发到某个窗口过程（这个DispatchMessage是user32.dll中的函数，是WindowsAPI），这个窗口过程就是StdWndProc函数，StdWndProc函数基本上起到了消息中转站的作用，由它将消息派发给某个对象

// 如果接收到消息对象为TWinControl、TCommonDialog、TClipboard、TDragObject、TPopupList类的实例，则MainWndProc方法会被调用以处理该消息。之后WndProc方法将得到该消息，消息在WndProc方法中进入VCL消息派发机制，由Dispath方法将消息发送给某个消息句柄。

unit Classes
type
  TWndMethod = procedure(var Message: TMessage) of object;
// TControl
TControl = class(TComponent)
private
  FWindowProc: TWndMethod;
protected
  procedure WndProc(var Message: TMessage); virtual; // 这个是虚方法，可以重载
public
  Constructor Create(AOwner: TComponent);
  property WindowProc: TWndMethod read FWindowProc write FWindowProc;
end;

constructor TControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindowProc := WndProc;
  .......
end;

procedure TControl.WndProc(var Message: TMessage);
var
  Form: TCustomForm;
  KeyState: TKeyboardState;
  WheelMsg: TCMMouseWheel;
  Panned: Boolean;
{$IF DEFINED(CLR)}
  LMsg: TMessage;
{$IFEND}
begin
  if (csDesigning in ComponentState) then
  begin
    Form := GetParentForm(Self, False);
    if (Form <> nil) and (Form.Designer <> nil) and
      Form.Designer.IsDesignMsg(Self, Message) then Exit
  end;
  if (Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST) then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and Form.WantChildKey(Self, Message) then Exit;
  end
  else if (Message.Msg >= WM_MOUSEFIRST) and (Message.Msg <= WM_MOUSELAST) then
  begin
    if not (csDoubleClicks in ControlStyle) then
      case Message.Msg of
        WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK:
          Dec(Message.Msg, WM_LBUTTONDBLCLK - WM_LBUTTONDOWN);
      end;
    case Message.Msg of
      WM_MOUSEMOVE: Application.HintMouseMessage(Self, Message);
      WM_MBUTTONDOWN:
      begin
        if (csPannable in ControlStyle) and
        (ControlState * [csDestroyingHandle, csPanning] = []) and
        not Mouse.IsDragging then
        begin
          Mouse.CreatePanningWindow;
          Panned := False;
          if Assigned(Mouse.PanningWindow) then
          begin
            if Self is TWinControl then
              Panned := Mouse.PanningWindow.StartPanning(TWinControl(Self).Handle, Self)
            else if Parent <> nil then
              Panned := Mouse.PanningWindow.StartPanning(Parent.Handle, Self)
            else
            begin
              Form := GetParentForm(Self, False);
              if Form <> nil then
                Panned := Mouse.PanningWindow.StartPanning(Form.Handle, Self);
            end;
          end;
          if Panned then
          begin
            Message.Result := 1;
            Application.HideHint;
          end
          else if Assigned(Mouse.PanningWindow) then
            Mouse.PanningWindow := nil;
        end;
      end;
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
        begin
          if FDragMode = dmAutomatic then
          begin
            BeginAutoDrag;
            Exit;
          end;
          Include(FControlState, csLButtonDown);
        end;
      WM_LBUTTONUP:
        Exclude(FControlState, csLButtonDown);
    else
      with Mouse do
        if WheelPresent and (RegWheelMessage <> 0) and
          (Integer(Message.Msg) = Integer(RegWheelMessage)) then
        begin
          GetKeyboardState(KeyState);
{$IF DEFINED(CLR)}
          WheelMsg := TCMMouseWheel.Create;
{$IFEND}
          with WheelMsg do
          begin
            Msg := Message.Msg;
            ShiftState := KeyboardStateToShiftState(KeyState);
            WheelDelta := Message.WParam;
            Pos := SmallPoint(Message.LParam and $FFFF, Message.LParam shr 16);
          end;
{$IF DEFINED(CLR)}
          LMsg := WheelMsg.OriginalMessage;
          MouseWheelHandler(LMsg);
{$ELSE}
          MouseWheelHandler(TMessage(WheelMsg));
{$IFEND}
          Exit;
        end;
    end;
  end
  else if Message.Msg = CM_VISIBLECHANGED then
    with Message do
      SendDockNotification(Msg, WParam, LParam);
  Dispatch(Message);  // 最后Dispatch消息。该函数在TObject中定义
end;

//TObject
TObject = class
  procedure Dispatch(var Message); virtual;
  procedure DefaultHandler(var Message); virtual;
end;
procedure TObject.Dispatch(var Message);  // 虚方法.
{$IF not defined(CPU386)}
type
  //THandlerProc = procedure(Self: Pointer; var Message) { of object };
  THandlerProc = procedure(var Message) of object;
var
  MsgID: Word;
  Addr: Pointer;
  M: THandlerProc;
begin
  MsgID := TDispatchMessage(Message).MsgID;
  if (MsgID <> 0) and (MsgID < $C000) then
  begin
    Addr := FindDynaMethod(PPointer(Self)^, MsgID);
    if Addr <> nil then
    begin
      //THandlerProc(Addr)(Self, Message)
      TMethod(M).Data := Self;
      TMethod(M).Code := Addr;
      M(Message);
    end
    else
      Self.DefaultHandler(Message);
  end
  else
    Self.DefaultHandler(Message);
end;
// 空方法。
procedure TObject.DefaultHandler(var Message);
begin
end;

// TWinControl
TWinControl = class(TControl)
constructor TWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ......
end;

// MainWndProc不是虚函数，无法重载
procedure TWinControl.MainWndProc(var Message: TMessage);
begin
  try
    try
      WindowProc(Message);
    finally
      FreeDeviceContexts;
      FreeMemoryContexts;
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TDragObject.MainWndProc(var Message: TMessage);
begin
  try
    WndProc(Message);
  except
    Application.HandleException(Self);
  end;
end;

//在消息处理过程处理完消息之后，TControl类的DefaultHandler方法获得消息处理权。DefaultHandle方法在对消息进行最后的处理后，消息处理流程离开VCL派发机制，返回到Windows的DefWindowProc函数或其他默认的缺省窗口过程。
```

## 消息与事件的关系

* 在delphi中，事件和消息不一定是一一对应的，完全可以在程序中声明和消息不相关的事件，而VCL中事件本身是为了更好的去响应windows消息去设计的。例如OnKeyDown事件对应WM_KEYDOWN消息

```pascal
unit Control
type
  TKeyEvent = procedure(Sender: TObject; var Key: Word;
    Shift: TShiftState) of object;
    
  TWinControl = class(TControl)
  private
    FOnKeyDown: TKeyEvent;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  protected
    function DoKeyDown(var Message: TWMKey): Boolean;
    procedure KeyDown(var key: Word; Shift: TShiftState); dynamic;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
  end;
  // WMkeyDown
  procedure TWinControl.WMKeyDown(var Message: TWMKeyDown);
  begin
    if not DoKeyDown(Message) then Inherited;
  end;
  // DOKeyDown 调用 KeyDown
  function TWinControl.DoKeyDown(var Message: TWMKey): Boolean;
  var
    ShiftState: TShiftState;
    Form: TCustomForm;  // TCustomForm 是从 TWinControl继承下来的，这里感觉不太好。
  begin
    Result := True;
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form <> self) and Form.KeyPreview and 
      TWinControl(Form).DoKeyDown(Message) then  Exit;
    with Message do
    begin
      ShiftState := KeyDataToShiftState(KeyData);
      if not (csNostdEvents in ControlStyle) then
      begin
        KeyDown(CharCode, ShiftState);
        if CharCode = 0 then 
          Exit;
      end;
    end;
    Result := False;
  end;
  // KeyDown
  procedure TWinControl.KeyDown(var Key: Word; Shift: TShiftState);
  begin
    if Assigned(FOnKeyDown) then FOnKeyDown(Self, key, Shift);
  end;
  
```

