# delphi过程类型详解

|      | 函数             | 函数指针                             |
| ---- | ---------------- | ------------------------------------ |
| 声明 | 一段代码的首地址 | 一个存储指向过程或函数地址的指针变量 |
| 使用 | 对首地址的引用   | 对该过程或地址的引用                 |

## 参考C++

1. 在delphi中，除了类引用外，没有其他的显示定义引用的方法。这点与C++不同，在C++中引用就是指针，但是在操作时不需要使用指针运算符。之所以可以这样使用，是因为引用相当于某个变量或值的别名，它没有引入任何新的存储对象，对引用的操作由编译器直接映射到该变量或值。

```c++
int n = 100;
int& nRef = n;
nRef = 630;
```

2. 在上面的代码中，nRef本身并不占用任何存储空间，编译器会使标识符nRef在程序运行时映射（联编）到n上，因而， 所有对nRef的操作都是对n的操作，所以，在运行玩第三行代码后，n的值时630.

## delphi 中的引用

1. 在delphi中，可以认为过程类型既是指针，又是引用，或者跟严格的说，它事实上是对指向过程或函数地址的指针变量的引用。

2. 声明一个过程变量G，实际上就是声明了一个名为‘@G’的指针变量，该指针变量占用32位的存储空间，它指向过程或函数地址。而G标识对该指针变量的引用，因而G相当于该指针变量的别名，它本身并不占用任何存储空间，因此，下述代码

   ```pascal
   G := Func;
   ```

   将使得指针变量"@G"指向Func地址"@Func"。

3. 当使用"@G"的时候，总是获得它所对应的过程或函数的地址"@Func"（它保存在指针变量"@G"中）。使用"@(@G)"则可获得指针变量"@G"自身的地址。因为"@"操作符的右结合特性，"@(@G)"自然等价于"@@G"。

4. 另外，"@"操作符还可以用于将无型指针变量指派给过程变量，例如下述代码

   ```pascal
   procedure InitDriveSpacePtr;
   var
     Kernel: THandle;
   begin
     Kernel := GetModuleHandle(Windows.Kernel32);
     if Kernel <> 0 then
       @GetDiskFreeSpaceEx := GetProAddress(Kernel, 'GetDiskFreeSpaceExA');
     if not Assigned(GetDiskFreeSpaceEx) then
       GetDiskFreeSpaceEx := @BackfillGetDiskFreeSpaceEx; // 这个"@"可有可无，有可以增强代码阅读性
   end;
   ```

5. 任何一个过程变量都可以取nil值，为nil值的过程变量在调用的时候是会报错的，所以在使用的时候可以用Assigned函数进行测试。

   ```pascal
   if Assigned(F) then Return := F(3, 4);
   ```

## 过程类型的使用

1. 声明了过程类型和过程变量后，就可以在赋值语句和表达式中使用该过程变量。

2. 过程类型实际上时过程指针，它指向过程或函数的首地址

3. 声明一个过程变量Func实际上就是声明一个过程类型的指针变量Func。

   ```pascal
   Func := Calc;// Func指向内存中函数Calc的首地址
   ```

4. 当过程变量出现在赋值语句的左边时，赋值语句右边要给出相应的过程类型的值。此时，左边的过程变量获得右边过程或函数的首地址，该过程变量作为一个指针指向右边的过程或函数。

5. 过程变量获得具体值之后，就可以在表达式中使用或作为实际参数传递给其他子程序。

   ```pascal
   var
     F: function(X: integer): integer;
     Return: Integer;
     
   function Func(X: integer): integer;
   begin
     Result := X * X;
   end;
   
   procedure Button1Click(Sender: TObject);
   begin
     F := Func;
     Return := F(4);  // 过程变量的使用
   end;
   
   // 过程变量之间也可以赋值
   var	
     F, G: function: Integer;
     i: integer;
   function Func: integer;
   begin
     //Do Something   
   end;
   
   procedure Button1Click(Sender: TObject);
   label 1, 2, 3;
   begin
     Caption := IntToStr(Integer(@Func));
     1: F := Func; // 将函数Func指派给过程变量F
     Caption := Caption + ',' + IntToStr(Integer(@F));
     2: G := F; // 将过程变量F对应的函数指派给过程变量G
     Caption := Caption + ', ' + IntToStr(Integer(@G));
     3: I := G; // 调用函数Func
   end;
   
   // 需要注意的是，过程变量的赋值不能使用括号，因此下面的语句是错误的；
   F := Func();
   G := F();
   
   // 下面的语句也是错误的。
   var
     F, G: function(X: integer): integer;
     i: integer;
     
   function Func(X: integer): integer;
   begin
     // Do something
   end;
   
   procedure TForm1.Button1Click(Sender: TObject);
   begin
     F := Func();  // 错误，没有声明无参类型函数Func，而且函数返回值与F类型不匹配
     F := Func(I); // 错误，函数返回值与F类型不匹配。
     F := Func; // 正确
     G := F(I);  // 错误，函数F返回值（integer）与G类型（procedure）不匹配。
   end;
   
   // 从上面的语句中可以看出，书写规范会使得程序的代码更加清晰。当调用无参类型过程或者函数的时候，都是用一个空括号是一个好习惯，这使得我们一眼就能看出是过程或者函数调用，而不是过程变量的复制。
   
   // 当表达式中出现过程变量时，过程变量就表示对它指向函数的调用。（比较特殊的是判等表达式"="）
   var
     F: function(x, y: Integer): integer;
     G: function: integer;
     Return: integer;
   
   function Func1(x, y: integer;): integer;
   begin
     Func1 := X * X + Y * Y;
   end;
   
   function Func2: Integer;
   begin
     Func2 := 10 * 10;
   end;
   
   procedure TForm1.Button1Click(Sender: TObject);
   begin
     F := Func1;
     G := Func2;
     Return := F(3, 4) + G;
   end;
   
   // 下面的语句可以看出
   G = Func2; // True
   G() = Func2(); //True
   
   // 如果要将过程变量的值（地址）和函数 过程值（函数或过程的地址）进行比较，应该使用"@"操作符，例如
   if @G = @Func2 then // True
   
   // @G 将G转换为一个包含地址值的无型指针变量，而"@Func2"返回Func函数的首地址。
   // 如果要获得一个过程变量本身所占用的实际内存地址，而不是它所存储的某个过程或函数的地址，要用"@@"来获得该变量的地址。
   ```

   