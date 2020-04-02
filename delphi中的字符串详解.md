# delphi 字符串详解

## 短字符串和字符数组

```pascal
// 字符串数组的定义
var
  Str1, str2: array of [0..6] of AnsiChar; //在delphiXE中Char是双字节，这里使用AnsiChar是为了和书中保持一致。
// 为了解决字符串类型不丰富的问题，delphi中引入了字符串，其中有Shortstring，ANSIString， WideString。
// 为了与传统pascal字符串相兼容，ShortString使用紧缩格式。它最多只能容纳255个标准ASCII字符。
var
  str1, str2: string[7];
  
// 下面时字符数组和字符串的比较
var
  A: array[0..6] of AnsiChar;
  B: String[7];
begin
  A := 'Delphi7';
  B := 'Delphi7';
end;
```

|      | A[0] | A[1] | A[2] | A[3] | A[4] | A[5] | A[6] | A[7] |
| :--: | :--: | :--: | :--: | :--: | :--: | :--: | :--: | :--: |
|  A   |  D   |  e   |  l   |  p   |  h   |  i   |  7   |      |
|  B   |  7   |  D   |  e   |  l   |  p   |  h   |  i   |  7   |

```pascal
  ShowMessage('a Sizeof' + IntToStr(SizeOf(a))); // 7
  ShowMessage('b Sizeof' + IntToStr(SizeOf(b))); // 8
  ShowMessage('a Length' + IntToStr(Length(a))); // 7
  ShowMessage('b Length' + IntToStr(Length(b))); // 7
  ShowMessage('a Low' + IntToStr(Low(a))); // 0
  ShowMessage('b Low' + IntToStr(Low(b))); // 0
  ShowMessage('a High' + IntToStr(High(a))); // 6
  ShowMessage('b High' + IntToStr(High(b))); // 7
```

## 字符串

* 这里讨论长字符串（AnisString）和宽字符串（WideString）， 在delphi中默认情况下string类型就是AnsiString类型。

* ANSIstring采用非紧缩格式。ANSIstring字符串可以在使用是动态分配内存。

* 当我们声明了长字符串变量，实际上分配了32位（4字节）的内存空间存储一个指针，该指针指向存储实际字符串的内存地址。

* 当字符串变量为空时，此指针值为nil，字符串内容本身不需占用额外的存储空间。当字符串非空时，此指针为动态分配的内存块的首地址，该内存块存储了相应的字符串内容（包括字符串值，一个32位（4字节）的长度指示符和一个32位的引用计数器）

* 存储字符串是在堆中分配的。

* 因为长字符串变量为隐式指针，因为多个长字符串变量可以指向相同的内容而不需要存储字符串的多个副本。只要一个长字符串变量被释放或赋予了新值，则原字符串的引用计数自动减1，而新字符串（如果存在）的引用计数自动递增1。如果某个长字符串的引用计数递减到0，则其占用的内存被释放。当改变长字符串时，而且仅当引用计数器大于1时才生成该字符串的一个副本，此即为生存期管理 的写时复制（copy-on-write）语义。

* 从本质上说，AnsiString类型与一维动态字符数组类似，他们的主要差别在于

  1. 虽然使用相同的引用计数技术，但AnsiString类型与一维动态字符数组的索引方式不同。

  ```pascal
  var
    A: string;
    B: AnsiChar;
    N: integer;
  begin
    SetLength(A, 6);
    B := A[0]; // 错误，不能访问A[0]
    N := Length(A); // N = 6
    B := A[1]; // B := 'D';
    N := SizeOf(A); // N = 4, 返回A的内存大小，而不是字符串本身占用的内存大小
  end;
  ```

  2. A的内容（p值）实际为A[1]的地址。32位引用计数、字符串长度以及最后的A[7]都是由Delphi自动维护的。程序不能进行操作。AnsiString类型变量的索引下标从1到长度值。字符串在尾部添加一个Null字符（这个字符其实是#0，不是真正意义上的Null，真正意义上了Null表示空，C语言的字符串是以'\0'为结尾的，'\0'就是#0）是为了与C语言null结尾的字符串相兼容。所以可以将长字符串赋值给某个null结尾的字符串变量。

  ![](长字符串的内存存储格式.png)

