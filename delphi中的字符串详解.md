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