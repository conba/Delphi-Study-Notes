```pascal
  HWND = type LongWord;
  type
    WPARAM = INT_PTR;
    LPARAM = INT_PTR;
    INT_PTR = Integer;
    UINT = LongWord;
  DWORD = Types.DWORD;
  DWORD = LongWord;
```



# absolute

```pascal
// 今天看书遇到了这个关键自，还是挺好玩的，使用这个关键字来定义的变量会和其他变量共享内存
procedure TForm13.btn2Click(Sender: TObject);
var
  num: Integer;
  shnum: Byte absolute num;
begin
  num := 512;  // 这个时候shnum 的值为0， 因为shNum和num共享内存，他和shNum共享了低1个字节的内存，所以shnum的值为0。
  shnum := 1;  // 将num的低1个字节设置为了1，所以值为513；
  ShowMessage(IntToStr(num));  // num 的值为 513；
end;
```

