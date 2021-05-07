# 判断文件是否被使用

```pascal
// 检查文件是否被使用
function CheckFileInUse(FileName: string): Boolean;
var
  iSum: Integer;
begin
  Result := True;
  iSum := 1;
  while iSum <= 10 do
  begin
    Result := IsFileInUse(FileName);
    if Not Result then
      Break;
    Sleep(1000);
    Inc(iSum);
  end;
end;

// 判断文件是否被使用
function IsFileInUse(fName: string): boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(fName) then
    exit;
  HFileRes := CreateFile(pchar(fName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;
```

