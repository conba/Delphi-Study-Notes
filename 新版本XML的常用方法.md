# 加载DLL并使用DLL中的方法

```pascal
type
  // 定义一个函数指针。
  TEpEncrypt = function(AGSBZ: PAnsiChar; ATotalValue: PAnsiChar): PAnsiChar; stdcall; 
// 调用
var
  LibHandle: THandle;
  Epencrypt: TEpencrypt;
begin
  // cst_Dzpb_AH_WuHu_2018_EpencryptDLL是对方提供的一个DLL，这个DLL放在主目录下。
  LibHandle := LoadLibrary(cst_Dpzb_AH_WuHu_2018_EpEncryptDLL);
  try
    if LibHandle = 0 then
      raise Exception.Create(Format(cst_Dzpb_AH_WuHu_2018_LoadError, [QuotedStr(cst_Dzpb_AH_WuHu_2018_EpEncryptDLL)]));
      @EpEncrypt := GetProcAddress(LibHandle, cst_Dzpb_AH_WuHu_2018_EpEncryptStr);
      if not (@Epencrypt = nil) then
      	sValue := EpEncrypt(PAnsiChar(cst_Dzpb_AH_WuHu_2018_EncryptStr), PAnsiChar('0.00'));
  finally
    FreeLibary(LibHandle);
  end;
end;
```

# 服务

## 费率设置服务

* 如果现在在Xmjg表中，要获取某个专业节点下的费率。

  ```pascal
  /// 获取专业节点中某个特项中的某个费率。
  /// 下面是获取专业节点下特项是99的管理费（Glf）费率和利润（Lr）费率。
  var
    ADataContext: IDataContext;
    AFlszService: ISSFlszService;
    AFl: Double;
    ISFind: Boolean;
  begin 
    ADataContext := FDzpbContext.FileDataContext.PrjDataManager.GetDataContext(
      ZygcNode.GetValueAsString(cst_Xmjg_Guid, ''));
    if FDzpbContext.FileDataContext.FileService.ExistsService(cst_FileService_Flsz) then
      AFlszService := FDzpbContext.FileDataContext.FileService.GetService(cst_FileService_Flsz) 
        as ISSFlszService;      
    if Assigned(ADataContext) and Assigned(AFlszService) then
    	AGlffl := AFlszService.GetFLValue(ADataContext, cst_Fltx_DefaultFltxId, cst_fylx_glf, 
    	  ISFind);  // 杭州地区只有一个特项，99
    if ISFind then
    	ZygcRec.SetValue(cst_Xml_ZyNodeXx_Glffl, AGlffl);
  end;
  
  /// 尝试获取费率和系数
  var
    dFL_WithoutXs, dXs: Double;
  begin
    // 当前工程节点，特项ID，费用类型，费率，系数
    FFlszService.TryGetFlAndXs(FDataContext, cst_Fltx_DefaultFltxId, cst_SsTableName_aqwm,
      dFL_WithoutXs, dXs);
  end;
  
  /// 通过费率设置服务获取flsz表和fltx表
  var
    FFlszDs, FTxgcDs: IPMDataSet
  begin
    FFlszDs := FFlszService.GetFlxmDataSet(FDataContext);
    FTxgcDs := FFlszService.GetTxgcDataSet(FDataContext);
  end;
  ```

* 如果在某个专业节点下，那么不用去找DataContext，其中的FDataContext就是该节点的DataContext。

## 获取人材机资源表（Glhz）

```pascal
var
  FGlhzDs: IPMDataSet;
begin
  FGlhzDs := FDzpbContext.FileDataContext.ResourceDataContext.GetDataSet(cst_Table_Glhz);
end;
```

## 获取XmjgDataSet

```pascal
/// 获得模版ID
var
  XmjgDs: IPMDataSet;
begin
  XmjgDs := FDzpbContext.FileDataContext.ProjectStructureDataSet;
  if not Assigned(XmjgDs) then Exit;
  Rec := XmjgDs.FindRec(cst_Xmjg_Guid, FDataContext.DataToken);
  if Assigned(rec) then
  	Result := XmjgDs.GetFieldValueAsInt(cst_Xmjg_Mbid, 0, Rec);
end;
```

## 获取Xmsx中的内容

```pascal
/// cst_XMSX_Key_JjmbGuid = 'JjmbGuid';
FDzpbContext.FileDataContext.Fileoption.Xmsx.GetValue(cst_XMSX_Key_JjmbGuid);
// 判断文件是招标文件还是投标文件
function GetProjectStatus(APrjDataManager: IPrjDataManager): TProjectStatus;
function GetProjectStatus(AFileOption: IYsFileOption): TProjectStatus;
function GetProjectStatus(AXmsx: IYsFileOption_Xmsx): TProjectStatus;
```

# 配置文件

## 项目信息

```pascal
var
  P: _PVarRec;
begin
  FDzpbContext.DzpbConfig.XmxxVars.Count;
  FDzpbContext.DzpbConfig.XmxxVars.Vars[i];
  // 这个是遍历获得xmxx
  P := FDzpbContext.DzpbConfig.BaVars_Jsxm.GetVarRecSsVar(SsNode.GetValueAsString(cst_qtxm_bl, ''));
  // 这个是根据胜算变量来获取BaJsxm中的某条记录
end;

// 组织措施中使用Fylx查找配置文件中的组织措施该Fylx的记录，并为Xml中第三方变量赋值
var
  sFylx: string;
  _MeasureRec: _PzyMeasure;
  dFl_WithoutXs, dXs: Double;
begin
  sFylx := SsNode.GetValueAsString(cst_Fbfx_Fylx, '');
  _MeasureRec := FDzpbContext.DzpbConfig.ZyMeasure.GetZyMeasureRec(sFylx);
  if Assigned(_MeasureRec) then
  begin
    XmlRec.SetValue(cst_Xml_Csxm1Mx_Xmlb, _MeasureRec.MeasType);
  end;
end;
```

## 错误日志

```pascal
FDzpbContext.DzpbLog.AddErrorLog();
```

# 胜算工程

## 分部分项

```pascal
/// 招标导出分部分项
/// 采用的是遍历导，找到分布分项表之后，然后获得分部分项表中的根节点，然后通过根节点获得它所有的子节点，然后判断是分布还是清单分别导出。
RootNode := GetFbfxRootNode(FSsDataView);
var
  AItemType: TItemType;
  SsNode: IPMNode;
begin
  for i := 0 to RootNode.Count - 1 do
  begin
    SsNode := RootNode.ChildNodes[i];
    AItemType := GetItemType(SsNode);
    case AItemType of
      ntxmFb: begin end;
      ntxmQd: begin end;
  end;
end;
```

## 措施项目

```pascal
/// 获取措施项目表中的根节点
RootNode := GetZzcsRootNode(FSsDataView);
```

# 杂项

```pascal
FDzpbContext.LockNumber  // 加密锁号
GetDiskDriveID_WMI       // 获取电脑信息， 该方法在 uWMIInterface 这个单元下面
TrimRight();             // 去掉右边空格
GetMacAdress;            // 获取Mac地址
FDataContext.DataToken   // 比较常用的获得DataToken的方法。
SimpleRoundToEx(StrtoDouble(sValue), cst_Dzpb_AH_WuHu_2018_Decimal_Two) // 保留两位小数
FDzpbContext.SysOption.Path.SysTempPath  // 临时路径
ExtractFileName(FileName);               // 取文件名
ExtractFilePath(FileName);               // 取文件路径
ExtractFileExt(FileName);                // 取文件后缀名
```
