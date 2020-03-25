```pascal
AXmlNode := FXmlNode.Root;
function GetProvinceXmlNode(XmlNode: IXmlNode): TXmlNode;
var
  AChildXmlNode: TXmlNode;
  i, iCount: Integer;
begin
  iCount := AXmlNode.NodeCount;
  for i = 0 to iCount - 1 do
  begin
    AChildXmlNode := AXmlNode.Nodes[i];
    if AChildXmlNode.AttributeByName['provinced'] = InttoStr(FProvinceId) then
    begin
      Result := AChildXmlNode;
      break;
    end;
  end;
end;
```

# TXMLDocument

```pascal
var
  FXMLDoc: TXMLDocument;
  AXMLNode: IXMLNode;
  i: Integer;
begin
  FXMLDoc.LoadFromFile(AFileName);
  AXMLNode := FXMLDoc.DocumentElement.ChildNodes[I];
end;
```

* XML 节点有自己的类型，其中ntComment是注释类型。

```pascal
if AXMLNode.NodeType = ntComment then  // 如果节点类型是注释，那么跳过
begin
  continue;
end;
```

