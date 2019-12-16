unit lcc_protocol_simplenodeinfo;

interface

{$IFDEF DWSCRIPT}
{$ELSE}
   {$I lcc_compilers}
{$ENDIF}

uses
{$IFDEF DWSCRIPT}
  System.Types,
  System.Types.Convert,
  System.Time,
  System.Streams,
  System.Reader,
  System.Writer,
  System.Lists,
  System.Device.Storage,
  SmartCL.Device.Storage,
  SmartCL.Application,
  SmartCL.Components,
  SmartCL.System,
  System.Memory.Buffer,
  System.Memory,
{$ELSE}
  Classes,
  SysUtils,
  lcc_xmlutilities,
{$ENDIF}
  lcc_protocol_base,
  lcc_defines,
  lcc_node_messages,
  lcc_utilities;

type

  { TProtocolSimpleNodeInfo }

  TProtocolSimpleNodeInfo = class(TNodeProtocolBase)
  private
    FHardwareVersion: String;
    FManufacturer: String;
    FModel: String;
    FSoftwareVersion: String;
    FPackedInfo: TDynamicByteArray;
    FUserDescription: String;
    FUserName: String;
    FUserVersion: Word;
    FVersion: Word;

    function GetPackedFormat: TDynamicByteArray;
    function GetUserDescription: String;
    function GetUserName: String;

    function FindElement(TestXML, Element: string; var Offset: Integer; var ALength: Integer): Boolean;
  public
    property Version: Word read FVersion write FVersion;
    property Manufacturer: String read FManufacturer write FManufacturer;
    property Model: String read FModel write FModel;
    property HardwareVersion: String read FHardwareVersion write FHardwareVersion;
    property SoftwareVersion: String read FSoftwareVersion write FSoftwareVersion;
    property UserVersion: Word read FUserVersion write FUserVersion;
    property UserName: String read GetUserName write FUserName;
    property UserDescription: String read GetUserDescription write FUserDescription;

    property PackedFormat: TDynamicByteArray read GetPackedFormat;

    {$IFNDEF DWSCRIPT}  // No access to filesin JavaScript
    function LoadFromXmlPath(CdiFilePath: String): Boolean;
    function LoadFromXmlDoc(CdiXMLDoc: LccXmlDocument): Boolean;
    procedure LoadFromLccMessage(SourceLccMessage: TLccMessage); override;
    {$ENDIF}
    function LoadFromXmlString(XMlString: string): Boolean;
  end;

implementation

{ TProtocolSimpleNodeInfo }

function TProtocolSimpleNodeInfo.GetPackedFormat: TDynamicByteArray;
const
  NULL_COUNT = 6;
  VERSION_COUNT = 2;
var
  iArray, i: Integer;
  TempArray: TDynamicByteArray;
begin

  Result := nil;

  i :=  Length(Manufacturer) + Length(Model) + Length(HardwareVersion) + Length(SoftwareVersion) + Length(UserName) + Length(UserDescription);
  i := i + NULL_COUNT + VERSION_COUNT;

  {$IFDEF DWSCRIPT}
  var BinaryData: TBinaryData;
  BinaryData := TBinaryData.Create(TMarshal.AllocMem(i).Segment);
  FPackedInfo := BinaryData.ToBytes;
  {$ELSE}
    SetLength(FPackedInfo, i);
 {$ENDIF}

  iArray := 0;
  TempArray := FPackedInfo;

  FPackedInfo[iArray] := Version;      // 4 Items follow
  Inc(iArray);
  StringToNullArray(Manufacturer, TempArray, iArray);
  StringToNullArray(Model, TempArray, iArray);
  StringToNullArray(HardwareVersion, TempArray, iArray);
  StringToNullArray(SoftwareVersion, TempArray, iArray);

  TempArray[iArray] := UserVersion;  // 2 items follow
  Inc(iArray);
  StringToNullArray(UserName, TempArray, iArray);
  StringToNullArray(UserDescription, TempArray, iArray);

  FPackedInfo := TempArray;

  Result := FPackedInfo;
end;

function TProtocolSimpleNodeInfo.GetUserDescription: String;
begin
  Result := FUserDescription;
 //JDK if Owner is TLccOwnedNode then
 //JDK   Result := (Owner as TLccOwnedNode).Configuration.ReadAsString(64);
end;

function TProtocolSimpleNodeInfo.GetUserName: String;
begin
  Result := FUserName;
//JDK  if Owner is TLccOwnedNode then
 //JDK   Result := (Owner as TLccOwnedNode).Configuration.ReadAsString(1);
end;

function TProtocolSimpleNodeInfo.FindElement(TestXML, Element: string; var Offset: Integer; var ALength: Integer): Boolean;
var
  OffsetEnd: Integer;
begin
  Result := False;
  TestXML := LowerCase(TestXML);
  Element := LowerCase(Element);
  Offset := Pos(Element, TestXML);
  if Offset > -1 then
  begin
    Inc(Offset, Length(Element));
    Element := StringReplace(Element, '<', '</', [rfReplaceAll]);
    OffsetEnd := Pos(Element, TestXML);
    if (OffsetEnd > -1) and (OffsetEnd > Offset) then
    begin
      ALength := OffsetEnd - Offset;
      Result := True;
    end else
    Exit;
  end else
  Exit;
end;

{$IFNDEF DWSCRIPT}
function TProtocolSimpleNodeInfo.LoadFromXmlPath(CdiFilePath: String): Boolean;
var
  XMLDoc: LccXmlDocument;
begin
  Result := False;
  Valid := False;
  if FileExists(CdiFilePath) then
  begin
    try
      XMLDoc := XmlLoadFromFile(CdiFilePath);
      Result := LoadFromXmlDoc(XMLDoc);
      XMLDoc.Free;
      Valid := True;
    except
      // Quiet fail
    end;
  end;
end;


function TProtocolSimpleNodeInfo.LoadFromXmlDoc(CdiXMLDoc: LccXmlDocument): Boolean;
var
  CdiNode, IdentificationNode, ChildNode: LccXmlNode;
begin
  if Assigned(CdiXMLDoc) then
  begin
    CdiNode := XmlFindRootNode(CdiXMLDoc, 'cdi');
    if Assigned(CdiNode) then
    begin
      IdentificationNode := XmlFindChildNode(CdiNode, 'identification');
      if Assigned(IdentificationNode) then
      begin
         Version := 1;
         ChildNode := XmlFindChildNode(IdentificationNode, 'manufacturer');
         if Assigned(ChildNode) then FManufacturer := XmlFirstChildValue(ChildNode) else Exit;
         ChildNode := XmlFindChildNode(IdentificationNode, 'model');
         if Assigned(ChildNode) then FModel := XmlFirstChildValue(ChildNode) else Exit;
         ChildNode := XmlFindChildNode(IdentificationNode, 'hardwareVersion');
         if Assigned(ChildNode) then FHardwareVersion := XmlFirstChildValue(ChildNode) else Exit;
         ChildNode := XmlFindChildNode(IdentificationNode, 'softwareVersion');
         if Assigned(ChildNode) then FSoftwareVersion := XmlFirstChildValue(ChildNode) else Exit;
         UserVersion := 1;
         Result := True;
      end;
    end;
  end;
end;

procedure TProtocolSimpleNodeInfo.LoadFromLccMessage(SourceLccMessage: TLccMessage);
  {$IFDEF LCC_MOBILE}
  function NextString(AStrPtr: PChar): PChar;
  {$ELSE}
  function NextString(AStrPtr: PAnsiChar): PAnsiChar;
  {$ENDIF}

  begin
    Result := AStrPtr;
    while Result^ <> #0 do
      Inc(Result);
    Inc(Result);
  end;

{$IFDEF LCC_MOBILE}
var
  StrPtr: PChar;
{$ELSE}
var
  StrPtr: PAnsiChar;
{$ENDIF}
begin

  Result := False;
  Result := True;
  StrPtr := @SourceLccMessage.DataArray[0];
  FVersion := Ord(StrPtr^);
  Inc(StrPtr);
  FManufacturer := StrPtr;
  StrPtr := NextString(StrPtr);
  FModel := StrPtr;
  StrPtr := NextString(StrPtr);
  FHardwareVersion := StrPtr;
  StrPtr := NextString(StrPtr);
  FSoftwareVersion := StrPtr;
  StrPtr := NextString(StrPtr);
  FUserVersion := Ord(StrPtr^);
  Inc(StrPtr);
  FUserName := StrPtr;
  StrPtr := NextString(StrPtr);
  FUserDescription := StrPtr;
end;

{$ENDIF}

function TProtocolSimpleNodeInfo.LoadFromXmlString(XMlString: string): Boolean;

var
  TempMfg, TempModel, TempHWVersion, TempSWVersion: string;
  AnOffset, ALength, i: Integer;
begin
  Result := False;
  if FindElement(XMlString, '<manufacturer>', AnOffset, ALength) then
  begin
    if ALength < LEN_MFG_NAME then
    begin
      SetLength(TempMfg, ALength);
      for i := AnOffset to AnOffset + ALength - 1 do
        TempMfg[i-AnOffset{$IFNDEF LCC_MOBILE}+1{$ENDIF}] := XMLString[i];
    end else Exit;
  end else Exit;
  if FindElement(XMlString, '<model>', AnOffset, ALength) then
  begin
    if ALength < LEN_MODEL_NAME then
    begin
      SetLength(TempModel, ALength);
      for i := AnOffset to AnOffset + ALength - 1 do
        TempModel[i-AnOffset{$IFNDEF LCC_MOBILE}+1{$ENDIF}] := XMLString[i];
    end else Exit;
  end else Exit;
  if FindElement(XMlString, '<hardwareVersion>', AnOffset, ALength) then
  begin
    if ALength < LEN_HARDWARE_VERSION then
    begin
      SetLength(TempHWVersion, ALength);
      for i := AnOffset to AnOffset + ALength - 1 do
        TempHWVersion[i-AnOffset{$IFNDEF LCC_MOBILE}+1{$ENDIF}] := XMLString[i];
    end else Exit;
  end else Exit;
  if FindElement(XMlString, '<softwareVersion>', AnOffset, ALength) then
  begin
    if ALength < LEN_SOFTWARE_VERSION then
    begin
      SetLength(TempSWVersion, ALength);
      for i := AnOffset to AnOffset + ALength - 1 do
        TempSWVersion[i-AnOffset{$IFNDEF LCC_MOBILE}+1{$ENDIF}] := XMLString[i];
    end else Exit;
  end else Exit;
  Manufacturer := TempMfg;
  Model := TempModel;
  HardwareVersion := TempHWVersion;
  SoftwareVersion := TempSWVersion;
end;

end.

