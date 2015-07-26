unit form_properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, lcc_nodeselector, lcc_nodemanager, lcc_defines, laz2_DOM, laz2_XMLRead,
  laz2_XMLWrite, SynEdit, SynHighlighterHTML;

type

  { TFormNodeProperties }

  TFormNodeProperties = class(TForm)
    CheckGroupProtocols: TCheckGroup;
    CheckGroupConfigMem: TCheckGroup;
    GroupBoxMemSpaces: TGroupBox;
    ImageListProperties: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    LabelLowSpace: TLabel;
    LabelHighSpace: TLabel;
    Label8: TLabel;
    LabelUserVersion: TLabel;
    Label7: TLabel;
    LabelVersion: TLabel;
    Label9: TLabel;
    LabelHardwareVer: TLabel;
    LabelUserName: TLabel;
    LabelUserDesc: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelMfg: TLabel;
    LabelModel: TLabel;
    LabelSoftwareVer: TLabel;
    LccNodeSelectorMemSpaces: TLccNodeSelector;
    PageControl1: TPageControl;
    SynEdit: TSynEdit;
    SynHTMLSyn: TSynHTMLSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
  private
    FActiveNode: TLccNode;
    procedure SetActiveNode(AValue: TLccNode);
    { private declarations }
  public
    { public declarations }
    property ActiveNode: TLccNode read FActiveNode write SetActiveNode;
    procedure ClearForm;
    function LoadConfigMemAddressSpaceInfo(ConfigMemAddressSpaceInfo: TConfigMemAddressSpaceInfoObject): Boolean;
    function LoadConfigMemOptions(ConfigurationMemOptions: TConfigurationMemOptions): Boolean;
    function LoadProtocols(Protocols: TProtocolSupport): Boolean;
    function LoadSnip(SimpleNodeInfo: TSimpleNodeInfo): Boolean;
    function LoadCdi(Cdi: TCDI): Boolean;
  end;

var
  FormNodeProperties: TFormNodeProperties;

implementation

{$R *.lfm}

{ TFormNodeProperties }

procedure TFormNodeProperties.SetActiveNode(AValue: TLccNode);
begin
  if FActiveNode =AValue then Exit;
  FActiveNode :=AValue;
  ClearForm;
end;

procedure TFormNodeProperties.ClearForm;
var
  i: Integer;
begin
  LabelVersion.Caption := '';
  LabelHardwareVer.Caption := '';
  LabelSoftwareVer.Caption := '';
  LabelMfg.Caption := '';
  LabelModel.Caption := '';
  LabelUserVersion.Caption := '';
  LabelUserDesc.Caption := '';
  LabelUserName.Caption := '';
  LabelHighSpace.Caption := '';
  LabelLowSpace.Caption := '';
  SynEdit.ClearAll;
  LccNodeSelectorMemSpaces.LccNodes.Clear;
  for i := 0 to CheckGroupConfigMem.Items.Count - 1 do
    CheckGroupConfigMem.Checked[i] := False;
  for i := 0 to CheckGroupProtocols.Items.Count - 1 do
    CheckGroupProtocols.Checked[i] := False;
end;

function TFormNodeProperties.LoadConfigMemAddressSpaceInfo(ConfigMemAddressSpaceInfo: TConfigMemAddressSpaceInfoObject): Boolean;

  function AddressSpaceToCaption(AddressSpace: Byte): string;
  begin
    Result := 'Address Space: 0x' + IntToHex(AddressSpace, 4) + ' (' + IntToStr(AddressSpace) + ')';
  end;

  function FindExisitingSpace: TLccGuiNode;
  var
    i: Integer;
    SpaceStr: string;
  begin
    Result := nil;
    SpaceStr := AddressSpaceToCaption(ConfigMemAddressSpaceInfo.AddressSpace);
    for i := 0 to LccNodeSelectorMemSpaces.LccNodes.Count - 1 do
    begin
      if LccNodeSelectorMemSpaces.LccNodes[i].Captions[0] = SpaceStr then
      begin
        Result := LccNodeSelectorMemSpaces.LccNodes[i];
        Break;
      end;
    end;
  end;

var
  NodeGui: TLccGuiNode;
begin
  Result := False;
  if Assigned(ConfigMemAddressSpaceInfo) then
  begin
    NodeGui := FindExisitingSpace;
    if not Assigned(NodeGui)  then
    begin
      LccNodeSelectorMemSpaces.BeginUpdate;
      try
        NodeGui := LccNodeSelectorMemSpaces.LccNodes.Add(NULL_NODE_ID, 0);
        NodeGui.Captions.Add(AddressSpaceToCaption(ConfigMemAddressSpaceInfo.AddressSpace));
        if ConfigMemAddressSpaceInfo.IsPresent then
          NodeGui.Captions.Add('Address space is present')
        else
          NodeGui.Captions.Add('Address space is not present');
        if ConfigMemAddressSpaceInfo.IsReadOnly then
          NodeGui.Captions.Add('Address space is read only')
        else
          NodeGui.Captions.Add('Address space is writable');
        if ConfigMemAddressSpaceInfo.ImpliedZeroLowAddress then
          NodeGui.Captions.Add('Low addess is implied to be 0')
        else
          NodeGui.Captions.Add('Low address is specified: 0x' + IntToHex(ConfigMemAddressSpaceInfo.LowAddress, 8) + ' (' + IntToStr(ConfigMemAddressSpaceInfo.LowAddress) + ')');
        NodeGui.Captions.Add('High address is 0x' + IntToHex(ConfigMemAddressSpaceInfo.HighAddress, 8) + ' (' + IntToStr(ConfigMemAddressSpaceInfo.HighAddress) + ')');
        NodeGui.ImageIndex := 0;
        NodeGui.Enabled := True;
      finally
        LccNodeSelectorMemSpaces.EndUpdate;
      end;
    end;
    Result := True;
  end;
end;

function TFormNodeProperties.LoadConfigMemOptions(ConfigurationMemOptions: TConfigurationMemOptions): Boolean;
begin
  Result := False;
  if ConfigurationMemOptions.Valid then
  begin
    CheckGroupConfigMem.Checked[0] := ConfigurationMemOptions.WriteLenOneByte;
    CheckGroupConfigMem.Checked[1] := ConfigurationMemOptions.WriteLenTwoBytes;
    CheckGroupConfigMem.Checked[2] := ConfigurationMemOptions.WriteLenFourBytes;
    CheckGroupConfigMem.Checked[3] := ConfigurationMemOptions.WriteLenSixyFourBytes;
    CheckGroupConfigMem.Checked[4] := ConfigurationMemOptions.WriteArbitraryBytes;
    CheckGroupConfigMem.Checked[5] := ConfigurationMemOptions.UnAlignedWrites;
    CheckGroupConfigMem.Checked[6] := ConfigurationMemOptions.WriteUnderMask;
    CheckGroupConfigMem.Checked[7] := ConfigurationMemOptions.WriteStream;
    CheckGroupConfigMem.Checked[8] := ConfigurationMemOptions.UnAlignedReads;
    CheckGroupConfigMem.Checked[9] := ConfigurationMemOptions.SupportACDIMfgRead;
    CheckGroupConfigMem.Checked[10] := ConfigurationMemOptions.SupportACDIUserRead;
    CheckGroupConfigMem.Checked[11] := ConfigurationMemOptions.SupportACDIUserWrite;
    LabelLowSpace.Caption := '0x'+IntToHex(ConfigurationMemOptions.LowSpace, 4) + ' (' + IntToStr(ConfigurationMemOptions.LowSpace) + ')';
    LabelHighSpace.Caption := '0x'+IntToHex(ConfigurationMemOptions.HighSpace, 4) + ' (' + IntToStr(ConfigurationMemOptions.HighSpace) + ')';
    Result := True;
  end;
end;

function TFormNodeProperties.LoadProtocols(Protocols: TProtocolSupport): Boolean;
begin
  Result := False;
  if Protocols.Valid then
  begin
    CheckGroupProtocols.Checked[0] := True;
    CheckGroupProtocols.Checked[1] := Protocols.SimpleNodeInfo;
    CheckGroupProtocols.Checked[2] := Protocols.MemConfig;
    CheckGroupProtocols.Checked[3] := Protocols.Teach_Learn;
    CheckGroupProtocols.Checked[4] := Protocols.EventExchange;
    CheckGroupProtocols.Checked[5] := Protocols.CDI;
    CheckGroupProtocols.Checked[6] := Protocols.ACDI;
    CheckGroupProtocols.Checked[7] := Protocols.Datagram;
    CheckGroupProtocols.Checked[8] := Protocols.Stream;
    Result := True;
  end;
end;

function TFormNodeProperties.LoadSnip(SimpleNodeInfo: TSimpleNodeInfo): Boolean;
begin
  Result := False;
  if SimpleNodeInfo.Valid then
  begin
    LabelVersion.Caption := IntToStr(SimpleNodeInfo.Version);
    LabelMfg.Caption := SimpleNodeInfo.Manufacturer;
    LabelModel.Caption := SimpleNodeInfo.Model;
    LabelSoftwareVer.Caption := SimpleNodeInfo.SoftwareVersion;
    LabelHardwareVer.Caption := SimpleNodeInfo.HardwareVersion;
    LabelUserVersion.Caption := IntToStr(SimpleNodeInfo.UserVersion);
    LabelUserName.Caption := SimpleNodeInfo.UserName;
    LabelUserDesc.Caption := SimpleNodeInfo.UserDescription;
    Result := True;
  end;
end;

function TFormNodeProperties.LoadCdi(Cdi: TCDI): Boolean;
var
  LocalText: string;
  i: Integer;
  XML: TXMLDocument;
  TempStream: TMemoryStream;
begin
  Result := False;
  if Cdi.Valid then
  begin
    TempStream := TMemoryStream.Create;
    Cdi.AStream.Position := 0;
    TempStream.CopyFrom(Cdi.AStream, Cdi.AStream.Size);
    TempStream.Position := TempStream.Size - 1;
    if Char(TempStream.ReadByte) = #0 then
      TempStream.Size := TempStream.Size - 1;     // Strip the null
    TempStream.Position := 0;
    ReadXMLFile(XML, TempStream);
    TempStream.Clear;
    WriteXML(XML, TempStream);
    LocalText := '';
    TempStream.Position := 0;
    for i := 0 to TempStream.Size - 1 do
      LocalText := LocalText + Char(TempStream.ReadByte);
    SynEdit.ClearAll;
    SynEdit.Text := LocalText;
    FreeAndNil(XML);
    FreeAndNil(TempStream);
    Result := True;
  end;
end;

end.

