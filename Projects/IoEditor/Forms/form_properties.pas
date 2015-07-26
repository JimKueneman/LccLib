unit form_properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, lcc_nodeselector, lcc_nodemanager;

type

  { TFormNodeProperties }

  TFormNodeProperties = class(TForm)
    CheckGroupProtocols: TCheckGroup;
    CheckGroupConfigMem: TCheckGroup;
    GroupBoxMemSpaces: TGroupBox;
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
    MemoCDI: TMemo;
    PageControl1: TPageControl;
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
  MemoCDI.Clear;
  LccNodeSelectorMemSpaces.LccNodes.Clear;
  for i := 0 to CheckGroupConfigMem.Items.Count - 1 do
    CheckGroupConfigMem.Checked[i] := False;
  for i := 0 to CheckGroupProtocols.Items.Count - 1 do
    CheckGroupProtocols.Checked[i] := False;
end;

function TFormNodeProperties.LoadConfigMemAddressSpaceInfo(ConfigMemAddressSpaceInfo: TConfigMemAddressSpaceInfoObject): Boolean;
begin
  Result := False;
  if Assigned(ConfigMemAddressSpaceInfo) then
  begin
  //  LccNodeSelectorMemSpaces.;    // See if we have this one already and update it or add a new one...
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
    CheckGroupConfigMem.Checked[5] := ConfigurationMemOptions.WriteUnderMask;
    CheckGroupConfigMem.Checked[6] := ConfigurationMemOptions.UnAlignedWrites;
    CheckGroupConfigMem.Checked[7] := ConfigurationMemOptions.WriteStream;
    CheckGroupConfigMem.Checked[8] := ConfigurationMemOptions.UnAlignedReads;
    CheckGroupConfigMem.Checked[9] := ConfigurationMemOptions.SupportACDIMfgRead;
    CheckGroupConfigMem.Checked[10] := ConfigurationMemOptions.SupportACDIUserRead;
    CheckGroupConfigMem.Checked[11] := ConfigurationMemOptions.SupportACDIUserWrite;
    CheckGroupConfigMem.Checked[12] := ConfigurationMemOptions.WriteStream;
    CheckGroupConfigMem.Checked[13] := ConfigurationMemOptions.WriteStream;
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
begin
  Result := False;
  if Cdi.Valid then
  begin
    LocalText := '';
    CDI.AStream.Position := 0;
    for i := 0 to CDI.AStream.Size - 1 do
      LocalText := LocalText + Char(CDI.AStream.ReadByte);
    MemoCDI.Text := LocalText;
    Result := True;
  end;
end;

end.

