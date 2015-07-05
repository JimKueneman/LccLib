unit frame_lcc_settings;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, Spin,
  Buttons, synaser, lcc_app_common_settings, Dialogs, LResources;

type

  TFrameLccSettings = class;

  { TUserSettings }

  TUserSettings = class(TPersistent)
  private
    FOwnerSettings: TFrameLccSettings;
    function GetComPort: Boolean;
    function GetEthernetClient: Boolean;
    function GetEthernetServer: Boolean;
    procedure SetComPort(AValue: Boolean);
    procedure SetEthernetClient(AValue: Boolean);
    procedure SetEthernetServer(AValue: Boolean);
  public
    property OwnerSettings: TFrameLccSettings read FOwnerSettings write FOwnerSettings;
  published
    property ComPort: Boolean read GetComPort write SetComPort;
    property EthernetClient: Boolean read GetEthernetClient write SetEthernetClient;
    property EthernetServer: Boolean read GetEthernetServer write SetEthernetServer;
  end;

  { TFrameLccSettings }

  TFrameLccSettings = class(TFrame)
    BitBtnRescanPorts: TBitBtn;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    ButtonSetLoopbackClient: TButton;
    ButtonSetLoopbackServer: TButton;
    ButtonSetLoopbackRemote: TButton;
    ComboBoxComPort: TComboBox;
    EditLocalClientIP: TEdit;
    EditLocalListenerIP: TEdit;
    EditRemoteListenerIP: TEdit;
    GroupBoxComPort: TGroupBox;
    GroupBoxEthernetClient: TGroupBox;
    GroupBoxEthernetServer: TGroupBox;
    LabelLocalIP: TLabel;
    LabelLocalIP1: TLabel;
    LabelLocalPort: TLabel;
    LabelLocalPort1: TLabel;
    LabelRemoteIP: TLabel;
    LabelRemotePort: TLabel;
    SpinEditLocalClientPort: TSpinEdit;
    SpinEditLocalListenerPort: TSpinEdit;
    SpinEditRemoteListenerPort: TSpinEdit;
    procedure BitBtnRescanPortsClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonSetLoopbackClientClick(Sender: TObject);
    procedure ButtonSetLoopbackRemoteClick(Sender: TObject);
    procedure ButtonSetLoopbackServerClick(Sender: TObject);
    procedure ComboBoxComPortChange(Sender: TObject);
    procedure EditRemoteListenerIPExit(Sender: TObject);
    procedure EditRemoteListenerIPKeyPress(Sender: TObject; var Key: char);
    procedure SpinEditRemoteListenerPortExit(Sender: TObject);
    procedure SpinEditRemoteListenerPortKeyPress(Sender: TObject; var Key: char);
  private
    FLockSetting: Boolean;
  private
    FComPort: Boolean;
    FEthernet: Boolean;
    FEthernetClient: Boolean;
    FEthernetServer: Boolean;
    FLccSettings: TLccSettings;
    FUserSettings: TUserSettings;
    { private declarations }
    procedure SetComPort(AValue: Boolean);
    procedure SetEthernetClient(AValue: Boolean);
    procedure SetEthernetServer(AValue: Boolean);
    property LockSetting: Boolean read FLockSetting write FLockSetting;
  protected
    procedure PositionButtons;
    property ComPort: Boolean read FComPort write SetComPort;
    property EthernetClient: Boolean read FEthernetClient write SetEthernetClient;
    property EthernetServer: Boolean read FEthernetServer write SetEthernetServer;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SyncWithLccSettings;
    procedure ScanComPorts;
    procedure StoreSettings;
  published
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property UserSettings: TUserSettings read FUserSettings write FUserSettings;
  end;
implementation

{$R *.lfm}

{ TUserSettings }

function TUserSettings.GetComPort: Boolean;
begin
  Result := OwnerSettings.ComPort;
end;

function TUserSettings.GetEthernetClient: Boolean;
begin
  Result := OwnerSettings.EthernetClient;
end;

function TUserSettings.GetEthernetServer: Boolean;
begin
  Result := OwnerSettings.EthernetServer;
end;


procedure TUserSettings.SetComPort(AValue: Boolean);
begin
  OwnerSettings.ComPort := AValue;
end;


procedure TUserSettings.SetEthernetClient(AValue: Boolean);
begin
  OwnerSettings.EthernetClient := AValue
end;

procedure TUserSettings.SetEthernetServer(AValue: Boolean);
begin
  OwnerSettings.EthernetServer := AValue
end;

{ TFrameLccSettings }

procedure TFrameLccSettings.BitBtnRescanPortsClick(Sender: TObject);
begin
  ScanComPorts;
end;

procedure TFrameLccSettings.ButtonOkClick(Sender: TObject);
begin
  StoreSettings
end;

procedure TFrameLccSettings.ButtonSetLoopbackClientClick(Sender: TObject);
begin
  EditLocalClientIP.Text := '127.0.0.1';
end;

procedure TFrameLccSettings.ButtonSetLoopbackRemoteClick(Sender: TObject);
begin
  EditRemoteListenerIP.Text := '127.0.0.1';
end;

procedure TFrameLccSettings.ButtonSetLoopbackServerClick(Sender: TObject);
begin
  EditLocalListenerIP.Text := '127.0.0.1';
end;

procedure TFrameLccSettings.ComboBoxComPortChange(Sender: TObject);
begin
  StoreSettings;
end;

constructor TFrameLccSettings.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUserSettings := TUserSettings.Create;
  UserSettings.OwnerSettings := Self;
end;

destructor TFrameLccSettings.Destroy;
begin
  FreeAndNil(FUserSettings);
  inherited Destroy;
end;

procedure TFrameLccSettings.ScanComPorts;
begin
  if Assigned(LccSettings) then
  begin
    LockSetting := True;
    try
      ComboBoxComPort.Items.Delimiter := ';';
      ComboBoxComPort.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);

      ComboBoxComPort.ItemIndex := ComboBoxComPort.Items.IndexOf(LccSettings.ComPort.Port);
      if (ComboBoxComPort.ItemIndex < 0) and (ComboBoxComPort.Items.Count > 0) then
        ComboBoxComPort.ItemIndex := 0;
    finally
      LockSetting := False;
    end;
  end;
end;

procedure TFrameLccSettings.SetComPort(AValue: Boolean);
begin
  FComPort:=AValue;
  if csDesigning in ComponentState then Exit;
  GroupBoxComPort.Visible := FComPort;
  PositionButtons;
end;

procedure TFrameLccSettings.SetEthernetClient(AValue: Boolean);
begin
  FEthernetClient:=AValue;
  if csDesigning in ComponentState then Exit;
  GroupBoxEthernetClient.Visible := AValue;
  PositionButtons;
end;

procedure TFrameLccSettings.SetEthernetServer(AValue: Boolean);
begin
  FEthernetServer:=AValue;
  if csDesigning in ComponentState then Exit;
  GroupBoxEthernetServer.Visible := AValue;
  PositionButtons;
end;

procedure TFrameLccSettings.SpinEditRemoteListenerPortExit(Sender: TObject);
begin
  StoreSettings;
end;

procedure TFrameLccSettings.SpinEditRemoteListenerPortKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    StoreSettings;
end;

procedure TFrameLccSettings.StoreSettings;
begin
  if not LockSetting and Assigned(LccSettings) then
  begin
    if LccSettings.FilePath = '' then
      ShowMessage('The SettingsFilePath is empty, can''t save the settings')
    else begin
      LccSettings.ComPort.Port := ComboBoxComPort.Caption;
      LccSettings.Ethernet.LocalClientIP := EditLocalClientIP.Text;
      LccSettings.Ethernet.LocalClientPort := SpinEditLocalClientPort.Value;
      LccSettings.Ethernet.RemoteListenerIP := EditRemoteListenerIP.Text;
      LccSettings.Ethernet.RemoteListenerPort := SpinEditRemoteListenerPort.Value;
      LccSettings.Ethernet.LocalListenerIP := EditLocalListenerIP.Text;
      LccSettings.Ethernet.LocalListenerPort := SpinEditLocalListenerPort.Value;
      LccSettings.SaveToFile(LccSettings.FilePath);
    end;
  end;
end;

procedure TFrameLccSettings.EditRemoteListenerIPExit(Sender: TObject);
begin
  StoreSettings;
end;

procedure TFrameLccSettings.EditRemoteListenerIPKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then
    StoreSettings;
end;

procedure TFrameLccSettings.PositionButtons;

  procedure PositionEthernetGroupsBoxes(Offset: Integer);
  begin
    if GroupBoxEthernetClient.Visible then
    begin
      GroupBoxEthernetClient.Top := Offset + 8;
      if GroupBoxEthernetServer.Visible then
      begin
        GroupBoxEthernetServer.Top := GroupBoxEthernetClient.Top + GroupBoxEthernetClient.Height + 8;
        ButtonCancel.Top := GroupBoxEthernetServer.Top + GroupBoxEthernetServer.Height + 8;
        ButtonOk.Top := ButtonCancel.Top;
        Height := ButtonOk.Top + ButtonOk.Height + 8;
      end else
      begin
        ButtonCancel.Top := GroupBoxEthernetClient.Top + GroupBoxEthernetClient.Height + 8;
        ButtonOk.Top := ButtonCancel.Top;
        Height := ButtonOk.Top + ButtonOk.Height + 8;
      end;
    end else
    begin
      if GroupBoxEthernetServer.Visible then
      begin
        GroupBoxEthernetServer.Top := Offset + 8;
        ButtonCancel.Top := GroupBoxEthernetServer.Top + GroupBoxEthernetServer.Height + 8;
        ButtonOk.Top := ButtonCancel.Top;
        Height := ButtonOk.Top + ButtonOk.Height + 8;
      end else
      begin
        ButtonCancel.Top := Offset + 8;
        ButtonOk.Top := ButtonCancel.Top;
        Height := ButtonOk.Top + ButtonOk.Height + 8;
      end;
    end;
  end;

begin
  if GroupBoxComPort.Visible then
  begin
    PositionEthernetGroupsBoxes(GroupBoxComPort.Top + GroupBoxComPort.Height);
  end else
  begin
    PositionEthernetGroupsBoxes(0);
  end;
end;

procedure TFrameLccSettings.SyncWithLccSettings;
begin
  if Assigned(LccSettings) then
  begin
    LockSetting := True;
    try
      BitBtnRescanPorts.Click;
      EditLocalClientIP.Text := LccSettings.Ethernet.LocalClientIP;
      SpinEditLocalClientPort.Value := LccSettings.Ethernet.LocalClientPort;
      EditRemoteListenerIP.Text := LccSettings.Ethernet.RemoteListenerIP;
      SpinEditRemoteListenerPort.Value := LccSettings.Ethernet.RemoteListenerPort;
      EditLocalListenerIP.Text := LccSettings.Ethernet.LocalListenerIP;
      SpinEditLocalListenerPort.Value := LccSettings.Ethernet.LocalListenerPort;
    finally
      LockSetting := False;
    end;
  end;
end;

end.

