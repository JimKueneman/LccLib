unit frame_lcc_settings;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, Spin,
  Buttons, synaser, lcc_app_common_settings, Dialogs, LResources;

type
  TFrameLccSettingType = (
    flst_Server,
    flst_Client
  );


type

  TFrameLccSettings = class;

  { TUserSettings }

  TUserSettings = class(TPersistent)
  private
    FOwnerSettings: TFrameLccSettings;
    function GetComPort: Boolean;
    function GetEthernet: Boolean;
    function GetSettingType: TFrameLccSettingType;
    procedure SetComPort(AValue: Boolean);
    procedure SetEthernet(AValue: Boolean);
    procedure SetSettingType(AValue: TFrameLccSettingType);
  public
    property OwnerSettings: TFrameLccSettings read FOwnerSettings write FOwnerSettings;
  published
    property SettingType: TFrameLccSettingType read GetSettingType write SetSettingType;
    property ComPort: Boolean read GetComPort write SetComPort;
    property Ethernet: Boolean read GetEthernet write SetEthernet;
  end;

  { TFrameLccSettings }

  TFrameLccSettings = class(TFrame)
    BitBtnRescanPorts: TBitBtn;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    ButtonSetLoopbackClient: TButton;
    ButtonSetLoopbackRemote: TButton;
    ComboBoxComPort: TComboBox;
    EditLocalIP: TEdit;
    EditRemoteIP: TEdit;
    GroupBoxComPort: TGroupBox;
    GroupBoxEthernet: TGroupBox;
    LabelLocalIP: TLabel;
    LabelLocalPort: TLabel;
    LabelRemoteIP: TLabel;
    LabelRemotePort: TLabel;
    SpinEditLocalPort: TSpinEdit;
    SpinEditRemotePort: TSpinEdit;
    procedure BitBtnRescanPortsClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonSetLoopbackRemoteClick(Sender: TObject);
    procedure ButtonSetLoopbackClientClick(Sender: TObject);
    procedure ComboBoxComPortChange(Sender: TObject);
    procedure EditRemoteIPExit(Sender: TObject);
    procedure EditRemoteIPKeyPress(Sender: TObject; var Key: char);
    procedure SpinEditRemotePortExit(Sender: TObject);
    procedure SpinEditRemotePortKeyPress(Sender: TObject; var Key: char);
  private
    FLockSetting: Boolean;
  private
    FComPort: Boolean;
    FEthernet: Boolean;
    FLccSettings: TLccSettings;
    FSettingsFilePath: string;
    FSettingType: TFrameLccSettingType;
    FUserSettings: TUserSettings;
    { private declarations }
    procedure SetComPort(AValue: Boolean);
    procedure SetEthernet(AValue: Boolean);
    procedure SetSettingType(AValue: TFrameLccSettingType);
    property SettingsFilePath: string read FSettingsFilePath write FSettingsFilePath;
    property LockSetting: Boolean read FLockSetting write FLockSetting;
  protected
    procedure PositionButtons;
    property SettingType: TFrameLccSettingType read FSettingType write SetSettingType;
    property ComPort: Boolean read FComPort write SetComPort;
    property Ethernet: Boolean read FEthernet write SetEthernet;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SyncSettingFile(SettingsFile: string);
    procedure ScanComPorts;
    procedure StoreSettings;
  published
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property UserSettings: TUserSettings read FUserSettings write FUserSettings;
  end;

procedure Register;

implementation

{$R *.lfm}

procedure Register;
begin
  {$I TFrameLccSettings.lrs}
  RegisterComponents('LCC',[TFrameLccSettings]);
end;

{ TUserSettings }

function TUserSettings.GetComPort: Boolean;
begin
  Result := OwnerSettings.ComPort;
end;

function TUserSettings.GetEthernet: Boolean;
begin
  Result := OwnerSettings.Ethernet;
end;

function TUserSettings.GetSettingType: TFrameLccSettingType;
begin
  Result := OwnerSettings.SettingType;
end;

procedure TUserSettings.SetComPort(AValue: Boolean);
begin
  OwnerSettings.ComPort := AValue;
end;

procedure TUserSettings.SetEthernet(AValue: Boolean);
begin
  OwnerSettings.Ethernet := AValue;
end;

procedure TUserSettings.SetSettingType(AValue: TFrameLccSettingType);
begin
  OwnerSettings.SettingType := AValue;
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

procedure TFrameLccSettings.ButtonSetLoopbackRemoteClick(Sender: TObject);
begin
  EditRemoteIP.Text := '127.0.0.1';
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
  if FComPort then
    GroupBoxEthernet.Top := 78
  else
    GroupBoxEthernet.Top := 8;
  PositionButtons;
end;

procedure TFrameLccSettings.SetEthernet(AValue: Boolean);
begin
  FEthernet:=AValue;
  if csDesigning in ComponentState then Exit;
  GroupBoxEthernet.Visible := FEthernet;
  PositionButtons;
end;

procedure TFrameLccSettings.SetSettingType(AValue: TFrameLccSettingType);
begin
  FSettingType:=AValue;
  if csDesigning in ComponentState then Exit;
  case SettingType of
    flst_Server :
      begin
        LabelRemoteIP.Visible := False;
        LabelRemotePort.Visible := False;
        EditRemoteIP.Visible := False;
        SpinEditRemotePort.Visible := False;
        ButtonSetLoopbackRemote.Visible := False;

        LabelLocalIP.Visible := True;
        LabelLocalPort.Visible := True;
        EditLocalIP.Visible := True;
        SpinEditLocalPort.Visible := True;

        LabelLocalIP.Caption := 'Listener IP';
        LabelLocalPort.Caption := 'Listener Port';

        GroupBoxEthernet.Height := 84;
      end;
    flst_Client :
      begin
        LabelRemoteIP.Visible := True;
        LabelRemotePort.Visible := True;
        EditRemoteIP.Visible := True;
        SpinEditRemotePort.Visible := True;
        ButtonSetLoopbackRemote.Visible := True;

        LabelLocalIP.Visible := True;
        LabelLocalPort.Visible := True;
        EditLocalIP.Visible := True;
        SpinEditLocalPort.Visible := True;

        LabelLocalIP.Caption := 'Client IP';
        LabelLocalPort.Caption := 'Client Port';
        LabelRemoteIP.Caption := 'Listener IP';
        LabelRemotePort.Caption := 'Listener Port';

        GroupBoxEthernet.Height := 146;
      end;
  end;
  PositionButtons;
end;

procedure TFrameLccSettings.SpinEditRemotePortExit(Sender: TObject);
begin
  StoreSettings;
end;

procedure TFrameLccSettings.SpinEditRemotePortKeyPress(
  Sender: TObject; var Key: char);
begin
  if Key = #13 then
    StoreSettings;
end;

procedure TFrameLccSettings.StoreSettings;
begin
  if not LockSetting and Assigned(LccSettings) then
  begin
    if SettingsFilePath = '' then
      ShowMessage('The SettingsFilePath is empty, can''t save the settings')
    else begin
      LccSettings.ComPort.Port := ComboBoxComPort.Caption;
      LccSettings.Ethernet.LocalIP := EditLocalIP.Text;
      LccSettings.Ethernet.LocalPort := SpinEditLocalPort.Value;
      LccSettings.Ethernet.RemoteIP := EditRemoteIP.Text;
      LccSettings.Ethernet.RemotePort := SpinEditRemotePort.Value;
      LccSettings.SaveToFile(SettingsFilePath);
    end;
  end;
end;

procedure TFrameLccSettings.ButtonSetLoopbackClientClick(Sender: TObject);
begin
  EditLocalIP.Text := '127.0.0.1';
end;

procedure TFrameLccSettings.EditRemoteIPExit(Sender: TObject);
begin
  StoreSettings;
end;

procedure TFrameLccSettings.EditRemoteIPKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then
    StoreSettings;
end;

procedure TFrameLccSettings.PositionButtons;
begin
  if GroupBoxEthernet.Visible then
  begin
    ButtonCancel.Top := GroupBoxEthernet.Top + GroupBoxEthernet.Height + 8;
    ButtonOk.Top := GroupBoxEthernet.Top + GroupBoxEthernet.Height + 8;
    Height := ButtonOk.Top + ButtonOk.Height + 8;
  end else
  begin
    ButtonCancel.Top := GroupBoxComPort.Top + GroupBoxComPort.Height + 8;
    ButtonOk.Top := GroupBoxComPort.Top + GroupBoxComPort.Height + 8;
    Height := ButtonOk.Top + ButtonOk.Height + 8;
  end;
end;

procedure TFrameLccSettings.SyncSettingFile(SettingsFile: string);
begin
  if Assigned(LccSettings) then
  begin
    LockSetting := True;
    try
      SettingsFilePath := SettingsFile;
      BitBtnRescanPorts.Click;
      EditLocalIP.Text := LccSettings.Ethernet.LocalIP;
      SpinEditLocalPort.Value := LccSettings.Ethernet.LocalPort;
      EditRemoteIP.Text := LccSettings.Ethernet.RemoteIP;
      SpinEditRemotePort.Value := LccSettings.Ethernet.RemotePort;
    finally
      LockSetting := False;
    end;
  end;
end;

end.

