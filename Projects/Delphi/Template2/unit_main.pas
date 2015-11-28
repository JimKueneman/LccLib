unit unit_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, lcc_app_common_settings,
  lcc_common_classes, lcc_ethernetclient, lcc_messages, file_utilities,
  FMX.ListBox, FMX.Edit, FMX.Layouts;

type
  TGeneralTimerActions = (gtaNone, gtaEthernetLogin, gtaLccLogin);

  TTabbedwithNavigationForm = class(TForm)
    GestureManager1: TGestureManager;
    LccEthernetClient: TLccEthernetClient;
    LccSettings: TLccSettings;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabControl2: TTabControl;
    TabItem5: TTabItem;
    ToolBar1: TToolBar;
    lblTitle1: TLabel;
    btnNext: TSpeedButton;
    TabItem6: TTabItem;
    ToolBar2: TToolBar;
    lblTitle2: TLabel;
    btnBack: TSpeedButton;
    TabItem2: TTabItem;
    ToolBar3: TToolBar;
    lblTitle3: TLabel;
    TabItem3: TTabItem;
    ToolBar4: TToolBar;
    lblTitle4: TLabel;
    TabItemSettings: TTabItem;
    ToolBar5: TToolBar;
    Label1: TLabel;
    LayoutBkgnd: TLayout;
    ListBoxSettings: TListBox;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItemNetworkServerBkGnd: TListBoxItem;
    Layout4: TLayout;
    EditServer: TEdit;
    ListBoxItem3: TListBoxItem;
    Layout3: TLayout;
    SwitchTcp: TSwitch;
    ListBoxGroupFooter1: TListBoxGroupFooter;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    Layout2: TLayout;
    EditNodeID: TEdit;
    ListBoxItem2: TListBoxItem;
    Layout1: TLayout;
    LabelAlias: TLabel;
    TimerGeneral: TTimer;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure LccEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure EditServerExit(Sender: TObject);
    procedure EditServerKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure SwitchTcpSwitch(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerGeneralTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FGeneralTimerActions: TGeneralTimerActions;
    { Private declarations }
  protected
    property GeneralTimerActions: TGeneralTimerActions read FGeneralTimerActions write FGeneralTimerActions;
    procedure SyncAppToSettings;
    procedure SyncSettingsToApp;
  public
    { Public declarations }
  end;

var
  TabbedwithNavigationForm: TTabbedwithNavigationForm;

implementation

{$R *.fmx}

procedure TTabbedwithNavigationForm.EditServerExit(Sender: TObject);
begin
  if ValidateIP(EditServer.Text) then
    SyncAppToSettings
  else
    ShowMessage('Invalid IP Address format');
end;

procedure TTabbedwithNavigationForm.EditServerKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
   begin
     if ValidateIP(EditServer.Text) then
      SyncAppToSettings
    else
      ShowMessage('Invalid IP Address format');
   end
end;

procedure TTabbedwithNavigationForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  LccEthernetClient.CloseConnection(nil);
end;

procedure TTabbedwithNavigationForm.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControl1.ActiveTab := TabItem1;
  LccSettings.FilePath := GetSettingsPath + 'Settings.ini';
  SyncSettingsToApp;
end;

procedure TTabbedwithNavigationForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    if (TabControl1.ActiveTab = TabItem1) and (TabControl2.ActiveTab = TabItem6) then
    begin
      TabControl2.Previous;
      Key := 0;
    end;
  end;
end;

procedure TTabbedwithNavigationForm.FormShow(Sender: TObject);
begin
  GeneralTimerActions := gtaEthernetLogin;
  TimerGeneral.Enabled := True;
end;

procedure TTabbedwithNavigationForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[TabControl1.TabCount - 1] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex + 1];
        Handled := True;
      end;

    sgiRight:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[0] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex - 1];
        Handled := True;
      end;
  end;
end;

procedure TTabbedwithNavigationForm.LccEthernetClientConnectionStateChange(
  Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting    : begin end;
    ccsClientConnected     : begin end;
    ccsClientDisconnecting : begin end;
    ccsClientDisconnected  : begin end;

    ccsListenerConnecting,
    ccsListenerConnected,
    ccsListenerDisconnecting,
    ccsListenerDisconnected,
    ccsListenerClientConnecting,
    ccsListenerClientConnected,
    ccsListenerClientDisconnecting,
    ccsListenerClientDisconnected,
    ccsPortConnecting,
    ccsPortConnected,
    ccsPortDisconnecting,
    ccsPortDisconnected  : begin end;  // Do nothing in client mode
  end;
end;

procedure TTabbedwithNavigationForm.SwitchTcpSwitch(Sender: TObject);
begin
  SyncAppToSettings
end;

procedure TTabbedwithNavigationForm.SyncAppToSettings;
begin
  LccSettings.Ethernet.RemoteListenerIP := EditServer.Text;
  LccSettings.Ethernet.Tcp := SwitchTcp.IsChecked;
  LccSettings.SaveToFile;
end;

procedure TTabbedwithNavigationForm.SyncSettingsToApp;
begin
  if FileExists(LccSettings.FilePath) then
    LccSettings.LoadFromFile;
  EditServer.Text := LccSettings.Ethernet.RemoteListenerIP;
  SwitchTcp.IsChecked := LccSettings.Ethernet.Tcp;
end;

procedure TTabbedwithNavigationForm.TimerGeneralTimer(Sender: TObject);
begin
  case GeneralTimerActions of
    gtaNone : begin end;
    gtaEthernetLogin :
      begin
        LccEthernetClient.OpenConnectionWithLccSettings;
        GeneralTimerActions := gtaNone;
      end;
    gtaLccLogin :
      begin

      end;
  end;

end;

end.

