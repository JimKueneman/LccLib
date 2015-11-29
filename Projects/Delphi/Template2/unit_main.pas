unit unit_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, lcc_app_common_settings,
  lcc_common_classes, lcc_ethernetclient, lcc_messages, file_utilities,
  FMX.ListBox, FMX.Edit, FMX.Layouts, lcc_node, lcc_node_protocol_helpers,
  lcc_can_message_assembler_disassembler, FMX.ScrollBox, FMX.Memo;

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
    Memo: TMemo;
    StatusBar1: TStatusBar;
    SpeedButtonClearMemo: TSpeedButton;
    SpeedButtonLog: TSpeedButton;
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
    procedure FormDestroy(Sender: TObject);
    procedure LccEthernetClientReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure SpeedButtonClearMemoClick(Sender: TObject);
    procedure SpeedButtonLogClick(Sender: TObject);
  private
    FGeneralTimerActions: TGeneralTimerActions;
    FVirtualNode: TLccVirtualNode;
    FGridConnectAssember: TLccMessageAssembler;
    FGridConnectDisassember: TLccMessageDisAssembler;
    FWorkerMessage: TLccMessage;
    FLogging: Boolean;
    { Private declarations }
  protected
    property GeneralTimerActions: TGeneralTimerActions read FGeneralTimerActions write FGeneralTimerActions;
    procedure RequestSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure SyncAppToSettings;
    procedure SyncSettingsToApp;
    procedure Log(IsSend: Boolean; const LogText: string);
  public
    { Public declarations }
    property VirtualNode: TLccVirtualNode read FVirtualNode write FVirtualNode;
    property GridConnectAssember: TLccMessageAssembler read FGridConnectAssember write FGridConnectAssember;
    property GridConnectDisassember: TLccMessageDisAssembler read FGridConnectDisassember write FGridConnectDisassember;
    property Logging: Boolean read FLogging write FLogging;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;

var
  TabbedwithNavigationForm: TTabbedwithNavigationForm;

implementation

{$R *.fmx}

procedure TTabbedwithNavigationForm.EditServerExit(Sender: TObject);
begin
  if ValidateIP(EditServer.Text) then
  begin
    SyncAppToSettings;
    GeneralTimerActions := gtaEthernetLogin;
  end else
    ShowMessage('Invalid IP Address format');
end;

procedure TTabbedwithNavigationForm.EditServerKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
   begin
     if ValidateIP(EditServer.Text) then
     begin
      SyncAppToSettings;
      GeneralTimerActions := gtaEthernetLogin;
     end else
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
  VirtualNode := TLccVirtualNode.Create(nil);
  VirtualNode.OnRequestSendMessage := RequestSendMessage;
  GridConnectAssember := TLccMessageAssembler.Create;
  GridConnectDisassember := TLccMessageDisAssembler.Create;
  WorkerMessage := TLccMessage.Create;
  TabControl1.ActiveTab := TabItem1;
  LccSettings.FilePath := GetSettingsPath + 'Settings.ini';
  SyncSettingsToApp;
end;

procedure TTabbedwithNavigationForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVirtualNode);
  FreeAndNil(FGridConnectAssember);
  FreeAndNil(FGridConnectDisassember);
  FreeAndNil(FWorkerMessage);
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
    ccsClientConnected     :
      begin
        VirtualNode.LoginWithLccSettings(False, LccSettings);
      end;
    ccsClientDisconnecting :
      begin
        VirtualNode.Logout;
      end;
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

procedure TTabbedwithNavigationForm.LccEthernetClientReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  Log(False, EthernetRec.MessageStr);
  case GridConnectAssember.IncomingMessageGridConnect(EthernetRec.MessageStr, WorkerMessage) of
    imgcr_False : begin end;
    imgcr_True : VirtualNode.ProcessMessage(WorkerMessage);
    imgcr_ErrorToSend : LccEthernetClientSendMessage(Self, WorkerMessage);
    imgcr_UnknownError : begin end;
  end;
end;

procedure TTabbedwithNavigationForm.LccEthernetClientSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  RequestSendMessage(Sender, LccMessage);
end;

procedure TTabbedwithNavigationForm.Log(IsSend: Boolean; const LogText: string);
begin
  if Logging then
  begin
    Memo.BeginUpdate;
    try
      if IsSend then
        Memo.Lines.Add('S: ' + LogText)
      else
        Memo.Lines.Add('R: ' + LogText)
    finally
      Memo.EndUpdate
    end;
  end;
end;

procedure TTabbedwithNavigationForm.RequestSendMessage(Sender: TObject; LccMessage: TLccMessage);
var
  OutString: string;
begin
  OutString := GridConnectDisassember.OutgoingMsgToGridConnect(LccMessage);
  Log(True, OutString);
  LccEthernetClient.SendMessageRawGridConnect(OutString);
end;

procedure TTabbedwithNavigationForm.SpeedButtonClearMemoClick(Sender: TObject);
begin
  Memo.BeginUpdate;
  try
    Memo.Lines.Clear
  finally
    Memo.EndUpdate
  end;
end;

procedure TTabbedwithNavigationForm.SpeedButtonLogClick(Sender: TObject);
begin
  if SpeedButtonLog.StyleLookup = 'playtoolbutton' then
  begin
    SpeedButtonLog.StyleLookup := 'pausetoolbutton';
    Logging := True;
  end else
  begin
    SpeedButtonLog.StyleLookup := 'playtoolbutton';
    Logging := False;
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
        LccEthernetClient.CloseConnection(nil);
        LccEthernetClient.OpenConnectionWithLccSettings;
        GeneralTimerActions := gtaNone;
      end;
    gtaLccLogin :
      begin

      end;
  end;

end;

end.

