unit unit_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, lcc_app_common_settings,
  lcc_common_classes, lcc_ethernetclient, lcc_messages, file_utilities, lcc_utilities,
  FMX.ListBox, FMX.Edit, FMX.Layouts, lcc_node, lcc_node_protocol_helpers,
  lcc_can_message_assembler_disassembler, FMX.ScrollBox, FMX.Memo,
  FMX.ListView.Types, FMX.ListView, FMX.Menus, ksTableView, System.UIConsts;

const
  STR_CONNECTED = 'Connected';
  STR_DISCONNECTED = 'Disconnected;';

type
  TGeneralTimerActions = (gtaNone, gtaEthernetLogin, gtaLccLogin);

  TTabbedwithNavigationForm = class(TForm)
    GestureManager1: TGestureManager;
    LccEthernetClient: TLccEthernetClient;
    LccSettings: TLccSettings;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    TabControlMain: TTabControl;
    TabItemTrain: TTabItem;
    TabControl2: TTabControl;
    TabItem5: TTabItem;
    ToolBar1: TToolBar;
    lblTitle1: TLabel;
    btnNext: TSpeedButton;
    TabItem6: TTabItem;
    ToolBar2: TToolBar;
    lblTitle2: TLabel;
    btnBack: TSpeedButton;
    TabItemConsist: TTabItem;
    ToolBar3: TToolBar;
    lblTitle3: TLabel;
    TabItemLog: TTabItem;
    ToolBar4: TToolBar;
    lblTitle4: TLabel;
    TabItemSettings: TTabItem;
    ToolBar5: TToolBar;
    Label1: TLabel;
    LayoutBkgnd: TLayout;
    TimerGeneral: TTimer;
    StatusBar1: TStatusBar;
    SpeedButtonClearMemo: TSpeedButton;
    SpeedButtonLog: TSpeedButton;
    Memo: TMemo;
    MainMenu1: TMainMenu;
    ListView1: TListView;
    ksTableViewSettings: TksTableView;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditServerExit(Sender: TObject);
    procedure EditServerKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure SwitchTcpSwitch(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerGeneralTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure LccEthernetClientReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure LccEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure SpeedButtonClearMemoClick(Sender: TObject);
    procedure SpeedButtonLogClick(Sender: TObject);
    procedure EditNodeIDExit(Sender: TObject);
    procedure EditNodeIDKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditNodeIDEnter(Sender: TObject);
  private
    FGeneralTimerActions: TGeneralTimerActions;
    FVirtualNode: TLccVirtualNode;
    FGridConnectAssember: TLccMessageAssembler;
    FGridConnectDisassember: TLccMessageDisAssembler;
    FWorkerMessage: TLccMessage;
    FLogging: Boolean;
    FOldNodeID: string;
    FEditNodeID: TksTableViewItemEmbeddedEdit;
    FEditServer: TksTableViewItemEmbeddedEdit;
    FTcpSwitch: TksTableViewItemSwitch;
    { Private declarations }
  protected
    property EditNodeID: TksTableViewItemEmbeddedEdit read FEditNodeID write FEditNodeID;
    property EditServer: TksTableViewItemEmbeddedEdit read FEditServer write FEditServer;
    property TcpSwitch: TksTableViewItemSwitch read FTcpSwitch write FTcpSwitch;
    property GeneralTimerActions: TGeneralTimerActions read FGeneralTimerActions write FGeneralTimerActions;
    property OldNodeID: string read FOldNodeID write FOldNodeID;

    procedure RequestSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeEvent(Sender: TLccCoreNode; EventCode: Integer);
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

procedure TTabbedwithNavigationForm.EditNodeIDEnter(Sender: TObject);
begin
  OldNodeID := EditNodeID.Text;
end;

procedure TTabbedwithNavigationForm.EditNodeIDExit(Sender: TObject);
begin
  if ValidateNodeIDAsHexString(EditNodeID.Text) then
  begin
    SyncAppToSettings;
    GeneralTimerActions := gtaEthernetLogin;
  end else
  begin
    EditNodeID.Text := OldNodeID;
    ShowMessage('Invalid NodeID');
  end;

end;

procedure TTabbedwithNavigationForm.EditNodeIDKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
   begin
     if ValidateNodeIDAsHexString(EditNodeID.Text) then
     begin
      SyncAppToSettings;
      GeneralTimerActions := gtaEthernetLogin;
     end else
     begin
       EditNodeID.Text := OldNodeID;
      ShowMessage('Invalid NodeID');
     end;
   end
end;

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
var
  AnItem: TksTableViewItem;
  AnEdit: TksTableViewItemEmbeddedEdit;
begin
  ksTableViewSettings.Items.AddHeader('Ethernet Settings');

  AnItem := ksTableViewSettings.Items.AddItem('IP Address');
  AnEdit := AnItem.AddEdit(0, 0, 120, '192.168.0.254');
  AnEdit.Align := TksTableItemAlign.Trailing;
  AnEdit.VertAlign := TksTableItemAlign.Center;
  EditServer := AnEdit;

  AnItem := ksTableViewSettings.Items.AddItem('IP Port');
  AnEdit := AnItem.AddEdit(0, 0, 120, '12021');
  AnEdit.Align := TksTableItemAlign.Trailing;
  AnEdit.VertAlign := TksTableItemAlign.Center;

  AnItem := ksTableViewSettings.Items.AddItem('Tcp Protocol');
  TcpSwitch := AnItem.AddSwitch(0, False);
  TcpSwitch.Align := TksTableItemAlign.Trailing;
  TcpSwitch.VertAlign := TksTableItemAlign.Center;

  ksTableViewSettings.Items.AddHeader('Lcc Settings');

  AnItem := ksTableViewSettings.Items.AddItem('Node ID');
  AnEdit := AnItem.AddEdit(0, 0, 120, '0x000000000000');
  AnEdit.Align := TksTableItemAlign.Trailing;
  AnEdit.VertAlign := TksTableItemAlign.Center;
  EditNodeID := AnEdit;

  AnItem := ksTableViewSettings.Items.AddItem('NodeAlias');
  AnEdit := AnItem.AddEdit(0, 0, 120, 'not assigned');
  AnEdit.Align := TksTableItemAlign.Trailing;
  AnEdit.VertAlign := TksTableItemAlign.Center;

  { This defines the default active tab at runtime }
  VirtualNode := TLccVirtualNode.Create(nil);
  VirtualNode.OnRequestSendMessage := RequestSendMessage;
  VirtualNode.OnEvent := OnNodeEvent;
  GridConnectAssember := TLccMessageAssembler.Create;
  GridConnectDisassember := TLccMessageDisAssembler.Create;
  WorkerMessage := TLccMessage.Create;
  TabControlMain.ActiveTab := TabItemTrain;
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
    if (TabControlMain.ActiveTab = TabItemTrain) and (TabControl2.ActiveTab = TabItem6) then
    begin
      TabControl2.Previous;
      Key := 0;
    end;
  end;
end;

procedure TTabbedwithNavigationForm.FormShow(Sender: TObject);
var
  Item: TListViewItem;
begin
  GeneralTimerActions := gtaEthernetLogin;
  TimerGeneral.Enabled := True;

  Item := ListView1.Items.Add;
  Item.Text := 'Bob';
  Item.Detail := 'Detail';
  Item.Accessory := TAccessoryType.Detail;
end;

procedure TTabbedwithNavigationForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControlMain.ActiveTab <> TabControlMain.Tabs[TabControlMain.TabCount - 1] then
          TabControlMain.ActiveTab := TabControlMain.Tabs[TabControlMain.TabIndex + 1];
        Handled := True;
      end;

    sgiRight:
      begin
        if TabControlMain.ActiveTab <> TabControlMain.Tabs[0] then
          TabControlMain.ActiveTab := TabControlMain.Tabs[TabControlMain.TabIndex - 1];
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
    //    LabelEthernetStatus.Text := STR_CONNECTED
      end;
    ccsClientDisconnecting :
      begin
        VirtualNode.Logout;
    //    LabelEthernetStatus.Text := STR_DISCONNECTED
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

procedure TTabbedwithNavigationForm.OnNodeEvent(Sender: TLccCoreNode; EventCode: Integer);
begin
  case EventCode of
    NODE_EVENT_LCC_LOGIN :
      begin
  //      LabelAlias.Text := Sender.AliasIDStr;
        EditNodeID.Text := LccSettings.General.NodeID;
  //      LabelLccStatus.Text := STR_CONNECTED
      end;
    NODE_EVENT_LCC_LOGOUT :
      begin
   //     LabelAlias.Text := 'Logged Out';
   //    LabelLccStatus.Text := STR_DISCONNECTED
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
 // LccSettings.Ethernet.Tcp := SwitchTcp.IsChecked;
  LccSettings.General.NodeID := EditNodeID.Text;
  LccSettings.SaveToFile;
  SyncSettingsToApp;
end;

procedure TTabbedwithNavigationForm.SyncSettingsToApp;
begin
  if FileExists(LccSettings.FilePath) then
    LccSettings.LoadFromFile;
  EditServer.Text := LccSettings.Ethernet.RemoteListenerIP;
 // SwitchTcp.IsChecked := LccSettings.Ethernet.Tcp;
  EditNodeID.Text := LccSettings.General.NodeID;
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

