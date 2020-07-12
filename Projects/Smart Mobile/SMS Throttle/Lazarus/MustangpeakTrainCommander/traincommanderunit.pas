unit TrainCommanderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, lcc_ethernet_server, lcc_defines, lcc_node,
  lcc_node_manager, lcc_ethernet_client, lcc_node_messages,
  lcc_node_commandstation, lcc_node_controller, lcc_node_train,
  lcc_comport, synaser;

type

  { TFormTrainCommander }

  TFormTrainCommander = class(TForm)
    Button1: TButton;
    ButtonTrainsClear: TButton;
    ButtonClear: TButton;
    ButtonManualConnect: TButton;
    CheckBoxUseSyncronize: TCheckBox;
    CheckBoxLogMessages: TCheckBox;
    CheckBoxLoopBackIP: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    ComboBox1: TComboBox;
    ImageListMain: TImageList;
    LabelNodeID: TLabel;
    LabelAliasID: TLabel;
    LabelAliasIDCaption: TLabel;
    LabelNodeIDCaption: TLabel;
    ListViewTrains: TListView;
    ListviewConnections: TListView;
    MemoLog: TMemo;
    PanelTrainsHeader: TPanel;
    PanelTrains: TPanel;
    PanelConnections: TPanel;
    PanelDetails: TPanel;
    SplitterTrains: TSplitter;
    SplitterConnections: TSplitter;
    StatusBarMain: TStatusBar;
    TimerIncomingMessagePump: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonManualConnectClick(Sender: TObject);
    procedure ButtonTrainsClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerIncomingMessagePumpTimer(Sender: TObject);
  private
    FNodeManager: TLccCanNodeManager;
    FWorkerMsg: TLccMessage;
    FLccServer: TLccEthernetServer;
  protected
    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;

    function ConnectServer: Boolean;
    procedure DisconnectServer;

    // Callbacks from the Ethernet Server
    procedure OnCommandStationServerConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationServerClientDisconnect(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);

    // Callbacks from the Node Manager
    procedure OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogout(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);

  public
    property LccServer: TLccEthernetServer read FLccServer write FLccServer;
    property NodeManager: TLccCanNodeManager read FNodeManager write FNodeManager;
  end;

var
  FormTrainCommander: TFormTrainCommander;

implementation

{$R *.lfm}

{ TFormTrainCommander }

procedure TFormTrainCommander.Button1Click(Sender: TObject);
begin
  ComboBox1.Items.Delimiter := ';';
  ComboBox1.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TFormTrainCommander.ButtonClearClick(Sender: TObject);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Clear;
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.ButtonManualConnectClick(Sender: TObject);
begin
  if LccServer.Connected then
    DisconnectServer
  else
    ConnectServer;
end;

procedure TFormTrainCommander.ButtonTrainsClearClick(Sender: TObject);
begin
  ListViewTrains.Clear;
end;

function TFormTrainCommander.ConnectServer: Boolean;
var
  EthernetRec: TLccEthernetRec;
begin
  Result := False;
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
  EthernetRec.ListenerIP := '127.0.0.1';
  EthernetRec.ListenerPort := 12021;
  LccServer.UseSynchronize := CheckBoxUseSyncronize.Checked;    // Do we call the timer to pump a receive buffer or do we let it call back through Syncronize on every message received
  Result := LccServer.OpenConnection(EthernetRec) <> nil;
end;

procedure TFormTrainCommander.DisconnectServer;
begin
  LccServer.CloseConnection(nil);
end;

procedure TFormTrainCommander.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  NodeManager.Clear;
  LccServer.CloseConnection(nil);
end;

procedure TFormTrainCommander.FormCreate(Sender: TObject);
begin
  FLccServer := TLccEthernetServer.Create(nil);
  LccServer.OnConnectionStateChange := @OnCommandStationServerConnectionState;
  LccServer.OnClientDisconnect := @OnCommandStationServerClientDisconnect;
  LccServer.OnErrorMessage := @OnCommandStationServerErrorMessage;
  LccServer.Gridconnect := True;
  LccServer.Hub := True;

  NodeManager := TLccCanNodeManager.Create(nil);
  NodeManager.OnLccNodeAliasIDChanged := @OnNodeManagerAliasIDChanged;
  NodeManager.OnLccNodeIDChanged := @OnNodeManagerIDChanged;
  NodeManager.OnLccMessageReceive := @OnNodeManagerReceiveMessage;
  NodeManager.OnLccMessageSend := @OnNodeManagerSendMessage;
  NodeManager.OnLccNodeLogin := @OnNodeManagerNodeLogin;
  NodeManager.OnLccNodeLogout := @OnNodeManagerNodeLogout;

  LccServer.NodeManager := NodeManager;

  FWorkerMsg := TLccMessage.Create;
end;

procedure TFormTrainCommander.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FNodeManager);
  FreeAndNil(FLccServer);
  FreeAndNil(FWorkerMsg);
end;

procedure TFormTrainCommander.OnCommandStationServerClientDisconnect(Sender: TObject; EthernetRec: TLccEthernetRec);
begin

end;

procedure TFormTrainCommander.OnCommandStationServerConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        ButtonManualConnect.Enabled := False;
        StatusBarMain.Panels[0].Text := 'Command Station Disconnecting from Ethernet';
        StatusBarMain.Panels[1].Text := '';
      end;
    ccsListenerConnected :
      begin
        ButtonManualConnect.Enabled := True;
        ButtonManualConnect.Caption := 'Manual Disconnect';
        StatusBarMain.Panels[0].Text := 'Command Station Connected at: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        NodeManager.AddNodeByClass('', TLccCommandStationNode, True);
      end;
    ccsListenerDisconnecting :
      begin
        ButtonManualConnect.Enabled := False;
        ButtonManualConnect.Caption := 'Command Station Disconnecting from Ethernet';
        NodeManager.Clear;
      end;
    ccsListenerDisconnected :
      begin
        ButtonManualConnect.Enabled := True;
        ButtonManualConnect.Caption := 'Manual Connect';
        StatusBarMain.Panels[0].Text := 'Command Station Disconnected from Ethernet';
      end;
    ccsListenerClientConnecting :
      begin
         StatusBarMain.Panels[1].Text := 'New Throttle Connecting to Command Station'
      end;
    ccsListenerClientConnected :
      begin
         StatusBarMain.Panels[1].Text := 'New Throttle Connected to Command Station';
         ListItem := ListviewConnections.Items.Add;
         Listitem.Caption := 'Throttle connected: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
         ListItem.ImageIndex := 3;
      end;
    ccsListenerClientDisconnecting :
      begin
        StatusBarMain.Panels[1].Text := 'Throttle Disconnecting from Command Station';
      end;
    ccsListenerClientDisconnected :
      begin
        StatusBarMain.Panels[1].Text := 'Throttle Disconnected from Command Station';
        ListItem := ListviewConnections.FindCaption(0, 'Throttle connected: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort), True, True, True, True);
        if Assigned(ListItem) then
          ListviewConnections.Items.Delete(ListItem.Index);
      end;
  end;
end;

procedure TFormTrainCommander.OnCommandStationServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  StatusBarMain.Panels[1].Text := EthernetRec.MessageStr;
end;

procedure TFormTrainCommander.OnNodeManagerAliasIDChanged(Sender: TObject;
  LccSourceNode: TLccNode);
begin
  LabelAliasID.Caption := ( LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TFormTrainCommander.OnNodeManagerIDChanged(Sender: TObject;
  LccSourceNode: TLccNode);
begin
  LabelNodeID.Caption := LccSourceNode.NodeIDStr;
end;

procedure TFormTrainCommander.OnNodeManagerNodeLogin(Sender: TObject;
  LccSourceNode: TLccNode);
var
  TrainNode: TLccTrainCanNode;
  Item: TListItem;
  SpeedStep: string;
begin
  if LccSourceNode is TLccTrainCanNode then
  begin
    TrainNode := LccSourceNode as TLccTrainCanNode;
    Item := ListViewTrains.Items.Add;
    Item.Data := TrainNode;
    Item.ImageIndex := 18;
    case TrainNode.SpeedStep of
      ldssDefault : SpeedStep := 'Default Step';
      ldss14      : SpeedStep := '14 Step';
      ldss28      : SpeedStep := '28 Step';
      ldss128     : SpeedStep := '128 Step';
    end;
    if TrainNode.DccLongAddress then
      Item.Caption := 'Train Node: ' + IntToStr(TrainNode.DccAddress) + ' Long ' + SpeedStep
    else
      Item.Caption := 'Train Node: ' + IntToStr(TrainNode.DccAddress) + ' Short ' + SpeedStep;
  end;
end;

procedure TFormTrainCommander.OnNodeManagerNodeLogout(Sender: TObject; LccSourceNode: TLccNode);
var
  i: Integer;
begin
  if LccSourceNode is TLccTrainCanNode then
  begin
    for i := 0 to ListViewTrains.Items.Count - 1 do
    begin
      if ListViewTrains.Items[i].Data = Pointer(LccSourceNode) then
      begin
        ListViewTrains.Items.Delete(i);
        Break;
      end;
    end;
  end;
end;

procedure TFormTrainCommander.OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  if CheckBoxLogMessages.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      MemoLog.Lines.Add('R: ' + LccMessage.ConvertToGridConnectStr('', False));
      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainCommander.OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  LccServer.SendMessage(LccMessage);

  if CheckBoxLogMessages.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      MemoLog.Lines.Add('S: ' + LccMessage.ConvertToGridConnectStr('', False));
      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainCommander.TimerIncomingMessagePumpTimer(Sender: TObject);
var
  List: TStringList;
  i: Integer;
  GridConnectStr: string;
  LocalMsg: TLccMessage;
begin
  LocalMsg := nil;
  List := LccServer.IncomingGridConnect.LockList;
  try
    if List.Count > 0 then
      LocalMsg := TLccMessage.Create;

    for i := 0 to List.Count - 1 do
    begin
      GridConnectStr := List[i];
      LocalMsg.LoadByGridConnectStr(GridConnectStr);
      NodeManager.ProcessMessage(LocalMsg);
    end;
  finally

    GridConnectStr := List.Text;

    List.Clear;
    LccServer.IncomingGridConnect.UnlockList;
    FreeAndNil(LocalMsg);
  end;
end;

end.

