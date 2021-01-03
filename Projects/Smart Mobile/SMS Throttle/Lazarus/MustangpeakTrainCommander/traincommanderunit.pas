unit TrainCommanderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, lcc_ethernet_server, lcc_defines, lcc_node,
  lcc_node_manager, lcc_ethernet_client, lcc_node_messages,
  lcc_node_commandstation, lcc_node_controller, lcc_node_train,
  lcc_comport, synaser, lcc_common_classes;

type

  { TFormTrainCommander }

  TFormTrainCommander = class(TForm)
    ButtonHTTPServer: TButton;
    ButtonActionObjectCount: TButton;
    ButtonWebserverConnect: TButton;
    ButtonManualConnectComPort: TButton;
    ButtonTrainsClear: TButton;
    ButtonClear: TButton;
    ButtonEthernetConnect: TButton;
    CheckBoxDetailedLog: TCheckBox;
    CheckBoxUseSyncronize: TCheckBox;
    CheckBoxLogMessages: TCheckBox;
    CheckBoxLoopBackIP: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    ComboBoxComPorts: TComboBox;
    ImageListMain: TImageList;
    LabelNodeID: TLabel;
    LabelAliasID: TLabel;
    LabelAliasIDCaption: TLabel;
    LabelNodeIDCaption: TLabel;
    ListViewTrains: TListView;
    ListviewConnections: TListView;
    MemoComPort: TMemo;
    MemoLog: TMemo;
    PanelTrainsHeader: TPanel;
    PanelTrains: TPanel;
    PanelConnections: TPanel;
    PanelDetails: TPanel;
    SplitterConnections1: TSplitter;
    SplitterTrains: TSplitter;
    SplitterConnections: TSplitter;
    StatusBarMain: TStatusBar;
    TimerIncomingMessagePump: TTimer;
    procedure ButtonActionObjectCountClick(Sender: TObject);
    procedure ButtonHTTPServerClick(Sender: TObject);
    procedure ButtonWebserverConnectClick(Sender: TObject);
    procedure ButtonManualConnectComPortClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonEthernetConnectClick(Sender: TObject);
    procedure ButtonTrainsClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerIncomingMessagePumpTimer(Sender: TObject);
  private
    FComPort: TLccComPort;
    FLccHTTPServer: TLccHTTPServer;
    FLccWebsocketServer: TLccWebsocketServer;
    FNodeManager: TLccCanNodeManager;
    FWorkerMessage: TLccMessage;
    FLccServer: TLccEthernetServer;
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    function ConnectServer: Boolean;
    procedure DisconnectServer;

    function ConnectWebsocketServer: Boolean;
    procedure DisconnectWebsocketServer;

    function ConnectHTTPServer: Boolean;
    procedure DisconnectHTTPServer;

    // Callbacks from the Ethernet Server
    procedure OnCommandStationServerConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    // Callbacks from the Websocket Server
    procedure OnCommandStationWebsocketConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationWebsocketErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    // Callbacks from the HTTP Server
    procedure OnCommandStationHTTPConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationHTTPErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);

    // Callbacks from the Node Manager
    procedure OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogout(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);

    procedure OnComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure OnComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure OnComPortReceiveMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure OnComPortSendMessage(Sender: TObject; var GridConnectStyleMessage: string);

  public
    property LccServer: TLccEthernetServer read FLccServer write FLccServer;
    property LccWebsocketServer: TLccWebsocketServer read FLccWebsocketServer write FLccWebsocketServer;
    property LccHTTPServer: TLccHTTPServer read FLccHTTPServer write FLccHTTPServer;
    property NodeManager: TLccCanNodeManager read FNodeManager write FNodeManager;
    property ComPort: TLccComPort read FComPort write FComPort;
  end;

var
  FormTrainCommander: TFormTrainCommander;

implementation

{$R *.lfm}

{ TFormTrainCommander }

procedure TFormTrainCommander.ButtonActionObjectCountClick(Sender: TObject);
begin
  ButtonActionObjectCount.Caption := 'Action Objects = ' + IntToStr(ActionObjectsAllocated);
end;

procedure TFormTrainCommander.ButtonHTTPServerClick(Sender: TObject);
begin
  if LccHTTPServer.Connected then
    DisconnectHTTPServer
  else
    ConnectHTTPServer;
end;

procedure TFormTrainCommander.ButtonWebserverConnectClick(Sender: TObject);
begin
  if LccWebsocketServer.Connected then
    DisconnectWebsocketServer
  else
    ConnectWebsocketServer;
end;

procedure TFormTrainCommander.ButtonManualConnectComPortClick(Sender: TObject);
var
  ComPortRec: TLccComPortRec;
begin
  if ComPort.Connected then
  begin
    ComPort.CloseComPort(nil);
  end else
  begin
    ComPortRec.Baud := 0;  // Keeps hint quiet
    FillChar(ComPortRec, SizeOf(ComPortRec), #0);
    ComPortRec.ComPort := ComboBoxComPorts.Items[ComboBoxComPorts.ItemIndex];
    ComPortRec.Baud := 9600;
    ComPortRec.StopBits := 8;
    ComPortRec.Parity := 'N';
    ComPort.OpenComPort(ComPortRec);
  end;
end;

procedure TFormTrainCommander.ButtonClearClick(Sender: TObject);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Clear;
  finally
    MemoLog.Lines.EndUpdate;
  end;

  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Clear;
  finally
    MemoComPort.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.ButtonEthernetConnectClick(Sender: TObject);
begin
if LccServer.Connected then
    DisconnectServer
  else
    ConnectServer;
end;

procedure TFormTrainCommander.ButtonTrainsClearClick(Sender: TObject);
var
  i: Integer;
  TrainNode: TLccTrainCanNode;
begin
  for i := 0 to NodeManager.GetNodeCount - 1 do
  begin
    if NodeManager.Node[i] is TLccTrainCanNode then
    begin
      TrainNode := NodeManager.Node[i] as TLccTrainCanNode;
      TrainNode.Logout;
      NodeManager.Nodes.Delete(i);
    end;
  end;
  ListViewTrains.Clear;
end;

function TFormTrainCommander.ConnectServer: Boolean;
var
  EthernetRec: TLccEthernetRec;
begin
  Result := False;
  EthernetRec.ErrorCode := 0;  // Keeps Hints quiet
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
  EthernetRec.ListenerIP := '127.0.0.1';
  EthernetRec.ListenerPort := 12021;
  LccServer.UseSynchronize := CheckBoxUseSyncronize.Checked;    // Do we call the timer to pump a receive buffer or do we let it call back through Syncronize on every message received
  EthernetRec.WebSocket := False;
  Result := LccServer.OpenConnection(EthernetRec) <> nil;
end;

procedure TFormTrainCommander.DisconnectServer;
begin
  LccServer.CloseConnection(nil);
end;

function TFormTrainCommander.ConnectWebsocketServer: Boolean;
var
  EthernetRec: TLccEthernetRec;
begin
  Result := False;
  EthernetRec.ErrorCode := 0;  // Keeps hint quiet
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
  EthernetRec.ListenerIP := '127.0.0.1';
  EthernetRec.ListenerPort := 12022;
  LccWebsocketServer.UseSynchronize := CheckBoxUseSyncronize.Checked;    // Do we call the timer to pump a receive buffer or do we let it call back through Syncronize on every message received
  EthernetRec.WebSocket := True;
  Result := LccWebsocketServer.OpenConnection(EthernetRec) <> nil;
end;

procedure TFormTrainCommander.DisconnectWebsocketServer;
begin
  LccWebsocketServer.CloseConnection(nil);
end;

function TFormTrainCommander.ConnectHTTPServer: Boolean;
var
  EthernetRec: TLccEthernetRec;
begin
  Result := False;
  EthernetRec.ErrorCode := 0;  // Keeps hint quiet
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
  EthernetRec.ListenerIP := '127.0.0.1';
  EthernetRec.ListenerPort := 12020;
  Result := LccHTTPServer.OpenConnection(EthernetRec) <> nil;
end;

procedure TFormTrainCommander.DisconnectHTTPServer;
begin
  LccHTTPServer.CloseConnection(nil);
end;

procedure TFormTrainCommander.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose; // Keep Hints quiet
  NodeManager.Clear;
  ComPort.CloseComPort(nil);
  LccServer.UnRegisterSiblingEthernetServer(nil);
  LccWebsocketServer.UnRegisterSiblingEthernetServer(nil);
  LccServer.CloseConnection(nil);
  LccWebsocketServer.CloseConnection(nil);
  LccHTTPServer.CloseConnection(nil);
end;

procedure TFormTrainCommander.FormCreate(Sender: TObject);
begin
  NodeManager := TLccCanNodeManager.Create(nil);
  NodeManager.OnLccNodeAliasIDChanged := @OnNodeManagerAliasIDChanged;
  NodeManager.OnLccNodeIDChanged := @OnNodeManagerIDChanged;
  NodeManager.OnLccMessageReceive := @OnNodeManagerReceiveMessage;
  NodeManager.OnLccMessageSend := @OnNodeManagerSendMessage;
  NodeManager.OnLccNodeLogin := @OnNodeManagerNodeLogin;
  NodeManager.OnLccNodeLogout := @OnNodeManagerNodeLogout;

  FLccServer := TLccEthernetServer.Create(nil, NodeManager);
  LccServer.OnConnectionStateChange := @OnCommandStationServerConnectionState;
  LccServer.OnErrorMessage := @OnCommandStationServerErrorMessage;
  LccServer.Gridconnect := True;
  LccServer.Hub := True;

  FLccWebsocketServer := TLccWebsocketServer.Create(nil, NodeManager);
  LccWebsocketServer.OnConnectionStateChange := @OnCommandStationWebsocketConnectionState;
  LccWebsocketServer.OnErrorMessage := @OnCommandStationWebsocketErrorMessage;
  LccWebsocketServer.Gridconnect := True;
  LccWebsocketServer.Hub := True;

  LccServer.RegisterSiblingEthernetServer(LccWebsocketServer);
  LccWebsocketServer.RegisterSiblingEthernetServer(LccServer);

  FLccHTTPServer := TLccHTTPServer.Create(nil, NodeManager);
  LccHTTPServer.OnConnectionStateChange := @OnCommandStationHTTPConnectionState;
  LccHTTPServer.OnErrorMessage := @OnCommandStationHTTPErrorMessage;

  ComPort := TLccComPort.Create(nil, NodeManager);
  ComPort.OnConnectionStateChange := @OnComPortConnectionStateChange;
  ComPort.OnErrorMessage := @OnComPortErrorMessage;
  ComPort.OnReceiveMessage := @OnComPortReceiveMessage;
  ComPort.RawData := True;

  FWorkerMessage := TLccMessage.Create;
end;

procedure TFormTrainCommander.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLccHTTPServer);
  FreeAndNil(FLccServer);
  FreeAndNil(FLccWebsocketServer);
  FreeAndNil(FNodeManager);   // after the servers are destroyed
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FComPort);
end;

procedure TFormTrainCommander.FormShow(Sender: TObject);
begin
  ComboBoxComPorts.Items.Delimiter := ';';
  ComboBoxComPorts.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  ComboBoxComPorts.ItemIndex := 0;
end;

procedure TFormTrainCommander.OnCommandStationServerConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        ButtonEthernetConnect.Enabled := False;
        StatusBarMain.Panels[0].Text := 'Connecting to Ethernet';
      end;
    ccsListenerConnected :
      begin
        ButtonEthernetConnect.Enabled := True;
        ButtonEthernetConnect.Caption := 'Disconnect Ethernet';
        StatusBarMain.Panels[0].Text := 'Ethernet: Command Station Connected at: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        if NodeManager.Nodes.Count = 0 then
          NodeManager.AddNodeByClass('', TLccCommandStationNode, True);
      end;
    ccsListenerDisconnecting :
      begin
        ButtonEthernetConnect.Caption := 'Command Station Disconnecting from Ethernet';
        if not LccWebsocketServer.Connected then
          NodeManager.Clear;
      end;
    ccsListenerDisconnected :
      begin
        ButtonEthernetConnect.Enabled := LccServer.Connected;
        ButtonEthernetConnect.Caption := 'Connect Ethernet';
        StatusBarMain.Panels[0].Text := 'Command Station Disconnected from Ethernet';
      end;
    ccsListenerClientConnecting :
      begin
      end;
    ccsListenerClientConnected :
      begin
         ListItem := ListviewConnections.Items.Add;
         Listitem.Caption := 'Throttle connected via Ethernet: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
         ListItem.ImageIndex := 3;
      end;
    ccsListenerClientDisconnecting :
      begin
      end;
    ccsListenerClientDisconnected :
      begin
        ListItem := ListviewConnections.FindCaption(0, 'Throttle connected via Ethernet: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort), True, True, True, True);
        if Assigned(ListItem) then
          ListviewConnections.Items.Delete(ListItem.Index);
      end;
  end;
end;

procedure TFormTrainCommander.OnCommandStationServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
 // ShowMessage('TCP Server: ' + EthernetRec.MessageStr);
end;

procedure TFormTrainCommander.OnCommandStationWebsocketConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        ButtonWebserverConnect.Enabled := False;
        StatusBarMain.Panels[1].Text := 'Connecting to Websocket';
      end;
    ccsListenerConnected :
      begin
        ButtonWebserverConnect.Enabled := True;
        ButtonWebserverConnect.Caption := 'Disconnect Websocket';
        StatusBarMain.Panels[1].Text := 'Websocket: Command Station Connected at: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        if NodeManager.Nodes.Count = 0 then
          NodeManager.AddNodeByClass('', TLccCommandStationNode, True);
      end;
    ccsListenerDisconnecting :
      begin
        ButtonWebserverConnect.Caption := 'Command Station Disconnecting from Websocket';
        if not LccServer.Connected then
          NodeManager.Clear;
      end;
    ccsListenerDisconnected :
      begin
        ButtonWebserverConnect.Enabled := LccWebsocketServer.Connected;
        ButtonWebserverConnect.Caption := 'Connect Websocket';
        StatusBarMain.Panels[1].Text := 'Command Station Disconnected from Websocket';
      end;
    ccsListenerClientConnecting :
      begin
      end;
    ccsListenerClientConnected :
      begin
         ListItem := ListviewConnections.Items.Add;
         Listitem.Caption := 'Throttle connected via Websocket: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
         ListItem.ImageIndex := 3;
      end;
    ccsListenerClientDisconnecting :
      begin
      end;
    ccsListenerClientDisconnected :
      begin
        ListItem := ListviewConnections.FindCaption(1, 'Throttle connected via Websocket: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort), True, True, True, True);
        if Assigned(ListItem) then
          ListviewConnections.Items.Delete(ListItem.Index);
      end;
  end;
end;

procedure TFormTrainCommander.OnCommandStationWebsocketErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
 //  ShowMessage('Websocket Server: ' + EthernetRec.MessageStr);
end;

procedure TFormTrainCommander.OnCommandStationHTTPConnectionState( Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        StatusBarMain.Panels[2].Text := 'HTTP Server Connecting';
        ButtonHTTPServer.Enabled := False;
      end;
    ccsListenerConnected :
      begin
        StatusBarMain.Panels[2].Text := 'HTTP Server Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        ButtonHTTPServer.Caption := 'HTTP Disconnect';
        ButtonHTTPServer.Enabled := True;
      end;
    ccsListenerDisconnecting :
      begin
        ButtonHTTPServer.Caption := 'Disconnecting from HTTP Server';
        ButtonHTTPServer.Enabled := False;
      end;
    ccsListenerDisconnected :
      begin
        StatusBarMain.Panels[2].Text := 'HTTP Server Disconnected';
        ButtonHTTPServer.Caption := 'HTTP Connect';
        ButtonHTTPServer.Enabled := True;
      end;
    ccsListenerClientConnecting :
      begin
      end;
    ccsListenerClientConnected :
      begin
         ListItem := ListviewConnections.Items.Add;
         Listitem.Caption := 'Device Connected HTTP: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
         ListItem.ImageIndex := 3;
      end;
    ccsListenerClientDisconnecting :
      begin
      end;
    ccsListenerClientDisconnected :
      begin
        ListItem := ListviewConnections.FindCaption(1, 'Device Connected HTTP: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort), True, True, True, True);
        if Assigned(ListItem) then
          ListviewConnections.Items.Delete(ListItem.Index);
      end;
  end;
end;

procedure TFormTrainCommander.OnCommandStationHTTPErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  EthernetRec := EthernetRec; // Keep Hints quiet
end;

procedure TFormTrainCommander.OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
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

  if LccSourceNode is TLccCommandStationNode then
  begin
    LccServer.AliasServer.NetworkRefreshed := True;
    LccWebsocketServer.AliasServer.NetworkRefreshed := True; // <<<<<<  NEED TO THINK ABOUT DEALING WITH BOTH IF I CAN START THEM A DIFFERENT TIMES

    WorkerMessage.LoadAME(LccSourceNode.NodeID, (LccSourceNode as TLccCanNode).AliasID, NULL_NODE_ID);

    LccServer.SendMessage(WorkerMessage);
    LccWebsocketServer.SendMessage(WorkerMessage);
  end;


  if LccSourceNode is TLccTrainCanNode then
  begin
    TrainNode := LccSourceNode as TLccTrainCanNode;

    TrainNode.OnSendMessageComPort := @OnComPortSendMessage;

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

procedure TFormTrainCommander.OnComPortConnectionStateChange(Sender: TObject;
  ComPortRec: TLccComPortRec);
begin
  case ComPortRec.ConnectionState of
    ccsPortConnecting :    StatusBarMain.Panels[2].Text := 'ComPort Connecting';
    ccsPortConnected :
      begin
        StatusBarMain.Panels[2].Text := 'ComPort: ' + ComPortRec.ComPort;
        ButtonManualConnectComPort.Caption := 'Close ComPort';
      end;
    ccsPortDisconnecting : StatusBarMain.Panels[2].Text := 'ComPort Disconnectiong';
    ccsPortDisconnected :
      begin
        ButtonManualConnectComPort.Caption := 'Open ComPort';
        StatusBarMain.Panels[2].Text := 'ComPort Disconnected';
      end;
  end;
end;

procedure TFormTrainCommander.OnComPortErrorMessage(Sender: TObject;
  ComPortRec: TLccComPortRec);
begin
  ShowMessage(ComPortRec.MessageStr);
end;

procedure TFormTrainCommander.OnComPortReceiveMessage(Sender: TObject;
  ComPortRec: TLccComPortRec);
begin
  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Add('R: ' + ComPortRec.MessageStr);
    MemoComPort.SelStart := Length(MemoComPort.Lines.Text);
  finally
    MemoComPort.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.OnComPortSendMessage(Sender: TObject; var GridConnectStyleMessage: string);
begin
  ComPort.SendMessageRawGridConnect(GridConnectStyleMessage);

  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Add('S: ' + GridConnectStyleMessage);
    MemoComPort.SelStart := Length(MemoComPort.Lines.Text);
  finally
    MemoComPort.Lines.EndUpdate;
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
      if CheckBoxDetailedLog.Checked then
        MemoLog.Lines.Add('R: ' + MessageToDetailedMessage(LccMessage))
      else
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
  LccWebsocketServer.SendMessage(LccMessage);

  if CheckBoxLogMessages.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      if CheckBoxDetailedLog.Checked then
        MemoLog.Lines.Add('S: ' + MessageToDetailedMessage(LccMessage))
      else
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

  LocalMsg := nil;
  List := LccWebsocketServer.IncomingGridConnect.LockList;
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
    LccWebsocketServer.IncomingGridConnect.UnlockList;
    FreeAndNil(LocalMsg);
  end;
end;

end.

