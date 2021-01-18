unit TrainCommanderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, lcc_ethernet_server, lcc_defines, lcc_node,
  lcc_node_manager, lcc_ethernet_client, lcc_node_messages,
  lcc_node_commandstation, lcc_node_controller, lcc_node_train,
  lcc_comport, synaser, lcc_common_classes, lcc_ethernet_common,
  lcc_ethernet_websocket, lcc_ethernet_http;

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
    CheckBox1: TCheckBox;
    CheckBoxDetailedLog: TCheckBox;
    CheckBoxUseSyncronize: TCheckBox;
    CheckBoxLogMessages: TCheckBox;
    CheckBoxLoopBackIP: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    ComboBoxComPorts: TComboBox;
    ImageListMain: TImageList;
    Label1: TLabel;
    LabelAliasServerCount: TLabel;
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
    procedure CheckBox1Change(Sender: TObject);
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

    function ConnectComPortServer: Boolean;
    procedure DisconnectComPortServer;

    // Callbacks from the Ethernet Server
    procedure OnCommandStationServerConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnCommandStationServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
    // Callbacks from the Websocket Server
    procedure OnCommandStationWebsocketConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnCommandStationWebsocketErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
    // Callbacks from the HTTP Server
    procedure OnCommandStationHTTPConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnCommandStationHTTPErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);

    // Callbacks from the ComPort
    procedure OnComPortConnectionStateChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnComPortErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnComPortReceiveMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnComPortSendMessage(Sender: TObject; var GridConnectStyleMessage: string);

    // Callbacks from the Node Manager
    procedure OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogout(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeAliasServerChange(Sender: TObject);

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
begin
  if ComPort.Connected then
    DisconnectComPortServer
  else
    ConnectComPortServer;
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
  if LccServer.ListenerConnected then
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

procedure TFormTrainCommander.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    Max_Allowed_Buffers := 1;
    NodeManager.AliasServerEnabled := False;
  end else
  begin
    Max_Allowed_Buffers := 2048;
    NodeManager.AliasServerEnabled := True;
  end;
end;

function TFormTrainCommander.ConnectServer: Boolean;
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  Result := False;
  LocalInfo := TLccEthernetConnectionInfo.Create;
  try
    LocalInfo.ErrorCode := 0;  // Keeps Hints quiet
    LocalInfo.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
    LocalInfo.ListenerIP := '127.0.0.1';
    LocalInfo.ListenerPort := 12021;
    LocalInfo.GridConnect := True;
    LocalInfo.Hub := True;
    LocalInfo.UseSyncronize := CheckBoxUseSyncronize.Checked;    // Do we call the timer to pump a receive buffer or do we let it call back through Syncronize on every message received
    Result := LccServer.OpenConnection(LocalInfo) <> nil;
  finally
  end;
end;

procedure TFormTrainCommander.DisconnectServer;
begin
  LccServer.CloseConnection(nil);
end;

function TFormTrainCommander.ConnectWebsocketServer: Boolean;
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  Result := False;
  LocalInfo := TLccEthernetConnectionInfo.Create;
  try
    LocalInfo.ErrorCode := 0;  // Keeps hint quiet
    LocalInfo.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
    LocalInfo.ListenerIP := '127.0.0.1';
    LocalInfo.ListenerPort := 12022;
    LocalInfo.GridConnect := True;
    LocalInfo.Hub := True;
    LocalInfo.UseSyncronize := CheckBoxUseSyncronize.Checked;    // Do we call the timer to pump a receive buffer or do we let it call back through Syncronize on every message received
    Result := LccWebsocketServer.OpenConnection(LocalInfo) <> nil;
  finally
    LocalInfo.Free;
  end;
end;

procedure TFormTrainCommander.DisconnectWebsocketServer;
begin
  LccWebsocketServer.CloseConnection(nil);
end;

function TFormTrainCommander.ConnectHTTPServer: Boolean;
var
    LocalInfo: TLccEthernetConnectionInfo;
begin
  Result := False;
  LocalInfo := TLccEthernetConnectionInfo.Create;
  try
    LocalInfo.ErrorCode := 0;  // Keeps hint quiet
    LocalInfo.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
    LocalInfo.ListenerIP := '127.0.0.1';
    LocalInfo.ListenerPort := 12020;
    Result := LccHTTPServer.OpenConnection(LocalInfo) <> nil;
  finally
    LocalInfo.Free;
  end;
end;

procedure TFormTrainCommander.DisconnectHTTPServer;
begin
  LccHTTPServer.CloseConnection(nil);
end;

function TFormTrainCommander.ConnectComPortServer: Boolean;
var
  LocalInfo: TLccComPortConnectionInfo;
begin
  Result := False;
  LocalInfo := TLccComPortConnectionInfo.Create;
  try
    LocalInfo.ComPort := ComboBoxComPorts.Items[ComboBoxComPorts.ItemIndex];
    LocalInfo.Baud := 9600;
    LocalInfo.StopBits := 8;
    LocalInfo.Parity := 'N';
    LocalInfo.GridConnect := True;
    Result := Assigned(ComPort.OpenConnection(LocalInfo))
  finally
    LocalInfo.Free;
  end;
end;

procedure TFormTrainCommander.DisconnectComPortServer;
begin
  ComPort.CloseConnection(nil);
end;

procedure TFormTrainCommander.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose; // Keep Hints quiet
  NodeManager.Clear;
  ComPort.CloseConnection(nil);
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
  NodeManager.AliasServer.OnAddMapping := @OnNodeAliasServerChange;
  NodeManager.AliasServer.OnDeleteMapping := @OnNodeAliasServerChange;

  FLccServer := TLccEthernetServer.Create(nil, NodeManager);
  LccServer.OnConnectionStateChange := @OnCommandStationServerConnectionState;
  LccServer.OnErrorMessage := @OnCommandStationServerErrorMessage;
//  LccServer.Hub := True;

  FLccWebsocketServer := TLccWebsocketServer.Create(nil, NodeManager);
  LccWebsocketServer.OnConnectionStateChange := @OnCommandStationWebsocketConnectionState;
  LccWebsocketServer.OnErrorMessage := @OnCommandStationWebsocketErrorMessage;
//  LccWebsocketServer.Hub := True;

  FLccHTTPServer := TLccHTTPServer.Create(nil, NodeManager); // OpenLCB messages do not move on this interface
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
  FreeAndNil(FComPort);
  FreeAndNil(FNodeManager);   // after the servers are destroyed
  FreeAndNil(FWorkerMessage);
end;

procedure TFormTrainCommander.FormShow(Sender: TObject);
begin
  ComboBoxComPorts.Items.Delimiter := ';';
  ComboBoxComPorts.Items.DelimitedText := StringReplace(GetSerialPortNames, ',', ';', [rfReplaceAll, rfIgnoreCase]);
  ComboBoxComPorts.ItemIndex := 0;
end;

procedure TFormTrainCommander.OnCommandStationServerConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
var
  ListItem: TListItem;
begin
  if Sender is TLccEthernetListener then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
          ButtonEthernetConnect.Enabled := False;
          StatusBarMain.Panels[0].Text := 'Connecting to Ethernet';
        end;
      lcsConnected :
        begin
          ButtonEthernetConnect.Enabled := True;
          ButtonEthernetConnect.Caption := 'Disconnect Ethernet';
          StatusBarMain.Panels[0].Text := 'Ethernet: Command Station Connected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort);
          if NodeManager.Nodes.Count = 0 then
            NodeManager.AddNodeByClass('', TLccCommandStationNode, True);
        end;
      lcsDisconnecting :
        begin
          ButtonEthernetConnect.Enabled := False;
          ButtonEthernetConnect.Caption := 'Command Station Disconnecting from Ethernet';
          if not LccWebsocketServer.Connected then
            NodeManager.Clear;
        end;
      lcsDisconnected :
        begin
          ButtonEthernetConnect.Enabled := True;
          ButtonEthernetConnect.Caption := 'Connect Ethernet';
          StatusBarMain.Panels[0].Text := 'Command Station Disconnected from Ethernet';
        end;
    end;

  end else
  if Sender is TLccConnectionThread then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
        end;
      lcsConnected :
        begin
           ListItem := ListviewConnections.Items.Add;
           Listitem.Caption := 'Throttle connected via Ethernet: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
           ListItem.ImageIndex := 3;
        end;
      lcsDisconnecting :
        begin
        end;
      lcsDisconnected :
        begin
          ListItem := ListviewConnections.FindCaption(0, 'Throttle connected via Ethernet: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort), True, True, True, True);
          if Assigned(ListItem) then
            ListviewConnections.Items.Delete(ListItem.Index);
        end;
    end;
  end
end;

procedure TFormTrainCommander.OnCommandStationServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  ShowMessage('TCP Server: ' + Info.MessageStr);

end;

procedure TFormTrainCommander.OnCommandStationWebsocketConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
var
  ListItem: TListItem;
begin
  if Sender is TLccEthernetListener then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
          ButtonWebserverConnect.Enabled := False;
          StatusBarMain.Panels[1].Text := 'Connecting to Websocket';
        end;
      lcsConnected :
        begin
          ButtonWebserverConnect.Enabled := True;
          ButtonWebserverConnect.Caption := 'Disconnect Websocket';
          StatusBarMain.Panels[1].Text := 'Websocket: Command Station Connected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort);
          if NodeManager.Nodes.Count = 0 then
            NodeManager.AddNodeByClass('', TLccCommandStationNode, True);
          end;
      lcsDisconnecting :
        begin
          ButtonWebserverConnect.Enabled := False;
          ButtonWebserverConnect.Caption := 'Connect Websocket';
          StatusBarMain.Panels[1].Text := 'Command Station Disconnected from Websocket';
          if not LccServer.Connected then
            NodeManager.Clear;
        end;
      lcsDisconnected :
        begin
          ButtonWebserverConnect.Enabled := True;
          ButtonWebserverConnect.Caption := 'Connect Ethernet';
          StatusBarMain.Panels[1].Text := 'Command Station Disconnected from Websocket';
        end;
    end;

  end else
  if Sender is TLccConnectionThread then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
        end;
      lcsConnected :
        begin
          ListItem := ListviewConnections.Items.Add;
          Listitem.Caption := 'Throttle connected via Websocket: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
          ListItem.ImageIndex := 3;
        end;
      lcsDisconnecting :
        begin
        end;
      lcsDisconnected :
        begin
          ListItem := ListviewConnections.FindCaption(1, 'Throttle connected via Websocket: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort), True, True, True, True);
          if Assigned(ListItem) then
            ListviewConnections.Items.Delete(ListItem.Index);
        end;
    end;
  end;
end;

procedure TFormTrainCommander.OnCommandStationWebsocketErrorMessage(
  Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
 //  ShowMessage('Websocket Server: ' + EthernetRec.MessageStr);
  Info := Info;
end;

procedure TFormTrainCommander.OnCommandStationHTTPConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
var
  ListItem: TListItem;
begin
  if Sender is TLccEthernetListener then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
          StatusBarMain.Panels[2].Text := 'HTTP Server Connecting';
          ButtonHTTPServer.Enabled := False;
        end;
      lcsConnected :
        begin
          StatusBarMain.Panels[2].Text := 'HTTP Server Connected: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort);
          ButtonHTTPServer.Caption := 'HTTP Disconnect';
          ButtonHTTPServer.Enabled := True;
        end;
      lcsDisconnecting :
        begin
          ButtonHTTPServer.Caption := 'Disconnecting from HTTP Server';
          ButtonHTTPServer.Enabled := False;
        end;
      lcsDisconnected :
        begin
          StatusBarMain.Panels[2].Text := 'HTTP Server Disconnected';
          ButtonHTTPServer.Caption := 'HTTP Connect';
          ButtonHTTPServer.Enabled := True;
        end;
    end;

  end else
  if Sender is TLccConnectionThread then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
        end;
      lcsConnected :
        begin
          ListItem := ListviewConnections.Items.Add;
          Listitem.Caption := 'Device connected HTTP: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
          ListItem.ImageIndex := 3;
        end;
      lcsDisconnecting :
        begin
        end;
      lcsDisconnected :
        begin
          ListItem := ListviewConnections.FindCaption(1, 'Device connected HTTP: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort), True, True, True, True);
          if Assigned(ListItem) then
            ListviewConnections.Items.Delete(ListItem.Index);
        end;
    end;
  end
end;

procedure TFormTrainCommander.OnCommandStationHTTPErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  ShowMessage('HTTP Server: ' + Info.MessageStr);
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

procedure TFormTrainCommander.OnComPortConnectionStateChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  if Sender is TLccConnectionThread then
  begin
    case (Info as TLccComPortConnectionInfo).ConnectionState of
      lcsConnecting :    StatusBarMain.Panels[3].Text := 'ComPort Connecting';
      lcsConnected :
        begin
          StatusBarMain.Panels[3].Text := 'ComPort: ' + (Info as TLccComPortConnectionInfo).ComPort;
          ButtonManualConnectComPort.Caption := 'Close ComPort';
        end;
      lcsDisConnecting : StatusBarMain.Panels[3].Text := 'ComPort Disconnectiong';
      lcsDisconnected :
        begin
          ButtonManualConnectComPort.Caption := 'Open ComPort';
          StatusBarMain.Panels[3].Text := 'ComPort Disconnected';
        end;
    end;
  end;
end;

procedure TFormTrainCommander.OnComPortErrorMessage(Sender: TObject;
  Info: TLccHardwareConnectionInfo);
begin
  ShowMessage(Info.MessageStr);
end;

procedure TFormTrainCommander.OnComPortReceiveMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  MemoComPort.Lines.BeginUpdate;
  try
    MemoComPort.Lines.Add('R: ' + Info.MessageStr);
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

procedure TFormTrainCommander.OnNodeAliasServerChange(Sender: TObject);
begin
  LabelAliasServerCount.Caption := IntToStr(NodeManager.AliasServer.Count);
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

