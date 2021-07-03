program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  , crt,
  synaser,
  lcc_defines,
  lcc_node_messages,
  lcc_math_float16,
  lcc_node,
  lcc_node_manager,
  lcc_utilities,
  lcc_node_train,
  lcc_node_commandstation,
  lcc_node_controller,
  lcc_comport,
  lcc_common_classes,
  lcc_ethernet_common,
  lcc_ethernet_websocket,
  lcc_ethernet_http,
  lcc_ethernet_server,
  lcc_ethernet_client
  ;

type

  { TLccCommandStationApplication }

  TLccCommandStationApplication = class(TCustomApplication)
  private
    FHTTPLinks: TStringList;
    FIsDebugLog: Boolean;
    FIsGridConnect: Boolean;
    FLccEthernetServer: TLccEthernetServer;
    FLccHTTPServer: TLccHTTPServer;
    FLccWebsocketServer: TLccWebsocketServer;
    FIsLoopBackIP: Boolean;
    FNodeManager: TLccNodeManager;
    FThrottleLinks: TStringList;
    FWebsocketLinks: TStringList;
    function GetLccEthernetServer: TLccEthernetServer;
    function GetLccHTTPServer: TLccHTTPServer;
    function GetLccWebsocketServer: TLccWebsocketServer;
    function GetNodeManager: TLccNodeManager;
  protected
    procedure DoRun; override;

    // Callbacks from the Ethernet Server
    procedure OnCommandStationServerConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnCommandStationServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
    // Callbacks from the Websocket Server
    procedure OnCommandStationWebsocketConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnCommandStationWebsocketErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
    // Callbacks from the HTTP Server
    procedure OnCommandStationHTTPConnectionState(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnCommandStationHTTPErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);

    // Callbacks from the Node Manager
    procedure OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogout(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeLogin(Sender: TObject; LccSourceNode: TLccNode);

    property ThrottleLinks: TStringList read FThrottleLinks write FThrottleLinks;
    property WebsocketLinks: TStringList read FWebsocketLinks write FWebsocketLinks;
    property HTTPLinks: TStringList read FHTTPLinks write FHTTPLinks;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property NodeManager: TLccNodeManager read GetNodeManager write FNodeManager;
    property LccEthernetServer: TLccEthernetServer read GetLccEthernetServer write FLccEthernetServer;
    property LccWebsocketServer: TLccWebsocketServer read GetLccWebsocketServer write FLccWebsocketServer;
    property LccHTTPServer: TLccHTTPServer read GetLccHTTPServer write FLccHTTPServer;
    property IsLoopBackIP: Boolean read FIsLoopBackIP;
    property IsGridConnect: Boolean read FIsGridConnect;
    property IsDebugLog: Boolean read FIsDebugLog;
  end;

{ TLccCommandStationApplication }

function TLccCommandStationApplication.GetNodeManager: TLccNodeManager;
begin
  if not Assigned(FNodeManager) then
    FNodeManager := TLccNodeManager.Create(nil, IsGridConnect);
  Result := FNodeManager;
end;

function TLccCommandStationApplication.GetLccEthernetServer: TLccEthernetServer;
begin
  if not Assigned(FLccEthernetServer) then
    FLccEthernetServer := TLccEthernetServer.Create(Self, NodeManager);
  Result := FLccEthernetServer;
end;

function TLccCommandStationApplication.GetLccHTTPServer: TLccHTTPServer;
begin
  if not Assigned(FLccHTTPServer) then
    FLccHTTPServer := TLccHTTPServer.Create(Self, NodeManager);
  Result := FLccHTTPServer;
end;

function TLccCommandStationApplication.GetLccWebsocketServer: TLccWebsocketServer;
begin
  if not Assigned(FLccWebsocketServer) then
    FLccWebsocketServer := TLccWebsocketServer.Create(Self, NodeManager);
  Result := FLccWebsocketServer;
end;

procedure TLccCommandStationApplication.DoRun;
var
  ErrorMsg: String;
  Running: Boolean;
  ConnectionInfo: TLccEthernetConnectionInfo;
  KeyPressedChar: Char;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  FThrottleLinks := TStringList.Create;
  ThrottleLinks.Duplicates := dupIgnore;
  FWebsocketLinks := TStringList.Create;
  WebsocketLinks.Duplicates := dupIgnore;
  FHTTPLinks := TStringList.Create;
  HTTPLinks.Duplicates := dupIgnore;

  NodeManager.OnLccMessageReceive := @OnNodeManagerReceiveMessage;
  NodeManager.OnLccMessageSend := @OnNodeManagerSendMessage;
  NodeManager.OnLccNodeAliasIDChanged := @OnNodeManagerAliasIDChanged;
  NodeManager.OnLccNodeIDChanged := @OnNodeManagerIDChanged;
  NodeManager.OnLccNodeLogin := @OnNodeManagerNodeLogin;
  NodeManager.OnLccNodeLogout := @OnNodeManagerNodeLogout;

  LccEthernetServer.OnConnectionStateChange := @OnCommandStationServerConnectionState;
  LccEthernetServer.OnErrorMessage := @OnCommandStationServerErrorMessage;

  WriteLn('Connecting to Ethernet');
  ConnectionInfo := TLccEthernetConnectionInfo.Create;
  try
    ConnectionInfo.AutoResolveIP := not IsLoopBackIP;
    ConnectionInfo.GridConnect := IsGridConnect;
    ConnectionInfo.ListenerPort := 12021;
    ConnectionInfo.ListenerIP := '127.0.0.1';
    ConnectionInfo.Hub := True;
    LccEthernetServer.OpenConnection(ConnectionInfo);
  finally
    ConnectionInfo.Free;
  end;

  while not LccEthernetServer.ListenerConnected do
  begin
    CheckSynchronize(0);
    Sleep(100);
  end;

  LccWebsocketServer.OnConnectionStateChange := @OnCommandStationWebsocketConnectionState;
  LccWebsocketServer.OnErrorMessage := @OnCommandStationWebsocketErrorMessage;

  WriteLn('Connecting to Websocket Server');
  ConnectionInfo := TLccEthernetConnectionInfo.Create;
  try
    ConnectionInfo.AutoResolveIP := not IsLoopBackIP;
    ConnectionInfo.GridConnect := IsGridConnect;
    ConnectionInfo.ListenerPort := 12022;
    ConnectionInfo.ListenerIP := '127.0.0.1';
    ConnectionInfo.Hub := True;
    LccWebsocketServer.OpenConnection(ConnectionInfo);
  finally
    ConnectionInfo.Free;
  end;

  while not LccWebsocketServer.ListenerConnected do
  begin
    CheckSynchronize(0);
    Sleep(100);
  end;

  LccHTTPServer.OnConnectionStateChange := @OnCommandStationHTTPConnectionState;
  LccHTTPServer.OnErrorMessage := @OnCommandStationHTTPErrorMessage;

  WriteLn('Connecting to HTTP Server');
  ConnectionInfo := TLccEthernetConnectionInfo.Create;
  try
    ConnectionInfo.AutoResolveIP := not IsLoopBackIP;
    ConnectionInfo.GridConnect := IsGridConnect;
    ConnectionInfo.ListenerPort := 12020;
    ConnectionInfo.ListenerIP := '127.0.0.1';
    ConnectionInfo.Hub := True;
    LccHTTPServer.OpenConnection(ConnectionInfo);
  finally
    ConnectionInfo.Free;
  end;

  while not LccHTTPServer.ListenerConnected do
  begin
    CheckSynchronize(0);
    Sleep(100);
  end;

  WriteLn('Starting NodeManager');
  if NodeManager.Nodes.Count > 0 then
  begin
    WriteLn('Logging in Command Station Node');
    NodeManager.Node[0].Login(NULL_NODE_ID);
  end else
    WriteLn('Login of the Command Station Node Failed:  No Node Found');

  WriteLn('Running');
  { add your program here }
  Running := True;
  while Running do
  begin
    CheckSynchronize();  // Pump the timers
      if KeyPressed then
      begin
        KeyPressedChar := ReadKey;
        Running := LowerCase(KeyPressedChar) <> 'q';
        if KeyPressedChar = 'l' then
        begin
          WriteLn('Throttle Links: ' + IntToStr(ThrottleLinks.Count));
          WriteLn('Websocket Links: ' + IntToStr(WebsocketLinks.Count));
          WriteLn('HTTP Links: ' + IntToStr(HTTPLinks.Count));
        end;
      end;
  end;

  NodeManager.Clear;

  WriteLn('Closing HTTP Connection');
  LccHTTPServer.CloseConnection(nil);
  while LccHTTPServer.ListenerConnected do
  begin
    CheckSynchronize(0);
    Sleep(100);
  end;

  WriteLn('Closing Websocket Connection');
  LccWebsocketServer.CloseConnection(nil);
  while LccWebsocketServer.ListenerConnected do
  begin
    CheckSynchronize(0);
    Sleep(100);
  end;

  WriteLn('Closing Ethernet Connection');
  LccEthernetServer.CloseConnection(nil);
  while LccEthernetServer.ListenerConnected do
  begin
    CheckSynchronize(0);
    Sleep(100);
  end;

  FThrottleLinks.Free;
  FWebsocketLinks.Free;
  FHTTPLinks.Free;

  WriteLn('Terminated');
  // stop program loop
  Terminate;
end;

procedure TLccCommandStationApplication.OnCommandStationServerConnectionState(
  Sender: TObject; Info: TLccHardwareConnectionInfo);
var
  Index: Integer;
begin
  if Sender is TLccEthernetListener then
  begin
    case Info.ConnectionState of
      lcsConnecting    : WriteLn('Ethernet: Command Station Connecting');
      lcsConnected     : begin
                           WriteLn('Ethernet: Command Station Connected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort));
                           if NodeManager.Nodes.Count = 0 then
                             NodeManager.AddNodeByClass('', TLccCommandStationNode, False);
                         end;
      lcsDisconnecting : WriteLn('Ethernet: Command Station Disconnecting');
      lcsDisconnected  : begin
                           WriteLn('Ethernet: Command Station Disconnected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort));
                           if not LccWebsocketServer.Connected then
                             NodeManager.Clear;
                         end;
    end;
  end else
  if Sender is TLccConnectionThread then
  begin
    case Info.ConnectionState of
      lcsConnecting    : WriteLn('Ethernet: Throttle is Connecting');
      lcsConnected     : begin
                           WriteLn('Ethernet: Throttle Connected: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           ThrottleLinks.Add((Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                         end;
      lcsDisconnecting : WriteLn('Ethernet: Throttle is Disconnecting');
      lcsDisconnected  : begin
                           WriteLn('Ethernet: Throttle Disconnected: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           Index := ThrottleLinks.IndexOf((Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           if Index > -1 then
                             ThrottleLinks.Delete(Index);
                         end;
    end;
  end
end;

procedure TLccCommandStationApplication.OnCommandStationServerErrorMessage(
  Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  WriteLn('Ethernet: Connection Error Code: ' + IntToStr(Info.ErrorCode) + ', ' + Info.MessageStr);
end;

procedure TLccCommandStationApplication.OnCommandStationWebsocketConnectionState
  (Sender: TObject; Info: TLccHardwareConnectionInfo);
var
  Index: Integer;
begin
  if Sender is TLccEthernetListener then
  begin
    case Info.ConnectionState of
      lcsConnecting    : WriteLn('Websocket: Command Station Connecting');
      lcsConnected     : begin
                           WriteLn('Websocket: Command Station Connected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort));
                           if NodeManager.Nodes.Count = 0 then
                             NodeManager.AddNodeByClass('', TLccCommandStationNode, False);
                         end;
      lcsDisconnecting : WriteLn('Websocket: Command Station Disconnecting');
      lcsDisconnected  : begin
                           WriteLn('Websocket: Command Station Disconnected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort));
                           if not LccEthernetServer.Connected then
                             NodeManager.Clear;
                         end;
    end;

  end else
  if Sender is TLccConnectionThread then
  begin
    case Info.ConnectionState of
      lcsConnecting    : WriteLn('Websocket: Throttle is Connecting');
      lcsConnected     : begin
                           WriteLn('Websocket: Throttle Connected: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           WebsocketLinks.Add((Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                         end;
      lcsDisconnecting : WriteLn('Websocket: Throttle is Disconnecting');
      lcsDisconnected  : begin
                           WriteLn('Websocket: Throttle Disconnected: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           Index := WebsocketLinks.IndexOf((Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           if Index > -1 then
                             WebsocketLinks.Delete(Index);
                         end;
    end;
  end;

end;

procedure TLccCommandStationApplication.OnCommandStationWebsocketErrorMessage(
  Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  WriteLn('Websocket: Connection Error Code: ' + IntToStr(Info.ErrorCode) + ', ' + Info.MessageStr);
end;

procedure TLccCommandStationApplication.OnCommandStationHTTPConnectionState(
  Sender: TObject; Info: TLccHardwareConnectionInfo);
var
  Index: Integer;
begin
  if Sender is TLccEthernetListener then
  begin
    case Info.ConnectionState of
      lcsConnecting    : WriteLn('HTTP: Command Station Connecting');
      lcsConnected     : WriteLn('HTTP: Command Station Connected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort));
      lcsDisconnecting : WriteLn('HTTP: Command Station Disconnecting');
      lcsDisconnected  : WriteLn('HTTP: Command Station Disconnected at: ' + (Info as TLccEthernetConnectionInfo).ListenerIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ListenerPort));
    end;
  end else
  if Sender is TLccConnectionThread then
  begin
    case Info.ConnectionState of
      lcsConnecting    : WriteLn('HTTP: Throttle is Connecting');
      lcsConnected     : begin
                           WriteLn('HTTP: Throttle Connected: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           HTTPLinks.Add((Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                         end;
      lcsDisconnecting : WriteLn('HTTP: Throttle is Disconnecting');
      lcsDisconnected  : begin
                           WriteLn('HTTP: Throttle Disconnected: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           Index := HTTPLinks.IndexOf((Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort));
                           if Index > -1 then
                             HTTPLinks.Delete(Index);
                         end;
    end;
  end
end;

procedure TLccCommandStationApplication.OnCommandStationHTTPErrorMessage(
  Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  WriteLn('HTTP: Connection Error Code: ' + IntToStr(Info.ErrorCode) + ', ' + Info.MessageStr);
end;

procedure TLccCommandStationApplication.OnNodeManagerSendMessage(
  Sender: TObject; LccMessage: TLccMessage);
begin
  if IsDebugLog then
    WriteLn('S: ' + MessageToDetailedMessage(LccMessage))
  else
    WriteLn('S: ' + LccMessage.ConvertToGridConnectStr('', False));
end;

procedure TLccCommandStationApplication.OnNodeManagerReceiveMessage(
  Sender: TObject; LccMessage: TLccMessage);
begin
  if IsDebugLog then
    WriteLn('R: ' + MessageToDetailedMessage(LccMessage))
  else
    WriteLn('R: ' + LccMessage.ConvertToGridConnectStr('', False));
end;

procedure TLccCommandStationApplication.OnNodeManagerAliasIDChanged(
  Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode is TLccCommandStationNode then
    WriteLn('AliasID Change: ' +LccSourceNode.AliasIDStr);
end;

procedure TLccCommandStationApplication.OnNodeManagerIDChanged(Sender: TObject;
  LccSourceNode: TLccNode);
begin
  if LccSourceNode is TLccCommandStationNode then
    WriteLn('Node ID Change: ' + LccSourceNode.NodeIDStr);
end;

procedure TLccCommandStationApplication.OnNodeManagerNodeLogout(
  Sender: TObject; LccSourceNode: TLccNode);
begin

end;

procedure TLccCommandStationApplication.OnNodeManagerNodeLogin(Sender: TObject;
  LccSourceNode: TLccNode);
begin

end;

constructor TLccCommandStationApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;

  FIsLoopBackIP := False;
  FIsGridConnect := True;
end;

destructor TLccCommandStationApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TLccCommandStationApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TLccCommandStationApplication;
begin
  Application:=TLccCommandStationApplication.Create(nil);
  Application.Title:='MustangpeakCommandStation';
  Application.Run;
  Application.Free;
end.

