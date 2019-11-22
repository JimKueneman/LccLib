program LccNode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  crt,
  CustApp,
  lcc_nodemanager,
 // lcc_sdn_utilities,
  lcc_ethenetserver,
  lcc_ethernetclient,
  lcc_messages,
  lcc_utilities,
  lcc_detailed_logging
  ;

type

  { TLccConsoleApplication }

  TLccConsoleApplication = class(TCustomApplication)
  private
    FEthernetClient: TLccEthernetClient;
    FEthernetServer: TLccEthernetServer;
    FIsLoopback: Boolean;
    FIsServer: Boolean;
    FIsServerIP: string;
    FIsServerPort: string;
    FIsVeryVerbose: Boolean;
    FNodeManager: TLccNodeManager;
    FIsVerbose: Boolean;
    FTerminating: Boolean;
    procedure OnNodeManagerOnRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
    procedure OnEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnEthernetMessageReceive(Sender: TObject; EthernetRec: TLccEthernetRec);
  protected
    procedure DecodeCommandLineParameters;
    procedure PrintHelp;
    procedure PrintMsg(AMessage: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRun; override;
    procedure DoStartNode(Start: Boolean);
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property EthernetServer: TLccEthernetServer read FEthernetServer write FEthernetServer;
    property IsServerIP: string read FIsServerIP;
    property IsServerPort: string read FIsServerPort;
    property IsLoopback: Boolean read FIsLoopback;
    property IsServer: Boolean read FIsServer;
    property IsVerbose: Boolean read FIsVerbose;
    property IsVeryVerbose: Boolean read FIsVeryVerbose;
    property Terminating: Boolean read FTerminating;
  end;

var
  Application: TLccConsoleApplication;

procedure TLccConsoleApplication.PrintHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
  writeln('-h   : prints help [-h]');
  writeln('-s   : starts as a ethernet server [-s]');
  WriteLn('-I   : connect to IP address [-I 10.0.3.123]');
  WriteLn('-P    : connect to Port [-P 12021]');
  WriteLn('-v    : verbose output [-h]');
  WriteLn('-V   : Very Verbose output [-V]');
  writeln('-l   : Use the loopback IP address');

//  writeln('-C   : filename of the CDI file [-C MyCdi.xml]');
//  writeln('-i   : nodeID for node [-i 0x203456123456]');
//  writeln('-t   : filename of the node template file [-n nodetemplate.xml]');
//  writeln('-f   : filename of the node definition file that is created from the template file [-n nodedefinition.xml]');
//  writeln('-d   : define the number of datagram buffers available, use 1 to run against Olcb python test suite [-d 1]');
//  writeln('-H   : If the node is a server (-s) this switch enables the node to be a hub to relay messages to other connections');
// writeln('-T   : Use pure LCC TCP protocol instead of CAN Gridconnect over TCP');
end;

procedure TLccConsoleApplication.PrintMsg(AMessage: string);
begin
 if IsVerbose or IsVeryVerbose then
    WriteLn(AMessage)
end;

procedure TLccConsoleApplication.OnEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting :
      begin
        PrintMsg('Connecting......');
      end;
    ccsClientConnected :
      begin
        PrintMsg('Connected: ' + EthernetRec.ClientIP + ' ' + IntToStr(EthernetRec.ClientPort));
        DoStartNode(True)
      end;
    ccsClientDisconnecting :
      begin
        PrintMsg('DisConnecting......');
      end;
    ccsClientDisconnected :
      begin
        PrintMsg('DisConnected: ' + EthernetRec.ClientIP + ' ' + IntToStr(EthernetRec.ClientPort));
        DoStartNode(False);
      end;
  end;
end;

procedure TLccConsoleApplication.OnEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        PrintMsg('Connecting Listener......');
      end;
    ccsListenerConnected :
      begin
        PrintMsg('Connected Listener: ' + EthernetRec.ListenerIP + ' ' + IntToStr(EthernetRec.ListenerPort));
        DoStartNode(True);
      end;
    ccsListenerDisconnecting :
      begin
        PrintMsg('Disconnecting Listener......');
      end;
    ccsListenerDisconnected :
      begin
        PrintMsg('Disconnected Listener: ' + EthernetRec.ListenerIP + ' ' + IntToStr(EthernetRec.ListenerPort));
        DoStartNode(False);
      end;
    ccsListenerClientConnecting :
      begin
        PrintMsg('Client Connecting......');
      end;
    ccsListenerClientConnected :
      begin
        PrintMsg('Client Connected: ' + EthernetRec.ClientIP + ' ' + IntToStr(EthernetRec.ClientPort));
      end;
    ccsListenerClientDisconnecting :
      begin
        PrintMsg('Client Disconnecting......');
      end;
    ccsListenerClientDisconnected :
      begin
         PrintMsg('Client Disconnected: ' + EthernetRec.ClientIP + ' ' + IntToStr(EthernetRec.ClientPort));
      end;
  end;
end;

procedure TLccConsoleApplication.OnEthernetMessageReceive(Sender: TObject;
  EthernetRec: TLccEthernetRec);
begin
  if IsVeryVerbose then
    WriteLn('R: ' + MessageToDetailedMessage( EthernetRec.LccMessage.ConvertToGridConnectStr('')))
  else
  if IsVerbose then
    WriteLn('R: ' + EthernetRec.LccMessage.ConvertToGridConnectStr(''))
end;

procedure TLccConsoleApplication.OnNodeManagerOnRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
begin
  if Assigned(EthernetClient) then
  begin
    if EthernetClient.Connected then
      EthernetClient.SendMessage(LccMessage);
  end else
  if Assigned(EthernetServer) then
  begin
    if EthernetServer.Connected then
      EthernetServer.SendMessage(LccMessage);
  end;
  if IsVeryVerbose then
    WriteLn('S: ' + MessageToDetailedMessage(LccMessage.ConvertToGridConnectStr('')))
  else
  if IsVerbose then
    WriteLn('S: ' + LccMessage.ConvertToGridConnectStr(''))
end;

procedure TLccConsoleApplication.DecodeCommandLineParameters;
var
  ErrorMsg: string;
begin
  // quick check parameters
//  ErrorMsg := CheckOptions('h s C i t f d H L', 'help server cdi id templatefile configurationfile datagram hub loopback');

  ErrorMsg := CheckOptions('h s l, I, P, v, V', 'help server loopback serverip serverport verbose veryverbose');
  if ErrorMsg <> '' then begin
    WriteLn('Invalid parameter list');
    WriteLn(ErrorMsg);
    FTerminating := False;
    Exit;
  end;

  FIsServer := HasOption('s', 'server');
  FIsLoopback :=  HasOption('l', 'loopback');
  FIsVerbose := HasOption('v', 'verbose');
  FIsVeryVerbose := HasOption('V', 'veryverbose');
  if HasOption('I', 'serverip') then
  begin
    FIsServerIP := GetOptionValue('I', 'serverip');
  end;
  if HasOption('P', 'serverport') then
  begin
    FIsServerPort := GetOptionValue('P', 'serverport');
  end;
  if HasOption('h', 'help') then
  begin
    PrintHelp;
    Terminate;
  end;

  if IsServer then WriteLn('isServer');
  if IsLoopback then WriteLn('isLoopback');
  if IsVerbose then WriteLn('isVerbose');
  if IsVeryVerbose then WriteLn('isVeryVerbose');
end;

constructor TLccConsoleApplication.Create(TheOwner: TComponent);
begin
  inherited;
end;

destructor TLccConsoleApplication.Destroy;
begin
  inherited;
  if Assigned(EthernetClient) then
   EthernetClient.NodeManager := nil;
  if Assigned(EthernetServer) then
   EthernetServer.NodeManager := nil;
  FreeAndNil(FEthernetClient);
  FreeAndNil(FEthernetServer);
  FreeAndNil(FNodeManager);
end;

procedure TLccConsoleApplication.DoRun;
var
  EthernetRec: TLccEthernetRec;
begin
  inherited;
  Writeln('Starting....');
  DecodeCommandLineParameters;
  Writeln('Decoded....');

  NodeManager := TLccNodeManager.Create(nil);
  NodeManager.CreateRootNode;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Count := 10;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Enable := True;
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Count := 10;
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Enable := True;
  NodeManager.OnRequestMessageSend := @OnNodeManagerOnRequestMessageSend;

  NodeManager.RootNode.EventsConsumed.AutoGenerate.Count := 5;;
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Enable := True;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Count := 5;;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Enable := True;

  FillChar(EthernetRec, SizeOf(EthernetRec), #0);
  EthernetRec.AutoResolveIP := not IsLoopback;
  EthernetRec.ListenerIP := '127.0.0.1';  // Loopback for now
  EthernetRec.ListenerPort := 12021;      // standard port for now

  if IsServer then
  begin
    EthernetServer := TLccEthernetServer.Create(nil);
    EthernetServer.Gridconnect := True;
    EthernetServer.Hub := False;
    EthernetServer.NodeManager := NodeManager;
    EthernetServer.OnConnectionStateChange := @OnEthernetServerConnectionStateChange;
    EthernetServer.OnReceiveMessage := @OnEthernetMessageReceive;
    EthernetServer.OpenConnection(EthernetRec);
  end else
  begin
    EthernetClient := TLccEthernetClient.Create(nil);
    EthernetClient.Gridconnect := True;
    EthernetClient.NodeManager := NodeManager;
    EthernetClient.OnConnectionStateChange := @OnEthernetClientConnectionStateChange;
    EthernetClient.OnReceiveMessage := @OnEthernetMessageReceive;
    EthernetClient.OpenConnection(EthernetRec);
  end;

  while not Terminating do
  begin
    CheckSynchronize();
    Sleep(1);
    if KeyPressed then
      if ReadKey = 'q' then
        Break;
  end;
  DoStartNode(False);
end;

procedure TLccConsoleApplication.DoStartNode(Start: Boolean);
begin
  NodeManager.Enabled := Start;
  if NodeManager.Enabled then
    PrintMsg('Node Running')
  else
    PrintMsg('Node Stopped');
end;

begin
  { Add your program code here }
  Application := TLccConsoleApplication.Create(nil);
  Application.DoRun;
  Application.Free;
end.
