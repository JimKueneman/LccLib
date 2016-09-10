program SimpleOlcbNode;

{$mode objfpc}{$H+}

uses
  {$IFDEF ULTIBO}
    RaspberryPi2,
    GlobalConfig,
    GlobalConst,
    GlobalTypes,
    Platform,
    Threads,
    SysUtils,
    Classes,
    Ultibo,
    Console,
    HTTP,
    WebStatus,
    uTFTP,
  {$ELSE}
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    SysUtils,
    Classes,
    crt,
  {$ENDIF}
  lcc_nodemanager,
  lcc_sdn_utilities,
  lcc_ethenetserver,
  lcc_ethernetclient,
  lcc_messages,
  lcc_utilities
  ;

type

  { TLccConsoleApplication }

  TLccConsoleApplication = class
  private
    FEthernetClient: TLccEthernetClient;
    FEthernetServer: TLccEthernetServer;
    FNodeManager: TLccNodeManager;
    FTerminated: Boolean;
    {$IFDEF ULTIBO}
    FHTTPListener: THTTPListener;
    {$ENDIF}
    procedure OnNodeManagerOnRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
    procedure OnEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoRun;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property EthernetServer: TLccEthernetServer read FEthernetServer write FEthernetServer;
    {$IFDEF ULTIBO}
    property HTTPListener: THTTPListener read FHTTPListener write FHTTPListener;
    {$ENDIF}
    property Terminated: Boolean read FTerminated;
  end;

var
  Application: TLccConsoleApplication;

{$IFDEF ULTIBO}
procedure FtpMsg(Sender : TObject; s : string);
begin
  ConsoleWriteLn('FTP: ' + s);
end;
{$ENDIF}

procedure TLccConsoleApplication.OnNodeManagerOnRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
begin
  if EthernetClient.Connected then
    EthernetClient.SendMessage(LccMessage);
  if EthernetServer.Connected then
    EthernetClient.SendMessage(LccMessage);
end;

procedure TLccConsoleApplication.OnEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
     ccsClientConnected :
       begin
         NodeManager.Enabled := True;
       end;
     ccsClientDisconnecting :
       begin
          NodeManager.Enabled := False;
          FTerminated := True;
       end;
  end;
end;

procedure TLccConsoleApplication.OnEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
     ccsListenerConnected :
       begin
          // Be the node
         {$IFDEF ULTIBO}
         ConsoleWriteLn('Starting the Node');
         {$ELSE}
         WriteLn('Starting the Node');
         WriteLn(EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort));
         {$ENDIF}
         NodeManager.Enabled := True;
       end;
     ccsListenerDisconnecting:
       begin
         NodeManager.Enabled := False;
         FTerminated := True;
       end;
     ccsListenerClientConnected :
       begin
       end;
     ccsListenerClientConnecting :
       begin
       end;
  end;
end;

constructor TLccConsoleApplication.Create;
var
  EthernetRec: TLccEthernetRec;
begin
  {$IFDEF ULTIBO}
  ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);
  ConsoleWriteLn('Welcome to the Mustangpeak Simple LCC Node with Ultibo');
  SetOnMsg(@FtpMsg);  // TFTP
  WaitForNetworkConnection(True);

  {Create and start HTTP Listener}
  HTTPListener := THTTPListener.Create;
  HTTPListener.Active := True;
  {Register Web Status}
  WebStatusRegister(HTTPListener,'','',True);
  {$ENDIF}
  NodeManager := TLccNodeManager.Create(nil);
  NodeManager.CreateRootNode;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Count := 10;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Enable := True;
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Count := 10;
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Enable := True;
  NodeManager.OnRequestMessageSend := @OnNodeManagerOnRequestMessageSend;

  EthernetClient := TLccEthernetClient.Create(nil);
  EthernetClient.Gridconnect := True;
  EthernetClient.NodeManager := NodeManager;
  EthernetClient.OnConnectionStateChange := @OnEthernetClientConnectionStateChange;

  EthernetServer := TLccEthernetServer.Create(nil);
  EthernetServer.Gridconnect := True;
  EthernetServer.Hub := False;
  EthernetServer.NodeManager := NodeManager;
  EthernetServer.OnConnectionStateChange := @OnEthernetServerConnectionStateChange;

  FillChar(EthernetRec, SizeOf(EthernetRec), #0);
  EthernetRec.AutoResolveIP := True;
  EthernetRec.ListenerIP := '10.0.3.178';
  EthernetRec.ListenerPort := 12021;
  EthernetClient.OpenConnection(EthernetRec);
 // EthernetServer.OpenConnection(EthernetRec);

end;

destructor TLccConsoleApplication.Destroy;
begin
  EthernetClient.NodeManager := nil;
  FreeAndNil(FEthernetClient);
  FreeAndNil(FNodeManager);
  {$IFDEF ULTIBO}
  WebStatusDeregister(HTTPListener, '');
  FreeAndNil(FHTTPListener);
  {$ENDIF}
end;

procedure TLccConsoleApplication.DoRun;
begin
  while not Terminated do
  begin
    CheckSynchronize();
    Sleep(1);
  end;
end;

begin
  { Add your program code here }
  Application := TLccConsoleApplication.Create;
  Application.DoRun;
  Application.Free;
end.
