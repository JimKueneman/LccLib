program SimpleOlcbNode;

uses
  SysUtils,
  Classes,
  crt,
  lcc_nodemanager,
  lcc_sdn_utilities,
  lcc_ethenetserver,
  lcc_ethernetclient,
  lcc_messages
  ;

type

  { TLccConsoleApplication }

  TLccConsoleApplication = class
  private
    FEthernetClient: TLccEthernetClient;
    FEthernetServer: TLccEthernetServer;
    FNodeManager: TLccNodeManager;
    FTerminated: Boolean;
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
    property Terminated: Boolean read FTerminated;
  end;

var
  Application: TLccConsoleApplication;

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
         WriteLn(EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort));
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
  EthernetRec.ListenerPort := 12021;
 // EthernetClient.OpenConnection(EthernetRec);
  EthernetServer.OpenConnection(EthernetRec);
end;

destructor TLccConsoleApplication.Destroy;
begin
  EthernetClient.NodeManager := nil;
  FreeAndNil(FEthernetClient);
  FreeAndNil(FNodeManager);
end;

procedure TLccConsoleApplication.DoRun;
begin
  while not Terminated do
  begin
    CheckSynchronize();
  end;
end;

begin
  { Add your program code here }
  Application := TLccConsoleApplication.Create;
  Application.DoRun;
  Application.Free;
end.

