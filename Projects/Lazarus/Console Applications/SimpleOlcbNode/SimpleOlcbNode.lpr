program SimpleOlcbNode;

uses
  SysUtils,
  Classes,
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
    FNodeManager: TLccNodeManager;
    FTerminated: Boolean;
    procedure OnNodeManagerOnRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
    procedure OnEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoRun;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property Terminated: Boolean read FTerminated;
  end;

var
  Application: TLccConsoleApplication;

procedure TLccConsoleApplication.OnNodeManagerOnRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
begin
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
  EthernetClient.Hub := False;
  EthernetClient.NodeManager := NodeManager;
  EthernetClient.OnConnectionStateChange := @OnEthernetClientConnectionStateChange;

  FillChar(EthernetRec, SizeOf(EthernetRec), #0);
  EthernetRec.AutoResolveIP := True;
  EthernetRec.ListenerIP := '10.0.3.194';
  EthernetRec.ListenerPort := 12021;
  EthernetClient.OpenConnection(EthernetRec);
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

