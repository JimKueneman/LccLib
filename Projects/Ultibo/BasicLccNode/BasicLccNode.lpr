program BasicLccNode;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  { Add additional units here }
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
    procedure OnNodeManagerOnRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoRun;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
  end;

var
  Application: TLccConsoleApplication;

procedure TLccConsoleApplication.OnNodeManagerOnRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
begin
  EthernetClient.SendMessage(LccMessage);
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

  FillChar(EthernetRec, SizeOf(EthernetRec), #0);
  EthernetRec.AutoResolveIP := True;
  EthernetRec.ListenerIP := '10.0.3.179';
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
  while True do
  begin

  end;
end;

begin
  { Add your program code here }
  Application := TLccConsoleApplication.Create;
  Application.DoRun;
  Application.Free;
end.

