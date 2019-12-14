unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ComCtrls, SynEdit,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_defines,
  lcc_ethernet_server,
  lcc_ethernet_client;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private

  public
    EthernetServer: TLccEthernetServer;
    NodeManager: TLccCanNodeManager;
    procedure OnEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure SendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetServer.OnConnectionStateChange := @OnEthernetConnectionChange;
  EthernetRec.ListenerIP := '127.0.0.1';
  EthernetRec.ListenerPort := 12021;
  if EthernetServer.Connected then
  begin
    NodeManager.LogoutAll;
    EthernetServer.CloseConnection(nil);
    Button1.Caption := 'Connect';
  end else
  begin
    EthernetServer.OpenConnection(EthernetRec);
    Button1.Caption := 'Disconnect';
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  CanNode: TLccCanNode;
begin
  if NodeManager.Nodes.Count = 0 then
  begin
    CanNode := NodeManager.AddNode;
    CanNode.Login(NULL_NODE_ID); // Create our own ID
  end else
    NodeManager.Clear;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose;
  NodeManager.Free;
  // There is a race of CloseSocket here... called twice in thread and in the CloseConnection call
  EthernetServer.CloseConnection(nil);
 // NodeManager.RootNode.LogOut;
  EthernetServer.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EthernetServer := TLccEthernetServer.Create(nil);
  EthernetServer.Gridconnect := True;
  NodeManager := TLccCanNodeManager.Create(nil);
  NodeManager.OnLccMessageSend := @SendMessage;
  NodeManager.OnLccMessageReceive := @ReceiveMessage;
  EthernetServer.NodeManager := NodeManager;
  SynEdit1.Clear;
end;

procedure TForm1.OnEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting:          StatusBar1.Panels[0].Text := 'Server Connecting: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    ccsListenerConnected:           StatusBar1.Panels[0].Text := 'Server Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    ccsListenerDisconnecting:       StatusBar1.Panels[0].Text := 'Server Disconnecting: ';
    ccsListenerDisconnected:        StatusBar1.Panels[0].Text := 'Server Disconnected: ';
    ccsListenerClientConnecting:    StatusBar1.Panels[1].Text := 'Client Connecting: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
    ccsListenerClientConnected:     StatusBar1.Panels[1].Text := 'Client Connected: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
    ccsListenerClientDisconnecting: StatusBar1.Panels[1].Text := 'Client Disconnecting: ';
    ccsListenerClientDisconnected:  StatusBar1.Panels[1].Text := 'Client Disconnected: ';
  end;
end;

procedure TForm1.ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  SynEdit1.BeginUpdate(False);
  try
    SynEdit1.Lines.Add('R: ' + LccMessage.ConvertToGridConnectStr(''));
    SynEdit1.EndUpdate;
  finally
  end;
end;

procedure TForm1.SendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  EthernetServer.SendMessage(LccMessage);

  SynEdit1.BeginUpdate(False);
  try
    SynEdit1.Lines.Add('S: ' + LccMessage.ConvertToGridConnectStr(''));
    SynEdit1.EndUpdate;
  finally
  end;
end;

end.

