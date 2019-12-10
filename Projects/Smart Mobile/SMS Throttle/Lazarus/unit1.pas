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
  ComCtrls,
  lcc.node.manager,
  lcc.node.messages,
  lcc.defines,
  lcc.ethernet.server,
  lcc.ethernet.client;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private

  public
     EthernetServer: TLccEthernetServer;
     NodeManager: TLccNodeManager;
    procedure OnEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
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
    //JDK    NodeManager.RootNode := TLccOwnedNode.Create(nil);
    NodeManager.RootNode.Login(True, False);
    EthernetServer.CloseConnection(nil);
    Button1.Caption := 'Connect';
  end else
  begin
    NodeManager.RootNode.LogOut;
    EthernetServer.OpenConnection(EthernetRec);
    Button1.Caption := 'Disconnect';
  end;
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

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose;
  // There is a race of CloseSocket here... called twice in thread and in the CloseConnection call
  EthernetServer.CloseConnection(nil);
  NodeManager.RootNode.LogOut;
  EthernetServer.Free;
  NodeManager.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EthernetServer := TLccEthernetServer.Create(nil);
  EthernetServer.Gridconnect := True;
  NodeManager := TLccNodeManager.Create(nil);
end;

end.

