unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, lcc_websocketserver, lcc_ethernetclient, lcc_messages,
  lcc_defines, lcc_mdns_singleshot,blcksock, synsock;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonTestMulitCast: TButton;
    ButtonLccConnect: TButton;
    ButtonLccDisconnect: TButton;
    Button3: TButton;
    ButtonLccSendVerifyNode: TButton;
    ButtonMDNSConnect: TButton;
    ButtonMDNSDisconnect: TButton;
    ButtonMDNSSend: TButton;
    LccWebSocketServer1: TLccWebSocketServer;
    Lcc_mDNS_SingleShotServer1: TLcc_mDNS_SinglShotServer;
    ListView1: TListView;
    Memo1: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    procedure ButtonLccConnectClick(Sender: TObject);
    procedure ButtonLccDisconnectClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ButtonLccSendVerifyNodeClick(Sender: TObject);
    procedure ButtonMDNSConnectClick(Sender: TObject);
    procedure ButtonMDNSDisconnectClick(Sender: TObject);
    procedure ButtonMDNSSendClick(Sender: TObject);
    procedure ButtonTestMulitCastClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LccWebSocketServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccWebSocketServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccWebSocketServer1ReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure Lcc_mDNS_SingleShotServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure Lcc_mDNS_SingleShotServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure Lcc_mDNS_SingleShotServer1Question(AQuestion: TmDNSQuestionRec);
  private
    FConnected: Boolean;
    FmDNSConnected: Boolean;
    FWorkerMessage: TLccMessage;
    { private declarations }
  public
    { public declarations }
    property Connected: Boolean read FConnected write FConnected;
    property mDNSConnected: Boolean read FmDNSConnected write FmDNSConnected;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure multicasttest;
var
  sndsock:TUDPBlockSocket;
  rcvsock:TUDPBlockSocket;
  buf:string;
begin
  sndsock:=TUDPBlockSocket.Create;
  rcvsock:=TUDPBlockSocket.Create;
  try
    rcvsock.createsocket;
    rcvsock.Bind('0.0.0.0','22401');
    rcvsock.AddMulticast('234.5.6.7');
    Assert(rcvsock.LastError = 0);

    sndsock.createsocket;
    sndsock.Bind('0.0.0.0','0');
    sndsock.MulticastTTL := 1;
    sndsock.connect('234.5.6.7','22401');
    Assert(sndsock.LastError = 0);

    sndsock.SendString('Test Payload');
    Assert(sndsock.LastError = 0);
    buf:=rcvsock.RecvPacket(1000);
    showmessage(buf);

    sndsock.CloseSocket;
    rcvsock.CloseSocket;
  finally
    sndsock.free;
    rcvsock.free;
  end;
end;

{ TForm1 }

procedure TForm1.ButtonLccConnectClick(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, SizeOf(TLccEthernetRec), #0);
  EthernetRec.AutoResolveIP := False;
  EthernetRec.ListenerIP := '127.0.0.1';   // Loopback
  EthernetRec.ListenerPort := 12021;
  LccWebSocketServer1.OpenConnection(EthernetRec);
end;

procedure TForm1.ButtonLccDisconnectClick(Sender: TObject);
begin
  if Connected then
    LccWebSocketServer1.CloseConnection(nil);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.BeginUpdate;
  Memo1.Clear;
  Memo1.Lines.EndUpdate;
end;

procedure TForm1.ButtonLccSendVerifyNodeClick(Sender: TObject);
begin
  WorkerMessage.LoadVerifyNodeID(NULL_NODE_ID, $0ABC);
  LccWebSocketServer1.SendMessage(WorkerMessage);
  LccWebSocketServer1.SendMessage(WorkerMessage);
  LccWebSocketServer1.SendMessage(WorkerMessage);
  LccWebSocketServer1.SendMessage(WorkerMessage);
end;

procedure TForm1.ButtonMDNSConnectClick(Sender: TObject);
begin
  Lcc_mDNS_SingleShotServer1.OpenConnection;
end;

procedure TForm1.ButtonMDNSDisconnectClick(Sender: TObject);
begin
  if mDNSConnected then
    Lcc_mDNS_SingleShotServer1.CloseConnection;
end;

procedure TForm1.ButtonMDNSSendClick(Sender: TObject);
begin
  lcc_mdns_singleshot.SendmDNSQuery('openlcb.local', True);
end;

procedure TForm1.ButtonTestMulitCastClick(Sender: TObject);
begin
  multicasttest
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if mDNSConnected then
    Lcc_mDNS_SingleShotServer1.CloseConnection;
  if Connected then
    LccWebSocketServer1.CloseConnection(nil);
  while Connected and mDNSConnected do
    Sleep(100);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WorkerMessage := TLccMessage.Create;
  ButtonMDNSDisconnect.Enabled := False;
  ButtonLccDisconnect.Enabled := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWorkerMessage);
end;

procedure TForm1.LccWebSocketServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
  sServer, sClient: string;
  i: Integer;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        StatusBar1.Panels[0].Text := 'Listener Connecting';
        Listview1.Clear;
        ButtonLccConnect.Enabled := False;
        ButtonLccDisconnect.Enabled := False;
        Connected := False;
      end;
    ccsListenerConnected :
      begin
        StatusBar1.Panels[0].Text := 'Listener Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        ButtonLccConnect.Enabled := False;
        ButtonLccDisconnect.Enabled := True;
        Connected := True;
      end;
    ccsListenerDisconnecting :
      begin
        StatusBar1.Panels[0].Text := 'Listener Disconnected';
        Listview1.Clear;
        ButtonLccConnect.Enabled := False;
        ButtonLccDisconnect.Enabled := False;
        Connected := False;
      end;
    ccsListenerDisconnected :
      begin
        StatusBar1.Panels[0].Text := 'Listener Disconnected';
        Listview1.Clear;
        ButtonLccConnect.Enabled := True;
        ButtonLccDisconnect.Enabled := False;
        Connected := False;
      end;
    ccsListenerClientConnecting :
      begin
        StatusBar1.Panels[1].Text := 'Client Connecting';
      end;
    ccsListenerClientConnected  :
      begin
        StatusBar1.Panels[1].Text := IntToStr( LccWebSocketServer1.EthernetThreads.Count) +  ' Clients Connected';
        ListItem := ListView1.Items.Add;
        ListItem.Caption := EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        ListItem.SubItems.Add(EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort));
      end;
    ccsListenerClientDisconnecting  :
      begin
        StatusBar1.Panels[1].Text := 'Client Disconnecting';
      end;
    ccsListenerClientDisconnected  :
      begin
        StatusBar1.Panels[1].Text := IntToStr( LccWebSocketServer1.EthernetThreads.Count) +  ' Clients Connected';
        sServer := EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        sClient := EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
        ListItem := nil;
        i := ListView1.Items.Count - 1;
        while i > -1 do
        begin
          if ListView1.Items[i].Caption = sServer then
          begin
            if ListView1.Items[i].SubItems[0] = sClient then
            begin
              ListItem := ListView1.Items[i];
            end;
          end;
          Dec(i);
        end;
        if Assigned(ListItem) then
          ListView1.Items.Delete(ListItem.Index);
      end;
  end;
end;

procedure TForm1.LccWebSocketServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('Olcb TCP Error Code: ' + IntToStr(EthernetRec.ErrorCode) + ' ' + EthernetRec.MessageStr);
end;

procedure TForm1.LccWebSocketServer1ReceiveMessage(Sender: TObject;
  EthernetRec: TLccEthernetRec);
begin
  Memo1.Lines.BeginUpdate;
  Memo1.Lines.Add(EthernetRec.MessageStr);
  Memo1.Lines.EndUpdate;
end;

procedure TForm1.Lcc_mDNS_SingleShotServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        StatusBar1.Panels[2].Text := 'mDNS Listener Connecting';
        Listview1.Clear;
        ButtonMDNSConnect.Enabled := False;
        ButtonMDNSDisconnect.Enabled := False;
        mDNSConnected := False;
      end;
    ccsListenerConnected :
      begin
        StatusBar1.Panels[2].Text := 'mDNS Listener Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        ButtonMDNSConnect.Enabled := False;
        ButtonMDNSDisconnect.Enabled := True;
        mDNSConnected := True;
      end;
    ccsListenerDisconnecting :
      begin
        StatusBar1.Panels[2].Text := 'mDNS Listener Disconnected';
        Listview1.Clear;
        ButtonMDNSConnect.Enabled := False;
        ButtonMDNSDisconnect.Enabled := False;
        mDNSConnected := False;
      end;
    ccsListenerDisconnected :
      begin
        StatusBar1.Panels[2].Text := 'mDNS Listener Disconnected';
        Listview1.Clear;
        ButtonMDNSConnect.Enabled := True;
        ButtonMDNSDisconnect.Enabled := False;
        mDNSConnected := False;
      end;
  end;
end;


procedure TForm1.Lcc_mDNS_SingleShotServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('mDNS Error Code: ' + IntToStr(EthernetRec.ErrorCode) + ' ' + EthernetRec.MessageStr);
end;

procedure TForm1.Lcc_mDNS_SingleShotServer1Question(AQuestion: TmDNSQuestionRec);
begin
  Memo1.Lines.BeginUpdate;
  Memo1.Lines.Add(AQuestion.QName);
  Memo1.Lines.EndUpdate;
 // if AQuestion.QName = 'openlcb.local' then
 //   beep;
end;

end.

