unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, lcc_websocketserver, lcc_ethernetclient, lcc_messages,
  lcc_defines, lcc_mdns_singleshot;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    LccWebSocketServer1: TLccWebSocketServer;
    Lcc_mDNS_SinglShotServer1: TLcc_mDNS_SinglShotServer;
    ListView1: TListView;
    Memo1: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LccWebSocketServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccWebSocketServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccWebSocketServer1ReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure Lcc_mDNS_SinglShotServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure Lcc_mDNS_SinglShotServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
  private
    FConnected: Boolean;
    FWorkerMessage: TLccMessage;
    { private declarations }
  public
    { public declarations }
    property Connected: Boolean read FConnected write FConnected;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
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
  FillChar(EthernetRec, SizeOf(TLccEthernetRec), #0);
  EthernetRec.AutoResolveIP := True;
  EthernetRec.ListenerPort := 12021;
  LccWebSocketServer1.OpenConnection(EthernetRec);
end;

procedure TForm1.Button2Click(Sender: TObject);
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

procedure TForm1.Button4Click(Sender: TObject);
begin
  WorkerMessage.LoadVerifyNodeID(NULL_NODE_ID, $0ABC);
  LccWebSocketServer1.SendMessage(WorkerMessage);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(TLccEthernetRec), #0);
//  EthernetRec.AutoResolveIP := True;
  Lcc_mDNS_SinglShotServer1.OpenConnection(EthernetRec);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Lcc_mDNS_SinglShotServer1.CloseConnection;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WorkerMessage := TLccMessage.Create;
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
        Connected := False;
      end;
    ccsListenerConnected :
      begin
        StatusBar1.Panels[0].Text := 'Listener Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        Connected := True;
      end;
    ccsListenerDisconnecting :
      begin
        StatusBar1.Panels[0].Text := 'Listener Disconnected';
        Listview1.Clear;
        Connected := False;
      end;
    ccsListenerDisconnected :
      begin
        StatusBar1.Panels[0].Text := 'Listener Disconnected';
        Listview1.Clear;
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

procedure TForm1.Lcc_mDNS_SinglShotServer1ConnectionStateChange(
  Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        StatusBar1.Panels[2].Text := 'mDNS Listener Connecting';
        Listview1.Clear;
        Connected := False;
      end;
    ccsListenerConnected :
      begin
        StatusBar1.Panels[2].Text := 'mDNS Listener Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        Connected := True;
      end;
    ccsListenerDisconnecting :
      begin
        StatusBar1.Panels[2].Text := 'mDNS Listener Disconnected';
        Listview1.Clear;
        Connected := False;
      end;
    ccsListenerDisconnected :
      begin
        StatusBar1.Panels[2].Text := 'mDNS Listener Disconnected';
        Listview1.Clear;
        Connected := False;
      end;
  end;
end;

procedure TForm1.Lcc_mDNS_SinglShotServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('mDNS Error Code: ' + IntToStr(EthernetRec.ErrorCode) + ' ' + EthernetRec.MessageStr);
end;

end.

