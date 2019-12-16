unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  lcc_ethenetserver, lcc_ethernetclient, lcc_messages,
  lcc_defines, lcc_websocketserver, blcksock, synsock;

type

  { TForm1 }

  TForm1 = class(TForm)
    LccEthernetServer: TLccEthernetServer;
    LccWebSocketServer: TLccWebSocketServer;
    ListViewServer: TListView;
    ListViewWebSocket: TListView;
    Panel1: TPanel;
    StatusBarServer: TStatusBar;
    StatusBarWebSocket: TStatusBar;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServerReceiveMessage(Sender: TObject;
      EthernetRec: TLccEthernetRec);
    procedure LccWebSocketServerConnectionStateChange(Sender: TObject;
      EthernetRec: TLccEthernetRec);
    procedure LccWebSocketServerReceiveMessage(Sender: TObject;
      EthernetRec: TLccEthernetRec);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
  sServer, sClient: string;
  i: Integer;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        StatusBarServer.Panels[0].Text := 'Listener Connecting';
        ListViewServer.Clear;
      end;
    ccsListenerConnected :
      begin
        StatusBarServer.Panels[0].Text := 'Listener Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsListenerDisconnecting :
      begin
        StatusBarServer.Panels[0].Text := 'Listener Disconnected';
        ListViewServer.Clear;
      end;
    ccsListenerDisconnected :
      begin
        StatusBarServer.Panels[0].Text := 'Listener Disconnected';
        ListViewServer.Clear;
      end;
    ccsListenerClientConnecting :
      begin
        StatusBarServer.Panels[1].Text := 'Client Connecting';
      end;
    ccsListenerClientConnected  :
      begin
        StatusBarServer.Panels[1].Text := IntToStr( LccEthernetServer.EthernetThreads.Count) +  ' Clients Connected';
        ListItem := ListViewServer.Items.Add;
        ListItem.Caption := EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        ListItem.SubItems.Add(EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort));
      end;
    ccsListenerClientDisconnecting  :
      begin
        StatusBarServer.Panels[1].Text := 'Client Disconnecting';
      end;
    ccsListenerClientDisconnected  :
      begin
        StatusBarServer.Panels[1].Text := IntToStr( LccEthernetServer.EthernetThreads.Count) +  ' Clients Connected';
        sServer := EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        sClient := EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
        ListItem := nil;
        i := ListViewServer.Items.Count - 1;
        while i > -1 do
        begin
          if ListViewServer.Items[i].Caption = sServer then
          begin
            if ListViewServer.Items[i].SubItems[0] = sClient then
            begin
              ListItem := ListViewServer.Items[i];
            end;
          end;
          Dec(i);
        end;
        if Assigned(ListItem) then
          ListViewServer.Items.Delete(ListItem.Index);
      end;
  end;
end;

procedure TForm1.LccEthernetServerReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  LccWebSocketServer.SendMessage(EthernetRec.LccMessage);
end;

procedure TForm1.LccWebSocketServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
  sServer, sClient: string;
  i: Integer;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        StatusBarWebSocket.Panels[0].Text := 'Listener Connecting';
        ListViewWebSocket.Clear;
      end;
    ccsListenerConnected :
      begin
        StatusBarWebSocket.Panels[0].Text := 'Listener Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsListenerDisconnecting :
      begin
        StatusBarWebSocket.Panels[0].Text := 'Listener Disconnected';
        ListViewWebSocket.Clear;
      end;
    ccsListenerDisconnected :
      begin
        StatusBarWebSocket.Panels[0].Text := 'Listener Disconnected';
        ListViewWebSocket.Clear;
      end;
    ccsListenerClientConnecting :
      begin
        StatusBarWebSocket.Panels[1].Text := 'Client Connecting';
      end;
    ccsListenerClientConnected  :
      begin
        StatusBarWebSocket.Panels[1].Text := IntToStr( LccWebSocketServer.EthernetThreads.Count) +  ' Clients Connected';
        ListItem := ListViewWebSocket.Items.Add;
        ListItem.Caption := EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        ListItem.SubItems.Add(EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort));
      end;
    ccsListenerClientDisconnecting  :
      begin
        StatusBarWebSocket.Panels[1].Text := 'Client Disconnecting';
      end;
    ccsListenerClientDisconnected  :
      begin
        StatusBarWebSocket.Panels[1].Text := IntToStr( LccWebSocketServer.EthernetThreads.Count) +  ' Clients Connected';
        sServer := EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        sClient := EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
        ListItem := nil;
        i := ListViewWebSocket.Items.Count - 1;
        while i > -1 do
        begin
          if ListViewWebSocket.Items[i].Caption = sServer then
          begin
            if ListViewWebSocket.Items[i].SubItems[0] = sClient then
            begin
              ListItem := ListViewWebSocket.Items[i];
            end;
          end;
          Dec(i);
        end;
        if Assigned(ListItem) then
          ListViewWebSocket.Items.Delete(ListItem.Index);
      end;
  end;
end;

procedure TForm1.LccWebSocketServerReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  LccEthernetServer.SendMessage(EthernetRec.LccMessage);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  LccWebSocketServer.CloseConnection(nil);
  LccEthernetServer.CloseConnection(nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormShow(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, SizeOf(TLccEthernetRec), 0);
  EthernetRec.AutoResolveIP := True;
  EthernetRec.ListenerPort := 12021;
  LccEthernetServer.Gridconnect := True;
  LccEthernetServer.OpenConnection(EthernetRec);

  FillChar(EthernetRec, SizeOf(TLccEthernetRec), 0);
  EthernetRec.AutoResolveIP := True;
  EthernetRec.ListenerPort := 12022;
  LccWebSocketServer.Gridconnect := True;
  LccWebSocketServer.OpenConnection(EthernetRec);
end;

end.

