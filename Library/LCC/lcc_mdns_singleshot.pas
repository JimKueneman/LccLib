unit lcc_mdns_singleshot;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
    {$DEFINE LOGGING}
  {$ENDIF}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP} LResources, Forms, Controls, Graphics, Dialogs, {$ENDIF}
  {$ELSE}
  FMX.Forms, Types, System.Generics.Collections,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging, lcc_detailed_logging,
  {$ENDIF}
  lcc_gridconnect, blcksock, synsock,
  lcc_can_message_assembler_disassembler,
  lcc_nodemanager, lcc_messages, lcc_ethernetclient, lcc_threadedcirculararray,
  lcc_tcp_protocol, lcc_app_common_settings, lcc_utilities,
  lcc_common_classes;


type
  TLcc_mDNS_SinglShotServer = class;

  { TLcc_mDNS_SingleShotListener }

  TLcc_mDNS_SingleShotListener = class(TThread)
  private
    FEthernetRec: TLccEthernetRec;
    FGridConnect: Boolean;
    FOnClientDisconnect: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FOwner: TLcc_mDNS_SinglShotServer;
    FRunning: Boolean;
    FSleepCount: Integer;
    FSocket: TUDPBlockSocket;
    function GetIsTerminated: Boolean;
  protected
    property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
    property Owner: TLcc_mDNS_SinglShotServer read FOwner write FOwner;
    property Running: Boolean read FRunning write FRunning;
    property Socket: TUDPBlockSocket read FSocket write FSocket;
    property IsTerminated: Boolean read GetIsTerminated;

    procedure DoConnectionState;
    procedure DoErrorMessage;
    procedure DoReceiveMessage;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLcc_mDNS_SinglShotServer; const AnEthernetRec: TLccEthernetRec); reintroduce; virtual;
    destructor Destroy; override;

    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write FSleepCount;
  end;

  { TLcc_mDNS_SinglShotServer }

  TLcc_mDNS_SinglShotServer = class(TLccHardwareConnectionManager)
  private
    FLccSettings: TLccSettings;
    FListenerThread: TLcc_mDNS_SingleShotListener;
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateListenerEvents(AListenerThread: TLcc_mDNS_SingleShotListener; Suspend: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenConnection(var AnEthernetRec: TLccEthernetRec): TLcc_mDNS_SingleShotListener;
    function OpenConnectionWithLccSettings: TLcc_mDNS_SingleShotListener;
    procedure CloseConnection;

    property ListenerThread: TLcc_mDNS_SingleShotListener read FListenerThread write FListenerThread;

  published
    { Published declarations }
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
    {$IFDEF FPC}
    //  {$I TLcc_mDNS_SinglShotServer.lrs}
    {$ENDIF}
    RegisterComponents('LCC',[TLcc_mDNS_SinglShotServer]);
  {$ENDIF}
end;

{ TLcc_mDNS_SingleShotListener }

constructor TLcc_mDNS_SingleShotListener.Create(CreateSuspended: Boolean;AnOwner: TLcc_mDNS_SinglShotServer; const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := nil;
  FEthernetRec.LccMessage := TLccMessage.Create;
end;

destructor TLcc_mDNS_SingleShotListener.Destroy;
begin
  FreeAndNil(FEthernetRec.LccMessage);
  inherited Destroy;
end;

procedure TLcc_mDNS_SingleShotListener.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLcc_mDNS_SingleShotListener.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLcc_mDNS_SingleShotListener.DoReceiveMessage;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event
    if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, FEthernetRec);
  end
end;

procedure TLcc_mDNS_SingleShotListener.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FEthernetRec.ConnectionState := NewConnectionState;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
  end;

  procedure HandleErrorAndDisconnect;
  begin
    FEthernetRec.ErrorCode := Socket.LastError;
    FEthernetRec.MessageStr := Socket.LastErrorDesc;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
    SendConnectionNotification(ccsListenerDisconnected);
    Terminate
  end;

var
  AByte: Byte;
  {$IFDEF LCC_WINDOWS}
  LocalName: String;
  IpStrings: TStringList;
  i: Integer;
  {$ENDIF}
begin
  FRunning := True;

  Socket := TUDPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  SendConnectionNotification(ccsListenerConnecting);

  if FEthernetRec.AutoResolveIP then
  begin
    {$IFDEF LCC_WINDOWS}
    LocalName := Socket.LocalName;
    IpStrings := TStringList.Create;
    try
       Socket.ResolveNameToIP(String( LocalName), IpStrings) ;  // '192.168.0.8';
       for i := 0 to IpStrings.Count - 1 do
         FEthernetRec.ListenerIP := IpStrings[i];
    finally
      IpStrings.Free;
    end;
    {$ELSE}
    FEthernetRec.ListenerIP := ResolveUnixIp;
    {$ENDIF}
  end;


  Socket.EnableReusePort(True);
  Socket.Bind(String( EthernetRec.ListenerIP), String( IntToStr(EthernetRec.ListenerPort)));
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    FRunning := False
  end else
  begin
    Socket.AddMulticast('224.0.0.251');
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      FRunning := False
    end else
    begin
      Socket.MulticastTTL := 1;
      // UDP Connections do not "Listen"
      SendConnectionNotification(ccsListenerConnected);
      try
        try
          while not Terminated and (FEthernetRec.ConnectionState = ccsListenerConnected) do
          begin
            if Socket.CanRead(1000) then
            begin
              if not Terminated then
              begin
                while Socket.LastError = 0 do
                begin
                  AByte := Socket.RecvByte(0);
                  case Socket.LastError of
                     0 :
                       begin
                       end;
                     WSAETIMEDOUT :
                       begin
                       end;
                     WSAECONNABORTED :
                       begin
                         HandleErrorAndDisconnect;
                       end;
                   end;
                 end
              end
            end
          end;
        finally
          SendConnectionNotification(ccsListenerDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
        end;
      finally
        SendConnectionNotification(ccsListenerDisconnected);
        FRunning := False;
      end;
    end;
  end;
end;

function TLcc_mDNS_SingleShotListener.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

{ TLcc_mDNS_SinglShotServer }

procedure TLcc_mDNS_SinglShotServer.CloseConnection;
var
  TimeCount: Integer;
begin
  if Assigned(ListenerThread) then
  begin
    TimeCount := 0;
    ListenerThread.Terminate;
    if Assigned(ListenerThread.Socket) then
      ListenerThread.Socket.CloseSocket;  // Force out of wait state with an error
    while ListenerThread.Running do
    begin
      {$IFNDEF FPC_CONSOLE_APP}
      Application.ProcessMessages;
      {$ELSE}
      CheckSynchronize();  // Pump the timers
      {$ENDIF}
      Inc(TimeCount);
      Sleep(100);
      if TimeCount = 10 then
      begin
         if Assigned(ListenerThread.Socket) then
           ListenerThread.Socket.CloseSocket
         else
           Break // Something went really wrong
      end;

    end;
    FreeAndNil(FListenerThread);
  end;
end;

procedure TLcc_mDNS_SinglShotServer.UpdateListenerEvents(AListenerThread: TLcc_mDNS_SingleShotListener; Suspend: Boolean);
begin
  if Assigned(AListenerThread) then
  begin
    AListenerThread.OnConnectionStateChange := OnConnectionStateChange;
    AListenerThread.OnErrorMessage := OnErrorMessage;
    AListenerThread.OnReceiveMessage := OnReceiveMessage;
    AListenerThread.OnSendMessage := OnSendMessage;
  end;
end;

constructor TLcc_mDNS_SinglShotServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLcc_mDNS_SinglShotServer.Destroy;
begin
  inherited Destroy;
end;

function TLcc_mDNS_SinglShotServer.OpenConnection(var AnEthernetRec: TLccEthernetRec): TLcc_mDNS_SingleShotListener;
begin
  // Default mDNS port to listen too
  if AnEthernetRec.ListenerIP = '' then
    AnEthernetRec.ListenerIP := '0.0.0.0';
  if AnEthernetRec.ListenerPort = 0 then
    AnEthernetRec.ListenerPort := 5353;

  Result := TLcc_mDNS_SingleShotListener.Create(True, Self, AnEthernetRec);
  Result.Owner := Self;
  UpdateListenerEvents(Result, True);
  Result.Suspended := False;
  ListenerThread := Result;
end;

function TLcc_mDNS_SinglShotServer.OpenConnectionWithLccSettings: TLcc_mDNS_SingleShotListener;
var
  AnEthernetRec: TLccEthernetRec;
begin
  Result := nil;
  if Assigned(LccSettings) then
  begin
    AnEthernetRec.ConnectionState := ccsListenerDisconnected;
    AnEthernetRec.SuppressNotification := False;
    AnEthernetRec.Thread := nil;
    AnEthernetRec.MessageStr := '';
    AnEthernetRec.ListenerPort := LccSettings.Ethernet.LocalListenerPort;
    AnEthernetRec.ListenerIP := LccSettings.Ethernet.LocalListenerIP;
    AnEthernetRec.ClientIP := '';
    AnEthernetRec.ClientPort := 0;
    AnEthernetRec.HeartbeatRate := 0;
    AnEthernetRec.ErrorCode := 0;
    AnEthernetRec.MessageArray := nil;
    AnEthernetRec.AutoResolveIP := LccSettings.Ethernet.AutoResolveListenerIP;
    Result := OpenConnection(AnEthernetRec);
  end;
end;


initialization
  RegisterClass(TLcc_mDNS_SinglShotServer);

finalization

end.

