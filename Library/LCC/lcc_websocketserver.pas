unit lcc_websocketserver;

// This does not support fragmentation so don't purposefully try to split a message up
// either send a complete OpenLCB message or send nothing.

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
  lcc_common_classes, sha1, base64;

const
 WEBSOCKET_OPCODE_CONTINUE = $00;
 WEBSOCKET_OPCODE_TEXT     = $01;
 WEBSOCKET_OPCODE_BINARY   = $02;
 WEBSOCKET_OPCODE_CLOSE    = $08;
 WEBSOCKET_OPCODE_PING     = $09;
 WEBSOCKET_OPCODE_PONG     = $0A;

 WEBSOCKET_CLOSE_HEADER_SERVER_LEN = 2;
 WEBSOCKET_CLOSE_HEADER_SERVER: array[0..WEBSOCKET_CLOSE_HEADER_SERVER_LEN-1] of Byte = ($88, $80);
 WEBSOCKET_CLOSE_HEADER_CLIENT_LEN = 6;
 WEBSOCKET_CLOSE_HEADER_CLIENT: array[0..WEBSOCKET_CLOSE_HEADER_CLIENT_LEN-1] of Byte = ($88, $00, $56, $78, $AB, $55);

type
  TLccWebSocketServerThread = class;     // Forward
  TLccWebSocketServer = class;

  { TLccWebSocketServerThread }

  TLccWebSocketServerThread =  class(TLccConnectionThread)
    private
      FEthernetRec: TLccEthernetRec;
      FOnClientDisconnect: TOnEthernetRecFunc;
      FOnErrorMessage: TOnEthernetRecFunc;
      FOnConnectionStateChange: TOnEthernetRecFunc;
      FOnReceiveMessage: TOnEthernetReceiveFunc;
      FOnSendMessage: TOnMessageEvent;
      FOwner: TLccWebSocketServer;
      FSocket: TTCPBlockSocket;
      FSocketHandleForListener: TSocket;
      FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
      FWebSocketInitialized: Boolean;
    protected
      procedure BuildAndSendInitializeSuccessReply(ASocket: TTCPBlockSocket; SecWebSocketKey: string);
      procedure BuildAndSendInitializeFailureReply(ASocket: TTCPBlockSocket);
      procedure DoClientDisconnect;
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure ExtractInitializationRequest(ASocket: TTCPBlockSocket; var Method, URL, Protocol: string);
      function ExtractInitializationHeaderKeys(ASocket: TTCPBlockSocket): TStringList;
      procedure HandleErrorAndDisconnect;
      procedure HandleSendConnectionNotification(NewConnectionState: TConnectionState);
      function ReceiveWebSocketStateMachine(ASocket: TTCPBlockSocket): TDynamicByteArray;
      procedure SendWebSocketStateMachine(ASocket: TTCPBlockSocket; OutStr: string);
      procedure SendMessage(AMessage: TLccMessage);

      property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
      property Socket: TTCPBlockSocket read FSocket write FSocket;
      property SocketHandleForListener: TSocket read FSocketHandleForListener write FSocketHandleForListener;
      property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
      property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
      property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
      property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
      property Owner: TLccWebSocketServer read FOwner write FOwner;
      property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
      property WebSocketInitialized: Boolean read FWebSocketInitialized write FWebSocketInitialized;
    public
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccWebSocketServer; const AnEthernetRec: TLccEthernetRec); reintroduce;
      destructor Destroy; override;
  end;

  { TLccWebSocketThreadList }

  TLccWebSocketThreadList = class(TThreadList)      // Contains TLccWebSocketServerThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure CloseEthernetPorts;
    procedure CloseEthernetPort(EthernetThread: TLccWebSocketServerThread);

    property Count: Integer read GetCount;
  end;

  { TLccWebSocketListener }

  TLccWebSocketListener = class(TThread)
  private
    FEthernetRec: TLccEthernetRec;
    FGridConnect: Boolean;
    FOnClientDisconnect: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FOwner: TLccWebSocketServer;
    FRunning: Boolean;
    FSleepCount: Integer;
    FSocket: TTCPBlockSocket;
    function GetIsTerminated: Boolean;
  protected
    property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
    property Owner: TLccWebSocketServer read FOwner write FOwner;
    property Running: Boolean read FRunning write FRunning;
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property IsTerminated: Boolean read GetIsTerminated;

    function CreateServerThread(ASocketHandle: TSocket): TLccWebSocketServerThread;
    procedure DoConnectionState;
    procedure DoErrorMessage;
    procedure DoReceiveMessage;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccWebSocketServer; const AnEthernetRec: TLccEthernetRec); reintroduce; virtual;
    destructor Destroy; override;

    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write FSleepCount;
  end;

  { TLccWebSocketServer }

  TLccWebSocketServer = class(TLccHardwareConnectionManager)
  private
    FEthernetThreads: TLccWebSocketThreadList;
    FGridConnect: Boolean;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FListenerThread: TLccWebSocketListener;
    {$IFDEF LOGGING}FLoggingFrame: TFrameLccLogging;{$ENDIF}
    FNodeManager: TLccNodeManager;
    FOnClientDisconnect: TOnEthernetRecFunc;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FSleepCount: Integer;
    procedure SetGridConnect(AValue: Boolean);
    procedure SetSleepCount(AValue: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateThreadEvents(EthernetThread: TLccWebSocketServerThread);
    procedure UpdateThreadsEvents;
    procedure UpdateListenerEvents(AListenerThread: TLccWebSocketListener; Suspend: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccWebSocketListener;
    function OpenConnectionWithLccSettings: TLccWebSocketListener;
    procedure CloseConnection( EthernetThread: TLccWebSocketServerThread);
    procedure SendMessage(AMessage: TLccMessage);  override;
    procedure SendMessageRawGridConnect(GridConnectStr: String); override;

    property EthernetThreads: TLccWebSocketThreadList read FEthernetThreads write FEthernetThreads;
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector
    property ListenerThread: TLccWebSocketListener read FListenerThread write FListenerThread;

  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property Gridconnect: Boolean read FGridConnect write SetGridConnect;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write SetSleepCount;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
    {$IFDEF FPC}
      {$I TLccWebSocketServer.lrs}
    {$ENDIF}
    RegisterComponents('LCC',[TLccWebSocketServer]);
  {$ENDIF}
end;

{ TLccWebSocketListener }

constructor TLccWebSocketListener.Create(CreateSuspended: Boolean;
  AnOwner: TLccWebSocketServer;
  const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := nil;
  FEthernetRec.LccMessage := TLccMessage.Create;
//  FOutgoingGridConnect := TThreadStringList.Create;
 // FMsgAssembler := TLccMessageAssembler.Create;
//  FMsgDisAssembler := TLccMessageDisAssembler.Create;
 // FWorkerMsg := TLccMessage.Create;
end;

function TLccWebSocketListener.CreateServerThread(ASocketHandle: TSocket): TLccWebSocketServerThread;
begin
  Result := TLccWebSocketServerThread.Create(True, Owner, FEthernetRec);
  Result.SocketHandleForListener := ASocketHandle;    // Back create the sockets with this handle
  Result.OnClientDisconnect := OnClientDisconnect;
  Result.OnConnectionStateChange := OnConnectionStateChange;
  Result.OnErrorMessage := OnErrorMessage;
  Result.OnReceiveMessage := OnReceiveMessage;
  Result.OnSendMessage := OnSendMessage;
  Result.SleepCount := FSleepCount;
  Result.GridConnect := FGridConnect;
  Result.Start;
end;

destructor TLccWebSocketListener.Destroy;
begin
//  FreeAndNil( FOutgoingGridConnect);
  FreeAndNil(FEthernetRec.LccMessage);
 // FreeAndNil(FMsgAssembler);
 // FreeandNil(FMsgDisAssembler);
//  FreeAndNIl(FWorkerMsg);
  inherited Destroy;
end;

procedure TLccWebSocketListener.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccWebSocketListener.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccWebSocketListener.DoReceiveMessage;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event
    if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, FEthernetRec);
  end
end;

procedure TLccWebSocketListener.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FEthernetRec.ConnectionState := NewConnectionState;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
  end;

  procedure HandleErrorAndDisconnect;
  begin
    Owner.EthernetThreads.Remove(Self);
    FEthernetRec.ErrorCode := Socket.LastError;
    FEthernetRec.MessageStr := Socket.LastErrorDesc;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
    SendConnectionNotification(ccsListenerDisconnected);
    Terminate
  end;

var
  NewLink: TLccWebSocketServerThread;
  {$IFDEF LCC_WINDOWS}
  LocalName: String;
  IpStrings: TStringList;
  i: Integer;
  {$ENDIF}
begin
  FRunning := True;

  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  SendConnectionNotification(ccsListenerConnecting);

  if FEthernetRec.AutoResolveIP then
  begin
    {$IFDEF LCC_WINDOWS}
    FEthernetRec.ListenerIP := ResolveWindowsIp(Socket);
    {$ELSE}
    FEthernetRec.ListenerIP := ResolveUnixIp;
    {$ENDIF}
  end;

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
    Socket.Listen;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      FRunning := False
    end else
    begin
      SendConnectionNotification(ccsListenerConnected);
      try
        try
          while not Terminated and (FEthernetRec.ConnectionState = ccsListenerConnected) do
          begin
            if Socket.CanRead(1000) then
            begin
              if not Terminated and (Socket.LastError <> WSAETIMEDOUT) then
              begin
                if Socket.LastError = 0 then
                begin
                  NewLink := CreateServerThread(Socket.Accept);
                  if Assigned(NewLink) then
                    Owner.EthernetThreads.Add(NewLink);
                end else
                  Terminate;
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

function TLccWebSocketListener.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

{ TLccWebSocketThreadList }

function TLccWebSocketThreadList.GetCount: Integer;
var
  L: TList;
begin
  L := LockList;
  try
    Result := L.Count
  finally
    UnlockList;
  end;
end;

destructor TLccWebSocketThreadList.Destroy;
begin
  CloseEthernetPorts;
  inherited Destroy;
end;

procedure TLccWebSocketThreadList.CloseEthernetPorts;
var
  L: TList;
  Thread: TLccWebSocketServerThread;
begin
  while Count > 0 do
  begin
    L := LockList;
    try
      Thread := TLccWebSocketServerThread( L[0]);
      L.Delete(0);
    finally
      UnlockList;
    end;
    CloseEthernetPort(Thread);
  end;
end;

procedure TLccWebSocketThreadList.CloseEthernetPort( EthernetThread: TLccWebSocketServerThread);
var
  TimeCount: Cardinal;
begin
  EthernetThread.Terminate;
  TimeCount := 0;
//  TimeCount := GetTickCount;            DON"T LINK OCLB_UTILITES, it causes issues with linking to different packages
  while (EthernetThread.Running) do
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
      if Assigned(EthernetThread.Socket) then
        EthernetThread.Socket.CloseSocket
      else
        Break // Something went really wrong
    end;
  end;
  FreeAndNil( EthernetThread);
end;

{ TLccWebSocketServer }

procedure TLccWebSocketServer.CloseConnection(EthernetThread: TLccWebSocketServerThread);
var
  TimeCount: Integer;
begin
  if Assigned(EthernetThread) then
  begin
    EthernetThreads.Remove(EthernetThread);
    EthernetThreads.CloseEthernetPort(EthernetThread);
  end else
    EthernetThreads.CloseEthernetPorts;
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

procedure TLccWebSocketServer.SetSleepCount(AValue: Integer);
begin
    if AValue <> FSleepCount then
  begin
    FSleepCount := AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread, True);
  end;
end;

procedure TLccWebSocketServer.UpdateListenerEvents(
  AListenerThread: TLccWebSocketListener; Suspend: Boolean);
begin
  if Assigned(AListenerThread) then
  begin
    AListenerThread.OnClientDisconnect := OnClientDisconnect;
    AListenerThread.OnConnectionStateChange := OnConnectionStateChange;
    AListenerThread.OnErrorMessage := OnErrorMessage;
    AListenerThread.OnReceiveMessage := OnReceiveMessage;
    AListenerThread.OnSendMessage := OnSendMessage;
    AListenerThread.GridConnect := FGridConnect;
  end;
end;

procedure TLccWebSocketServer.UpdateThreadEvents(EthernetThread: TLccWebSocketServerThread);
begin
  EthernetThread.OnSendMessage := OnSendMessage;
  EthernetThread.SleepCount := SleepCount;
  EthernetThread.GridConnect := FGridConnect;
end;

procedure TLccWebSocketServer.UpdateThreadsEvents;
var
  i: Integer;
  L: TList;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
      UpdateThreadEvents(TLccWebSocketServerThread( L[i]));
  finally
    EthernetThreads.UnlockList;
  end;
end;

constructor TLccWebSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEthernetThreads := TLccWebSocketThreadList.Create;
  FHub := False;
end;

destructor TLccWebSocketServer.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  inherited Destroy;
end;

function TLccWebSocketServer.OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccWebSocketListener;
begin
  Result := TLccWebSocketListener.Create(True, Self, AnEthernetRec);
  Result.Owner := Self;
  UpdateListenerEvents(Result, True);
  Result.Suspended := False;
  ListenerThread := Result;
end;

function TLccWebSocketServer.OpenConnectionWithLccSettings: TLccWebSocketListener;
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

procedure TLccWebSocketServer.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
  L: TList;
  EthernetThread: TLccWebSocketServerThread;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccWebSocketServerThread( L[i]);
      if not EthernetThread.IsTerminated then
        EthernetThread.SendMessage(AMessage);
    end;
  finally
    EthernetThreads.UnlockList;
  end;
end;

procedure TLccWebSocketServer.SendMessageRawGridConnect(GridConnectStr: String);
//var
 // List: TList;
 // i: Integer;
begin
 // List := EthernetThreads.LockList;
  try            // TODO
 //   for i := 0 to List.Count - 1 do
 //     TLccEthernetClientThread(List[i]).OutgoingGridConnect.Add(GridConnectStr);
  finally
 //   EthernetThreads.UnlockList;
  end;
end;

procedure TLccWebSocketServer.SetGridConnect(AValue: Boolean);
begin
  if AValue <> FGridConnect then
  begin
    FGridConnect:=AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread, True);
  end;

end;

{ TLccWebSocketServerThread }

procedure TLccWebSocketServerThread.Execute;
var
  TxStr: String;
  RcvByte: Byte;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList: TStringList;
  DynamicByteArray: TDynamicByteArray;
  LocalSleepCount, i: Integer;
  HeaderStrings: TStringList;
  RequestMethod, RequestURL, RequestProtocol: string;
begin
  FRunning := True;

  HandleSendConnectionNotification(ccsListenerClientConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  Socket.Socket := SocketHandleForListener;    // Read back the handle
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    GridConnectHelper.Free;
    FRunning := False
  end else
  begin
    FEthernetRec.ClientIP := Socket.GetRemoteSinIP;
    FEthernetRec.ClientPort := Socket.GetRemoteSinPort;
    FEthernetRec.ListenerIP := Socket.GetLocalSinIP;
    FEthernetRec.ListenerPort := Socket.GetLocalSinPort;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      GridConnectHelper.Free;
      FRunning := False
    end else
    begin
      HandleSendConnectionNotification(ccsListenerClientConnected);
      try
        try
          LocalSleepCount := 0;
          while not IsTerminated and (FEthernetRec.ConnectionState = ccsListenerClientConnected) do
          begin

            if not WebSocketInitialized then
            begin
              ExtractInitializationRequest(Socket, RequestMethod, RequestURL, RequestProtocol);
              HeaderStrings := ExtractInitializationHeaderKeys(Socket);

              if (RequestURL = '/') and (HeaderStrings.Values['Upgrade:'] = 'websocket') and (HeaderStrings.Values['Sec-WebSocket-Protocol:'] = 'openlcb.websocket') then
                BuildAndSendInitializeSuccessReply(Socket, HeaderStrings.Values['Sec-WebSocket-Key:'])
              else begin
                BuildAndSendInitializeFailureReply(Socket);
                HandleErrorAndDisconnect;
              end;

              FreeAndNil(HeaderStrings);
              WebSocketInitialized := True;
            end;

            if not Terminated then
            begin
              // Handle the Socket using GridConnect
              if Gridconnect then
              begin
                if LocalSleepCount >= SleepCount then
                begin
                  TxStr := '';
                  TxList := OutgoingGridConnect.LockList;
                  try
                    if TxList.Count > 0 then
                    begin
                      TxStr := TxList[0];
                      TxList.Delete(0);
                    end;
                  finally
                    OutgoingGridConnect.UnlockList;
                  end;

                  if (TxStr <> '') and WebSocketInitialized then
                    SendWebSocketStateMachine(Socket, TxStr);
                  LocalSleepCount := 0;
                end;
                Inc(LocalSleepCount);

                if not Terminated then
                begin
                  DynamicByteArray := ReceiveWebSocketStateMachine(Socket);
                  if Length(DynamicByteArray) > 0 then
                  begin
                    GridConnectStrPtr := nil;
                    for i := 0 to Length(DynamicByteArray) - 1 do
                    begin
                      if GridConnectHelper.GridConnect_DecodeMachine(DynamicByteArray[i], GridConnectStrPtr) and WebSocketInitialized then
                      begin
                        FEthernetRec.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                        FEthernetRec.LccMessage.LoadByGridConnectStr(FEthernetRec.MessageStr);
                        Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                      end;
                    end;
                  end
                end;
              end else
              begin    // Handle the Socket with LCC TCP Protocol
              {
                if LocalSleepCount >= SleepCount then
                begin
                  DynamicByteArray := nil;
                  OutgoingCircularArray.LockArray;
                  try
                    if OutgoingCircularArray.Count > 0 then
                      OutgoingCircularArray.PullArray(DynamicByteArray);
                  finally
                    OutgoingCircularArray.UnLockArray;
                  end;

                  if Length(DynamicByteArray) > 0 then
                  begin
                    Socket.SendBuffer(@DynamicByteArray[0], Length(DynamicByteArray));
                    if Socket.LastError <> 0 then
                      HandleErrorAndDisconnect;
                    DynamicByteArray := nil;
                  end;
                  LocalSleepCount := 0;
                end;
                Inc(LocalSleepCount);

                RcvByte := Socket.RecvByte(1);
                case Socket.LastError of
                  0 :
                    begin
                      DynamicByteArray := nil;
                      if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, FEthernetRec.MessageArray) then
                        Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                    end;
                  WSAETIMEDOUT :
                    begin

                    end;
                  WSAECONNRESET   :
                    begin
                      FEthernetRec.MessageStr := Socket.LastErrorDesc;
                      Socket.ResetLastError;
                      Synchronize({$IFDEF FPC}@{$ENDIF}DoClientDisconnect);
                      FEthernetRec.MessageStr := '';
                      Terminate;
                    end
                else
                  HandleErrorAndDisconnect
                end;    }
              end
            end;
          end;
        finally
          HandleSendConnectionNotification(ccsListenerClientDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        HandleSendConnectionNotification(ccsListenerClientDisconnected);
        Owner.EthernetThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

procedure TLccWebSocketServerThread.ExtractInitializationRequest(ASocket: TTCPBlockSocket; var Method, URL, Protocol: string);
var
  s: String;
  StrList: TSTringList;
begin
  s := ASocket.RecvString(120000);

  s := StringReplace(s, ' ', #10, [rfReplaceAll]);
  StrList := TStringList.Create;
  StrList.Delimiter := #10;
  StrList.DelimitedText := s;
  if StrList.Count = 3 then
  begin
    Method := StrList[0];
    URL := StrList[1];
    Protocol := StrList[2];
  end else
  begin
    Method := '';
    URL := '';
    Protocol := ''
  end;
end;

function TLccWebSocketServerThread.ExtractInitializationHeaderKeys(ASocket: TTCPBlockSocket): TStringList;
var
  s: string;
begin
  Result := TStringList.Create;
  repeat
    s := ASocket.RecvString(120000);
    s := StringReplace(s, ' ', '=', []);
    Result.Add(s);
  until s = '';
end;

procedure TLccWebSocketServerThread.HandleErrorAndDisconnect;
begin
  Owner.EthernetThreads.Remove(Self);
  FEthernetRec.ErrorCode := Socket.LastError;
  FEthernetRec.MessageStr := Socket.LastErrorDesc;
  if not FEthernetRec.SuppressNotification and (FEthernetRec.ErrorCode <> 0) then
    Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
  HandleSendConnectionNotification(ccsListenerClientDisconnected);
  Terminate;
end;

procedure TLccWebSocketServerThread.HandleSendConnectionNotification(NewConnectionState: TConnectionState);
begin
  FEthernetRec.ConnectionState := NewConnectionState;
  if not FEthernetRec.SuppressNotification then
    Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
end;

function TLccWebSocketServerThread.ReceiveWebSocketStateMachine(ASocket: TTCPBlockSocket): TDynamicByteArray;
var
  AByte: Byte;
  iHeader: Integer;
  PayloadSize, iPayload: QWord;
  HasMask, IsText, IsBinary, IsClose, IsContinuation, IsPing, IsPong: Boolean;
  Mask: array[0..3] of Byte;
begin
  Result := nil;
  AByte := ASocket.RecvByte(0);
  case ASocket.LastError of
    0 :
      begin
        // We assume we are in sync and this is the start of the Message
        IsBinary := AByte and $0F = WEBSOCKET_OPCODE_BINARY;
        IsText := AByte and $0F = WEBSOCKET_OPCODE_TEXT;
        IsClose := AByte and $0F = WEBSOCKET_OPCODE_CLOSE;
        IsContinuation := AByte and $0F = WEBSOCKET_OPCODE_CONTINUE;
        IsPing := AByte and $0F = WEBSOCKET_OPCODE_PING;
        IsPong := AByte and $0F = WEBSOCKET_OPCODE_PONG;

        if (AByte and $80 = $80) then  // Last Frame of a concatinated message
        begin
          AByte := ASocket.RecvByte(0);
          PayloadSize := AByte and $7F;
          HasMask := AByte and $80 = $80;
          if PayloadSize = 126 then
          begin
            PayloadSize := ASocket.RecvByte(0) shl 8;
            PayloadSize := PayloadSize or ASocket.RecvByte(0);
          end else
          if PayloadSize = 127 then
          begin
            PayloadSize := ASocket.RecvByte(0) shl 56;
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 48);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 40);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 32);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 24);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 16);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 8);
            PayloadSize := PayloadSize or ASocket.RecvByte(0);
          end;
          if HasMask then
          begin
            Mask[0] := ASocket.RecvByte(0);
            Mask[1] := ASocket.RecvByte(0);
            Mask[2] := ASocket.RecvByte(0);
            Mask[3] := ASocket.RecvByte(0);
          end;
          iPayload := 0;
          SetLength(Result, PayloadSize);
          while iPayload < PayloadSize do
          begin
            AByte := ASocket.RecvByte(0);
            Result[iPayload] := AByte XOR Mask[iPayload mod 4];
            Inc(iPayload);
          end;
        end;
        if IsClose then
        begin
          ASocket.ResetLastError;
          for iHeader := 0 to WEBSOCKET_CLOSE_HEADER_SERVER_LEN - 1 do
            ASocket.SendByte(WEBSOCKET_CLOSE_HEADER_SERVER[iHeader]);
          HandleErrorAndDisconnect
        end else
        if IsPing then
        begin

        end else
        if IsPong then
        begin

        end
      end;
    WSAETIMEDOUT :
      begin

      end;
    WSAECONNRESET   :
      begin
        ASocket.ResetLastError;
        HandleErrorAndDisconnect
      end
  end;
end;

procedure TLccWebSocketServerThread.SendWebSocketStateMachine(ASocket: TTCPBlockSocket; OutStr: string);
var
  i: Integer;
begin
  // No mask from Server to Client
  ASocket.SendByte($80 or WEBSOCKET_OPCODE_TEXT);
  ASocket.SendByte(Length(OutStr));
  {$IFDEF LCC_MOBILE}
   for i := 0 to Length(OutStr) - 1 do
    ASocket.SendByte(Ord(OutStr[i]));
  {$ELSE}
  for i := 1 to Length(OutStr) do
    ASocket.SendByte(Ord(OutStr[i]));
  {$ENDIF}

 if ASocket.LastError <> 0 then
   HandleErrorAndDisconnect;
end;

procedure TLccWebSocketServerThread.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
  ByteArray: TDynamicByteArray;
begin
  if not IsTerminated then
  begin
    if Gridconnect then
    begin
      MsgDisAssembler.OutgoingMsgToMsgList(AMessage, MsgStringList);
      for i := 0 to MsgStringList.Count - 1 do
      begin
        OutgoingGridConnect.Add(MsgStringList[i]);
        {$IFDEF LOGGING}
        if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
          PrintToSynEdit( 'S EthSrv:' + MsgStringList[i],
                          Owner.LoggingFrame.SynEdit,
                          Owner.LoggingFrame.ActionLogPause.Checked,
                          Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                          Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
        {$ENDIF}
      end;
      DoSendMessage(AMessage);
    end else
    begin
      ByteArray := nil;
      if AMessage.ConvertToLccTcp(ByteArray) then
      begin
        OutgoingCircularArray.AddChunk(ByteArray);
        {$IFDEF LOGGING}
        if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
          PrintTCPToSynEdit( 'EthSrv ...Sending TCP...',
                          ByteArray,
                          Owner.LoggingFrame.SynEdit,
                          Owner.LoggingFrame.ActionLogPause.Checked,
                          Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                          Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
        {$ENDIF}
        DoSendMessage(AMessage);
      end;
    end;
  end;
end;

constructor TLccWebSocketServerThread.Create(CreateSuspended: Boolean; AnOwner: TLccWebSocketServer; const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := Self;
  FEthernetRec.LccMessage := TLccMessage.Create;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
end;

destructor TLccWebSocketServerThread.Destroy;
begin
  FreeAndNil(FEthernetRec.LccMessage);
  FreeAndNil(FTcpDecodeStateMachine);
  inherited Destroy;
end;

procedure TLccWebSocketServerThread.BuildAndSendInitializeSuccessReply(
  ASocket: TTCPBlockSocket; SecWebSocketKey: string);
var
  Hash: TSHA1Digest;
  StringStream: TStringStream;
  Base64Stream: TBase64EncodingStream;
  tempS: string;
begin
  StringStream := TStringStream.Create('');
  Base64Stream := TBase64EncodingStream.Create(StringStream);

  ASocket.SendString('HTTP/1.1 101 Switching Protocols' + CRLF);
  ASocket.SendString('Upgrade: websocket' + CRLF);
  ASocket.SendString('Connection: Upgrade' + CRLF);
  tempS := SecWebSocketKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  Hash := SHA1String(tempS);
  tempS := SHA1Print(Hash);   // Test only

  Base64Stream.Write(Hash, SizeOf(TSHA1Digest));
  Base64Stream.Flush;  // not multiple of 3 so need to pad
  tempS := StringStream.DataString;

  ASocket.SendString('Sec-WebSocket-Accept: ' + tempS + CRLF);
  ASocket.SendString('Sec-WebSocket-Protocol: openlcb.websocket' + CRLF);
  ASocket.SendString('' + CRLF);

  FreeAndNil(StringStream);
  FreeAndNil(Base64Stream);
end;

procedure TLccWebSocketServerThread.BuildAndSendInitializeFailureReply(ASocket: TTCPBlockSocket);
begin

end;

procedure TLccWebSocketServerThread.DoClientDisconnect;
begin
  if Assigned(OnClientDisconnect) then
    OnClientDisconnect(Self, FEthernetRec)
end;

procedure TLccWebSocketServerThread.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccWebSocketServerThread.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccWebSocketServerThread.DoReceiveMessage;
var
  L: TList;
  i: Integer;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event

    if Gridconnect then
    begin
      {$IFDEF LOGGING}
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
        PrintToSynEdit( 'R EthSrv: ' + EthernetRec.MessageStr,
                        Owner.LoggingFrame.SynEdit,
                        Owner.LoggingFrame.ActionLogPause.Checked,
                        Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                        Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
      {$ENDIF}

      if Assigned(OnReceiveMessage) then
        OnReceiveMessage(Self, FEthernetRec);

      case MsgAssembler.IncomingMessageGridConnect(FEthernetRec.MessageStr, WorkerMsg) of
        imgcr_True :
          begin
            if Owner.NodeManager <> nil then
              Owner.NodeManager.ProcessMessage(WorkerMsg);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

            if Owner.Hub then
            begin
              L := Owner.EthernetThreads.LockList;
              try
                for i := 0 to L.Count - 1 do
                begin
                  if TLccWebSocketServerThread(L[i]) <> Self then
                    TLccWebSocketServerThread(L[i]).SendMessage(WorkerMsg);
                end;
              finally
                Owner.EthernetThreads.UnlockList;
              end
            end
          end;
        imgcr_ErrorToSend :
          begin
            if Owner.NodeManager <> nil then
              if Owner.NodeManager.FindOwnedNodeBySourceID(WorkerMsg) <> nil then
                Owner.NodeManager.SendLccMessage(WorkerMsg);
          end;
      end
    end else
    begin   // TCP Protocol
      {$IFDEF LOGGING}
      if Assigned(Owner) and Assigned(Owner.LoggingFrame)  and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
        PrintTCPToSynEdit( 'EthSrv ...Receiving TCP...',
                        EthernetRec.MessageArray,
                        Owner.LoggingFrame.SynEdit,
                        Owner.LoggingFrame.ActionLogPause.Checked,
                        Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                        Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
      {$ENDIF}

      if Assigned(OnReceiveMessage) then
        OnReceiveMessage(Self, FEthernetRec);

      if WorkerMsg.LoadByLccTcp(FEthernetRec.MessageArray) then // In goes a raw message
      begin
        if (Owner.NodeManager <> nil) then
          Owner.NodeManager.ProcessMessage(WorkerMsg);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

        if Owner.Hub then
        begin
          L := Owner.EthernetThreads.LockList;
          try
            for i := 0 to L.Count - 1 do
            begin
              if TLccWebSocketServerThread(L[i]) <> Self then
                TLccWebSocketServerThread(L[i]).SendMessage(WorkerMsg);
            end;
          finally
            Owner.EthernetThreads.UnlockList;
          end
        end
      end
    end;
  end
end;

procedure TLccWebSocketServerThread.DoSendMessage(AMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, AMessage);
end;

initialization
  RegisterClass(TLccWebSocketServer);

finalization

end.

