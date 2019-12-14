unit lcc_ethernet_server;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
  //  {$DEFINE LOGGING}
  {$ENDIF}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP} LResources, Forms, Controls, Graphics, Dialogs, {$ENDIF}
  {$ELSE}
  FMX.Forms, Types, System.Generics.Collections,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging,
  lcc_detailed_logging,
  {$ENDIF}

  {$IFDEF ULTIBO}
  lcc_threaded_stringlist,
  Winsock2,
  Console,
  {$ELSE}
  blcksock,
  synsock,
  {$ENDIF}
  lcc_gridconnect,
  lcc_utilities,
  lcc_defines,
  lcc_node_messages_can_assembler_disassembler,
  lcc_node_manager,
  lcc_node_messages,
  lcc_ethernet_client,
  lcc_threaded_circulararray,
  lcc_ethernet_tcp,
  lcc_app_common_settings,
  lcc_common_classes;

type
  TLccEthernetServerThread = class;     // Forward
  TLccEthernetServer = class;

  { TLccEthernetServerThread }

  TLccEthernetServerThread =  class(TLccConnectionThread)
    private
      FEthernetRec: TLccEthernetRec;
      FOnClientDisconnect: TOnEthernetRecFunc;
      FOnErrorMessage: TOnEthernetRecFunc;
      FOnConnectionStateChange: TOnEthernetRecFunc;
      FOnReceiveMessage: TOnEthernetReceiveFunc;
      FOnSendMessage: TOnMessageEvent;
      FOwner: TLccEthernetServer;
      {$IFDEF ULTIBO}
      {$ELSE}
      FSocket: TTCPBlockSocket;
      FSocketHandleForListener: TSocket;
      {$ENDIF}
      FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
    protected
      procedure DoClientDisconnect;
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage);

      property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
      {$IFDEF ULTIBO}
      {$ELSE}
      property Socket: TTCPBlockSocket read FSocket write FSocket;
      property SocketHandleForListener: TSocket read FSocketHandleForListener write FSocketHandleForListener;
      {$ENDIF}
      property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
      property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
      property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
      property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
      property Owner: TLccEthernetServer read FOwner write FOwner;
      property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
    public
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec); reintroduce;
      destructor Destroy; override;
  end;

  { TLccEthernetThreadList }

  TLccEthernetThreadList = class(TThreadList)      // Contains TServerSocketThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure CloseEthernetPorts;
    procedure CloseEthernetPort(EthernetThread: TLccEthernetServerThread);

    property Count: Integer read GetCount;
  end;

  { TLccEthernetListener }

  TLccEthernetListener = class(TThread)
  private
    FEthernetRec: TLccEthernetRec;
    FGridConnect: Boolean;
    FOnClientDisconnect: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FOwner: TLccEthernetServer;
    FRunning: Boolean;
    FSleepCount: Integer;
    {$IFDEF ULTIBO}
    FStringList: TThreadStringList;
    FTcpServer: TWinsock2TCPServer;
    {$ELSE}
    FSocket: TTCPBlockSocket;
    {$ENDIF}
    function GetIsTerminated: Boolean;
  protected
    property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
    property Owner: TLccEthernetServer read FOwner write FOwner;
    property Running: Boolean read FRunning write FRunning;
    {$IFDEF ULTIBO}
    property StringList: TThreadStringList read FStringList write FStringList;
    property TcpServer: TWinsock2TCPServer read FTcpServer write FTcpServer;
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
    property IsTerminated: Boolean read GetIsTerminated;

    {$IFDEF ULTIBO}
    {$ELSE}
    function CreateServerThread(ASocketHandle: TSocket): TLccEthernetServerThread;
    {$ENDIF}
    procedure DoConnectionState;
    procedure DoErrorMessage;
    procedure DoReceiveMessage;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec); reintroduce; virtual;
    destructor Destroy; override;

    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write FSleepCount;
  end;

  { TLccEthernetServer }

  TLccEthernetServer = class(TLccHardwareConnectionManager)
  private
    FEthernetThreads: TLccEthernetThreadList;
    FGridConnect: Boolean;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FListenerThread: TLccEthernetListener;
    {$IFDEF LOGGING}FLoggingFrame: TFrameLccLogging;{$ENDIF}
    FNodeManager: TLccNodeManager;
    FOnClientDisconnect: TOnEthernetRecFunc;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FSleepCount: Integer;
    function GetConnected: Boolean;
    procedure SetGridConnect(AValue: Boolean);
    procedure SetSleepCount(AValue: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateThreadEvents(EthernetThread: TLccEthernetServerThread);
    procedure UpdateThreadsEvents;
    procedure UpdateListenerEvents(AListenerThread: TLccEthernetListener);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
    function OpenConnectionWithLccSettings: TLccEthernetListener;
    procedure CloseConnection( EthernetThread: TLccEthernetServerThread);
    procedure SendMessage(AMessage: TLccMessage);  override;
    procedure SendMessageRawGridConnect(GridConnectStr: String); override;

    property Connected: Boolean read GetConnected;
    property EthernetThreads: TLccEthernetThreadList read FEthernetThreads write FEthernetThreads;
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector
    property ListenerThread: TLccEthernetListener read FListenerThread write FListenerThread;

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

//JDK procedure Register;

implementation

//JDK
(*
procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
  {$IFDEF FPC}
  {$I TLccEthernetServer.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccEthernetServer]);
  {$ENDIF}
end;
*)

{ TLccEthernetListener }

constructor TLccEthernetListener.Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec);
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

{$IFDEF ULTIBO}
{$ELSE}
function TLccEthernetListener.CreateServerThread(ASocketHandle: TSocket): TLccEthernetServerThread;
begin
  Result := TLccEthernetServerThread.Create(True, Owner, FEthernetRec);
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
{$ENDIF}

destructor TLccEthernetListener.Destroy;
begin
//  FreeAndNil( FOutgoingGridConnect);
  FreeAndNil(FEthernetRec.LccMessage);
 // FreeAndNil(FMsgAssembler);
 // FreeandNil(FMsgDisAssembler);
//  FreeAndNIl(FWorkerMsg);
  inherited Destroy;
end;

procedure TLccEthernetListener.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccEthernetListener.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccEthernetListener.DoReceiveMessage;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event
    if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, FEthernetRec);
  end
end;

{$IFDEF ULTIBO}
procedure TLccEthernetListener.Execute;
begin

end;
{$ELSE}
procedure TLccEthernetListener.Execute;

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
  NewLink: TLccEthernetServerThread;
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
{$ENDIF}

function TLccEthernetListener.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

{ TLccEthernetThreadList }

function TLccEthernetThreadList.GetCount: Integer;
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

destructor TLccEthernetThreadList.Destroy;
begin
  CloseEthernetPorts;
  inherited Destroy;
end;

procedure TLccEthernetThreadList.CloseEthernetPorts;
var
  L: TList;
  Thread: TLccEthernetServerThread;
begin
  while Count > 0 do
  begin
    L := LockList;
    try
      Thread := TLccEthernetServerThread( L[0]);
      L.Delete(0);
    finally
      UnlockList;
    end;
    CloseEthernetPort(Thread);
  end;
end;

procedure TLccEthernetThreadList.CloseEthernetPort( EthernetThread: TLccEthernetServerThread);
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
      {$IFDEF ULTIBO}
      {$ELSE}
      if Assigned(EthernetThread.Socket) then
        EthernetThread.Socket.CloseSocket
      else
        Break // Something went really wrong
      {$ENDIF}
    end;
  end;
  {$IFDEF ULTIBO}
  {$ELSE}
  FreeAndNil( EthernetThread);
  {$ENDIF}
end;

{ TLccEthernetServer }

procedure TLccEthernetServer.CloseConnection(EthernetThread: TLccEthernetServerThread);
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
    {$IFDEF ULTIBO}
    {$ELSE}
    if Assigned(ListenerThread.Socket) then
      ListenerThread.Socket.CloseSocket;  // Force out of wait state with an error
    {$ENDIF}
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
        {$IFDEF ULTIBO}
        {$ELSE}
         if Assigned(ListenerThread.Socket) then
           ListenerThread.Socket.CloseSocket
         else
           Break // Something went really wrong
        {$ENDIF}
      end;
    end;
    FreeAndNil(FListenerThread);
  end;
end;

procedure TLccEthernetServer.SetSleepCount(AValue: Integer);
begin
    if AValue <> FSleepCount then
  begin
    FSleepCount := AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread);
  end;
end;

procedure TLccEthernetServer.UpdateListenerEvents( AListenerThread: TLccEthernetListener);
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

procedure TLccEthernetServer.UpdateThreadEvents(EthernetThread: TLccEthernetServerThread);
begin
  EthernetThread.OnSendMessage := OnSendMessage;
  EthernetThread.SleepCount := SleepCount;
  EthernetThread.GridConnect := FGridConnect;
end;

procedure TLccEthernetServer.UpdateThreadsEvents;
var
  i: Integer;
  L: TList;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
      UpdateThreadEvents(TLccEthernetServerThread( L[i]));
  finally
    EthernetThreads.UnlockList;
  end;
end;

constructor TLccEthernetServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEthernetThreads := TLccEthernetThreadList.Create;
  FHub := False;
end;

destructor TLccEthernetServer.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  inherited Destroy;
end;

function TLccEthernetServer.OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
begin
  Result := TLccEthernetListener.Create(True, Self, AnEthernetRec);
  Result.Owner := Self;
  UpdateListenerEvents(Result);
  Result.Suspended := False;
  ListenerThread := Result;
end;

function TLccEthernetServer.OpenConnectionWithLccSettings: TLccEthernetListener;
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

procedure TLccEthernetServer.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
  L: TList;
  EthernetThread: TLccEthernetServerThread;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccEthernetServerThread( L[i]);
      if not EthernetThread.IsTerminated then
        EthernetThread.SendMessage(AMessage);
    end;
  finally
    EthernetThreads.UnlockList;
  end;
end;

procedure TLccEthernetServer.SendMessageRawGridConnect(GridConnectStr: String);
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

procedure TLccEthernetServer.SetGridConnect(AValue: Boolean);
begin
  if AValue <> FGridConnect then
  begin
    FGridConnect:=AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread);
  end;

end;

function TLccEthernetServer.GetConnected: Boolean;
begin
  Result := Assigned(ListenerThread);
end;

{ TLccEthernetServerThread }

{$IFDEF ULTIBO}
procedure TLccEthernetServerThread.Execute;
begin

end;
{$ELSE}
procedure TLccEthernetServerThread.Execute;

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
    SendConnectionNotification(ccsListenerClientDisconnected);
    Terminate;
  end;

var
  TxStr: String;
  RcvByte: Byte;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList: TStringList;
  DynamicByteArray: TDynamicByteArray;
  LocalSleepCount: Integer;
begin
  FRunning := True;

  SendConnectionNotification(ccsListenerClientConnecting);
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
      SendConnectionNotification(ccsListenerClientConnected);
      try
        try
          LocalSleepCount := 0;
          while not IsTerminated and (FEthernetRec.ConnectionState = ccsListenerClientConnected) do
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

                if TxStr <> '' then
                begin
                  Socket.SendString(String( TxStr) + LF);
                  if Socket.LastError <> 0 then
                    HandleErrorAndDisconnect;
                end;
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);


              RcvByte := Socket.RecvByte(1);
              case Socket.LastError of
                0 :
                  begin
                    GridConnectStrPtr := nil;
                    if GridConnectHelper.GridConnect_DecodeMachine(RcvByte, GridConnectStrPtr) then
                    begin
                      FEthernetRec.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                      FEthernetRec.LccMessage.LoadByGridConnectStr(FEthernetRec.MessageStr);
                      Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                    end;
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
              end;
            end else
            begin    // Handle the Socket with LCC TCP Protocol
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
              end;
            end;
          end;
        finally
          SendConnectionNotification(ccsListenerClientDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        SendConnectionNotification(ccsListenerClientDisconnected);
        Owner.EthernetThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;
{$ENDIF}

procedure TLccEthernetServerThread.SendMessage(AMessage: TLccMessage);
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

constructor TLccEthernetServerThread.Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := Self;
  FEthernetRec.LccMessage := TLccMessage.Create;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
end;

destructor TLccEthernetServerThread.Destroy;
begin
  FreeAndNil(FEthernetRec.LccMessage);
  FreeAndNil(FTcpDecodeStateMachine);
  inherited Destroy;
end;

procedure TLccEthernetServerThread.DoClientDisconnect;
begin
  if Assigned(OnClientDisconnect) then
    OnClientDisconnect(Self, FEthernetRec)
end;

procedure TLccEthernetServerThread.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccEthernetServerThread.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccEthernetServerThread.DoReceiveMessage;
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
                  if TLccEthernetServerThread(L[i]) <> Self then
                    TLccEthernetServerThread(L[i]).SendMessage(WorkerMsg);
                end;
              finally
                Owner.EthernetThreads.UnlockList;
              end
            end
          end;
        imgcr_ErrorToSend :
          begin
            if Owner.NodeManager <> nil then
              if Owner.NodeManager.FindOwnedNodeBySourceID(WorkerMsg) <> nil then  // We decode ALL messages so only send the error if it was for our nodes
                Owner.NodeManager.LccMessageSend(WorkerMsg);
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
              if TLccEthernetServerThread(L[i]) <> Self then
                TLccEthernetServerThread(L[i]).SendMessage(WorkerMsg);
            end;
          finally
            Owner.EthernetThreads.UnlockList;
          end
        end
      end
    end;
  end
end;

procedure TLccEthernetServerThread.DoSendMessage(AMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, AMessage);
end;

initialization
  RegisterClass(TLccEthernetServer);

finalization

end.

