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
  Synautil,
  {$ENDIF}
  lcc_threaded_stringlist,
  lcc_gridconnect,
  lcc_utilities,
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_ethernet_client,
  lcc_app_common_settings,
  lcc_node_messages_can_assembler_disassembler,
  lcc_common_classes,
  lcc_ethernet_common;

type
  TLccEthernetServerThread = class;     // Forward
  TLccEthernetServer = class;

  { TLccEthernetServerThread }

  TLccEthernetServerThread =  class(TLccBaseEthernetThread)
  protected
    procedure ReceiveMessage; override;
    procedure Execute; override;
  end;

  { TLccEthernetListener }

  TLccEthernetListener = class(TThread)
  private
    FConnectionInfo: TLccEthernetConnectionInfo;
    FGridConnect: Boolean;
    FOnClientDisconnect: TOnEthernetEvent;
    FOnConnectionStateChange: TOnEthernetEvent;
    FOnErrorMessage: TOnEthernetEvent;
    FOnReceiveMessage: TOnConnectionReceiveEvent;
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
    property ConnectionInfo: TLccEthernetConnectionInfo read FConnectionInfo write FConnectionInfo;
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
    function CreateThreadObject: TLccEthernetServerThread; virtual;
    {$ENDIF}
    procedure DoConnectionState;
    procedure DoErrorMessage;
    procedure DoReceiveMessage;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; AConnectionInfo: TLccEthernetConnectionInfo); reintroduce; virtual;
    destructor Destroy; override;

    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property OnClientDisconnect: TOnEthernetEvent read FOnClientDisconnect write FOnClientDisconnect;
    property OnConnectionStateChange: TOnEthernetEvent read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetEvent read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnConnectionReceiveEvent read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write FSleepCount;
  end;

  { TLccEthernetServer }

  TLccEthernetServer = class(TLccEthernetHardwareConnectionManager)
  private
    FHub: Boolean;
    FListenerThread: TLccEthernetListener;
    { Private declarations }
  protected
    { Protected declarations }
    function GetConnected: Boolean; override;
    function CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener; virtual;
    procedure UpdateAllThreadProperites; override;
    procedure UpdateListenerThreadProperites(AListenerThread: TLccEthernetListener);
    function IsLccLink: Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); override;
    destructor Destroy; override;

    function OpenConnection(AConnectionInfo: TLccHardwareConnectionInfo): TThread; override;
    procedure CloseConnection(EthernetThread: TLccConnectionThread);  override;

    property ListenerThread: TLccEthernetListener read FListenerThread write FListenerThread;

  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
  end;


procedure Register;

implementation

procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
  {$IFDEF FPC}
 //JDK {$I TLccEthernetServer.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccEthernetServer]);
  {$ENDIF}
end;


{ TLccEthernetListener }

constructor TLccEthernetListener.Create(CreateSuspended: Boolean;
  AnOwner: TLccEthernetServer; AConnectionInfo: TLccEthernetConnectionInfo);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FConnectionInfo := AConnectionInfo.Clone as TLccEthernetConnectionInfo;
  ConnectionInfo.Thread := nil;
  ConnectionInfo.LccMessage := TLccMessage.Create;
end;

{$IFDEF ULTIBO}
{$ELSE}
function TLccEthernetListener.CreateServerThread(ASocketHandle: TSocket): TLccEthernetServerThread;
begin
  Result := CreateThreadObject;
  Result.ListenerSocketHandle := ASocketHandle;    // Back create the sockets with this handle
  OnConnectionStateChange := OnConnectionStateChange;
  OnErrorMessage := OnErrorMessage;
  OnReceiveMessage := OnReceiveMessage;
  Result.SleepCount := FSleepCount;
  Result.GridConnect := FGridConnect;
  Result.UseSynchronize := Owner.UseSynchronize;
  Result.Start;
end;

function TLccEthernetListener.CreateThreadObject: TLccEthernetServerThread;
begin
   Result := TLccEthernetServerThread.Create(True, Owner, ConnectionInfo);
end;

{$ENDIF}

destructor TLccEthernetListener.Destroy;
begin
  FreeAndNil(FConnectionInfo);
  inherited Destroy;
end;

procedure TLccEthernetListener.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, ConnectionInfo)
end;

procedure TLccEthernetListener.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, ConnectionInfo)
  end;
end;

procedure TLccEthernetListener.DoReceiveMessage;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event
    if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, ConnectionInfo);
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
    ConnectionInfo.ConnectionState := NewConnectionState;
    Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
  end;

  procedure HandleErrorAndDisconnect;
  begin
    Owner.EthernetThreads.Remove(Self);
    ConnectionInfo.ErrorCode := Socket.LastError;
    ConnectionInfo.MessageStr := Socket.LastErrorDesc;
    Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
    SendConnectionNotification(ccsListenerDisconnected);
    Terminate
  end;

var
  NewLink: TLccEthernetServerThread;
begin
  FRunning := True;

  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := ConnectionInfo.HeartbeatRate;
  Socket.SetTimeout(0);
  SendConnectionNotification(ccsListenerConnecting);

  if ConnectionInfo.AutoResolveIP then
  begin
    {$IFDEF LCC_WINDOWS}
    ConnectionInfo.ListenerIP := ResolveWindowsIp(Socket);
    {$ELSE}
    ConnectionInfo.ListenerIP := ResolveUnixIp;
    {$ENDIF}
  end;

  Socket.Bind(String( ConnectionInfo.ListenerIP), String( IntToStr(ConnectionInfo.ListenerPort)));
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
          while not Terminated and (ConnectionInfo.ConnectionState = ccsListenerConnected) do
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

{ TLccEthernetServer }

procedure TLccEthernetServer.CloseConnection(
  EthernetThread: TLccConnectionThread);
var
  TimeCount: Integer;
begin
  inherited CloseConnection(EthernetThread);


  if Assigned(ListenerThread) then
  begin
    TimeCount := 0;
    ListenerThread.Terminate;
    {$IFDEF ULTIBO}
      // TODO
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

procedure TLccEthernetServer.UpdateListenerThreadProperites( AListenerThread: TLccEthernetListener);
begin
  if Assigned(AListenerThread) then
  begin
    AListenerThread.OnConnectionStateChange := OnConnectionStateChange;
    AListenerThread.OnErrorMessage := OnErrorMessage;
    AListenerThread.OnReceiveMessage := OnReceiveMessage;
    AListenerThread.OnSendMessage := OnSendMessage;
    AListenerThread.GridConnect := GridConnect;
  end;
end;

function TLccEthernetServer.IsLccLink: Boolean;
begin
  Result := True;
end;

function TLccEthernetServer.GetConnected: Boolean;
begin
  Result := Assigned(ListenerThread)
end;

procedure TLccEthernetServer.UpdateAllThreadProperites;
begin
  inherited UpdateAllThreadProperites;
  UpdateListenerThreadProperites(ListenerThread);
end;

constructor TLccEthernetServer.Create(AOwner: TComponent; ANodeManager: TLccNodeManager);
begin
  inherited;
  FHub := False;
end;

destructor TLccEthernetServer.Destroy;
begin
  inherited Destroy;
end;

function TLccEthernetServer.OpenConnection(AConnectionInfo: TLccHardwareConnectionInfo): TThread;
begin
  Result := inherited OpenConnection(AConnectionInfo);
  Result := CreateListenerObject(AConnectionInfo as TLccEthernetConnectionInfo);
  (Result as TLccEthernetListener).Owner := Self;
  UpdateListenerThreadProperites((Result as TLccEthernetListener));
  (Result as TLccEthernetListener).Suspended := False;
  ListenerThread := (Result as TLccEthernetListener);
end;

function TLccEthernetServer.CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener;
begin
  Result := TLccEthernetListener.Create(True, Self, AConnectionInfo);
end;

{ TLccEthernetServerThread }

{$IFDEF ULTIBO}
procedure TLccEthernetServerThread.Execute;
begin

end;
{$ELSE}
procedure TLccEthernetServerThread.Execute;
var
  GridConnectHelper: TGridConnectHelper;
  LocalSleepCount: Integer;
begin
  FRunning := True;

  HandleSendConnectionNotification(ccsListenerClientConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := ConnectionInfo.HeartbeatRate;
  Socket.SetTimeout(0);
  Socket.Socket := ListenerSocketHandle;    // Read back the handle
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
    ConnectionInfo.ClientIP := Socket.GetRemoteSinIP;
    ConnectionInfo.ClientPort := Socket.GetRemoteSinPort;
    ConnectionInfo.ListenerIP := Socket.GetLocalSinIP;
    ConnectionInfo.ListenerPort := Socket.GetLocalSinPort;
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
          while not IsTerminated and (ConnectionInfo.ConnectionState = ccsListenerClientConnected) do
          begin  // Handle the Socket using GridConnect
            if Gridconnect then
            begin
              if LocalSleepCount >= SleepCount then
              begin
                TryTransmitGridConnect(True);
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              TryReceiveGridConnect(GridConnectHelper, True);
            end else
            begin    // Handle the Socket with LCC TCP Protocol
              if LocalSleepCount >= SleepCount then
              begin
                TryTransmitTCPProtocol(True);
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              TryReceiveTCPProtocol(True);
            end;
          end;
        finally
          HandleSendConnectionNotification(ccsListenerClientDisconnecting);
          if Gridconnect then
            TryTransmitGridConnect(False) // Flush it
          else
            TryTransmitTCPProtocol(False);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        HandleSendConnectionNotification(ccsListenerClientDisconnected);
        (Owner as TLccEthernetHardwareConnectionManager).EthernetThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

{$ENDIF}

procedure TLccEthernetServerThread.ReceiveMessage;
var
  L: TList;
  i: Integer;
begin
  // Called in the content of the main thread through Syncronize

  // here is where we call backwards into the Connection Manager that owns this thread.
  // The Connection Manager then call back into the Node Manager.  The Node Manager passes
  // the message to its nodes plus then stars back out to any other Connection Managers
  // that are registered (as we are registered).  The Manager decendant must override
  // IsLccLink and return TRUE in order to be included in this system
  inherited ReceiveMessage;

  // Now we look upwards into any thread we have that depend on use to pass on messages
  // assuming we are a Hub.
  if (Owner as TLccEthernetServer).Hub then
  begin
    L := (Owner as TLccEthernetHardwareConnectionManager).EthernetThreads.LockList;
    try
      for i := 0 to L.Count - 1 do
      begin
        if TLccEthernetServerThread(L[i]) <> Self then
          TLccEthernetServerThread(L[i]).SendMessage(ConnectionInfo.LccMessage);
      end;
    finally
      (Owner as TLccEthernetHardwareConnectionManager).EthernetThreads.UnlockList;
    end
  end;
end;

initialization
  RegisterClass(TLccEthernetServer);

finalization

end.

