unit lcc_common_classes;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP}Forms, {$ENDIF}
  {$ELSE}
  FMX.Forms,
  {$ENDIF}

  {$IFDEF FPC}
  {$ELSE}
    System.Generics.Collections,
  {$ENDIF}
  {$IFDEF ULTIBO}
  Winsock2,
  Console,
  {$ELSE}
  blcksock,
  synsock,
  {$ENDIF}
  lcc_node_messages,
  lcc_node_manager,
  lcc_app_common_settings,
  lcc_threaded_circulararray,
  lcc_ethernet_tcp,
  lcc_threaded_stringlist,
  lcc_defines,
  lcc_node_messages_can_assembler_disassembler;

type
  TLccHardwareConnectionManager = class;
  TLccHardwareConnectionInfo = class;
  TLccConnectionThread = class;

  TOnConnectionReceiveEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;
  TOnHardwareConnectionStateChangeEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;
  TOnHardwareConnectionErrorEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;

  { TLccHardwareConnectionInfo }

  TLccHardwareConnectionInfo = class
  private
    FConnectionState: TLccConnectionState;
    FErrorCode: Integer;
    FGridConnect: Boolean;
    FHub: Boolean;
    FLccMessage: TLccMessage;
    FMessageStr: String;
    FSleepCofunt: Integer;
    FSleepCount: Integer;
    FSuppressErrorMessages: Boolean;
    FThread: TLccConnectionThread;
    FUseSyncronize: Boolean;
  public
    MessageArray: TLccDynamicByteArray;                                         // Contains the TCP Protocol message bytes of not using GridConnect

    constructor Create;
    destructor Destroy; override;

    function Clone: TLccHardwareConnectionInfo; virtual;

    property ConnectionState: TLccConnectionState read FConnectionState write FConnectionState;  // Current State of the connection
    property GridConnect: Boolean read FGridConnect write FGridConnect;
    property Thread: TLccConnectionThread read FThread write FThread;
    property Hub: Boolean read FHub write FHub;
    property LccMessage: TLccMessage read FLccMessage write FLccMessage;
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property MessageStr: String read FMessageStr write FMessageStr;             // Contains the string for the resulting message from the thread
    property SleepCount: Integer read FSleepCount write FSleepCofunt;
    property SuppressErrorMessages: Boolean read FSuppressErrorMessages write FSuppressErrorMessages;
    property UseSyncronize: Boolean read FUseSyncronize write FUseSyncronize;
  end;

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FConnectionInfo: TLccHardwareConnectionInfo;
    {$IFDEF ULTIBO}
    {$ELSE}
    FListenerSocketHandle: TSocket;
    {$ENDIF}
    FMsgStringList: TStringList;
    FOutgoingCircularArray: TThreadedCirularArray;
    FOutgoingGridConnect: TThreadStringList;
    FOwner: TLccHardwareConnectionManager;
    FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
    FWorkerMessage: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;

    property Owner: TLccHardwareConnectionManager read FOwner write FOwner;
    property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;

    procedure HandleErrorAndDisconnect(SuppressMessage: Boolean); virtual;
    procedure HandleSendConnectionNotification(NewConnectionState: TLccConnectionState); virtual;

    procedure SendMessage(AMessage: TLccMessage); virtual;
    procedure ReceiveMessage; virtual;
    procedure ErrorMessage; virtual;
    procedure RequestErrorMessageSent; virtual;
    procedure ConnectionStateChange; virtual;
    procedure ForceTerminate; virtual;  // Override to implement something to force a thread loop to terminate if it is in an infinate wait state or something

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); reintroduce; virtual;
    destructor Destroy; override;

    {$IFDEF ULTIBO}
    {$ELSE}
    // If the thread was created by a Listener (server) this is socket the listener created.  You will need this to assign a socket to the new thread
    property ListenerSocketHandle: TSocket read FListenerSocketHandle write FListenerSocketHandle;
    {$ENDIF}
    property ConnectionInfo: TLccHardwareConnectionInfo read FConnectionInfo;
    property MsgStringList: TStringList read FMsgStringList write FMsgStringList;
    property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
    property OutgoingCircularArray: TThreadedCirularArray read FOutgoingCircularArray write FOutgoingCircularArray;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;

    { TLccComPortThreadList }

  { TLccConnectionThreadList }

  TLccConnectionThreadList = class(TThreadList)      // Contains TClientSocketThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure CloseConnections;
    procedure CloseConnection(ConnectionThread: TLccConnectionThread);

    property Count: Integer read GetCount;
  end;

  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent, IHardwareConnectionManagerLink)
  private
    FConnectionThreads: TLccConnectionThreadList;
    FHub: Boolean;
    FNodeManager: TLccNodeManager;
    FOnConnectionStateChange: TOnHardwareConnectionStateChangeEvent;
    FOnErrorMessage: TOnHardwareConnectionErrorEvent;
    FOnReceiveMessage: TOnConnectionReceiveEvent;
    FOnSendMessage: TOnMessageEvent;
    FWorkerMessage: TLccMessage;
  protected
    FIncomingCircularArray: TThreadedCirularArray;
    FIncomingGridConnect: TThreadStringList;
    // useful in decendants, GetConnected could just return this with the object setting FConnected correctly
    FConnected: Boolean;

    // IHardwareConnectionManagerLink
    function GetConnected: Boolean;

    property Hub: Boolean read FHub write FHub;

    // Event call method
    procedure DoReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // Event call method
    procedure DoSendMessage(ALccMessage: TLccMessage); virtual;
    // Event call method
    procedure DoConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // Event call method
    procedure DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;


    // Decendants override this to tell the Node Manager if this Connection is used to move Lcc packets or not (HTTP server, ComPort with custom protocol server are examples on "no")
    function IsLccLink: Boolean; virtual; abstract;  // IHardwareConnectionManagerLink
    // Call to implement a Hub
    procedure RelayMessageToOtherThreads(SourceThread: TLccConnectionThread; ALccMessage: TLccMessage);

  public
    // Threads running with connections
    property ConnectionThreads: TLccConnectionThreadList read FConnectionThreads write FConnectionThreads;
    // True if the Manager is capabable of receiveing/sending messages on the wire... getter must be overridden
    property Connected: Boolean read GetConnected;
    // Decendants can use this to move gridconnect strings to a thread for it to pick up and put on the wire
    property IncomingGridConnect: TThreadStringList read FIncomingGridConnect;
    // Decendants can use this to move raw data to a thread to put on the wire (Ethernet TCP LCC for instance)
    property IncomingCircularArray: TThreadedCirularArray read FIncomingCircularArray;
    // The Connection Mangaer is assigned to this Connection Manager and it uses it to pass messages
    property NodeManager: TLccNodeManager read FNodeManager;

    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); reintroduce; virtual;
    destructor Destroy; override;

    // When a thread owned by the manager receives a message it will call these centraized methods
    // ----------------------
    // Decendant must override this.  The Connection Threads call when a message come in on the "wire".
    procedure ReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // Decendant must override this.  The Node Manager calls this when its nodes needs to send a message to the "wire".
    procedure SendMessage(ALccMessage: TLccMessage); virtual;
    // Puts a GridConnect string in the buffer to be sent without needing to deal with a TLccMessage
    // IHardwareConnectionManagerLink
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual;
    // When a thread owned by the manager receives a message it will call this centraized method
    procedure ConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // Decendant must override this.  The Node Manager calls this when its nodes needs to send a message to the "wire".
    procedure ErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // ----------------------

    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; virtual;
    function OpenConnectionWithLccSettings: TLccConnectionThread; virtual;
    procedure CloseConnection(ConnectionThread: TLccConnectionThread); virtual;

  published
    property OnConnectionStateChange: TOnHardwareConnectionStateChangeEvent read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnHardwareConnectionErrorEvent read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnConnectionReceiveEvent read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;


implementation

{ TLccConnectionThreadList }

function TLccConnectionThreadList.GetCount: Integer;
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

destructor TLccConnectionThreadList.Destroy;
begin
  CloseConnections;
  inherited Destroy;
end;

procedure TLccConnectionThreadList.CloseConnections;
var
  L: TList;
  ConnectionThread: TLccConnectionThread;
begin
  while Count > 0 do
  begin
    L := LockList;
    try
      ConnectionThread := TLccConnectionThread( L[0]); // Thread takes itself out of the list when done
    finally
      UnlockList;
    end;
    CloseConnection(ConnectionThread);
  end;
end;

procedure TLccConnectionThreadList.CloseConnection(ConnectionThread: TLccConnectionThread);
var
  TimeCount: Cardinal;
begin
  TimeCount := 0;
  ConnectionThread.Terminate;
  while (ConnectionThread.Running) do
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
      ConnectionThread.ForceTerminate;
      {$ENDIF}
    end;
    if TimeCount = 20 then
      Break;  // Give up....
  end;
  FreeAndNil( ConnectionThread);
end;

{ TLccHardwareConnectionInfo }

constructor TLccHardwareConnectionInfo.Create;
begin
  inherited;
  FLccMessage := TLccMessage.Create;
  FConnectionState := lcsDisconnected;
  FUseSyncronize := True;
end;

function TLccHardwareConnectionInfo.Clone: TLccHardwareConnectionInfo;
begin
  Result := Self.ClassType.Create as TLccHardwareConnectionInfo;
  Result.ConnectionState := ConnectionState;
  Result.GridConnect := GridConnect;;
  Result.Thread := Thread;
  Result.Hub := Hub;
  Result.ErrorCode := ErrorCode;
  Result.MessageStr := MessageStr;
  Result.MessageArray := MessageArray;
  Result.SleepCount := SleepCount;
  Result.UseSyncronize := UseSyncronize;
  Result.FLccMessage := TLccMessage.Create;
  Result.SuppressErrorMessages := SuppressErrorMessages;
end;

destructor TLccHardwareConnectionInfo.Destroy;
begin
  FreeAndNil(FLccMessage);
  inherited Destroy;
end;


{ TLccHardwareConnectionManager }

function TLccHardwareConnectionManager.GetConnected: Boolean;
var
  LocalThreads: TList;
  i: Integer;
begin
  Result := False;
  LocalThreads := ConnectionThreads.LockList;
  try
    i := 0;
    while (i < LocalThreads.Count) and not Result do
    begin
      Result := TLccConnectionThread(LocalThreads[i]).ConnectionInfo.ConnectionState = lcsConnected;
      Inc(i);
    end;
  finally
    ConnectionThreads.UnlockList;
  end;
end;

procedure TLccHardwareConnectionManager.DoReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnReceiveMessage) then
    OnReceiveMessage(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.DoSendMessage(ALccMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, ALccMessage);
end;

procedure TLccHardwareConnectionManager.DoConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.RelayMessageToOtherThreads(SourceThread: TLccConnectionThread; ALccMessage: TLccMessage);
var
  L: TList;
  i: Integer;
begin
  L :=ConnectionThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      if TLccConnectionThread(L[i]) <> SourceThread then
        TLccConnectionThread(L[i]).SendMessage(ALccMessage);
    end;
  finally
    ConnectionThreads.UnlockList;
  end;

  // May be useful when TCP is being implmented....

  {
    // Called in the content of the main thread through Syncronize
  if ConnectionInfo.Gridconnect then
  begin
    if Owner.NodeManager <> nil then
      Owner.NodeManager.ProcessMessage(ConnectionInfo.LccMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

    if (Owner as TLccComPort).Hub then
    begin
      L := (Owner as TLccComPort).ComPortThreads.LockList;
      try
        for i := 0 to L.Count - 1 do
        begin
          if TLccComPortThread(L[i]) <> Self then
            TLccComPortThread(L[i]).SendMessage(ConnectionInfo.LccMessage);
        end;
      finally
        (Owner as TLccComPort).ComPortThreads.UnlockList;
      end
    end;
  end else
  begin   // TCP Protocol
    if WorkerMessage.LoadByLccTcp(ConnectionInfo.MessageArray) then // In goes a raw message
    begin
      if (Owner.NodeManager <> nil) then
        Owner.NodeManager.ProcessMessage(WorkerMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

      if (Owner as TLccComPort).Hub then
      begin
        L := (Owner as TLccComPort).ComPortThreads.LockList;
        try
          for i := 0 to L.Count - 1 do
          begin
            if TLccComPortThread(L[i]) <> Self then
              TLccComPortThread(L[i]).SendMessage(WorkerMessage);
          end;
        finally
          (Owner as TLccComPort).ComPortThreads.UnlockList;
        end
      end
    end
  end }
end;

procedure TLccHardwareConnectionManager.ReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin

  // Now send the message to the NodeManager to fan out to all the nodes and other Hardware Connection Managers it owns
  if ConnectionInfo.GridConnect then
    NodeManager.ReceiveMessage(Self as IHardwareConnectionManagerLink, ConnectionInfo.LccMessage)
  else begin
    if WorkerMessage.LoadByLccTcp(ConnectionInfo.MessageArray) then // In goes a raw message
      NodeManager.ReceiveMessage(Self as IHardwareConnectionManagerLink, WorkerMessage);
  end;


  // Act as a Hub and fan out the message to all other threads that this Connection Manager owns
  if Hub then
    RelayMessageToOtherThreads(Thread, Connectioninfo.LccMessage);

  DoReceiveMessage(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.SendMessage(ALccMessage: TLccMessage);
var
  i: Integer;
  L: TList;
  ConnectionThread: TLccConnectionThread;
begin
  DoSendMessage(ALccMessage);

  L := ConnectionThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      ConnectionThread := TLccConnectionThread( L[i]);
      ConnectionThread.SendMessage(ALccMessage);
    end;
  finally
    ConnectionThreads.UnlockList;
  end;
end;

procedure TLccHardwareConnectionManager.ConnectionStateChange(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  NodeManager.HardwareConnectionLinkNotifyConnectionChange(Self);
  DoConnectionStateChange(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.ErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  DoErrorMessage(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.SendMessageRawGridConnect(GridConnectStr: String);
var
  List: TList;
  StrList: TStringList;
  i: Integer;
  OldText, NewText: ansistring;
begin
  List := ConnectionThreads.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      StrList := TLccConnectionThread(List[i]).OutgoingGridConnect.LockList;
      try
        StrList.Delimiter := Chr(10);
        OldText := StrList.DelimitedText;
      if OldText <> '' then
      begin
        StrList.DelimitedText := GridConnectStr;
        NewText := StrList.DelimitedText;
        StrList.DelimitedText := OldText + Chr(10) + NewText
      end else
        StrList.DelimitedText := GridConnectStr;
      finally
         TLccConnectionThread(List[i]).OutgoingGridConnect.UnlockList;
      end;
    end;
  finally
    ConnectionThreads.UnlockList;
  end;
end;

function TLccHardwareConnectionManager.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  Result := nil;
  FHub := ConnectionInfo.Hub;
end;

function TLccHardwareConnectionManager.OpenConnectionWithLccSettings: TLccConnectionThread;
begin
  Result := nil;   // Override
end;

procedure TLccHardwareConnectionManager.CloseConnection(ConnectionThread: TLccConnectionThread);
begin
   if Assigned(ConnectionThread) then
  begin
    ConnectionThreads.CloseConnection(ConnectionThread);
    ConnectionThreads.Remove(ConnectionThread);
    {$IFDEF ULTIBO}
    {$ELSE}
    FreeAndNil( ConnectionThread);
    {$ENDIF}
  end else
    ConnectionThreads.CloseConnections;
end;

constructor TLccHardwareConnectionManager.Create(AOwner: TComponent; ANodeManager: TLccNodeManager);
begin
  inherited Create(AOwner);
  FNodeManager := ANodeManager;
  if Assigned(NodeManager) then
    NodeManager.RegisterHardwareConnectionLink(Self as IHardwareConnectionManagerLink);
  FIncomingGridConnect := TThreadStringList.Create;
  FIncomingCircularArray := TThreadedCirularArray.Create;
  FWorkerMessage := TLccMessage.Create;
  FConnectionThreads := TLccConnectionThreadList.Create;
end;

destructor TLccHardwareConnectionManager.Destroy;
begin
  if Assigned(NodeManager) then
    NodeManager.UnRegisterHardwareConnectionLink(Self as IHardwareConnectionManagerLink);
  FreeAndNil(FConnectionThreads);
  FreeAndNil(FIncomingCircularArray);
  FreeAndNil(FIncomingGridConnect);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

{ TLccConnectionThread }

constructor TLccConnectionThread.Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FConnectionInfo := AConnectionInfo.Clone;
  FConnectionInfo.Thread := Self;
  FConnectionInfo.ConnectionState := lcsDisconnected;
  FConnectionInfo.ErrorCode := 0;
  FConnectionInfo.FMessageStr := '';
  FWorkerMessage := TLccMessage.Create;
  FMsgStringList := TStringList.Create;
  FOutgoingCircularArray := TThreadedCirularArray.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
  OutgoingGridConnect.Delimiter := #10;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
end;

destructor TLccConnectionThread.Destroy;
begin
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FMsgStringList);
  FreeAndNil(FOutgoingCircularArray);
  FreeAndNil(FOutgoingGridConnect);
  FreeAndNil(FTcpDecodeStateMachine);
  inherited Destroy;
end;

function TLccConnectionThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TLccConnectionThread.HandleErrorAndDisconnect(SuppressMessage: Boolean);
begin
  Owner.ConnectionThreads.Remove(Self);
  if not SuppressMessage and (ConnectionInfo.ErrorCode <> 0) then
    Synchronize({$IFDEF FPC}@{$ENDIF}ErrorMessage);
  HandleSendConnectionNotification(lcsDisconnected);
  Terminate;
end;

procedure TLccConnectionThread.HandleSendConnectionNotification(NewConnectionState: TLccConnectionState);
begin
  ConnectionInfo.ConnectionState := NewConnectionState;
  Synchronize({$IFDEF FPC}@{$ENDIF}ConnectionStateChange);
end;

procedure TLccConnectionThread.SendMessage(AMessage: TLccMessage);
var
  ByteArray: TLccDynamicByteArray;
  i: Integer;
begin
  if ConnectionInfo.GridConnect then
  begin
    MsgStringList.Text := AMessage.ConvertToGridConnectStr(#10, False);
    for i := 0 to MsgStringList.Count - 1 do
      OutgoingGridConnect.Add(MsgStringList[i]);
  end else
  begin
    ByteArray := nil;
    if AMessage.ConvertToLccTcp(ByteArray) then
      OutgoingCircularArray.AddChunk(ByteArray);
  end;
end;

procedure TLccConnectionThread.ReceiveMessage;
begin
  // here is where we call backwards into the Connection Manager that owns this thread.
  // The Connection Manager then call back into the Node Manager.  The Node Manager passes
  // the message to its nodes plus then back out to any other Connection Managers
  // that are registered.  The Manager decendant must override
  // IsLccLink and return TRUE in order to be included in this system
  Owner.ReceiveMessage(Self, ConnectionInfo);
end;

procedure TLccConnectionThread.ErrorMessage;
// Called in context of main thread from connection thread
var
  LocalConnectionInfo: TLccHardwareConnectionInfo;
begin
  LocalConnectionInfo := ConnectionInfo.Clone;
  try
    Owner.ErrorMessage(Self, LocalConnectionInfo);
  finally
    LocalConnectionInfo.Free
  end;
end;

procedure TLccConnectionThread.RequestErrorMessageSent;
begin

end;

procedure TLccConnectionThread.ConnectionStateChange;
// Called in context of main thread from connection thread
var
  LocalConnectionInfo: TLccHardwareConnectionInfo;
begin
  LocalConnectionInfo := ConnectionInfo.Clone;
  try
    Owner.ConnectionStateChange(Self, LocalConnectionInfo);
  finally
    LocalConnectionInfo.Free
  end;
end;

procedure TLccConnectionThread.ForceTerminate;
begin

end;


end.

