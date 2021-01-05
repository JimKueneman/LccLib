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
  lcc_threaded_stringlist,
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
  lcc_alias_server,
  lcc_defines,
  lcc_gridconnect,
  lcc_node_messages_can_assembler_disassembler;

type
  TLccHardwareConnectionManager = class;
  TLccHardwareConnectionInfo = class;
  TLccConnectionThread = class;

  TOnConnectionReceiveEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;

  { TLccHardwareConnectionInfo }

  TLccHardwareConnectionInfo = class
  private
    FErrorCode: Integer;
    FLccMessage: TLccMessage;
    FMessageStr: String;
    FThread: TLccConnectionThread;
    FThreads: TLccConnectionThread;
  public
    MessageArray: TLccDynamicByteArray;                                         // Contains the TCP Protocol message bytes of not using GridConnect

    constructor Create;
    destructor Destroy; override;

    function Clone: TLccHardwareConnectionInfo; virtual;

    property Thread: TLccConnectionThread read FThread write FThreads;
    property LccMessage: TLccMessage read FLccMessage write FLccMessage;
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property MessageStr: String read FMessageStr write FMessageStr;             // Contains the string for the resulting message from the thread
  end;

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FGridConnect: Boolean;
    {$IFDEF ULTIBO}
    {$ELSE}
    FListenerSocketHandle: TSocket;
    {$ENDIF}
    FMsgStringList: TStringList;
    FOutgoingCircularArray: TThreadedCirularArray;
    FOutgoingGridConnect: TThreadStringList;
    FOwner: TLccHardwareConnectionManager;
    FSleepCount: Integer;
    FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
    FUseSynchronize: Boolean;
    FWorkerMessage: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;

    property Owner: TLccHardwareConnectionManager read FOwner write FOwner;
    property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;

    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
    procedure ReceiveMessage; virtual; abstract;

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager); reintroduce; virtual;
    destructor Destroy; override;

    // Must override and create an object of type "self"
    function CreateThreadObject: TLccConnectionThread; virtual; abstract;

    property GridConnect: Boolean read FGridConnect write FGridConnect;
    {$IFDEF ULTIBO}
    {$ELSE}
    // If the thread was created by a Listener (server) this is socket the listener created.  You will need this to assign a socket to the new thread
    property ListenerSocketHandle: TSocket read FListenerSocketHandle write FListenerSocketHandle;
    {$ENDIF}
    property MsgStringList: TStringList read FMsgStringList write FMsgStringList;
    property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
    property OutgoingCircularArray: TThreadedCirularArray read FOutgoingCircularArray write FOutgoingCircularArray;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    property SleepCount: Integer read FSleepCount write FSleepCount;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
  end;

  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent, IHardwareConnectionManagerLink)
  private
    FGridConnect: Boolean;
    FNodeManager: TLccNodeManager;
    FOnReceiveMessage: TOnConnectionReceiveEvent;
    FOnSendMessage: TOnMessageEvent;
    FWorkerMessage: TLccMessage;
    procedure SetGridConnect(AValue: Boolean); virtual;
  protected
    FIncomingCircularArray: TThreadedCirularArray;
    FIncomingGridConnect: TThreadStringList;
    // useful in decendants, GetConnected could just return this with the object setting FConnected correctly
    FConnected: Boolean;

    // Property getter must override and make definition based on connection type
    function GetConnected: Boolean; virtual; abstract;

    // Event call method
    procedure DoReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // Event call method
    procedure DoSendMessage(ALccMessage: TLccMessage); virtual;

    // Decendants override this to tell the Node Manager if this Connection is used to move Lcc packets or not (HTTP server, ComPort with custom protocol server are examples on "no")
    function IsLccLink: Boolean; virtual; abstract;

  public
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

    // When a thread owned by the manager receives a message it will call this centraized method
    procedure ReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    // Decendant must override this.  The Node Manager calls this when its nodes needs to send a message to the "wire".
    procedure SendMessage(ALccMessage: TLccMessage); virtual;
    // Used for the ComPort... need to understand the value of having this all the way back to here....
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual; abstract;

  published
    property Gridconnect: Boolean read FGridConnect write SetGridConnect;
    property OnReceiveMessage: TOnConnectionReceiveEvent read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  end;


implementation

{ TLccHardwareConnectionInfo }

constructor TLccHardwareConnectionInfo.Create;
begin
  FLccMessage := TLccMessage.Create;
end;

function TLccHardwareConnectionInfo.Clone: TLccHardwareConnectionInfo;
begin
  Result := Self.ClassType.Create as TLccHardwareConnectionInfo;
  Result.Thread := Self.Thread;
  Result.ErrorCode := Self.ErrorCode;
  Result.MessageStr := Self.MessageStr;
  Result.MessageArray := Self.MessageArray;
end;

destructor TLccHardwareConnectionInfo.Destroy;
begin
  FreeAndNil(FLccMessage);
  inherited Destroy;
end;


{ TLccHardwareConnectionManager }

procedure TLccHardwareConnectionManager.SetGridConnect(AValue: Boolean);
begin
  if AValue <> FGridConnect then
  begin
    FGridConnect := AValue;
  //  UpdateAllThreadProperites;
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

procedure TLccHardwareConnectionManager.ReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  // Now send the message to the NodeManager to fan out to all the nodes and other Hardware Connection Managers it owns
  if Gridconnect then
    NodeManager.HardwareConnectionRelayMessage(Self as IHardwareConnectionManagerLink, ConnectionInfo.LccMessage)
  else begin
    if WorkerMessage.LoadByLccTcp(ConnectionInfo.MessageArray) then // In goes a raw message
      NodeManager.HardwareConnectionRelayMessage(Self as IHardwareConnectionManagerLink, WorkerMessage);
  end;
  DoReceiveMessage(Thread, ConnectionInfo);
end;

procedure TLccHardwareConnectionManager.SendMessage(ALccMessage: TLccMessage);
begin
  DoSendMessage(ALccMessage);
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
end;

destructor TLccHardwareConnectionManager.Destroy;
begin
  if Assigned(NodeManager) then
    NodeManager.UnRegisterHardwareConnectionLink(Self as IHardwareConnectionManagerLink);
  FreeAndNil(FIncomingCircularArray);
  FreeAndNil(FIncomingGridConnect);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

{ TLccConnectionThread }

constructor TLccConnectionThread.Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
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


end.

