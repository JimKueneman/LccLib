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
  TLccEthernetHardwareConnectionManager = class;
  TLccBaseEthernetThread = class;

  { TLccEthernetRec }

  TLccEthernetRec = record
    Thread: TLccBaseEthernetThread;    // Thread owing the Record
    AutoResolveIP: Boolean;          // Tries to autoresolve the local unique netword IP of the machine
    ClientIP,
    ListenerIP: String;
    ClientPort,
    ListenerPort: Word;
    HeartbeatRate: Integer;
    ConnectionState: TConnectionState; // Current State of the connection
    MessageStr: String;                // Contains the string for the resuting message from the thread
    MessageArray: lcc_defines.TDynamicByteArray;   // Contains the TCP Protocol message bytes of not using GridConnect
    ErrorCode: Integer;
    LccMessage: TLccMessage;
    SuppressNotification: Boolean;    // True to stop any Syncronoize() call being called
    WebSocket: Boolean;               // Create A Websocket thread vs a basic TCP thread
  end;

  TOnEthernetRecFunc = procedure(Sender: TObject; EthernetRec: TLccEthernetRec) of object;
  TOnEthernetReceiveFunc = procedure(Sender: TObject; EthernetRec: TLccEthernetRec) of object;


  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FGridConnect: Boolean;
    FMsgStringList: TStringList;
    FOutgoingCircularArray: TThreadedCirularArray;
    FOutgoingGridConnect: TThreadStringList;
    FSleepCount: Integer;
    FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
    FUseSynchronize: Boolean;
    FWorkerMsg: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;

    property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;

    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
  public
    {$IFDEF FPC}
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    {$ELSE}
    constructor Create(CreateSuspended: Boolean); reintroduce;
    {$ENDIF}
    destructor Destroy; override;

    property GridConnect: Boolean read FGridConnect write FGridConnect;    // Ethernet Only
    property MsgStringList: TStringList read FMsgStringList write FMsgStringList;
    property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
    property OutgoingCircularArray: TThreadedCirularArray read FOutgoingCircularArray write FOutgoingCircularArray;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    property SleepCount: Integer read FSleepCount write FSleepCount;
    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
  end;

  { TLccBaseEthernetThread }

  TLccBaseEthernetThread = class(TLccConnectionThread)
  private
    FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
    FOnSendMessage: TOnMessageEvent;
    FOwner: TLccEthernetHardwareConnectionManager;
    {$IFDEF ULTIBO}
     FStringList: TThreadStringList;
     FTcpClient: TWinsock2TCPClient;
    {$ELSE}
    FSocket: TTCPBlockSocket;
    {$ENDIF}
  protected
    FEthernetRec: TLccEthernetRec;
    {$IFDEF ULTIBO}
    property StringList: TThreadStringList read FStringList write FStringList;
    property TcpClient: TWinsock2TCPClient read FTcpClient write FTcpClient;
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
    property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
    property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;
    property Owner: TLccEthernetHardwareConnectionManager read FOwner write FOwner;

    procedure DoConnectionState; virtual;
    procedure DoErrorMessage; virtual;
    procedure DoReceiveMessage; virtual;
    procedure DoSendMessage(AMessage: TLccMessage);

    procedure HandleErrorAndDisconnect;
    procedure HandleSendConnectionNotification(NewConnectionState: TConnectionState);
    procedure SendMessage(AMessage: TLccMessage); override;

    procedure TryTransmitGridConnect(HandleErrors: Boolean);
    procedure TryTransmitTCPProtocol(HandleErrors: Boolean);
    procedure TryReceiveGridConnect(AGridConnectHelper: TGridConnectHelper; HandleErrors: Boolean);
    procedure TryReceiveTCPProtocol(HandleErrors: Boolean);

  public
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;

    constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetHardwareConnectionManager; const AnEthernetRec: TLccEthernetRec); reintroduce; virtual;
    destructor Destroy; override;
  end;


  { TLccEthernetThreadList }

  TLccEthernetThreadList = class(TThreadList)      // Contains TLccBaseEthernetThread and decendent objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure CloseEthernetPorts;
    procedure CloseEthernetPort(EthernetThread: TLccBaseEthernetThread);

    property Count: Integer read GetCount;
  end;


  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent)
  private
    FAliasServer: TLccAliasServer;
  protected
    FIncomingCircularArray: TThreadedCirularArray;
    FIncomingGridConnect: TThreadStringList;
  public
    property IncomingGridConnect: TThreadStringList read FIncomingGridConnect;
    property IncomingCircularArray: TThreadedCirularArray read FIncomingCircularArray;
    property AliasServer: TLccAliasServer read FAliasServer write FAliasServer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual; abstract;
  end;

  { TLccEthernetHardwareConnectionManager }

  TLccEthernetHardwareConnectionManager = class(TLccHardwareConnectionManager)
  private
    FEthernetThreads: TLccEthernetThreadList;
    FGridConnect: Boolean;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FSleepCount: Integer;
    FUseSynchronize: Boolean;    // If set the threads will call back on a Syncronize call else incoming messages are put in the IncomingGridConnect or IncomingCircularArray buffers and the app needs to poll this buffer
    procedure SetGridConnect(AValue: Boolean);
    procedure SetSleepCount(AValue: Integer);
  protected
    function GetConnected: Boolean; virtual; abstract;

    procedure DoConnectionState(Thread: TLccBaseEthernetThread; EthernetRec: TLccEthernetRec); virtual;
    procedure DoErrorMessage(Thread: TLccBaseEthernetThread; EthernetRec: TLccEthernetRec); virtual;
    procedure DoReceiveMessage(Thread: TLccBaseEthernetThread; EthernetRec: TLccEthernetRec); virtual;
    procedure DoSendMessage(Thread: TLccBaseEthernetThread; AMessage: TLccMessage); virtual;

    procedure UpdateAllThreadProperites; virtual;
    procedure UpdateThreadProperties(EthernetThread: TLccBaseEthernetThread); virtual;
  public
    property Connected: Boolean read GetConnected;
    property EthernetThreads: TLccEthernetThreadList read FEthernetThreads write FEthernetThreads;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenConnection(AnEthernetRec: TLccEthernetRec): TThread; virtual;
    procedure CloseConnection(EthernetThread: TLccBaseEthernetThread); virtual;
    function OpenConnectionWithLccSettings: TThread; virtual;
    procedure SendMessage(AMessage: TLccMessage);  override;
    procedure SendMessageRawGridConnect(GridConnectStr: String); override;
  published
    property Gridconnect: Boolean read FGridConnect write SetGridConnect;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write SetSleepCount;
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
  end;

implementation

{ TLccEthernetHardwareConnectionManager }

procedure TLccEthernetHardwareConnectionManager.SetGridConnect(AValue: Boolean);
begin
  if AValue <> FGridConnect then
  begin
    FGridConnect:=AValue;
    UpdateAllThreadProperites;
  end;
end;

procedure TLccEthernetHardwareConnectionManager.SetSleepCount(AValue: Integer);
begin
  if AValue <> FSleepCount then
  begin
    FSleepCount := AValue;
    UpdateAllThreadProperites;
  end;
end;

procedure TLccEthernetHardwareConnectionManager.DoConnectionState(
  Thread: TLccBaseEthernetThread; EthernetRec: TLccEthernetRec);
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Thread, EthernetRec)
end;

procedure TLccEthernetHardwareConnectionManager.DoErrorMessage(
  Thread: TLccBaseEthernetThread; EthernetRec: TLccEthernetRec);
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Thread, EthernetRec)
end;

procedure TLccEthernetHardwareConnectionManager.DoReceiveMessage(
  Thread: TLccBaseEthernetThread; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.LccMessage.CAN.MTI of
    MTI_CAN_AMR : AliasServer.RemoveMapping(EthernetRec.LccMessage.CAN.SourceAlias);
    MTI_CAN_AMD : AliasServer.ForceMapping(EthernetRec.LccMessage.SourceID, EthernetRec.LccMessage.CAN.SourceAlias)
  end;
  if Assigned(OnReceiveMessage) then
    OnReceiveMessage(Thread, EthernetRec);
end;

procedure TLccEthernetHardwareConnectionManager.DoSendMessage(Thread: TLccBaseEthernetThread; AMessage: TLccMessage);
begin

end;

procedure TLccEthernetHardwareConnectionManager.UpdateAllThreadProperites;
var
  i: Integer;
  L: TList;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
      UpdateThreadProperties(TLccBaseEthernetThread( L[i]));
  finally
    EthernetThreads.UnlockList;
  end;
end;

procedure TLccEthernetHardwareConnectionManager.UpdateThreadProperties(
  EthernetThread: TLccBaseEthernetThread);
begin
  EthernetThread.OnSendMessage := OnSendMessage;
  EthernetThread.SleepCount := SleepCount;
  EthernetThread.GridConnect := FGridConnect;
end;

constructor TLccEthernetHardwareConnectionManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEthernetThreads := TLccEthernetThreadList.Create;
  FUseSynchronize := True;
end;

destructor TLccEthernetHardwareConnectionManager.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  inherited Destroy;
end;

function TLccEthernetHardwareConnectionManager.OpenConnection(AnEthernetRec: TLccEthernetRec): TThread;
begin
  Result := nil;
  AliasServer.Clear;
end;

procedure TLccEthernetHardwareConnectionManager.CloseConnection(EthernetThread: TLccBaseEthernetThread);
begin
  if Assigned(EthernetThread) then
  begin
    EthernetThreads.CloseEthernetPort(EthernetThread);
    EthernetThreads.Remove(EthernetThread);
    {$IFDEF ULTIBO}
    {$ELSE}
    FreeAndNil( EthernetThread);
    {$ENDIF}
  end else
    EthernetThreads.CloseEthernetPorts;
end;

function TLccEthernetHardwareConnectionManager.OpenConnectionWithLccSettings: TThread;
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
    AnEthernetRec.ListenerPort := LccSettings.Ethernet.RemoteListenerPort;
    AnEthernetRec.ListenerIP := LccSettings.Ethernet.RemoteListenerIP;
    AnEthernetRec.ClientIP := LccSettings.Ethernet.LocalClientIP;
    AnEthernetRec.ClientPort := LccSettings.Ethernet.LocalClientPort;
    AnEthernetRec.HeartbeatRate := 0;
    AnEthernetRec.ErrorCode := 0;
    AnEthernetRec.MessageArray := nil;
    AnEthernetRec.AutoResolveIP := LccSettings.Ethernet.AutoResolveClientIP;
    Result := OpenConnection(AnEthernetRec);
  end;
end;

procedure TLccEthernetHardwareConnectionManager.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
  L: TList;
  EthernetThread: TLccBaseEthernetThread;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccBaseEthernetThread( L[i]);
      EthernetThread.SendMessage(AMessage);
    end;
  finally
    EthernetThreads.UnlockList;
  end;
end;

procedure TLccEthernetHardwareConnectionManager.SendMessageRawGridConnect(GridConnectStr: String);
var
  i: Integer;
  List: TList;
  EthernetThread: TLccBaseEthernetThread;
  StringList: TStringList;
  TempText: string;
begin
  List := EthernetThreads.LockList;
  try  // TODO
    for i := 0 to List.Count - 1 do
    begin
      EthernetThread := TLccBaseEthernetThread( List[i]);
      StringList := EthernetThread.OutgoingGridConnect.LockList;
      try
        TempText := StringList.DelimitedText;
        TempText := TempText + #10 + GridConnectStr;
        StringList.DelimitedText := TempText;
      finally
        EthernetThread.OutgoingGridConnect.UnLockList
      end;
    end;
  finally
    EthernetThreads.UnlockList;
  end;
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
  EthernetThread: TLccBaseEthernetThread;
begin
  while Count > 0 do
  begin
    L := LockList;
    try
      EthernetThread := TLccBaseEthernetThread( L[0]);
    finally
      UnlockList;
    end;

    CloseEthernetPort(EthernetThread);

    // Thread removed itself from the List no need to do it here

    {$IFDEF ULTIBO}
    {$ELSE}
    FreeAndNil( EthernetThread);
    {$ENDIF}
  end;
end;

procedure TLccEthernetThreadList.CloseEthernetPort(
  EthernetThread: TLccBaseEthernetThread);
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
end;

{ TLccBaseEthernetThread }

procedure TLccBaseEthernetThread.DoConnectionState;
begin
  Owner.DoConnectionState(Self, FEthernetRec);
end;

procedure TLccBaseEthernetThread.DoErrorMessage;
begin
  Owner.DoErrorMessage(Self, FEthernetRec);
end;

procedure TLccBaseEthernetThread.DoReceiveMessage;
begin
  case FEthernetRec.LccMessage.CAN.MTI of
    MTI_CAN_AMR : (Owner as TLccHardwareConnectionManager).AliasServer.RemoveMapping(FEthernetRec.LccMessage.CAN.SourceAlias);
    MTI_CAN_AMD : (Owner as TLccHardwareConnectionManager).AliasServer.ForceMapping(FEthernetRec.LccMessage.SourceID, FEthernetRec.LccMessage.CAN.SourceAlias)
  end;
  Owner.DoReceiveMessage(Self, FEthernetRec);
end;

procedure TLccBaseEthernetThread.DoSendMessage(AMessage: TLccMessage);
begin
  case AMessage.CAN.MTI of
    MTI_CAN_AMR : (Owner as TLccHardwareConnectionManager).AliasServer.RemoveMapping(AMessage.CAN.SourceAlias);
    MTI_CAN_AMD : (Owner as TLccHardwareConnectionManager).AliasServer.ForceMapping(AMessage.SourceID, AMessage.CAN.SourceAlias)
  end;
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, AMessage);
end;

procedure TLccBaseEthernetThread.HandleErrorAndDisconnect;
begin
  Owner.EthernetThreads.Remove(Self);
  FEthernetRec.ErrorCode := Socket.LastError;
  FEthernetRec.MessageStr := Socket.LastErrorDesc;
  if not FEthernetRec.SuppressNotification and (FEthernetRec.ErrorCode <> 0) then
    Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
  HandleSendConnectionNotification(ccsListenerClientDisconnected);
  Terminate;
end;

procedure TLccBaseEthernetThread.HandleSendConnectionNotification(NewConnectionState: TConnectionState);
begin
  // Taken from the ethernet_client file... not sure if it is useful....
  if Assigned(Socket) then
    begin
      Socket.GetSinLocal;
      FEthernetRec.ClientIP := Socket.GetLocalSinIP;
      FEthernetRec.ClientPort := Socket.GetLocalSinPort;
    end;

  FEthernetRec.ConnectionState := NewConnectionState;
  if not FEthernetRec.SuppressNotification then
    Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
end;

procedure TLccBaseEthernetThread.SendMessage(AMessage: TLccMessage);
var
  ByteArray: TDynamicByteArray;
  i: Integer;
begin
  if Gridconnect then
  begin
 //   UpdateAliasServer(AMessage);
    MsgStringList.Text := AMessage.ConvertToGridConnectStr(#10, False);
    for i := 0 to MsgStringList.Count - 1 do
      OutgoingGridConnect.Add(MsgStringList[i]);
  end else
  begin
    ByteArray := nil;
    if AMessage.ConvertToLccTcp(ByteArray) then
      OutgoingCircularArray.AddChunk(ByteArray);
  end;
  DoSendMessage(AMessage);
end;

procedure TLccBaseEthernetThread.TryTransmitGridConnect(HandleErrors: Boolean);
var
  TxStr: string;
  TxList: TStringList;
  i: Integer;
begin
  TxStr := '';
  TxList := OutgoingGridConnect.LockList;
  try
    for i := 0 to TxList.Count - 1 do
      TxStr := TxStr + TxList[i] + #10;
    TxList.Clear;
  finally
    OutgoingGridConnect.UnlockList;
  end;

  if TxStr <> '' then
  begin
    Socket.SendString(String( TxStr) + #10);
    if (Socket.LastError <> 0) and HandleErrors then
      HandleErrorAndDisconnect;
  end;
end;

procedure TLccBaseEthernetThread.TryTransmitTCPProtocol(HandleErrors: Boolean);
var
  DynamicByteArray: TDynamicByteArray;
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
    if (Socket.LastError <> 0) and HandleErrors then
      HandleErrorAndDisconnect;
  end;
end;

procedure TLccBaseEthernetThread.TryReceiveGridConnect(
  AGridConnectHelper: TGridConnectHelper; HandleErrors: Boolean);
var
  RcvByte: Byte;
  GridConnectStrPtr: PGridConnectString;
  RxList: TStringList;
begin
  RcvByte := Socket.RecvByte(1);
  case Socket.LastError of
    0 :
      begin
        GridConnectStrPtr := nil;
        if AGridConnectHelper.GridConnect_DecodeMachine(RcvByte, GridConnectStrPtr) then
        begin
          FEthernetRec.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
          FEthernetRec.LccMessage.LoadByGridConnectStr(FEthernetRec.MessageStr);

          case GridConnectMessageAssembler.IncomingMessageGridConnect(FEthernetRec.LccMessage) of
            imgcr_True :
              begin
                if UseSynchronize then
                  Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage)
                else begin
                  RxList := Owner.IncomingGridConnect.LockList;
                  try
                    RxList.Add(FEthernetRec.LccMessage.ConvertToGridConnectStr('', False));
                  finally
                    Owner.IncomingGridConnect.UnlockList;
                  end;
                end;
              end;
            imgcr_False,
            imgcr_ErrorToSend,
            imgcr_UnknownError : begin end;
          end;
        end;
      end;
    WSAETIMEDOUT :
      begin

      end;
    WSAECONNRESET   :
      begin
        if HandleErrors then
          HandleErrorAndDisconnect;
      end
  else
    if HandleErrors then
      HandleErrorAndDisconnect
  end;
end;

procedure TLccBaseEthernetThread.TryReceiveTCPProtocol(HandleErrors: Boolean);
var
  RcvByte: Byte;
begin
  RcvByte := Socket.RecvByte(1);
  case Socket.LastError of
    0 :
      begin
        if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, FEthernetRec.MessageArray) then
        begin
          if UseSynchronize then
            Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage)
          else begin
            Owner.IncomingCircularArray.LockArray;
            try
              Owner.IncomingCircularArray.AddChunk(FEthernetRec.MessageArray);
            finally
              Owner.IncomingCircularArray.UnLockArray;
            end;
          end
        end;
      end;
    WSAETIMEDOUT :
      begin

      end;
    WSAECONNRESET   :
      begin
        if HandleErrors then
          HandleErrorAndDisconnect;
      end
  else
    if HandleErrors then
      HandleErrorAndDisconnect
  end;
end;

constructor TLccBaseEthernetThread.Create(CreateSuspended: Boolean;
  AnOwner: TLccEthernetHardwareConnectionManager;
  const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := Self;
  FEthernetRec.LccMessage := TLccMessage.Create;
  GridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
  {$IFDEF ULTIBO}
  StringList := TThreadStringList.Create;
  TcpClient := TWinsock2TCPClient.Create;
  {$ENDIF}
end;

destructor TLccBaseEthernetThread.Destroy;
begin
 {$IFDEF ULTIBO}
  FreeAndNil(FStringList);
  FreeAndNil(FTcpClient);
  {$ENDIF}
  {$IFDEF DWSCRIPT}
  FEthernetRec.LccMessage.Free;
  FGridConnectMessageAssembler.Free;
  {$ELSE}
  FreeAndNil(FEthernetRec.LccMessage);
  FreeAndNil(FGridConnectMessageAssembler);
  {$ENDIF}
  inherited Destroy;
end;

{ TLccHardwareConnectionManager }

constructor TLccHardwareConnectionManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIncomingGridConnect := TThreadStringList.Create;
  FIncomingCircularArray := TThreadedCirularArray.Create;
  FAliasServer := TLccAliasServer.Create;
end;

destructor TLccHardwareConnectionManager.Destroy;
begin
  {$IFDEF DWSCRIPT}
  FIncomingCircularArray.Free;
  FIncomingGridConnect.Free;
  FAliasServer.Free;
  {$ELSE}
  FreeAndNil(FIncomingCircularArray);
  FreeAndNil(FIncomingGridConnect);
  FreeAndNil(FAliasServer);
  {$ENDIF}
  inherited Destroy;
end;

{ TLccConnectionThread }

{$IFDEF FPC}
constructor TLccConnectionThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
{$ELSE}
constructor TLccConnectionThread.Create(CreateSuspended: Boolean);
{$ENDIF}
begin
  inherited Create(CreateSuspended {$IFDEF FPC}, StackSize{$ENDIF});
  FWorkerMsg := TLccMessage.Create;
  FMsgStringList := TStringList.Create;
  FOutgoingCircularArray := TThreadedCirularArray.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
  OutgoingGridConnect.Delimiter := #10;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
end;

destructor TLccConnectionThread.Destroy;
begin
  {$IFDEF DWSCRIPT}
  FWorkerMsg.Free;
  FMsgStringList.Free);
  FOutgoingCircularArray.Free);
  FOutgoingGridConnect.Free);
  FTcpDecodeStateMachine.Free);
  {$ELSE}
  FreeAndNil(FWorkerMsg);
  FreeAndNil(FMsgStringList);
  FreeAndNil(FOutgoingCircularArray);
  FreeAndNil(FOutgoingGridConnect);
  FreeAndNil(FTcpDecodeStateMachine);
  {$ENDIF}
  inherited Destroy;
end;

function TLccConnectionThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;


end.

