unit lcc_ethernet_common;

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
  lcc_common_classes,
  lcc_node_messages_can_assembler_disassembler;


type

  TLccBaseEthernetThread = class;
  TLccEthernetConnectionInfo = class;

  TOnEthernetEvent = procedure(Sender: TObject; ConnectionInfo: TLccHardwareConnectionInfo) of object;

  { TLccEthernetConnectionInfo }

  TLccEthernetConnectionInfo = class(TLccHardwareConnectionInfo)
  private
    FAutoResolve: Boolean;
    FClientIP: string;
    FClientPort: word;
    FConnectionState: TConnectionState;
    FHeartbeat: Integer;
    FListenerIP: string;
    FListenerPort: word;
    FWebSocket: Boolean;
  public
    property AutoResolveIP: Boolean read FAutoResolve write FAutoResolve;                     // Tries to autoresolve the local unique netword IP of the machine
    property ClientIP: string read FClientIP write FClientIP;
    property ClientPort: word read FClientPort write FClientPort;
    property ConnectionState: TConnectionState read FConnectionState write FConnectionState;  // Current State of the connection
    property HeartbeatRate: Integer read FHeartbeat write FHeartbeat;
    property ListenerIP: string read FListenerIP write FListenerIP;
    property ListenerPort: word read FListenerPort write FListenerPort;
    property WebSocket: Boolean read FWebSocket write FWebSocket;                            // Create A Websocket thread vs a basic TCP thread

    function Clone: TLccHardwareConnectionInfo; override;
  end;

  { TLccBaseEthernetThread }

  TLccBaseEthernetThread = class(TLccConnectionThread)
  private
    FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
    {$IFDEF ULTIBO}
     FStringList: TThreadStringList;
     FTcpClient: TWinsock2TCPClient;
    {$ELSE}
    FSocket: TTCPBlockSocket;
    {$ENDIF}
  protected
    FConnectionInfo: TLccEthernetConnectionInfo;
    {$IFDEF ULTIBO}
    property StringList: TThreadStringList read FStringList write FStringList;
    property TcpClient: TWinsock2TCPClient read FTcpClient write FTcpClient;
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
    property ConnectionInfo: TLccEthernetConnectionInfo read FConnectionInfo write FConnectionInfo;
    property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;

    procedure HandleErrorAndDisconnect;
    procedure HandleSendConnectionNotification(NewConnectionState: TConnectionState);
    procedure OnConnectionStateChange; virtual;
    procedure OnErrorMessageReceive; virtual;
    procedure ReceiveMessage; override;
    procedure SendMessage(AMessage: TLccMessage); override;

    procedure TryTransmitGridConnect(HandleErrors: Boolean);
    procedure TryTransmitTCPProtocol(HandleErrors: Boolean);
    procedure TryReceiveGridConnect(AGridConnectHelper: TGridConnectHelper; HandleErrors: Boolean);
    procedure TryReceiveTCPProtocol(HandleErrors: Boolean);

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccEthernetConnectionInfo); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TLccEthernetHardwareConnectionListener = class(TThread)
  public
    // Must override and create an object of the decentant type
    function CreateThreadObject: TLccEthernetHardwareConnectionListener; virtual; abstract;
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

  { TLccEthernetHardwareConnectionManager }

  TLccEthernetHardwareConnectionManager = class(TLccHardwareConnectionManager)
  private
    FEthernetThreads: TLccEthernetThreadList;
    FLccSettings: TLccSettings;
    FOnErrorMessage: TOnEthernetEvent;
    FOnConnectionStateChange: TOnEthernetEvent;
    FSleepCount: Integer;
    FUseSynchronize: Boolean;    // If set the threads will call back on a Syncronize call else incoming messages are put in the IncomingGridConnect or IncomingCircularArray buffers and the app needs to poll this buffer
    procedure SetSleepCount(AValue: Integer);
  protected

    procedure DoConnectionState(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    procedure DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    procedure DoReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); override;

    procedure UpdateAllThreadProperites; virtual;
    procedure UpdateThreadProperties(AThread: TLccConnectionThread); virtual;
  public
    property EthernetThreads: TLccEthernetThreadList read FEthernetThreads write FEthernetThreads;

    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); override;
    destructor Destroy; override;

    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TThread; virtual;
    procedure CloseConnection(EthernetThread: TLccConnectionThread); virtual;

    function OpenConnectionWithLccSettings: TThread; virtual;
    procedure SendMessage(AMessage: TLccMessage);  override;
    procedure SendMessageRawGridConnect(GridConnectStr: String); override;
  published
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property OnConnectionStateChange: TOnEthernetEvent read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetEvent read FOnErrorMessage write FOnErrorMessage;
    property SleepCount: Integer read FSleepCount write SetSleepCount;
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
  end;

implementation

{ TLccEthernetConnectionInfo }

function TLccEthernetConnectionInfo.Clone: TLccHardwareConnectionInfo;
begin
  Result := inherited Clone;
  (Result as TLccEthernetConnectionInfo).AutoResolveIP := (Self as TLccEthernetConnectionInfo).AutoResolveIP;
  (Result as TLccEthernetConnectionInfo).ClientIP  := (Self as TLccEthernetConnectionInfo).ClientIP;
  (Result as TLccEthernetConnectionInfo).ClientPort := (Self as TLccEthernetConnectionInfo).ClientPort;
  (Result as TLccEthernetConnectionInfo).ConnectionState := (Self as TLccEthernetConnectionInfo).ConnectionState;
  (Result as TLccEthernetConnectionInfo).ListenerIP := (Self as TLccEthernetConnectionInfo).ListenerIP;
  (Result as TLccEthernetConnectionInfo).ListenerPort := (Self as TLccEthernetConnectionInfo).ListenerPort;
  (Result as TLccEthernetConnectionInfo).HeartbeatRate := (Self as TLccEthernetConnectionInfo).HeartbeatRate;
  (Result as TLccEthernetConnectionInfo).WebSocket := (Self as TLccEthernetConnectionInfo).WebSocket;
end;

{ TLccEthernetHardwareConnectionManager }

procedure TLccEthernetHardwareConnectionManager.SetSleepCount(AValue: Integer);
begin
  if AValue <> FSleepCount then
  begin
    FSleepCount := AValue;
    UpdateAllThreadProperites;
  end;
end;

procedure TLccEthernetHardwareConnectionManager.DoConnectionState(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Thread, ConnectionInfo)
end;

procedure TLccEthernetHardwareConnectionManager.DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  if Assigned(OnErrorMessage) then
    OnErrorMessage(Thread, ConnectionInfo)
end;

procedure TLccEthernetHardwareConnectionManager.DoReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited;

    // Received a message, see if it is an alias we need to save (eventually for now save them all)
  case ConnectionInfo.LccMessage.CAN.MTI of
    MTI_CAN_AMR : NodeManager.AliasServer.RemoveMapping(ConnectionInfo.LccMessage.CAN.SourceAlias);
    MTI_CAN_AMD : NodeManager.AliasServer.ForceMapping(ConnectionInfo.LccMessage.SourceID, ConnectionInfo.LccMessage.CAN.SourceAlias)
  end;
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
  end
end;

procedure TLccEthernetHardwareConnectionManager.UpdateThreadProperties(AThread: TLccConnectionThread);
begin
  inherited;
  (AThread as TLccBaseEthernetThread).SleepCount := SleepCount;
end;

constructor TLccEthernetHardwareConnectionManager.Create(AOwner: TComponent; ANodeManager: TLccNodeManager);
begin
  inherited Create(AOwner, ANodeManager);
  FEthernetThreads := TLccEthernetThreadList.Create;
  FUseSynchronize := True;
end;

destructor TLccEthernetHardwareConnectionManager.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  inherited Destroy;
end;

function TLccEthernetHardwareConnectionManager.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TThread;
begin
  Result := nil;
end;

procedure TLccEthernetHardwareConnectionManager.CloseConnection(EthernetThread: TLccConnectionThread);
begin
  if Assigned(EthernetThread) then
  begin
    EthernetThreads.CloseEthernetPort(EthernetThread as TLccBaseEthernetThread);
    EthernetThreads.Remove(EthernetThread as TLccBaseEthernetThread);
    {$IFDEF ULTIBO}
    {$ELSE}
    FreeAndNil( EthernetThread);
    {$ENDIF}
  end else
    EthernetThreads.CloseEthernetPorts;
end;

function TLccEthernetHardwareConnectionManager.OpenConnectionWithLccSettings: TThread;
var
  ConnectionInfo: TLccEthernetConnectionInfo;
begin
  Result := nil;
  if Assigned(LccSettings) then
  begin
    ConnectionInfo := TLccEthernetConnectionInfo.Create;
    try
      ConnectionInfo.ConnectionState := ccsListenerDisconnected;
      ConnectionInfo.Thread := nil;
      ConnectionInfo.MessageStr := '';
      ConnectionInfo.ListenerPort := LccSettings.Ethernet.RemoteListenerPort;
      ConnectionInfo.ListenerIP := LccSettings.Ethernet.RemoteListenerIP;
      ConnectionInfo.ClientIP := LccSettings.Ethernet.LocalClientIP;
      ConnectionInfo.ClientPort := LccSettings.Ethernet.LocalClientPort;
      ConnectionInfo.HeartbeatRate := 0;
      ConnectionInfo.ErrorCode := 0;
      ConnectionInfo.MessageArray := nil;
      ConnectionInfo.AutoResolveIP := LccSettings.Ethernet.AutoResolveClientIP;
      Result := OpenConnection(ConnectionInfo);
    finally
      ConnectionInfo.Free;
    end;
  end;
end;

procedure TLccEthernetHardwareConnectionManager.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
  L: TList;
  EthernetThread: TLccBaseEthernetThread;
begin
  inherited;

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

procedure TLccEthernetThreadList.CloseEthernetPort(EthernetThread: TLccBaseEthernetThread);
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

procedure TLccBaseEthernetThread.HandleErrorAndDisconnect;
begin
  (Owner as TLccEthernetHardwareConnectionManager).EthernetThreads.Remove(Self);
  ConnectionInfo.ErrorCode := Socket.LastError;
  ConnectionInfo.MessageStr := Socket.LastErrorDesc;
  if (ConnectionInfo.ErrorCode <> 0) then
    Synchronize({$IFDEF FPC}@{$ENDIF}OnErrorMessageReceive);
  HandleSendConnectionNotification(ccsListenerClientDisconnected);
  Terminate;
end;

procedure TLccBaseEthernetThread.HandleSendConnectionNotification(NewConnectionState: TConnectionState);
begin
  // Taken from the ethernet_client file... not sure if it is useful....
  if Assigned(Socket) then
    begin
      Socket.GetSinLocal;
      ConnectionInfo.ClientIP := Socket.GetLocalSinIP;
      ConnectionInfo.ClientPort := Socket.GetLocalSinPort;
    end;

  ConnectionInfo.ConnectionState := NewConnectionState;
    Synchronize({$IFDEF FPC}@{$ENDIF}OnConnectionStateChange);
end;

procedure TLccBaseEthernetThread.ReceiveMessage;
begin
  // Called in context of main thread through Syncronize
  Owner.ReceiveMessage(Self, ConnectionInfo);
end;

procedure TLccBaseEthernetThread.OnConnectionStateChange;
begin
  inherited;
  (Owner as TLccEthernetHardwareConnectionManager).DoConnectionState(Self, ConnectionInfo);
end;

procedure TLccBaseEthernetThread.OnErrorMessageReceive;
begin
  inherited;
  (Owner as TLccEthernetHardwareConnectionManager).DoErrorMessage(Self, ConnectionInfo);
end;

procedure TLccBaseEthernetThread.SendMessage(AMessage: TLccMessage);
var
  ByteArray: TLccDynamicByteArray;
  i: Integer;
begin
  if Gridconnect then
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
  DynamicByteArray: TLccDynamicByteArray;
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
          ConnectionInfo.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
          ConnectionInfo.LccMessage.LoadByGridConnectStr(ConnectionInfo.MessageStr);

          case GridConnectMessageAssembler.IncomingMessageGridConnect(ConnectionInfo.LccMessage) of
            imgcr_True :
              begin
                if UseSynchronize then
                  Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage)
                else begin
                  // DANGER: This method do not allow for the AliasServer update to be
                  // called automatically.  The program is responsible for pumping the messages
                  // out of the Owner.IncomingGridConnect
                  RxList := Owner.IncomingGridConnect.LockList;
                  try
                    RxList.Add(ConnectionInfo.LccMessage.ConvertToGridConnectStr('', False));
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
        if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, ConnectionInfo.MessageArray) then
        begin
          if UseSynchronize then
            Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage)
          else begin
             // DANGER: This method do not allow for the AliasServer update to be
             // called automatically.  The program is responsible for pumping the messages
             // out of the Owner.IncomingGridConnect
            Owner.IncomingCircularArray.LockArray;
            try
              Owner.IncomingCircularArray.AddChunk(ConnectionInfo.MessageArray);
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
  AnOwner: TLccHardwareConnectionManager;
  AConnectionInfo: TLccEthernetConnectionInfo);
begin
  inherited Create(CreateSuspended, AnOwner);
  FConnectionInfo := AConnectionInfo.Clone as TLccEthernetConnectionInfo;
  ConnectionInfo.Thread := Self;
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
  FreeAndNil(FConnectionInfo);
  FreeAndNil(FGridConnectMessageAssembler);
  inherited Destroy;
end;

end.

