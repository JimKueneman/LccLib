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
    FHeartbeat: Integer;
    FListenerIP: string;
    FListenerPort: word;
  public
    property AutoResolveIP: Boolean read FAutoResolve write FAutoResolve;                     // Tries to autoresolve the local unique netword IP of the machine
    property ClientIP: string read FClientIP write FClientIP;
    property ClientPort: word read FClientPort write FClientPort;
    property HeartbeatRate: Integer read FHeartbeat write FHeartbeat;
    property ListenerIP: string read FListenerIP write FListenerIP;
    property ListenerPort: word read FListenerPort write FListenerPort;

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
    {$IFDEF ULTIBO}
    property StringList: TThreadStringList read FStringList write FStringList;
    property TcpClient: TWinsock2TCPClient read FTcpClient write FTcpClient;
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
    property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;

    procedure HandleErrorAndDisconnect; override;
    procedure HandleSendConnectionNotification(NewConnectionState: TLccConnectionState); override;
    procedure OnConnectionStateChange; virtual;
    procedure OnErrorMessageReceive; virtual;
    procedure ReceiveMessage; override;
    procedure SendMessage(AMessage: TLccMessage); override;
    procedure ForceTerminate; override;

    procedure TryTransmitGridConnect(HandleErrors: Boolean);
    procedure TryTransmitTCPProtocol(HandleErrors: Boolean);
    procedure TryReceiveGridConnect(AGridConnectHelper: TGridConnectHelper; HandleErrors: Boolean);
    procedure TryReceiveTCPProtocol(HandleErrors: Boolean);

  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
    destructor Destroy; override;
  end;

  TLccEthernetHardwareConnectionListener = class(TThread)
  public
    // Must override and create an object of the decentant type
    function CreateThreadObject: TLccEthernetHardwareConnectionListener; virtual; abstract;
  end;

  { TLccEthernetHardwareConnectionManager }

  TLccEthernetHardwareConnectionManager = class(TLccHardwareConnectionManager)
  private
    FLccSettings: TLccSettings;
  protected
    procedure DoConnectionState(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); virtual;
    procedure DoErrorMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); override;
    procedure DoReceiveMessage(Thread: TLccConnectionThread; ConnectionInfo: TLccHardwareConnectionInfo); override;
  public
    function OpenConnectionWithLccSettings: TLccConnectionThread; override;
  published
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
  end;

implementation

{ TLccEthernetConnectionInfo }

function TLccEthernetConnectionInfo.Clone: TLccHardwareConnectionInfo;
begin
  Result := inherited Clone;
  (Result as TLccEthernetConnectionInfo).AutoResolveIP := (Self as TLccEthernetConnectionInfo).AutoResolveIP;
  (Result as TLccEthernetConnectionInfo).ClientIP  := (Self as TLccEthernetConnectionInfo).ClientIP;
  (Result as TLccEthernetConnectionInfo).ClientPort := (Self as TLccEthernetConnectionInfo).ClientPort;
  (Result as TLccEthernetConnectionInfo).ListenerIP := (Self as TLccEthernetConnectionInfo).ListenerIP;
  (Result as TLccEthernetConnectionInfo).ListenerPort := (Self as TLccEthernetConnectionInfo).ListenerPort;
  (Result as TLccEthernetConnectionInfo).HeartbeatRate := (Self as TLccEthernetConnectionInfo).HeartbeatRate;
end;

{ TLccEthernetHardwareConnectionManager }

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

function TLccEthernetHardwareConnectionManager.OpenConnectionWithLccSettings: TLccConnectionThread;
var
  ConnectionInfo: TLccEthernetConnectionInfo;
begin
  Result := nil;
  if Assigned(LccSettings) then
  begin
    ConnectionInfo := TLccEthernetConnectionInfo.Create;
    try
      ConnectionInfo.ListenerPort := LccSettings.Ethernet.RemoteListenerPort;
      ConnectionInfo.ListenerIP := LccSettings.Ethernet.RemoteListenerIP;
      ConnectionInfo.ClientIP := LccSettings.Ethernet.LocalClientIP;
      ConnectionInfo.ClientPort := LccSettings.Ethernet.LocalClientPort;
      ConnectionInfo.AutoResolveIP := LccSettings.Ethernet.AutoResolveClientIP;
      Result := OpenConnection(ConnectionInfo);
    finally
      ConnectionInfo.Free;
    end;
  end;
end;

{ TLccBaseEthernetThread }

procedure TLccBaseEthernetThread.HandleErrorAndDisconnect;
begin
  ConnectionInfo.ErrorCode := Socket.LastError;
  ConnectionInfo.MessageStr := Socket.LastErrorDesc;
  inherited HandleErrorAndDisconnect;
end;

procedure TLccBaseEthernetThread.HandleSendConnectionNotification(NewConnectionState: TLccConnectionState);
begin
  // Taken from the ethernet_client file... not sure if it is useful....
  if Assigned(Socket) then
  begin
    Socket.GetSinLocal;
    (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := Socket.GetLocalSinIP;
    (ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := Socket.GetLocalSinPort;
  end;
  inherited HandleSendConnectionNotification(NewConnectionState);
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

procedure TLccBaseEthernetThread.ForceTerminate;
begin
  inherited;

  if Assigned(Socket) then
   Socket.CloseSocket;
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
  i: Integer;
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
                if ConnectionInfo.UseSyncronize then
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
          if ConnectionInfo.UseSyncronize then
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

constructor TLccBaseEthernetThread.Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended, AnOwner, AConnectionInfo);
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
  FreeAndNil(FGridConnectMessageAssembler);
  inherited Destroy;
end;

end.

