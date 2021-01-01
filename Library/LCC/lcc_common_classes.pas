unit lcc_common_classes;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
    contnrs,
  {$ELSE}
    System.Generics.Collections,
  {$ENDIF}
  lcc_messages,
  lcc_can_message_assembler_disassembler,
  lcc_threadedcirculararray,
  lcc_threaded_stringlist,
  lcc_alias_server;

type
  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FGridConnect: Boolean;
    FMsgAssembler: TLccMessageAssembler;
    FMsgDisAssembler: TLccMessageDisAssembler;
    FMsgStringList: TStringList;
    FOutgoingCircularArray: TThreadedCirularArray;
    FOutgoingGridConnect: TThreadStringList;
    FSleepCount: Integer;
    FWorkerMsg: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;
  public
    {$IFDEF FPC}
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    {$ELSE}
    constructor Create(CreateSuspended: Boolean); reintroduce;
    {$ENDIF}
    destructor Destroy; override;
    property GridConnect: Boolean read FGridConnect write FGridConnect;    // Ethernet Only
    property MsgAssembler: TLccMessageAssembler read FMsgAssembler write FMsgAssembler;
    property MsgDisAssembler: TLccMessageDisAssembler read FMsgDisAssembler write FMsgDisAssembler;
    property MsgStringList: TStringList read FMsgStringList write FMsgStringList;
    property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
    property OutgoingCircularArray: TThreadedCirularArray read FOutgoingCircularArray write FOutgoingCircularArray;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    property SleepCount: Integer read FSleepCount write FSleepCount;
    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
  end;

  { TLccBaseEthernetThread }

  TLccBaseEthernetThread = class(TLccConnectionThread)
  protected
    procedure TryTransmitGridConnect(HandleErrors: Boolean);
    procedure TryTransmitTCPProtocol(HandleErrors: Boolean);
    procedure TryReceiveGridConnect(AGridConnectHelper: TGridConnectHelper; HandleErrors: Boolean);
    procedure TryReceiveTCPProtocol(HandleErrors: Boolean);
  end;


  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent)
  public
    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual; abstract;
  end;

  { TLccTimerThread }

  TLccTimerThread = class(TThread)
  private
    FInterval: Word;
    FOnTimeTick: TNotifyEvent;
  protected
    procedure Execute; override;
    procedure TimeTick;
  public
    property Interval: Word read FInterval write FInterval;
    property OnTimeTick: TNotifyEvent read FOnTimeTick write FOnTimeTick;
  end;

implementation

{ TLccBaseEthernetThread }

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
      TxStr := TxList[i] + LF;
    TxList.Clear;
  finally
    OutgoingGridConnect.UnlockList;
  end;

  if TxStr <> '' then
  begin
    Socket.SendString(String( TxStr) + LF);
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

{ TLccTimerThread }

procedure TLccTimerThread.Execute;
begin
  while not Terminated do
  begin
    Sleep(Interval);
    Synchronize(@TimeTick);
  end;
end;

procedure TLccTimerThread.TimeTick;
begin
  if Assinged(OnTimeTick) then
    OnTimeTick(Self)
end;

{ TLccConnectionThread }

{$IFDEF FPC}
constructor TLccConnectionThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
{$ELSE}
constructor TLccConnectionThread.Create(CreateSuspended: Boolean);
{$ENDIF}
begin
  inherited Create(CreateSuspended {$IFDEF FPC}, StackSize{$ENDIF});
  FMsgAssembler := TLccMessageAssembler.Create;
  FMsgDisAssembler := TLccMessageDisAssembler.Create;
  FWorkerMsg := TLccMessage.Create;
  FMsgStringList := TStringList.Create;
  FOutgoingCircularArray := TThreadedCirularArray.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
  OutgoingGridConnect.Delimiter := #10;
end;

destructor TLccConnectionThread.Destroy;
begin
  FreeAndNil(FMsgAssembler);
  FreeAndNIl(FMsgDisAssembler);
  FreeAndNil(FWorkerMsg);
  FreeAndNil(FMsgStringList);
  FreeAndNil(FOutgoingCircularArray);
  FreeAndNil(FOutgoingGridConnect);
  inherited Destroy;
end;

function TLccConnectionThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

end.

