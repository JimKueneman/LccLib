unit lcc_comport;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP} LResources, Forms, Controls, Graphics, Dialogs, {$ENDIF}
  {$ELSE}
  FMX.Forms, Types, System.Generics.Collections,
  {$ENDIF}

  {$IFDEF ULTIBO}
  lcc_threaded_stringlist,
  Winsock2,
  Console,
  {$ELSE}
  blcksock,
  synsock,
  {$ENDIF}
  synaser,
  lcc_threaded_circulararray,
  lcc_threaded_stringlist,
  lcc_gridconnect,
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_app_common_settings,
  lcc_common_classes,
  lcc_ethernet_tcp,
  lcc_node_messages_can_assembler_disassembler;

type
  TLccComPortThread = class;             // Forward
  TLccComPort = class;
  TLccComPortConnectionInfo = class;

  { TComPortConnectionInfo }

  { TLccComPortConnectionInfo }

  TLccComPortConnectionInfo = class(TLccHardwareConnectionInfo)
  private
    FBaud: Integer;
    FBits: Integer;
    FComPort: String;
    FHardwareHandshake: Boolean;
    FParity: Char;
    FSoftwareHandshake: Boolean;
    FStopBits: Integer;
  public
    property ComPort: String read FComPort write FComPort;                     // Comport
    property Baud: Integer read FBaud write FBaud;                      // Define connection speed. Baud rate can be from 50 to 4000000 bits per second. (it depends on your hardware!))
    property Bits: Integer read FBits write FBits;                      // Number of bits in communication.
    property Parity: Char read FParity write FParity;                        // Define communication parity (N - None, O - Odd, E - Even, M - Mark or S - Space)
    property StopBits: Integer read FStopBits write FStopBits;                   // Use constants SB1, SB1andHalf, SB2
    property SoftwareHandshake: Boolean read FSoftwareHandshake write FSoftwareHandshake;          // Enable XON/XOFF handshake.
    property HardwareHandShake: Boolean read FHardwareHandshake write FHardwareHandshake;         // Enable CTS/RTS handshake

    function Clone: TLccHardwareConnectionInfo; override;
  end;


  { TLccComPortThread }

  TLccComPortThread =  class(TLccConnectionThread)
    private
      FRawData: Boolean;
      FSerial: TBlockSerial;                                                      // Serial object
    protected
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage); override;

      property Serial: TBlockSerial read FSerial write FSerial;
    public
      property RawData: Boolean read FRawData write FRawData;

      constructor Create(CreateSuspended: Boolean; AnOwner: TLccHardwareConnectionManager; AConnectionInfo: TLccHardwareConnectionInfo); override;
      destructor Destroy; override;
  end;

  { TLccComPortThreadList }

  TLccComPortThreadList = class(TThreadList)      // Contains TClientSocketThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure CloseComPorts;
    procedure CloseComPort(ComPortThread: TLccComPortThread);

    property Count: Integer read GetCount;
  end;

  { TLccComPort }

  TLccComPort = class(TLccHardwareConnectionManager)
  private
    FCurrentConnectionState: TLccConnectionState;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
    FRawData: Boolean;
    { Private declarations }
  protected
    { Protected declarations }
      // Property getter must override and make definition based on connection type
    function GetConnected: Boolean; override;

    function IsLccLink: Boolean; override;
  public
    { Public declarations }
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector
    property RawData: Boolean read FRawData write FRawData;
    property CurrentConnectionState: TLccConnectionState read FCurrentConnectionState write FCurrentConnectionState;  // Current State of the connection

    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); override;

    function FormatComPortString(ComPort: string): string;
    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; override;
    function OpenConnectionWithLccSettings: TLccConnectionThread;
  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
  end;


implementation

{ TLccComPortConnectionInfo }

function TLccComPortConnectionInfo.Clone: TLccHardwareConnectionInfo;
begin
  Result := inherited Clone;
  (Result as TLccComPortConnectionInfo).ComPort := ComPort;
  (Result as TLccComPortConnectionInfo).Baud := Baud;
  (Result as TLccComPortConnectionInfo).Bits := Bits;
  (Result as TLccComPortConnectionInfo).Parity := Parity;
  (Result as TLccComPortConnectionInfo).StopBits := StopBits;
  (Result as TLccComPortConnectionInfo).SoftwareHandshake := SoftwareHandshake;
  (Result as TLccComPortConnectionInfo).HardwareHandShake := HardwareHandShake;
  (Result as TLccComPortConnectionInfo).ConnectionState := ConnectionState;
end;

{ TLccComPortThreadList }

function TLccComPortThreadList.GetCount: Integer;
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

destructor TLccComPortThreadList.Destroy;
begin
  CloseComPorts;
  inherited Destroy;
end;

procedure TLccComPortThreadList.CloseComPorts;
var
  L: TList;
  Thread: TLccComPortThread;
begin
  while Count > 0 do
  begin
    L := LockList;
    try
      Thread := TLccComPortThread( L[0]);
      L.Delete(0);
    finally
      UnlockList;
    end;
    CloseComPort(Thread);
  end;
end;

procedure TLccComPortThreadList.CloseComPort( ComPortThread: TLccComPortThread);
//var
//  TimeCount: Cardinal;
begin
  ComPortThread.Terminate;
//  TimeCount := GetTickCount;            DON"T LINK OCLB_UTILITES, it causes issues with linking to different packages
  while (ComPortThread.Running) do
  begin
 //   if (GetTickCount - TimeCount < 5000) then
      Application.ProcessMessages
 //   else begin
  //    KillThread(ComPortThread.Handle);
  //    ComPortThread.Running := False;
  //  end;
  end;
  FreeAndNil( ComPortThread);
end;

{ TLccComPort }

function TLccComPort.IsLccLink: Boolean;
begin
  Result := False;
end;

constructor TLccComPort.Create(AOwner: TComponent; ANodeManager: TLccNodeManager);
begin
  inherited Create(AOwner, ANodeManager);
  FHub := False;
end;

function TLccComPort.FormatComPortString(ComPort: string): string;
begin
  {$IFDEF MSWINDOWS}
    Result := ComPort;
  {$ELSE}
    {$IFDEF DARWIN}
    Result := PATH_OSX_DEV + ComPort;
    {$ELSE}
    Result := PATH_LINUX_DEV + ComPort;
    {$ENDIF}
  {$ENDIF}
end;

function TLccComPort.GetConnected: Boolean;
begin
 // TODO Result := CurrentConnectionState = ccsClientConnected;
end;

function TLccComPort.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  Result := nil;
  {$IFDEF MSWINDOWS}

  {$ELSE}
    {$IFDEF DARWIN}
    (ConnectionInfo as TLccComPortConnectionInfo).ComPort := PATH_OSX_DEV + (ConnectionInfo as TLccComPortConnectionInfo).ComPort;
    {$ELSE}
    ConnectionInfo.ComPort := PATH_LINUX_DEV + ComPortConnectionInfo.ComPort;
    {$ENDIF}
  {$ENDIF}
  Result := TLccComPortThread.Create(True, Self, ConnectionInfo);
  (Result as TLccComPortThread).RawData := RawData;
  ConnectionThreads.Add(Result);
  Result.Suspended := False;
end;

function TLccComPort.OpenConnectionWithLccSettings: TLccConnectionThread;
var
  AComPortConnectionInfo: TLccComPortConnectionInfo;
begin
  if Assigned(LccSettings) then
  begin
    AComPortConnectionInfo := TLccComPortConnectionInfo.Create;
    try
      AComPortConnectionInfo.Baud := LccSettings.ComPort.BaudRate;
      AComPortConnectionInfo.ComPort := FormatComPortString(LccSettings.ComPort.Port);

      case LccSettings.ComPort.StopBits of
        cpsb_1_StopBit   : AComPortConnectionInfo.StopBits := SB1;
        cpsb_1_5_StopBit : AComPortConnectionInfo.StopBits := SB1andHalf;
        cpsb_2_StopBit   : AComPortConnectionInfo.StopBits := SB2;
      end;

      case LccSettings.ComPort.DataBits of
        cpdb_8_Bits : AComPortConnectionInfo.Bits :=  8;
        cpdb_9_Bits : AComPortConnectionInfo.Bits :=  9;
      end;

      case LccSettings.ComPort.FlowControl of
        cpf_None      :
          begin
            AComPortConnectionInfo.HardwareHandShake := False;
            AComPortConnectionInfo.SoftwareHandshake := False;
          end;
        cpf_CTS_RTS,                // Hardware with CTS/RTS
        cpf_DTR_DSR :              // Hardware with DTR/DSR
          begin
            AComPortConnectionInfo.HardwareHandShake := True;
            AComPortConnectionInfo.SoftwareHandshake := False;
          end;
        cpf_XON_XOFF :            // Software;
          begin
            AComPortConnectionInfo.HardwareHandShake := False;
            AComPortConnectionInfo.SoftwareHandshake := True;
          end;
      end;

      case LccSettings.ComPort.Parity of
        cpp_None    : AComPortConnectionInfo.Parity := 'N';
        cpp_Even    : AComPortConnectionInfo.Parity := 'E';
        cpp_Odd     : AComPortConnectionInfo.Parity := 'O';
        cpp_Mark    : AComPortConnectionInfo.Parity := 'M';
        cpp_Space   : AComPortConnectionInfo.Parity := 'S';
      end;

      Result := OpenConnection(AComPortConnectionInfo);
    finally
      AComPortConnectionInfo.Free;
    end;
  end;
end;

{ TLccComPortThread }

procedure TLccComPortThread.Execute;
var
  TxStr, RcvStr: String;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList: TStringList;
  LocalSleepCount: Integer;
  DynamicByteArray: TLccDynamicByteArray;
  RcvByte: Byte;
begin
  FRunning := True;

  HandleSendConnectionNotification(lcsConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Serial := TBlockSerial.Create;                                                // Create the Serial object in the context of the thread
  Serial.LinuxLock:=False;
  Serial.RaiseExcept:=False;
  Serial.Connect((ConnectionInfo as TLccComPortConnectionInfo).ComPort);
  if Serial.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Running := False;
  end
  else begin
    with (ConnectionInfo as TLccComPortConnectionInfo) do
      Serial.Config(Baud, Bits, Parity, StopBits, SoftwareHandshake, HardwareHandShake);
    if Serial.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Serial.CloseSocket;
      Serial.Free;
      Serial := nil;
      Running := False;
    end
    else begin
      HandleSendConnectionNotification(lcsConnected);
      try
        try
          LocalSleepCount := 0;
          while not IsTerminated and ((ConnectionInfo as TLccComPortConnectionInfo).ConnectionState = lcsConnected) do
          begin
            if ConnectionInfo.Gridconnect then              // Handle the ComPort using GridConnect
            begin
              if LocalSleepCount >= ConnectionInfo.SleepCount then
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
                  Serial.SendString(TxStr);
                  if Serial.LastError <> 0 then
                    HandleErrorAndDisconnect;
                end;
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              RcvStr := Serial.Recvstring(1);
              case Serial.LastError of
                0, ErrTimeout : begin end;
              else
                HandleErrorAndDisconnect
              end;
              for i := 1 to Length(RcvStr) do
              begin
                GridConnectStrPtr := nil;

                if GridConnectHelper.GridConnect_DecodeMachine(Ord( RcvStr[i]), GridConnectStrPtr) then
                begin
                  ConnectionInfo.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                  if not RawData then
                    ConnectionInfo.LccMessage.LoadByGridConnectStr(ConnectionInfo.MessageStr);
                  Synchronize(@ReceiveMessage);
                end;
              end;
            end else
            begin    // Handle the Socket with LCC TCP Protocol
              if LocalSleepCount >= ConnectionInfo.SleepCount then
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
                  Serial.SendBuffer(@DynamicByteArray[0], Length(DynamicByteArray));
                  if Serial.LastError <> 0 then
                    HandleErrorAndDisconnect;
                  DynamicByteArray := nil;
                end;
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              RcvByte := Serial.RecvByte(1);
              case Serial.LastError of
                0 :
                  begin
                    DynamicByteArray := nil;
                    if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, ConnectionInfo.MessageArray) then
                    begin
                      if ConnectionInfo.UseSyncronize then
                        Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage)
                      else begin
                        DynamicByteArray := nil;
                        Owner.IncomingCircularArray.LockArray;
                        try
                          Owner.IncomingCircularArray.AddChunk(ConnectionInfo.MessageArray);
                        finally
                          Owner.IncomingCircularArray.UnLockArray;
                        end;
                      end
                    end;
                  end;
                ErrTimeout :
                  begin

                  end;
              else
                HandleErrorAndDisconnect
              end;
            end;
          end;
        finally
          HandleSendConnectionNotification(lcsDisconnecting);

          if Serial.InstanceActive then
            Serial.CloseSocket;
          Serial.Free;
          GridConnectHelper.Free;
        end;
      finally
        HandleSendConnectionNotification(lcsDisconnected);
        Owner.ConnectionThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

procedure TLccComPortThread.SendMessage(AMessage: TLccMessage);
var
  ByteArray: TLccDynamicByteArray;
  i: Integer;
begin
  if not IsTerminated then
  begin
    if ConnectionInfo.Gridconnect then
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
end;


constructor TLccComPortThread.Create(CreateSuspended: Boolean;
  AnOwner: TLccHardwareConnectionManager;
  AConnectionInfo: TLccHardwareConnectionInfo);
begin
  inherited Create(CreateSuspended, AnOwner, AConnectionInfo);
end;

destructor TLccComPortThread.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TLccComPort);

finalization

end.
