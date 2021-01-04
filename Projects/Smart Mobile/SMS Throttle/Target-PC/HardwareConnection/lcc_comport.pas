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

  TLccComPortRec = record
    Thread: TLccComPortThread;           // Thread owing the Record
    ComPort: String;                     // Comport
    Baud: Integer;                       // Define connection speed. Baud rate can be from 50 to 4000000 bits per second. (it depends on your hardware!))
    Bits: Integer;                       // Number of bits in communication.
    Parity: Char;                        // Define communication parity (N - None, O - Odd, E - Even, M - Mark or S - Space)
    StopBits: Integer;                   // Use constants SB1, SB1andHalf, SB2
    SoftwareHandshake: Boolean;          // Enable XON/XOFF handshake.
    HardwareHandShake: Boolean;          // Enable CTS/RTS handshake
    ConnectionState: TConnectionState;   // Current State of the connection
    MessageStr: String;                  // Contains the string for the resuting message from the thread
    MessageArray: lcc_defines.TDynamicByteArray;   // Contains the TCP Protocol message bytes of not using GridConnect
    LccMessage: TLccMessage;
    SuppressNotification: Boolean;       // True to stop any Syncronoize() call being called
  end;


  TOnComChangeFunc = procedure(Sender: TObject; ComPortRec: TLccComPortRec) of object;
  TOnComReceiveFunc = procedure(Sender: TObject; ComPortRec: TLccComPortRec) of object;



  { TLccComPortThread }

  TLccComPortThread =  class(TLccConnectionThread)
    private
      FComPortRec: TLccComPortRec;
      FOnComErrorMessage: TOnComChangeFunc;
      FOnConnectionStateChange: TOnComChangeFunc;
      FOnReceiveMessage: TOnComReceiveFunc;
      FOnSendMessage: TOnMessageEvent;
      FOwner: TLccComPort;
      FRawData: Boolean;
      FSerial: TBlockSerial;                                                      // Serial object
    protected
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage); override;
      procedure ReceiveMessage; override;

      property ComPortRec: TLccComPortRec read FComPortRec write FComPortRec;
      property Serial: TBlockSerial read FSerial write FSerial;
      property OnConnectionStateChange: TOnComChangeFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnErrorMessage: TOnComChangeFunc read FOnComErrorMessage write FOnComErrorMessage;
      property OnReceiveMessage: TOnComReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
      property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
      property Owner: TLccComPort read FOwner write FOwner;
    public
      property RawData: Boolean read FRawData write FRawData;

      constructor Create(CreateSuspended: Boolean; AnOwner: TLccComPort; const AComPortRec: TLccComPortRec); reintroduce;
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
    FComPortThreads: TLccComPortThreadList;
    FConnected: Boolean;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnComChangeFunc;
    FOnConnectionStateChange: TOnComChangeFunc;
    FOnReceiveMessage: TOnComReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FRawData: Boolean;
    FSleepCount: Integer;
    procedure SetOnSendMessage(AValue: TOnMessageEvent);
    procedure SetSleepCount(AValue: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateThreadEvents(ComPortThread: TLccComPortThread);
    procedure UpdateThreadsEvents;

    function IsLccLink: Boolean; override;
  public
    { Public declarations }
    property ComPortThreads: TLccComPortThreadList read FComPortThreads write FComPortThreads;
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector
    property Connected: Boolean read FConnected;
    property RawData: Boolean read FRawData write FRawData;

    constructor Create(AOwner: TComponent; ANodeManager: TLccNodeManager); override;
    destructor Destroy; override;

    function FormatComPortString(ComPort: string): string;
    function OpenComPort(const AComPortRec: TLccComPortRec): TLccComPortThread;
    function OpenComPortWithLccSettings: TLccComPortThread;
    procedure CloseComPort( ComPortThread: TLccComPortThread);
    procedure SendMessage(AMessage: TLccMessage); override;
    procedure SendMessageRawGridConnect(GridConnectStr: ansistring); override;
  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnComChangeFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnComChangeFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnComReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write SetSleepCount;
  end;


implementation

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

procedure TLccComPort.CloseComPort(ComPortThread: TLccComPortThread);
begin
  if Assigned(ComPortThread) then
  begin
    ComPortThreads.Remove(ComPortThread);
    ComPortThreads.CloseComPort(ComPortThread);
  end else
    ComPortThreads.CloseComPorts;
end;

procedure TLccComPort.SetSleepCount(AValue: Integer);
var
  i: Integer;
  L: TList;
begin
  if FSleepCount=AValue then Exit;
  FSleepCount := AValue;
  L := ComPortThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
      TLccComPortThread( L[i]).SleepCount := SleepCount;
  finally
    ComPortThreads.UnlockList;
  end;
end;

procedure TLccComPort.UpdateThreadEvents(ComPortThread: TLccComPortThread);
begin
  ComPortThread.OnSendMessage := OnSendMessage;
end;

procedure TLccComPort.UpdateThreadsEvents;
var
  i: Integer;
  L: TList;
begin
  L := ComPortThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
       UpdateThreadEvents(TLccComPortThread( L[i]));
  finally
    ComPortThreads.UnlockList;
  end;
end;

function TLccComPort.IsLccLink: Boolean;
begin
  Result := False;
end;

constructor TLccComPort.Create(AOwner: TComponent; ANodeManager: TLccNodeManager);
begin
  inherited;
  FComPortThreads := TLccComPortThreadList.Create;
  FHub := False;
end;

destructor TLccComPort.Destroy;
begin
  FreeAndNil( FComPortThreads);
  inherited Destroy;
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

function TLccComPort.OpenComPort(const AComPortRec: TLccComPortRec): TLccComPortThread;
begin
  Result := TLccComPortThread.Create(True, Self, AComPortRec);
  Result.OnConnectionStateChange := OnConnectionStateChange;
  Result.OnErrorMessage := OnErrorMessage;
  Result.OnReceiveMessage := OnReceiveMessage;
  Result.OnSendMessage := OnSendMessage;
  Result.SleepCount := SleepCount;
  Result.RawData := RawData;
  {$IFDEF MSWINDOWS}

  {$ELSE}
    {$IFDEF DARWIN}
    Result.FComPortRec.ComPort := PATH_OSX_DEV + Result.ComPortRec.ComPort;
    {$ELSE}
    Result.FComPortRec.ComPort := PATH_LINUX_DEV + Result.ComPortRec.ComPort;
    {$ENDIF}
  {$ENDIF}

  ComPortThreads.Add(Result);
  Result.Suspended := False;
end;

function TLccComPort.OpenComPortWithLccSettings: TLccComPortThread;
var
  AComPortRec: TLccComPortRec;
begin
  if Assigned(LccSettings) then
  begin
    AComPortRec.Baud := LccSettings.ComPort.BaudRate;
    AComPortRec.ComPort := FormatComPortString(LccSettings.ComPort.Port);

    case LccSettings.ComPort.StopBits of
      cpsb_1_StopBit   : AComPortRec.StopBits := SB1;
      cpsb_1_5_StopBit : AComPortRec.StopBits := SB1andHalf;
      cpsb_2_StopBit   : AComPortRec.StopBits := SB2;
    end;

    case LccSettings.ComPort.DataBits of
      cpdb_8_Bits : AComPortRec.Bits :=  8;
      cpdb_9_Bits : AComPortRec.Bits :=  9;
    end;

    case LccSettings.ComPort.FlowControl of
      cpf_None      :
        begin
          AComPortRec.HardwareHandShake := False;
          AComPortRec.SoftwareHandshake := False;
        end;
      cpf_CTS_RTS,                // Hardware with CTS/RTS
      cpf_DTR_DSR :              // Hardware with DTR/DSR
        begin
          AComPortRec.HardwareHandShake := True;
          AComPortRec.SoftwareHandshake := False;
        end;
      cpf_XON_XOFF :            // Software;
        begin
          AComPortRec.HardwareHandShake := False;
          AComPortRec.SoftwareHandshake := True;
        end;
    end;

    case LccSettings.ComPort.Parity of
      cpp_None    : AComPortRec.Parity := 'N';
      cpp_Even    : AComPortRec.Parity := 'E';
      cpp_Odd     : AComPortRec.Parity := 'O';
      cpp_Mark    : AComPortRec.Parity := 'M';
      cpp_Space   : AComPortRec.Parity := 'S';
    end;

    AComPortRec.LccMessage := nil;
    AComPortRec.MessageStr := '';
    AComPortRec.Thread := nil;
    AComPortRec.SuppressNotification := False;
    AComPortRec.ConnectionState := ccsPortDisconnected;

    Result := OpenComPort(AComPortRec);
  end;
end;

procedure TLccComPort.SendMessage(AMessage: TLccMessage);
var
  L: TList;
  ComPortThread: TLccComPortThread;
  i: Integer;
begin
  L := ComPortThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      ComPortThread := TLccComPortThread( L[i]);
      if not ComPortThread.IsTerminated then
        ComPortThread.SendMessage(AMessage);
    end;
  finally
    ComPortThreads.UnlockList;
  end;
end;

procedure TLccComPort.SendMessageRawGridConnect(GridConnectStr: ansistring);
var
  List: TList;
  StrList: TStringList;
  i: Integer;
  OldText, NewText: ansistring;
begin
  List := ComPortThreads.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      StrList := TLccComPortThread(List[i]).OutgoingGridConnect.LockList;
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
         TLccComPortThread(List[i]).OutgoingGridConnect.UnlockList;
      end;
    end;
  finally
    ComPortThreads.UnlockList;
  end;
end;

procedure TLccComPort.SetOnSendMessage(AValue: TOnMessageEvent);
begin
  if FOnSendMessage <> AValue then
  begin
    FOnSendMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

{ TLccComPortThread }

procedure TLccComPortThread.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FComPortRec.ConnectionState := NewConnectionState;
    if not FComPortRec.SuppressNotification then
      Synchronize(@DoConnectionState);
  end;

  procedure HandleErrorAndDisconnect;
  begin
    Owner.ComPortThreads.Remove(Self);
    FComPortRec.MessageStr := Serial.LastErrorDesc;
    if not FComPortRec.SuppressNotification then
      Synchronize(@DoErrorMessage);
    SendConnectionNotification(ccsPortDisconnected);
    Terminate;
  end;

var
  TxStr, RcvStr: String;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList: TStringList;
  LocalSleepCount: Integer;
  DynamicByteArray: TDynamicByteArray;
  RcvByte: Byte;
begin
  FRunning := True;

  SendConnectionNotification(ccsPortConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Serial := TBlockSerial.Create;                                                // Create the Serial object in the context of the thread
  Serial.LinuxLock:=False;
  Serial.RaiseExcept:=False;
  Serial.Connect(FComPortRec.ComPort);
  if Serial.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Running := False;
  end
  else begin
    Serial.Config(FComPortRec.Baud, FComPortRec.Bits, FComPortRec.Parity, FComPortRec.StopBits, FComPortRec.SoftwareHandshake, FComPortRec.HardwareHandShake);
    if Serial.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Serial.CloseSocket;
      Serial.Free;
      Serial := nil;
      Running := False;
    end
    else begin
      SendConnectionNotification(ccsPortConnected);
      try
        try
          LocalSleepCount := 0;
          while not IsTerminated and (FComPortRec.ConnectionState = ccsPortConnected) do
          begin
            if Gridconnect then              // Handle the ComPort using GridConnect
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
                  FComPortRec.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                  if not RawData then
                    FComPortRec.LccMessage.LoadByGridConnectStr(FComPortRec.MessageStr);
                  Synchronize(@DoReceiveMessage);
                end;
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
                    if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, FComPortRec.MessageArray) then
                    begin
                      if UseSynchronize then
                        Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage)
                      else begin
                        DynamicByteArray := nil;
                        Owner.IncomingCircularArray.LockArray;
                        try
                          Owner.IncomingCircularArray.AddChunk(FComPortRec.MessageArray);
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
                    FComPortRec.MessageStr := Serial.LastErrorDesc;
          //          Synchronize({$IFDEF FPC}@{$ENDIF}DoClientDisconnect);
                    FComPortRec.MessageStr := '';
                    Terminate;
                  end
              else
                HandleErrorAndDisconnect
              end;
            end;
          end;
        finally
          SendConnectionNotification(ccsPortDisconnecting);

          if Serial.InstanceActive then
            Serial.CloseSocket;
          Serial.Free;
          GridConnectHelper.Free;
        end;
      finally
        SendConnectionNotification(ccsPortDisconnected);
        Owner.ComPortThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

procedure TLccComPortThread.ReceiveMessage;
begin

end;

procedure TLccComPortThread.SendMessage(AMessage: TLccMessage);
var
  ByteArray: TDynamicByteArray;
  i: Integer;
begin
  if not IsTerminated then
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
    DoSendMessage(AMessage);
  end;
end;


constructor TLccComPortThread.Create(CreateSuspended: Boolean; AnOwner: TLccComPort; const AComPortRec: TLccComPortRec);
begin
  inherited Create(CreateSuspended, AnOwner);
  FComPortRec := AComPortRec;
  FComPortRec.Thread := Self;
  FComPortRec.LccMessage := TLccMessage.Create;
  GridConnect := True;
end;

destructor TLccComPortThread.Destroy;
begin
  FreeAndNil(FComPortRec.LccMessage);
  inherited Destroy;
end;

procedure TLccComPortThread.DoConnectionState;
begin
  Owner.FConnected := FComPortRec.ConnectionState = ccsPortConnected;

  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FComPortRec)
end;

procedure TLccComPortThread.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FComPortRec)
  end;
end;

procedure TLccComPortThread.DoReceiveMessage;
var
  L: TList;
  i: Integer;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    if Assigned(OnReceiveMessage) then    // Do first so we get notified before any response is sent in ProcessMessage
      OnReceiveMessage(Self, FComPortRec);

    if Gridconnect then
    begin
      if Owner.NodeManager <> nil then
        Owner.NodeManager.ProcessMessage(FComPortRec.LccMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

      if Owner.Hub then
      begin
        L := Owner.ComPortThreads.LockList;
        try
          for i := 0 to L.Count - 1 do
          begin
            if TLccComPortThread(L[i]) <> Self then
              TLccComPortThread(L[i]).SendMessage(FComPortRec.LccMessage);
          end;
        finally
          Owner.ComPortThreads.UnlockList;
        end
      end;
    end else
    begin   // TCP Protocol
      if WorkerMessage.LoadByLccTcp(FComPortRec.MessageArray) then // In goes a raw message
      begin
        if (Owner.NodeManager <> nil) then
          Owner.NodeManager.ProcessMessage(WorkerMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

        if Owner.Hub then
        begin
          L := Owner.ComPortThreads.LockList;
          try
            for i := 0 to L.Count - 1 do
            begin
              if TLccComPortThread(L[i]) <> Self then
                TLccComPortThread(L[i]).SendMessage(WorkerMessage);
            end;
          finally
            Owner.ComPortThreads.UnlockList;
          end
        end
      end
    end;
  end
end;

procedure TLccComPortThread.DoSendMessage(AMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, AMessage);
end;

initialization
  RegisterClass(TLccComPort);

finalization

end.
