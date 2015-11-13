unit lcc_raspberrypi_spiport;

{$mode objfpc}{$H+}

{$DEFINE LOGGING}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF CPUARM}
    {$IFDEF FPC}
    LResources, Forms, Controls, Graphics, Dialogs, baseUnix,
    {$ENDIF}
    {$IFDEF LOGGING}
    frame_lcc_logging, lcc_detailed_logging,
    {$ENDIF}
    lcc_gridconnect, synaser, lcc_threaded_stringlist,
    lcc_nodemanager, lcc_messages, lcc_defines, lcc_utilities, lcc_app_common_settings,
    lcc_common_classes, file_utilities, lcc_compiler_types, lcc_can_message_assembler_disassembler,
  {$ENDIF}
  {$IFDEF CPUARM}
  lcc_raspberrypi,
  {$ENDIF}
  contnrs ;

{$IFDEF CPUARM}
type
  TLccRaspberryPiSpiPortThread = class;
  TLccRaspberryPiSpiPort = class;

  TLccRaspberryPiSpiPortRec = record
    Thread: TLccRaspberryPiSpiPortThread;         // Thread owing the Record
    Port: String;                    // Spiport
    Mode: TPiSpiMode;
    Bits: TPiSpiBits;
    Speed: TPiSpiSpeed;
    ConnectionState: TConnectionState;   // Current State of the connection
    MessageStr: String;               // Contains the string for the resuting message from the thread
    LccMessage: TLccMessage;
    SuppressNotification: Boolean;       // True to stop any Syncronoize() call being called
  end;

  TOnRaspberryPiSpiChangeFunc = procedure(Sender: TObject; PiSpiPortRec: TLccRaspberryPiSpiPortRec) of object;
  TOnRaspberryPiSpiReceiveFunc = procedure(Sender: TObject; PiSpiPortRec: TLccRaspberryPiSpiPortRec) of object;

  { TLccRaspberryPiSpiPortThread }

  TLccRaspberryPiSpiPortThread = class(TLccConnectionThread)
    private
      FOnErrorMessage: TOnRaspberryPiSpiChangeFunc;
      FRaspberryPiSpiPortRec: TLccRaspberryPiSpiPortRec;
      FOnConnectionStateChange: TOnRaspberryPiSpiChangeFunc;
      FOnReceiveMessage: TOnRaspberryPiSpiReceiveFunc;
      FOnSendMessage: TOnMessageEvent;
      FOwner: TLccRaspberryPiSpiPort;
      FRaspberryPiSpi: TRaspberryPiSpi;                          // PiSpi object
    protected
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage);

      property RaspberryPiSpiPortRec: TLccRaspberryPiSpiPortRec read FRaspberryPiSpiPortRec write FRaspberryPiSpiPortRec;
      property RaspberryPiSpi: TRaspberryPiSpi read FRaspberryPiSpi write FRaspberryPiSpi;
      property OnConnectionStateChange: TOnRaspberryPiSpiChangeFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnErrorMessage: TOnRaspberryPiSpiChangeFunc read FOnErrorMessage write FOnErrorMessage;
      property OnReceiveMessage: TOnRaspberryPiSpiReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
      property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
      property Owner: TLccRaspberryPiSpiPort read FOwner write FOwner;
    public
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccRaspberryPiSpiPort; const APiSpiPortRec: TLccRaspberryPiSpiPortRec); reintroduce; virtual;
      destructor Destroy; override;
  end;

  { TLccRaspberryPiSpiPortThreadList }

  TLccRaspberryPiSpiPortThreadList = class(TThreadList)      // Contains TClientSocketThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure CloseRaspberryPiSpiPorts;
    procedure CloseRaspberryPiSpiPort(RaspberryPiSpiPortThread: TLccRaspberryPiSpiPortThread);

    property Count: Integer read GetCount;
  end;

  { TLccRaspberryPiSpiPort }

  TLccRaspberryPiSpiPort = class(TLccHardwareConnectionManager)
  private
    FRaspberryPiSpiPortThreads: TLccRaspberryPiSpiPortThreadList;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FLoggingFrame: TFrameLccLogging;
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnRaspberryPiSpiChangeFunc;
    FOnConnectionStateChange: TOnRaspberryPiSpiChangeFunc;
    FOnReceiveMessage: TOnRaspberryPiSpiReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FSleepCount: Integer;
    procedure SetOnSendMessage(AValue: TOnMessageEvent);
    procedure SetSleepCount(AValue: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateThreadEvents(PiSpiPortThread: TLccRaspberryPiSpiPortThread);
    procedure UpdateThreadsEvents;
  public
    { Public declarations }
    property RaspberryPiSpiPortThreads: TLccRaspberryPiSpiPortThreadList read FRaspberryPiSpiPortThreads write FRaspberryPiSpiPortThreads;
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FormatRaspberryPiSpiPortString(PiSpiPort: string): string;
    function OpenConnection(const APiSpiPortRec: TLccRaspberryPiSpiPortRec): TLccRaspberryPiSpiPortThread;
    function OpenConnectionWithLccSettings: TLccRaspberryPiSpiPortThread;
    procedure CloseConnection(PiSpiPortThread: TLccRaspberryPiSpiPortThread);
    procedure SendMessage(AMessage: TLccMessage); override;
    procedure SendMessageRawGridConnect(GridConnectStr: ansistring); override;
  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnRaspberryPiSpiChangeFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnRaspberryPiSpiChangeFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnRaspberryPiSpiReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write SetSleepCount;
  end;

  {$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF CPUARM}
 // {$I TLccRaspberryPiSpiPort.lrs}
  RegisterComponents('LCC',[TLccRaspberryPiSpiPort]);
  {$ENDIF}
end;

{$IFDEF CPUARM}

{ TLccRaspberryPiSpiPort }

procedure TLccRaspberryPiSpiPort.SetOnSendMessage(AValue: TOnMessageEvent);
begin
  if FOnSendMessage <> AValue then
  begin
    FOnSendMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccRaspberryPiSpiPort.SetSleepCount(AValue: Integer);
var
  i: Integer;
  L: TList;
begin
  if FSleepCount=AValue then Exit;
  FSleepCount := AValue;
  L := RaspberryPiSpiPortThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
      TLccRaspberryPiSpiPortThread( L[i]).SleepCount := SleepCount;
  finally
    RaspberryPiSpiPortThreads.UnlockList;
  end;
end;

procedure TLccRaspberryPiSpiPort.UpdateThreadEvents(PiSpiPortThread: TLccRaspberryPiSpiPortThread);
begin
  PiSpiPortThread.OnSendMessage := OnSendMessage;
end;

procedure TLccRaspberryPiSpiPort.UpdateThreadsEvents;
var
  i: Integer;
  L: TList;
begin
  L := RaspberryPiSpiPortThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
       UpdateThreadEvents(TLccRaspberryPiSpiPortThread( L[i]));
  finally
    RaspberryPiSpiPortThreads.UnlockList;
  end;
end;

constructor TLccRaspberryPiSpiPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRaspberryPiSpiPortThreads := TLccRaspberryPiSpiPortThreadList.Create;
  FHub := False;
end;

destructor TLccRaspberryPiSpiPort.Destroy;
begin
  FreeAndNil(FRaspberryPiSpiPortThreads);
  inherited Destroy;
end;

function TLccRaspberryPiSpiPort.FormatRaspberryPiSpiPortString(PiSpiPort: string): string;
begin
  {$IFDEF MSWINDOWS}
    Result := PiSpiPort;
  {$ELSE}
    {$IFDEF DARWIN}
    Result := PATH_OSX_DEV + PiSpiPort;
    {$ELSE}
    Result := PATH_LINUX_DEV + PiSpiPort;
    {$ENDIF}
  {$ENDIF}
end;

function TLccRaspberryPiSpiPort.OpenConnection(const APiSpiPortRec: TLccRaspberryPiSpiPortRec): TLccRaspberryPiSpiPortThread;
begin
  Result := TLccRaspberryPiSpiPortThread.Create(True, Self, APiSpiPortRec);
  Result.OnConnectionStateChange := OnConnectionStateChange;
  Result.OnErrorMessage := OnErrorMessage;
  Result.OnReceiveMessage := OnReceiveMessage;
  Result.OnSendMessage := OnSendMessage;
  Result.SleepCount := SleepCount;
  RaspberryPiSpiPortThreads.Add(Result);
  Result.Suspended := False;
end;

function TLccRaspberryPiSpiPort.OpenConnectionWithLccSettings: TLccRaspberryPiSpiPortThread;
var
  APiSpiPortRec: TLccRaspberryPiSpiPortRec;
begin
  if Assigned(LccSettings) then
  begin
    APiSpiPortRec.Speed := LccSettings.PiSpiPort.Speed;
    APiSpiPortRec.Mode := LccSettings.PiSpiPort.Mode;
    APiSpiPortRec.Bits := LccSettings.PiSpiPort.Bits;
    APiSpiPortRec.Port := FormatRaspberryPiSpiPortString(LccSettings.PiSpiPort.Port);
    APiSpiPortRec.LccMessage := nil;
    APiSpiPortRec.MessageStr := '';
    APiSpiPortRec.Thread := nil;
    APiSpiPortRec.SuppressNotification := False;
    APiSpiPortRec.ConnectionState := ccsPortDisconnected;

    Result := OpenConnection(APiSpiPortRec);
  end;
end;

procedure TLccRaspberryPiSpiPort.CloseConnection(PiSpiPortThread: TLccRaspberryPiSpiPortThread);
begin
  if Assigned(PiSpiPortThread) then
  begin
    RaspberryPiSpiPortThreads.Remove(PiSpiPortThread);
    RaspberryPiSpiPortThreads.CloseRaspberryPiSpiPort(PiSpiPortThread);
  end else
    RaspberryPiSpiPortThreads.CloseRaspberryPiSpiPorts;
end;

procedure TLccRaspberryPiSpiPort.SendMessage(AMessage: TLccMessage);
var
  L: TList;
  PiSpiPortThread: TLccRaspberryPiSpiPortThread;
  i: Integer;
begin
  L := RaspberryPiSpiPortThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      PiSpiPortThread := TLccRaspberryPiSpiPortThread( L[i]);
      if not PiSpiPortThread.IsTerminated then
        PiSpiPortThread.SendMessage(AMessage);
    end;
  finally
    RaspberryPiSpiPortThreads.UnlockList;
  end;
end;

procedure TLccRaspberryPiSpiPort.SendMessageRawGridConnect(GridConnectStr: ansistring);
var
  List: TList;
  StrList: TStringList;
  i: Integer;
  OldText, NewText: ansistring;
begin
  List := RaspberryPiSpiPortThreads.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      StrList := TLccRaspberryPiSpiPortThread(List[i]).OutgoingGridConnect.LockList;
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
         TLccRaspberryPiSpiPortThread(List[i]).OutgoingGridConnect.UnlockList;
      end;
    end;
  finally
    RaspberryPiSpiPortThreads.UnlockList;
  end;
end;

{ TLccRaspberryPiSpiPortThreadList }

function TLccRaspberryPiSpiPortThreadList.GetCount: Integer;
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

destructor TLccRaspberryPiSpiPortThreadList.Destroy;
begin
  CloseRaspberryPiSpiPorts;
  inherited Destroy;;
end;

procedure TLccRaspberryPiSpiPortThreadList.CloseRaspberryPiSpiPorts;
var
  L: TList;
  Thread: TLccRaspberryPiSpiPortThread;
begin
  while Count > 0 do
  begin
    L := LockList;
    try
      Thread := TLccRaspberryPiSpiPortThread( L[0]);
      L.Delete(0);
    finally
      UnlockList;
    end;
    CloseRaspberryPiSpiPort(Thread);
  end;
end;

procedure TLccRaspberryPiSpiPortThreadList.CloseRaspberryPiSpiPort(
  RaspberryPiSpiPortThread: TLccRaspberryPiSpiPortThread);
//var
//  TimeCount: Cardinal;
begin
  RaspberryPiSpiPortThread.Terminate;
//  TimeCount := GetTickCount;            DON"T LINK OCLB_UTILITES, it causes issues with linking to different packages
  while (RaspberryPiSpiPortThread.Running) do
  begin
 //   if (GetTickCount - TimeCount < 5000) then
      Application.ProcessMessages
 //   else begin
  //    KillThread(ComPortThread.Handle);
  //    ComPortThread.Running := False;
  //  end;
  end;
  FreeAndNil( RaspberryPiSpiPortThread);
end;

{ TLccRaspberryPiSpiPortThread }

procedure TLccRaspberryPiSpiPortThread.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FRaspberryPiSpiPortRec)
end;

procedure TLccRaspberryPiSpiPortThread.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FRaspberryPiSpiPortRec)
  end;
end;

procedure TLccRaspberryPiSpiPortThread.DoReceiveMessage;
var
  LocalMessage: TLccMessage;
begin
  if not IsTerminated then
  begin
    if GridConnect then
    begin
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
        PrintToSynEdit( 'R PiSpiPort : ' + RaspberryPiSpiPortRec.MessageStr,
                        Owner.LoggingFrame.SynEdit,
                        Owner.LoggingFrame.ActionLogPause.Checked,
                        Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                        Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);

      // Called in the content of the main thread through Syncronize
      // Send all raw GridConnect Messages to the event
      if Assigned(OnReceiveMessage) then
        OnReceiveMessage(Self, FRaspberryPiSpiPortRec);

      LocalMessage := nil;
      if Owner.NodeManager <> nil then
        if MsgAssembler.IncomingMessageGridConnect(FRaspberryPiSpiPortRec.MessageStr, LocalMessage) = imgcr_True then // In goes a raw message
          Owner.NodeManager.ProcessMessage(LocalMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages
    end else
    begin
    //  Fill in TCP Code from Ethernet
    end;
  end;
end;

procedure TLccRaspberryPiSpiPortThread.DoSendMessage(AMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, AMessage);
end;

procedure TLccRaspberryPiSpiPortThread.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FRaspberryPiSpiPortRec.ConnectionState := NewConnectionState;
    if not FRaspberryPiSpiPortRec.SuppressNotification then
      Synchronize(@DoConnectionState);
  end;

  procedure HandleErrorAndDisconnect;
  begin
    Owner.RaspberryPiSpiPortThreads.Remove(Self);
    FRaspberryPiSpiPortRec.MessageStr := RaspberryPiSpi.LastErrorDesc;
    if not FRaspberryPiSpiPortRec.SuppressNotification then
      Synchronize(@DoErrorMessage);
    SendConnectionNotification(ccsPortDisconnected);
    Terminate;
  end;

  const
    GRIDCONNECT_STR_LEN = 32;    // Number of GC strings message to send in one TX cycle
    GRIDCONNECT_CHAR_LEN = 30;  // 30 characters in the GC string
    TX_BUFFER_LEN = GRIDCONNECT_CHAR_LEN * GRIDCONNECT_STR_LEN;  // Spi buffer holds GRIDCONNECT_STR_LEN GridConnect strings

  type
    TRaspberryPiBuffer = array[0..TX_BUFFER_LEN-1] of byte;

  procedure LoadSpiTxBuffer(var TxBuffer: TRaspberryPiBuffer; var GridConnect: TGridConnectString; Index: Integer);
  var
    i, Offset: Integer;
  begin
    Offset := GRIDCONNECT_CHAR_LEN * Index;
    for i := 0 to GRIDCONNECT_CHAR_LEN-1 do
      TxBuffer[Offset + i] := GridConnect[i];
  end;
var
  i, j, ByteCount: Integer;
  TxList: TStringList;
  LocalSleepCount: Integer;
  SpiRec: TSpiIocTransfer;
  TxBuffer, RxBuffer:  TRaspberryPiBuffer;  // Spi needs an extra one at then end
  GridConnectBuffer: TGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  GridConnectStrPtr: PGridConnectString;
  s: ansistring;
begin
  FRunning := True;

  SendConnectionNotification(ccsPortConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  RaspberryPiSpi := TRaspberryPiSpi.Create;
  RaspberryPiSpi.Speed := FRaspberryPiSpiPortRec.Speed;
  RaspberryPiSpi.Mode := FRaspberryPiSpiPortRec.Mode;
  RaspberryPiSpi.Bits := FRaspberryPiSpiPortRec.Bits;
  if not RaspberryPiSpi.OpenSpi(FRaspberryPiSpiPortRec.Port) then
  begin
    HandleErrorAndDisconnect;
    Running := False;
  end
  else begin
    SendConnectionNotification(ccsPortConnected);
    try
      LocalSleepCount := 0;
      while not IsTerminated and (FRaspberryPiSpiPortRec.ConnectionState = ccsPortConnected) do
      begin

        FillChar(RxBuffer, SizeOf(RxBuffer), 0);
        FillChar(TxBuffer, SizeOf(TxBuffer), 0);

        // Transmit new message on every N (SleepCount) Receive trys
        if LocalSleepCount >= SleepCount then
        begin
          TxList := OutgoingGridConnect.LockList;
          try
            i := 0;
            while TxList.Count > 0 do
            begin
              FillChar(GridConnectBuffer, SizeOf(GridConnectBuffer), 0);
              s := TxList[0];
              for j := 0 to Length(s)-1 do
                GridConnectBuffer[j] := Ord(s[j+1]);
              LoadSpiTxBuffer(TxBuffer, GridConnectBuffer, i);
              TxList.Delete(0);
              Inc(i);
              if i > GRIDCONNECT_STR_LEN-1 then
                Break
            end;
          finally
            OutgoingGridConnect.UnlockList;
          end;
          LocalSleepCount := 0;
        end;
        Inc(LocalSleepCount);

        // if nothing to send still pump receive messages by sending nulls
        ByteCount := i*GRIDCONNECT_CHAR_LEN;
        if ByteCount = 0 then
          ByteCount := GRIDCONNECT_CHAR_LEN;

        if RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, ByteCount) then
        begin
         for i := 0 to ByteCount-1 do
         begin
           GridConnectStrPtr := nil;

           if GridConnectHelper.GridConnect_DecodeMachine(Ord( RxBuffer[i]), GridConnectStrPtr) then
           begin
             FRaspberryPiSpiPortRec.MessageStr := NullArrayToString(GridConnectStrPtr^);
             FRaspberryPiSpiPortRec.LccMessage.LoadByGridConnectStr(FRaspberryPiSpiPortRec.MessageStr);
             Synchronize(@DoReceiveMessage);
           end;
         end;
        end else
          HandleErrorAndDisconnect;
      end;
    finally
      SendConnectionNotification(ccsPortDisconnecting);
      FRunning := False;
      RaspberryPiSpi.CloseSpi;
      RaspberryPiSpi.Free;
      GridConnectHelper.Free;
      SendConnectionNotification(ccsPortDisconnected);
    end;
  end;
end;

procedure TLccRaspberryPiSpiPortThread.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
begin
  if not IsTerminated then
  begin
    if GridConnect then
    begin
      MsgDisAssembler.OutgoingMsgToMsgList(AMessage, MsgStringList);

      for i := 0 to MsgStringList.Count - 1 do
      begin
      OutgoingGridConnect.Add(MsgStringList[i]);
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
        PrintToSynEdit( 'S ComPort: ' + MsgStringList[i],
                        Owner.LoggingFrame.SynEdit,
                        Owner.LoggingFrame.ActionLogPause.Checked,
                        Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                        Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
      end;
      DoSendMessage(AMessage);
    end else
    begin
    //  Fill in for TCP
    end;
  end;
end;

constructor TLccRaspberryPiSpiPortThread.Create(CreateSuspended: Boolean; AnOwner: TLccRaspberryPiSpiPort; const APiSpiPortRec: TLccRaspberryPiSpiPortRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FRaspberryPiSpiPortRec := APiSpiPortRec;
  FRaspberryPiSpiPortRec.Thread := Self;
  FRaspberryPiSpiPortRec.LccMessage := TLccMessage.Create;
  GridConnect := True;
end;

destructor TLccRaspberryPiSpiPortThread.Destroy;
begin
  FreeAndNil(FRaspberryPiSpiPortRec.LccMessage);
  inherited Destroy;
end;


initialization
  RegisterClass(TLccRaspberryPiSpiPort);

finalization

{$ENDIF}

end.

