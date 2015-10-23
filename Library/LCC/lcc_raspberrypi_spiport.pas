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
  contnrs;

{$IFDEF CPUARM}
const
  // Clock Phase
  SPI_CPHA           =     $01;
  // Clock Polarity
  SPI_CPOL           =     $02;

  // 4 possible SPI Clock Modes
  SPI_MODE_0         =     $0000 or $0000;
  SPI_MODE_1         =     $0000 or SPI_CPHA;
  SPI_MODE_2         =     SPI_CPOL or $0000;
  SPI_MODE_3         =     SPI_CPOL or SPI_CPHA;

  // Chip Select is High
  SPI_CS_HIGH        =     $0004;
  // Send LSB First
  SPI_LSB_FIRST      =     $0008;
  // Shared SI/SO
  SPI_3WIRE          =     $0010;
  // LoopBack
  SPI_LOOP           =     $0020;
  // No CS
  SPI_NO_CS          =     $0040;
  // SPI Ready
  SPI_READY          =     $0080;
  // TX Stuff?
  SPI_TX_DUAL        =     $0100;
  SPI_TX_QUAD        =     $0200;
  SPI_RX_DUAL        =     $0400;
  SPI_RX_QUAD        =     $0800;

  SPI_IOC_MAGIC = ord('k');

  // 33 # define _IOW(x,y,z)    (((x)<<8)|y)
  // 36 # define _IOR(x,y,z)    (((x)<<8)|y)
  // 119 /* Read / Write of SPI mode (SPI_MODE_0..SPI_MODE_3) (limited to 8 bits) */
  // 120 #define SPI_IOC_RD_MODE                 _IOR(SPI_IOC_MAGIC, 1, __u8)
  // 121 #define SPI_IOC_WR_MODE                 _IOW(SPI_IOC_MAGIC, 1, __u8)
  SPI_IOC_RD_MODE = (SPI_IOC_MAGIC shl 8) or 1 or $80010000;
  SPI_IOC_WR_MODE = (SPI_IOC_MAGIC shl 8) or 1 or $40010000;

  // Read / Write SPI bit justification */
  SPI_IOC_RD_LSB_FIRST = (SPI_IOC_MAGIC shl 8) or 2 or $40010000;
  SPI_IOC_WR_LSB_FIRST = (SPI_IOC_MAGIC shl 8) or 2 or $80010000;

  // Read / Write SPI device word length (1..N) */
  SPI_IOC_RD_BITS_PER_WORD = (SPI_IOC_MAGIC shl 8) or 3 or $80010000;
  SPI_IOC_WR_BITS_PER_WORD = (SPI_IOC_MAGIC shl 8) or 3 or $40010000;

  // Read / Write SPI device default max speed hz */
  SPI_IOC_RD_MAX_SPEED_HZ = (SPI_IOC_MAGIC shl 8) or 4 or $80040000;
  SPI_IOC_WR_MAX_SPEED_HZ = (SPI_IOC_MAGIC shl 8) or 4 or $40040000;

const
  // How many message records are sent in IOCtl
  SPI_IOC_MSGSIZE_1 = (SPI_IOC_MAGIC shl 8) or 0 or $40200000;
  SPI_IOC_MSGSIZE_2 = (SPI_IOC_MAGIC shl 8) or 0 or $40400000;
  SPI_IOC_MSGSIZE_3 = (SPI_IOC_MAGIC shl 8) or 0 or $40600000;
  SPI_IOC_MSGSIZE_4 = (SPI_IOC_MAGIC shl 8) or 0 or $40800000;

type
  TPiSpiBuffer = array[0..4095] of Byte;
  PPiSpiBuffer = ^TPiSpiBuffer;

type
  _spi_ioc_transfer = record
    tx_buffPtr    : Int64;       // Pointer a buffer
    rx_buffPtr    : Int64;       // Pointer to a buffer
    len           : LongWord;   // how many to send
    speed_hz      : LongWord;  // speed
    delay_usecs   : Word;   // how long to delay?
    bits_per_word : Byte; // Bits per word (8,16)
    cs_change     : Byte;     // CS Change?
    pad           : LongWord;       // Padding?
  end;

  TSpiIocTransfer = _spi_ioc_transfer;

  { TRaspberryPiSpi }

  TRaspberryPiSpi = class
  private
    FBits: TPiSpiBits;
    FHandle: Integer;
    FIOCtlResult: Integer;
    FMode: TPiSpiMode;
    FSpeed: TPiSpiSpeed;
    function GetLastErrorDesc: string;
    procedure SetBits(AValue: TPiSpiBits);
    procedure SetMode(AValue: TPiSpiMode);
    procedure SetSpeed(AValue: TPiSpiSpeed);
  public
    property Handle: Integer read FHandle;
    property IOCtlResult: Integer read FIOCtlResult write FIOCtlResult;
    property Mode: TPiSpiMode read FMode write SetMode;
    property Bits: TPiSpiBits read FBits write SetBits;
    property Speed: TPiSpiSpeed read FSpeed write SetSpeed;
    property LastErrorDesc: string read GetLastErrorDesc;

    constructor Create;
    destructor Destroy; override;
    function OpenSpi(SpiDevicePath: string): Boolean;
    procedure CloseSpi;
    function Transfer(TxBuffer: PPiSpiBuffer; RxBuffer: PPiSpiBuffer; Count: Integer): Boolean;
  end;

type
  TLccRaspberryPiSpiPortThread = class;
  TLccRaspberryPiSpiPort = class;

  TLccRaspberryPiSpiPortRec = record
    Thread: TLccRaspberryPiSpiPortThread;         // Thread owing the Record
    Port: LccString;                    // Spiport
    Mode: TPiSpiMode;
    Bits: TPiSpiBits;
    Speed: TPiSpiSpeed;
    ConnectionState: TConnectionState;   // Current State of the connection
    MessageStr: LccString;               // Contains the string for the resuting message from the thread
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


  function GetRaspberryPiSpiPortNames: string;
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
function GetRaspberryPiSpiPortNames: string;
var
  Index: Integer;
  Data: string;
  TmpPorts: String;
  sr : TSearchRec;
begin
  try
    TmpPorts := '';
    if FindFirst('/dev/spidev*', LongInt($FFFFFFFF), sr) = 0 then
    begin
      repeat
        if (sr.Attr and $FFFFFFFF) = Sr.Attr then
        begin
          TmpPorts := TmpPorts + #13 + ExtractFileName(sr.Name);
        end;
      until FindNext(sr) <> 0;
    end;
    FindClose(sr);
  finally
    Result:=TmpPorts;
  end;
end;

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
        PiSpiPortThread.Scheduler.OutgoingMsg(AMessage);
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

function TLccRaspberryPiSpiPortThread.GetIsTerminated: Boolean;
begin
   Result := Terminated;
end;

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
      if MsgAssembler.IncomingMessageGridConnect(FEthernetRec.MessageStr, LocalMessage) = imgcr_True then // In goes a raw message
        Owner.NodeManager.ProcessMessage(LocalMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages
  end
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
    MessageStr := AMessage.ConvertToGridConnectStr('');
    OutgoingGridConnect.Add(MessageStr);
    if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
      PrintToSynEdit( 'S PiSpi: ' + MessageStr,
                      Owner.LoggingFrame.SynEdit,
                      Owner.LoggingFrame.ActionLogPause.Checked,
                      Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                      Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
    DoSendMessage(AMessage);
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


{ TRaspberryPiSpi }

procedure TRaspberryPiSpi.SetMode(AValue: TPiSpiMode);
var
  LocalMode: Byte;
begin
  FIOCtlResult := 0;
  FMode := AValue;
  case FMode of
    psm_ClkIdleLo_DataRising  : LocalMode := SPI_MODE_0;
    psm_ClkIdleLo_DataFalling : LocalMode := SPI_MODE_1;
    psm_ClkIdleHi_DataRising  : LocalMode := SPI_MODE_2;
    psm_ClkIdleHi_DataFalling : LocalMode := SPI_MODE_3;
  end;
  if Handle > -1 then
    IOCtlResult := fpIOCtl(Handle, SPI_IOC_WR_MODE, @LocalMode);
end;

procedure TRaspberryPiSpi.SetSpeed(AValue: TPiSpiSpeed);
var
  LocalSpeed: LongWord;
begin
  FIOCtlResult := 0;
  FSpeed := AValue;
  case FSpeed of
    pss_7629Hz   : LocalSpeed := 7629;
    pss_15_2kHz  : LocalSpeed := 15200;
    pss_30_5kHz  : LocalSpeed := 30500;
    pss_61kHz    : LocalSpeed := 61000;
    pss_122kHz   : LocalSpeed := 122000;
    pss_244kHz   : LocalSpeed := 244000;
    pss_488kHz   : LocalSpeed := 488000;
    pss_976kHz   : LocalSpeed := 976000;
    pss_1_953MHz : LocalSpeed := 1953000;
    pss_3_9Mhz   : LocalSpeed := 3900000;
    pss_7_8Mhz   : LocalSpeed := 7800000;
    pss_15_6Mhz  : LocalSpeed := 15600000;
    pss_31_2Mhz  : LocalSpeed := 31200000;
    pss_62_5Mhz  : LocalSpeed := 62500000;
    pss_125Mhz   : LocalSpeed := 125000000;
  end;
  if Handle > -1 then
    IOCtlResult := fpIOCtl(Handle, SPI_IOC_WR_MAX_SPEED_HZ, @LocalSpeed);
end;

procedure TRaspberryPiSpi.SetBits(AValue: TPiSpiBits);
var
  LocalBits: Byte;
begin
  FIOCtlResult := 0;
  FBits := AValue;
  case FBits of
    psb_8 : LocalBits := 8;
    psb_16 : LocalBits := 16;
  end;
  if Handle > -1 then
    IOCtlResult := fpIOCtl(Handle, SPI_IOC_WR_BITS_PER_WORD, @LocalBits);
end;

function TRaspberryPiSpi.GetLastErrorDesc: string;
begin
  Result := 'Unimplmeneted'
end;

constructor TRaspberryPiSpi.Create;
begin
  FHandle := -1;
  FIOCtlResult := 0;
end;

destructor TRaspberryPiSpi.Destroy;
begin
  if Handle > -1 then
    CloseSpi;
  inherited Destroy;
end;

function TRaspberryPiSpi.OpenSpi(SpiDevicePath: string): Boolean;
begin
  Result := False;
  FHandle := fpopen(SpiDevicePath, O_RDWR);
  if Handle > -1 then
  begin
    Mode := FMode;
    if IOCtlResult > -1 then
    begin
      Bits := FBits;
      if IOCtlResult > -1 then
      begin
        Speed := FSpeed;
        Result := IOCtlResult > -1;
      end;
    end;
  end;
end;

procedure TRaspberryPiSpi.CloseSpi;
begin
  if FHandle > - 1 then
    fpclose(FHandle);
  FHandle := -1
end;

function TRaspberryPiSpi.Transfer(TxBuffer: PPiSpiBuffer; RxBuffer: PPiSpiBuffer; Count: Integer): Boolean;
var
  LocalBuffer: TSpiIocTransfer;
begin
  Result := False;
  if Handle > -1 then
  begin
    case FBits of
      psb_8 : LocalBuffer.bits_per_word := 8;
      psb_16 : LocalBuffer.bits_per_word := 16;
    end;
    case FSpeed of
      pss_7629Hz   : LocalBuffer.speed_hz := 7629;
      pss_15_2kHz  : LocalBuffer.speed_hz := 15200;
      pss_30_5kHz  : LocalBuffer.speed_hz := 30500;
      pss_61kHz    : LocalBuffer.speed_hz := 61000;
      pss_122kHz   : LocalBuffer.speed_hz := 122000;
      pss_244kHz   : LocalBuffer.speed_hz := 244000;
      pss_488kHz   : LocalBuffer.speed_hz := 488000;
      pss_976kHz   : LocalBuffer.speed_hz := 976000;
      pss_1_953MHz : LocalBuffer.speed_hz := 1953000;
      pss_3_9Mhz   : LocalBuffer.speed_hz := 3900000;
      pss_7_8Mhz   : LocalBuffer.speed_hz := 7800000;
      pss_15_6Mhz  : LocalBuffer.speed_hz := 15600000;
      pss_31_2Mhz  : LocalBuffer.speed_hz := 31200000;
      pss_62_5Mhz  : LocalBuffer.speed_hz := 62500000;
      pss_125Mhz   : LocalBuffer.speed_hz := 125000000;
    end;
    LocalBuffer.cs_change := 0;
    LocalBuffer.delay_usecs := 0;
    LocalBuffer.len := Count;
    LocalBuffer.rx_buffPtr := LongWord(RxBuffer);
    LocalBuffer.tx_buffPtr := LongWord(TxBuffer);
    LocalBuffer.Pad := 0;
    IOCtlResult := fpIOCtl(handle, SPI_IOC_MSGSIZE_1, @LocalBuffer);
    if IOCtlResult > -1 then
      Result := True
  end;
end;

initialization
  RegisterClass(TLccRaspberryPiSpiPort);

finalization

{$ENDIF}

end.

