unit lcc_comport;

{$IFDEF FPC}
  {$DEFINE LOGGING}
{$ENDIF}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  LResources, Forms, Controls, Graphics, Dialogs,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging, lcc_detailed_logging,
  {$ENDIF}
  lcc_gridconnect, synaser, lcc_threaded_stringlist, lcc_message_scheduler,
  lcc_nodemanager, lcc_messages, lcc_defines, lcc_utilities, lcc_app_common_settings,
  lcc_common_classes, file_utilities;

type
  TLccComPortThread = class;             // Forward
  TLccComPort = class;

  TLccComPortRec = record
    Thread: TLccComPortThread;           // Thread owing the Record
    ComPort: LccString;                  // Comport
    Baud: Integer;                       // Define connection speed. Baud rate can be from 50 to 4000000 bits per second. (it depends on your hardware!))
    Bits: Integer;                       // Number of bits in communication.
    Parity: Char;                        // Define communication parity (N - None, O - Odd, E - Even, M - Mark or S - Space)
    StopBits: Integer;                   // Use constants SB1, SB1andHalf, SB2
    SoftwareHandshake: Boolean;          // Enable XON/XOFF handshake.
    HardwareHandShake: Boolean;          // Enable CTS/RTS handshake
    ConnectionState: TConnectionState;   // Current State of the connection
    MessageStr: LccString;               // Contains the string for the resuting message from the thread
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
      FOnSchedulerClass: TOnSchedulerClassEvent;
      FOnSchedulerRemoveOutgoingMessage: TOnMessageEvent;
      FOnSchedulerAddOutgoingMessage: TOnMessageEvent;
      FOnSchedulerAddWaitingForReplyMessage: TOnMessageEvent;
      FOnSchedulerRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent;
      FOnSendMessage: TOnMessageEvent;
      FOutgoingGridConnect: TThreadStringList;
      FOwner: TLccComPort;
      FRunning: Boolean;
      FScheduler: TSchedulerBase;
      FSerial: TBlockSerial;                                                      // Serial object
      FSleepCount: Integer;
      function GetIsTerminated: Boolean;
      function GetScheduler: TSchedulerBase;
    protected
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage);

      property ComPortRec: TLccComPortRec read FComPortRec write FComPortRec;
      property Serial: TBlockSerial read FSerial write FSerial;
      property OnConnectionStateChange: TOnComChangeFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnErrorMessage: TOnComChangeFunc read FOnComErrorMessage write FOnComErrorMessage;
      property OnReceiveMessage: TOnComReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
      property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
      property OnSchedulerClass: TOnSchedulerClassEvent read FOnSchedulerClass write FOnSchedulerClass;
      property OnSchedulerAddOutgoingMessage: TOnMessageEvent read FOnSchedulerAddOutgoingMessage write FOnSchedulerAddOutgoingMessage;
      property OnSchedulerRemoveOutgoingMessage: TOnMessageEvent read FOnSchedulerRemoveOutgoingMessage write FOnSchedulerRemoveOutgoingMessage;
      property OnSchedulerAddWaitingForReplyMessage: TOnMessageEvent read FOnSchedulerAddWaitingForReplyMessage write FOnSchedulerAddWaitingForReplyMessage;
      property OnSchedulerRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent read FOnSchedulerRemoveWaitingForReplyMessage write FOnSchedulerRemoveWaitingForReplyMessage;
      property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
      property Owner: TLccComPort read FOwner write FOwner;
      property Running: Boolean read FRunning write FRunning;
      property IsTerminated: Boolean read GetIsTerminated;
      property Scheduler: TSchedulerBase read GetScheduler;
      property SleepCount: Integer read FSleepCount write FSleepCount;
    public
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
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FLoggingFrame: TFrameLccLogging;
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnComChangeFunc;
    FOnConnectionStateChange: TOnComChangeFunc;
    FOnReceiveMessage: TOnComReceiveFunc;
    FOnSchedulerClass: TOnSchedulerClassEvent;
    FOnSchedulerRemoveOutgoingMessage: TOnMessageEvent;
    FOnSchedulerAddOutgoingMessage: TOnMessageEvent;
    FOnSchedulerAddWaitingForReplyMessage: TOnMessageEvent;
    FOnSchedulerRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent;
    FOnSendMessage: TOnMessageEvent;
    FSchedulerPipelineSize: Integer;
    FSleepCount: Integer;
    procedure SetOnSchedulerRemoveOutgoingMessage(AValue: TOnMessageEvent);
    procedure SetOnSchedulerAddOutgoingMessage(AValue: TOnMessageEvent);
    procedure SetOnSchedulerAddWaitingForReplyMessage(AValue: TOnMessageEvent);
    procedure SetOnSchedulerRemoveWaitingForReplyMessage(AValue: TOnMessageRemoveWaitingForReplyEvent);
    procedure SetOnSendMessage(AValue: TOnMessageEvent);
    procedure SetSchedulerPipelineSize(AValue: Integer);
    procedure SetSleepCount(AValue: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateThreadEvents(ComPortThread: TLccComPortThread);
    procedure UpdateThreadsEvents;
  public
    { Public declarations }
    property ComPortThreads: TLccComPortThreadList read FComPortThreads write FComPortThreads;
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FormatComPortString(ComPort: string): string;
    function OpenComPort(const AComPortRec: TLccComPortRec): TLccComPortThread;
    function OpenComPortWithLccSettings: TLccComPortThread;
    procedure CloseComPort( ComPortThread: TLccComPortThread);
    procedure SendMessage(AMessage: TLccMessage); override;
    procedure ClearSchedulerQueues;
  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;

    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnComChangeFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnComChangeFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnComReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property OnSchedulerClass: TOnSchedulerClassEvent read FOnSchedulerClass write FOnSchedulerClass;
    property OnSchedulerAddOutgoingMessage: TOnMessageEvent read FOnSchedulerAddOutgoingMessage write FOnSchedulerAddOutgoingMessage;
    property OnSchedulerRemoveOutgoingMessage: TOnMessageEvent read FOnSchedulerRemoveOutgoingMessage write FOnSchedulerRemoveOutgoingMessage;
    property OnSchedulerAddWaitingForReplyMessage: TOnMessageEvent read FOnSchedulerAddWaitingForReplyMessage write FOnSchedulerAddWaitingForReplyMessage;
    property OnSchedulerRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent read FOnSchedulerRemoveWaitingForReplyMessage write FOnSchedulerRemoveWaitingForReplyMessage;
    property SchedulerPipelineSize: Integer read FSchedulerPipelineSize write SetSchedulerPipelineSize;
    property SleepCount: Integer read FSleepCount write SetSleepCount;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I TLccComPort.lrs}
  RegisterComponents('LCC',[TLccComPort]);
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

procedure TLccComPort.CloseComPort(ComPortThread: TLccComPortThread);
begin
  if Assigned(ComPortThread) then
  begin
    ComPortThreads.Remove(ComPortThread);
    ComPortThreads.CloseComPort(ComPortThread);
  end else
    ComPortThreads.CloseComPorts;
end;

procedure TLccComPort.SetSchedulerPipelineSize(AValue: Integer);
begin
  if (AValue < 1) or (AValue > 10) then
    AValue := 1;

  if AValue <> FSchedulerPipelineSize then
  begin
    FSchedulerPipelineSize:=AValue;
    UpdateThreadsEvents;
  end;
end;

procedure TLccComPort.SetSleepCount(AValue: Integer);
var
  i: Integer;
  L: TList;
begin
  if FSleepCount=AValue then Exit;
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
  ComPortThread.Scheduler.OnAddOutgoingMessage := OnSchedulerAddOutgoingMessage;
  ComPortThread.Scheduler.OnRemoveOutgoingMessage := OnSchedulerRemoveOutgoingMessage;
  ComPortThread.Scheduler.OnAddWaitingForReplyMessage := OnSchedulerAddWaitingForReplyMessage;
  ComPortThread.Scheduler.OnRemoveWaitingForReplyMessage := OnSchedulerRemoveWaitingForReplyMessage;
  ComPortThread.Scheduler.PipelineSize := FSchedulerPipelineSize;
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

constructor TLccComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComPortThreads := TLccComPortThreadList.Create;
  FSchedulerPipelineSize := 1;
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
  Result.OnSchedulerClass := OnSchedulerClass;
  Result.OnSendMessage := OnSendMessage;
  Result.OnSchedulerAddOutgoingMessage := OnSchedulerAddOutgoingMessage;
  Result.OnSchedulerRemoveOutgoingMessage := OnSchedulerRemoveOutgoingMessage;
  Result.OnSchedulerAddWaitingForReplyMessage := OnSchedulerAddWaitingForReplyMessage;
  Result.OnSchedulerRemoveWaitingForReplyMessage := OnSchedulerRemoveWaitingForReplyMessage;
  Result.Scheduler.PipelineSize := FSchedulerPipelineSize;
  Result.SleepCount := SleepCount;
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
    AComPortRec.ConnectionState := ccsComDisconnected;

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
        ComPortThread.Scheduler.OutgoingMsg(AMessage);
    end;
  finally
    ComPortThreads.UnlockList;
  end;
end;

procedure TLccComPort.SetOnSchedulerRemoveOutgoingMessage(AValue: TOnMessageEvent);
begin
  if FOnSchedulerRemoveOutgoingMessage <> AValue then
  begin
    FOnSchedulerRemoveOutgoingMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccComPort.SetOnSchedulerAddOutgoingMessage(AValue: TOnMessageEvent);
begin
  if FOnSchedulerAddOutgoingMessage <> AValue then
  begin
    FOnSchedulerAddOutgoingMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccComPort.SetOnSchedulerAddWaitingForReplyMessage(AValue: TOnMessageEvent);
begin
  if FOnSchedulerAddWaitingForReplyMessage <> AValue then
  begin
    FOnSchedulerAddWaitingForReplyMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccComPort.SetOnSchedulerRemoveWaitingForReplyMessage(AValue: TOnMessageRemoveWaitingForReplyEvent);
begin
  if FOnSchedulerRemoveWaitingForReplyMessage <> AValue then
  begin
    FOnSchedulerRemoveWaitingForReplyMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
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

procedure TLccComPort.ClearSchedulerQueues;
var
  i: Integer;
  L: TList;
  ComPortThread: TLccComPortThread;
begin
  L := ComPortThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      ComPortThread := TLccComPortThread( L[i]);
      begin
        ComPortThread.Scheduler.ClearPermenentErrorQueue;
        ComPortThread.Scheduler.ClearQueue;
        ComPortThread.Scheduler.ClearSentQueue;
      end;
    end;
  finally
    ComPortThreads.UnlockList;
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
    SendConnectionNotification(ccsComDisconnected);
    Terminate;
  end;

var
  TxStr, RcvStr: LccString;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList: TStringList;
begin
  FRunning := True;

  SendConnectionNotification(ccsComConnecting);
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
      SendConnectionNotification(ccsComConnected);
      try
        try
          while not IsTerminated and (FComPortRec.ConnectionState = ccsComConnected) do
          begin
            for i := 0 to SleepCount - 1 do
              ThreadSwitch;

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

            RcvStr := Serial.Recvstring(1);
            case Serial.LastError of
              0, ErrTimeout : begin end;
            else
              HandleErrorAndDisconnect
            end;
             {$IFDEF FPC}
             for i := 1 to Length(RcvStr) do
             begin
               GridConnectStrPtr := nil;

               if GridConnectHelper.GridConnect_DecodeMachine(Ord( RcvStr[i]), GridConnectStrPtr) then
               begin
                 FComPortRec.MessageStr := NullArrayToString(GridConnectStrPtr^);
                 Synchronize(@DoReceiveMessage);
               end;
             end;
             {$ELSE}
             for i := Low(RcvStr) to Length(RcvStr) do
             begin
               if GridConnectHelper.GridConnect_DecodeMachine(RcvStr[i], GridConnectStrPtr) then
               begin
                 FComPortRec.MessageStr := GridConnectStrPtr^;
                 Synchronize(@DoReceiveMessage);
               end;
             end;
             {$ENDIF}
          end;
        finally
          SendConnectionNotification(ccsComDisconnecting);

          if Serial.InstanceActive then
            Serial.CloseSocket;
          Serial.Free;
          GridConnectHelper.Free;
        end;
      finally
        SendConnectionNotification(ccsComDisconnected);
        Owner.ComPortThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

function TLccComPortThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

function TLccComPortThread.GetScheduler: TSchedulerBase;
var
  SchedulerClass: TSchedulerBaseClass;
begin
  if not Assigned(FScheduler) then
  begin
    SchedulerClass := TSchedulerSimplePipeline;
    if Assigned(OnSchedulerClass) then
      OnSchedulerClass(Owner, SchedulerClass);
    FScheduler := SchedulerClass.Create(Owner, @SendMessage);
    FScheduler.OnAddOutgoingMessage := OnSchedulerAddOutgoingMessage;
    FScheduler.OnRemoveOutgoingMessage := OnSchedulerRemoveOutgoingMessage;
    FScheduler.OnAddWaitingForReplyMessage := OnSchedulerAddWaitingForReplyMessage;
    FScheduler.OnRemoveWaitingForReplyMessage := OnSchedulerRemoveWaitingForReplyMessage;
    FScheduler.OwnerThread := Self;
  end;
  Result := FScheduler
end;


procedure TLccComPortThread.SendMessage(AMessage: TLccMessage);
var
  MessageStr: LccString;
begin
  if not IsTerminated then
  begin
    MessageStr := AMessage.ConvertToGridConnectStr('');
    OutgoingGridConnect.Add(MessageStr);
    if Assigned(Owner) and Assigned(Owner.LoggingFrame) and Owner.LoggingFrame.Visible then
      PrintToSynEdit( 'S: ' + MessageStr,
                      Owner.LoggingFrame.SynEdit,
                      Owner.LoggingFrame.ActionLogPause.Checked,
                      Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                      Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
    DoSendMessage(AMessage);
  end;
end;


constructor TLccComPortThread.Create(CreateSuspended: Boolean; AnOwner: TLccComPort; const AComPortRec: TLccComPortRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FComPortRec := AComPortRec;
  FComPortRec.Thread := Self;
  FComPortRec.LccMessage := TLccMessage.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
  GridConnect := True;
end;

destructor TLccComPortThread.Destroy;
begin
  FreeAndNil( FOutgoingGridConnect);
  FreeAndNil(FComPortRec.LccMessage);
  inherited Destroy;
end;

procedure TLccComPortThread.DoConnectionState;
begin
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
  LocalMessage: TLccMessage;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event
    if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, FComPortRec);

    if Assigned(Owner) and Assigned(Owner.LoggingFrame) and Owner.LoggingFrame.Visible then
      PrintToSynEdit( 'R: ' + ComPortRec.MessageStr,
                      Owner.LoggingFrame.SynEdit,
                      Owner.LoggingFrame.ActionLogPause.Checked,
                      Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                      Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);

    LocalMessage := nil;
    if (Scheduler <> nil) and (Owner.NodeManager <> nil) then
      if Scheduler.IncomingMsgGridConnectStr(FComPortRec.MessageStr, LocalMessage) then // In goes a raw message
        Owner.NodeManager.ProcessMessage(LocalMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages
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
