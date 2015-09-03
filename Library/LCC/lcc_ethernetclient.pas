unit lcc_ethernetclient;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE LOGGING}
{$ENDIF}

interface

uses
  Classes, SysUtils, contnrs,
  {$IFDEF FPC}
  LResources, Forms, Controls, Graphics, Dialogs,
  {$ELSE}
  FMX.Forms, Types, System.Generics.Collections,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging, lcc_detailed_logging,
  {$ENDIF}
  lcc_gridconnect, blcksock, synsock, lcc_threaded_stringlist,
  lcc_can_message_assembler_disassembler, lcc_message_scheduler,
  lcc_nodemanager, lcc_messages, lcc_defines, lcc_threadedcirculararray,
  lcc_tcp_protocol, lcc_utilities, lcc_app_common_settings, synabyte,
  lcc_common_classes;

type
  TLccEthernetClient = class;   // Forward


  TLccEthernetRec = record
    Thread: TLccConnectionThread;    // Thread owing the Record
    AutoResolveIP: Boolean;          // Tries to autoresolve the local unique netword IP of the machine
    ClientIP,
    ListenerIP: LccString;
    ClientPort,
    ListenerPort: Word;
    HeartbeatRate: Integer;
    ConnectionState: TConnectionState; // Current State of the connection
    MessageStr: LccString;             // Contains the string for the resuting message from the thread
    MessageArray: TDynamicByteArray;   // Contains the TCP Protocol message bytes of not using GridConnect
    ErrorCode: Integer;
    LccMessage: TLccMessage;
    SuppressNotification: Boolean;    // True to stop any Syncronoize() call being called
  end;


  TOnEthernetRecFunc = procedure(Sender: TObject; EthernetRec: TLccEthernetRec) of object;
  TOnEthernetReceiveFunc = procedure(Sender: TObject; EthernetRec: TLccEthernetRec) of object;


  { TLccEthernetClientThread }

  TLccEthernetClientThread =  class(TLccConnectionThread)
    private
      FEthernetRec: TLccEthernetRec;
      FMsgAssembler: TLccMessageAssembler;
      FMsgDisAssembler: TLccMessageDisAssembler;
      FOnErrorMessage: TOnEthernetRecFunc;
      FOnConnectionStateChange: TOnEthernetRecFunc;
      FOnReceiveMessage: TOnEthernetReceiveFunc;
      FOnSchedulerAddOutgoingMessage: TOnMessageEvent;
      FOnSchedulerAddWaitingForReplyMessage: TOnMessageEvent;
      FOnSchedulerClass: TOnSchedulerClassEvent;
      FOnSchedulerRemoveOutgoingMessage: TOnMessageEvent;
      FOnSchedulerRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent;
      FOnSendMessage: TOnMessageEvent;
      FOutgoingCircularArray: TThreadedCirularArray;
      FOutgoingGridConnect: TThreadStringList;
      FOwner: TLccEthernetClient;
      FRunning: Boolean;
      FScheduler: TSchedulerBase;
      FSleepCount: Integer;
      FSocket: TTCPBlockSocket;
      FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
      FWorkerMsg: TLccMessage;
      function GetIsTerminated: Boolean;
      function GetScheduler: TSchedulerBase;
    protected
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage);

      property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
      property Socket: TTCPBlockSocket read FSocket write FSocket;
      property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
      property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
      property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
      property OnSchedulerClass: TOnSchedulerClassEvent read FOnSchedulerClass write FOnSchedulerClass;
      property OnSchedulerAddOutgoingMessage: TOnMessageEvent read FOnSchedulerAddOutgoingMessage write FOnSchedulerAddOutgoingMessage;
      property OnSchedulerRemoveOutgoingMessage: TOnMessageEvent read FOnSchedulerRemoveOutgoingMessage write FOnSchedulerRemoveOutgoingMessage;
      property OnSchedulerAddWaitingForReplyMessage: TOnMessageEvent read FOnSchedulerAddWaitingForReplyMessage write FOnSchedulerAddWaitingForReplyMessage;
      property OnSchedulerRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent read FOnSchedulerRemoveWaitingForReplyMessage write FOnSchedulerRemoveWaitingForReplyMessage;
      property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
      property OutgoingCircularArray: TThreadedCirularArray read FOutgoingCircularArray write FOutgoingCircularArray;
      property Owner: TLccEthernetClient read FOwner write FOwner;
      property Running: Boolean read FRunning write FRunning;
      property IsTerminated: Boolean read GetIsTerminated;
      property Scheduler: TSchedulerBase read GetScheduler;
      property SleepCount: Integer read FSleepCount write FSleepCount;
      property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
      property MsgAssembler: TLccMessageAssembler read FMsgAssembler write FMsgAssembler;
      property MsgDisAssembler: TLccMessageDisAssembler read FMsgDisAssembler write FMsgDisAssembler;
      property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
    public
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetClient; const AnEthernetRec: TLccEthernetRec); reintroduce;
      destructor Destroy; override;
  end;

  { TLccEthernetThreadList }

 // {$IFDEF FPC}
  TLccEthernetThreadList = class(TThreadList)      // Contains TClientSocketThread objects
 // {$ELSE}
 // TLccEthernetThreadList<T> = class(TThreadList<T>)
 // {$ENDIF}
  public
    destructor Destroy; override;
    procedure CloseEthernetPorts;
    procedure CloseEthernetPort(EthernetThread: TLccEthernetClientThread);
  end;

  { TLccEthernetClient }

  TLccEthernetClient = class(TLccHardwareConnectionManager)
  private
 //   {$IFDEF FPC}
    FEthernetThreads: TLccEthernetThreadList;
    FGridConnect: Boolean;
  //  {$ELSE}
  //  FEthernetThreads: TThreadList<TLccEthernetClientThread>;
  //  {$ENDIF}
    FHub: Boolean;
    FLccSettings: TLccSettings;
    {$IFDEF LOGGING}FLoggingFrame: TFrameLccLogging;{$ENDIF}
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSchedulerAddOutgoingMessage: TOnMessageEvent;
    FOnSchedulerAddWaitingForReplyMessage: TOnMessageEvent;
    FOnSchedulerClass: TOnSchedulerClassEvent;
    FOnSchedulerRemoveOutgoingMessage: TOnMessageEvent;
    FOnSchedulerRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent;
    FOnSendMessage: TOnMessageEvent;
    FSchedulerPipelineSize: Integer;
    FSleepCount: Integer;
 //   procedure SetOnSchedulerRemoveOutgoingMessage(AValue: TOnMessageEvent);
 //   procedure SetOnSchedulerAddOutgoingMessage(AValue: TOnMessageEvent);
 //   procedure SetOnSchedulerAddWaitingForReplyMessage(AValue: TOnMessageEvent);
 //   procedure SetOnSchedulerRemoveWaitingForReplyMessage(AValue: TOnMessageRemoveWaitingForReplyEvent);
 //   procedure SetOnSendMessage(AValue: TOnMessageEvent);
    procedure SetSchedulerPipelineSize(AValue: Integer);
    procedure SetSleepCount(AValue: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateThreadEvents(EthernetThread: TLccEthernetClientThread);
    procedure UpdateThreadsEvents;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetClientThread;
    function OpenConnectionWithLccSettings: TLccEthernetClientThread;
    procedure ClearSchedulerQueues;
    procedure CloseConnection( EthernetThread: TLccEthernetClientThread);
    procedure FillWaitingMessageList(WaitingMessageList: TObjectList); override;
    procedure SendMessage(AMessage: TLccMessage); override;
  //  {$IFDEF FPC}
    property EthernetThreads: TLccEthernetThreadList read FEthernetThreads write FEthernetThreads;
  //  {$ELSE}
 //   property EthernetThreads: TThreadList<TLccEthernetClientThread> read FEthernetThreads write FEthernetThreads;
 //   {$ENDIF}
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector
  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
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
  {$IFDEF FPC}
  {$I TLccEthernetClient.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccEthernetClient]);
end;

{ TLccEthernetThreadList }

destructor TLccEthernetThreadList(*{$IFNDEF FPC}<T>{$ENDIF}*).Destroy;
begin
  CloseEthernetPorts;
  inherited Destroy;
end;

procedure TLccEthernetThreadList(*{$IFNDEF FPC}<T>{$ENDIF}*).CloseEthernetPorts;
var
 // {$IFDEF FPC}
  List: TList;
 // {$ELSE}
 // List: TList<TLccEthernetClientThread>;
 // {$ENDIF}
  Len: Integer;
  Thread: TLccEthernetClientThread;
begin
  List := LockList;
  try
    Len := List.Count
  finally
    UnlockList
  end;

  while Len > 0 do
  begin
    List := LockList;
    try
  //    {$IFDEF FPC}
      Thread := TLccEthernetClientThread( List[0]);
  //    {$ELSE}
  //    Thread := List[0];
  //    {$ENDIF}
      List.Delete(0);
    finally
      UnlockList
    end;
    CloseEthernetPort(Thread);

    List := LockList;
    try
      Len := List.Count
    finally
      UnlockList
    end;
  end;
end;

procedure TLccEthernetThreadList(*{$IFNDEF FPC}<T>{$ENDIF}*).CloseEthernetPort( EthernetThread: TLccEthernetClientThread);
var
  TimeCount: Cardinal;
begin
  TimeCount := 0;
  EthernetThread.Terminate;
//  TimeCount := GetTickCount;            DON"T LINK OCLB_UTILITES, it causes issues with linking to different packages
  while (EthernetThread.Running) do
  begin
    Application.ProcessMessages;
    Inc(TimeCount);
    if TimeCount = 10 then
    begin
       if Assigned(EthernetThread.Socket) then
         EthernetThread.Socket.CloseSocket
       else
         Break // Something went really wrong
    end;
  end;
  FreeAndNil( EthernetThread);
end;

{ TLccEthernetClient }

procedure TLccEthernetClient.CloseConnection(EthernetThread: TLccEthernetClientThread);
//{$IFNDEF FPC}
//var
 // List: TList<TLccEthernetThreadList>;
//{$ENDIF}
begin
 // {$IFDEF FPC}
    if Assigned(EthernetThread) then
    begin
      EthernetThreads.Remove(EthernetThread);
      EthernetThreads.CloseEthernetPort(EthernetThread);
    end else
      EthernetThreads.CloseEthernetPorts;
 // {$ELSE}
 {   List := EthernetThreads.LockList;
    try
      if Assigned(EthernetThread) then
      begin
        EthernetThreads.re
        List.Remove(EthernetThread);
        List.CloseEthernetPort(EthernetThread);
      end else
        List.CloseEthernetPorts
    finally
      EthernetThreads.UnlockList
    end;   }
 // {$ENDIF}
end;

procedure TLccEthernetClient.SetSchedulerPipelineSize(AValue: Integer);
begin
  if (AValue < 1) or (AValue > 10) then
    AValue := 1;

  if AValue <> FSchedulerPipelineSize then
  begin
    FSchedulerPipelineSize := AValue;
    UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetClient.SetSleepCount(AValue: Integer);
begin
  if FSleepCount <> AValue then
  begin
    FSleepCount := AValue;
    UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetClient.UpdateThreadEvents(EthernetThread: TLccEthernetClientThread);
begin
  EthernetThread.OnSendMessage := OnSendMessage;
  EthernetThread.Scheduler.OnAddWaitingForReplyMessage := OnSchedulerAddWaitingForReplyMessage;
  EthernetThread.Scheduler.OnRemoveWaitingForReplyMessage := OnSchedulerRemoveWaitingForReplyMessage;
  EthernetThread.Scheduler.OnAddOutgoingMessage := OnSchedulerAddOutgoingMessage;
  EthernetThread.Scheduler.OnRemoveOutgoingMessage := OnSchedulerRemoveOutgoingMessage;
  EthernetThread.Scheduler.PipelineSize := FSchedulerPipelineSize;
  EthernetThread.SleepCount := SleepCount;
  EthernetThread.GridConnect := Gridconnect;
end;

procedure TLccEthernetClient.UpdateThreadsEvents;
var
  i: Integer;
  L: TList;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
      UpdateThreadEvents(TLccEthernetClientThread( L[i]));
  finally
    EthernetThreads.UnlockList;
  end;
end;

constructor TLccEthernetClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEthernetThreads := TLccEthernetThreadList.Create;
  FSchedulerPipelineSize := 1;
  FHub := False;
end;

destructor TLccEthernetClient.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  inherited Destroy;
end;

procedure TLccEthernetClient.FillWaitingMessageList(WaitingMessageList: TObjectList);
var
  i, j: Integer;
  L: TList;
  EthernetThread: TLccEthernetClientThread;
begin
  if Assigned(WaitingMessageList) then
  begin
    WaitingMessageList.Clear;
      L := EthernetThreads.LockList;
    try
      for i := 0 to L.Count - 1 do
      begin
        EthernetThread := TLccEthernetClientThread( L[i]);
        for j := 0 to EthernetThread.Scheduler.MessagesWaitingForReplyList.Count - 1 do
          WaitingMessageList.Add( (EthernetThread.Scheduler.MessagesWaitingForReplyList[j] as TLccMessage).Clone);
      end;
    finally
      EthernetThreads.UnlockList;
    end;
  end;
end;

function TLccEthernetClient.OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetClientThread;
begin
  Result := TLccEthernetClientThread.Create(True, Self, AnEthernetRec);
  Result.OnConnectionStateChange := OnConnectionStateChange;
  Result.OnErrorMessage := OnErrorMessage;
  Result.OnReceiveMessage := OnReceiveMessage;
  Result.OnSchedulerClass := OnSchedulerClass;
  Result.OnSendMessage := OnSendMessage;
  Result.OnSchedulerRemoveWaitingForReplyMessage := OnSchedulerRemoveWaitingForReplyMessage;
  Result.OnSchedulerAddOutgoingMessage := OnSchedulerAddOutgoingMessage;
  Result.OnSchedulerRemoveOutgoingMessage := OnSchedulerRemoveOutgoingMessage;
  Result.OnSchedulerAddWaitingForReplyMessage := OnSchedulerAddWaitingForReplyMessage;
  Result.Scheduler.PipelineSize := FSchedulerPipelineSize;
  Result.GridConnect := Gridconnect;
  EthernetThreads.Add(Result);
  Result.Suspended := False;
end;

function TLccEthernetClient.OpenConnectionWithLccSettings: TLccEthernetClientThread;
var
  AnEthernetRec: TLccEthernetRec;
begin
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

procedure TLccEthernetClient.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
  L: TList;
  EthernetThread: TLccEthernetClientThread;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccEthernetClientThread( L[i]);
      if not EthernetThread.IsTerminated then
        EthernetThread.Scheduler.OutgoingMsg(AMessage);
    end;
  finally
    EthernetThreads.UnlockList;
  end;
end;

{
procedure TLccEthernetClient.SetOnSchedulerRemoveOutgoingMessage(AValue: TOnMessageEvent);
begin
  if @FOnSchedulerRemoveOutgoingMessage <> @AValue then
  begin
    FOnSchedulerRemoveOutgoingMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetClient.SetOnSchedulerAddOutgoingMessage(AValue: TOnMessageEvent);
begin
  if @FOnSchedulerAddOutgoingMessage <> @AValue then
  begin
    FOnSchedulerAddOutgoingMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetClient.SetOnSchedulerAddWaitingForReplyMessage(AValue: TOnMessageEvent);
begin
  if @FOnSchedulerAddWaitingForReplyMessage <> @AValue then
  begin
    FOnSchedulerAddWaitingForReplyMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetClient.SetOnSchedulerRemoveWaitingForReplyMessage(AValue: TOnMessageRemoveWaitingForReplyEvent);
begin
  if @FOnSchedulerRemoveWaitingForReplyMessage <> @AValue then
  begin
    FOnSchedulerRemoveWaitingForReplyMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetClient.SetOnSendMessage(AValue: TOnMessageEvent);
begin
  if @FOnSendMessage <> @AValue then
  begin
    FOnSendMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;
}

procedure TLccEthernetClient.ClearSchedulerQueues;
var
  i: Integer;
  L: TList;
  EthernetThread: TLccEthernetClientThread;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccEthernetClientThread( L[i]);
      begin
        EthernetThread.Scheduler.ClearPermenentErrorQueue;
        EthernetThread.Scheduler.ClearQueue;
        EthernetThread.Scheduler.ClearSentQueue;
      end;
    end;
  finally
    EthernetThreads.UnlockList;
  end;
end;

{ TLccEthernetClientThread }

procedure TLccEthernetClientThread.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FEthernetRec.ConnectionState := NewConnectionState;
    if Assigned(Socket) then
    begin
      Socket.GetSinLocal;
      FEthernetRec.ClientIP := Socket.GetLocalSinIP;
      FEthernetRec.ClientPort := Socket.GetLocalSinPort;
    end;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
  end;

  procedure HandleErrorAndDisconnect;
  begin
    Owner.EthernetThreads.Remove(Self);
    FEthernetRec.ErrorCode := Socket.LastError;
    FEthernetRec.MessageStr := Socket.LastErrorDesc;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
    SendConnectionNotification(ccsClientDisconnected);
    Terminate;
  end;

var
  TxStr: LccString;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  GridConnectStr: TGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList: TStringList;
  RetryCount: Integer;
  Peer: TVarSin;
  DynamicByteArray: TDynamicByteArray;
  SynaBytes: TSynaBytes;
  RcvByte: Byte;
  LocalSleepCount: Integer;
  LocalName: string;
  IpStrings: TStringList;
  {$IFNDEF WINDOWS}
  Ip: array[0..15] of char;
  {$ENDIF}
begin
  FRunning := True;

  SendConnectionNotification(ccsClientConnecting);

  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  if Gridconnect then
    Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    GridConnectHelper.Free;
    FRunning := False
  end else
  begin
    RetryCount := 0;

    if FEthernetRec.AutoResolveIP then
    begin
      {$IFDEF WINDOWS}
      LocalName := Socket.LocalName;
      IpStrings := TStringList.Create;
      try
         Socket.ResolveNameToIP(LocalName, IpStrings) ;  // '192.168.0.8';
         for i := 0 to IpStrings.Count - 1 do
           FEthernetRec.ClientIP := IpStrings[i];
      finally
        IpStrings.Free;
      end;
      {$ELSE}
      ResolveUnixIp(Ip, 16);
      FEthernetRec.ClientIP := Ip;
      {$ENDIF}
    end;


    Socket.Connect(EthernetRec.ListenerIP, IntToStr(EthernetRec.ListenerPort));
    while (Socket.LastError = WSAEINPROGRESS) or (Socket.LastError = WSAEALREADY) and (RetryCount < 40) do   {20 Second Wait}
    begin
      Socket.ResetLastError;
      if GetPeerName(Socket.Socket, Peer) = 0 then
        Break;
      Inc(RetryCount);
      Sleep(500);
    end;

    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      GridConnectHelper.Free;
      FRunning := False
    end else
    begin
      SendConnectionNotification(ccsClientConnected);
      try
        LocalSleepCount := 0;
        try
          while not IsTerminated and (FEthernetRec.ConnectionState = ccsClientConnected) do
          begin

            if Gridconnect then
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
                  StringToGridConnectBuffer(TxStr, GridConnectStr);
                  for i := 0 to Length(TxStr) - 1 do
                    Socket.SendByte(GridConnectStr[i]);
                  Socket.SendByte(Ord(#10));
                  if Socket.LastError <> 0 then
                    HandleErrorAndDisconnect;
                end;
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              RcvByte := Socket.RecvByte(1);
              case Socket.LastError of
                0 :
                  begin
                    GridConnectStrPtr := nil;
                    if GridConnectHelper.GridConnect_DecodeMachine(RcvByte, GridConnectStrPtr) then
                    begin
                      FEthernetRec.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                      FEthernetRec.LccMessage.LoadByGridConnectStr(FEthernetRec.MessageStr);
                      Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                    end;
                  end;
                WSAETIMEDOUT :
                  begin
                  end;
              else
                HandleErrorAndDisconnect
              end;
            end else
            begin

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
                  Socket.SendBuffer(@DynamicByteArray[0], Length(DynamicByteArray));
                  if Socket.LastError <> 0 then
                    HandleErrorAndDisconnect;
                  DynamicByteArray := nil;
                end;
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

          //    SynaBytes := Socket.RecvPacket(1);
              RcvByte := Socket.RecvByte(1);
              case Socket.LastError of
                0 :
                  begin
                    DynamicByteArray := nil;
                    if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, FEthernetRec.MessageArray) then
                      Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);

                    // I get a random "0" at the beginning of the message in Lazarus OSX at least
             (*       DynamicByteArray := nil;
                    {$IFDEF UNICODE}
                    for i := 0 to SynaBytes.Length - 1 do
                    {$ELSE}
                    for i := 0 to Length(SynaBytes) - 1 do
                    {$ENDIF}
                    begin
                      {$IFDEF UNICODE}
                      if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(SynaBytes.Bytes[i], FEthernetRec.MessageArray) then
                      {$ELSE}
                      if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(Ord(SynaBytes[i]), FEthernetRec.MessageArray) then
                      {$ENDIF}
                        Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                    end            *)
                  end;
                WSAETIMEDOUT :
                  begin

                  end;
              else
                HandleErrorAndDisconnect
              end;
            end;
          end;
        finally
          SendConnectionNotification(ccsClientDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        SendConnectionNotification(ccsClientDisconnected);
        Owner.EthernetThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

function TLccEthernetClientThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

function TLccEthernetClientThread.GetScheduler: TSchedulerBase;
var
  SchedulerClass: TSchedulerBaseClass;
begin
  if not Assigned(FScheduler) then
  begin
    SchedulerClass := TSchedulerSimplePipeline;
    if Assigned(OnSchedulerClass) then
      OnSchedulerClass(Owner, SchedulerClass);
    FScheduler := SchedulerClass.Create(Owner, {$IFDEF FPC}@{$ENDIF}SendMessage);
    FScheduler.OnAddOutgoingMessage := OnSchedulerAddOutgoingMessage;
    FScheduler.OnRemoveOutgoingMessage := OnSchedulerRemoveOutgoingMessage;
    FScheduler.OnAddWaitingForReplyMessage := OnSchedulerAddWaitingForReplyMessage;
    FScheduler.OnRemoveWaitingForReplyMessage := OnSchedulerRemoveWaitingForReplyMessage;
    FScheduler.OwnerThread := Self;
  end;
  Result := FScheduler
end;


procedure TLccEthernetClientThread.SendMessage(AMessage: TLccMessage);
var
  MessageStr: LccString;
  ByteArray: TDynamicByteArray;
begin
  if not IsTerminated then
  begin
    if Gridconnect then
    begin
      MessageStr := AMessage.ConvertToGridConnectStr('');
      OutgoingGridConnect.Add(MessageStr);
      {$IFDEF LOGGING}
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
        PrintToSynEdit( 'S: ' + MessageStr,
                        Owner.LoggingFrame.SynEdit,
                        Owner.LoggingFrame.ActionLogPause.Checked,
                        Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                        Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
      {$ENDIF}
      DoSendMessage(AMessage);
    end else
    begin
      ByteArray := nil;
      if AMessage.ConvertToLccTcp(ByteArray) then
      begin
        OutgoingCircularArray.AddChunk(ByteArray);
        {$IFDEF LOGGING}
        if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
          PrintTCPToSynEdit( '...Sending TCP...',
                          ByteArray,
                          Owner.LoggingFrame.SynEdit,
                          Owner.LoggingFrame.ActionLogPause.Checked,
                          Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                          Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
        {$ENDIF}
        DoSendMessage(AMessage);
      end;
    end;
  end;
end;

constructor TLccEthernetClientThread.Create(CreateSuspended: Boolean; AnOwner: TLccEthernetClient; const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := Self;
  FEthernetRec.LccMessage := TLccMessage.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
  OutgoingCircularArray := TThreadedCirularArray.Create;
  FMsgAssembler := TLccMessageAssembler.Create;
  FMsgDisAssembler := TLccMessageDisAssembler.Create;
  FWorkerMsg := TLccMessage.Create;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
end;

destructor TLccEthernetClientThread.Destroy;
begin
  FreeAndNil(FOutgoingGridConnect);
  FreeAndNil(FEthernetRec.LccMessage);
  FreeAndNil(FMsgAssembler);
  FreeandNil(FMsgDisAssembler);
  FreeAndNIl(FWorkerMsg);
  FreeAndNil(FOutgoingCircularArray);
  FreeAndNil(FTcpDecodeStateMachine);
  inherited Destroy;
end;

procedure TLccEthernetClientThread.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccEthernetClientThread.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccEthernetClientThread.DoReceiveMessage;
var
  LocalMessage: TLccMessage;
begin
  if not IsTerminated then
  begin

    if Gridconnect then
    begin
      {$IFDEF LOGGING}
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
        PrintToSynEdit( 'R EthCli : ' + EthernetRec.MessageStr,
                        Owner.LoggingFrame.SynEdit,
                        Owner.LoggingFrame.ActionLogPause.Checked,
                        Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                        Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
      {$ENDIF}
      // Called in the content of the main thread through Syncronize
      // Send all raw GridConnect Messages to the event
      if Assigned(OnReceiveMessage) then
        OnReceiveMessage(Self, FEthernetRec);

      LocalMessage := nil;
      if (Scheduler <> nil) and (Owner.NodeManager <> nil) then
        if Scheduler.IncomingMsgGridConnectStr(FEthernetRec.MessageStr, LocalMessage) then // In goes a raw message
          Owner.NodeManager.ProcessMessage(LocalMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages
    end else
    begin
      {$IFDEF LOGGING}
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
        PrintTCPToSynEdit( 'EthCli ...Receiving TCP...',
                          EthernetRec.MessageArray,
                          Owner.LoggingFrame.SynEdit,
                          Owner.LoggingFrame.ActionLogPause.Checked,
                          Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                          Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
      {$ENDIF}
      // Called in the content of the main thread through Syncronize
      // Send all raw GridConnect Messages to the event
      if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, FEthernetRec);

      LocalMessage := nil;
      if (Scheduler <> nil) and (Owner.NodeManager <> nil) then
        if Scheduler.IncomingMsgEthernet(FEthernetRec.MessageArray, LocalMessage) then // In goes a raw message
          Owner.NodeManager.ProcessMessage(LocalMessage);  // What comes out is
    end
  end
end;

procedure TLccEthernetClientThread.DoSendMessage(AMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, AMessage);
end;

initialization
  RegisterClass(TLccEthernetClient);

finalization

end.

