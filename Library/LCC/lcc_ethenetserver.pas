unit lcc_ethenetserver;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE LOGGING}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  LResources, Forms, Controls, Graphics, Dialogs,
  {$ELSE}
  FMX.Forms, Types,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging, lcc_detailed_logging,
  {$ENDIF}
  lcc_gridconnect, blcksock, synsock, lcc_threaded_stringlist,
  lcc_can_message_assembler_disassembler, lcc_message_scheduler,
  lcc_nodemanager, lcc_messages, lcc_ethernetclient, lcc_threadedcirculararray,
  lcc_defines, lcc_tcp_protocol, lcc_app_common_settings, lcc_utilities,
  lcc_common_classes;

type
  TLccEthernetServerThread = class;     // Forward
  TLccEthernetServer = class;

  { TLccEthernetServerThread }

  TLccEthernetServerThread =  class(TLccConnectionThread)
    private
      FEthernetRec: TLccEthernetRec;
      FMsgAssembler: TLccMessageAssembler;
      FMsgDisAssembler: TLccMessageDisAssembler;
      FOnClientDisconnect: TOnEthernetRecFunc;
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
      FOwner: TLccEthernetServer;
      FRunning: Boolean;
      FScheduler: TSchedulerBase;
      FSleepCount: Integer;
      FSocket: TTCPBlockSocket;
      FSocketHandleForListener: TSocket;
      FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
      FWorkerMsg: TLccMessage;
      function GetIsTerminated: Boolean;
      function GetScheduler: TSchedulerBase;
    protected
      procedure DoClientDisconnect;
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage);

      property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
      property Socket: TTCPBlockSocket read FSocket write FSocket;
      property SocketHandleForListener: TSocket read FSocketHandleForListener write FSocketHandleForListener;
      property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
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
      property Owner: TLccEthernetServer read FOwner write FOwner;
      property Running: Boolean read FRunning write FRunning;
      property IsTerminated: Boolean read GetIsTerminated;
      property Scheduler: TSchedulerBase read GetScheduler;
      property SleepCount: Integer read FSleepCount write FSleepCount;
      property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
      property MsgAssembler: TLccMessageAssembler read FMsgAssembler write FMsgAssembler;
      property MsgDisAssembler: TLccMessageDisAssembler read FMsgDisAssembler write FMsgDisAssembler;
      property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
    public
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec); reintroduce;
      destructor Destroy; override;
  end;

  { TLccEthernetThreadList }

  TLccEthernetThreadList = class(TThreadList)      // Contains TServerSocketThread objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure CloseEthernetPorts;
    procedure CloseEthernetPort(EthernetThread: TLccEthernetServerThread);

    property Count: Integer read GetCount;
  end;

  { TLccEthernetListener }

  TLccEthernetListener = class(TThread)
  private
    FEthernetRec: TLccEthernetRec;
    FGridConnect: Boolean;
    FOnClientDisconnect: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSchedulerAddOutgoingMessage: TOnMessageEvent;
    FOnSchedulerAddWaitingForReplyMessage: TOnMessageEvent;
    FOnSchedulerClass: TOnSchedulerClassEvent;
    FOnSchedulerRemoveOutgoingMessage: TOnMessageEvent;
    FOnSchedulerRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent;
    FOnSendMessage: TOnMessageEvent;
    FOwner: TLccEthernetServer;
    FRunning: Boolean;
    FSchedulerPipelineSize: Integer;
    FSleepCount: Integer;
    FSocket: TTCPBlockSocket;
    function GetIsTerminated: Boolean;
    procedure SetSchedulerPipelineSize(AValue: Integer);
  protected
    property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
    property Owner: TLccEthernetServer read FOwner write FOwner;
    property Running: Boolean read FRunning write FRunning;
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property IsTerminated: Boolean read GetIsTerminated;

    function CreateServerThread(ASocketHandle: TSocket): TLccEthernetServerThread;
    procedure DoConnectionState;
    procedure DoErrorMessage;
    procedure DoReceiveMessage;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec); reintroduce;
    destructor Destroy; override;

    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
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
    property SleepCount: Integer read FSleepCount write FSleepCount;
  end;

  { TLccEthernetServer }

  TLccEthernetServer = class(TLccHardwareConnectionManager)
  private
    FEthernetThreads: TLccEthernetThreadList;
    FGridConnect: Boolean;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FListenerThread: TLccEthernetListener;
    {$IFDEF LOGGING}FLoggingFrame: TFrameLccLogging;{$ENDIF}
    FNodeManager: TLccNodeManager;
    FOnClientDisconnect: TOnEthernetRecFunc;
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
    procedure SetGridConnect(AValue: Boolean);
    procedure SetSchedulerPipelineSize(AValue: Integer);
    procedure SetSleepCount(AValue: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateThreadEvents(EthernetThread: TLccEthernetServerThread);
    procedure UpdateThreadsEvents;
    procedure UpdateListenerEvents(AListenerThread: TLccEthernetListener; Suspend: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenEthernetConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
    function OpenEthernetConnectionWithLccSettings: TLccEthernetListener;
    procedure CloseEthernetConnection( EthernetThread: TLccEthernetServerThread);
    procedure SendMessage(AMessage: TLccMessage);  override;
    procedure ClearSchedulerQueues;

    property EthernetThreads: TLccEthernetThreadList read FEthernetThreads write FEthernetThreads;
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector
    property ListenerThread: TLccEthernetListener read FListenerThread write FListenerThread;

  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property Gridconnect: Boolean read FGridConnect write SetGridConnect;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
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
  {$I TLccEthernetServer.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccEthernetServer]);
end;

{ TLccEthernetListener }

constructor TLccEthernetListener.Create(CreateSuspended: Boolean;
  AnOwner: TLccEthernetServer;
  const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := nil;
  FEthernetRec.LccMessage := TLccMessage.Create;
//  FOutgoingGridConnect := TThreadStringList.Create;
 // FMsgAssembler := TLccMessageAssembler.Create;
//  FMsgDisAssembler := TLccMessageDisAssembler.Create;
 // FWorkerMsg := TLccMessage.Create;
end;

function TLccEthernetListener.CreateServerThread(ASocketHandle: TSocket): TLccEthernetServerThread;
begin
  Result := TLccEthernetServerThread.Create(True, Owner, FEthernetRec);
  Result.SocketHandleForListener := ASocketHandle;    // Back create the sockets with this handle
  Result.OnClientDisconnect := OnClientDisconnect;
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
  Result.SleepCount := FSleepCount;
  Result.GridConnect := FGridConnect;
  Result.Start;
end;

destructor TLccEthernetListener.Destroy;
begin
//  FreeAndNil( FOutgoingGridConnect);
  FreeAndNil(FEthernetRec.LccMessage);
 // FreeAndNil(FMsgAssembler);
 // FreeandNil(FMsgDisAssembler);
//  FreeAndNIl(FWorkerMsg);
  inherited Destroy;
end;

procedure TLccEthernetListener.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccEthernetListener.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccEthernetListener.DoReceiveMessage;
//var
//  LocalMessage: TLccMessage;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event
    if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, FEthernetRec);

 //   LocalMessage := nil;
 //   if (Scheduler <> nil) and (Owner.NodeManager <> nil) then
 //     if Scheduler.IncomingMsgGridConnectStr(FEthernetRec.MessageStr, LocalMessage) then // In goes a raw message
  //      Owner.NodeManager.ProcessMessage(LocalMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages
  end
end;

procedure TLccEthernetListener.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FEthernetRec.ConnectionState := NewConnectionState;
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
    SendConnectionNotification(ccsListenerDisconnected);
    Terminate
  end;

var
  NewLink: TLccEthernetServerThread;

begin
  FRunning := True;

  SendConnectionNotification(ccsListenerConnecting);
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  SendConnectionNotification(ccsListenerConnecting);
  {IpStrings := TStringList.Create;
  try
     Socket.ResolveNameToIP(Socket.LocalName, IpStrings) ;  // '192.168.0.8';
     for i := 0 to IpStrings.Count - 1 do
       FEthernetRec.ListenerIP := IpStrings[i];
  finally
    IpStrings.Free;
  end;   }
  Socket.Bind(EthernetRec.ListenerIP, IntToStr(EthernetRec.ListenerPort));
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    FRunning := False
  end else
  begin
    Socket.Listen;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      FRunning := False
    end else
    begin
      SendConnectionNotification(ccsListenerConnected);
      try
        try
          while not Terminated and (FEthernetRec.ConnectionState = ccsListenerConnected) do
          begin
            if Socket.CanRead(1000) then
            begin
              if not Terminated and (Socket.LastError <> WSAETIMEDOUT) then
              begin
                if Socket.LastError = 0 then
                begin
                  NewLink := CreateServerThread(Socket.Accept);
                  if Assigned(NewLink) then
                    Owner.EthernetThreads.Add(NewLink);
                end else
                  Terminate;
              end
            end
          end;
        finally
          SendConnectionNotification(ccsListenerDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
        end;
      finally
        SendConnectionNotification(ccsListenerDisconnected);
        FRunning := False;
      end;
    end;
  end;
end;

function TLccEthernetListener.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TLccEthernetListener.SetSchedulerPipelineSize(AValue: Integer);
begin
  if FSchedulerPipelineSize=AValue then Exit;
  FSchedulerPipelineSize:=AValue;
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
  Thread: TLccEthernetServerThread;
begin
  while Count > 0 do
  begin
    L := LockList;
    try
      Thread := TLccEthernetServerThread( L[0]);
      L.Delete(0);
    finally
      UnlockList;
    end;
    CloseEthernetPort(Thread);
  end;
end;

procedure TLccEthernetThreadList.CloseEthernetPort( EthernetThread: TLccEthernetServerThread);
//var
//  TimeCount: Cardinal;
begin
  EthernetThread.Terminate;
//  TimeCount := GetTickCount;            DON"T LINK OCLB_UTILITES, it causes issues with linking to different packages
  while (EthernetThread.Running) do
  begin
 //   if (GetTickCount - TimeCount < 5000) then
      Application.ProcessMessages
 //   else begin
  //    KillThread(EthernetThread.Handle);
  //    EthernetThread.Running := False;
  //  end;
  end;
  FreeAndNil( EthernetThread);
end;

{ TLccEthernetServer }

procedure TLccEthernetServer.CloseEthernetConnection(EthernetThread: TLccEthernetServerThread);
begin
  if Assigned(EthernetThread) then
  begin
    EthernetThreads.Remove(EthernetThread);
    EthernetThreads.CloseEthernetPort(EthernetThread);
  end else
    EthernetThreads.CloseEthernetPorts;
  if Assigned(ListenerThread) then
  begin
    ListenerThread.Terminate;
    if Assigned(ListenerThread.Socket) then
      ListenerThread.Socket.CloseSocket;  // Force out of wait state with an error
    while ListenerThread.Running do
      Application.ProcessMessages;
    FreeAndNil(FListenerThread);
  end;
end;

procedure TLccEthernetServer.SetSchedulerPipelineSize(AValue: Integer);
begin
  if (AValue < 1) or (AValue > 10) then
    AValue := 1;

  if AValue <> FSchedulerPipelineSize then
  begin
    FSchedulerPipelineSize:=AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread, True);
  end;
end;

procedure TLccEthernetServer.SetSleepCount(AValue: Integer);
begin
    if AValue <> FSleepCount then
  begin
    FSleepCount := AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread, True);
  end;
end;

procedure TLccEthernetServer.UpdateListenerEvents(
  AListenerThread: TLccEthernetListener; Suspend: Boolean);
begin
  if Assigned(AListenerThread) then
  begin
    AListenerThread.OnClientDisconnect := OnClientDisconnect;
    AListenerThread.OnConnectionStateChange := OnConnectionStateChange;
    AListenerThread.OnErrorMessage := OnErrorMessage;
    AListenerThread.OnReceiveMessage := OnReceiveMessage;
    AListenerThread.OnSchedulerClass := OnSchedulerClass;
    AListenerThread.OnSendMessage := OnSendMessage;
    AListenerThread.OnSchedulerRemoveWaitingForReplyMessage := OnSchedulerRemoveWaitingForReplyMessage;
    AListenerThread.OnSchedulerAddOutgoingMessage := OnSchedulerAddOutgoingMessage;
    AListenerThread.OnSchedulerRemoveOutgoingMessage := OnSchedulerRemoveOutgoingMessage;
    AListenerThread.OnSchedulerAddWaitingForReplyMessage := OnSchedulerAddWaitingForReplyMessage;
    AListenerThread.SchedulerPipelineSize := FSchedulerPipelineSize;
    AListenerThread.GridConnect := FGridConnect;
  end;
end;

procedure TLccEthernetServer.UpdateThreadEvents(EthernetThread: TLccEthernetServerThread);
begin
  EthernetThread.OnSendMessage := OnSendMessage;
  EthernetThread.Scheduler.OnAddWaitingForReplyMessage := OnSchedulerAddWaitingForReplyMessage;
  EthernetThread.Scheduler.OnRemoveWaitingForReplyMessage := OnSchedulerRemoveWaitingForReplyMessage;
  EthernetThread.Scheduler.OnAddOutgoingMessage := OnSchedulerAddOutgoingMessage;
  EthernetThread.Scheduler.OnRemoveOutgoingMessage := OnSchedulerRemoveOutgoingMessage;
  EthernetThread.Scheduler.PipelineSize := FSchedulerPipelineSize;
  EthernetThread.SleepCount := SleepCount;
  EthernetThread.GridConnect := FGridConnect;
end;

procedure TLccEthernetServer.UpdateThreadsEvents;
var
  i: Integer;
  L: TList;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
      UpdateThreadEvents(TLccEthernetServerThread( L[i]));
  finally
    EthernetThreads.UnlockList;
  end;
end;

constructor TLccEthernetServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEthernetThreads := TLccEthernetThreadList.Create;
  FSchedulerPipelineSize := 1;
  FHub := False;
end;

destructor TLccEthernetServer.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  inherited Destroy;
end;

function TLccEthernetServer.OpenEthernetConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
begin
  Result := TLccEthernetListener.Create(True, Self, AnEthernetRec);
  Result.Owner := Self;
  UpdateListenerEvents(Result, True);
  Result.Suspended := False;
  ListenerThread := Result;
end;

function TLccEthernetServer.OpenEthernetConnectionWithLccSettings: TLccEthernetListener;
var
  AnEthernetRec: TLccEthernetRec;
begin
  if Assigned(LccSettings) then
  begin
    AnEthernetRec.ConnectionState := ccsListenerDisconnected;
    AnEthernetRec.SuppressNotification := False;
    AnEthernetRec.Thread := nil;
    AnEthernetRec.MessageStr := '';
    AnEthernetRec.ListenerPort := LccSettings.Ethernet.LocalListenerPort;
    AnEthernetRec.ListenerIP := LccSettings.Ethernet.LocalListenerIP;
    AnEthernetRec.ClientIP := '';
    AnEthernetRec.ClientPort := 0;
    AnEthernetRec.HeartbeatRate := 0;
    AnEthernetRec.ErrorCode := 0;
    AnEthernetRec.MessageArray := nil;
    Result := OpenEthernetConnection(AnEthernetRec);
  end;
end;

procedure TLccEthernetServer.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
  L: TList;
  EthernetThread: TLccEthernetServerThread;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccEthernetServerThread( L[i]);
      if not EthernetThread.IsTerminated then
        EthernetThread.Scheduler.OutgoingMsg(AMessage);
    end;
  finally
    EthernetThreads.UnlockList;
  end;
end;

procedure TLccEthernetServer.SetGridConnect(AValue: Boolean);
begin
  if AValue <> FGridConnect then
  begin
    FGridConnect:=AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread, True);
  end;

end;

{
procedure TLccEthernetServer.SetOnSchedulerRemoveOutgoingMessage(AValue: TOnMessageEvent);
begin
  if @FOnSchedulerRemoveOutgoingMessage <> @AValue then
  begin
    FOnSchedulerRemoveOutgoingMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetServer.SetOnSchedulerAddOutgoingMessage(AValue: TOnMessageEvent);
begin
  if @FOnSchedulerAddOutgoingMessage <> @AValue then
  begin
    FOnSchedulerAddOutgoingMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetServer.SetOnSchedulerAddWaitingForReplyMessage(AValue: TOnMessageEvent);
begin
  if @FOnSchedulerAddWaitingForReplyMessage <> @AValue then
  begin
    FOnSchedulerAddWaitingForReplyMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetServer.SetOnSchedulerRemoveWaitingForReplyMessage(AValue: TOnMessageRemoveWaitingForReplyEvent);
begin
  if @FOnSchedulerRemoveWaitingForReplyMessage <> @AValue then
  begin
    FOnSchedulerRemoveWaitingForReplyMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;

procedure TLccEthernetServer.SetOnSendMessage(AValue: TOnMessageEvent);
begin
  if @FOnSendMessage <> @AValue then
  begin
    FOnSendMessage:=AValue;
    if not (csDesigning in ComponentState) then
      UpdateThreadsEvents;
  end;
end;
}

procedure TLccEthernetServer.ClearSchedulerQueues;
var
  i: Integer;
  L: TList;
  EthernetThread: TLccEthernetServerThread;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccEthernetServerThread( L[i]);
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

{ TLccEthernetServerThread }

procedure TLccEthernetServerThread.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FEthernetRec.ConnectionState := NewConnectionState;
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
    SendConnectionNotification(ccsListenerClientDisconnected);
    Terminate;
  end;

var
  TxStr: LccString;
  RcvByte: Byte;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList: TStringList;
  i: Integer;
  DynamicByteArray: TDynamicByteArray;
begin
  FRunning := True;

  SendConnectionNotification(ccsListenerClientConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  Socket.Socket := SocketHandleForListener;    // Read back the handle
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
    FEthernetRec.ClientIP := Socket.GetRemoteSinIP;
    FEthernetRec.ClientPort := Socket.GetRemoteSinPort;
    FEthernetRec.ListenerIP := Socket.GetLocalSinIP;
    FEthernetRec.ListenerPort := Socket.GetLocalSinPort;
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
      SendConnectionNotification(ccsListenerClientConnected);
      try
        try
          while not IsTerminated and (FEthernetRec.ConnectionState = ccsListenerClientConnected) do
          begin
            for i := 0 to SleepCount - 1 do
              {$IFDEF FPC}
              ThreadSwitch;
              {$ELSE}
              Sleep(1);
              {$ENDIF}

            // Handle the Socket using GridConnect
            if Gridconnect then
            begin
              TxStr := '';
              TxList := OutgoingGridConnect.LockList;
              try
                if TxList.Count > 0 then
                begin
                  TxStr := LccString( TxList[0]);
                  TxList.Delete(0);
                end;
              finally
                OutgoingGridConnect.UnlockList;
              end;

              if TxStr <> '' then
              begin
                Socket.SendString(LccString( TxStr) + LF);
                if Socket.LastError <> 0 then
                  HandleErrorAndDisconnect;
              end;

              RcvByte := Socket.RecvByte(1);
              case Socket.LastError of
                0 :
                  begin
                    GridConnectStrPtr := nil;
                    if GridConnectHelper.GridConnect_DecodeMachine(RcvByte, GridConnectStrPtr) then
                    begin
                      FEthernetRec.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                      Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                    end;
                  end;
                WSAETIMEDOUT :
                  begin

                  end;
                WSAECONNRESET   :
                  begin
                    FEthernetRec.MessageStr := Socket.LastErrorDesc;
                    Socket.ResetLastError;
                    Synchronize({$IFDEF FPC}@{$ENDIF}DoClientDisconnect);
                    FEthernetRec.MessageStr := '';
                    Terminate;
                  end
              else
                HandleErrorAndDisconnect
              end;
            end else
            begin    // Handle the Socket with LCC TCP Protocol
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

              RcvByte := Socket.RecvByte(1);
              case Socket.LastError of
                0 :
                  begin
                    DynamicByteArray := nil;
                    if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, FEthernetRec.MessageArray) then
                      Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                  end;
                WSAETIMEDOUT :
                  begin

                  end;
                WSAECONNRESET   :
                  begin
                    FEthernetRec.MessageStr := Socket.LastErrorDesc;
                    Socket.ResetLastError;
                    Synchronize({$IFDEF FPC}@{$ENDIF}DoClientDisconnect);
                    FEthernetRec.MessageStr := '';
                    Terminate;
                  end
              else
                HandleErrorAndDisconnect
              end;
            end;
          end;
        finally
          SendConnectionNotification(ccsListenerClientDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        SendConnectionNotification(ccsListenerClientDisconnected);
        Owner.EthernetThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

function TLccEthernetServerThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

function TLccEthernetServerThread.GetScheduler: TSchedulerBase;
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


procedure TLccEthernetServerThread.SendMessage(AMessage: TLccMessage);
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
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and Owner.LoggingFrame.Visible then
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
        if Assigned(Owner) and Assigned(Owner.LoggingFrame) and Owner.LoggingFrame.Visible then
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

constructor TLccEthernetServerThread.Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec);
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

destructor TLccEthernetServerThread.Destroy;
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

procedure TLccEthernetServerThread.DoClientDisconnect;
begin
  if Assigned(OnClientDisconnect) then
    OnClientDisconnect(Self, FEthernetRec)
end;

procedure TLccEthernetServerThread.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccEthernetServerThread.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccEthernetServerThread.DoReceiveMessage;
var
  LocalMessage: TLccMessage;
  L: TList;
  i: Integer;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event
    if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, FEthernetRec);

    if Gridconnect then
    begin
      {$IFDEF LOGGING}
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and Owner.LoggingFrame.Visible then
        PrintToSynEdit( 'R: ' + EthernetRec.MessageStr,
                        Owner.LoggingFrame.SynEdit,
                        Owner.LoggingFrame.ActionLogPause.Checked,
                        Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                        Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
      {$ENDIF}

      LocalMessage := nil;
      if (Scheduler <> nil) then
      begin
        if Scheduler.IncomingMsgGridConnectStr(FEthernetRec.MessageStr, LocalMessage) then // In goes a raw message
        begin
          if (Owner.NodeManager <> nil) then
            Owner.NodeManager.ProcessMessage(LocalMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

          if Owner.Hub then
          begin
            L := Owner.EthernetThreads.LockList;
            try
              for i := 0 to L.Count - 1 do
              begin
                if TLccEthernetServerThread(L[i]) <> Self then
                  TLccEthernetServerThread(L[i]).SendMessage(LocalMessage);
              end;
            finally
              Owner.EthernetThreads.UnlockList;
            end
          end
        end
      end
    end else
    begin   // TCP Protocol
      {$IFDEF LOGGING}
      if Assigned(Owner) and Assigned(Owner.LoggingFrame) and Owner.LoggingFrame.Visible then
        PrintTCPToSynEdit( '...Receiving TCP...',
                        EthernetRec.MessageArray,
                        Owner.LoggingFrame.SynEdit,
                        Owner.LoggingFrame.ActionLogPause.Checked,
                        Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                        Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
      {$ENDIF}
      LocalMessage := nil;
      if (Scheduler <> nil) then
      begin
        if Scheduler.IncomingMsgEthernet(FEthernetRec.MessageArray, LocalMessage) then // In goes a raw message
        begin
          if (Owner.NodeManager <> nil) then
            Owner.NodeManager.ProcessMessage(LocalMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

          if Owner.Hub then
          begin
            L := Owner.EthernetThreads.LockList;
            try
              for i := 0 to L.Count - 1 do
              begin
                if TLccEthernetServerThread(L[i]) <> Self then
                  TLccEthernetServerThread(L[i]).SendMessage(LocalMessage);
              end;
            finally
              Owner.EthernetThreads.UnlockList;
            end
          end
        end
      end

    end;
  end
end;

procedure TLccEthernetServerThread.DoSendMessage(AMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, AMessage);
end;

initialization
  RegisterClass(TLccEthernetServer);

finalization

end.

