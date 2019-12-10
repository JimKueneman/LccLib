unit lcc.ethernet.client;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
    {$DEFINE LOGGING}
  {$ENDIF}
{$ENDIF}


// TEMP
{$UNDEF LOGGING}

{$IFDEF ULTIBO}
  {$DEFINE DEBUG}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP} LResources, Forms, Controls, Graphics, Dialogs, {$ENDIF}
  {$ELSE}
  FMX.Forms, Types, System.Generics.Collections,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging,
  lcc_detailed_logging,
  {$ENDIF}
  lcc_gridconnect,
  {$IFDEF ULTIBO}
  lcc_threaded_stringlist,
  Winsock2,
  Console,
  {$ELSE}
  blcksock,
  synsock,
  {$ENDIF}
  lcc.defines,
  lcc.node.messages.can.assembler_disassembler,
  lcc.node.manager,
  lcc.node.messages,
  lcc.threaded.circulararray,
  lcc.ethernet.tcp,
  lcc.utilities,
  lcc_app_common_settings,
  lcc_common_classes;

type
  TLccEthernetClient = class;   // Forward


  { TLccEthernetRec }

  TLccEthernetRec = record
    Thread: TLccConnectionThread;    // Thread owing the Record
    AutoResolveIP: Boolean;          // Tries to autoresolve the local unique netword IP of the machine
    ClientIP,
    ListenerIP: String;
    ClientPort,
    ListenerPort: Word;
    HeartbeatRate: Integer;
    ConnectionState: TConnectionState; // Current State of the connection
    MessageStr: String;                // Contains the string for the resuting message from the thread
    MessageArray: TDynamicByteArray;   // Contains the TCP Protocol message bytes of not using GridConnect
    ErrorCode: Integer;
    LccMessage: TLccMessage;
    SuppressNotification: Boolean;    // True to stop any Syncronoize() call being called
  end;


  TOnEthernetRecFunc = procedure(Sender: TObject; EthernetRec: TLccEthernetRec) of object;
  TOnEthernetReceiveFunc = procedure(Sender: TObject; EthernetRec: TLccEthernetRec) of object;


  {$IFDEF ULTIBO}

  { TUltiboTcpReadThread }

  TUltiboTcpReadThread = class(TThread)
  private
    FStringList: TThreadStringList;
    FTcpClient: TWinsock2TCPClient;
  protected
    procedure Execute; override;
  public
    property StringList: TThreadStringList read FStringList write FStringList;
    property TcpClient: TWinsock2TCPClient read FTcpClient write FTcpClient;
  end;
  {$ENDIF}

  { TLccEthernetClientThread }

  TLccEthernetClientThread =  class(TLccConnectionThread)
    private
      FEthernetRec: TLccEthernetRec;
      FOnErrorMessage: TOnEthernetRecFunc;
      FOnConnectionStateChange: TOnEthernetRecFunc;
      FOnReceiveMessage: TOnEthernetReceiveFunc;
      FOnSendMessage: TOnMessageEvent;
      FOwner: TLccEthernetClient;
      {$IFDEF ULTIBO}
      FStringList: TThreadStringList;
      FTcpClient: TWinsock2TCPClient;
      {$ELSE}
      FSocket: TTCPBlockSocket;
      {$ENDIF}
      FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
    protected
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage);

      property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
      {$IFDEF ULTIBO}
      property StringList: TThreadStringList read FStringList write FStringList;
      property TcpClient: TWinsock2TCPClient read FTcpClient write FTcpClient;
      {$ELSE}
      property Socket: TTCPBlockSocket read FSocket write FSocket;
      {$ENDIF}
      property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
      property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
      property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
      property Owner: TLccEthernetClient read FOwner write FOwner;
      property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
    public
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetClient; const AnEthernetRec: TLccEthernetRec); reintroduce;
      destructor Destroy; override;
  end;

  { TLccEthernetThreadList }

  TLccEthernetThreadList = class(TThreadList)      // Contains TClientSocketThread objects
  public
    destructor Destroy; override;
    procedure CloseEthernetPorts;
    procedure CloseEthernetPort(EthernetThread: TLccEthernetClientThread);
  end;

  { TLccEthernetClient }

  TLccEthernetClient = class(TLccHardwareConnectionManager)
  private
    FEthernetThreads: TLccEthernetThreadList;
    FGridConnect: Boolean;
    FLccSettings: TLccSettings;
    {$IFDEF LOGGING}FLoggingFrame: TFrameLccLogging;{$ENDIF}
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FSleepCount: Integer;
    function GetConnected: Boolean;
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
    procedure CloseConnection( EthernetThread: TLccEthernetClientThread);
    procedure SendMessage(AMessage: TLccMessage); override;
    procedure SendMessageRawGridConnect(GridConnectStr: String); override;
    property Connected: Boolean read GetConnected;
    property EthernetThreads: TLccEthernetThreadList read FEthernetThreads write FEthernetThreads;
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector
  published
    { Published declarations }
    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write SetSleepCount;
  end;

//JDK procedure Register;

implementation

//JDK
(*
procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
    {$IFDEF FPC}
    {$I TLccEthernetClient.lrs}
    {$ENDIF}
    RegisterComponents('LCC',[TLccEthernetClient]);
  {$ENDIF}
end;
*)

{$IFDEF ULTIBO}
{ TUltiboTcpReadThread }

procedure TUltiboTcpReadThread.Execute;
var
  RxBuffer: array[0..1199] of Byte;
  ACount: Integer;
  IsClosed: Boolean;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
begin
  {$IFDEF DEBUG}ConsoleWriteLn('Starting ClientRead Thread');{$ENDIF}
  GridConnectHelper := TGridConnectHelper.Create;
  while not Terminated do
  begin
    ACount := 0;
    IsClosed := False;
    if TcpClient.ReadAvailable(@RxBuffer, Sizeof(RxBuffer), ACount, IsClosed) then
    begin
      if IsClosed then
        Terminate
      else begin
        GridConnectStrPtr := nil;
        for i := 0 to ACount - 1 do
        begin
          if GridConnectHelper.GridConnect_DecodeMachine(RxBuffer[i], GridConnectStrPtr) then
          begin
             if Assigned(StringList) then
               StringList.Add(GridConnectBufferToString(GridConnectStrPtr^));
          end;
        end
      end;
     end
  end;
  FreeAndNil(GridConnectHelper);
  {$IFDEF DEBUG}ConsoleWriteLn('Terminating ClientRead Thread');{$ENDIF}
end;
{$ENDIF}

{ TLccEthernetThreadList }

destructor TLccEthernetThreadList.Destroy;
begin
  CloseEthernetPorts;
  inherited Destroy;
end;

procedure TLccEthernetThreadList.CloseEthernetPorts;
var
  List: TList;
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
      Thread := TLccEthernetClientThread( List[0]);
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

procedure TLccEthernetThreadList.CloseEthernetPort( EthernetThread: TLccEthernetClientThread);
var
  TimeCount: Cardinal;
begin
  TimeCount := 0;
  EthernetThread.Terminate;
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
  FreeAndNil( EthernetThread);
end;

{ TLccEthernetClient }

procedure TLccEthernetClient.CloseConnection(EthernetThread: TLccEthernetClientThread);
begin
  if Assigned(EthernetThread) then
  begin
    EthernetThreads.Remove(EthernetThread);
    EthernetThreads.CloseEthernetPort(EthernetThread);
  end else
    EthernetThreads.CloseEthernetPorts;
end;

procedure TLccEthernetClient.SetSleepCount(AValue: Integer);
begin
  if FSleepCount <> AValue then
  begin
    FSleepCount := AValue;
    UpdateThreadsEvents;
  end;
end;

function TLccEthernetClient.GetConnected: Boolean;
var
  List: TList;
begin
  List := EthernetThreads.LockList;
  Result := List.Count > 0;
  EthernetThreads.UnlockList;
end;

procedure TLccEthernetClient.UpdateThreadEvents(EthernetThread: TLccEthernetClientThread);
begin
  EthernetThread.OnSendMessage := OnSendMessage;
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
end;

destructor TLccEthernetClient.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  inherited Destroy;
end;

function TLccEthernetClient.OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetClientThread;
begin
  Result := TLccEthernetClientThread.Create(True, Self, AnEthernetRec);
  Result.OnConnectionStateChange := OnConnectionStateChange;
  Result.OnErrorMessage := OnErrorMessage;
  Result.OnReceiveMessage := OnReceiveMessage;
  Result.OnSendMessage := OnSendMessage;
  Result.GridConnect := Gridconnect;
  EthernetThreads.Add(Result);
  Result.Start
end;

function TLccEthernetClient.OpenConnectionWithLccSettings: TLccEthernetClientThread;
var
  AnEthernetRec: TLccEthernetRec;
begin
  Result := nil;
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
        EthernetThread.SendMessage(AMessage);
    end;
  finally
    EthernetThreads.UnlockList;
  end;
end;

procedure TLccEthernetClient.SendMessageRawGridConnect(GridConnectStr: String);
var
  i: Integer;
  L: TList;
  EthernetThread: TLccEthernetClientThread;
  StringList: TStringList;
  TempText: string;
begin
  L := EthernetThreads.LockList;
  try  // TODO
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccEthernetClientThread( L[i]);
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

{ TLccEthernetClientThread }

{$IFDEF ULTIBO}
procedure TLccEthernetClientThread.Execute;
var
  TcpReadThread: TUltiboTcpReadThread;
  SafetyNet: Integer;
  List: TStringList;
  i: Integer;
  OutString: ansistring;
begin
  {$IFDEF DEBUG}ConsoleWriteLn('Starting Client Thread');{$ENDIF}
  FEthernetRec.ConnectionState := ccsClientConnecting;
  DoConnectionState;


  if EthernetRec.AutoResolveIP then
    FEthernetRec.ClientIP := ResolveUltiboIp;

  TcpClient.RemoteAddress := EthernetRec.ListenerIP;
  TcpClient.RemotePort := EthernetRec.ListenerPort;

  {$IFDEF DEBUG}ConsoleWriteLn('RPi IP Address: ' + EthernetRec.ClientIP);{$ENDIF}
  {$IFDEF DEBUG}ConsoleWriteLn('Connecting to: ' + TcpClient.RemoteAddress + ':' + IntToStr(TcpClient.RemotePort));{$ENDIF}
  while (not TcpClient.Connect) and (not Terminated) do;

  if not Terminated then
  begin
    {$IFDEF DEBUG}ConsoleWriteLn('Started, creating read thread');{$ENDIF}
    TcpReadThread := TUltiboTcpReadThread.Create(True);
    TcpReadThread.StringList := StringList;
    TcpReadThread.TcpClient := TcpClient;
    TcpReadThread.Start;

    FEthernetRec.ConnectionState := ccsClientConnected;
    DoConnectionState;
    while not Terminated do
    begin
      List := StringList.LockList;
      try
        for i := 0 to List.Count - 1 do
        begin
          FEthernetRec.MessageStr := List[i];
          FEthernetRec.LccMessage.LoadByGridConnectStr(FEthernetRec.MessageStr);
          Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
        end;
      finally
        List.Clear;
        StringList.UnlockList;
      end;

      List := OutgoingGridConnect.LockList;
      try
        for i := 0 to List.Count - 1 do
        begin
          OutString := List[i] + #10;
          TcpClient.WriteData(@OutString[1], Length(OutString));
        end;
      finally
        List.Clear;
        OutgoingGridConnect.UnlockList;
      end;
      Sleep(1);
    end;
  end else
    Terminate;

  {$IFDEF DEBUG}ConsoleWriteLn('Terminating Client Thread');{$ENDIF}
  FEthernetRec.ConnectionState := ccsClientDisconnecting;
  DoConnectionState;

  TcpReadThread.StringList := nil;
  TcpReadThread.Terminate;
  SafetyNet := 0;
  while not TcpReadThread.Terminated do
  begin
    Inc(SafetyNet);
    Sleep(1000);
    if SafetyNet > 10 then
    begin
      TcpReadThread.Free;
      Break;
    end;
  end;
  TcpClient.CloseSocket;

  FEthernetRec.ConnectionState := ccsClientDisconnected;
  DoConnectionState;
  {$IFDEF DEBUG}ConsoleWriteLn('Terminated Client Thread');{$ENDIF}
end;
{$ELSE}

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
  TxStr: String;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  GridConnectStr: TGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList: TStringList;
  RetryCount: Integer;
  Peer: TVarSin;
  DynamicByteArray: TDynamicByteArray;
  RcvByte: Byte;
  LocalSleepCount: Integer;
  {$IFDEF LCC_WINDOWS}
  LocalName: string;
  IpStrings: TStringList;
  {$ELSE}
  Ip: String;
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
      {$IFDEF LCC_WINDOWS}
      FEthernetRec.ClientIP := ResolveWindowsIp(Socket);
      {$ELSE}
      FEthernetRec.ClientIP := ResolveUnixIp;
      {$ENDIF}
    end;


    Socket.Connect(String( EthernetRec.ListenerIP), String( IntToStr(EthernetRec.ListenerPort)));
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
                  GridConnectStr[0] := 0;
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
{$ENDIF}

procedure TLccEthernetClientThread.SendMessage(AMessage: TLccMessage);
var
  ByteArray: TDynamicByteArray;
  i: Integer;
begin
  if not IsTerminated then
  begin
    if Gridconnect then
    begin
      MsgDisAssembler.OutgoingMsgToMsgList(AMessage, MsgStringList);

      for i := 0 to MsgStringList.Count - 1 do
      begin;
        OutgoingGridConnect.Add(MsgStringList[i]);
        {$IFDEF LOGGING}
        if Assigned(Owner) and Assigned(Owner.LoggingFrame) and not Owner.LoggingFrame.Paused and Owner.LoggingFrame.Visible then
          PrintToSynEdit( 'S: ' + MsgStringList[i],
                          Owner.LoggingFrame.SynEdit,
                          Owner.LoggingFrame.ActionLogPause.Checked,
                          Owner.LoggingFrame.CheckBoxDetailedLogging.Checked,
                          Owner.LoggingFrame.CheckBoxJMRIFormat.Checked);
        {$ENDIF}
      end;
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
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
  {$IFDEF ULTIBO}
  StringList := TThreadStringList.Create;
  TcpClient := TWinsock2TCPClient.Create;
  {$ENDIF}
end;

destructor TLccEthernetClientThread.Destroy;
begin
  {$IFDEF ULTIBO}
  FreeAndNil(FStringList);
  FreeAndNil(FTcpClient);
  {$ENDIF}
  FreeAndNil(FEthernetRec.LccMessage);
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

      case MsgAssembler.IncomingMessageGridConnect(FEthernetRec.MessageStr, WorkerMsg) of
        imgcr_True :
          begin
            if Owner.NodeManager <> nil then
              Owner.NodeManager.ProcessMessage(WorkerMsg);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages
          end;
        imgcr_ErrorToSend :
          begin
            if Owner.NodeManager <> nil then
              if Owner.NodeManager.FindOwnedNodeBySourceID(WorkerMsg) <> nil then
                 Owner.NodeManager.SendLccMessage(WorkerMsg);
          end;
      end;
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

      if Owner.NodeManager <> nil then
        if WorkerMsg.LoadByLccTcp(FEthernetRec.MessageArray) then // In goes a raw message
          Owner.NodeManager.ProcessMessage(WorkerMsg);  // What comes out is
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

