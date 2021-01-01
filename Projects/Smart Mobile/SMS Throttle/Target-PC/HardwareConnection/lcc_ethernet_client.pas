unit lcc_ethernet_client;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
    {$DEFINE LOGGING}
  {$ENDIF}
{$ENDIF}

{$IFNDEF DWSCRIPT}
{$I lcc_compilers.inc}
{$ENDIF}


// TEMP
{$UNDEF LOGGING}

{$IFDEF ULTIBO}
  {$DEFINE DEBUG}
{$ENDIF}

{$IFNDEF DWSCRIPT}
{$I ..\lcc_compilers.inc}
{$ENDIF}

interface

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

  {$IFDEF ULTIBO}
  lcc_threaded_stringlist,
  Winsock2,
  Console,
  {$ELSE}
  blcksock,
  synsock,
  {$ENDIF}
  lcc_gridconnect,
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_threaded_circulararray,
  lcc_ethernet_tcp,
  lcc_utilities,
  lcc_app_common_settings,
  lcc_common_classes,
  lcc_node_messages_can_assembler_disassembler,
  lcc_alias_server;

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
    MessageArray: lcc_defines.TDynamicByteArray;   // Contains the TCP Protocol message bytes of not using GridConnect
    ErrorCode: Integer;
    LccMessage: TLccMessage;
    SuppressNotification: Boolean;    // True to stop any Syncronoize() call being called
    WebSocket: Boolean;               // Create A Websocket thread vs a basic TCP thread
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
      FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
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
      procedure SendMessage(AMessage: TLccMessage); override;

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
      property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;
    public
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetClient; AnEthernetRec: TLccEthernetRec); reintroduce;
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
    FUseSynchronize: Boolean;  // If set the threads will call back on a Syncronize call else incoming messages are put in the IncomingGridConnect or IncomingCircularArray buffers and the app needs to poll this buffer
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

    function OpenConnection(AnEthernetRec: TLccEthernetRec): TLccEthernetClientThread;
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
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
    {$IFDEF FPC}
 //JDK   {$I TLccEthernetClient.lrs}
    {$ENDIF}
    RegisterComponents('LCC',[TLccEthernetClient]);
  {$ENDIF}
end;

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
 // {$IFDEF DELPHI}
 //mm List: TList<TLccEthernetClientThread>;
 // {$ELSE}
  List: TList;
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
 // {$IFDEF DELPHI}
 // List: TList<TLccEthernetClientThread>;
 // {$ELSE}
  List: TList;
 // {$ENDIF}
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
 // {$IFDEF DELPHI}
 // L: TList<TLccEthernetClientThread>;
 // {$ELSE}
  L: TList;
 // {$ENDIF}
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
  FUseSynchronize := True;
  FEthernetThreads := TLccEthernetThreadList.Create;
end;

destructor TLccEthernetClient.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  inherited Destroy;
end;

function TLccEthernetClient.OpenConnection(AnEthernetRec: TLccEthernetRec): TLccEthernetClientThread;
begin
  Result := TLccEthernetClientThread.Create(True, Self, AnEthernetRec);
  Result.OnConnectionStateChange := OnConnectionStateChange;
  Result.OnErrorMessage := OnErrorMessage;
  Result.OnReceiveMessage := OnReceiveMessage;
  Result.OnSendMessage := OnSendMessage;
  Result.GridConnect := Gridconnect;
  Result.UseSynchronize := UseSynchronize;
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
 // {$IFDEF DELPHI}
 // List: TList<TLccEthernetClientThread>;
 // {$ELSE}
  List: TList;
//  {$ENDIF}
  EthernetThread: TLccEthernetClientThread;
begin
  List := EthernetThreads.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      EthernetThread := TLccEthernetClientThread( List[i]);
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
//  {$IFDEF DELPHI}
//  List: TList<TLccEthernetClientThread>;
 // {$ELSE}
  List: TList;
//  {$ENDIF}
  EthernetThread: TLccEthernetClientThread;
  StringList: TStringList;
  TempText: string;
begin
  List := EthernetThreads.LockList;
  try  // TODO
    for i := 0 to List.Count - 1 do
    begin
      EthernetThread := TLccEthernetClientThread( List[i]);
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
  TxList, RxList: TStringList;
  RetryCount: Integer;
  Peer: TVarSin;
  DynamicByteArray: TDynamicByteArray;
  RcvByte: Byte;
  LocalSleepCount: Integer;
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
      FillChar(Peer, Sizeof(TVarSin), #0);
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
                    begin
                      if UseSynchronize then
                        Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage)
                      else begin
                        DynamicByteArray := nil;
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
      UpdateAliasServer(AMessage);
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

constructor TLccEthernetClientThread.Create(CreateSuspended: Boolean; AnOwner: TLccEthernetClient; AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := Self;
  FEthernetRec.LccMessage := TLccMessage.Create;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
  GridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
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
  {$IFDEF DWSCRIPT}
  FEthernetRec.LccMessage.Free;
  FTcpDecodeStateMachine.Free;
  FGridConnectMessageAssembler.Free;
  {$ELSE}
  FreeAndNil(FEthernetRec.LccMessage);
  FreeAndNil(FTcpDecodeStateMachine);
  FreeAndNil(FGridConnectMessageAssembler);
  {$ENDIF}
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
  // Called in the content of the main thread through Syncronize
  if not IsTerminated then
  begin
    if Assigned(OnReceiveMessage) then    // Do first so we get notified before any response is sent in ProcessMessage
      OnReceiveMessage(Self, FEthernetRec);

    if Gridconnect then
    begin
      if Owner.NodeManager <> nil then
      begin
        UpdateAliasServer(EthernetRec.LccMessage);
        Owner.NodeManager.ProcessMessage(EthernetRec.LccMessage);
      end;
    end else
    begin
      // Called in the content of the main thread through Syncronize
      if Owner.NodeManager <> nil then
        if WorkerMsg.LoadByLccTcp(FEthernetRec.MessageArray) then // In goes a raw message
          Owner.NodeManager.ProcessMessage(WorkerMsg);  // What comes out is
    end;
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

