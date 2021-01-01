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
  lcc_utilities,
  lcc_app_common_settings,
  lcc_common_classes,
  lcc_alias_server;

type
  TLccEthernetClient = class;   // Forward


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

  TLccEthernetClientThread =  class(TLccBaseEthernetThread)
    protected
      procedure DoReceiveMessage; override;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure SendMessage(AMessage: TLccMessage); override;
  end;


  { TLccEthernetClient }

  TLccEthernetClient = class(TLccEthernetHardwareConnectionManager)
  private
    { Private declarations }
  protected
    { Protected declarations }
    function GetConnected: Boolean; override;
  public
    { Public declarations }
    function OpenConnection(AnEthernetRec: TLccEthernetRec): TThread; override;
  published
    { Published declarations }
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

{ TLccEthernetClient }

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

function TLccEthernetClient.OpenConnection(AnEthernetRec: TLccEthernetRec): TThread;
begin
  Result := TLccEthernetClientThread.Create(True, Self, AnEthernetRec);
  (Result as TLccEthernetClientThread).OnConnectionStateChange := OnConnectionStateChange;
  (Result as TLccEthernetClientThread).OnErrorMessage := OnErrorMessage;
  (Result as TLccEthernetClientThread).OnReceiveMessage := OnReceiveMessage;
  (Result as TLccEthernetClientThread).OnSendMessage := OnSendMessage;
  (Result as TLccEthernetClientThread).GridConnect := Gridconnect;
  (Result as TLccEthernetClientThread).UseSynchronize := UseSynchronize;
  EthernetThreads.Add(Result);
  (Result as TLccEthernetClientThread).Start
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

  HandleSendConnectionNotification(ccsClientConnecting);
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
      HandleSendConnectionNotification(ccsClientConnected);
      try
        LocalSleepCount := 0;
        try
          while not IsTerminated and (FEthernetRec.ConnectionState = ccsClientConnected) do

          begin  // Handle the Socket using GridConnect
            if Gridconnect then
            begin
              if LocalSleepCount >= SleepCount then
              begin
                TryTransmitGridConnect(True);
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              TryReceiveGridConnect(GridConnectHelper, True);
            end else
            begin    // Handle the Socket with LCC TCP Protocol
              if LocalSleepCount >= SleepCount then
              begin
                TryTransmitTCPProtocol(True);
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              TryReceiveTCPProtocol(True);
            end;
          end;
        finally
          HandleSendConnectionNotification(ccsClientDisconnecting);
          if Gridconnect then
            TryTransmitGridConnect(False) // Flush it
          else
            TryTransmitTCPProtocol(False);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        HandleSendConnectionNotification(ccsClientDisconnected);
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
//  if Socket.SocksOpen then
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

