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
  lcc_utilities,
  lcc_app_common_settings,
  lcc_common_classes,
  lcc_ethernet_common;

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
      procedure Execute; override;
  end;


  { TLccEthernetClient }

  TLccEthernetClient = class(TLccEthernetHardwareConnectionManager)
  private
    { Private declarations }
  protected
    { Protected declarations }
    function IsLccLink: Boolean; override;
  public
    { Public declarations }
    function OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread; override;
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

function TLccEthernetClient.IsLccLink: Boolean;
begin
  Result := True;
end;

function TLccEthernetClient.OpenConnection(ConnectionInfo: TLccHardwareConnectionInfo): TLccConnectionThread;
begin
  inherited OpenConnection(ConnectionInfo as TLccEthernetConnectionInfo);
  Result := TLccEthernetClientThread.Create(True, Self, ConnectionInfo);
  OnConnectionStateChange := OnConnectionStateChange;
  OnErrorMessage := OnErrorMessage;
  OnReceiveMessage := OnReceiveMessage;
  ConnectionThreads.Add(Result);
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
  GridConnectHelper: TGridConnectHelper;
  RetryCount: Integer;
  Peer: TVarSin;
  LocalSleepCount: Integer;
begin
  FRunning := True;

  HandleSendConnectionNotification(lcsConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  if (ConnectionInfo as TLccEthernetConnectionInfo).LingerTime > 0 then
    Socket.SetLinger(True, (ConnectionInfo as TLccEthernetConnectionInfo).LingerTime);
  Socket.Family := SF_IP4;                  // IP4
  if ConnectionInfo.GridConnect then
    Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := (ConnectionInfo as TLccEthernetConnectionInfo).HeartbeatRate;
  Socket.SetTimeout(0);
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    GridConnectHelper.Free;
    FRunning := False
  end else
  begin
    RetryCount := 0;

    if (ConnectionInfo as TLccEthernetConnectionInfo).AutoResolveIP then
    begin
      {$IFDEF LCC_WINDOWS}
      (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := ResolveWindowsIp(Socket);
      {$ELSE}
      (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := ResolveUnixIp;
      {$ENDIF}
    end;

    Socket.Connect(String( (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP), String( IntToStr((ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort)));
    while (Socket.LastError = WSAEINPROGRESS) or (Socket.LastError = WSAEALREADY) and (RetryCount < 40) do   {20 Second Wait}
    begin
      Socket.ResetLastError;
      Peer.AddressFamily := 0;  // remove the compiler hint
      FillChar(Peer, Sizeof(TVarSin), #0);
      if GetPeerName(Socket.Socket, Peer) = 0 then
        Break;
      Inc(RetryCount);
      Sleep(500);
    end;

    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      GridConnectHelper.Free;
      FRunning := False
    end else
    begin
      HandleSendConnectionNotification(lcsConnected);
      try
        LocalSleepCount := 0;
        try
          while not IsTerminated and (ConnectionInfo.ConnectionState = lcsConnected) do

          begin  // Handle the Socket using GridConnect
            if ConnectionInfo.Gridconnect then
            begin
              if LocalSleepCount >= ConnectionInfo.SleepCount then
              begin
                TryTransmitGridConnect;
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              TryReceiveGridConnect(GridConnectHelper);
            end else
            begin    // Handle the Socket with LCC TCP Protocol
              if LocalSleepCount >= ConnectionInfo.SleepCount then
              begin
                TryTransmitTCPProtocol;
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);

              TryReceiveTCPProtocol;
            end;
          end;
        finally
          HandleSendConnectionNotification(lcsDisconnecting);
          if ConnectionInfo.Gridconnect then
            TryTransmitGridConnect // Flush it
          else
            TryTransmitTCPProtocol;
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
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
{$ENDIF}


initialization
  RegisterClass(TLccEthernetClient);

finalization

end.

