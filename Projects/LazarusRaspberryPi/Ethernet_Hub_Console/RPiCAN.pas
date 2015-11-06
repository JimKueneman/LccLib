program RPiCAN;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, lcc_raspberrypi, blcksock, synsock, baseUnix, sockets,
  contnrs, lcc_gridconnect, crt
  { you can add units after this };

const
  GRIDCONNECT_STR_COUNT = 32;
  GRIDCONNECT_CHAR_COUNT = 30;

type

  { TClientConnection }

  TClientConnection = class
  private
    FGridConnectHelper: TGridConnectHelper;
    FSocket: TTCPBlockSocket;
    FInBuffer: TStringList;
    FOutBuffer: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property GridConnectHelper: TGridConnectHelper read FGridConnectHelper write FGridConnectHelper;
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property InBuffer: TStringList read FInBuffer write FInBuffer;
    property OutBuffer: TStringList read FOutBuffer write FOutBuffer;
  end;

  { TRPiCAN }

  TRPiCAN = class(TCustomApplication)
  private
    FClientConnections: TObjectList;
    FListening: Boolean;
    FListenSocket: TTCPBlockSocket;
    FRaspberryPiInBuffer: TStringList;
    FRaspberryPiSpi: TRaspberryPiSpi;
    FSocketHandleForListener: TSocket;
    FTxBufferNull: TPiSpiBuffer;
    function GetClientConnection(Index: Integer): TClientConnection;
    procedure SetClientConnection(Index: Integer; AValue: TClientConnection);
  protected
    property TxBufferNull: TPiSpiBuffer read FTxBufferNull write FTxBufferNull;
    property ClientConnections: TObjectList read FClientConnections write FClientConnections;
    procedure DoRun; override;
    procedure ExtractSpiRxBuffer(var RxBuffer: TPiSpiBuffer; Count: Integer);
  public
    property ClientConnection[Index: Integer]: TClientConnection read GetClientConnection write SetClientConnection;
    property Listening: Boolean read FListening write FListening;
    property ListenSocket: TTCPBlockSocket read FListenSocket write FListenSocket;
    property RaspberryPiSpi: TRaspberryPiSpi read FRaspberryPiSpi write FRaspberryPiSpi;
    property SocketHandleForListener: TSocket read FSocketHandleForListener write FSocketHandleForListener;
    property RaspberryPiInBuffer: TStringList read FRaspberryPiInBuffer write FRaspberryPiInBuffer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloseListenSocket;
    procedure DoCheckForNewClients;
    procedure DoCheckClientReceive;
    procedure DoHandleEthernetClientHub;
    procedure DoHandleRaspberryPiHub;
    procedure DoIdleRaspberryPiRx;
    procedure OpenListenSocket;
    procedure WriteHelp; virtual;
  end;

function ResolveUnixIp: String;
const
  CN_GDNS_ADDR = '127.0.0.1';
  CN_GDNS_PORT = 53;
var
  sock: longint;
  err: longint;
  HostAddr: TSockAddr;
  l: Integer;
  UnixAddr: TInetSockAddr;
begin
  err := 0;

  sock := fpsocket(AF_INET, SOCK_DGRAM, 0);
  assert(sock <> -1);

  UnixAddr.sin_family := AF_INET;
  UnixAddr.sin_port := htons(CN_GDNS_PORT);
  UnixAddr.sin_addr := StrToHostAddr(CN_GDNS_ADDR);

  if (fpConnect(sock, @UnixAddr, SizeOf(UnixAddr)) = 0) then
  begin
    try
      l := SizeOf(HostAddr);
      if (fpgetsockname(sock, @HostAddr, @l) = 0) then
      begin
        Result := NetAddrToStr(HostAddr.sin_addr);
      end
      else
      begin
        err:=socketError;
      end;
    finally
      if (fpclose(sock) <> 0) then
      begin
        err := socketError;
      end;
    end;
  end else
  begin
    err:=socketError;
  end;

  if (err <> 0) then
  begin
    // report error
  end;
end;

constructor TClientConnection.Create;
begin
  Inherited;
  InBuffer := TStringList.Create;
  OutBuffer := TStringList.Create;
  GridConnectHelper := TGridConnectHelper.Create;
end;

destructor TClientConnection.Destroy;
begin
  FreeAndNil(FSocket);
  FreeAndNil(FInBuffer);
  FreeAndNil(FOutBuffer);
  FreeAndNil(FGridConnectHelper);
  inherited Destroy;
end;



function TRPiCAN.GetClientConnection(Index: Integer): TClientConnection;
begin
  Result := TClientConnection( ClientConnections[Index]);
end;

procedure TRPiCAN.SetClientConnection(Index: Integer; AValue: TClientConnection);
begin
  ClientConnections[Index] := AValue;
end;

{ TRPiCAN }

procedure TRPiCAN.DoRun;
var
  ErrorMsg: String;
  C: Char;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  RaspberryPiSpi.Mode := psm_ClkIdleLo_DataFalling;
  RaspberryPiSpi.Speed := pss_976kHz;
  RaspberryPiSpi.Bits := psb_8;
  if RaspberryPiSpi.OpenSpi('/dev/spidev0.0') then
  begin
    WriteLn('Connected To Spi');

    OpenListenSocket;
    if Listening then
    begin
      C := 'G';
      try
        repeat
          DoCheckForNewClients;
          DoCheckClientReceive;
          DoHandleRaspberryPiHub;
          DoHandleEthernetClientHub;

          if KeyPressed then
            C := ReadKey;

        until C = 'q';
      finally
        ListenSocket.CloseSocket;
      end;
    end else
    begin
      WriteLn('Listener Socket Failed');
    end;
    RaspberryPiSpi.CloseSpi;
  end else
  begin
    WriteLn('Unable to connect to Spi');
  end;

  // stop program loop
  WriteLn('Shutting down');
  Terminate;
end;

procedure TRPiCAN.ExtractSpiRxBuffer(var RxBuffer: TPiSpiBuffer; Count: Integer);
var
  iRxBuffer, iRxChar: Integer;
  IncomingArray: array[0..GRIDCONNECT_CHAR_COUNT-1] of ansiChar;
begin
  iRxChar := 1;
  for iRxBuffer := 1 to Count do
  begin
    for iRxChar := 1 to GRIDCONNECT_CHAR_COUNT do
      IncomingArray[iRxChar - 1] := AnsiChar( RxBuffer[iRxBuffer - 1]);
    if IncomingArray[0] = ':' then
      RaspberryPiInBuffer.Add(IncomingArray);
  end;
end;

constructor TRPiCAN.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(TheOwner);
  StopOnException := True;
  RaspberryPiSpi := TRaspberryPiSpi.Create;
  ListenSocket := TTCPBlockSocket.Create;
  ClientConnections := TObjectList.Create;
  RaspberryPiInBuffer := TStringList.Create;
  for i := 0 to Length(TxBufferNull) - 1 do
    FTxBufferNull[i] := $00
end;

destructor TRPiCAN.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FRaspberryPiSpi);
  FreeAndNil(FListenSocket);
  FreeAndNil(FClientConnections);
  FreeAndNil(FRaspberryPiInBuffer);
  inherited Destroy;
end;

procedure TRPiCAN.CloseListenSocket;
begin
  if Listening then
    ListenSocket.CloseSocket;
end;

procedure TRPiCAN.DoCheckForNewClients;
var
  ClientSocket: TTCPBlockSocket;
  LocalClientConnection: TClientConnection;
begin
  if ListenSocket.CanRead(1) then
  begin
    if (ListenSocket.LastError <> WSAETIMEDOUT) and (ListenSocket.LastError = 0) then
    begin
      WriteLn('Client Connecting');
      ClientSocket := TTCPBlockSocket.Create;
      try
        ClientSocket.Family := SF_IP4;                  // IP4
        ClientSocket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
        ClientSocket.HeartbeatRate := 0;
        ClientSocket.SetTimeout(0);
        ClientSocket.Socket := ListenSocket.Accept;
        if ClientSocket.LastError = 0 then
        begin
          WriteLn('Client Connected: ' + ClientSocket.GetRemoteSinIP + ':' + IntToStr(ClientSocket.GetRemoteSinPort));
          LocalClientConnection := TClientConnection.Create;
          try
            LocalClientConnection.Socket := ClientSocket;
            ClientConnections.Add(LocalClientConnection)
          except
            WriteLn('Failed to Listen');
            FreeAndNil(ClientSocket);
          end;
        end;
      except
        WriteLn('Failed to Listen');
        FreeAndNil(ClientSocket);
      end;
    end
  end
end;

procedure TRPiCAN.DoCheckClientReceive;
var
  i: Integer;
  LocalSocket: TTCPBlockSocket;
  LocalConnection: TClientConnection;
  RcvByte: Byte;
  GridConnectStrPtr: PGridConnectString;
  GridConnectStr: string;
  Done: Boolean;
begin
  for i := ClientConnections.Count - 1 downto 0 do
  begin
    LocalConnection := ClientConnection[i];
    LocalSocket := ClientConnection[i].Socket;

    // Pull all bytes from the sockets incoming buffer
    Done := False;
    repeat
      RcvByte := LocalSocket.RecvByte(1);
      case LocalSocket.LastError of
        0 :
          begin
            GridConnectStrPtr := nil;
            if LocalConnection.GridConnectHelper.GridConnect_DecodeMachine(RcvByte, GridConnectStrPtr) then
            begin
              GridConnectStr := GridConnectBufferToString(GridConnectStrPtr^);
              LocalConnection.InBuffer.Add(GridConnectStr);
              WriteLn(GridConnectStr);
            end;
          end;
        WSAETIMEDOUT :
          begin
            Done := True;
          end;
        WSAECONNRESET   :
          begin
            WriteLn(LocalSocket.LastErrorDesc + ',' + LocalSocket.GetRemoteSinIP + ':' + IntToStr(LocalSocket.GetRemoteSinPort));
            ClientConnections.Delete(i);
            Done := True;
          end;
      else begin
          WriteLn(LocalSocket.LastErrorDesc + ',' + LocalSocket.GetRemoteSinIP + ':' + IntToStr(LocalSocket.GetRemoteSinPort));
          ClientConnections.Delete(i);
          Done := True;
        end;
      end;
    until Done;
  end;
end;

procedure TRPiCAN.DoHandleEthernetClientHub;
var
  i, j, iStr, iChar, iSpiStr: Integer;
  InConnection: TClientConnection;
  OutConnection: TClientConnection;
  OutString: string;
begin
  for i := 0 to ClientConnections.Count - 1 do
  begin
    InConnection := TClientConnection( ClientConnections[i]);
    for j := 0 to ClientConnections.Count - 1 do
    begin
      OutConnection := TClientConnection( ClientConnections[j]);

      // Handle the strings in the incoming Spi Buffer for this Outgoing Ehernet Connection
      // Sends to ALL Ethernet connections
      for iSpiStr := 0 to RaspberryPiInBuffer.Count - 1 do
      begin
        OutString := RaspberryPiInBuffer[i];
        // TODO: If Filter Allows then...
        for iChar := 1 to Length(OutString) do
          OutConnection.Socket.SendByte( Ord(OutString[iChar]));
      end;

      if j <> i then  // Don't send it back to the source socket
      begin
        for iStr := 0 to InConnection.InBuffer.Count - 1 do
        begin
          OutString := InConnection.InBuffer[iStr];
          // TODO: If Filter Allows then...
          for iChar := 1 to Length(OutString) do
            OutConnection.Socket.SendByte( Ord(OutString[iChar]));
        end;
      end;
    end;
    InConnection.InBuffer.Clear;
    RaspberryPiInBuffer.Clear;
  end;
end;

procedure TRPiCAN.DoHandleRaspberryPiHub;
var
  RxBuffer, TxBuffer: TPiSpiBuffer;
  i, iStr, iChar, iTxBuffer: Integer;
  InConnection: TClientConnection;
  OutString: ansistring;
begin
  for i := 0 to ClientConnections.Count - 1 do
  begin
    InConnection := TClientConnection( ClientConnections[i]);
    iTxBuffer := 1;
    for iStr := 0 to InConnection.InBuffer.Count - 1 do
    begin
      if iTxBuffer > (GRIDCONNECT_CHAR_COUNT*GRIDCONNECT_STR_COUNT) then
      begin
        iTxBuffer := 1;
        RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, GRIDCONNECT_CHAR_COUNT*GRIDCONNECT_STR_COUNT);
        ExtractSpiRxBuffer(RxBuffer, GRIDCONNECT_CHAR_COUNT*GRIDCONNECT_STR_COUNT);
      end;
      OutString := InConnection.InBuffer[iStr];
      // TODO: If Filter Allows then..
      iChar := 1;
      while iChar <= GRIDCONNECT_CHAR_COUNT do
      begin
        if iChar <= Length(OutString) then
          TxBuffer[iTxBuffer-1] := Ord( OutString[iChar])
        else
          TxBuffer[iTxBuffer-1] := $00 ;
        Inc(iChar);
        Inc(iTxBuffer);
      end;
    end;
    if iTxBuffer > 1 then
    begin
      RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, GRIDCONNECT_CHAR_COUNT*(iTxBuffer div GRIDCONNECT_CHAR_COUNT));
      ExtractSpiRxBuffer(RxBuffer, GRIDCONNECT_CHAR_COUNT*(iTxBuffer div GRIDCONNECT_CHAR_COUNT));
    end;
  end;
end;

procedure TRPiCAN.DoIdleRaspberryPiRx;
var
  RxBuffer: TPiSpiBuffer;
begin
  RaspberryPiSpi.Transfer(@TxBufferNull, @RxBuffer, GRIDCONNECT_CHAR_COUNT*5);
  ExtractSpiRxBuffer(RxBuffer, GRIDCONNECT_CHAR_COUNT*5);
end;

procedure TRPiCAN.OpenListenSocket;
var
  IP: string;
begin
  WriteLn('Opening Listening Socket');
  Listening := False;
  ListenSocket := TTCPBlockSocket.Create;          // Created in context of the thread
  ListenSocket.Family := SF_IP4;                  // IP4
  ListenSocket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  ListenSocket.HeartbeatRate := 0;
  ListenSocket.SetTimeout(0);
  WriteLn('Resolving IP Address');
  IP := ResolveUnixIp;
  WriteLn('IP Address: ' + IP);
  ListenSocket.Bind(String( IP), String( IntToStr(12021)));
  if ListenSocket.LastError = 0 then
  begin
    ListenSocket.Listen;
    if ListenSocket.LastError = 0 then
    begin
      WriteLn('Listening for Clients');
      Listening := True;
    end else
    begin
      ListenSocket.CloseSocket;
      WriteLn('Failed to Listen');
    end
  end;
end;

procedure TRPiCAN.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TRPiCAN;
begin
  Application := TRPiCAN.Create(nil);
  Application.Title := 'RPi CAN Hub';
  Application.Run;
  Application.Free;
end.

