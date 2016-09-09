unit NodeConnections;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Winsock2,
  UltiboClasses,
  lcc_gridconnect,
  lcc_can_message_assembler_disassembler,
  lcc_defines,
  lcc_utilities,
  lcc_messages;

const
  CMD_LINK               = $EF;
  CMD_UNLINK             = $EE;
  CMD_SYNC               = $ED;
  CMD_REQUEST_FLASH_DATA = $80;
  CMD_SET_ADDRESS        = $02;
  CMD_RESET              = $81;
  CMD_ERASE_BLOCKS       = $10;
  CMD_WRITE_BLOCK        = $30;
  CMD_WRITE              = $31;
  CMD_PARAMETERLESS_MASK = $80;

  CMD_SET_ADDRESS_WRITE  = $00;
  CMD_SET_ADDRESS_ERASE  = $01;

  DELAY_BOOTLOADER_MS = 30;

  LCC_MESSAGES_PER_SPI_PACKET = 8;
  BYTES_PER_LCC_GRIDCONNECT_MESSAGE = 30;
  BYTES_PER_SPI_PACKET = BYTES_PER_LCC_GRIDCONNECT_MESSAGE * LCC_MESSAGES_PER_SPI_PACKET;

type
  TPiSpiBuffer = array[0..BYTES_PER_SPI_PACKET-1] of Byte;

  (*
type
  TRevision = array[0..1] of Byte;
type
  TBootInfo = record
    StructSize: Word;
    EraseBlockSize: DWord;
    WriteBufferSize: DWord;
    ProgramFlashSize: DWord;
    BootloaderAddress: DWord;
    BootloaderSize : DWord;
    ConfigurationAdddress: DWord;
    McuFamily: Byte;
    Revision: TRevision;
    ApplicationName: string;
  end;

type

  TRPiCAN = class;

  TRaspberryPiSpi = class

  end;

  { TClientConnection }

  TClientConnection = class
  private
    FFiltered: Boolean;
    FGridConnectHelper: TGridConnectHelper;
    FRPiCAN: TRPiCAN;
    FSocket: TWinsock2TCPSocket;
    FInBuffer: TStringList;
    FOutBuffer: TStringList;
    FWorkerMessage: TLccMessage;
    FXOn: Boolean;
    procedure SetXon(AValue: Boolean);
  public
    constructor Create(ARPiCAN: TRPiCAN); virtual;
    destructor Destroy; override;
    function FilterMessage(GridConnectMsg: string): Boolean;
    procedure DispatchGridConnectMessages(GridConnectMsgList: TStringList); virtual;
    function PollForIncomingMessage: Boolean; virtual;

    property Filtered: Boolean read FFiltered write FFiltered;
    property GridConnectHelper: TGridConnectHelper read FGridConnectHelper write FGridConnectHelper;
    property InBuffer: TStringList read FInBuffer write FInBuffer;
    property OutBuffer: TStringList read FOutBuffer write FOutBuffer;
    property RPiCAN: TRPiCAN read FRPiCAN write FRPiCAN;
    property Socket: TWinsock2TCPSocket read FSocket write FSocket;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property XOn: Boolean read FXOn write SetXon;
  end;

  { TRPiClientConnection }

  TRPiClientConnection = class(TClientConnection)
  private
    FRaspberryPiSpi: TRaspberryPiSpi;
  public
    procedure DispatchGridConnectMessages(GridConnectMsgList: TStringList); override;
    procedure ExtractSpiRxBuffer(var RxBuffer: TPiSpiBuffer; Count: Integer);
    function PollForIncomingMessage: Boolean; override;

    property RaspberryPiSpi: TRaspberryPiSpi read FRaspberryPiSpi write FRaspberryPiSpi;
  end;

  { TVirtualNodeConnection }

  TVirtualNodeConnection = class(TClientConnection)
  private
    FGridConnectAssembler: TLccMessageAssembler;
    FGridConnectDisassembler: TLccMessageDisAssembler;
  //  FVirtualNode: TLccVirtualNode;
  protected
    property GridConnectAssembler: TLccMessageAssembler read FGridConnectAssembler write FGridConnectAssembler;
    property GridConnectDisassembler: TLccMessageDisAssembler read FGridConnectDisassembler write FGridConnectDisassembler;
    procedure DoRequestSendMessage(Sender: TObject; LccMessage: TLccMessage);
  public
    constructor Create(ARPiCAN: TRPiCAN); override;
    destructor Destroy; override;

    procedure DispatchGridConnectMessages(GridConnectMsgList: TStringList); override;
    function PollForIncomingMessage: Boolean; override;
//    property VirtualNode: TLccVirtualNode read FVirtualNode write FVirtualNode;
  end;

  { TRPiCAN }

  TRPiCAN = class
  private
    FBootloadHexPath: string;
    FVirtualNodeConnection: TVirtualNodeConnection;
    FClientConnections: TObjList;
    FVirtualNodeOpened: Boolean;
    FHubOnly: Boolean;
    FRaspberryPiSpiConnection: TRPiClientConnection;
    FRaspberryPiSpiOpened: Boolean;
    FSpiDevicePath: string;
    FListeningSocketOpened: Boolean;
    FListenSocket: TWinsock2TCPSocket;
    FRaspberryPiSpi: TRaspberryPiSpi;
    FVerbose: Boolean;
    FWorkerGridConnectStrings: TStringList;
    function GetConnection(Index: Integer): TClientConnection;
    procedure SetConnection(Index: Integer; AValue: TClientConnection);
    procedure SetVerbose(AValue: Boolean);
  protected
    property ClientConnections: TObjList read FClientConnections write FClientConnections;
    property WorkerGridConnectStrings: TStringList read FWorkerGridConnectStrings write FWorkerGridConnectStrings;

    procedure CloseVirtualNodeConnection;
    procedure CloseListenSocket;
    procedure CloseRaspberryPiSpiConnection;
    procedure DoRunBootloader;
    procedure DoCheckForNewClients;
    procedure DoPollClientInputs;
    procedure DoDispatchToClients;
    procedure DoRun;
    procedure OpenVirtualNodeSocketAndCreateClientConnection;
    procedure OpenListenSocket;
    procedure OpenRaspberryPiSpiAndCreateClientConnection;
    procedure WriteHelp; virtual;
  public
    property BootloadHexPath: string read FBootloadHexPath write FBootloadHexPath;
    property VirtualNodeConnection: TVirtualNodeConnection read FVirtualNodeConnection write FVirtualNodeConnection;
    property VirtualNodeOpened: Boolean read FVirtualNodeOpened write FVirtualNodeOpened;
    property Connection[Index: Integer]: TClientConnection read GetConnection write SetConnection;
    property HubOnly: Boolean read FHubOnly write FHubOnly;
    property ListenSocket: TWinsock2TCPSocket read FListenSocket write FListenSocket;
    property ListeningSocketOpened: Boolean read FListeningSocketOpened write FListeningSocketOpened;
    property RaspberryPiSpi: TRaspberryPiSpi read FRaspberryPiSpi write FRaspberryPiSpi;
    property RaspberryPiSpiConnection: TRPiClientConnection read FRaspberryPiSpiConnection write FRaspberryPiSpiConnection;
    property RaspberryPiSpiOpened: Boolean read FRaspberryPiSpiOpened write FRaspberryPiSpiOpened;
    property SpiDevicePath: string read FSpiDevicePath write FSpiDevicePath;
    property Verbose: Boolean read FVerbose write SetVerbose;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLccPiCanNode }
//  TLccPiCanNode = class( TLccVirtualNode)
//  public
//    constructor Create(AnOwner: TComponent); override;
//  end;

var
  GlobalBufferNull: TPiSpiBuffer;
         *)
implementation

(*
function Low(X: DWord): Byte;
begin
Result := Byte(X and $000000FF);
end;

function High(X: DWord): Byte;
begin
Result := Byte((X shr 8) and $000000FF);
end;

function Higher(X: DWord): Byte;
begin
Result := Byte((X shr 16) and $000000FF);
end;

function Highest(X: DWord): Byte;
begin
Result := Byte((X shr 24) and $000000FF);
end;


{ TLccPiCanNode }
 {
constructor TLccPiCanNode.Create(AnOwner: TComponent);
begin
inherited Create(AnOwner);
EventsConsumed.AutoGenerate.Count := 16;
EventsConsumed.AutoGenerate.Enable := True;
EventsProduced.AutoGenerate.Count := 16;
EventsProduced.AutoGenerate.Enable := True;
end;  }

{ TVirtualNodeConnection }

procedure TVirtualNodeConnection.DoRequestSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
OutBuffer.AddText( GridConnectDisassembler.OutgoingMsgToGridConnect(LccMessage));
end;

constructor TVirtualNodeConnection.Create(ARPiCAN: TRPiCAN);
begin
inherited Create(ARPiCAN);
FGridConnectAssembler := TLccMessageAssembler.Create;
FGridConnectDisassembler := TLccMessageDisAssembler.Create;
//FVirtualNode := TLccPiCanNode.Create(nil);
//FVirtualNode.OnRequestSendMessage := @DoRequestSendMessage;
OutBuffer.Delimiter := #10;
end;

destructor TVirtualNodeConnection.Destroy;
begin
//VirtualNode.OnRequestSendMessage := nil;
//FreeAndNil(FVirtualNode);
FreeAndNil(FGridConnectAssembler);
FreeAndNil(FGridConnectDisassembler);
inherited Destroy;
end;

procedure TVirtualNodeConnection.DispatchGridConnectMessages(GridConnectMsgList: TStringList);
var
i: Integer;
begin
for i := 0 to GridConnectMsgList.Count - 1 do
begin
  case GridConnectAssembler.IncomingMessageGridConnect(GridConnectMsgList[i], WorkerMessage{, VirtualNode.AliasID}) of
    imgcr_False        : begin end;
 //   imgcr_True         : VirtualNode.ProcessMessage(WorkerMessage);
    imgcr_ErrorToSend  : OutBuffer.Add(WorkerMessage.ConvertToGridConnectStr(#10));
    imgcr_UnknownError : WriteLn('Unexpected error converting incoming GridConnect string');
  end;
end;
end;

function TVirtualNodeConnection.PollForIncomingMessage: Boolean;
var
i: Integer;
begin
Result := True;
for i := 0 to OutBuffer.Count - 1 do
  InBuffer.Add(OutBuffer[i]);
OutBuffer.Clear;
end;

{ TRPiClientConnection }

procedure TRPiClientConnection.DispatchGridConnectMessages(GridConnectMsgList: TStringList);
var
RxBuffer, TxBuffer: TPiSpiBuffer;
iSpiPacket, iLccMessage, iChar, iTxBuffer, SpiPacketCount, i: Integer;
OutString: ansistring;
begin
// Double buffer it so we can deal with the handshake
for i := 0 to GridConnectMsgList.Count - 1 do
  OutBuffer.Add(GridConnectMsgList[i]);

if XOn then
begin
  // Calculate how many Spi Packet Bursts we need to send
  SpiPacketCount := OutBuffer.Count div LCC_MESSAGES_PER_SPI_PACKET;
  // Do we have a partial packet to send?
  if OutBuffer.Count mod LCC_MESSAGES_PER_SPI_PACKET <> 0 then
    SpiPacketCount := SpiPacketCount + 1;

  // Run through all the necessary Spi Packets to send all the messages
  for iSpiPacket := 0 to SpiPacketCount - 1 do
  begin
    // The micro could have signaled us to stop when we sent the last packet
    if XOn then
    begin
      iTxBuffer := 0;
      // Fill up the Spi Packet with as many Lcc Messages as will fit
      for iLccMessage := 0 to LCC_MESSAGES_PER_SPI_PACKET - 1 do
      begin
        // Grab the next string in the String List or a null string if we hit the end of the list
        if OutBuffer.Count > 0 then
        begin
          OutString := OutBuffer[0];
          OutBuffer.Delete(0);          // Painfully slow but anyother way is very complex
        end else
          OutString := '';

        // Move the string into the Spi Transmit Buffer or fill it with nulls
        iChar := 0;
        while iChar < BYTES_PER_LCC_GRIDCONNECT_MESSAGE do
        begin
          if iChar < Length(OutString) then
            TxBuffer[iTxBuffer] := Ord( OutString[iChar + 1])
          else
            TxBuffer[iTxBuffer] := $00 ;
          Inc(iChar);
          Inc(iTxBuffer);
        end;
      end;
      // Transfer the packet
      RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, BYTES_PER_SPI_PACKET);
      // Data may have been sent back when the packet was transmitted
      ExtractSpiRxBuffer(RxBuffer, BYTES_PER_SPI_PACKET);
    end;
  end;
end;
end;

procedure TRPiClientConnection.ExtractSpiRxBuffer(var RxBuffer: TPiSpiBuffer; Count: Integer);
var
i: Integer;
GridConnectStrPtr: PGridConnectString;
MessageStr: ansistring;
begin
GridConnectStrPtr := nil;
for i := 0 to Count - 1 do
begin
  if GridConnectHelper.GridConnect_DecodeMachine(RxBuffer[i], GridConnectStrPtr) then
  begin
    case GridConnectStrPtr^[1] of
      Ord('X') : begin
                   MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                   InBuffer.Add(MessageStr);
                   if RPiCAN.Verbose then WriteLn(MessageStr);
                 end;
      Ord('R') : XOn := GridConnectStrPtr^[2] <> Ord('0');
      end;
    end;
  end;
end;

function TRPiClientConnection.PollForIncomingMessage: Boolean;
var
RxBuffer: TPiSpiBuffer;
begin
Result := True;
RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, BYTES_PER_SPI_PACKET);
ExtractSpiRxBuffer(RxBuffer, BYTES_PER_SPI_PACKET);
end;

constructor TClientConnection.Create(ARPiCAN: TRPiCAN);
begin
inherited Create;
FRPiCAN := ARPiCAN;
InBuffer := TStringList.Create;
InBuffer.Delimiter := #10;
GridConnectHelper := TGridConnectHelper.Create;
Filtered := False;
FXOn := True;
FOutBuffer := TStringList.Create;
WorkerMessage := TLccMessage.Create;
end;

destructor TClientConnection.Destroy;
begin
FreeAndNil(FSocket);
FreeAndNil(FInBuffer);
FreeAndNil(FGridConnectHelper);
FreeAndNil(FWorkerMessage);
FreeAndNil(FOutBuffer);
inherited Destroy;
end;

procedure TClientConnection.DispatchGridConnectMessages(GridConnectMsgList: TStringList);
var
iChar, iString: Integer;
TempStr: string;
begin
if Socket.Socket <> INVALID_SOCKET then
  for iString := 0 to GridConnectMsgList.Count - 1 do
  begin
    if FilterMessage(GridConnectMsgList[iString]) then
    begin
      TempStr := GridConnectMsgList[iString];
      for iChar := 1 to Length(TempStr) do
        Socket.SendByte( Ord(TempStr[iChar]));
      Socket.SendByte( Ord( #10));
    end;
  end;
end;

function TClientConnection.PollForIncomingMessage: Boolean;
var
RcvByte: Byte;
GridConnectStrPtr: PGridConnectString;
GridConnectStr: string;
Done: Boolean;
begin
Result := True;
Done := False;   // Pull all bytes from the sockets incoming buffer
repeat
  RcvByte := Socket.RecvByte(1);
  case Socket.LastError of
    0 :
      begin
        GridConnectStrPtr := nil;
        if GridConnectHelper.GridConnect_DecodeMachine(RcvByte, GridConnectStrPtr) then
        begin
          GridConnectStr := GridConnectBufferToString(GridConnectStrPtr^);
          InBuffer.Add(GridConnectStr);
          if RPiCAN.Verbose then
            WriteLn(GridConnectStr);
        end;
      end;
    WSAETIMEDOUT :
      begin
        Done := True;
      end;
    WSAECONNRESET   :
      begin
        WriteLn(Socket.LastErrorDesc + ',' + Socket.GetRemoteSinIP + ':' + IntToStr(Socket.GetRemoteSinPort));
        Done := True;
        Result := False; // Failure of the socket
      end;
  else begin
      WriteLn(Socket.LastErrorDesc + ',' + Socket.GetRemoteSinIP + ':' + IntToStr(Socket.GetRemoteSinPort));
      Done := True;
      Result := False; // Failure of the socket
    end;
  end;
until Done;
end;

procedure TClientConnection.SetXon(AValue: Boolean);
begin
if FXOn = AValue then Exit;
FXOn := AValue;
if RPiCAN.Verbose then
  if XOn then WriteLn('XOn') else WriteLn('XOff');
end;

function TClientConnection.FilterMessage(GridConnectMsg: string): Boolean;
begin
if Filtered then
begin
  Result := True;
end else
  Result := True;
end;

function TRPiCAN.GetConnection(Index: Integer): TClientConnection;
begin
Result := TClientConnection( ClientConnections[Index]);
end;

procedure TRPiCAN.SetConnection(Index: Integer; AValue: TClientConnection);
begin
ClientConnections[Index] := AValue;
end;

procedure TRPiCAN.SetVerbose(AValue: Boolean);
begin
if FVerbose = AValue then Exit;
FVerbose := AValue;
if FVerbose then
  WriteLn('Verbose on')
else
  WriteLn('Verbose off');
end;

procedure TRPiCAN.CloseVirtualNodeConnection;
begin
VirtualNodeOpened := False;
if Assigned(VirtualNodeConnection) then
  ClientConnections.Remove(VirtualNodeConnection);
VirtualNodeConnection := nil;
end;

{ TRPiCAN }

// ****************************************************************************
// Main message loop
// ***************************************************************************
procedure TRPiCAN.DoRun;
var
ErrorMsg: String;
LccMessage: TLccMessage;
i: Integer;
begin
// quick check parameters
ErrorMsg := CheckOptions('h b s v o', 'help bootfile spidriver verbose onlyhub');
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

FVerbose := HasOption('v', 'verbose');
FHubOnly := HasOption('o', 'onlyhub');

if HasOption('c', 'clock') then begin
   GetOptionValue('c', 'clock');
end;

if HasOption('b', 'bootfile') then
begin
  BootloadHexPath := GetOptionValue('b', 'bootfile');
  if FileExists(BootloadHexPath) then
   DoRunBootloader
  else begin
    ShowException(Exception.Create('Can not find bootloader HEX file: ' + BootloadHexPath + ' Usage: rpican -b myhexfile.hex'));
    Terminate;
    Exit;
  end;
end;

if HasOption('s', 'spidriver') then
begin
   if FileExists(GetOptionValue('s', 'spidriver')) then
     SpiDevicePath := GetOptionValue('s', 'spidriver');
end;

{ add your program here }
LccMessage := TLccMessage.Create;

OpenRaspberryPiSpiAndCreateClientConnection;
OpenVirtualNodeSocketAndCreateClientConnection;
OpenListenSocket;
if ListeningSocketOpened and Assigned(VirtualNodeConnection) and (RaspberryPiSpiOpened or HubOnly) then
begin
  try

    repeat
      DoCheckForNewClients;
      DoPollClientInputs;
      DoDispatchToClients;
      CheckSynchronize(0);

      if KeyPressed then
      begin
        case ReadKey of
          'l', 'L' : begin
                       if VirtualNodeConnection.VirtualNode.LoggedIn then
                       begin
                         VirtualNodeConnection.VirtualNode.Logout;
                         WriteLn('Node logged out');
                       end else
                       begin
                         VirtualNodeConnection.VirtualNode.Login(True, False);
                         WriteLn('Node logged in');
                         WriteLn(VirtualNodeConnection.VirtualNode.NodeIDStr + ':' + VirtualNodeConnection.VirtualNode.AliasIDStr);
                       end;
                     end;
          'v', 'V' : Verbose := not Verbose;
          'q', 'Q' : Terminate;
        end;
      end;

    until Terminated;
  finally
    DoDispatchToClients;
    CloseRaspberryPiSpiConnection;
    CloseVirtualNodeConnection;
    CloseListenSocket;
  end;
end;

LccMessage.Free;
// stop program loop
WriteLn('Shutting down');
Terminate;
end;

procedure TRPiCAN.OpenVirtualNodeSocketAndCreateClientConnection;
{var
IP: string;
i: Integer;
LocalConnection: TClientConnection;
LocalSocket: TWinsock2TCPSocket; }
begin
VirtualNodeConnection := TVirtualNodeConnection.Create(Self);
ClientConnections.Add(VirtualNodeConnection);


{
VirtualNodeOpened := False;
if ListeningSocketOpened then
begin
  WriteLn('Opening Client Socket');
  LocalSocket := TWinsock2TCPSocket.Create;          // Created in context of the thread
  LocalSocket.Family := SF_IP4;                  // IP4
  LocalSocket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  LocalSocket.HeartbeatRate := 0;
  LocalSocket.SetTimeout(0);
  WriteLn('Resolving IP Address');
  IP := ResolveUnixIp;
  WriteLn('IP Address: ' + IP);
  LocalSocket.Connect(IP, IntToStr(12021));  // Can we connect to ourselves?
  if LocalSocket.LastError = 0 then
  begin
    DoCheckForNewClients;                     // Get this connection in the Connection List

    for i := 0 to ClientConnections.Count - 1 do
    begin
      LocalConnection := TClientConnection( ClientConnections[i]);
      if Assigned(LocalConnection.Socket) then
      begin
        if (LocalConnection.Socket.GetRemoteSinIP = LocalSocket.GetLocalSinIP) and
          (LocalConnection.Socket.GetRemoteSinPort = LocalSocket.GetLocalSinPort) then
          begin
            VirtualNodeConnection := LocalConnection;
            VirtualNodeOpened := True
          end
      end
    end;
    WriteLn('Client Connected')
  end
  else begin
    LocalSocket.CloseSocket;
    WriteLn('Client Failed to Connect');
  end;
end;   }
end;


// ****************************************************************************
// Create the main object
// ***************************************************************************
constructor TRPiCAN.Create(TheOwner: TComponent);
var
i: Integer;
begin
inherited Create(TheOwner);
StopOnException := True;
RaspberryPiSpi := TRaspberryPiSpi.Create;
ListenSocket := TWinsock2TCPSocket.Create;
ClientConnections := TObjectList.Create;
WorkerGridConnectStrings := TStringList.Create;
SpiDevicePath := '/dev/spidev0.0';
HubOnly := False;
end;

// ****************************************************************************
// Destroys the main object
// ***************************************************************************
destructor TRPiCAN.Destroy;
begin
FreeAndNil(FClientConnections);
FreeAndNil(FRaspberryPiSpi);
FreeAndNil(FListenSocket);
FreeAndNil(FWorkerGridConnectStrings);
inherited Destroy;
end;

// ****************************************************************************
// Closes the listening socket
// ***************************************************************************
procedure TRPiCAN.CloseListenSocket;
begin
if ListeningSocketOpened then
  ListenSocket.CloseSocket;
ListeningSocketOpened := False;
end;

procedure TRPiCAN.CloseRaspberryPiSpiConnection;
begin
if RaspberryPiSpiOpened then
  RaspberryPiSpi.CloseSpi;
if Assigned(RaspberryPiSpiConnection) then
  ClientConnections.Remove(RaspberryPiSpiConnection);
RaspberryPiSpiOpened := False;
end;


procedure TRPiCAN.DoRunBootloader;
var
TxBuffer, RxBuffer: TPiSpiBuffer;
IgnoreAddressStart, IgnoreAddressEnd: DWord;
RxOffset, i, j: Integer;
IntelParser: TIntelHexParser;
IntelHexInfo: TIntelHexInfo;
IgnoreList: TIgnoreBoundsGroups;
BootInfo: TBootInfo;
Group: TIgnoreBoundsList;
begin
try
  WriteLn('Entering Booloader Mode');
  if Verbose then WriteLn('Locating hex file: ' + BootloadHexPath);
  if FileExists(BootloadHexPath) then
  begin
     IntelParser := TIntelHexParser.Create;
     IgnoreList := TIgnoreBoundsGroups.Create;
     try
       if Verbose then WriteLn('Requsting bootloader mode');

       // Tell the micro to go into boot loader mode
       RaspberryPiSpi.ZeroBuffer(@TxBuffer, BYTES_PER_SPI_PACKET);
       TxBuffer[0] := Ord(':');
       TxBuffer[1] := Ord('B');
       RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, BYTES_PER_SPI_PACKET);
       Delay(DELAY_BOOTLOADER_MS);

       // Tell the micro to get ready for bootloader syncronization
       TxBuffer[0] := CMD_SYNC;
       RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 1);
       Delay(DELAY_BOOTLOADER_MS);

       // The micro must reply in the next packet block
       // Now we are bit banging the SPI so the count can be variable
       RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 2);
       if RxBuffer[0] = CMD_LINK then
       begin
         if Verbose then WriteLn('Requesting microcontroller information');
         // Tell the micro to return information about the micro
         TxBuffer[0] := CMD_REQUEST_FLASH_DATA;
         Delay(DELAY_BOOTLOADER_MS);
         RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 1);

         // The micro must reply in this next packet block
         Delay(DELAY_BOOTLOADER_MS);
         RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 62);
         if RxBuffer[0] = CMD_REQUEST_FLASH_DATA then
         begin
           if Verbose then WriteLn('Microcontroller information returned');
           RxOffset := 1;
           BootInfo.StructSize := (RxBuffer[RxOffset] shl 8) or RxBuffer[RxOffset+1];
           Inc(RxOffset, 2);
           BootInfo.EraseBlockSize := (RxBuffer[RxOffset] shl 24) or (RxBuffer[RxOffset+1] shl 16) or (RxBuffer[RxOffset+2] shl 8) or RxBuffer[RxOffset+3];
           Inc(RxOffset, 4);
           BootInfo.WriteBufferSize := (RxBuffer[RxOffset] shl 24) or (RxBuffer[RxOffset+1] shl 16) or (RxBuffer[RxOffset+2] shl 8) or RxBuffer[RxOffset+3];
           Inc(RxOffset, 4);
           BootInfo.ProgramFlashSize := (RxBuffer[RxOffset] shl 24) or (RxBuffer[RxOffset+1] shl 16) or (RxBuffer[RxOffset+2] shl 8) or RxBuffer[RxOffset+3];
           Inc(RxOffset, 4);
           BootInfo.BootloaderAddress := (RxBuffer[RxOffset] shl 24) or (RxBuffer[RxOffset+1] shl 16) or (RxBuffer[RxOffset+2] shl 8) or RxBuffer[RxOffset+3];
           Inc(RxOffset, 4);
           BootInfo.BootloaderSize := (RxBuffer[RxOffset] shl 24) or (RxBuffer[RxOffset+1] shl 16) or (RxBuffer[RxOffset+2] shl 8) or RxBuffer[RxOffset+3];
           Inc(RxOffset, 4);
           BootInfo.ConfigurationAdddress := (RxBuffer[RxOffset] shl 24) or (RxBuffer[RxOffset+1] shl 16) or (RxBuffer[RxOffset+2] shl 8) or RxBuffer[RxOffset+3];
           Inc(RxOffset, 4);
           BootInfo.McuFamily := RxBuffer[RxOffset];
           Inc(RxOffset);
           BootInfo.Revision[0] := RxBuffer[RxOffset];
           Inc(RxOffset);
           BootInfo.Revision[1] := RxBuffer[RxOffset];
           Inc(RxOffset);
           BootInfo.ApplicationName := ansistring( PChar( @RxBuffer[RxOffset]));

           IntelHexInfo.AddressIncrement := 2;
           IntelHexInfo.BytesPerInstruction := 3;
           IntelHexInfo.DoubleAddress := True;
           IntelHexInfo.HexType := iht_INH24_dsPIC33;

           if Verbose then WriteLn('Structure Size: ' + IntToStr(BootInfo.StructSize));
           if Verbose then WriteLn('Erase Block Size (bytes): ' + IntToStr(BootInfo.EraseBlockSize) + ' [0x' + IntToHex(BootInfo.EraseBlockSize, 8) + ']');
           if Verbose then  WriteLn('Write Block Size (bytes): ' + IntToStr(BootInfo.WriteBufferSize) + ' [0x' + IntToHex(BootInfo.WriteBufferSize, 8) + ']');
           if Verbose then WriteLn('Flash Size (bytes): ' + IntToStr(BootInfo.ProgramFlashSize) + ' [0x' + IntToHex(BootInfo.ProgramFlashSize, 8) + ']' + ' [0x' + IntToHex(BootInfo.ProgramFlashSize div IntelHexInfo.BytesPerInstruction * IntelHexInfo.AddressIncrement, 8) + ' User Addresses]');
           if Verbose then WriteLn('Bootloader Address: 0x' + IntToHex(BootInfo.BootloaderAddress, 8));
           if Verbose then WriteLn('Bootloader Size (bytes): ' + IntToStr(BootInfo.BootloaderSize) + ' [0x' + IntToHex(BootInfo.BootloaderSize, 8) + ']');
           if Verbose then WriteLn('Configruation Address: 0x' + IntToHex(BootInfo.ConfigurationAdddress, 8));
           if Verbose then WriteLn('McuFamily: ' + IntToStr(BootInfo.McuFamily));
           if Verbose then WriteLn('Revision: ' + IntToStr(BootInfo.Revision[1]) + '.' + IntToStr(BootInfo.Revision[0]));
           if Verbose then WriteLn('App Name: ' + BootInfo.ApplicationName);

           // Add an ignore space where the bootloader is so we don't overwrite it
           IgnoreAddressStart := BootInfo.BootloaderAddress;
           IgnoreAddressEnd := BootInfo.BootloaderAddress + (BootInfo.BootloaderSize div IntelHexInfo.BytesPerInstruction * IntelHexInfo.AddressIncrement);
           WriteLn('Ignoring from: 0x' + IntToHex(IgnoreAddressStart, 8) + ' to: 0x' + IntToHex(IgnoreAddressEnd, 8));
           Group := IgnoreList.AddGroup;
           Group.AddBound(IgnoreAddressStart, IgnoreAddressEnd);
           if BootInfo.ConfigurationAdddress > 0 then
           begin
             IgnoreAddressStart := BootInfo.ConfigurationAdddress;
             IgnoreAddressEnd := BootInfo.ConfigurationAdddress + (BootInfo.EraseBlockSize div IntelHexInfo.BytesPerInstruction * IntelHexInfo.AddressIncrement);
             if Verbose then WriteLn('Ignoring from: 0x' + IntToHex(IgnoreAddressStart, 8) + ' to: 0x' + IntToHex(IgnoreAddressEnd, 8));
             Group := IgnoreList.AddGroup;
             Group.AddBound(IgnoreAddressStart, IgnoreAddressEnd);
           end;
           if Verbose then WriteLn('Parsing Intel HEX file');
           if IntelParser.ParseHex(BootloadHexPath, IntelHexInfo, BootInfo.EraseBlockSize, BootInfo.WriteBufferSize, 1, IgnoreList, False) then
           begin
             if Verbose then WriteLn('File parsed');

       {      for i := 0 to IntelParser.PhysicalAddressBlockList.Count - 1 do
             begin
               WriteLn('Start: 0x' + IntToHex( IntelParser.PhysicalAddressBlockList[i].AddressStart, 8));
               WriteLn('End  : 0x' + IntToHex( IntelParser.PhysicalAddressBlockList[i].AddressLast, 8));
             end; }

             if Verbose then WriteLn('Building Write Block List');
             if IntelParser.BuildWriteBlockList = ERROR_WRITE_BLOCK_OK then
             begin
               if Verbose then WriteLn('Write block lists built');
               if Verbose then WriteLn('Building Erase Block List');
               if IntelParser.BuildEraseBlockList = ERROR_ERASE_BLOCK_OK then
               begin
                 if Verbose then WriteLn('Erase block lists built');

                 for i := 0 to IntelParser.PhysicalEraseBlockList.Count - 1 do
                 begin
                   if Verbose then WriteLn('Setting Erase Address: ' + IntToHex(IntelParser.PhysicalEraseBlockList[i].AddressStart, 8));
                   TxBuffer[0] := CMD_SET_ADDRESS;
                   TxBuffer[1] := CMD_SET_ADDRESS_ERASE;
                   TxBuffer[2] := Highest( IntelParser.PhysicalEraseBlockList[i].AddressStart);
                   TxBuffer[3] := Higher( IntelParser.PhysicalEraseBlockList[i].AddressStart);
                   TxBuffer[4] := High( IntelParser.PhysicalEraseBlockList[i].AddressStart);
                   TxBuffer[5] := Low( IntelParser.PhysicalEraseBlockList[i].AddressStart);
                   RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 6);

                   // No Reply defined

                   if Verbose then WriteLn('Erasing: ' + IntToStr(IntelParser.PhysicalEraseBlockList[i].BlockCount) + ' blocks');
                   TxBuffer[0] := CMD_ERASE_BLOCKS;
                   TxBuffer[1] := Highest( IntelParser.PhysicalEraseBlockList[i].BlockCount);
                   TxBuffer[2] := Higher( IntelParser.PhysicalEraseBlockList[i].BlockCount);
                   TxBuffer[3] := High( IntelParser.PhysicalEraseBlockList[i].BlockCount);
                   TxBuffer[4] := Low( IntelParser.PhysicalEraseBlockList[i].BlockCount);
                   RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 5);

                   // The micro must reply in the next packet block
                   // Now we are bit banging the SPI so the count can be variable
                   repeat
                     Delay(DELAY_BOOTLOADER_MS * 2 * IntelParser.PhysicalEraseBlockList[i].BlockCount);
                     RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 2);
                   until RxBuffer[0] = CMD_ERASE_BLOCKS;
                 end;

                 for i := 0 to IntelParser.PhysicalWriteBlockList.Count - 1 do
                 begin
                   if Verbose then WriteLn('Setting Write Address: ' + IntToHex(IntelParser.PhysicalWriteBlockList[i].AddressStart, 8));
                   TxBuffer[0] := CMD_SET_ADDRESS;
                   TxBuffer[1] := CMD_SET_ADDRESS_WRITE;
                   TxBuffer[2] := Highest( IntelParser.PhysicalWriteBlockList[i].AddressStart);
                   TxBuffer[3] := Higher( IntelParser.PhysicalWriteBlockList[i].AddressStart);
                   TxBuffer[4] := High( IntelParser.PhysicalWriteBlockList[i].AddressStart);
                   TxBuffer[5] := Low( IntelParser.PhysicalWriteBlockList[i].AddressStart);
                   RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 6);

                   // No Reply Defined

                   if IntelParser.PhysicalWriteBlockList[i].ByteCount = BootInfo.WriteBufferSize then
                   begin
                     if Verbose then WriteLn('Writing Block: ' + IntToStr(IntelParser.PhysicalWriteBlockList[i].ByteCount) + ' bytes');
                     TxBuffer[0] := CMD_WRITE_BLOCK;
                     for j := 0 to BootInfo.WriteBufferSize - 1 do
                       TxBuffer[j+1] := IntelParser.PhysicalWriteBlockList[i].DataBlock[j];
                     RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, BootInfo.WriteBufferSize + 1);
                     // The micro must reply in the next packet block
                     // Now we are bit banging the SPI so the count can be variable
                     repeat
                       Delay(DELAY_BOOTLOADER_MS);
                       RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 2);
                     until RxBuffer[0] = CMD_WRITE_BLOCK;
                   end else
                   begin
                     if Verbose then WriteLn('Writing: ' + IntToStr(IntelParser.PhysicalWriteBlockList[i].ByteCount) + ' bytes');
                     TxBuffer[0] := CMD_WRITE;
                     TxBuffer[1] := Highest( IntelParser.PhysicalWriteBlockList[i].ByteCount);
                     TxBuffer[2] := Higher( IntelParser.PhysicalWriteBlockList[i].ByteCount);
                     TxBuffer[3] := High( IntelParser.PhysicalWriteBlockList[i].ByteCount);
                     TxBuffer[4] := Low( IntelParser.PhysicalWriteBlockList[i].ByteCount);
                     for j := 0 to BootInfo.WriteBufferSize - 1 do
                       TxBuffer[j+5] := IntelParser.PhysicalWriteBlockList[i].DataBlock[j];
                     RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, IntelParser.PhysicalWriteBlockList[i].ByteCount + 5);
                     // The micro must reply in the next packet block
                     // Now we are bit banging the SPI so the count can be variable
                     repeat
                       Delay(DELAY_BOOTLOADER_MS);
                       RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 2);
                     until RxBuffer[0] = CMD_WRITE;
                   end;
                 end;
                 // Reboot the Microcontroller
                 TxBuffer[0] := CMD_RESET;
                 RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 1);
               end else
                  WriteLn('Failed to Build Erase Blocks');
             end else
               WriteLn('Failed to Build Write Blocks');
           end;
         end else
           WriteLn('Failed to access microcontroller info');
       end else
       begin
         WriteLn('Failed to enter bootloader mode');
       end;
     finally
       IntelParser.Free;
       IgnoreList.Free;
     end;
  end else
    WriteLn('Unable to locate hex file: ' + BootloadHexPath);
finally
  WriteLn('Exiting Booloader Mode');
end;
end;

// ****************************************************************************
// Looks for new Ethernet Clients that want to connect to the RPi
// ***************************************************************************
procedure TRPiCAN.DoCheckForNewClients;
var
LocalClientSocket: TWinsock2TCPSocket;
LocalClientConnection: TClientConnection;
begin
if ListenSocket.CanRead(1) then
begin
  if (ListenSocket.LastError <> WSAETIMEDOUT) and (ListenSocket.LastError = 0) then
  begin
    WriteLn('Client Connecting');
    LocalClientSocket := TWinsock2TCPSocket.Create;
    try
      LocalClientSocket.Family := SF_IP4;                  // IP4
      LocalClientSocket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
      LocalClientSocket.HeartbeatRate := 0;
      LocalClientSocket.SetTimeout(0);
      LocalClientSocket.Socket := ListenSocket.Accept;
      if LocalClientSocket.LastError = 0 then
      begin
        WriteLn('Client Connected: ' + LocalClientSocket.GetRemoteSinIP + ':' + IntToStr(LocalClientSocket.GetRemoteSinPort));
        LocalClientConnection := TClientConnection.Create(Self);
        try
          LocalClientConnection.Socket := LocalClientSocket;
          ClientConnections.Add(LocalClientConnection)
        except
          WriteLn('Failed to Listen');
          FreeAndNil(LocalClientSocket);
        end;
      end;
    except
      WriteLn('Failed to Listen');
      FreeAndNil(LocalClientSocket);
    end;
  end
end
end;

// ****************************************************************************
// Looks for incoming messages on the ethernet.  During this time it is safe to
// pump on the RPi SPI to ensure we flush out the microcontroller and save the
// messages coming from the SPI to the RPi Buffer
// ***************************************************************************
procedure TRPiCAN.DoPollClientInputs;
var
iClient: Integer;
begin
for iClient := ClientConnections.Count - 1 downto 0 do
begin
  if not TClientConnection( ClientConnections[iClient]).PollForIncomingMessage then
  begin
    DoDispatchToClients;
    ClientConnections.Delete(iClient);
  end;
end;
end;

procedure TRPiCAN.DoDispatchToClients;
var
iSourceConnection, iDestConnection: Integer;
begin
for iSourceConnection := 0 to ClientConnections.Count - 1 do
begin
  for iDestConnection := 0 to ClientConnections.Count - 1 do
    if  TClientConnection( ClientConnections[iDestConnection]) <> TClientConnection( ClientConnections[iSourceConnection]) then
      TClientConnection( ClientConnections[iDestConnection]).DispatchGridConnectMessages(TClientConnection( ClientConnections[iSourceConnection]).InBuffer);
  TClientConnection( ClientConnections[iSourceConnection]).InBuffer.Clear;
end;
end;

// ****************************************************************************
// Opens the Listening socket
// ***************************************************************************
procedure TRPiCAN.OpenListenSocket;
var
IP: string;
begin
WriteLn('Opening Listening Socket');
ListeningSocketOpened := False;
ListenSocket := TWinsock2TCPSocket.Create;          // Created in context of the thread
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
    ListeningSocketOpened := True;
  end else
  begin
    ListenSocket.CloseSocket;
    WriteLn('Failed to Listen');
  end
end;
end;

procedure TRPiCAN.OpenRaspberryPiSpiAndCreateClientConnection;
begin
RaspberryPiSpiConnection := nil;
RaspberryPiSpiOpened := False;
if not HubOnly then
begin
  RaspberryPiSpi.Mode := psm_ClkIdleLo_DataFalling;
  RaspberryPiSpi.Speed := pss_976kHz;
  RaspberryPiSpi.Bits := psb_8;
  RaspberryPiSpiOpened := RaspberryPiSpi.OpenSpi(SpiDevicePath);
  if RaspberryPiSpiOpened then
  begin
    RaspberryPiSpiConnection := TRPiClientConnection.Create(Self);
    RaspberryPiSpiConnection.RaspberryPiSpi := RaspberryPiSpi;
    ClientConnections.Add(RaspberryPiSpiConnection);
    WriteLn('Connected To Spi');
  end else
    WriteLn('Failed to open Spi');
end;
end;

// ****************************************************************************
// Prints out the help
// ***************************************************************************
procedure TRPiCAN.WriteHelp;
begin
{ add your help code here }
writeln('Usage: ', ExeName, ' -h; Display this help file');
WriteLn('  -b [-bootfile] {HEXfilepath.hex}; Update the the PiCan boards micro code with the passed file');
WriteLn('  -s [-spidriver] {spidriverpath}; Defines custom path for the spi driver');
WriteLn('  -v [-verbose] Displays detailed info in the command window, off is default');
WriteLn('  -h [-hub] Runs only as a hub, does not interact with the Spi hardware');
end;



initialization
  for i := 0 to Length(GlobalBufferNull) - 1 do
    GlobalBufferNull[i] := $00;
finalization
      *)
end.

