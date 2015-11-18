program RPiCAN;

{$mode objfpc}{$H+}

{$DEFINE dsPIC33EP256GP502}
{$DEFINE dsPIC33EPxxxGP50x}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, lcc_raspberrypi, blcksock, synsock, baseUnix, sockets,
  contnrs, lcc_gridconnect, crt, intel_hex_parser
  { you can add units after this };

const
  LCC_MESSAGES_PER_SPI_PACKET = 8;
  BYTES_PER_LCC_GRIDCONNECT_MESSAGE = 30;
  BYTES_PER_SPI_PACKET = BYTES_PER_LCC_GRIDCONNECT_MESSAGE * LCC_MESSAGES_PER_SPI_PACKET;

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

  DELAY_BOOTLOADER_MS = 1;

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

  { TClientConnection }

  TClientConnection = class
  private
    FFiltered: Boolean;
    FGridConnectHelper: TGridConnectHelper;
    FRPiCAN: TRPiCAN;
    FSocket: TTCPBlockSocket;
    FInBuffer: TStringList;
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
    property RPiCAN: TRPiCAN read FRPiCAN write FRPiCAN;
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property XOn: Boolean read FXOn write SetXon;
  end;

  { TRPiClientConnection }

  TRPiClientConnection = class(TClientConnection)
  private
    FOutBuffer: TStringList;
    FRaspberryPiSpi: TRaspberryPiSpi;
  public
    constructor Create(ARPiCAN: TRPiCAN); override;
    destructor Destroy; override;
    procedure DispatchGridConnectMessages(GridConnectMsgList: TStringList); override;
    procedure ExtractSpiRxBuffer(var RxBuffer: TPiSpiBuffer; Count: Integer);
    function PollForIncomingMessage: Boolean; override;

    property OutBuffer: TStringList read FOutBuffer write FOutBuffer;
    property RaspberryPiSpi: TRaspberryPiSpi read FRaspberryPiSpi write FRaspberryPiSpi;
  end;

  { TRPiCAN }

  TRPiCAN = class(TCustomApplication)
  private
    FBootloading: Boolean;
    FClientConnections: TObjectList;
    FListening: Boolean;
    FListenSocket: TTCPBlockSocket;
    FRaspberryPiSpi: TRaspberryPiSpi;
    FVerbose: Boolean;
    function GetEthernetConnection(Index: Integer): TClientConnection;
    procedure SetEthenetConnection(Index: Integer; AValue: TClientConnection);
    procedure SetVerbose(AValue: Boolean);
  protected
    property ClientConnections: TObjectList read FClientConnections write FClientConnections;

    procedure CloseListenSocket;
    procedure CreateRaspberryPiClient;
    procedure DoRunBootloader;
    procedure DoCheckForNewClients;
    procedure DoPollClientInputs;
    procedure DoDispatchToClients;
    procedure DoRun; override;
    procedure OpenListenSocket;
    procedure WriteHelp; virtual;
  public
    property Bootloading: Boolean read FBootloading write FBootloading;
    property EthernetConnection[Index: Integer]: TClientConnection read GetEthernetConnection write SetEthenetConnection;
    property Listening: Boolean read FListening write FListening;
    property ListenSocket: TTCPBlockSocket read FListenSocket write FListenSocket;
    property RaspberryPiSpi: TRaspberryPiSpi read FRaspberryPiSpi write FRaspberryPiSpi;
    property Verbose: Boolean read FVerbose write SetVerbose;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  GlobalBufferNull: TPiSpiBuffer;

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

{ TRPiClientConnection }

constructor TRPiClientConnection.Create(ARPiCAN: TRPiCAN);
begin
  inherited Create(ARPiCAN);
  FOutBuffer := TStringList.Create;
end;

destructor TRPiClientConnection.Destroy;
begin
  FreeAndNil(FOutBuffer);
  inherited Destroy;
end;

procedure TRPiClientConnection.DispatchGridConnectMessages(GridConnectMsgList: TStringList);
var
  RxBuffer, TxBuffer: TPiSpiBuffer;
  iSpiPacket, iLccMessage, iChar, iTxBuffer, SpiPacketCount, iString: Integer;
  OutString: ansistring;
begin
  // Double buffer it so we can deal with the handshake
  for iString := 0 to GridConnectMsgList.Count - 1 do
    OutBuffer.Add(GridConnectMsgList[iString]);

  if XOn then
  begin
    // Calculate how many Spi Packet Bursts we need to send
    SpiPacketCount := OutBuffer.Count div LCC_MESSAGES_PER_SPI_PACKET;
    // Do we have a partial packet to send?
    if OutBuffer.Count mod LCC_MESSAGES_PER_SPI_PACKET <> 0 then
      SpiPacketCount := SpiPacketCount + 1;

    iString := 0;
    // Run through all the necessary Spi Packets to send all the messages
    for iSpiPacket := 0 to SpiPacketCount - 1 do
    begin
      iTxBuffer := 0;
      // Fill up the Spi Packet with as many Lcc Messages as will fit
      for iLccMessage := 0 to LCC_MESSAGES_PER_SPI_PACKET - 1 do
      begin
        // Grab the next string in the String List or a null string if we hit the end of the list
        if iString < OutBuffer.Count then
          OutString := OutBuffer[iString]
        else
          OutString := '';
        Inc(iString);

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
        Ord('R') : XOn := GridConnectStrPtr^[2] = Ord('0');
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
  Inherited;
  FRPiCAN := ARPiCAN;
  InBuffer := TStringList.Create;
  GridConnectHelper := TGridConnectHelper.Create;
  Filtered := False;
  XOn := True;
end;

destructor TClientConnection.Destroy;
begin
  FreeAndNil(FSocket);
  FreeAndNil(FInBuffer);
  FreeAndNil(FGridConnectHelper);
  inherited Destroy;
end;

procedure TClientConnection.DispatchGridConnectMessages(GridConnectMsgList: TStringList);
var
  iChar, iString: Integer;
begin
  if Socket.Socket <> INVALID_SOCKET then
    for iString := 0 to GridConnectMsgList.Count - 1 do
      if FilterMessage(GridConnectMsgList[iString]) then
      begin
        for iChar := 1 to Length(GridConnectMsgList[iString]) do
          Socket.SendByte( Ord(GridConnectMsgList[iString][iChar]));
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
    if XOn then WriteLn('XOn') else WriteLn('XOff);
end;

function TClientConnection.FilterMessage(GridConnectMsg: string): Boolean;
begin
  if Filtered then
  begin
    Result := True;
  end else
    Result := True;
end;

function TRPiCAN.GetEthernetConnection(Index: Integer): TClientConnection;
begin
  Result := TClientConnection( ClientConnections[Index]);
end;

procedure TRPiCAN.SetEthenetConnection(Index: Integer; AValue: TClientConnection);
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

{ TRPiCAN }

// ****************************************************************************
// Main message loop
// ***************************************************************************
procedure TRPiCAN.DoRun;
var
  ErrorMsg: String;
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

  if HasOption('c', 'clock') then begin
     GetOptionValue('c', 'clock');
  end;

  { add your program here }
  RaspberryPiSpi.Mode := psm_ClkIdleLo_DataFalling;
  RaspberryPiSpi.Speed := pss_976kHz;
  RaspberryPiSpi.Bits := psb_8;
  if RaspberryPiSpi.OpenSpi('/dev/spidev0.0') then
  begin
    WriteLn('Connected To Spi');

    OpenListenSocket;
    CreateRaspberryPiClient;
    if Listening then
    begin
      try
        repeat
          if Bootloading then
            DoRunBootloader
          else begin
            DoCheckForNewClients;
            DoPollClientInputs;
            DoDispatchToClients;
          end;

          if KeyPressed then
          begin
            case ReadKey of
              'v', 'V' : Verbose := not Verbose;
              'q', 'Q' : Terminate;
              'b', 'B' : BootLoading := True;
            end;
          end;

        until Terminated;
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
  ListenSocket := TTCPBlockSocket.Create;
  ClientConnections := TObjectList.Create;
end;

// ****************************************************************************
// Destroys the main object
// ***************************************************************************
destructor TRPiCAN.Destroy;
begin
  FreeAndNil(FClientConnections);
  FreeAndNil(FRaspberryPiSpi);
  FreeAndNil(FListenSocket);
  inherited Destroy;
end;

// ****************************************************************************
// Closes the listening socket
// ***************************************************************************
procedure TRPiCAN.CloseListenSocket;
begin
  if Listening then
    ListenSocket.CloseSocket;
end;

procedure TRPiCAN.CreateRaspberryPiClient;
var
  RaspberryPiConnection: TRPiClientConnection;
begin
  RaspberryPiConnection := TRPiClientConnection.Create;
  RaspberryPiConnection.RaspberryPiSpi := RaspberryPiSpi;
  ClientConnections.Add(RaspberryPiConnection);
end;

procedure TRPiCAN.DoRunBootloader;
var
  TxBuffer, RxBuffer: TPiSpiBuffer;
  FilePath: string;
  IgnoreAddressStart, IgnoreAddressEnd: DWord;
  RxOffset, i, j: Integer;
  IntelParser: TIntelHexParser;
  IntelHexInfo: TIntelHexInfo;
  IgnoreList: TIgnoreBoundsGroups;
  BootInfo: TBootInfo;
  Group: TIgnoreBoundsList;
begin
  FilePath := './EthernetCanHub.hex';
  WriteLn('Locating hex file: ' + FilePath);
  if FileExists(FilePath) then
  begin
     IntelParser := TIntelHexParser.Create;
     IgnoreList := TIgnoreBoundsGroups.Create;
     try
       WriteLn('Requsting bootloader mode');

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
       Delay(DELAY_BOOTLOADER_MS);
       RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 1);
       if RxBuffer[0] = CMD_LINK then
       begin
         WriteLn('Requesting microcontroller information');
         // Tell the micro to return information about the micro
         TxBuffer[0] := CMD_REQUEST_FLASH_DATA;
         Delay(DELAY_BOOTLOADER_MS);
         RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 1);

         // The micro must reply in this next packet block
         Delay(DELAY_BOOTLOADER_MS);
         RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 62);
         if RxBuffer[0] = CMD_REQUEST_FLASH_DATA then
         begin
           WriteLn('Microcontroller information returned');
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

           WriteLn('Structure Size: ' + IntToStr(BootInfo.StructSize));
           WriteLn('Erase Block Size (bytes): ' + IntToStr(BootInfo.EraseBlockSize) + ' [0x' + IntToHex(BootInfo.EraseBlockSize, 8) + ']');
           WriteLn('Write Block Size (bytes): ' + IntToStr(BootInfo.WriteBufferSize) + ' [0x' + IntToHex(BootInfo.WriteBufferSize, 8) + ']');
           WriteLn('Flash Size (bytes): ' + IntToStr(BootInfo.ProgramFlashSize) + ' [0x' + IntToHex(BootInfo.ProgramFlashSize, 8) + ']' + ' [0x' + IntToHex(BootInfo.ProgramFlashSize div IntelHexInfo.BytesPerInstruction * IntelHexInfo.AddressIncrement, 8) + ' User Addresses]');
           WriteLn('Bootloader Address: 0x' + IntToHex(BootInfo.BootloaderAddress, 8));
           WriteLn('Bootloader Size (bytes): ' + IntToStr(BootInfo.BootloaderSize) + ' [0x' + IntToHex(BootInfo.BootloaderSize, 8) + ']');
           WriteLn('Configruation Address: 0x' + IntToHex(BootInfo.ConfigurationAdddress, 8));
           WriteLn('McuFamily: ' + IntToStr(BootInfo.McuFamily));
           WriteLn('Revision: ' + IntToStr(BootInfo.Revision[1]) + '.' + IntToStr(BootInfo.Revision[0]));
           WriteLn('App Name: ' + BootInfo.ApplicationName);

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
             WriteLn('Ignoring from: 0x' + IntToHex(IgnoreAddressStart, 8) + ' to: 0x' + IntToHex(IgnoreAddressEnd, 8));
             Group := IgnoreList.AddGroup;
             Group.AddBound(IgnoreAddressStart, IgnoreAddressEnd);
           end;
           WriteLn('Parsing Intel HEX file');
           if IntelParser.ParseHex(FilePath, IntelHexInfo, BootInfo.EraseBlockSize, BootInfo.WriteBufferSize, 1, IgnoreList, False) then
           begin
             WriteLn('File parsed');

             WriteLn('Building Write Block List');
             if IntelParser.BuildWriteBlockList = ERROR_WRITE_BLOCK_OK then
             begin
               WriteLn('Write block lists built');
               WriteLn('Building Erase Block List');
               if IntelParser.BuildEraseBlockList = ERROR_ERASE_BLOCK_OK then
               begin
                 WriteLn('Erase block lists built');

                 for i := 0 to IntelParser.PhysicalEraseBlockList.Count - 1 do
                 begin
                   WriteLn('Setting Erase Address: ' + IntToHex(IntelParser.PhysicalEraseBlockList[i].AddressStart, 8));
                   TxBuffer[0] := CMD_SET_ADDRESS;
                   TxBuffer[1] := CMD_SET_ADDRESS_ERASE;
                   TxBuffer[2] := Highest( IntelParser.PhysicalEraseBlockList[i].AddressStart);
                   TxBuffer[3] := Higher( IntelParser.PhysicalEraseBlockList[i].AddressStart);
                   TxBuffer[4] := High( IntelParser.PhysicalEraseBlockList[i].AddressStart);
                   TxBuffer[5] := Low( IntelParser.PhysicalEraseBlockList[i].AddressStart);
                   RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 6);

                   WriteLn('Erasing: ' + IntToStr(IntelParser.PhysicalEraseBlockList[i].BlockCount) + ' blocks');
                   TxBuffer[0] := CMD_ERASE_BLOCKS;
                   TxBuffer[1] := Highest( IntelParser.PhysicalEraseBlockList[i].BlockCount);
                   TxBuffer[2] := Higher( IntelParser.PhysicalEraseBlockList[i].BlockCount);
                   TxBuffer[3] := High( IntelParser.PhysicalEraseBlockList[i].BlockCount);
                   TxBuffer[4] := Low( IntelParser.PhysicalEraseBlockList[i].BlockCount);
                   RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 5);

                   // The micro must reply in the next packet block
                   // Now we are bit banging the SPI so the count can be variable
                   repeat
                     Delay(DELAY_BOOTLOADER_MS);
                     RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 1);
                   until RxBuffer[0] = CMD_ERASE_BLOCKS;
                 end;

                 for i := 0 to IntelParser.PhysicalWriteBlockList.Count - 1 do
                 begin
                   WriteLn('Setting Write Address: ' + IntToHex(IntelParser.PhysicalWriteBlockList[i].AddressStart, 8));
                   TxBuffer[0] := CMD_SET_ADDRESS;
                   TxBuffer[1] := CMD_SET_ADDRESS_WRITE;
                   TxBuffer[2] := Highest( IntelParser.PhysicalWriteBlockList[i].AddressStart);
                   TxBuffer[3] := Higher( IntelParser.PhysicalWriteBlockList[i].AddressStart);
                   TxBuffer[4] := High( IntelParser.PhysicalWriteBlockList[i].AddressStart);
                   TxBuffer[5] := Low( IntelParser.PhysicalWriteBlockList[i].AddressStart);
                   RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 6);

                   if IntelParser.PhysicalWriteBlockList[i].ByteCount = BootInfo.WriteBufferSize then
                   begin
                     WriteLn('Writing Block: ' + IntToStr(IntelParser.PhysicalWriteBlockList[i].ByteCount) + ' bytes');
                     TxBuffer[0] := CMD_WRITE_BLOCK;
                     for j := 0 to BootInfo.WriteBufferSize - 1 do
                       TxBuffer[j+1] := IntelParser.PhysicalWriteBlockList[i].DataBlock[j];
                     RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, BootInfo.WriteBufferSize + 1);
                     // The micro must reply in the next packet block
                     // Now we are bit banging the SPI so the count can be variable
                     repeat
                       Delay(DELAY_BOOTLOADER_MS);
                       RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 1);
                     until RxBuffer[0] = CMD_WRITE_BLOCK;
                   end else
                   begin
                     WriteLn('Writing: ' + IntToStr(IntelParser.PhysicalWriteBlockList[i].ByteCount) + ' bytes');
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
                       RaspberryPiSpi.Transfer(@GlobalBufferNull, @RxBuffer, 1);
                     until RxBuffer[0] = CMD_WRITE;
                   end;
                 end;
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
    WriteLn('Unable to locate hex file: ' + FilePath);
  Bootloading := False;
end;

// ****************************************************************************
// Looks for new Ethernet Clients that want to connect to the RPi
// ***************************************************************************
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
      ClientConnections.Delete(iClient);
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

// ****************************************************************************
// Prints out the help
// ***************************************************************************
procedure TRPiCAN.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

// ****************************************************************************
// Main
// ***************************************************************************
var
  Application: TRPiCAN;
  i: Integer;
begin
  for i := 0 to Length(GlobalBufferNull) - 1 do GlobalBufferNull[i] := $00;
  Application := TRPiCAN.Create(nil);
  Application.Title := 'RPi CAN Hub';
  Application.Run;
  Application.Free;
end.

