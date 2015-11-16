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
  GRIDCONNECT_STR_COUNT = 8;
  GRIDCONNECT_CHAR_COUNT = 30;
  MAX_RAWBUFFER_BYTES = GRIDCONNECT_CHAR_COUNT * GRIDCONNECT_STR_COUNT;

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

  { TClientConnection }

  TClientConnection = class
  private
    FGridConnectHelper: TGridConnectHelper;
    FSocket: TTCPBlockSocket;
    FInBuffer: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property GridConnectHelper: TGridConnectHelper read FGridConnectHelper write FGridConnectHelper;
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property InBuffer: TStringList read FInBuffer write FInBuffer;
  end;

  { TRPiCAN }

  TRPiCAN = class(TCustomApplication)
  private
    FBootloading: Boolean;
    FClientConnections: TObjectList;
    FGridConnectHelper: TGridConnectHelper;
    FGridConnectHelperRPi: TGridConnectHelper;
    FListening: Boolean;
    FListenSocket: TTCPBlockSocket;
    FRaspberryPiInBuffer: TStringList;
    FRaspberryPiSpi: TRaspberryPiSpi;
    FSocketHandleForListener: TSocket;
    FTxBufferNull: TPiSpiBuffer;
    FVerbose: Boolean;
    function GetClientConnection(Index: Integer): TClientConnection;
    procedure SetClientConnection(Index: Integer; AValue: TClientConnection);
    procedure SetVerbose(AValue: Boolean);
  protected
    property TxBufferNull: TPiSpiBuffer read FTxBufferNull write FTxBufferNull;
    property ClientConnections: TObjectList read FClientConnections write FClientConnections;
    procedure DoRun; override;
    procedure ExtractSpiRxBuffer(var RxBuffer: TPiSpiBuffer; Count: Integer);
  public
    property Bootloading: Boolean read FBootloading write FBootloading;
    property ClientConnection[Index: Integer]: TClientConnection read GetClientConnection write SetClientConnection;
    property GridConnectHelper: TGridConnectHelper read FGridConnectHelper write FGridConnectHelper;
    property GridConnectHelperRPi: TGridConnectHelper read FGridConnectHelperRPi write FGridConnectHelperRPi;
    property Listening: Boolean read FListening write FListening;
    property ListenSocket: TTCPBlockSocket read FListenSocket write FListenSocket;
    property RaspberryPiSpi: TRaspberryPiSpi read FRaspberryPiSpi write FRaspberryPiSpi;
    property SocketHandleForListener: TSocket read FSocketHandleForListener write FSocketHandleForListener;
    property RaspberryPiInBuffer: TStringList read FRaspberryPiInBuffer write FRaspberryPiInBuffer;
    property Verbose: Boolean read FVerbose write SetVerbose;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure CloseListenSocket;
    procedure DoBootLoad;
    procedure DoCheckForNewClients;
    procedure DoCheckEthernetClientsForIncomingMessages;
    procedure DoDispatchToEthernetClientsAndClearMessages;
    procedure DoDispatchToRaspberryPi;
    procedure DoCheckRaspberryPiForIncomingMessages;
    procedure OpenListenSocket;
    procedure WriteHelp; virtual;
  end;

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

constructor TClientConnection.Create;
begin
  Inherited;
  InBuffer := TStringList.Create;
  GridConnectHelper := TGridConnectHelper.Create;
end;

destructor TClientConnection.Destroy;
begin
  FreeAndNil(FSocket);
  FreeAndNil(FInBuffer);
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
      try
        repeat
          if Bootloading then
            DoBootload
          else begin
            DoCheckForNewClients;
            DoCheckRaspberryPiForIncomingMessages;
            DoCheckEthernetClientsForIncomingMessages;
            DoDispatchToRaspberryPi;
            DoDispatchToEthernetClientsAndClearMessages;
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
// Runs the raw RPi SPI receive buffer and extracts valid LCC messages from the
// array and stores them in the RPi Buffer
// ***************************************************************************
procedure TRPiCAN.ExtractSpiRxBuffer(var RxBuffer: TPiSpiBuffer; Count: Integer);
var
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
begin
  GridConnectStrPtr := nil;
  for i := 0 to Count - 1 do
  begin
    if GridConnectHelperRPi.GridConnect_DecodeMachine(RxBuffer[i], GridConnectStrPtr) then
    begin
      case GridConnectStrPtr^[1] of
        Ord('X') : RaspberryPiInBuffer.Add(GridConnectBufferToString(GridConnectStrPtr^));
        Ord('R') : begin
                     if GridConnectStrPtr^[2] = Ord('1') then
                       WriteLn('XOn')
                     else
                       WriteLn('XOff');
                   end;
      end;
    end;
  end;
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
  RaspberryPiInBuffer := TStringList.Create;
  GridConnectHelper := TGridConnectHelper.Create;
  GridConnectHelperRPi := TGridConnectHelper.Create;
  for i := 0 to Length(TxBufferNull) - 1 do
    FTxBufferNull[i] := $00
end;

// ****************************************************************************
// Destroys the main object
// ***************************************************************************
destructor TRPiCAN.Destroy;
begin
  FreeAndNil(FRaspberryPiSpi);
  FreeAndNil(FListenSocket);
  FreeAndNil(FClientConnections);
  FreeAndNil(FRaspberryPiInBuffer);
  FreeAndNil(FGridConnectHelper);
  FreeAndNil(FGridConnectHelperRPi);
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

procedure TRPiCAN.DoBootLoad;
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
       RaspberryPiSpi.ZeroBuffer(@TxBuffer, MAX_RAWBUFFER_BYTES);
       TxBuffer[0] := Ord(':');
       TxBuffer[1] := Ord('B');
       RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, MAX_RAWBUFFER_BYTES);
       Delay(DELAY_BOOTLOADER_MS);

       // Tell the micro to get ready for bootloader syncronization
       TxBuffer[0] := CMD_SYNC;
       RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 1);
       Delay(DELAY_BOOTLOADER_MS);

       // The micro must reply in the next packet block
       // Now we are bit banging the SPI so the count can be variable
       Delay(DELAY_BOOTLOADER_MS);
       RaspberryPiSpi.Transfer(@TxBufferNull, @RxBuffer, 1);
       if RxBuffer[0] = CMD_LINK then
       begin
         WriteLn('Requesting microcontroller information');
         // Tell the micro to return information about the micro
         TxBuffer[0] := CMD_REQUEST_FLASH_DATA;
         Delay(DELAY_BOOTLOADER_MS);
         RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 1);

         // The micro must reply in this next packet block
         Delay(DELAY_BOOTLOADER_MS);
         RaspberryPiSpi.Transfer(@TxBufferNull, @RxBuffer, 62);
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
           Group := IgnoreList.AddGroup;
           IgnoreAddressStart := BootInfo.BootloaderAddress;
           IgnoreAddressEnd := BootInfo.BootloaderAddress + (BootInfo.BootloaderSize div IntelHexInfo.BytesPerInstruction * IntelHexInfo.AddressIncrement);
           WriteLn('Ignoring from: 0x' + IntToHex(IgnoreAddressStart, 8) + ' to: 0x' + IntToHex(IgnoreAddressEnd, 8));
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
                     RaspberryPiSpi.Transfer(@TxBufferNull, @RxBuffer, 1);
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
                       RaspberryPiSpi.Transfer(@TxBufferNull, @RxBuffer, 1);
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
                       RaspberryPiSpi.Transfer(@TxBufferNull, @RxBuffer, 1);
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
procedure TRPiCAN.DoCheckEthernetClientsForIncomingMessages;
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
              if Verbose then
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

// ****************************************************************************
// Do not call anything that will change the number of strings in the incoming
// buffers of the connections or RPi in this function
// ***************************************************************************
procedure TRPiCAN.DoDispatchToEthernetClientsAndClearMessages;
var
  i, j, iStr, iChar, iSpiStr: Integer;
  SourceConnection: TClientConnection;
  DestinationConnection: TClientConnection;
  OutString: string;
begin
  // Dispatch the messages to all other connections
  for i := 0 to ClientConnections.Count - 1 do
  begin
    // Grab a connecton to grab its incoming messages as the source
    SourceConnection := TClientConnection( ClientConnections[i]);

    // Send any buffered incoming RPi message to this out connection
    for iSpiStr := 0 to RaspberryPiInBuffer.Count - 1 do
    begin
      OutString := RaspberryPiInBuffer[iSpiStr];
      // TODO: If Filter Allows then...
      for iChar := 1 to Length(OutString) do
        SourceConnection.Socket.SendByte( Ord(OutString[iChar]));
    end;

    // Now run through all the connections (including the RPi) and dispatch them
    // to the other connections and RPi
    for j := 0 to ClientConnections.Count - 1 do
    begin
      // Grab the connection to send the message to
      DestinationConnection := TClientConnection( ClientConnections[j]);

      // Don't send it back to the source socket
      if j <> i then
      begin
        // Send an buffered incoming messages from the in connection to the out connection
        for iStr := 0 to SourceConnection.InBuffer.Count - 1 do
        begin
          OutString := SourceConnection.InBuffer[iStr];
          // TODO: If Filter Allows then...
          for iChar := 1 to Length(OutString) do
            DestinationConnection.Socket.SendByte( Ord(OutString[iChar]));
        end;
      end;
    end;
    // The sources message are dispatched to all connections and RPi, get the next one
    SourceConnection.InBuffer.Clear;
  end;
  // The RPi messages have been dispatched to all Connections
  RaspberryPiInBuffer.Clear;
end;

// ****************************************************************************
// Enumerates the Ethernet Connection Buffers and send the outgoing messages to
// the RPi SPI to the micro controller.  Need to eventually have a filter to not
// overwhelm the micro with messages it does not care about
// ***************************************************************************
procedure TRPiCAN.DoDispatchToRaspberryPi;
var
  RxBuffer, TxBuffer: TPiSpiBuffer;
  i, j, iStr, iChar, iTxBuffer, PacketCount, iStrOffset: Integer;
  SourceConnection: TClientConnection;
  OutString: ansistring;
begin
  for i := 0 to ClientConnections.Count - 1 do
  begin
    SourceConnection := TClientConnection( ClientConnections[i]);

    PacketCount := SourceConnection.InBuffer.Count div GRIDCONNECT_STR_COUNT;
    if SourceConnection.InBuffer.Count mod GRIDCONNECT_STR_COUNT <> 0 then
      PacketCount := PacketCount + 1;

    iStrOffset := 0;
    for j := 0 to PacketCount - 1 do
    begin
      iTxBuffer := 0;
      for iStr := 0 to GRIDCONNECT_STR_COUNT - 1 do
      begin
        if iStrOffset < SourceConnection.InBuffer.Count then
          OutString := SourceConnection.InBuffer[iStrOffset]
        else
          OutString := '';
        Inc(iStrOffset);


        iChar := 0;
        while iChar < GRIDCONNECT_CHAR_COUNT do
        begin
          if iChar < Length(OutString) then
            TxBuffer[iTxBuffer] := Ord( OutString[iChar + 1])
          else
            TxBuffer[iTxBuffer] := $00 ;
          Inc(iChar);
          Inc(iTxBuffer);
        end;
      end;
      RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, MAX_RAWBUFFER_BYTES);
      ExtractSpiRxBuffer(RxBuffer, MAX_RAWBUFFER_BYTES);
    end;
  end;
end;

// ****************************************************************************
// Sends nulls down the SPI to flush any waiting messages in the microcontroller
// to the RPi
// ***************************************************************************
procedure TRPiCAN.DoCheckRaspberryPiForIncomingMessages;
var
  RxBuffer: TPiSpiBuffer;
begin
  RaspberryPiSpi.Transfer(@TxBufferNull, @RxBuffer, MAX_RAWBUFFER_BYTES);
  ExtractSpiRxBuffer(RxBuffer, MAX_RAWBUFFER_BYTES);
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
begin
  Application := TRPiCAN.Create(nil);
  Application.Title := 'RPi CAN Hub';
  Application.Run;
  Application.Free;
end.

