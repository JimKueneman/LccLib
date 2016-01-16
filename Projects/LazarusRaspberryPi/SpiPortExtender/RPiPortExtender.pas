program RPiPortExtender;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, blcksock, synsock, baseUnix,
  sockets, contnrs, lcc_gridconnect, crt, intel_hex_parser,
  lcc_messages, lcc_raspberrypi, lcc_node, lcc_node_protocol_helpers,
  lcc_defines;


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


  { TRPiCAN }

  TRPiCAN = class(TCustomApplication)
  private
    FRaspberryPiSpi: TRaspberryPiSpi;
    FVerbose: Boolean;
    procedure SetVerbose(AValue: Boolean);
  protected

    procedure DoRun; override;
    procedure WriteHelp; virtual;
  public
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
  LccMessage: TLccMessage;
  i: Integer;
  TxBuffer, RxBuffer: TPiSpiBuffer;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h v', 'help verbose');
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

  for i := 0 to MAX_PISPIBUFFER - 1 do
    TxBuffer[i] := i;

  { add your program here }
  LccMessage := TLccMessage.Create;
  try
    repeat
      CheckSynchronize(0);

      if RaspberryPiSpi.OpenSpi(SPI_DRIVER_PATH_CS0) then
      begin
        RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 100);
        RaspberryPiSpi.CloseSpi;
      end;

       if RaspberryPiSpi.OpenSpi(SPI_DRIVER_PATH_CS1) then
      begin
        RaspberryPiSpi.Transfer(@TxBuffer, @RxBuffer, 100);
        RaspberryPiSpi.CloseSpi;
      end;

      if KeyPressed then
      begin
        case ReadKey of
          'l', 'L' : begin

                     end;
          'v', 'V' : Verbose := not Verbose;
          'q', 'Q' : Terminate;
        end;
      end;

    until Terminated;
  finally

  end;

  LccMessage.Free;
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
  RaspberryPiSpi.Mode := psm_ClkIdleLo_DataFalling;
  RaspberryPiSpi.Speed := pss_976kHz;
  RaspberryPiSpi.Bits := psb_8;
end;

// ****************************************************************************
// Destroys the main object
// ***************************************************************************
destructor TRPiCAN.Destroy;
begin
  FreeAndNil(FRaspberryPiSpi);
  inherited Destroy;
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

// ****************************************************************************
// Main
// ***************************************************************************
var
  Application: TRPiCAN;
  i: Integer;
begin
  for i := 0 to Length(GlobalBufferNull) - 1 do GlobalBufferNull[i] := $00;
  Application := TRPiCAN.Create(nil);
  Application.Title := 'RPi Port Extender';
  Application.Run;
  Application.Free;
end.

