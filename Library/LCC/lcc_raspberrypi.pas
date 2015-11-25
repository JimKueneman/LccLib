unit lcc_raspberrypi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF CPUARM}
  , BaseUnix
  {$ENDIF}
  ;

{$IFDEF CPUARM}
const
  // Clock Phase
  SPI_CPHA           =     $01;
  // Clock Polarity
  SPI_CPOL           =     $02;

  // 4 possible SPI Clock Modes
  SPI_MODE_0         =     $0000 or $0000;
  SPI_MODE_1         =     $0000 or SPI_CPHA;
  SPI_MODE_2         =     SPI_CPOL or $0000;
  SPI_MODE_3         =     SPI_CPOL or SPI_CPHA;

  // Chip Select is High
  SPI_CS_HIGH        =     $0004;
  // Send LSB First
  SPI_LSB_FIRST      =     $0008;
  // Shared SI/SO
  SPI_3WIRE          =     $0010;
  // LoopBack
  SPI_LOOP           =     $0020;
  // No CS
  SPI_NO_CS          =     $0040;
  // SPI Ready
  SPI_READY          =     $0080;
  // TX Stuff?
  SPI_TX_DUAL        =     $0100;
  SPI_TX_QUAD        =     $0200;
  SPI_RX_DUAL        =     $0400;
  SPI_RX_QUAD        =     $0800;

  SPI_IOC_MAGIC = ord('k');

  // 33 # define _IOW(x,y,z)    (((x)<<8)|y)
  // 36 # define _IOR(x,y,z)    (((x)<<8)|y)
  // 119 /* Read / Write of SPI mode (SPI_MODE_0..SPI_MODE_3) (limited to 8 bits) */
  // 120 #define SPI_IOC_RD_MODE                 _IOR(SPI_IOC_MAGIC, 1, __u8)
  // 121 #define SPI_IOC_WR_MODE                 _IOW(SPI_IOC_MAGIC, 1, __u8)
  SPI_IOC_RD_MODE = (SPI_IOC_MAGIC shl 8) or 1 or $80010000;
  SPI_IOC_WR_MODE = (SPI_IOC_MAGIC shl 8) or 1 or $40010000;

  // Read / Write SPI bit justification */
  SPI_IOC_RD_LSB_FIRST = (SPI_IOC_MAGIC shl 8) or 2 or $40010000;
  SPI_IOC_WR_LSB_FIRST = (SPI_IOC_MAGIC shl 8) or 2 or $80010000;

  // Read / Write SPI device word length (1..N) */
  SPI_IOC_RD_BITS_PER_WORD = (SPI_IOC_MAGIC shl 8) or 3 or $80010000;
  SPI_IOC_WR_BITS_PER_WORD = (SPI_IOC_MAGIC shl 8) or 3 or $40010000;

  // Read / Write SPI device default max speed hz */
  SPI_IOC_RD_MAX_SPEED_HZ = (SPI_IOC_MAGIC shl 8) or 4 or $80040000;
  SPI_IOC_WR_MAX_SPEED_HZ = (SPI_IOC_MAGIC shl 8) or 4 or $40040000;

const
  // How many message records are sent in IOCtl
  SPI_IOC_MSGSIZE_1 = (SPI_IOC_MAGIC shl 8) or 0 or $40200000;
  SPI_IOC_MSGSIZE_2 = (SPI_IOC_MAGIC shl 8) or 0 or $40400000;
  SPI_IOC_MSGSIZE_3 = (SPI_IOC_MAGIC shl 8) or 0 or $40600000;
  SPI_IOC_MSGSIZE_4 = (SPI_IOC_MAGIC shl 8) or 0 or $40800000;

type
  TPiSpiBuffer = array[0..4095] of Byte;
  PPiSpiBuffer = ^TPiSpiBuffer;

type
  _spi_ioc_transfer = record
    tx_buffPtr    : Int64;       // Pointer a buffer
    rx_buffPtr    : Int64;       // Pointer to a buffer
    len           : LongWord;   // how many to send
    speed_hz      : LongWord;  // speed
    delay_usecs   : Word;   // how long to delay?
    bits_per_word : Byte; // Bits per word (8,16)
    cs_change     : Byte;     // CS Change?
    pad           : LongWord;       // Padding?
  end;

  TSpiIocTransfer = _spi_ioc_transfer;

{$ENDIF}

type
  TPiSpiMode = (psm_ClkIdleLo_DataRising,
                psm_ClkIdleLo_DataFalling,
                psm_ClkIdleHi_DataRising,
                psm_ClkIdleHi_DataFalling,
                psm_Unknown);

  TPiSpiBits = (psb_8,
                psb_16);

  TPiSpiSpeed = (pss_7629Hz,
                 pss_15_2kHz,
                 pss_30_5kHz,
                 pss_61kHz,
                 pss_122kHz,
                 pss_244kHz,
                 pss_488kHz,
                 pss_976kHz,
                 pss_1_953MHz,
                 pss_3_9Mhz,
                 pss_7_8Mhz,
                 pss_15_6Mhz,
                 pss_31_2Mhz,
                 pss_62_5Mhz,
                 pss_125Mhz);


 {$IFDEF CPUARM}
  { TRaspberryPiSpi }
  TRaspberryPiSpi = class
  private
    FBits: TPiSpiBits;
    FHandle: Integer;
    FIOCtlResult: Integer;
    FMode: TPiSpiMode;
    FSpeed: TPiSpiSpeed;
    function GetLastErrorDesc: string;
    procedure SetBits(AValue: TPiSpiBits);
    procedure SetMode(AValue: TPiSpiMode);
    procedure SetSpeed(AValue: TPiSpiSpeed);
  public
    property Handle: Integer read FHandle;
    property IOCtlResult: Integer read FIOCtlResult write FIOCtlResult;
    property Mode: TPiSpiMode read FMode write SetMode;
    property Bits: TPiSpiBits read FBits write SetBits;
    property Speed: TPiSpiSpeed read FSpeed write SetSpeed;
    property LastErrorDesc: string read GetLastErrorDesc;

    constructor Create;
    destructor Destroy; override;
    function CopyStringToBuffer(AString: ansistring; Buffer: PPiSpiBuffer; Offset, BufferLen: Integer): Integer;
    function OpenSpi(SpiDevicePath: string): Boolean;
    procedure CloseSpi;
    function Transfer(TxBuffer: PPiSpiBuffer; RxBuffer: PPiSpiBuffer; Count: Integer): Boolean;
    procedure ZeroBuffer(Buffer: PPiSpiBuffer; Count: Integer);
  end;

  function GetRaspberryPiSpiPortNames: string;
{$ENDIF}


implementation

{$IFDEF CPUARM}

function GetRaspberryPiSpiPortNames: string;
var
  TmpPorts: String;
  sr : TSearchRec;
begin
  try
    TmpPorts := '';
    if FindFirst('/dev/spidev*', LongInt($FFFFFFFF), sr) = 0 then
    begin
      repeat
        if (sr.Attr and $FFFFFFFF) = Sr.Attr then
        begin
          TmpPorts := TmpPorts + #13 + ExtractFileName(sr.Name);
        end;
      until FindNext(sr) <> 0;
    end;
    FindClose(sr);
  finally
    Result:=TmpPorts;
  end;
end;

{ TRaspberryPiSpi }

procedure TRaspberryPiSpi.SetMode(AValue: TPiSpiMode);
var
  LocalMode: Byte;
begin
  FIOCtlResult := 0;
  FMode := AValue;
  case FMode of
    psm_ClkIdleLo_DataRising  : LocalMode := SPI_MODE_0;
    psm_ClkIdleLo_DataFalling : LocalMode := SPI_MODE_1;
    psm_ClkIdleHi_DataRising  : LocalMode := SPI_MODE_2;
    psm_ClkIdleHi_DataFalling : LocalMode := SPI_MODE_3;
  end;
  if Handle > -1 then
    IOCtlResult := fpIOCtl(Handle, SPI_IOC_WR_MODE, @LocalMode);
end;

procedure TRaspberryPiSpi.SetSpeed(AValue: TPiSpiSpeed);
var
  LocalSpeed: LongWord;
begin
  FIOCtlResult := 0;
  FSpeed := AValue;
  case FSpeed of
    pss_7629Hz   : LocalSpeed := 7629;
    pss_15_2kHz  : LocalSpeed := 15200;
    pss_30_5kHz  : LocalSpeed := 30500;
    pss_61kHz    : LocalSpeed := 61000;
    pss_122kHz   : LocalSpeed := 122000;
    pss_244kHz   : LocalSpeed := 244000;
    pss_488kHz   : LocalSpeed := 488000;
    pss_976kHz   : LocalSpeed := 976000;
    pss_1_953MHz : LocalSpeed := 1953000;
    pss_3_9Mhz   : LocalSpeed := 3900000;
    pss_7_8Mhz   : LocalSpeed := 7800000;
    pss_15_6Mhz  : LocalSpeed := 15600000;
    pss_31_2Mhz  : LocalSpeed := 31200000;
    pss_62_5Mhz  : LocalSpeed := 62500000;
    pss_125Mhz   : LocalSpeed := 125000000;
  end;
  if Handle > -1 then
    IOCtlResult := fpIOCtl(Handle, SPI_IOC_WR_MAX_SPEED_HZ, @LocalSpeed);
end;

procedure TRaspberryPiSpi.SetBits(AValue: TPiSpiBits);
var
  LocalBits: Byte;
begin
  FIOCtlResult := 0;
  FBits := AValue;
  case FBits of
    psb_8 : LocalBits := 8;
    psb_16 : LocalBits := 16;
  end;
  if Handle > -1 then
    IOCtlResult := fpIOCtl(Handle, SPI_IOC_WR_BITS_PER_WORD, @LocalBits);
end;

function TRaspberryPiSpi.GetLastErrorDesc: string;
begin
  Result := 'Unimplmeneted'
end;

constructor TRaspberryPiSpi.Create;
begin
  FHandle := -1;
  FIOCtlResult := 0;
end;

destructor TRaspberryPiSpi.Destroy;
begin
  if Handle > -1 then
    CloseSpi;
  inherited Destroy;
end;

function TRaspberryPiSpi.CopyStringToBuffer(AString: ansistring; Buffer: PPiSpiBuffer; Offset, BufferLen: Integer): Integer;
var
  i, iBufferIndex: Integer;
begin
  iBufferIndex := Offset;
  for i := 0 to Length(AString) - 1 do
  begin
    if iBufferIndex < BufferLen then
      Buffer^[Offset + i] := Ord( AString[i+1]);
    Inc(iBufferIndex);
  end;
  Result := Offset + Length(AString);
end;

function TRaspberryPiSpi.OpenSpi(SpiDevicePath: string): Boolean;
begin
  Result := False;
  FHandle := fpopen(SpiDevicePath, O_RDWR);
  if Handle > -1 then
  begin
    Mode := FMode;
    if IOCtlResult > -1 then
    begin
      Bits := FBits;
      if IOCtlResult > -1 then
      begin
        Speed := FSpeed;
        Result := IOCtlResult > -1;
      end;
    end;
  end;
end;

procedure TRaspberryPiSpi.CloseSpi;
begin
  if FHandle > - 1 then
    fpclose(FHandle);
  FHandle := -1
end;

function TRaspberryPiSpi.Transfer(TxBuffer: PPiSpiBuffer; RxBuffer: PPiSpiBuffer; Count: Integer): Boolean;
var
  LocalBuffer: TSpiIocTransfer;
begin
  Result := False;
  if Handle > -1 then
  begin
    case FBits of
      psb_8 : LocalBuffer.bits_per_word := 8;
      psb_16 : LocalBuffer.bits_per_word := 16;
    end;
    case FSpeed of
      pss_7629Hz   : LocalBuffer.speed_hz := 7629;
      pss_15_2kHz  : LocalBuffer.speed_hz := 15200;
      pss_30_5kHz  : LocalBuffer.speed_hz := 30500;
      pss_61kHz    : LocalBuffer.speed_hz := 61000;
      pss_122kHz   : LocalBuffer.speed_hz := 122000;
      pss_244kHz   : LocalBuffer.speed_hz := 244000;
      pss_488kHz   : LocalBuffer.speed_hz := 488000;
      pss_976kHz   : LocalBuffer.speed_hz := 976000;
      pss_1_953MHz : LocalBuffer.speed_hz := 1953000;
      pss_3_9Mhz   : LocalBuffer.speed_hz := 3900000;
      pss_7_8Mhz   : LocalBuffer.speed_hz := 7800000;
      pss_15_6Mhz  : LocalBuffer.speed_hz := 15600000;
      pss_31_2Mhz  : LocalBuffer.speed_hz := 31200000;
      pss_62_5Mhz  : LocalBuffer.speed_hz := 62500000;
      pss_125Mhz   : LocalBuffer.speed_hz := 125000000;
    end;
    LocalBuffer.cs_change := 0;
    LocalBuffer.delay_usecs := 0;
    LocalBuffer.len := Count;
    LocalBuffer.rx_buffPtr := LongWord(RxBuffer);
    LocalBuffer.tx_buffPtr := LongWord(TxBuffer);
    LocalBuffer.Pad := 0;
    IOCtlResult := fpIOCtl(handle, SPI_IOC_MSGSIZE_1, @LocalBuffer);
    if IOCtlResult > -1 then
      Result := True
  end;
end;

procedure TRaspberryPiSpi.ZeroBuffer(Buffer: PPiSpiBuffer; Count: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Buffer^[i] := 0;
end;

{$ENDIF}

end.

