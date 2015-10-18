unit lcc_raspberrypi_gpio;
{
 BCM2835 GPIO Registry Driver, also can use to manipulate cpu other registry areas

 This code is tested only Broadcom bcm2835 cpu, different arm cpus may need different
 gpio driver implementation

 2013 Gabor Szollosi
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  REG_GPIO = $20000;//bcm2835 gpio register 0x2000 0000. new fpMap uses page offset, one page is 4096bytes
  // hex 0x1000 so simply calculate 0x2000 0000 / 0x1000  = 0x2000 0
  PAGE_SIZE = 4096;
  BLOCK_SIZE = 4096;
    // The BCM2835 has 54 GPIO pins.
//	BCM2835 data sheet, Page 90 onwards.
//	There are 6 control registers, each control the functions of a block
//	of 10 pins.

  CLOCK_BASE = (REG_GPIO + $101);
  GPIO_BASE =  (REG_GPIO + $200);
  GPIO_PWM =   (REG_GPIO + $20C);

     INPUT = 0;
     OUTPUT = 1;
     PWM_OUTPUT = 2;
     LOW = False;
     HIGH = True;
     PUD_OFF = 0;
     PUD_DOWN = 1;
     PUD_UP = 2;

   // PWM

  PWM_CONTROL = 0;
  PWM_STATUS  = 4;
  PWM0_RANGE  = 16;
  PWM0_DATA   = 20;
  PWM1_RANGE  = 32;
  PWM1_DATA   = 36;

  PWMCLK_CNTL =	160;
  PWMCLK_DIV  =	164;

  PWM1_MS_MODE    = $8000;  // Run in MS mode
  PWM1_USEFIFO    = $2000;  // Data from FIFO
  PWM1_REVPOLAR   = $1000;  // Reverse polarity
  PWM1_OFFSTATE   = $0800;  // Ouput Off state
  PWM1_REPEATFF   = $0400;  // Repeat last value if FIFO empty
  PWM1_SERIAL     = $0200;  // Run in serial mode
  PWM1_ENABLE     = $0100;  // Channel Enable

  PWM0_MS_MODE    = $0080;  // Run in MS mode
  PWM0_USEFIFO    = $0020;  // Data from FIFO
  PWM0_REVPOLAR   = $0010;  // Reverse polarity
  PWM0_OFFSTATE   = $0008;  // Ouput Off state
  PWM0_REPEATFF   = $0004;  // Repeat last value if FIFO empty
  PWM0_SERIAL     = $0002;  // Run in serial mode
  PWM0_ENABLE     = $0001;  // Channel Enable


type

  { TIoPort }

  TIoPort = class // IO bank object
  private
    //function get_pinDirection(aPin: TGpIoPin): TGpioPinConf;
  public
   FGpio: ^LongWord;
   FClk: ^LongWord;
   FPwm: ^LongWord;
   procedure SetPinMode(gpin, mode: byte);
   function GetBit(gpin: byte):boolean;inline; // gets pin bit}
   procedure ClearBit(gpin: byte);inline;// write pin to 0
   procedure SetBit(gpin: byte);inline;// write pin to 1
   procedure SetPullMode(gpin, mode: byte);
   procedure PwmWrite(gpin: byte; value: LongWord);inline;// write pin to pwm value
  end;

  { TIoDriver }

  TIoDriver = class
  private

  public
    destructor Destroy;override;
    function MapIo:boolean;// creates io memory mapping
    procedure UnmapIoRegisrty(FMap: TIoPort);// close io memory mapping
    function CreatePort(PortGpio, PortClk, PortPwm: LongWord):TIoPort; // create new IO port
  end;

var
  fd: integer;// /dev/mem file handle


procedure delayNanoseconds (howLong : LongWord);


implementation

uses
  baseUnix, Unix;

procedure delayNanoseconds (howLong : LongWord);
var
  sleeper, dummy : timespec;
begin
  sleeper.tv_sec  := 0 ;
  sleeper.tv_nsec := howLong ;
  fpnanosleep (@sleeper,@dummy) ;
end;
{ TIoDriver }

destructor TIoDriver.Destroy;
begin
  inherited Destroy;
end;

function TIoDriver.MapIo: boolean;
{$IFNDEF CPUARM}
const
  O_Sync = 0;   // Dummy for anything but the RPi
{$ENDIF}
begin
  Result := True;
  fd := fpopen('/dev/mem', O_RdWr or O_Sync); // Open the master /dev/memory device
  if fd < 0 then
  begin
    Result := False; // unsuccessful memory mapping
  end;
 //
end;

procedure TIoDriver.UnmapIoRegisrty(FMap:TIoPort);
begin
  if FMap.FGpio <> nil then
 begin
   fpMUnmap(FMap.FGpio,PAGE_SIZE);
   FMap.FGpio := Nil;
 end;
 if FMap.FClk <> nil then
 begin
   fpMUnmap(FMap.FClk,PAGE_SIZE);
   FMap.FClk := Nil;
 end;
 if FMap.FPwm <> nil then
 begin
   fpMUnmap(FMap.FPwm ,PAGE_SIZE);
   FMap.FPwm := Nil;
 end;
end;

function TIoDriver.CreatePort(PortGpio, PortClk, PortPwm: LongWord): TIoPort;
begin
  Result := TIoPort.Create;// new io port, pascal calls new fpMap, where offst is page sized 4096 bytes!!!
  Result.FGpio := FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, fd, PortGpio); // port config gpio memory
  Result.FClk:= FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, fd, PortClk);; // port clk
  Result.FPwm:= FpMmap(Nil, PAGE_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, fd, PortPwm);; // port pwm
end;
//*******************************************************************************
procedure TIoPort.SetPinMode(gpin, mode: byte);
var
  fSel, shift, alt : byte;
  gpiof, clkf, pwmf : ^LongWord;
begin
  fSel := (gpin div 10)*4 ;  //Select Gpfsel 0 to 5 register
  shift := (gpin mod 10)*3 ;  //0-9 pin shift
  gpiof := Pointer(LongWord(Self.FGpio)+fSel);
  if (mode = INPUT) then
    gpiof^ := gpiof^ and ($FFFFFFFF - (7 shl shift))  //7 shl shift komplemens - Sets bits to zero = input
  else if (mode = OUTPUT) then
  begin
    gpiof^ := gpiof^ and ($FFFFFFFF - (7 shl shift)) or (1 shl shift);
  end
  else if (mode = PWM_OUTPUT) then
  begin
    Case gpin of
      12,13,40,41,45 : alt:= 4 ;
      18,19          : alt:= 2 ;
      else alt:= 0 ;
    end;
    If alt > 0 then
    begin
      gpiof^ := gpiof^ and ($FFFFFFFF - (7 shl shift)) or (alt shl shift);
      clkf := Pointer(LongWord(Self.FClk)+PWMCLK_CNTL);
      clkf^ := $5A000011 or (1 shl 5) ;                  //stop clock
      delayNanoseconds(200);
      clkf := Pointer(LongWord(Self.FClk)+PWMCLK_DIV);
      clkf^ := $5A000000 or (32 shl 12) ;	// set pwm clock div to 32 (19.2/3 = 600KHz)
      clkf := Pointer(LongWord(Self.FClk)+PWMCLK_CNTL);
      clkf^ := $5A000011 ;                               //start clock
      Self.ClearBit(gpin);
      pwmf := Pointer(LongWord(Self.FPwm)+PWM_CONTROL);
      pwmf^ := 0 ;		 	        // Disable PWM
      delayNanoseconds(200);
      pwmf := Pointer(LongWord(Self.FPwm)+PWM0_RANGE);
      pwmf^ := $400 ;                             //max: 1023
      delayNanoseconds(200);
      pwmf := Pointer(LongWord(Self.FPwm)+PWM1_RANGE);
      pwmf^ := $400 ;                             //max: 1023
      delayNanoseconds(200);
      // Enable PWMs
      pwmf := Pointer(LongWord(Self.FPwm)+PWM0_DATA);
      pwmf^ := 0 ;                                //start value
      pwmf := Pointer(LongWord(Self.FPwm)+PWM1_DATA);
      pwmf^ := 0 ;                                //start value
      pwmf := Pointer(LongWord(Self.FPwm)+PWM_CONTROL);
      pwmf^ := PWM0_ENABLE or PWM1_ENABLE ;
    end;
  end;
end;

procedure TIoPort.SetBit(gpin : byte);
var
   gpiof : ^LongWord;
begin
  gpiof := Pointer(LongWord(Self.FGpio) + 28 + (gpin shr 5) shl 2);
  gpiof^ := 1 shl gpin;
end;

procedure TIoPort.ClearBit(gpin : byte);
var
   gpiof : ^LongWord;
begin
  gpiof := Pointer(LongWord(Self.FGpio) + 40 + (gpin shr 5) shl 2);
  gpiof^ := 1 shl gpin;
end;

function TIoPort.GetBit(gpin : byte):boolean;
var
   gpiof : ^LongWord;
begin
  gpiof := Pointer(LongWord(Self.FGpio) + 52 + (gpin shr 5) shl 2);
  if (gpiof^ and (1 shl gpin)) = 0 then Result := False else Result := True;
end;

procedure TIoPort.SetPullMode(gpin, mode: byte);
var
   pudf, pudclkf : ^LongWord;
begin
  pudf := Pointer(LongWord(Self.FGpio) + 148 );
  pudf^ := mode;   //mode = 0, 1, 2 :Off, Down, Up
  delayNanoseconds(200);
  pudclkf := Pointer(LongWord(Self.FGpio) + 152 + (gpin shr 5) shl 2);
  pudclkf^ := 1 shl gpin ;
  delayNanoseconds(200);
  pudf^ := 0 ;
  pudclkf^ := 0 ;
end;

procedure TIoPort.PwmWrite(gpin : byte; value : Longword);
var
   pwmf : ^LongWord;
   port : byte;
begin
  Case gpin of
      12,18,40 : port:= PWM0_DATA ;
      13,19,41,45 : port:= PWM1_DATA ;
      else exit;
  end;
  pwmf := Pointer(LongWord(Self.FPwm) + port);
  pwmf^ := value and $FFFFFBFF; // $400 complemens
end;

end.

