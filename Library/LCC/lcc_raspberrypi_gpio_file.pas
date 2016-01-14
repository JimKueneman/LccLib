unit lcc_raspberrypi_gpio_file;

// This is untested as of Jan 9 2016.  Access to the file needs root priviliges
// I will just use the SPI interface for what I a working on now....

{$mode objfpc}{$H+}

interface

{$I lcc_compilers.inc}


{$IFDEF CPUARM}

uses
  Classes, SysUtils, unixtype;

type

  TGpioPinDirection = (gpd_Input, gpd_Output);
  TGpioPinLevel     = (gpd_Lo, gpd_Hi);

  { TRpiGPIO }

  TRpiGPIO = class
  private
    FFileDescriptor: cInt;
  protected
    property FileDescriptor: cInt read FFileDescriptor write FFileDescriptor;

    function OpenGPIO: Boolean;
    procedure CloseGPIO;
    function OpenGpioPin(PinNumber: Integer): Boolean; overload;
    function OpenGpioPin(PinNumber: AnsiString): Boolean; overload;
  public
    constructor Create;
    destructor Destroy;override;

    function SetPinDirection(GpioNumber: Integer; Direction: TGpioPinDirection): Boolean;
    function SetPin(GpioNumber: Integer; PinLevel: TGpioPinLevel): Boolean;
    function ReadPin(GpioNumber: Integer; var PinLevel: TGpioPinLevel): Boolean;
  end;

{$ENDIF}

implementation

{$IFDEF CPUARM}

uses
  baseUnix, Unix;

{ TRpiGPIO }

function TRpiGPIO.OpenGPIO: Boolean;
begin
  if FileDescriptor = -1 then
    FileDescriptor := FpOpen('/sys/class/gpio/export', O_WRONLY);
  Result := FileDescriptor > -1;
end;

procedure TRpiGPIO.CloseGPIO;
begin
  if FileDescriptor > 0 then
    FpClose(FileDescriptor);
  FileDescriptor := -1;
end;

function TRpiGPIO.OpenGpioPin(PinNumber: Integer): Boolean;
begin
  Result := OpenGpioPin(IntToStr(PinNumber));
end;

function TRpiGPIO.OpenGpioPin(PinNumber: AnsiString): Boolean;
begin
  Result := False;
  if OpenGPIO then
  begin
    Result := fpwrite(FileDescriptor, PAnsiChar(PinNumber), Length(PinNumber)) > 0;
    CloseGPIO;
  end;
end;

constructor TRpiGPIO.Create;
begin
  FileDescriptor := -1;
end;

destructor TRpiGPIO.Destroy;
begin
  inherited Destroy;
end;

function TRpiGPIO.SetPinDirection(GpioNumber: Integer; Direction: TGpioPinDirection): Boolean;
var
  PinName, AValue: AnsiString;
  LocalDescriptor: cint;
begin
  Result := False;
  if OpenGpioPin(GpioNumber) then
  begin
    PinName := IntToStr(GpioNumber);
    if OpenGpioPin(PinName) then
    begin
      LocalDescriptor := fpopen('/sys/class/gpio/gpio' + PinName + '/direction', O_WRONLY);
      if LocalDescriptor > -1 then
      begin
        if Direction = gpd_Output then
          AValue := 'out'
        else
          AValue := 'in';
        Result := fpwrite(LocalDescriptor, AValue, Length(AValue)) > 0
      end;
      fpclose(LocalDescriptor);
    end;
  end;
end;

function TRpiGPIO.SetPin(GpioNumber: Integer; PinLevel: TGpioPinLevel): Boolean;
var
  PinName, AValue: AnsiString;
  LocalDescriptor: cint;
begin
  Result := False;
  PinName := IntToStr(GpioNumber);
  if OpenGpioPin(PinName) then
  begin
    LocalDescriptor := fpopen('/sys/class/gpio/gpio' + PinName + '/value', O_WRONLY);
    if LocalDescriptor > -1 then
    begin
      if PinLevel = gpd_Hi then
        AValue := '1'
      else
        AValue := '0';
      Result := fpwrite(LocalDescriptor, PChar( AValue), Length(AValue)) > 0
    end;
    fpclose(LocalDescriptor);
  end;
end;

function TRpiGPIO.ReadPin(GpioNumber: Integer; var PinLevel: TGpioPinLevel): Boolean;
var
  PinName, AValue: AnsiString;
  LocalDescriptor: cint;
begin
  Result := False;
  PinName := IntToStr(GpioNumber);
  if OpenGpioPin(PinName) then
  begin
    LocalDescriptor := fpopen('/sys/class/gpio/gpio' + PinName + '/value', O_WRONLY);
    if LocalDescriptor > -1 then
    begin
      SetLength(AValue, 16);
      if fpread(LocalDescriptor, PChar( AValue), Length(AValue)) > 0 then
      begin
        SetLength(AValue, StrLen( PAnsiChar( AValue)));
        if AValue = '0' then
        begin
          Result := True;
          PinLevel := gpd_Lo
        end else
        if AValue = '1' then
        begin
          Result := True;
          PinLevel := gpd_Hi
        end;
      end;
    end;
    fpclose(LocalDescriptor);
  end;
end;


{$ENDIF}

end.

