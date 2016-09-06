program Project1;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  { Add additional units here }
  Console,
  Devices,
  SPI;

const
  BUFFER_SIZE = 64;

var
  SpiDevice: PSPIDevice;
  MainConsole:  THandle;
  WriteBuff, ReadBuff: array[0..BUFFER_SIZE-1] of Byte;
  ReadCount: LongWord;

function SpiEnumerate(Device:PDevice;Data:Pointer): LongWord;
begin
  ConsoleWriteLn('Device Found: ');
  ConsoleWriteLn('Name: ' + Device^.DeviceName);
  ConsoleWriteLn('Class: ' + DeviceClassToString( Device^.DeviceClass));
  ConsoleWriteLn('Description: ' + Device^.DeviceDescription);
  Result := ERROR_SUCCESS;
end;

begin
  { Add your program code here }
  MainConsole := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);
  ConsoleWriteLn('Console Created');

  DeviceEnumerate(DEVICE_CLASS_ANY, @SpiEnumerate, nil);

  {To prove that worked let's output some text on the console window}
  ConsoleWriteLn('Welcome to Example SPI test');

  ConsoleWriteLn('Looking for the SPI Device');
  SpiDevice := PSPIDevice(DeviceFindByDescription('BCM2836 SPI0 Master'));
  if Assigned(SpiDevice) then
  begin
    ConsoleWriteLn('Device Found');
    ConsoleWriteLn('Starting Device');
    if SPIDeviceStart(SPIDevice, SPI_MODE_4WIRE, 976000, SPI_CLOCK_PHASE_LOW, SPI_CLOCK_POLARITY_LOW) = ERROR_SUCCESS then
    begin
      ConsoleWriteLn('Device Started');
      while True do
      begin
        ReadCount := 0;
        if SPIDeviceWriteRead(SpiDevice, SPI_CS_0, @WriteBuff, @ReadBuff, BUFFER_SIZE, SPI_TRANSFER_NONE, ReadCount) = ERROR_SUCCESS then
        begin

        end;
      end;
    end else
      ConsoleWriteLn('Device not started');
  end else
    ConsoleWriteLn('Device not found');
end.

