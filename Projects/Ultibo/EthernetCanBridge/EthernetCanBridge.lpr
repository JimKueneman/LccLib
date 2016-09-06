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
  Winsock2,
  Devices,
  Console,
  HTTP,
  WebStatus,
  SPI,
  TCP,
  Keyboard,
  WiFi,

  lcc_gridconnect,
  lcc_can_message_assembler_disassembler,
  lcc_defines,
  lcc_utilities
  ;

const
  LCC_MESSAGES_PER_SPI_PACKET = 8;
  BYTES_PER_LCC_GRIDCONNECT_MESSAGE = 30;
  BYTES_PER_SPI_PACKET = BYTES_PER_LCC_GRIDCONNECT_MESSAGE * LCC_MESSAGES_PER_SPI_PACKET;

type
  TPiSpiBuffer = array[0..BYTES_PER_SPI_PACKET-1] of Byte;

var
  SpiDevice: PSPIDevice;
  MainConsole:  THandle;
  WriteBuff, ReadBuff: TPiSpiBuffer;
  ReadCount: LongWord;
  HTTPListener: THTTPListener;
  TcpSocket: TTCPSocket;
  i: Integer;
  GridConnectHelper: TGridConnectHelper;
  InBuffer: TStringList;
  Xon, Verbose: Boolean;

function EnumerateDevices(Device:PDevice;Data:Pointer): LongWord;
begin
  ConsoleWriteLn('Device Found: ');
  ConsoleWriteLn('Name: ' + Device^.DeviceName);
  ConsoleWriteLn('Class: ' + DeviceClassToString( Device^.DeviceClass));
  ConsoleWriteLn('Description: ' + Device^.DeviceDescription);
  Result := ERROR_SUCCESS;
end;

function GetNetworkConnected:Boolean;
var
 Address:String;
begin
 {}
 Result:=False;

 {Get Address}
 Address := ResolveUltiboIp;

 {Check Address}
 if Address = '' then Exit;
 if Address = '0.0.0.0' then Exit;
 if Address = '255.255.255.255' then Exit;
 if Copy(Address,1,Length('192.168.100.')) = '192.168.100.' then Exit;

 Result:=True;
end;

procedure ExtractSpiRxBuffer(var RxBuffer: TPiSpiBuffer; Count: Integer);
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
                     if Verbose then ConsoleWriteLn(MessageStr);
                   end;
        Ord('R') : XOn := GridConnectStrPtr^[2] <> Ord('0');
        end;
      end;
    end;
end;

begin
  { Add your program code here }
  GridConnectHelper := TGridConnectHelper.Create;
  InBuffer := TStringList.Create;
  InBuffer.Delimiter := #10;
  XOn := True;  // Software handshake ON
  Verbose := True;

  for i := 0 to BYTES_PER_SPI_PACKET - 1 do
    WriteBuff[i] := 0;

  {Create and start HTTP Listener}
  HTTPListener:=THTTPListener.Create;
  HTTPListener.Active:=True;
  {Register Web Status}
  WebStatusRegister(HTTPListener,'','',True);

  // Create TCP Socket



  MainConsole := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);
  ConsoleWriteLn('Console Created');

  DeviceEnumerate(DEVICE_CLASS_ANY, @EnumerateDevices, nil);

  {To prove that worked let's output some text on the console window}
  ConsoleWriteLn('Welcome to Example SPI test');

  ConsoleWriteLn('Looking IP Address');
  while GetNetworkConnected = False do
    Sleep(1000);

  ConsoleWriteLn('IP Address found: ' + ResolveUltiboIp);

  ConsoleWriteLn('Looking for the SPI Device');
  SpiDevice := PSPIDevice(DeviceFindByDescription('BCM2836 SPI0 Master'));
  if Assigned(SpiDevice) then
  begin
    ConsoleWriteLn('Device Found');
    ConsoleWriteLn('Starting Device');
    if SPIDeviceStart(SPIDevice, SPI_MODE_4WIRE, 976000, SPI_CLOCK_PHASE_HIGH, SPI_CLOCK_POLARITY_LOW) = ERROR_SUCCESS then
    begin
      ConsoleWriteLn('Device Started');
      while True do
      begin
        ReadCount := 0;
        if SPIDeviceWriteRead(SpiDevice, SPI_CS_0, @WriteBuff, @ReadBuff, BYTES_PER_SPI_PACKET, SPI_TRANSFER_NONE, ReadCount) = ERROR_SUCCESS then
        begin
          if ReadCount > 0 then
          begin
            ExtractSpiRxBuffer(ReadBuff, BYTES_PER_SPI_PACKET);
            // Throw them away for now
            InBuffer.Clear;
          end;
        end;
      end;
    end else
      ConsoleWriteLn('Device not started');
  end else
    ConsoleWriteLn('Device not found');
end.

