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
  uTFTP,
  GPIO,

  lcc_gridconnect,
  lcc_can_message_assembler_disassembler,
  lcc_defines,
  lcc_utilities,
  lcc_nodemanager,
  NodeConnections,
  lcc_threaded_stringlist
  ;

type

  { TcpReadThread }

  { TTcpReadThread }

  TTcpReadThread = class(TThread)
  private
    FStringList: TThreadStringList;
  protected
    procedure Execute; override;
  public
    property StringList: TThreadStringList read FStringList write FStringList;
  end;

var
  SpiDevice: PSPIDevice;
  WriteBuff, ReadBuff: TPiSpiBuffer;
  ReadCount: LongWord;
  HTTPListener: THTTPListener;
  TcpClient: TWinsock2TCPClient;
  GridConnectHelper: TGridConnectHelper;
  Xon: Boolean;
  TcpReadThread: TTcpReadThread;
  AStringList: TThreadStringList;




procedure FtpMsg(Sender : TObject; s : string);
begin
  ConsoleWriteLn('FTP: ' + s);
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

procedure WaitForNetworkConnection;
begin
 ConsoleWriteLn('Looking for the Raspberry Pi''s IP Address');
 while GetNetworkConnected = False do
    Sleep(1000);
 ConsoleWriteLn('IP Address found: ' + ResolveUltiboIp);
end;

procedure WaitForLccConnection(Socket: TWinsock2TCPClient);
begin
 Socket.RemoteAddress := '10.0.3.178';
 Socket.RemotePort := 12021;
 ConsoleWriteLn('Looking for the Lcc Server at ' + Socket.RemoteAddress + ':' + IntToStr(Socket.RemotePort));
 while not Socket.Connect do
   Sleep(1000);
 ConsoleWriteLn('Lcc Server found and connected');
end;

procedure WaitForSpiConnection;
begin
  ConsoleWriteLn('Looking for the SPI Device');
  while not Assigned(SpiDevice) do
  begin
    SpiDevice := PSPIDevice(DeviceFindByDescription('BCM2836 SPI0 Master'));
    if not Assigned(SpiDevice) then
      Sleep(1000);
  end;
  ConsoleWriteLn('Device Found');
  ConsoleWriteLn('Starting Device');
  while SPIDeviceStart(SPIDevice, SPI_MODE_4WIRE, 976000, SPI_CLOCK_PHASE_HIGH, SPI_CLOCK_POLARITY_LOW) <> ERROR_SUCCESS do
    Sleep(1000);
  ConsoleWriteLn('Device Started');
end;

procedure ExtractSpiRxBufferAndSendToSocket(var RxBuffer: TPiSpiBuffer; Count: Integer; Socket: TWinsock2TCPClient);
var
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
  MessageStr: ansistring;
  DelimiterChar: char;
begin
  GridConnectStrPtr := nil;
  DelimiterChar := #10;
  for i := 0 to Count - 1 do
  begin
    if GridConnectHelper.GridConnect_DecodeMachine(RxBuffer[i], GridConnectStrPtr) then
    begin
      case GridConnectStrPtr^[1] of
        Ord('X') : begin
                     MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                     if Socket.Connected then
                     begin
                       Socket.WriteData(@MessageStr[1], Length(MessageStr));
                       Socket.WriteData(@DelimiterChar, 1);
                       Sleep(1);
                     end;
                   end;
        Ord('R') : XOn := GridConnectStrPtr^[2] <> Ord('0');
        end;
      end;
    end;
end;

procedure ExtractSocketBuffer(var TxBuffer, RxBuffer: TPiSpiBuffer; Socket: TWinsock2TCPClient);
var
  SpiPacketCount, iSpiPacket, iTxBuffer, iChar, iLccMessage: Integer;
  List: TStringList;
  OutString: ansistring;
begin
  FillChar(TxBuffer, SizeOf(TxBuffer), #0);
  FillChar(RxBuffer, SizeOf(RxBuffer), #0);
  if XOn then
  begin
    List := AStringList.LockList;
    try
      if List.Count > 0 then
      begin
        // Calculate how many Spi Packet Bursts we need to send
        SpiPacketCount := List.Count div LCC_MESSAGES_PER_SPI_PACKET;
        // Do we have a partial packet to send?
        if List.Count mod LCC_MESSAGES_PER_SPI_PACKET <> 0 then
          SpiPacketCount := SpiPacketCount + 1;

        // Run through all the necessary Spi Packets to send all the messages
        for iSpiPacket := 0 to SpiPacketCount - 1 do
        begin

          GPIOOutputSet(GPIO_PIN_18,GPIO_LEVEL_HIGH);

          // The micro could have signaled us to stop when we sent the last packet
          if XOn then
          begin
            iTxBuffer := 0;
            // Fill up the Spi Packet with as many Lcc Messages as will fit
            for iLccMessage := 0 to LCC_MESSAGES_PER_SPI_PACKET - 1 do
            begin
              // Grab the next string in the String List or a null string if we hit the end of the list
              if List.Count > 0 then
              begin
                OutString := List[0];
                List.Delete(0);          // Painfully slow but anyother way is very complex
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

            GPIOOutputSet(GPIO_PIN_18,GPIO_LEVEL_LOW);

            // Transfer the packet
            if SPIDeviceWriteRead(SpiDevice, SPI_CS_0, @TxBuffer, @RxBuffer, BYTES_PER_SPI_PACKET, SPI_TRANSFER_NONE, ReadCount) = ERROR_SUCCESS then
              ExtractSpiRxBufferAndSendToSocket(RxBuffer, BYTES_PER_SPI_PACKET, Socket);
          end;
        end;
      end;
    finally
      AStringList.UnlockList;
    end;
  end;
end;

{ TTcpReadThread }

procedure TTcpReadThread.Execute;
var
  RxBuff: TPiSpiBuffer;
  ACount: Integer;
  IsClosed: Boolean;
  i: Integer;
  GridConnectStrPtr: PGridConnectString;
begin
  while not Terminated do
  begin
    FillChar(RxBuff, SizeOf(RxBuff), #0);
    ACount := Sizeof(RxBuff);
    IsClosed := False;
    if TcpClient.ReadAvailable(@RxBuff, Sizeof(RxBuff), ACount, IsClosed) then
    begin
    //  ConsoleWriteLn(IntToStr(ACount));
      if IsClosed then
        Terminate
      else begin
        GridConnectStrPtr := nil;
        for i := 0 to ACount - 1 do
        begin
          if GridConnectHelper.GridConnect_DecodeMachine(RxBuff[i], GridConnectStrPtr) then
          begin
             if Assigned(StringList) then
               StringList.Add(GridConnectBufferToString(GridConnectStrPtr^));
          end;
        end
      end;
     end
  end;
end;

begin
  { Add your program code here }
  SpiDevice := nil;
  FillChar(WriteBuff, SizeOf(WriteBuff), #0);
  FillChar(ReadBuff, SizeOf(WriteBuff), #0);
  AStringList := TThreadStringList.Create;
  GridConnectHelper := TGridConnectHelper.Create;
  TcpClient := TWinsock2TCPClient.Create;
  XOn := True;  // Software handshake ON

  {Now set GPIO pin 16 to Pull None}
  GPIOPullSelect(GPIO_PIN_18,GPIO_PULL_NONE);
  {And make GPIO pin 16 an Output so we can turn the LED on or off}
  GPIOFunctionSelect(GPIO_PIN_18,GPIO_FUNCTION_OUT);
  {Finally set the value of GPIO pin 16 to Low so the LED will be off}
  GPIOOutputSet(GPIO_PIN_18,GPIO_LEVEL_LOW);

  {Create and start HTTP Listener}
  HTTPListener:=THTTPListener.Create;
  HTTPListener.Active:=True;
  {Register Web Status}
  WebStatusRegister(HTTPListener,'','',True);

  ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);
  ConsoleWriteLn('Welcome to the Mustangpeak CAN to Ethernet Bridge');
  ConsoleWriteLn('Console Created');
  SetOnMsg(@FtpMsg);
  WaitForNetworkConnection;
  WaitForLccConnection(TcpClient);
  WaitForSpiConnection;

  TcpReadThread := TTcpReadThread.Create(True);
  TcpReadThread.StringList := AStringList;
  TcpReadThread.Start;

  while True do
  begin
    ReadCount := 0;

    ExtractSocketBuffer(WriteBuff, ReadBuff, TcpClient);
    FillChar(WriteBuff, SizeOf(WriteBuff), #0);
    if SPIDeviceWriteRead(SpiDevice, SPI_CS_0, @WriteBuff, @ReadBuff, BYTES_PER_SPI_PACKET, SPI_TRANSFER_NONE, ReadCount) = ERROR_SUCCESS then
      ExtractSpiRxBufferAndSendToSocket(ReadBuff, BYTES_PER_SPI_PACKET, TcpClient);
  end;

  TcpReadThread.StringList := nil;
  TcpReadThread.Terminate;
  while not TcpReadThread.Finished do
    Sleep(1000);
  TcpClient.Disconnect;
  GridConnectHelper.Free;
  TcpClient.Free;
  HTTPListener.Free;
end.

