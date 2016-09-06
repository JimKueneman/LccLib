{
  The Micro Pascal WebServer

  This is a very simple example webserver implemented with the Synapse library.

  It works with blocking sockets and a single thread, so it
  can only handle one request at a given time.

  It will write the headers that it receives from the browser
  to the standard output.

  It serves a fixed webpage for the / URI
  For any other URI it will return 404 not found
}
program project1;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$apptype console}

uses
  cthreads, Classes, blcksock, synsock, sockets, Synautil, SysUtils, sha1, base64, contnrs,
  lcc_websocketserver, lcc_ethernetclient, crt;

//var
//  SocketList: TObjectList;

{@@
  Attends a connection. Reads the headers and gives an
  appropriate response
}
procedure AttendConnection(ASocket: TTCPBlockSocket);
var
  timeout: integer;
  s: string;
  method, uri, protocol, KeyValuePair: string;
  tempS: AnsiString;
  OutputDataString: string;
  ResultCode: integer;
  HeaderStrings: TStringList;
  Hash: TSHA1Digest;
  StringStream: TStringStream;
  Base64Stream: TBase64EncodingStream;
begin
  HeaderStrings := TStringList.Create;
  StringStream := TStringStream.Create('');
  Base64Stream := TBase64EncodingStream.Create(StringStream);

  timeout := 120000;

  WriteLn('Received headers+document from browser:');

  //read request line
  s := ASocket.RecvString(timeout);
  WriteLn(s);
  method := fetch(s, ' ');
  uri := fetch(s, ' ');
  protocol := fetch(s, ' ');

  //read request headers
  HeaderStrings.Clear;
  repeat
    s := ASocket.RecvString(Timeout);
    WriteLn(s);
    tempS := s;
    KeyValuePair := fetch(tempS, ' ');
    KeyValuePair := KeyValuePair + '=' + fetch(tempS, ' ');
    HeaderStrings.Add(KeyValuePair);
  until s = '';

  // Now write the document to the output stream
  if uri = '/' then
  begin
    if (HeaderStrings.Values['Upgrade:'] = 'websocket') and (HeaderStrings.Values['Sec-WebSocket-Protocol:'] = 'openlcb.websocket') then
    begin
      // Write the headers back to the client
      ASocket.SendString('HTTP/1.1 101 Switching Protocols' + CRLF);
      ASocket.SendString('Upgrade: websocket' + CRLF);
      ASocket.SendString('Connection: Upgrade' + CRLF);
      tempS := HeaderStrings.Values['Sec-WebSocket-Key:'] + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
      Hash := SHA1String(tempS);
      tempS := SHA1Print(Hash);   // Test only

      Base64Stream.Write(Hash, SizeOf(TSHA1Digest));
      Base64Stream.Flush;  // not multiple of 3 so need to pad
      tempS := StringStream.DataString;

      ASocket.SendString('Sec-WebSocket-Accept: ' + tempS + CRLF);
      ASocket.SendString('Sec-WebSocket-Protocol: openlcb.websocket' + CRLF);
      ASocket.SendString('' + CRLF);
    end else
    begin
      // Write the output document to the stream
      OutputDataString :=
        '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
        + ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + CRLF
        + '<html><h1>Teste</h1></html>' + CRLF;

      // Write the headers back to the client
      ASocket.SendString('HTTP/1.0 200' + CRLF);
      ASocket.SendString('Content-type: Text/Html' + CRLF);
      ASocket.SendString('Content-length: ' + IntTostr(Length(OutputDataString)) + CRLF);
      ASocket.SendString('Connection: close' + CRLF);
      ASocket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
      ASocket.SendString('Server: Servidor do Felipe usando Synapse' + CRLF);
      ASocket.SendString('' + CRLF);

    //  if ASocket.lasterror <> 0 then HandleError;

      // Write the document back to the browser
      ASocket.SendString(OutputDataString);
    end
  end else
    ASocket.SendString('HTTP/1.0 404' + CRLF);

  HeaderStrings.Free;
  Base64Stream.Free;    // Owns the string stream
end;

{$IFDEF Darwin}
const
 IP_SERVER = '10.0.3.198';   // OSX IP
{$ENDIF}
{$IFDEF Windows}
const
  IP_SERVER = '10.0.3.182';   // This IP
{$ENDIF}

 WEBSOCKET_OPCODE_CONTINUE = $00;
 WEBSOCKET_OPCODE_TEXT     = $01;
 WEBSOCKET_OPCODE_BINARY   = $02;
 WEBSOCKET_OPCODE_CLOSE    = $08;
 WEBSOCKET_OPCODE_PING     = $09;
 WEBSOCKET_OPCODE_PONG     = $0A;

 WEBSOCKET_CLOSE_HEADER_SERVER_LEN = 2;
 WEBSOCKET_CLOSE_HEADER_SERVER: array[0..WEBSOCKET_CLOSE_HEADER_SERVER_LEN-1] of Byte = ($88, $00);
 WEBSOCKET_CLOSE_HEADER_CLIENT_LEN = 6;
 WEBSOCKET_CLOSE_HEADER_CLIENT: array[0..WEBSOCKET_CLOSE_HEADER_CLIENT_LEN-1] of Byte = ($88, $00, $56, $78, $AB, $55);

var
//  ListenerSocket, ConnectionSocket: TTCPBlockSocket;
{  i, iHeader: Integer;
  PayloadSize, iPayload: QWord;
  AByte: Byte;
  HasMask, IsText, IsBinary, IsClose, IsContinuation, IsPing, IsPong: Boolean;
  Mask: array[0..3] of Byte;
  Msg: string; }
  WebSocketServer: TLccWebSocketServer;
  EthernetRec: TLccEthernetRec;
begin
 // SocketList := TObjectList.Create;
 // ListenerSocket := TTCPBlockSocket.Create;

  WebSocketServer := TLccWebSocketServer.Create(nil);
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.AutoResolveIP := True;
  EthernetRec.ListenerPort := 12021;
  EthernetRec.SuppressNotification := True;
  if Assigned(WebSocketServer.OpenConnection(EthernetRec)) then
  begin
    while False do
    begin

      if KeyPressed then
      begin
        if ReadKey = 'q' then Break;
      end;
    end;
  end;

 { ListenerSocket.CreateSocket;
  ListenerSocket.setLinger(true,10);
  ListenerSocket.bind(IP_SERVER,'12021');
  ListenerSocket.listen;
  repeat
  {  if ListenerSocket.canread(100) then
    begin
      ConnectionSocket := TTCPBlockSocket.Create;
      ConnectionSocket.Socket := ListenerSocket.accept;
      WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
      AttendConnection(ConnectionSocket);
      SocketList.Add(ConnectionSocket);
    end;
    for i := SocketList.Count - 1 downto 0 do
    begin
      ConnectionSocket := SocketList[i] as TTCPBlockSocket;
      AByte := ConnectionSocket.RecvByte(0);
      case ConnectionSocket.LastError of
        0 :
          begin
            // We assume we are in sync and this is the start of the Message
            IsBinary := AByte and $0F = WEBSOCKET_OPCODE_BINARY;
            IsText := AByte and $0F = WEBSOCKET_OPCODE_TEXT;
            IsClose := AByte and $0F = WEBSOCKET_OPCODE_CLOSE;
            IsContinuation := AByte and $0F = WEBSOCKET_OPCODE_CONTINUE;
            IsPing := AByte and $0F = WEBSOCKET_OPCODE_PING;
            IsPong := AByte and $0F = WEBSOCKET_OPCODE_PONG;

            if (AByte and $80 = $80) then  // Last Frame of a concatinated message
            begin
              AByte := ConnectionSocket.RecvByte(0);
              PayloadSize := AByte and $7F;
              HasMask := AByte and $80 = $80;
              if PayloadSize = 126 then
              begin
                PayloadSize := ConnectionSocket.RecvByte(0) shl 8;
                PayloadSize := PayloadSize or ConnectionSocket.RecvByte(0);
              end else
              if PayloadSize = 127 then
              begin
                PayloadSize := ConnectionSocket.RecvByte(0) shl 56;
                PayloadSize := PayloadSize or (ConnectionSocket.RecvByte(0) shl 48);
                PayloadSize := PayloadSize or (ConnectionSocket.RecvByte(0) shl 40);
                PayloadSize := PayloadSize or (ConnectionSocket.RecvByte(0) shl 32);
                PayloadSize := PayloadSize or (ConnectionSocket.RecvByte(0) shl 24);
                PayloadSize := PayloadSize or (ConnectionSocket.RecvByte(0) shl 16);
                PayloadSize := PayloadSize or (ConnectionSocket.RecvByte(0) shl 8);
                PayloadSize := PayloadSize or ConnectionSocket.RecvByte(0);
              end;
              if HasMask then
              begin
                Mask[0] := ConnectionSocket.RecvByte(0);
                Mask[1] := ConnectionSocket.RecvByte(0);
                Mask[2] := ConnectionSocket.RecvByte(0);
                Mask[3] := ConnectionSocket.RecvByte(0);
              end;
              Msg := '';
              iPayload := 0;
              while iPayload < PayloadSize do
              begin
                AByte := ConnectionSocket.RecvByte(0);
                AByte := AByte XOR Mask[iPayload mod 4];
                Msg := Msg + chr(AByte);
                Inc(iPayload);
              end;
            end;
            if IsClose then
            begin
              ConnectionSocket.ResetLastError;
              for iHeader := 0 to WEBSOCKET_CLOSE_HEADER_SERVER_LEN - 1 do
                ConnectionSocket.SendByte(WEBSOCKET_CLOSE_HEADER_SERVER[iHeader]);
              SocketList.Remove(ConnectionSocket);
            end else
            if IsPing then
            begin
              WriteLn('Pong detected');
            end else
            if IsPong then
            begin
              WriteLn('Pong detected');
            end else
              WriteLn(Msg);
          end;
        WSAETIMEDOUT :
          begin

          end;
        WSAECONNRESET   :
          begin
            ConnectionSocket.ResetLastError;
            SocketList.Remove(ConnectionSocket);
          end
      end;
    end;
    if KeyPressed then
    begin
      if ReadKey = 'q' then Break;
    end;
  until false;    }

  WriteLn('Quiting');
  WebSocketServer.Free;
//  ListenerSocket.Free;
//  SocketList.Free;
end.

