unit lcc_ethernet_websocket;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
  //  {$DEFINE LOGGING}
  {$ENDIF}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP} LResources, Forms, Controls, Graphics, Dialogs, {$ENDIF}
  {$ELSE}
  FMX.Forms, Types, System.Generics.Collections,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging,
  lcc_detailed_logging,
  {$ENDIF}

  {$IFDEF ULTIBO}
  lcc_threaded_stringlist,
  Winsock2,
  Console,
  {$ELSE}
  blcksock,
  synsock,
  Synautil,
    {$IFNDEF DELPHI}
    Base64,
    sha1,
    {$ENDIF}
  {$ENDIF}
  lcc_threaded_stringlist,
  lcc_gridconnect,
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_ethernet_client,
  lcc_app_common_settings,
  lcc_node_messages_can_assembler_disassembler,
  lcc_common_classes,
  lcc_ethernet_common,
  lcc_ethernet_server;



const
  LCC_WEBSOCKET_OPCODE_CONTINUE = $00;
  LCC_WEBSOCKET_OPCODE_TEXT     = $01;
  LCC_WEBSOCKET_OPCODE_BINARY   = $02;
  LCC_WEBSOCKET_OPCODE_CLOSE    = $08;
  LCC_WEBSOCKET_OPCODE_PING     = $09;
  LCC_WEBSOCKET_OPCODE_PONG     = $0A;

  LCC_WEBSOCKET_CLOSE_HEADER_SERVER_LEN = 2;
  LCC_WEBSOCKET_CLOSE_HEADER_SERVER: array[0..LCC_WEBSOCKET_CLOSE_HEADER_SERVER_LEN-1] of Byte = ($88, $80);
  LCC_WEBSOCKET_CLOSE_HEADER_CLIENT_LEN = 6;
  LCC_WEBSOCKET_CLOSE_HEADER_CLIENT: array[0..LCC_WEBSOCKET_CLOSE_HEADER_CLIENT_LEN-1] of Byte = ($88, $00, $56, $78, $AB, $55);


type

  { TLccWebSocketServerThread }

  TLccWebSocketServerThread =  class(TLccEthernetServerThread)
  private
   FWebSocketInitialized: Boolean;
  protected
    procedure BuildAndSendInitializeSuccessReply(ASocket: TTCPBlockSocket; SecWebSocketKey: string);
    procedure BuildAndSendInitializeFailureReply(ASocket: TTCPBlockSocket);
    procedure Execute; override;
    procedure ExtractInitializationRequest(ASocket: TTCPBlockSocket; var Method, URL, Protocol: string);
    function ExtractInitializationHeaderKeys(ASocket: TTCPBlockSocket): TStringList;
   function ReceiveWebSocketStateMachine(ASocket: TTCPBlockSocket): TLccDynamicByteArray;
    procedure SendWebSocketStateMachine(ASocket: TTCPBlockSocket; OutStr: string);

    property WebSocketInitialized: Boolean read FWebSocketInitialized write FWebSocketInitialized;
  end;


  { TLccWebSocketListener }

  TLccWebSocketListener = class(TLccEthernetListener)
  protected
    {$IFDEF ULTIBO}
    {$ELSE}
    function CreateThreadObject: TLccEthernetServerThread; override;
    {$ENDIF}
  end;

  { TLccWebsocketServer }

  TLccWebsocketServer = class(TLccEthernetServer)
  protected
    function CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener; override;
    function IsLccLink: Boolean; override;
  end;

implementation

{ TLccWebSocketListener }

function TLccWebSocketListener.CreateThreadObject: TLccEthernetServerThread;
begin
  Result := TLccWebSocketServerThread.Create(True, Owner, ConnectionInfo)
end;

{ TLccWebSocketServerThread }

procedure TLccWebSocketServerThread.BuildAndSendInitializeSuccessReply(ASocket: TTCPBlockSocket; SecWebSocketKey: string);
var
  Hash: TSHA1Digest;
  StringStream: TStringStream;
  Base64Stream: TBase64EncodingStream;
  tempS: string;
begin
  StringStream := TStringStream.Create('');
  Base64Stream := TBase64EncodingStream.Create(StringStream);

  ASocket.SendString('HTTP/1.1 101 Switching Protocols' + CRLF);
  ASocket.SendString('Upgrade: websocket' + CRLF);
  ASocket.SendString('Connection: Upgrade' + CRLF);
  tempS := SecWebSocketKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  Hash := SHA1String(tempS);
  tempS := SHA1Print(Hash);   // Test only

  Base64Stream.Write(Hash, SizeOf(TSHA1Digest));
  Base64Stream.Flush;  // not multiple of 3 so need to pad
  tempS := StringStream.DataString;

  ASocket.SendString('Sec-WebSocket-Accept: ' + tempS + CRLF);
  ASocket.SendString('Sec-WebSocket-Protocol: openlcb.websocket' + CRLF);
  ASocket.SendString('' + CRLF);

  FreeAndNil(StringStream);
  FreeAndNil(Base64Stream);

end;

procedure TLccWebSocketServerThread.BuildAndSendInitializeFailureReply(ASocket: TTCPBlockSocket);
begin
  // TODO
end;

procedure TLccWebSocketServerThread.Execute;
var
  TxStr: String;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList, RxList: TStringList;
  DynamicByteArray: TLccDynamicByteArray;
  LocalSleepCount, i: Integer;
  HeaderStrings: TStringList;
  RequestMethod, RequestURL, RequestProtocol: string;
begin
  FRunning := True;

  HandleSendConnectionNotification(lcsConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  if (ConnectionInfo as TLccEthernetConnectionInfo).LingerTime > 0 then
    Socket.SetLinger(True, (ConnectionInfo as TLccEthernetConnectionInfo).LingerTime);
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := (ConnectionInfo as TLccEthernetConnectionInfo).HeartbeatRate;
  Socket.SetTimeout(0);
  Socket.Socket := ListenerSocketHandle;    // Read back the handle
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    GridConnectHelper.Free;
    FRunning := False
  end else
  begin
    (ConnectionInfo as TLccEthernetConnectionInfo).ClientIP := Socket.GetRemoteSinIP;
    (ConnectionInfo as TLccEthernetConnectionInfo).ClientPort := Socket.GetRemoteSinPort;
    (ConnectionInfo as TLccEthernetConnectionInfo).ListenerIP := Socket.GetLocalSinIP;
    (ConnectionInfo as TLccEthernetConnectionInfo).ListenerPort := Socket.GetLocalSinPort;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      GridConnectHelper.Free;
      FRunning := False
    end else
    begin
      HandleSendConnectionNotification(lcsConnected);
      try
        try
          LocalSleepCount := 0;
          while not IsTerminated and (ConnectionInfo.ConnectionState = lcsConnected) do
          begin

            if not WebSocketInitialized then
            begin
              RequestMethod := '';
              RequestURL := '';
              RequestProtocol := '';
              ExtractInitializationRequest(Socket, RequestMethod, RequestURL, RequestProtocol);
              HeaderStrings := ExtractInitializationHeaderKeys(Socket);

              if (RequestURL = '/') and (HeaderStrings.Values['Upgrade:'] = 'websocket') and (HeaderStrings.Values['Sec-WebSocket-Protocol:'] = 'openlcb.websocket') then
                BuildAndSendInitializeSuccessReply(Socket, HeaderStrings.Values['Sec-WebSocket-Key:'])
              else begin
                BuildAndSendInitializeFailureReply(Socket);
                HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
              end;

              FreeAndNil(HeaderStrings);
              WebSocketInitialized := True;
            end;

            if not Terminated then
            begin
              // Handle the Socket using GridConnect
              if ConnectionInfo.Gridconnect then
              begin
                if LocalSleepCount >= ConnectionInfo.SleepCount then
                begin
                  TxStr := '';
                  TxList := OutgoingGridConnect.LockList;
                  try
                    if TxList.Count > 0 then
                    begin
                      TxStr := TxList[0];
                      TxList.Delete(0);
                    end;
                  finally
                    OutgoingGridConnect.UnlockList;
                  end;

                  if (TxStr <> '') and WebSocketInitialized then
                    SendWebSocketStateMachine(Socket, TxStr);
                  LocalSleepCount := 0;
                end;
                Inc(LocalSleepCount);

                if not Terminated then
                begin
                  DynamicByteArray := ReceiveWebSocketStateMachine(Socket);
                  if Length(DynamicByteArray) > 0 then
                  begin
                    GridConnectStrPtr := nil;
                    for i := 0 to Length(DynamicByteArray) - 1 do
                    begin
                      if GridConnectHelper.GridConnect_DecodeMachine(DynamicByteArray[i], GridConnectStrPtr) and WebSocketInitialized then
                      begin
                        ConnectionInfo.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                        ConnectionInfo.LccMessage.LoadByGridConnectStr(ConnectionInfo.MessageStr);

                        case GridConnectMessageAssembler.IncomingMessageGridConnect(ConnectionInfo.LccMessage) of
                            imgcr_True :
                              begin
                                if ConnectionInfo.UseSyncronize  then
                                  Synchronize({$IFDEF FPC}@{$ENDIF}ReceiveMessage)
                                else begin
                                  RxList := Owner.IncomingGridConnect.LockList;
                                  try
                                    RxList.Add(ConnectionInfo.LccMessage.ConvertToGridConnectStr('', False));
                                  finally
                                    Owner.IncomingGridConnect.UnlockList;
                                  end;
                                end;
                              end;
                            imgcr_False,
                            imgcr_ErrorToSend,
                            imgcr_UnknownError : begin end;
                         end;
                      end;
                    end;
                  end
                end;
              end else
              begin    // Handle the Socket with LCC TCP Protocol
              {
                if LocalSleepCount >= SleepCount then
                begin
                  DynamicByteArray := nil;
                  OutgoingCircularArray.LockArray;
                  try
                    if OutgoingCircularArray.Count > 0 then
                      OutgoingCircularArray.PullArray(DynamicByteArray);
                  finally
                    OutgoingCircularArray.UnLockArray;
                  end;

                  if Length(DynamicByteArray) > 0 then
                  begin
                    Socket.SendBuffer(@DynamicByteArray[0], Length(DynamicByteArray));
                    if Socket.LastError <> 0 then
                      HandleErrorAndDisconnect;
                    DynamicByteArray := nil;
                  end;
                  LocalSleepCount := 0;
                end;
                Inc(LocalSleepCount);

                RcvByte := Socket.RecvByte(1);
                case Socket.LastError of
                  0 :
                    begin
                      DynamicByteArray := nil;
                      if TcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(RcvByte, FEthernetRec.MessageArray) then
                        Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage);
                    end;
                  WSAETIMEDOUT :
                    begin

                    end;
                  WSAECONNRESET   :
                    begin
                      if not (ConnectionInfo as TLccEthernetConnectionInfo).SuppressConnectionResetError then
                        HandleErrorAndDisconnect
                    end
                else
                  HandleErrorAndDisconnect
                end;    }
              end
            end;
          end;
        finally
          HandleSendConnectionNotification(lcsDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        HandleSendConnectionNotification(lcsDisconnected);
        (Owner as TLccEthernetHardwareConnectionManager).ConnectionThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

procedure TLccWebSocketServerThread.ExtractInitializationRequest(ASocket: TTCPBlockSocket; var Method, URL, Protocol: string);
var
  s: String;
  StrList: TSTringList;
begin
  s := ASocket.RecvString(120000);

  s := StringReplace(s, ' ', #10, [rfReplaceAll]);
  StrList := TStringList.Create;
  StrList.Delimiter := #10;
  StrList.DelimitedText := s;
  if StrList.Count = 3 then
  begin
    Method := StrList[0];
    URL := StrList[1];
    Protocol := StrList[2];
  end else
  begin
    Method := '';
    URL := '';
    Protocol := ''
  end;
end;

function TLccWebSocketServerThread.ExtractInitializationHeaderKeys(ASocket: TTCPBlockSocket): TStringList;
var
  s: string;
begin
  Result := TStringList.Create;
  repeat
    s := ASocket.RecvString(120000);
    s := StringReplace(s, ' ', '=', []);
    Result.Add(s);
  until s = '';
end;

function TLccWebSocketServerThread.ReceiveWebSocketStateMachine(ASocket: TTCPBlockSocket): TLccDynamicByteArray;
var
  AByte: Byte;
  iHeader: Integer;
  PayloadSize, iPayload: QWord;
  HasMask, IsClose, IsPing, IsPong: Boolean;
  Mask: array[0..3] of Byte;
begin
  Result := nil;
  AByte := ASocket.RecvByte(0);
  case ASocket.LastError of
    0 :
      begin
        // We assume we are in sync and this is the start of the Message
  //      IsBinary := AByte and $0F = WEBSOCKET_OPCODE_BINARY;
  //      IsText := AByte and $0F = WEBSOCKET_OPCODE_TEXT;
        IsClose := AByte and $0F = LCC_WEBSOCKET_OPCODE_CLOSE;
  //      IsContinuation := AByte and $0F = WEBSOCKET_OPCODE_CONTINUE;
        IsPing := AByte and $0F = LCC_WEBSOCKET_OPCODE_PING;
        IsPong := AByte and $0F = LCC_WEBSOCKET_OPCODE_PONG;

        if (AByte and $80 = $80) then  // Last Frame of a concatinated message
        begin
          AByte := ASocket.RecvByte(0);
          PayloadSize := AByte and $7F;
          HasMask := AByte and $80 = $80;
          if PayloadSize = 126 then
          begin
            PayloadSize := ASocket.RecvByte(0) shl 8;
            PayloadSize := PayloadSize or ASocket.RecvByte(0);
          end else
          if PayloadSize = 127 then
          begin
            PayloadSize := ASocket.RecvByte(0) shl 56;
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 48);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 40);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 32);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 24);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 16);
            PayloadSize := PayloadSize or (ASocket.RecvByte(0) shl 8);
            PayloadSize := PayloadSize or ASocket.RecvByte(0);
          end;
          if HasMask then
          begin
            Mask[0] := ASocket.RecvByte(0);
            Mask[1] := ASocket.RecvByte(0);
            Mask[2] := ASocket.RecvByte(0);
            Mask[3] := ASocket.RecvByte(0);
          end;
          iPayload := 0;
          SetLength(Result, PayloadSize);
          while iPayload < PayloadSize do
          begin
            AByte := ASocket.RecvByte(0);
            Result[iPayload] := AByte XOR Mask[iPayload mod 4];
            Inc(iPayload);
          end;
        end;
        if IsClose then
        begin
          ASocket.ResetLastError;
          for iHeader := 0 to LCC_WEBSOCKET_CLOSE_HEADER_SERVER_LEN - 1 do
            ASocket.SendByte(LCC_WEBSOCKET_CLOSE_HEADER_SERVER[iHeader]);
          HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages)
        end else
        if IsPing then
        begin

        end else
        if IsPong then
        begin

        end
      end;
    WSAETIMEDOUT :
      begin

      end;
    WSAECONNRESET   :
      begin
        HandleErrorAndDisconnect((ConnectionInfo.SuppressErrorMessages) or (ConnectionInfo as TLccEthernetConnectionInfo).SuppressConnectionResetError)
      end
  end;
end;

procedure TLccWebSocketServerThread.SendWebSocketStateMachine(ASocket: TTCPBlockSocket; OutStr: string);
var
  i: Integer;
begin
  // No mask from Server to Client
  ASocket.SendByte($80 or LCC_WEBSOCKET_OPCODE_TEXT);
  ASocket.SendByte(Length(OutStr));
  {$IFDEF LCC_MOBILE}
   for i := 0 to Length(OutStr) - 1 do
    ASocket.SendByte(Ord(OutStr[i]));
  {$ELSE}
  for i := 1 to Length(OutStr) do
    ASocket.SendByte(Ord(OutStr[i]));
  {$ENDIF}

 if ASocket.LastError <> 0 then
   HandleErrorAndDisconnect(ConnectionInfo.SuppressErrorMessages);
end;

{ TLccWebsocketServer }

function TLccWebsocketServer.CreateListenerObject(
  AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener;
begin
  Result := TLccWebSocketListener.Create(True, Self, AConnectionInfo)
end;

function TLccWebsocketServer.IsLccLink: Boolean;
begin
  Result := True;
end;


end.

