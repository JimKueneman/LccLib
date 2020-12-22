unit lcc_ethernet_server;

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
  Base64,
  sha1,
  {$ENDIF}
  lcc_threaded_circulararray,
  lcc_threaded_stringlist,
  lcc_gridconnect,
  lcc_utilities,
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_ethernet_client,
  lcc_ethernet_tcp,
  lcc_app_common_settings,
  lcc_common_classes,
  lcc_node_messages_can_assembler_disassembler;

type
  TLccEthernetServerThread = class;     // Forward
  TLccEthernetServer = class;

  { TLccEthernetServerThread }

  TLccEthernetServerThread =  class(TLccConnectionThread)
    private
      FEthernetRec: TLccEthernetRec;
      FGridConnectMessageAssembler: TLccGridConnectMessageAssembler;
      FOnClientDisconnect: TOnEthernetRecFunc;
      FOnErrorMessage: TOnEthernetRecFunc;
      FOnConnectionStateChange: TOnEthernetRecFunc;
      FOnReceiveMessage: TOnEthernetReceiveFunc;
      FOnSendMessage: TOnMessageEvent;
      FOwner: TLccEthernetServer;
      {$IFDEF ULTIBO}
      {$ELSE}
      FSocket: TTCPBlockSocket;
      FSocketHandleForListener: TSocket;
      {$ENDIF}
    protected
      procedure DoConnectionState;
      procedure DoErrorMessage;
      procedure DoReceiveMessage;
      procedure DoSendMessage(AMessage: TLccMessage);
      procedure Execute; override;
      procedure HandleErrorAndDisconnect;
      procedure HandleSendConnectionNotification(NewConnectionState: TConnectionState);
      procedure SendMessage(AMessage: TLccMessage);

      property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
      {$IFDEF ULTIBO}
      {$ELSE}
      property Socket: TTCPBlockSocket read FSocket write FSocket;
      property SocketHandleForListener: TSocket read FSocketHandleForListener write FSocketHandleForListener;
      {$ENDIF}
      property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
      property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
      property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
      property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
      property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
      property Owner: TLccEthernetServer read FOwner write FOwner;
      property GridConnectMessageAssembler: TLccGridConnectMessageAssembler read FGridConnectMessageAssembler write FGridConnectMessageAssembler;
    public
      constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec); reintroduce;
      destructor Destroy; override;
  end;

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
    function ReceiveWebSocketStateMachine(ASocket: TTCPBlockSocket): TDynamicByteArray;
    procedure SendWebSocketStateMachine(ASocket: TTCPBlockSocket; OutStr: string);

    property WebSocketInitialized: Boolean read FWebSocketInitialized write FWebSocketInitialized;
  end;

   { TLccHTTPServerThread }

   TLccHTTPServerThread = class(TLccEthernetServerThread)
     procedure Execute; override;
   end;

  { TLccEthernetThreadList }

  TLccEthernetThreadList = class(TThreadList)      // Contains TLccEthernetServerThread and decendent objects
  private
    function GetCount: Integer;
  public
    destructor Destroy; override;
    procedure CloseEthernetPorts;
    procedure CloseEthernetPort(EthernetThread: TLccEthernetServerThread);

    property Count: Integer read GetCount;
  end;

  { TLccEthernetListener }

  TLccEthernetListener = class(TThread)
  private
    FEthernetRec: TLccEthernetRec;
    FGridConnect: Boolean;
    FOnClientDisconnect: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FOwner: TLccEthernetServer;
    FRunning: Boolean;
    FSleepCount: Integer;
    {$IFDEF ULTIBO}
    FStringList: TThreadStringList;
    FTcpServer: TWinsock2TCPServer;
    {$ELSE}
    FSocket: TTCPBlockSocket;
    {$ENDIF}
    function GetIsTerminated: Boolean;
  protected
    property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
    property Owner: TLccEthernetServer read FOwner write FOwner;
    property Running: Boolean read FRunning write FRunning;
    {$IFDEF ULTIBO}
    property StringList: TThreadStringList read FStringList write FStringList;
    property TcpServer: TWinsock2TCPServer read FTcpServer write FTcpServer;
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
    property IsTerminated: Boolean read GetIsTerminated;

    {$IFDEF ULTIBO}
    {$ELSE}
    function CreateServerThread(ASocketHandle: TSocket): TLccEthernetServerThread;
    function CreateThreadObject: TLccEthernetServerThread; virtual;
    {$ENDIF}
    procedure DoConnectionState;
    procedure DoErrorMessage;
    procedure DoReceiveMessage;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec); reintroduce; virtual;
    destructor Destroy; override;

    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write FSleepCount;
  end;


  { TLccWebSocketListener }

  TLccWebSocketListener = class(TLccEthernetListener)
  protected
    {$IFDEF ULTIBO}
    {$ELSE}
    function CreateThreadObject: TLccEthernetServerThread; override;
    {$ENDIF}
  end;

  { TLccHTTPListener }

  TLccHTTPListener = class(TLccEthernetListener)
  protected
    {$IFDEF ULTIBO}
    {$ELSE}
    function CreateThreadObject: TLccEthernetServerThread; override;
    {$ENDIF}
  end;

  { TLccEthernetServer }

  TLccEthernetServer = class(TLccHardwareConnectionManager)
  private
    FEthernetThreads: TLccEthernetThreadList;
    FGridConnect: Boolean;
    FHub: Boolean;
    FLccSettings: TLccSettings;
    FListenerThread: TLccEthernetListener;
    {$IFDEF LOGGING}FLoggingFrame: TFrameLccLogging;{$ENDIF}
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    FSiblingEthernetThreads: TThreadList;
    FSleepCount: Integer;
    FUseSynchronize: Boolean;    // If set the threads will call back on a Syncronize call else incoming messages are put in the IncomingGridConnect or IncomingCircularArray buffers and the app needs to poll this buffer
    function GetConnected: Boolean;
    procedure SetGridConnect(AValue: Boolean);
    procedure SetSleepCount(AValue: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateThreadEvents(EthernetThread: TLccEthernetServerThread);
    procedure UpdateThreadsEvents;
    procedure UpdateListenerEvents(AListenerThread: TLccEthernetListener);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
    function OpenConnectionWithLccSettings: TLccEthernetListener;
    function CreateListenerObject(AnEthernetRec: TLccEthernetRec): TLccEthernetListener; virtual;
    procedure CloseConnection( EthernetThread: TLccEthernetServerThread);
    procedure SendMessage(AMessage: TLccMessage);  override;
    procedure SendMessageRawGridConnect(GridConnectStr: String); override;

    procedure RegisterSiblingEthernetServer(AnEthenetServer: TLccEthernetServer);
    procedure UnRegisterSiblingEthernetServer(AnEthernetServer: TLccEthernetServer);

    property Connected: Boolean read GetConnected;
    property EthernetThreads: TLccEthernetThreadList read FEthernetThreads write FEthernetThreads;
    {$IFDEF LOGGING}property LoggingFrame: TFrameLccLogging read FLoggingFrame write FLoggingFrame;{$ENDIF}     // Designtime can't find Frames to assign in Object Inspector
    property ListenerThread: TLccEthernetListener read FListenerThread write FListenerThread;
    property SiblingEthernetThreads: TThreadList read FSiblingEthernetThreads write FSiblingEthernetThreads;

  published
    { Published declarations }
    property Hub: Boolean read FHub write FHub;
    property Gridconnect: Boolean read FGridConnect write SetGridConnect;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnReceiveMessage: TOnEthernetReceiveFunc read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property SleepCount: Integer read FSleepCount write SetSleepCount;
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
  end;

  TLccWebsocketServer = class(TLccEthernetServer)
  protected
    function CreateListenerObject(AnEthernetRec: TLccEthernetRec): TLccEthernetListener; override;
  end;

  TLccHTTPServer = class(TLccEthernetServer)
  protected
    function CreateListenerObject(AnEthernetRec: TLccEthernetRec): TLccEthernetListener; override;
  end;

procedure Register;

implementation

const
 WEBSOCKET_OPCODE_CONTINUE = $00;
 WEBSOCKET_OPCODE_TEXT     = $01;
 WEBSOCKET_OPCODE_BINARY   = $02;
 WEBSOCKET_OPCODE_CLOSE    = $08;
 WEBSOCKET_OPCODE_PING     = $09;
 WEBSOCKET_OPCODE_PONG     = $0A;

 WEBSOCKET_CLOSE_HEADER_SERVER_LEN = 2;
 WEBSOCKET_CLOSE_HEADER_SERVER: array[0..WEBSOCKET_CLOSE_HEADER_SERVER_LEN-1] of Byte = ($88, $80);
 WEBSOCKET_CLOSE_HEADER_CLIENT_LEN = 6;
 WEBSOCKET_CLOSE_HEADER_CLIENT: array[0..WEBSOCKET_CLOSE_HEADER_CLIENT_LEN-1] of Byte = ($88, $00, $56, $78, $AB, $55);

procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
  {$IFDEF FPC}
 //JDK {$I TLccEthernetServer.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccEthernetServer]);
  {$ENDIF}
end;

{ TLccHTTPServer }

function TLccHTTPServer.CreateListenerObject(AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
begin
  Result := TLccHTTPListener.Create(True, Self, AnEthernetRec);
end;

{ TLccHTTPServerThread }

procedure TLccHTTPServerThread.Execute;
begin
  inherited Execute;
end;

{ TLccHTTPListener }

function TLccHTTPListener.CreateThreadObject: TLccEthernetServerThread;
begin
  Result := TLccHTTPServerThread.Create(True, Owner, FEthernetRec);
end;

{ TLccWebsocketServer }

function TLccWebsocketServer.CreateListenerObject(AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
begin
  Result := TLccWebSocketListener.Create(True, Self, AnEthernetRec)
end;

{ TLccWebSocketListener }

function TLccWebSocketListener.CreateThreadObject: TLccEthernetServerThread;
begin
  Result := TLccWebSocketServerThread.Create(True, Owner, FEthernetRec)
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

end;

procedure TLccWebSocketServerThread.Execute;
var
  TxStr: String;
  RcvByte: Byte;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList, RxList: TStringList;
  DynamicByteArray: TDynamicByteArray;
  LocalSleepCount, i: Integer;
  HeaderStrings: TStringList;
  RequestMethod, RequestURL, RequestProtocol: string;
begin
  FRunning := True;

  HandleSendConnectionNotification(ccsListenerClientConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  Socket.Socket := SocketHandleForListener;    // Read back the handle
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    GridConnectHelper.Free;
    FRunning := False
  end else
  begin
    FEthernetRec.ClientIP := Socket.GetRemoteSinIP;
    FEthernetRec.ClientPort := Socket.GetRemoteSinPort;
    FEthernetRec.ListenerIP := Socket.GetLocalSinIP;
    FEthernetRec.ListenerPort := Socket.GetLocalSinPort;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      GridConnectHelper.Free;
      FRunning := False
    end else
    begin
      HandleSendConnectionNotification(ccsListenerClientConnected);
      try
        try
          LocalSleepCount := 0;
          while not IsTerminated and (FEthernetRec.ConnectionState = ccsListenerClientConnected) do
          begin

            if not WebSocketInitialized then
            begin
              ExtractInitializationRequest(Socket, RequestMethod, RequestURL, RequestProtocol);
              HeaderStrings := ExtractInitializationHeaderKeys(Socket);

              if (RequestURL = '/') and (HeaderStrings.Values['Upgrade:'] = 'websocket') and (HeaderStrings.Values['Sec-WebSocket-Protocol:'] = 'openlcb.websocket') then
                BuildAndSendInitializeSuccessReply(Socket, HeaderStrings.Values['Sec-WebSocket-Key:'])
              else begin
                BuildAndSendInitializeFailureReply(Socket);
                HandleErrorAndDisconnect;
              end;

              FreeAndNil(HeaderStrings);
              WebSocketInitialized := True;
            end;

            if not Terminated then
            begin
              // Handle the Socket using GridConnect
              if Gridconnect then
              begin
                if LocalSleepCount >= SleepCount then
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
                        FEthernetRec.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                        FEthernetRec.LccMessage.LoadByGridConnectStr(FEthernetRec.MessageStr);

                        case GridConnectMessageAssembler.IncomingMessageGridConnect(FEthernetRec.LccMessage) of
                            imgcr_True :
                              begin
                                if UseSynchronize then
                                  Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage)
                                else begin
                                  RxList := Owner.IncomingGridConnect.LockList;
                                  try
                                    RxList.Add(FEthernetRec.LccMessage.ConvertToGridConnectStr('', False));
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
                      HandleErrorAndDisconnect;
                    end
                else
                  HandleErrorAndDisconnect
                end;    }
              end
            end;
          end;
        finally
          HandleSendConnectionNotification(ccsListenerClientDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        HandleSendConnectionNotification(ccsListenerClientDisconnected);
        Owner.EthernetThreads.Remove(Self);
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

function TLccWebSocketServerThread.ReceiveWebSocketStateMachine(ASocket: TTCPBlockSocket): TDynamicByteArray;
var
  AByte: Byte;
  iHeader: Integer;
  PayloadSize, iPayload: QWord;
  HasMask, IsText, IsBinary, IsClose, IsContinuation, IsPing, IsPong: Boolean;
  Mask: array[0..3] of Byte;
begin
  Result := nil;
  AByte := ASocket.RecvByte(0);
  case ASocket.LastError of
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
          for iHeader := 0 to WEBSOCKET_CLOSE_HEADER_SERVER_LEN - 1 do
            ASocket.SendByte(WEBSOCKET_CLOSE_HEADER_SERVER[iHeader]);
          HandleErrorAndDisconnect
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
        ASocket.ResetLastError;
        HandleErrorAndDisconnect
      end
  end;
end;

procedure TLccWebSocketServerThread.SendWebSocketStateMachine(ASocket: TTCPBlockSocket; OutStr: string);
var
  i: Integer;
begin
  // No mask from Server to Client
  ASocket.SendByte($80 or WEBSOCKET_OPCODE_TEXT);
  ASocket.SendByte(Length(OutStr));
  {$IFDEF LCC_MOBILE}
   for i := 0 to Length(OutStr) - 1 do
    ASocket.SendByte(Ord(OutStr[i]));
  {$ELSE}
  for i := 1 to Length(OutStr) do
    ASocket.SendByte(Ord(OutStr[i]));
  {$ENDIF}

 if ASocket.LastError <> 0 then
   HandleErrorAndDisconnect;
end;

{ TLccEthernetListener }

constructor TLccEthernetListener.Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := nil;
  FEthernetRec.LccMessage := TLccMessage.Create;
//  FOutgoingGridConnect := TThreadStringList.Create;
 // FMsgAssembler := TLccMessageAssembler.Create;
//  FMsgDisAssembler := TLccMessageDisAssembler.Create;
 // FWorkerMsg := TLccMessage.Create;
end;

{$IFDEF ULTIBO}
{$ELSE}
function TLccEthernetListener.CreateServerThread(ASocketHandle: TSocket): TLccEthernetServerThread;
begin
  Result := CreateThreadObject;
  Result.SocketHandleForListener := ASocketHandle;    // Back create the sockets with this handle
  Result.OnConnectionStateChange := OnConnectionStateChange;
  Result.OnErrorMessage := OnErrorMessage;
  Result.OnReceiveMessage := OnReceiveMessage;
  Result.OnSendMessage := OnSendMessage;
  Result.SleepCount := FSleepCount;
  Result.GridConnect := FGridConnect;
  Result.UseSynchronize := Owner.UseSynchronize;
  Result.Start;
end;

function TLccEthernetListener.CreateThreadObject: TLccEthernetServerThread;
begin
   Result := TLccEthernetServerThread.Create(True, Owner, FEthernetRec);
end;

{$ENDIF}

destructor TLccEthernetListener.Destroy;
begin
//  FreeAndNil( FOutgoingGridConnect);
  FreeAndNil(FEthernetRec.LccMessage);
 // FreeAndNil(FMsgAssembler);
 // FreeandNil(FMsgDisAssembler);
//  FreeAndNIl(FWorkerMsg);
  inherited Destroy;
end;

procedure TLccEthernetListener.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccEthernetListener.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccEthernetListener.DoReceiveMessage;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    // Send all raw GridConnect Messages to the event
    if Assigned(OnReceiveMessage) then
      OnReceiveMessage(Self, FEthernetRec);
  end
end;

{$IFDEF ULTIBO}
procedure TLccEthernetListener.Execute;
begin

end;
{$ELSE}
procedure TLccEthernetListener.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FEthernetRec.ConnectionState := NewConnectionState;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
  end;

  procedure HandleErrorAndDisconnect;
  begin
    Owner.EthernetThreads.Remove(Self);
    FEthernetRec.ErrorCode := Socket.LastError;
    FEthernetRec.MessageStr := Socket.LastErrorDesc;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
    SendConnectionNotification(ccsListenerDisconnected);
    Terminate
  end;

var
  NewLink: TLccEthernetServerThread;
begin
  FRunning := True;

  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  SendConnectionNotification(ccsListenerConnecting);

  if FEthernetRec.AutoResolveIP then
  begin
    {$IFDEF LCC_WINDOWS}
    FEthernetRec.ListenerIP := ResolveWindowsIp(Socket);
    {$ELSE}
    FEthernetRec.ListenerIP := ResolveUnixIp;
    {$ENDIF}
  end;

  Socket.Bind(String( EthernetRec.ListenerIP), String( IntToStr(EthernetRec.ListenerPort)));
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    FRunning := False
  end else
  begin
    Socket.Listen;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      FRunning := False
    end else
    begin
      SendConnectionNotification(ccsListenerConnected);
      try
        try
          while not Terminated and (FEthernetRec.ConnectionState = ccsListenerConnected) do
          begin
            if Socket.CanRead(1000) then
            begin
              if not Terminated and (Socket.LastError <> WSAETIMEDOUT) then
              begin
                if Socket.LastError = 0 then
                begin
                  NewLink := CreateServerThread(Socket.Accept);
                  if Assigned(NewLink) then
                    Owner.EthernetThreads.Add(NewLink);
                end else
                  Terminate;
              end
            end
          end;
        finally
          SendConnectionNotification(ccsListenerDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
        end;
      finally
        SendConnectionNotification(ccsListenerDisconnected);
        FRunning := False;
      end;
    end;
  end;
end;
{$ENDIF}

function TLccEthernetListener.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

{ TLccEthernetThreadList }

function TLccEthernetThreadList.GetCount: Integer;
var
  L: TList;
begin
  L := LockList;
  try
    Result := L.Count
  finally
    UnlockList;
  end;
end;

destructor TLccEthernetThreadList.Destroy;
begin
  CloseEthernetPorts;
  inherited Destroy;
end;

procedure TLccEthernetThreadList.CloseEthernetPorts;
var
  L: TList;
  Thread: TLccEthernetServerThread;
begin
  while Count > 0 do
  begin
    L := LockList;
    try
      Thread := TLccEthernetServerThread( L[0]);
      L.Delete(0);
    finally
      UnlockList;
    end;
    CloseEthernetPort(Thread);
  end;
end;

procedure TLccEthernetThreadList.CloseEthernetPort( EthernetThread: TLccEthernetServerThread);
var
  TimeCount: Cardinal;
begin
  EthernetThread.Terminate;
  TimeCount := 0;
//  TimeCount := GetTickCount;            DON"T LINK OCLB_UTILITES, it causes issues with linking to different packages
  while (EthernetThread.Running) do
  begin
    {$IFNDEF FPC_CONSOLE_APP}
    Application.ProcessMessages;
    {$ELSE}
    CheckSynchronize();  // Pump the timers
    {$ENDIF}
    Inc(TimeCount);
    Sleep(100);
    if TimeCount = 10 then
    begin
      {$IFDEF ULTIBO}
      {$ELSE}
      if Assigned(EthernetThread.Socket) then
        EthernetThread.Socket.CloseSocket
      else
        Break // Something went really wrong
      {$ENDIF}
    end;
  end;
  {$IFDEF ULTIBO}
  {$ELSE}
  FreeAndNil( EthernetThread);
  {$ENDIF}
end;

{ TLccEthernetServer }

procedure TLccEthernetServer.CloseConnection(EthernetThread: TLccEthernetServerThread);
var
  TimeCount: Integer;
begin
  if Assigned(EthernetThread) then
  begin
    EthernetThreads.Remove(EthernetThread);
    EthernetThreads.CloseEthernetPort(EthernetThread);
  end else
    EthernetThreads.CloseEthernetPorts;
  if Assigned(ListenerThread) then
  begin
    TimeCount := 0;
    ListenerThread.Terminate;
    {$IFDEF ULTIBO}
    {$ELSE}
    if Assigned(ListenerThread.Socket) then
      ListenerThread.Socket.CloseSocket;  // Force out of wait state with an error
    {$ENDIF}
    while ListenerThread.Running do
    begin
      {$IFNDEF FPC_CONSOLE_APP}
      Application.ProcessMessages;
      {$ELSE}
      CheckSynchronize();  // Pump the timers
      {$ENDIF}
      Inc(TimeCount);
      Sleep(100);
      if TimeCount = 10 then
      begin
        {$IFDEF ULTIBO}
        {$ELSE}
         if Assigned(ListenerThread.Socket) then
           ListenerThread.Socket.CloseSocket
         else
           Break // Something went really wrong
        {$ENDIF}
      end;
    end;
    FreeAndNil(FListenerThread);
  end;
end;

procedure TLccEthernetServer.SetSleepCount(AValue: Integer);
begin
    if AValue <> FSleepCount then
  begin
    FSleepCount := AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread);
  end;
end;

procedure TLccEthernetServer.UpdateListenerEvents( AListenerThread: TLccEthernetListener);
begin
  if Assigned(AListenerThread) then
  begin
    AListenerThread.OnConnectionStateChange := OnConnectionStateChange;
    AListenerThread.OnErrorMessage := OnErrorMessage;
    AListenerThread.OnReceiveMessage := OnReceiveMessage;
    AListenerThread.OnSendMessage := OnSendMessage;
    AListenerThread.GridConnect := FGridConnect;
  end;
end;

procedure TLccEthernetServer.UpdateThreadEvents(EthernetThread: TLccEthernetServerThread);
begin
  EthernetThread.OnSendMessage := OnSendMessage;
  EthernetThread.SleepCount := SleepCount;
  EthernetThread.GridConnect := FGridConnect;
end;

procedure TLccEthernetServer.UpdateThreadsEvents;
var
  i: Integer;
  L: TList;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
      UpdateThreadEvents(TLccEthernetServerThread( L[i]));
  finally
    EthernetThreads.UnlockList;
  end;
end;

constructor TLccEthernetServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseSynchronize := True;
  FEthernetThreads := TLccEthernetThreadList.Create;
  FSiblingEthernetThreads := TThreadList.Create;
  SiblingEthernetThreads.Duplicates := dupIgnore;
  FHub := False;
end;

destructor TLccEthernetServer.Destroy;
begin
  FreeAndNil( FEthernetThreads);
  FreeAndNil( FSiblingEthernetThreads);
  inherited Destroy;
end;

function TLccEthernetServer.OpenConnection(const AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
begin
  Result := CreateListenerObject(AnEthernetRec);
  Result.Owner := Self;
  UpdateListenerEvents(Result);
  Result.Suspended := False;
  ListenerThread := Result;
end;

function TLccEthernetServer.OpenConnectionWithLccSettings: TLccEthernetListener;
var
  AnEthernetRec: TLccEthernetRec;
begin
  Result := nil;
  if Assigned(LccSettings) then
  begin
    AnEthernetRec.ConnectionState := ccsListenerDisconnected;
    AnEthernetRec.SuppressNotification := False;
    AnEthernetRec.Thread := nil;
    AnEthernetRec.MessageStr := '';
    AnEthernetRec.ListenerPort := LccSettings.Ethernet.LocalListenerPort;
    AnEthernetRec.ListenerIP := LccSettings.Ethernet.LocalListenerIP;
    AnEthernetRec.ClientIP := '';
    AnEthernetRec.ClientPort := 0;
    AnEthernetRec.HeartbeatRate := 0;
    AnEthernetRec.ErrorCode := 0;
    AnEthernetRec.MessageArray := nil;
    AnEthernetRec.AutoResolveIP := LccSettings.Ethernet.AutoResolveListenerIP;
    Result := OpenConnection(AnEthernetRec);
  end;
end;

function TLccEthernetServer.CreateListenerObject(AnEthernetRec: TLccEthernetRec): TLccEthernetListener;
begin
  Result := TLccEthernetListener.Create(True, Self, AnEthernetRec);
end;

procedure TLccEthernetServer.SendMessage(AMessage: TLccMessage);
var
  i: Integer;
  L: TList;
  EthernetThread: TLccEthernetServerThread;
begin
  L := EthernetThreads.LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      EthernetThread := TLccEthernetServerThread( L[i]);
      if not EthernetThread.IsTerminated then
        EthernetThread.SendMessage(AMessage);
    end;
  finally
    EthernetThreads.UnlockList;
  end;
end;

procedure TLccEthernetServer.SendMessageRawGridConnect(GridConnectStr: String);
//var
 // List: TList;
 // i: Integer;
begin
 // List := EthernetThreads.LockList;
  try            // TODO
 //   for i := 0 to List.Count - 1 do
 //     TLccEthernetClientThread(List[i]).OutgoingGridConnect.Add(GridConnectStr);
  finally
 //   EthernetThreads.UnlockList;
  end;
end;

procedure TLccEthernetServer.RegisterSiblingEthernetServer(AnEthenetServer: TLccEthernetServer);
begin
  SiblingEthernetThreads.Add(AnEthenetServer);
end;

procedure TLccEthernetServer.UnRegisterSiblingEthernetServer(AnEthernetServer: TLccEthernetServer);
begin
  if Assigned(AnEthernetServer) then
    SiblingEthernetThreads.Remove(AnEthernetServer)
  else
    SiblingEthernetThreads.Clear;
end;

procedure TLccEthernetServer.SetGridConnect(AValue: Boolean);
begin
  if AValue <> FGridConnect then
  begin
    FGridConnect:=AValue;
    UpdateThreadsEvents;
    UpdateListenerEvents(ListenerThread);
  end;

end;

function TLccEthernetServer.GetConnected: Boolean;
begin
  Result := Assigned(ListenerThread);
end;

{ TLccEthernetServerThread }

{$IFDEF ULTIBO}
procedure TLccEthernetServerThread.Execute;
begin

end;
{$ELSE}
procedure TLccEthernetServerThread.Execute;
var
  TxStr: String;
  RcvByte: Byte;
  GridConnectStrPtr: PGridConnectString;
  GridConnectHelper: TGridConnectHelper;
  TxList, RxList: TStringList;
  DynamicByteArray: TDynamicByteArray;
  LocalSleepCount: Integer;
begin
  FRunning := True;

  HandleSendConnectionNotification(ccsListenerClientConnecting);
  GridConnectHelper := TGridConnectHelper.Create;
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := EthernetRec.HeartbeatRate;
  Socket.SetTimeout(0);
  Socket.Socket := SocketHandleForListener;    // Read back the handle
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    GridConnectHelper.Free;
    FRunning := False
  end else
  begin
    FEthernetRec.ClientIP := Socket.GetRemoteSinIP;
    FEthernetRec.ClientPort := Socket.GetRemoteSinPort;
    FEthernetRec.ListenerIP := Socket.GetLocalSinIP;
    FEthernetRec.ListenerPort := Socket.GetLocalSinPort;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      GridConnectHelper.Free;
      FRunning := False
    end else
    begin
      HandleSendConnectionNotification(ccsListenerClientConnected);
      try
        try
          LocalSleepCount := 0;
          while not IsTerminated and (FEthernetRec.ConnectionState = ccsListenerClientConnected) do
          begin

            // Handle the Socket using GridConnect
            if Gridconnect then
            begin
              if LocalSleepCount >= SleepCount then
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

                if TxStr <> '' then
                begin
                  Socket.SendString(String( TxStr) + LF);
                  if Socket.LastError <> 0 then
                    HandleErrorAndDisconnect;
                end;
                LocalSleepCount := 0;
              end;
              Inc(LocalSleepCount);


              RcvByte := Socket.RecvByte(1);
              case Socket.LastError of
                0 :
                  begin
                    GridConnectStrPtr := nil;
                    if GridConnectHelper.GridConnect_DecodeMachine(RcvByte, GridConnectStrPtr) then
                    begin
                      FEthernetRec.MessageStr := GridConnectBufferToString(GridConnectStrPtr^);
                      FEthernetRec.LccMessage.LoadByGridConnectStr(FEthernetRec.MessageStr);

                      case GridConnectMessageAssembler.IncomingMessageGridConnect(FEthernetRec.LccMessage) of
                        imgcr_True :
                          begin
                            if UseSynchronize then
                              Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage)
                            else begin
                              RxList := Owner.IncomingGridConnect.LockList;
                              try
                                RxList.Add(FEthernetRec.LccMessage.ConvertToGridConnectStr('', False));
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
                WSAETIMEDOUT :
                  begin

                  end;
                WSAECONNRESET   :
                  begin
                    HandleErrorAndDisconnect;
                  end
              else
                HandleErrorAndDisconnect
              end;
            end else
            begin    // Handle the Socket with LCC TCP Protocol
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
                    begin
                      if UseSynchronize then
                        Synchronize({$IFDEF FPC}@{$ENDIF}DoReceiveMessage)
                      else begin
                        DynamicByteArray := nil;
                        Owner.IncomingCircularArray.LockArray;
                        try
                          Owner.IncomingCircularArray.AddChunk(FEthernetRec.MessageArray);
                        finally
                          Owner.IncomingCircularArray.UnLockArray;
                        end;
                      end
                    end;
                  end;
                WSAETIMEDOUT :
                  begin

                  end;
                WSAECONNRESET   :
                  begin
                    HandleErrorAndDisconnect;
                  end
              else
                HandleErrorAndDisconnect
              end;
            end;
          end;
        finally
          HandleSendConnectionNotification(ccsListenerClientDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
          GridConnectHelper.Free;
        end;
      finally
        HandleSendConnectionNotification(ccsListenerClientDisconnected);
        Owner.EthernetThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

procedure TLccEthernetServerThread.HandleErrorAndDisconnect;
begin
  Owner.EthernetThreads.Remove(Self);
  FEthernetRec.ErrorCode := Socket.LastError;
  FEthernetRec.MessageStr := Socket.LastErrorDesc;
  if not FEthernetRec.SuppressNotification and (FEthernetRec.ErrorCode <> 0) then
    Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
  HandleSendConnectionNotification(ccsListenerClientDisconnected);
  Terminate;
end;

procedure TLccEthernetServerThread.HandleSendConnectionNotification(NewConnectionState: TConnectionState);
begin
   FEthernetRec.ConnectionState := NewConnectionState;
  if not FEthernetRec.SuppressNotification then
    Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
end;

{$ENDIF}

procedure TLccEthernetServerThread.SendMessage(AMessage: TLccMessage);
var
  ByteArray: TDynamicByteArray;
  i: Integer;
begin
  if not IsTerminated then
  begin
    if Gridconnect then
    begin
      MsgStringList.Text := AMessage.ConvertToGridConnectStr(#10, False);
      for i := 0 to MsgStringList.Count - 1 do
        OutgoingGridConnect.Add(MsgStringList[i]);
    end else
    begin
      ByteArray := nil;
      if AMessage.ConvertToLccTcp(ByteArray) then
        OutgoingCircularArray.AddChunk(ByteArray);
    end;
    DoSendMessage(AMessage);
  end;
end;

constructor TLccEthernetServerThread.Create(CreateSuspended: Boolean; AnOwner: TLccEthernetServer; const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := Self;
  FEthernetRec.LccMessage := TLccMessage.Create;
  GridConnectMessageAssembler := TLccGridConnectMessageAssembler.Create;
end;

destructor TLccEthernetServerThread.Destroy;
begin
  {$IFDEF DWSCRIPT}
  FEthernetRec.LccMessage.Free;
  FTcpDecodeStateMachine.Free;
  FGridConnectMessageAssembler.Free;
  {$ELSE}
  FreeAndNil(FEthernetRec.LccMessage);
  FreeAndNil(FGridConnectMessageAssembler);
  {$ENDIF}
  inherited Destroy;
end;

procedure TLccEthernetServerThread.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLccEthernetServerThread.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLccEthernetServerThread.DoReceiveMessage;
var
  L, LSibling: TList;
  SiblingServer: TLccEthernetServer;
  i, j: Integer;
begin
  if not IsTerminated then
  begin
    // Called in the content of the main thread through Syncronize
    if Assigned(OnReceiveMessage) then    // Do first so we get notified before any response is sent in ProcessMessage
      OnReceiveMessage(Self, FEthernetRec);

    if Gridconnect then
    begin
      if Owner.NodeManager <> nil then
        Owner.NodeManager.ProcessMessage(EthernetRec.LccMessage);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

      if Owner.Hub then
      begin
        L := Owner.EthernetThreads.LockList;
        try
          for i := 0 to L.Count - 1 do
          begin
            if TLccEthernetServerThread(L[i]) <> Self then
              TLccEthernetServerThread(L[i]).SendMessage(EthernetRec.LccMessage);
          end;
        finally
          Owner.EthernetThreads.UnlockList;
        end
      end;

      // Now run all the threads in each sibling server that is registered
      LSibling := Owner.SiblingEthernetThreads.LockList;
      try
        for j := 0 to LSibling.Count - 1 do
        begin
          SiblingServer := TLccEthernetServer( LSibling[i]);
          L := SiblingServer.EthernetThreads.LockList;
          try
            for i := 0 to L.Count - 1 do
            begin
              if TLccEthernetServerThread(L[i]) <> Self then
                TLccEthernetServerThread(L[i]).SendMessage(EthernetRec.LccMessage);
            end;
          finally
            SiblingServer.EthernetThreads.UnlockList;
          end
        end;
      finally
        Owner.SiblingEthernetThreads.UnlockList;
      end;

    end else
    begin   // TCP Protocol
      if WorkerMsg.LoadByLccTcp(FEthernetRec.MessageArray) then // In goes a raw message
      begin
        if (Owner.NodeManager <> nil) then
          Owner.NodeManager.ProcessMessage(WorkerMsg);  // What comes out is a fully assembled message that can be passed on to the NodeManager, NodeManager does not seem to pieces of multiple frame messages

        if Owner.Hub then
        begin
          L := Owner.EthernetThreads.LockList;
          try
            for i := 0 to L.Count - 1 do
            begin
              if TLccEthernetServerThread(L[i]) <> Self then
                TLccEthernetServerThread(L[i]).SendMessage(WorkerMsg);
            end;
          finally
            Owner.EthernetThreads.UnlockList;
          end
        end
      end
    end;
  end
end;

procedure TLccEthernetServerThread.DoSendMessage(AMessage: TLccMessage);
begin
  if Assigned(OnSendMessage) then
    OnSendMessage(Self, AMessage);
end;

initialization
  RegisterClass(TLccEthernetServer);

finalization

end.

