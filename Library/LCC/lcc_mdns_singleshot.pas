unit lcc_mdns_singleshot;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
    {$DEFINE LOGGING}
  {$ENDIF}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP} LResources, Forms, Controls, Graphics, Dialogs, {$ENDIF}
  {$ELSE}
  FMX.Forms, Types, System.Generics.Collections,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging, lcc_detailed_logging,
  {$ENDIF}
  lcc_gridconnect, blcksock, synsock,
  lcc_can_message_assembler_disassembler,
  lcc_nodemanager, lcc_messages, lcc_ethernetclient, lcc_threadedcirculararray,
  lcc_tcp_protocol, lcc_app_common_settings, lcc_utilities,
  lcc_common_classes, dnssend;

const
  QCLASS_INET = 1;  // Class = Internet

type
  TmDNSQuestionRec = record
    QName: AnsiString;
    QType,
    QClass: Word;
  end;

  TmDNSAnswerRec = record
    AName: AnsiString;
    AType,
    AClass: Word;
    ATTL: DWORD;
    ARDLength: Word;
    ARData: array of Byte;
  end;

type
  TmDNSRec = record
    ID,
    Flags,
    QDCount,
    ANCount,
    NSCount,
    ARCount: Word;
    Questions: array of TmDNSQuestionRec;
    Answers: array of TmDNSAnswerRec;
  end;

type
  TOnLccMdnsQuestion = procedure(AQuestion: TmDNSQuestionRec) of object;
  TLcc_mDNS_SinglShotServer = class;

  { TLcc_mDNS_SingleShotListener }

  TLcc_mDNS_SingleShotListener = class(TThread)
  private
    FEthernetRec: TLccEthernetRec;
    FGridConnect: Boolean;
    FmDNSIncomingRec: TmDNSRec;
    FmDNSOutgoingRec: TmDNSRec;
    FOnClientDisconnect: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnQuestion: TOnLccMdnsQuestion;
    FOwner: TLcc_mDNS_SinglShotServer;
    FRunning: Boolean;
    FSleepCount: Integer;
    FSocket: TUDPBlockSocket;
    function GetIsTerminated: Boolean;
  protected
    property EthernetRec: TLccEthernetRec read FEthernetRec write FEthernetRec;
    property Owner: TLcc_mDNS_SinglShotServer read FOwner write FOwner;
    property Running: Boolean read FRunning write FRunning;
    property Socket: TUDPBlockSocket read FSocket write FSocket;
    property IsTerminated: Boolean read GetIsTerminated;
    property mDNSIncomingRec: TmDNSRec read FmDNSIncomingRec write FmDNSIncomingRec;
    property mDNSOutgoingRec: TmDNSRec read FmDNSOutgoingRec write FmDNSOutgoingRec;

    procedure DoConnectionState;
    procedure DoErrorMessage;
    procedure DoQuestion;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AnOwner: TLcc_mDNS_SinglShotServer; const AnEthernetRec: TLccEthernetRec); reintroduce; virtual;
    destructor Destroy; override;

    property Gridconnect: Boolean read FGridConnect write FGridConnect;
    property OnClientDisconnect: TOnEthernetRecFunc read FOnClientDisconnect write FOnClientDisconnect;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnQuestion: TOnLccMdnsQuestion read FOnQuestion write FOnQuestion;
    property SleepCount: Integer read FSleepCount write FSleepCount;
  end;

  { TLcc_mDNS_SinglShotServer }

  TLcc_mDNS_SinglShotServer = class(TLccHardwareConnectionManager)
  private
    FLccSettings: TLccSettings;
    FListenerThread: TLcc_mDNS_SingleShotListener;
    FNodeManager: TLccNodeManager;
    FOnErrorMessage: TOnEthernetRecFunc;
    FOnConnectionStateChange: TOnEthernetRecFunc;
    FOnQuestion: TOnLccMdnsQuestion;
    FOnReceiveMessage: TOnEthernetReceiveFunc;
    FOnSendMessage: TOnMessageEvent;
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateListenerEvents(AListenerThread: TLcc_mDNS_SingleShotListener; Suspend: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenConnection(var AnEthernetRec: TLccEthernetRec): TLcc_mDNS_SingleShotListener;
    function OpenConnectionWithLccSettings: TLcc_mDNS_SingleShotListener;
    procedure CloseConnection;

    property ListenerThread: TLcc_mDNS_SingleShotListener read FListenerThread write FListenerThread;

  published
    { Published declarations }
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property OnConnectionStateChange: TOnEthernetRecFunc read FOnConnectionStateChange write FOnConnectionStateChange;
    property OnErrorMessage: TOnEthernetRecFunc read FOnErrorMessage write FOnErrorMessage;
    property OnQuestion: TOnLccMdnsQuestion read FOnQuestion write FOnQuestion;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
    {$IFDEF FPC}
    //  {$I TLcc_mDNS_SinglShotServer.lrs}
    {$ENDIF}
    RegisterComponents('LCC',[TLcc_mDNS_SinglShotServer]);
  {$ENDIF}
end;

{ TLcc_mDNS_SingleShotListener }

constructor TLcc_mDNS_SingleShotListener.Create(CreateSuspended: Boolean;AnOwner: TLcc_mDNS_SinglShotServer; const AnEthernetRec: TLccEthernetRec);
begin
  inherited Create(CreateSuspended);
  FOwner := AnOwner;
  FEthernetRec := AnEthernetRec;
  FEthernetRec.Thread := nil;
  FEthernetRec.LccMessage := TLccMessage.Create;
end;

destructor TLcc_mDNS_SingleShotListener.Destroy;
begin
  FreeAndNil(FEthernetRec.LccMessage);
  inherited Destroy;
end;

procedure TLcc_mDNS_SingleShotListener.DoConnectionState;
begin
  if Assigned(OnConnectionStateChange) then
    OnConnectionStateChange(Self, FEthernetRec)
end;

procedure TLcc_mDNS_SingleShotListener.DoErrorMessage;
begin
  if not IsTerminated then
  begin
    if Assigned(OnErrorMessage) then
      OnErrorMessage(Self, FEthernetRec)
  end;
end;

procedure TLcc_mDNS_SingleShotListener.DoQuestion;
var
  i: Integer;
begin
  if Assigned(OnQuestion) then
  begin
    for i := 0 to Length(mDNSIncomingRec.Questions) - 1 do
      OnQuestion(mDNSIncomingRec.Questions[i]);
  end;
end;

procedure TLcc_mDNS_SingleShotListener.Execute;

  procedure SendConnectionNotification(NewConnectionState: TConnectionState);
  begin
    FEthernetRec.ConnectionState := NewConnectionState;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoConnectionState);
  end;

  procedure HandleErrorAndDisconnect;
  begin
    FEthernetRec.ErrorCode := Socket.LastError;
    FEthernetRec.MessageStr := Socket.LastErrorDesc;
    if not FEthernetRec.SuppressNotification then
      Synchronize({$IFDEF FPC}@{$ENDIF}DoErrorMessage);
    SendConnectionNotification(ccsListenerDisconnected);
    Terminate
  end;

  function DecodeUrlLabel(var StartOffset: Integer; RawUrl: AnsiString): AnsiString;
  var
    i: Integer;
    Count: Integer;
  begin
    Result := '';
    Count := Ord( RawUrl[StartOffset]);
    Inc(StartOffset);
    for i := 0 to Count - 1 do
    begin
      Result := Result + RawUrl[StartOffset];
      Inc(StartOffset);
    end;
  end;

var
  Packet: AnsiString;
  PacketOffset, UrlOffset: Integer;
  i: Integer;
  IpAddress: String;
begin
  FRunning := True;

  Socket := TUDPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
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


  Socket.EnableReusePort(True);
  Socket.EnableMulticastLoop(True);
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
    Socket.AddMulticast('224.0.0.251');
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      FRunning := False
    end else
    begin
      Socket.MulticastTTL := 1;
      // UDP Connections do not "Listen"
      SendConnectionNotification(ccsListenerConnected);
      try
        try
          while not Terminated and (FEthernetRec.ConnectionState = ccsListenerConnected) do
          begin
            if Socket.CanRead(1000) then
            begin
              if not Terminated then
              begin
                while Socket.LastError = 0 do
                begin
                  Packet := Socket.RecvPacket(0);
                  case Socket.LastError of
                    0 :
                      begin
                        if Length(Packet) > 12 then
                        begin
                          {$IFDEF LCC_MOBILE}
                            PacketOffset := 0;
                          {$ELSE}
                            PacketOffset := 1;
                          {$ENDIF}
                          FmDNSIncomingRec.ID := Ord(Packet[PacketOffset]) shl 8;
                          Inc(PacketOffset);
                          FmDNSIncomingRec.ID := FmDNSIncomingRec.ID or Ord(Packet[PacketOffset]);
                          Inc(PacketOffset);
                          FmDNSIncomingRec.Flags := Ord(Packet[PacketOffset]) shl 8;
                          Inc(PacketOffset);
                          FmDNSIncomingRec.Flags := FmDNSIncomingRec.Flags or Ord(Packet[PacketOffset]);
                          Inc(PacketOffset);
                          FmDNSIncomingRec.QDCount := Ord(Packet[PacketOffset]) shl 8;
                          Inc(PacketOffset);
                          FmDNSIncomingRec.QDCount := FmDNSIncomingRec.QDCount or Ord(Packet[PacketOffset]);
                          Inc(PacketOffset);
                          FmDNSIncomingRec.ANCount := Ord(Packet[PacketOffset]) shl 8;
                          Inc(PacketOffset);
                          FmDNSIncomingRec.ANCount := FmDNSIncomingRec.ANCount or Ord(Packet[PacketOffset]);
                          Inc(PacketOffset);
                          FmDNSIncomingRec.NSCount := Ord(Packet[PacketOffset]) shl 8;
                          Inc(PacketOffset);
                          FmDNSIncomingRec.NSCount := FmDNSIncomingRec.NSCount or Ord(Packet[PacketOffset]);
                          Inc(PacketOffset);
                          FmDNSIncomingRec.ARCount := Ord(Packet[PacketOffset]) shl 8;
                          Inc(PacketOffset);
                          FmDNSIncomingRec.ARCount := FmDNSIncomingRec.ARCount or Ord(Packet[PacketOffset]);
                          Inc(PacketOffset);

                          SetLength(FmDNSIncomingRec.Questions, FmDNSIncomingRec.QDCount);
                          SetLength(FmDNSIncomingRec.Answers, FmDNSIncomingRec.ANCount);

                 //         if FmDNSIncomingRec.QDCount > 10 then
                //            beep;

                          for i := 0 to FmDNSIncomingRec.QDCount - 1 do
                          begin
                            FmDNSIncomingRec.Questions[i].QName := '';
                            FmDNSIncomingRec.Questions[i].QClass := 0;
                            FmDNSIncomingRec.Questions[i].QType := 0;

                            while Packet[PacketOffset] <> #0 do
                            begin
                              // This could be an offset into the stucture some where else
                              if Ord(Packet[PacketOffset]) and $C0 = $C0 then
                              begin
                                // Get the Offset into the structure
                                UrlOffset := Ord(Packet[PacketOffset]) shl 8;
                                Inc(PacketOffset);
                                UrlOffset := UrlOffset or Ord(Packet[PacketOffset]);
                                UrlOffset := UrlOffset and not $C000;
                                {$IFNDEF LCC_MOBILE}
                                Inc(UrlOffset);
                                {$ENDIF}
                                while Packet[UrlOffset] <> #0 do
                                  FmDNSIncomingRec.Questions[i].QName := FmDNSIncomingRec.Questions[i].QName + DecodeUrlLabel(UrlOffset, Packet) + '.';
                                Inc(PacketOffset);
                              end else
                              begin
                                FmDNSIncomingRec.Questions[i].QName := FmDNSIncomingRec.Questions[i].QName + DecodeUrlLabel(PacketOffset, Packet) + '.';
                              end;
                            end;
                            SetLength(FmDNSIncomingRec.Questions[i].QName, Length(FmDNSIncomingRec.Questions[i].QName) - 1); // Strip off '.';

                            Inc(PacketOffset);
                            FmDNSIncomingRec.Questions[i].QType := Ord(Packet[PacketOffset]) shl 8;
                            Inc(PacketOffset);
                            FmDNSIncomingRec.Questions[i].QType := FmDNSIncomingRec.Questions[i].QType or Ord(Packet[PacketOffset]);

                            Inc(PacketOffset);
                            FmDNSIncomingRec.Questions[i].QClass := Ord(Packet[PacketOffset]) shl 8;
                            Inc(PacketOffset);
                            FmDNSIncomingRec.Questions[i].QClass := FmDNSIncomingRec.Questions[i].QClass or Ord(Packet[PacketOffset]);

                            Inc(PacketOffset);
                          end;
                        end;

                        for i := 0 to FmDNSIncomingRec.QDCount - 1 do
                          Synchronize({$IFDEF FPC}@{$ENDIF}DoQuestion);

                        for i := 0 to FmDNSIncomingRec.QDCount - 1 do
                        begin
                          if (LowerCase(FmDNSIncomingRec.Questions[i].QName) = 'openlcb.local') and (FmDNSIncomingRec.Questions[i].QType = QTYPE_A) and (FmDNSIncomingRec.Questions[i].QClass = QCLASS_INET) then
                          begin
                            SetLength(FmDNSOutgoingRec.Answers, 1);
                            SetLength(FmDNSOutgoingRec.Questions, 1);
                            FmDNSOutgoingRec.Questions[0] := FmDNSIncomingRec.Questions[i];
                            FmDNSOutgoingRec.Flags := $01;  // Reply
                            FmDNSOutgoingRec.QDCount := 1;
                            FmDNSOutgoingRec.ANCount := 1;
                            FmDNSOutgoingRec.ARCount := 0;
                            FmDNSOutgoingRec.NSCount := 0;
                            FmDNSOutgoingRec.Answers[0].AName := FmDNSIncomingRec.Questions[i].QName;
                            FmDNSOutgoingRec.Answers[0].AType := QTYPE_A;
                            FmDNSOutgoingRec.Answers[0].AClass := QCLASS_INET;
                            FmDNSOutgoingRec.Answers[0].ARDLength := 4;
                            SetLength(FmDNSOutgoingRec.Answers[0].ARData, 4);
                            {$IFDEF LCC_WINDOWS}
                            IpAddress := ResolveWindowsIp(Socket);
                            {$ELSE}
                            IpAddress := ResolveUnixIp;
                            {$ENDIF}
                            FmDNSOutgoingRec.Answers[0].ARData := Ip4Address_StrToBytes(IpAddress);
                          end;
                        end;
                      end;
                    WSAETIMEDOUT :
                      begin
                      end;
                    WSAECONNABORTED :
                      begin
                        HandleErrorAndDisconnect;
                      end;
                  end;
                end
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

function TLcc_mDNS_SingleShotListener.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

{ TLcc_mDNS_SinglShotServer }

procedure TLcc_mDNS_SinglShotServer.CloseConnection;
var
  TimeCount: Integer;
begin
  if Assigned(ListenerThread) then
  begin
    TimeCount := 0;
    ListenerThread.Terminate;
    if Assigned(ListenerThread.Socket) then
      ListenerThread.Socket.CloseSocket;  // Force out of wait state with an error
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
         if Assigned(ListenerThread.Socket) then
           ListenerThread.Socket.CloseSocket
         else
           Break // Something went really wrong
      end;

    end;
    FreeAndNil(FListenerThread);
  end;
end;

procedure TLcc_mDNS_SinglShotServer.UpdateListenerEvents(AListenerThread: TLcc_mDNS_SingleShotListener; Suspend: Boolean);
begin
  if Assigned(AListenerThread) then
  begin
    AListenerThread.OnConnectionStateChange := OnConnectionStateChange;
    AListenerThread.OnErrorMessage := OnErrorMessage;
    AListenerThread.OnQuestion := OnQuestion;
  end;
end;

constructor TLcc_mDNS_SinglShotServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLcc_mDNS_SinglShotServer.Destroy;
begin
  inherited Destroy;
end;

function TLcc_mDNS_SinglShotServer.OpenConnection(var AnEthernetRec: TLccEthernetRec): TLcc_mDNS_SingleShotListener;
begin
  // Default mDNS port to listen too
  if AnEthernetRec.ListenerIP = '' then
    AnEthernetRec.ListenerIP := '224.0.0.251' ; //'0.0.0.0';
  if AnEthernetRec.ListenerPort = 0 then
    AnEthernetRec.ListenerPort := 5353;

  Result := TLcc_mDNS_SingleShotListener.Create(True, Self, AnEthernetRec);
  Result.Owner := Self;
  UpdateListenerEvents(Result, True);
  Result.Suspended := False;
  ListenerThread := Result;
end;

function TLcc_mDNS_SinglShotServer.OpenConnectionWithLccSettings: TLcc_mDNS_SingleShotListener;
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


initialization
  RegisterClass(TLcc_mDNS_SinglShotServer);

finalization

end.

