unit lcc_node;

{$mode objfpc}{$H+}

interface

{$I lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
  laz2_DOM,
  laz2_XMLRead,
  fptimer,
  {$ENDIF}
  lcc_app_common_settings,
  lcc_node_protocol_helpers,
  lcc_math_float16,
  lcc_messages,
  lcc_defines,
  lcc_utilities;

type

  // The Database Node is a container that mirrors some node on the network that
  // this program does not maintain.  It mainly handles reply and information broadcast
  // by those nodes.  It should NEVER send out a message with the stored NodeID or Alias

  { TLccDatabaseNode }

  TLccDatabaseNode = class(TLccCoreNode)
  protected
    function GetInitialized: Boolean; override;
    function GetPermitted: Boolean; override;

    function DoCanAME(LccMessage: TLccMessage): Boolean; override;
    function DoCanAMD(LccMessage: TLccMessage): Boolean; override;
    function DoCanRID(LccMessage: TLccMessage): Boolean; override;

    function DoInitializatinComplete(LccMessage: TLccMessage): Boolean; override;
    function DoProtocolSupportReply(LccMessage: TLccMessage): Boolean; override;
    function DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean; override;
    function DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean; override;
    function DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedSet(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedClear(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; override;
    function DoConsumerIdentifiedSet(LccMessage: TLccMessage): Boolean; override;
    function DConsumerIdentifiedClear(LccMessage: TLccMessage): Boolean; override;
    function DoConsumerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; override;
    function DoTractionProtocol(LccMessage: TLccMessage): Boolean; override;
    function DoTractionProtocolReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigurationRead(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationReadStream(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationReadReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationReadStreamReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationWrite(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationWriteStream(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationWriteReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationOperation(LccMessage: TLccMessage): Boolean; override;
    procedure DoUnknownLccCanMessage(LccMessage: TLccMessage); override;
    procedure DoUnknownLccDatagramConfigruationMessage(LccMessage: TLccMessage); override;
    procedure DoUnknownLccDatagramMessage(LccMessage: TLccMessage); override;
    procedure DoUnknownLccMessge(LccMessage: TLccMessage); override;
    function IsMessageForThisNode(LccMessage: TLccMessage): Boolean; override;
  public
    procedure Login(NewNodeID, RegenerateAliasSeed: Boolean); override;
  end;

  { TLccVirtualNode }

  TLccVirtualNode = class(TLccCoreNode)
  private
    FDuplicateAliasDetected: Boolean;
    FLoggedIn: Boolean;
    FLogInAliasID: Word;
    FLoginTimer: TFPTimer;
    FSeedNodeID: TNodeID;
  protected
    function DoCanAME(LccMessage: TLccMessage): Boolean; override;
    function DoCanAMD(LccMessage: TLccMessage): Boolean; override;
    function DoCanRID(LccMessage: TLccMessage): Boolean; override;

    function DoInitializatinComplete(LccMessage: TLccMessage): Boolean; override;
    function DoProtocolSupportReply(LccMessage: TLccMessage): Boolean; override;
    function DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean; override;
    function DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean; override;
    function DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedSet(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedClear(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; override;
    function DoConsumerIdentifiedSet(LccMessage: TLccMessage): Boolean; override;
    function DConsumerIdentifiedClear(LccMessage: TLccMessage): Boolean; override;
    function DoConsumerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; override;
    function DoTractionProtocol(LccMessage: TLccMessage): Boolean; override;
    function DoTractionProtocolReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigurationRead(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationReadStream(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationReadReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationReadStreamReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationWrite(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationWriteStream(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationWriteReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramConfigruationOperation(LccMessage: TLccMessage): Boolean; override;
    procedure DoUnknownLccCanMessage(LccMessage: TLccMessage); override;
    procedure DoUnknownLccDatagramConfigruationMessage(LccMessage: TLccMessage); override;
    procedure DoUnknownLccDatagramMessage(LccMessage: TLccMessage); override;
    procedure DoUnknownLccMessge(LccMessage: TLccMessage); override;

    function CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
    function GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
    procedure GenerateNewNodeID;
    procedure OnLoginTimer(Sender: TObject);
    procedure PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
    procedure SendAliasLoginRequest;
    procedure SendAliasLogin;
    procedure SendAMR;
    procedure SendEvents;
    procedure SendConsumedEvents;
    procedure SendProducedEvents;

    property DuplicateAliasDetected: Boolean read FDuplicateAliasDetected write FDuplicateAliasDetected;
    property LogInAliasID: Word read FLogInAliasID write FLogInAliasID;
    {$IFNDEF FPC_CONSOLE_APP} property LoginTimer: TTimer read FLoginTimer write FLoginTimer;
    {$ELSE}                   property LoginTimer: TFPTimer read FLoginTimer write FLoginTimer;{$ENDIF}
    property SeedNodeID: TNodeID read FSeedNodeID write FSeedNodeID;
  public
    property Initialized;
    property LoggedIn: Boolean read FLoggedIn;
    property Permitted;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Login(NewNodeID, RegenerateAliasSeed: Boolean); override;
    procedure LoginWithLccSettings(RegenerateAliasSeed: Boolean; LccSettings: TLccSettings);
    procedure LoginWithNodeID(ANodeID: TNodeId; RegenerateAliasSeed: Boolean);
  end;


implementation

type
  TLccEventHack = class(TLccEvent)
  end;

{ TLccVirtualNode }

function TLccVirtualNode.DoCanAME(LccMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
begin
  Result := True;
  TestNodeID[0] := 0;
  TestNodeID[1] := 0;

  if LccMessage.DataCount = 6 then
  begin
    LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
    if EqualNodeID(TestNodeID, NodeID, False) then
    begin
      WorkerMessage.LoadAMD(NodeID, AliasID);
      DoSendMessage(WorkerMessage);
    end
  end else
  begin
    WorkerMessage.LoadAMD(NodeID, AliasID);
    DoSendMessage(WorkerMessage);
  end;
end;

function TLccVirtualNode.DoCanAMD(LccMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
begin
  Result := True;
  TestNodeID[0] := 0;
  TestNodeID[1] := 0;
  if LccMessage.DataCount = 6 then
  begin
    LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
    if EqualNodeID(TestNodeID, NodeID, False) then                  // some Dog has my Node ID!
    begin
      WorkerMessage.LoadPCER(NodeID, AliasID, @EVENT_DUPLICATE_ID_DETECTED);
      DoSendMessage(WorkerMessage);
    end
  end;
end;

function TLccVirtualNode.DoCanRID(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoInitializatinComplete(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoProtocolSupportReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoProducerIdentifiedSet(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoProducerIdentifiedClear(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoProducerIdentifiedUnknown(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoConsumerIdentifiedSet(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DConsumerIdentifiedClear(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoConsumerIdentifiedUnknown(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoTractionProtocol(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoTractionProtocolReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoDatagramConfigurationRead(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoDatagramConfigruationReadStream(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoDatagramConfigruationReadReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoDatagramConfigruationReadStreamReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoDatagramConfigruationWrite(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoDatagramConfigruationWriteStream(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoDatagramConfigruationWriteReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoDatagramConfigruationOperation(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

procedure TLccVirtualNode.DoUnknownLccCanMessage(LccMessage: TLccMessage);
begin

end;

procedure TLccVirtualNode.DoUnknownLccDatagramConfigruationMessage(LccMessage: TLccMessage);
begin

end;

procedure TLccVirtualNode.DoUnknownLccDatagramMessage(LccMessage: TLccMessage);
begin

end;

procedure TLccVirtualNode.DoUnknownLccMessge(LccMessage: TLccMessage);
begin

end;

function TLccVirtualNode.CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
begin
 if Regenerate then
   PsudoRandomNumberGeneratorOnSeed(Seed);
 Result := GenerateID_Alias_From_Seed(Seed);
 if Result = 0 then
 begin
   PsudoRandomNumberGeneratorOnSeed(Seed);
   Result := GenerateID_Alias_From_Seed(Seed);
 end;
end;

function TLccVirtualNode.GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
begin
  Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
end;

procedure TLccVirtualNode.GenerateNewNodeID;
begin
  Randomize;
  FNodeID[1] := StrToInt('0x020112');
  FNodeID[0] := Random($FFFFFF);
  FSeedNodeID[0] := FNodeID[0];
  FSeedNodeID[1] := FNodeID[1];
end;

procedure TLccVirtualNode.OnLoginTimer(Sender: TObject);
begin
 // LoginTimer.Enabled := False;   // This locks up on some systems (Raspberry Pi)
  if not FLoggedIn then
  begin
    FAliasID := LoginAliasID;
    LogInAliasID := 0;
 //   if Assigned(OwnerManager) then
//      OwnerManager.DoAliasIDChanged(Self);
    SendAliasLogin;
    SendEvents;
  {  if OwnerManager.RootNode = Self then
    begin
      if OwnerManager.AutoSendVerifyNodesOnStart then
      begin
        WorkerMessage.LoadVerifyNodeID(NodeID, AliasID);
        OwnerManager.DoRequestMessageSend(WorkerMessage);
      end;
    end;  }
    FLoggedIn := True;
  end;
end;

procedure TLccVirtualNode.PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
var
  temp1,              // Upper 24 Bits of temp 48 bit number
  temp2: DWORD;       // Lower 24 Bits of temp 48 Bit number
begin
  temp1 := ((Seed[1] shl 9) or ((Seed[0] shr 15) and $000001FF)) and $00FFFFFF;   // x(i+1)(2^9 + 1)*x(i) + C  = 2^9 * x(i) + x(i) + C
  temp2 := (Seed[0] shl 9) and $00FFFFFF;                                                                  // Calculate 2^9 * x

  Seed[0] := Seed[0] + temp2 + $7A4BA9;   // Now y = 2^9 * x so all we have left is x(i+1) = y + x + c
  Seed[1] := Seed[1] + temp1 + $1B0CA3;

  Seed[1] := (Seed[1] and $00FFFFFF) or (Seed[0] and $FF000000) shr 24;   // Handle the carries of the lower 24 bits into the upper
  Seed[0] := Seed[0] and $00FFFFFF;
end;

procedure TLccVirtualNode.SendAliasLoginRequest;
begin
  WorkerMessage.LoadCID(NodeID, LoginAliasID, 0);
  DoSendMessage(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, LoginAliasID, 1);
  DoSendMessage(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, LoginAliasID, 2);
  DoSendMessage(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, LoginAliasID, 3);
  DoSendMessage(WorkerMessage);
end;

procedure TLccVirtualNode.SendAliasLogin;
begin
  WorkerMessage.LoadRID(AliasID);
  DoSendMessage(WorkerMessage);
  WorkerMessage.LoadAMD(NodeID, AliasID);
  DoSendMessage(WorkerMessage);
  FPermitted := True;
  WorkerMessage.LoadInitializationComplete(NodeID, AliasID);
  DoSendMessage(WorkerMessage);
  FInitialized := True;
end;

procedure TLccVirtualNode.SendAMR;
begin
  WorkerMessage.LoadAMR(NodeID, AliasID);
  DoSendMessage(WorkerMessage);
end;

procedure TLccVirtualNode.SendEvents;
begin
  SendConsumedEvents;
  SendProducedEvents;
end;

procedure TLccVirtualNode.SendConsumedEvents;
var
  i: Integer;
begin
  for i := 0 to EventsConsumed.EventList.Count - 1 do
  begin
    WorkerMessage.LoadConsumerIdentified(NodeID, AliasID, TLccEventHack( EventsConsumed.Event[i]).FID, EventsConsumed.Event[i].State);
    DoSendMessage(WorkerMessage);
  end;
end;

procedure TLccVirtualNode.SendProducedEvents;
var
  i: Integer;
begin
  for i := 0 to EventsProduced.EventList.Count - 1 do
  begin
    WorkerMessage.LoadProducerIdentified(NodeID, AliasID, TLccEventHack( EventsProduced.Event[i]).FID , EventsProduced.Event[i].State);
    DoSendMessage(WorkerMessage);
  end;
end;

constructor TLccVirtualNode.Create(AnOwner: TComponent);
begin
   inherited Create(AnOwner);
  {$IFNDEF FPC_CONSOLE_APP} LoginTimer := TTimer.Create(Self);
  {$ELSE}                   LoginTimer := TFPTimer.Create(Self);{$ENDIF}
  LoginTimer.Enabled := False;
  LoginTimer.Interval := 800;
  LoginTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}OnLoginTimer;
  LogInAliasID := 0;
end;

destructor TLccVirtualNode.Destroy;
begin
  if Permitted then
  begin
    WorkerMessage.LoadAMR(NodeID, AliasID);
    DoSendMessage(WorkerMessage);
  end;
  inherited Destroy;
end;

procedure TLccVirtualNode.Login(NewNodeID, RegenerateAliasSeed: Boolean);
begin
 if NewNodeID then
   GenerateNewNodeID;
 Configuration.LoadFromFile;
// if Assigned(OwnerManager) then
//   OwnerManager.DoNodeIDChanged(Self);
 LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
 SendAliasLoginRequest;
 DuplicateAliasDetected := False;
 LoginTimer.Enabled := True;
end;

procedure TLccVirtualNode.LoginWithLccSettings(RegenerateAliasSeed: Boolean; LccSettings: TLccSettings);
var
  TempNodeID: TNodeID;
  TempID, TempID1, TempID2: QWord;
  TempEventID: TEventID;
  i: Integer;
begin
  TempNodeID[0] := 0;
  TempNodeID[1] := 0;
  if Assigned(LccSettings)then
  begin
    if LccSettings.General.NodeIDAsVal = 0 then
    begin
      GenerateNewNodeID;
      LccSettings.General.NodeIDAsTNodeID(TempNodeID);
      if not EqualNodeID(TempNodeID, NodeID, True) then
      begin
         TempID1 := QWord(NodeID[0]);
         TempID2 := QWord(NodeID[1]);
         TempID2 := TempID2 shl 24;
         TempID := TempID1 or TempID2;
         LccSettings.General.NodeID := '0x'+IntToHex(TempID, 12);
         LccSettings.SaveToFile;
      end;
    end else
    begin
      LccSettings.General.NodeIDAsTNodeID(TempNodeID);
      FNodeID[0] := TempNodeID[0];
      FNodeID[1] := TempNodeID[1];
    end;

    if EventsProduced.AutoGenerate.Enable then
    begin
      for i := 0 to EventsProduced.AutoGenerate.Count - 1 do
      begin
        NodeIDToEventID(NodeID, EventsProduced.AutoGenerate.StartIndex + i, TempEventID);
        EventsProduced.Add(TempEventID, EventsProduced.AutoGenerate.DefaultState);
      end;
      EventsProduced.Valid := True;
    end;

    if EventsConsumed.AutoGenerate.Enable then
    begin
      for i := 0 to EventsConsumed.AutoGenerate.Count - 1 do
      begin
        NodeIDToEventID(NodeID, EventsConsumed.AutoGenerate.StartIndex + i, TempEventID);
        EventsConsumed.Add(TempEventID, EventsConsumed.AutoGenerate.DefaultState);
      end;
      EventsConsumed.Valid := True;
    end;

    Configuration.LoadFromFile;
  //  if Assigned(OwnerManager) then
  //    OwnerManager.DoNodeIDChanged(Self);
    LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
    SendAliasLoginRequest;
    DuplicateAliasDetected := False;
    LoginTimer.Enabled := True;
  end else
    Login(True, True)
end;

procedure TLccVirtualNode.LoginWithNodeID(ANodeID: TNodeId; RegenerateAliasSeed: Boolean);
begin
  FNodeID[0] := ANodeID[0];
  FNodeID[1] := ANodeID[1];
  FSeedNodeID[0] := ANodeID[0];
  FSeedNodeID[1] := ANodeID[1];

  Configuration.LoadFromFile;
//  if Assigned(OwnerManager) then
 //   OwnerManager.DoNodeIDChanged(Self);
  LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
  SendAliasLoginRequest;
  DuplicateAliasDetected := False;
  LoginTimer.Enabled := True;
end;

{ TLccDatabaseNode }

function TLccDatabaseNode.GetInitialized: Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.GetPermitted: Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoCanAME(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoCanAMD(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoCanRID(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoInitializatinComplete(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  FAliasID := LccMessage.CAN.SourceAlias;
  LccMessage.ExtractDataBytesAsNodeID(0, FNodeID);
 // OwnerManager.DoInitializationComplete(Self);
end;

function TLccDatabaseNode.DoProtocolSupportReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  ProtocolSupport.ProcessMessage(LccMessage);
 // OwnerManager.DoProtocolIdentifyReply(Self, LccDestNode);
end;

function TLccDatabaseNode.DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  FAliasID := LccMessage.CAN.SourceAlias;
  LccMessage.ExtractDataBytesAsNodeID(0, FNodeID);
//  OwnerManager.DoVerifiedNodeID(Self);
end;

function TLccDatabaseNode.DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SimpleNodeInfo.ProcessMessage(LccMessage);
 // OwnerManager.DoSimpleNodeIdentReply(Self, LccDestNode);
end;

function TLccDatabaseNode.DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SimpleTrainNodeInfo.ProcessMessage(LccMessage, Traction);
//  OwnerManager.DoSimpleTrainNodeIdentReply(Self, LccDestNode);
end;

function TLccDatabaseNode.DoProducerIdentifiedSet(LccMessage: TLccMessage): Boolean;
var
  EventPtr: PEventID;
begin
  Result := True;
  EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
  EventsProduced.Add(EventPtr^, evs_Valid);
 // OwnerManager.DoProducerIdentified(Self, EventPtr^ , evs_Valid);
end;

function TLccDatabaseNode.DoProducerIdentifiedClear(LccMessage: TLccMessage): Boolean;
var
  EventPtr: PEventID;
begin
  Result := True;
  EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
  EventsProduced.Add(EventPtr^, evs_InValid);
 // OwnerManager.DoProducerIdentified(Self, EventPtr^ , evs_InValid);
end;

function TLccDatabaseNode.DoProducerIdentifiedUnknown(LccMessage: TLccMessage): Boolean;
var
  EventPtr: PEventID;
begin
  Result := True;
  EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
  EventsProduced.Add(EventPtr^, evs_Unknown);
 // OwnerManager.DoProducerIdentified(Self, EventPtr^ , evs_Unknown);
end;

function TLccDatabaseNode.DoConsumerIdentifiedSet(LccMessage: TLccMessage): Boolean;
var
  EventPtr: PEventID;
begin
  Result := True;
  EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
  EventsConsumed.Add(EventPtr^, evs_Valid);
 // OwnerManager.DoConsumerIdentified(Self, EventPtr^ , evs_Valid);
end;

function TLccDatabaseNode.DConsumerIdentifiedClear(LccMessage: TLccMessage): Boolean;
var
  EventPtr: PEventID;
begin
  Result := True;
  EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
  EventsConsumed.Add(EventPtr^, evs_InValid);
  // OwnerManager.DoConsumerIdentified(Self, EventPtr^ , evs_InValid);
end;

function TLccDatabaseNode.DoConsumerIdentifiedUnknown(LccMessage: TLccMessage): Boolean;
var
  EventPtr: PEventID;
begin
  Result := True;
  EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
  EventsConsumed.Add(EventPtr^, evs_Unknown);
  // OwnerManager.DoConsumerIdentified(Self, EventPtr^ , evs_Unknown);
end;

function TLccDatabaseNode.DoTractionProtocol(LccMessage: TLccMessage): Boolean;
var
  Allow: Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[0] of
    TRACTION_CONTROLLER_CONFIG :
        begin
          case LccMessage.DataArrayIndexer[1] of
              TRACTION_CONTROLLER_CONFIG_NOTIFY :
                begin
                  Allow := True;
              //    OwnerManager.DoTractionControllerChangeNotify(Self, LccDestNode, LccMessage.ExtractDataBytesAsNodeID(3, ANodeID)^, LccMessage.ExtractDataBytesAsInt(9, 10), Allow);
                  WorkerMessage.LoadTractionControllerChangeNotifyReply(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, Allow);
                  DoSendMessage(WorkerMessage);
                  Result := True;
                end;
          end;
        end;
  end
end;

function TLccDatabaseNode.DoTractionProtocolReply(LccMessage: TLccMessage): Boolean;
var
  ANodeID: TNodeID;
begin
  Result := True;
  ANodeID := NULL_NODE_ID;
  Traction.ProcessMessage(LccMessage);

 case LccMessage.DataArrayIndexer[0] of
   TRACTION_QUERY_SPEED       : begin {OwnerManager.DoTractionReplyQuerySpeed(Self, LccDestNode); } end;
   TRACTION_QUERY_FUNCTION    : begin {OwnerManager.DoTractionReplyQueryFunction(Self, LccDestNode); } end;
   TRACTION_CONTROLLER_CONFIG :
       begin
         case LccMessage.DataArrayIndexer[1] of
           TRACTION_CONTROLLER_CONFIG_ASSIGN :
               begin
             //    OwnerManager.DoTractionReplyControllerAssign(Self, LccDestNode, LccMessage.DataArrayIndexer[2]);
                 Result := True;
               end;
           TRACTION_CONTROLLER_CONFIG_QUERY  :
               begin
             //    if LccMessage.DataArrayIndexer[2] and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
            //       OwnerManager.DoTractionReplyControllerQuery(Self, LccDestNode, LccMessage.ExtractDataBytesAsNodeID(3, ANodeID)^, LccMessage.ExtractDataBytesAsInt(9, 10))
            //     else
            //       OwnerManager.DoTractionReplyControllerQuery(Self, LccDestNode, LccMessage.ExtractDataBytesAsNodeID(3, ANodeID)^, 0);
                 Result := True;
               end;
           TRACTION_CONTROLLER_CONFIG_NOTIFY :
               begin
             //    OwnerManager.DoTractionReplyControllerChangeNotify(Self, LccDestNode, LccMessage.DataArrayIndexer[2]);
                 Result := True;
               end;
         end;
       end;
   TRACTION_MANAGE :
       begin
      //   OwnerManager.DoTractionReplyManage(Self, LccDestNode, LccMessage.DataArrayIndexer[2]);
         Result := True;
       end;
  end;
end;

function TLccDatabaseNode.DoDatagramConfigurationRead(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoDatagramConfigruationReadStream(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoDatagramConfigruationReadReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  if LccMessage.DataArrayIndexer[1] and $08 = 0 then
  begin
    case ExtractAddressSpaceFromDatagramConfigurationMessage(LccMessage) of
      MSI_CDI             : begin
                              SendAckReply(LccMessage, False, 0);
                              CDI.ProcessMessage(LccMessage);
                              Result := True;
                            end;
      MSI_ALL             : begin
                              SendAckReply(LccMessage, False, 0);
                            end;
      MSI_CONFIG          : begin
                              SendAckReply(LccMessage, False, 0);
                              ConfigurationMem.ProcessMessage(LccMessage);
                              Result := True;
                            end;
      MSI_ACDI_MFG        : begin end;
      MSI_ACDI_USER       : begin end;
      MSI_FDI             : begin
                              SendAckReply(LccMessage, False, 0);
                              FDI.ProcessMessage(LccMessage);
                              Result := True;
                            end;
      MSI_FUNCTION_CONFIG : begin
                              SendAckReply(LccMessage, False, 0);
                              FunctionConfiguration.ProcessMessage(LccMessage);
                              Result := True;
                            end;
    end;
  end
end;

function TLccDatabaseNode.DoDatagramConfigruationReadStreamReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoDatagramConfigruationWrite(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoDatagramConfigruationWriteStream(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoDatagramConfigruationWriteReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  if LccMessage.DataArrayIndexer[1] and $08 = 0 then
  begin
    case ExtractAddressSpaceFromDatagramConfigurationMessage(LccMessage) of
      MSI_CDI              : begin end; // Not writable
      MSI_ALL              : begin end; // Not writeable
      MSI_CONFIG           : begin
                               SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
                               ConfigurationMem.ProcessMessage(LccMessage);
                               Result := True;
                             end;
      MSI_ACDI_MFG         : begin end;
      MSI_ACDI_USER        : begin end;
      MSI_FDI              : begin end; // Not writeable
      MSI_FUNCTION_CONFIG  : begin
                               SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
                               FunctionConfiguration.ProcessMessage(LccMessage);
                               Result := True;
                             end;
    end;
  end
end;

function TLccDatabaseNode.DoDatagramConfigruationOperation(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[1] of
    MCP_OP_GET_CONFIG :
        begin
        end;
    MCP_OP_GET_CONFIG_REPLY :
        begin
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
          ConfigMemOptions.ProcessMessage(LccMessage);
          Result := True;
        end;
    MCP_OP_GET_ADD_SPACE_INFO :
        begin
        end;
    MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY,
    MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY:
        begin
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
          ConfigMemAddressSpaceInfo.ProcessMessage(LccMessage);
          Result := True;
        end;
    MCP_OP_LOCK :
        begin
        end;
    MCP_OP_GET_UNIQUEID :
        begin
        end;
    MCP_OP_FREEZE :
        begin
        end;
    MCP_OP_INDICATE :
        begin
        end;
    MCP_OP_RESETS :
        begin
        end;
  end;
end;

procedure TLccDatabaseNode.DoUnknownLccDatagramConfigruationMessage(
  LccMessage: TLccMessage);
begin
  // Do nothing
end;

procedure TLccDatabaseNode.DoUnknownLccDatagramMessage(LccMessage: TLccMessage);
begin
  // Do nothing
end;

procedure TLccDatabaseNode.DoUnknownLccMessge(LccMessage: TLccMessage);
begin
  // Do nothing
end;

function TLccDatabaseNode.IsMessageForThisNode(LccMessage: TLccMessage): Boolean;
begin
  Result := True;  // Don't do anything, can't be sure if there is an error in a Database Node or not
end;

procedure TLccDatabaseNode.Login(NewNodeID, RegenerateAliasSeed: Boolean);
begin
  // Do nothing
end;

procedure TLccDatabaseNode.DoUnknownLccCanMessage(LccMessage: TLccMessage);
begin
  // Do nothing
end;


end.

