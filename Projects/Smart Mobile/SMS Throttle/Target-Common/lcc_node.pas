unit lcc_node;

interface

uses
{$IFDEF DWSCRIPT}
  System.Types,
  System.Types.Convert,
  System.Time,
  System.Streams,
  System.Reader,
  System.Writer,
  System.Lists,
  System.Device.Storage,
  SmartCL.Device.Storage,
  SmartCL.Application,
  SmartCL.Components,
  SmartCL.System,
{$ELSE}
  Classes,
  SysUtils,
  ExtCtrls,
  contnrs,
{$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
//  lcc_protocol_traction,
//  lcc_protocol_traction_simpletrainnodeinfo,
//  lcc_protocol_traction_configuruation_functiondefinitioninfo,
//  lcc_protocol_traction_configuration_functions,
  lcc_protocol_memory_configuration,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_protocol_memory_options,
  lcc_protocol_memory_information,
  lcc_protocol_simplenodeinfo,
  lcc_protocol_acdi,
  lcc_protocol_events,
  lcc_protocol_supportedprotocols,
  lcc_protocol_datagram,
  lcc_protocol_base;

const
  ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH = $0001;

type

{ TLccNode }

TLccNode = class(TNodeProtocolBase)
private
  FDatagramWorkerMessage: TLccMessage;
  FInitialized: Boolean;
  FTProtocolMemoryConfigurationDefinitionInfo: TProtocolMemoryConfigurationDefinitionInfo;
  FProtocolMemoryOptions: TProtocolMemoryOptions;
  FMemoryConfiguration: TProtocolMemoryConfiguration;
  FProtocolEventConsumed: TProtocolEvents;
  FProtocolEventsProduced: TProtocolEvents;
  FProtocolSupportedProtocols: TProtocolSupportedProtocols;
  FProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo;
  FProtocolMemoryInfo: TProtocolMemoryInfo;
  FACDIMfg: TACDIMfg;
  FACDIUser: TACDIUser;
  FDatagramResendQueue: TDatagramQueue;
  {$IFDEF DWSCRIPT}
    F_800msTimer: TW3Timer;
  {$ELSE}
    {$IFNDEF FPC_CONSOLE_APP}
    F_800msTimer: TTimer;
    {$ELSE}
    F_800msTimer: TFPTimer;
    {$ENDIF}
  {$ENDIF}

  function GetNodeIDStr: String;
protected
  FNodeID: TNodeID;

  property DatagramWorkerMessage: TLccMessage read FDatagramWorkerMessage write FDatagramWorkerMessage;
  {$IFDEF DWSCRIPT}
    property _800msTimer: TW3Timer read F_800msTimer write F_800msTimer;
  {$ELSE}
    {$IFNDEF FPC_CONSOLE_APP}
    property _800msTimer: TTimer read F_800msTimer write F_800msTimer;
    {$ELSE}
    property _800msTimer: TFPTimer read F_800msTimer write F_800msTimer;
    {$ENDIF}
  {$ENDIF}

  function GetAlias: Word; virtual;
  function IsDestinationEqual(LccMessage: TLccMessage): Boolean; virtual;
  procedure AutoGenerateEvents;
  procedure SendDatagramAckReply(SourceLccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
  procedure SendDatagramRejectedReply(SourceLccMessage: TLccMessage; Reason: Word);
  procedure SendDatagramRequiredReply(SourceLccMessage, ReplyLccMessage: TLccMessage);
  procedure On_800msTimer(Sender: TObject);  virtual;
public
  property DatagramResendQueue: TDatagramQueue read FDatagramResendQueue;
  property NodeID: TNodeID read FNodeID;
  property NodeIDStr: String read GetNodeIDStr;
  property Initialized: Boolean read FInitialized;

  property ACDIMfg: TACDIMfg read FACDIMfg write FACDIMfg;
  property ACDIUser: TACDIUser read FACDIUser write FACDIUser;
  property ProtocolMemoryConfiguration: TProtocolMemoryConfiguration read FMemoryConfiguration write FMemoryConfiguration;
  property ProtocolConfigurationDefinitionInfo: TProtocolMemoryConfigurationDefinitionInfo read FTProtocolMemoryConfigurationDefinitionInfo write FTProtocolMemoryConfigurationDefinitionInfo;
  property ProtocolMemoryOptions: TProtocolMemoryOptions read FProtocolMemoryOptions write FProtocolMemoryOptions;
  property ProtocolMemoryInfo: TProtocolMemoryInfo read FProtocolMemoryInfo write FProtocolMemoryInfo;
  property ProtocolEventConsumed: TProtocolEvents read FProtocolEventConsumed write FProtocolEventConsumed;
  property ProtocolEventsProduced: TProtocolEvents read FProtocolEventsProduced write FProtocolEventsProduced;
  property ProtocolSupportedProtocols: TProtocolSupportedProtocols read FProtocolSupportedProtocols write FProtocolSupportedProtocols;
  property ProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo read FProtocolSimpleNodeInfo write FProtocolSimpleNodeInfo;

  constructor Create(ASendMessageFunc: TLccSendMessageFunc); override;
  destructor Destroy; override;

  function IsNode(ALccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean; virtual;
  procedure Login(ANodeID: TNodeID); virtual;
  procedure Logout; virtual;
  function ProcessMessage(SourceLccMessage: TLccMessage): Boolean; override;
  procedure SendEvents;
  procedure SendConsumedEvents;
  procedure SendConsumerIdentify(var Event: TEventID);
  procedure SendProducedEvents;
  procedure SendProducerIdentify(var Event: TEventID);
  procedure SendInitializeComplete;
end;

{ TLccCanNode }

TLccCanNode = class(TLccNode)
private
  FAliasID: Word;
  FDuplicateAliasDetected: Boolean;
  FInProcessMultiFrameMessage: TObjectList;
  FSeedNodeID: TNodeID;
  FPermitted: Boolean;

  function GetAliasIDStr: String;
protected
  property DuplicateAliasDetected: Boolean read FDuplicateAliasDetected write FDuplicateAliasDetected;
  property InProcessMultiFrameMessage: TObjectList read FInProcessMultiFrameMessage write FInProcessMultiFrameMessage;
  property SeedNodeID: TNodeID read FSeedNodeID write FSeedNodeID;

  procedure CreateNodeID(var Seed: TNodeID);
  function GetAlias: Word; override;
  function GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
  procedure GenerateNewSeed(var Seed: TNodeID);
  procedure InProcessMessageClear;
  procedure InProcessMessageAddMessage(NewMessage: TLccMessage);
  procedure InProcessMessageFlushBySourceAlias(TestMessage: TLccMessage);
  function InProcessMessageFindAndFreeByAliasAndMTI(TestMessage: TLccMessage): Boolean;
  function InProcessMessageFindByAliasAndMTI(TestMessage: TLccMessage): TLccMessage;
  function InProcessMessageRemoveAndFree(AMessage: TLccMessage): Boolean;
  function IsDestinationEqual(LccMessage: TLccMessage): Boolean; override;
  procedure On_800msTimer(Sender: TObject); override;
  procedure Relogin;
  procedure SendAMD;
  procedure SendAMR;
public
   property AliasID: Word read FAliasID;
   property AliasIDStr: String read GetAliasIDStr;
   property Permitted: Boolean read FPermitted;

   constructor Create(ASendMessageFunc: TLccSendMessageFunc); override;
   destructor Destroy; override;
   function IsNode(ALccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean; override;
   procedure Login(ANodeID: TNodeID); override;
   procedure Logout; override;
   function ProcessMessage(SourceLccMessage: TLccMessage): Boolean; override;
end;

var
  InprocessMessageAllocated: Integer = 0;

implementation

const
  STR_SENDMESSAGENIL = 'SendMessage Function is nil';

{ TLccCanNode }

constructor TLccCanNode.Create(ASendMessageFunc: TLccSendMessageFunc);
begin
  inherited Create(ASendMessageFunc);
  FInProcessMultiFrameMessage := TObjectList.Create;
  {$IFNDEF DWSCRIPT}
  InProcessMultiFrameMessage.OwnsObjects := False
  {$ENDIF};
end;

destructor TLccCanNode.Destroy;
begin
  if Permitted then
  begin
    WorkerMessage.LoadAMR(NodeID, AliasID);
    SendMessageFunc(WorkerMessage);
  end;
  InProcessMessageClear;
  InProcessMultiFrameMessage.Free;
  inherited Destroy;
end;

function TLccCanNode.GetAliasIDStr: String;
begin
   Result := '0x' + IntToHex(FAliasID, 4);
end;

procedure TLccCanNode.InProcessMessageClear;
var
  i: Integer;
  AMessage: TLccMessage;
begin
  for i := InProcessMultiFrameMessage.Count - 1 downto 0 do
  begin
    AMessage := TLccMessage(InProcessMultiFrameMessage[i]);
    {$IFDEF DWSCRIPT}
    InProcessMultiFrameMessage.Remove(i);
    {$ELSE}
    InProcessMultiFrameMessage.Delete(i);
    {$ENDIF}
    Dec(InprocessMessageAllocated);
    AMessage.Free
  end;
end;

procedure TLccCanNode.InProcessMessageAddMessage(NewMessage: TLccMessage);
begin
   InProcessMultiFrameMessage.Add(NewMessage);
   Inc(InprocessMessageAllocated);
end;

function TLccCanNode.InProcessMessageFindAndFreeByAliasAndMTI(TestMessage: TLccMessage): Boolean;
begin
  Result := InProcessMessageRemoveAndFree(InProcessMessageFindByAliasAndMTI(TestMessage));
end;

function TLccCanNode.InProcessMessageFindByAliasAndMTI(TestMessage: TLccMessage): TLccMessage;
var
  i: Integer;
  LccMessage: TLccMessage;
begin
  Result := nil;
  for i := 0 to InProcessMultiFrameMessage.Count - 1 do
  begin
    LccMessage := TLccMessage(InProcessMultiFrameMessage[i]);
    if (TestMessage.CAN.SourceAlias = LccMessage.CAN.SourceAlias) and (TestMessage.CAN.DestAlias = LccMessage.CAN.DestAlias) and (TestMessage.MTI = LccMessage.MTI) then
    begin
      Result := LccMessage;
      Break
    end;
  end;

end;

procedure TLccCanNode.InProcessMessageFlushBySourceAlias(TestMessage: TLccMessage);
var
  i: Integer;
  AMessage: TLccMessage;
begin
  for i := InProcessMultiFrameMessage.Count - 1 downto 0  do
  begin
    AMessage := TLccMessage(InProcessMultiFrameMessage[i]);
    if (AMessage.CAN.SourceAlias = TestMessage.CAN.SourceAlias) {or (AMessage.CAN.DestAlias = TestMessage.CAN.SourceAlias)} then
    begin
      {$IFDEF DWSCRIPT}
      InProcessMultiFrameMessage.Remove(i);
      {$ELSE}
      InProcessMultiFrameMessage.Delete(i);
      {$ENDIF}
      Dec(InprocessMessageAllocated);
      AMessage.Free
    end;
  end;
end;

function TLccCanNode.InProcessMessageRemoveAndFree(AMessage: TLccMessage): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Assigned(AMessage) then
  begin
   {$IFDEF DWSCRIPT}
    i := InProcessMultiFrameMessage.IndexOf(AMessage);
    if i > -1 then
    begin
      InProcessMultiFrameMessage.Remove(i);
      Result := True;
    end;
    {$ELSE}
    if InProcessMultiFrameMessage.Remove(AMessage) > -1 then
      Result := True;
    {$ENDIF}
    AMessage.Free;
    Dec(InprocessMessageAllocated);
  end;
end;

function TLccCanNode.IsDestinationEqual(LccMessage: TLccMessage): Boolean;
begin
  Result := AliasID = LccMessage.CAN.DestAlias;
end;

function TLccCanNode.IsNode(ALccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
begin
  Result := False;
  if TestType = ntt_Dest then
  begin
    if (AliasID <> 0) and (ALccMessage.CAN.DestAlias <> 0) then
      Result := AliasID = ALccMessage.CAN.DestAlias
  end else
  if TestType = ntt_Source then
  begin
    if (AliasID <> 0) and (ALccMessage.CAN.SourceAlias <> 0) then
      Result := AliasID = ALccMessage.CAN.SourceAlias
  end;
end;

procedure TLccCanNode.Login(ANodeID: TNodeID);
var
  Temp: TNodeID;
begin
  if NullNodeID(ANodeID) then
    CreateNodeID(ANodeID);
  SeedNodeID := ANodeID;
  Temp := FSeedNodeID;
  FAliasID := GenerateID_Alias_From_Seed(Temp);
  FNodeID := ANodeID;

  WorkerMessage.LoadCID(NodeID, AliasID, 0);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 1);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 2);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 3);
  SendMessageFunc(WorkerMessage);

  _800msTimer.Enabled := True;  //  Next state is in the event handler to see if anyone objects tor our Alias
end;

procedure TLccCanNode.Logout;
begin
  SendAMR;
  FPermitted := False;
  InProcessMessageClear;
  inherited Logout;
end;

procedure TLccCanNode.On_800msTimer(Sender: TObject);
var
  Temp: TNodeID;
begin
  if not Permitted then
  begin
     // Did any node object to this Alias through ProcessMessage?
    if DuplicateAliasDetected then
    begin
      Temp := FSeedNodeID;
      GenerateNewSeed(Temp);     // DWSCRIPT Forced
      FSeedNodeID := Temp;
      FAliasID := GenerateID_Alias_From_Seed(Temp);
      WorkerMessage.LoadCID(NodeID, AliasID, 0);
      SendMessageFunc(WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 1);
      SendMessageFunc(WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 2);
      SendMessageFunc(WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 3);
      SendMessageFunc(WorkerMessage);
    end else
    begin
      FPermitted := True;
      WorkerMessage.LoadRID(AliasID);
      SendMessageFunc(WorkerMessage);
      WorkerMessage.LoadAMD(NodeID, AliasID);
      SendMessageFunc(WorkerMessage);
      inherited Login(NodeID);
    end
  end;
  if Permitted then
    inherited On_800msTimer(Sender);
end;

function TLccCanNode.ProcessMessage(SourceLccMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
  InProcessMessage: TLccMessage;
  i: Integer;
begin
  Result := False;
  // Check for a message with the Alias equal to our own.
  if (AliasID <> 0) and (SourceLccMessage.CAN.SourceAlias = AliasID) then
  begin
    // Check if it is a Check ID message for a node trying to use our Alias and if so tell them no.
    if ((SourceLccMessage.CAN.MTI and $0F000000) >= MTI_CAN_CID6) and ((SourceLccMessage.CAN.MTI and $0F000000) <= MTI_CAN_CID0) then
    begin
      WorkerMessage.LoadRID(AliasID);                   // sorry charlie this is mine
      SendMessageFunc(WorkerMessage);
      Result := True;
    end else
    if Permitted then
    begin
      // Another node used out Alias, stop using this Alias, log out and allocate a new node and relog in
      Logout;
      Relogin;
      Result := True;   // Logout covers any LccNode logoffs, so don't call ancester Process Message
    end
  end;

  if not Permitted then
  begin
    // We are still trying to allocate a new Alias, someone else is using this alias to try atain
    if SourceLccMessage.CAN.SourceAlias = AliasID then
      DuplicateAliasDetected := True;
  end else
  begin
    // Normal message loop once successfully allocating an Alias

    if SourceLccMessage.CAN.IsMultiFrame and SourceLccMessage.DestinationMatchs(AliasID, NodeID) then
    begin
      // This a a multi frame CAN message addressed to us that may need assembling into a fully qualified message before using
      if SourceLccMessage.IsCAN then
      begin
        case SourceLccMessage.CAN.MTI of
          MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY :
            begin
              if InProcessMessageFindAndFreeByAliasAndMTI(SourceLccMessage) then
              begin // If there is another datagram for this node from the same source something is wrong, interleaving is not allowed
                SourceLccMessage.SwapDestAndSourceIDs;
                SendDatagramRejectedReply(SourceLccMessage, REJECTED_OUT_OF_ORDER);
                Exit; // Jump Out
              end else
              begin // Hack to allow Python Scripts to work with limited buffer size
                if InProcessMultiFrameMessage.Count < Max_Allowed_Buffers then
                begin // Convert this CAN message into a fully qualified LccMessage
                  SourceLccMessage.IsCAN := False;
                  SourceLccMessage.CAN.MTI := 0;
                  SourceLccMessage.MTI := MTI_DATAGRAM;
                end else
                begin
                  SourceLccMessage.SwapDestAndSourceIDs;
                  SendDatagramRejectedReply(SourceLccMessage, REJECTED_BUFFER_FULL);
                  Exit; // Jump Out
                end
              end
            end;
          MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START :
            begin
              if InProcessMessageFindAndFreeByAliasAndMTI(SourceLccMessage) then
              begin  // If there is another datagram for this node from the same source something is wrong, interleaving is not allowed
                // We wait for the final frame before we send any error messages
              end else
              begin  // Hack to allow Python Scripts to work with limited buffer size
                if InProcessMultiFrameMessage.Count < Max_Allowed_Buffers then
                begin // Create an InProcessMessage to pickup and concat later CAN datagram messages
                  InProcessMessage := TLccMessage.Create;
                  InProcessMessage.MTI := MTI_DATAGRAM;
                  SourceLccMessage.CopyToTarget(InProcessMessage);
                  InProcessMessageAddMessage(InProcessMessage);
                  Exit;  // Jump out
                end else
                begin
           //       SourceLccMessage.SwapDestAndSourceIDs;
           //       SendDatagramRejectedReply(SourceLccMessage, REJECTED_BUFFER_FULL);
                  Exit; // Jump Out
                end
              end;
            end;
          MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME :
            begin // We wait for the final frame before we send any error messages
              InProcessMessage := InProcessMessageFindByAliasAndMTI(SourceLccMessage);
              if Assigned(InProcessMessage) then
                InProcessMessage.AppendDataArray(SourceLccMessage);
              Exit;  // Jump out
            end;
          MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END :
            begin
              InProcessMessage := InProcessMessageFindByAliasAndMTI(SourceLccMessage);
              if Assigned(InProcessMessage) then
              begin
                InProcessMessage.AppendDataArray(SourceLccMessage);
                InProcessMessage.CopyToTarget(SourceLccMessage);
                InProcessMessageRemoveAndFree(InProcessMessage);  // Don't Free it we are using it.
                SourceLccMessage.IsCAN := False;  // Convert this CAN message into a fully qualified LccMessage
                SourceLccMessage.CAN.MTI := 0;
                SourceLccMessage.MTI := MTI_DATAGRAM;
              end else
              begin


                // HOW TO FIX THIS>>>>
                // Node sends only the End you need to send a Out of Order but Buffer Full also passes the Python scripts....
                // Node sends start but we say buffer full how do we NOT send Out of Order if that node sends an end after a start?


                // Out of order but let the node handle that if needed, note this could be also if we ran out of buffers....
                SourceLccMessage.SwapDestAndSourceIDs;
                SendDatagramRejectedReply(SourceLccMessage, REJECTED_BUFFER_FULL);
          //      SendDatagramRejectedReply(SourceLccMessage, REJECTED_OUT_OF_ORDER);
                Exit; // Jump out
              end;
            end;
          MTI_CAN_FRAME_TYPE_CAN_STREAM_SEND :
            begin

            end
        end
      end else   // Not IsCan
      begin
        if SourceLccMessage.CAN.FramingBits <> $00 then                                // Is it a Multi Frame Message?
        begin
          case SourceLccMessage.CAN.FramingBits of                                     // Train SNIP falls under this now
            $10 : begin   // First Frame
                    if not InProcessMessageFindAndFreeByAliasAndMTI(SourceLccMessage) then
                    begin // If there is another datagram for this node from the same source something is wrong, interleaving is not allowed
                      InProcessMessage := TLccMessage.Create;
                      SourceLccMessage.CopyToTarget(InProcessMessage);
                      InProcessMessageAddMessage(InProcessMessage);
                    end;
                    Exit; // Jump Out
                  end;
            $20 : begin   // Last Frame
                    InProcessMessage := InProcessMessageFindByAliasAndMTI(SourceLccMessage);
                    if Assigned(InProcessMessage) then
                    begin
                      InProcessMessage.AppendDataArray(SourceLccMessage);
                      InProcessMessage.CopyToTarget(SourceLccMessage);
                      InProcessMessageRemoveAndFree(InProcessMessage);
                      // Process the message
                    end else
                    begin
                      // Out of order but let the node handle that if needed (Owned Nodes Only)
                      // Don't swap the IDs, need to find the right target node first
                      SourceLccMessage.LoadOptionalInteractionRejected(SourceLccMessage.DestID, SourceLccMessage.CAN.DestAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, REJECTED_OUT_OF_ORDER, SourceLccMessage.MTI);
                      SendMessageFunc(SourceLccMessage);
                      Exit; // Move on
                    end;
                  end;
            $30 : begin   // Middle Frame
                    InProcessMessage := InProcessMessageFindByAliasAndMTI(SourceLccMessage);
                    if Assigned(InProcessMessage) then
                    begin
                      InProcessMessage.AppendDataArray(SourceLccMessage);
                      InProcessMessage.CopyToTarget(SourceLccMessage);
                    end;
                    Exit; // Move on
                  end;
          end;
        end else  // Is it a SNIP... special case as the Framing Bits did not exist yet
        if (SourceLccMessage.MTI = MTI_SIMPLE_NODE_INFO_REPLY) then
        begin
          InProcessMessage := InProcessMessageFindByAliasAndMTI(SourceLccMessage);
          if Assigned(InProcessMessage) then
          begin
            if InProcessMessage.AppendDataArrayAsString(SourceLccMessage, 6) then
            begin
              InProcessMessage.CopyToTarget(SourceLccMessage);
              InProcessMessageRemoveAndFree(InProcessMessage);
              // Run the message
            end else
              Exit; // Move on
          end else
          begin
            InProcessMessage := TLccMessage.Create;
            SourceLccMessage.CopyToTarget(InProcessMessage);
            for i := 0 to InProcessMessage.DataCount - 1 do
            begin
              if InProcessMessage.DataArray[i] = Ord(#0) then
                InProcessMessage.CAN.iTag := InProcessMessage.CAN.iTag + 1
            end;
            InProcessMessageAddMessage(InProcessMessage);
            Exit; // Move on
          end
        end
      end
    end;

    TestNodeID[0] := 0;
    TestNodeID[1] := 0;
    if SourceLccMessage.IsCAN then
    begin
      case SourceLccMessage.CAN.MTI of
        MTI_CAN_AME :          // Alias Map Enquiry
          begin
            if SourceLccMessage.DataCount = 6 then
            begin
              SourceLccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
              if EqualNodeID(TestNodeID, NodeID, False) then
              begin
                WorkerMessage.LoadAMD(NodeID, AliasID);
                SendMessageFunc(WorkerMessage);
              end
            end else
            begin
              WorkerMessage.LoadAMD(NodeID, AliasID);
              SendMessageFunc(WorkerMessage);
            end;
            Result := True;
          end;
        MTI_CAN_AMR : InProcessMessageFlushBySourceAlias(SourceLccMessage); // If the Alias is being reset flush all messages associated with it
        MTI_CAN_AMD : InProcessMessageFlushBySourceAlias(SourceLccMessage); // If the Alias now coming on line, any old messages for this Alias are out dated
      end
    end;
    if not Result then
      Result := inherited ProcessMessage(SourceLccMessage);
  end;
end;

procedure TLccCanNode.Relogin;
var
  Temp: TNodeID;
begin
  // Typically due to an alias conflict to create a new one
  Temp := FSeedNodeID;
  GenerateNewSeed(Temp);   // DWSCRIPT forced
  FSeedNodeID := Temp;
  FAliasID := GenerateID_Alias_From_Seed(Temp);
  WorkerMessage.LoadCID(NodeID, AliasID, 0);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 1);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 2);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 3);
  SendMessageFunc(WorkerMessage);

  _800msTimer.Enabled := True;  //  Next state is in the event handler to see if anyone objects tor our Alias
end;

function TLccCanNode.GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
begin
  Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
end;

procedure TLccCanNode.CreateNodeID(var Seed: TNodeID);
begin
  Randomize;
  Seed[1] := StrToInt('0x020112');
  {$IFDEF DWSCRIPT}
  Seed[0] := RandomInt($FFFFFF);
  {$ELSE}
  Seed[0] := Random($FFFFFF);
  {$ENDIF}
end;

procedure TLccCanNode.GenerateNewSeed(var Seed: TNodeID);
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

function TLccCanNode.GetAlias: Word;
begin
  Result := AliasID;
end;

procedure TLccCanNode.SendAMD;
begin
  if Permitted then
  begin
    WorkerMessage.LoadAMD(NodeID, AliasID);
    SendMessageFunc(WorkerMessage);
  end;
end;

procedure TLccCanNode.SendAMR;
begin
  if Permitted then
  begin
    FPermitted := False;
    WorkerMessage.LoadAMR(NodeID, AliasID);
    SendMessageFunc(WorkerMessage);
  end;
end;



{TLccNode }

function TLccNode.GetNodeIDStr: String;
begin
 Result := IntToHex(NodeID[1], 6);
 Result := Result + IntToHex(NodeID[0], 6);
 Result := '0x' + Result
end;

function TLccNode.IsDestinationEqual(LccMessage: TLccMessage): Boolean;
begin
  Result := EqualNodeID(NodeID, LccMessage.DestID, False);
end;

function TLccNode.IsNode(ALccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
begin
  Result := False;
  if TestType = ntt_Dest then
  begin
    if ALccMessage.HasDestNodeID and not NullNodeID(NodeID) then
      Result := ((NodeID[0] = ALccMessage.DestID[0]) and (NodeID[1] = ALccMessage.DestID[1]))
  end else
  if TestType = ntt_Source then
  begin
    if ALccMessage.HasSourceNodeID and not NullNodeID(NodeID) then
      Result := ((NodeID[0] = ALccMessage.SourceID[0]) and (NodeID[1] = ALccMessage.SourceID[1]))
  end;
end;

constructor TLccNode.Create(ASendMessageFunc: TLccSendMessageFunc);
begin
  inherited Create(ASendMessageFunc);
  FProtocolSupportedProtocols := TProtocolSupportedProtocols.Create(ASendMessageFunc);
  FProtocolSimpleNodeInfo := TProtocolSimpleNodeInfo.Create(ASendMessageFunc);
  FTProtocolMemoryConfigurationDefinitionInfo := TProtocolMemoryConfigurationDefinitionInfo.Create(ASendMessageFunc, MSI_CDI, True);
  FProtocolMemoryOptions := TProtocolMemoryOptions.Create(ASendMessageFunc);
  FMemoryConfiguration := TProtocolMemoryConfiguration.Create(SendMessageFunc, MSI_CONFIG, False);
  FProtocolMemoryInfo := TProtocolMemoryInfo.Create(ASendMessageFunc);
 //JDK FConfigurationMem := TConfigurationMemory.Create(ASendMessageFunc);
  FProtocolEventConsumed := TProtocolEvents.Create(ASendMessageFunc);
  FProtocolEventsProduced := TProtocolEvents.Create(ASendMessageFunc);
  FACDIMfg := TACDIMfg.Create(nil, MSI_ACDI_MFG, True);
  FACDIUser := TACDIUser.Create(SendMessageFunc, MSI_ACDI_USER, True);

  FDatagramResendQueue := TDatagramQueue.Create(SendMessageFunc);
  FDatagramWorkerMessage := TLccMessage.Create;

  {$IFDEF DWSCRIPT}
  _800msTimer := TW3Timer.Create(nil);
  _800msTimer.Enabled := False;
  _800msTimer.OnTime := @On_800msTimer;
  _800msTimer.Delay := 800;
  {$ELSE}
    {$IFNDEF FPC_CONSOLE_APP}
    _800msTimer := TTimer.Create(nil);
    {$ELSE}
    _800msTimer := TFPTimer.Create(nil);
    {$ENDIF}
    _800msTimer.Enabled := False;
    _800msTimer.OnTimer := @On_800msTimer;
    _800msTimer.Interval := 800;
  {$ENDIF}
end;

procedure TLccNode.AutoGenerateEvents;
var
  i: Integer;
  TempEventID: TEventID;
begin
  TempEventID := NULL_EVENT_ID;
  if ProtocolEventConsumed.AutoGenerate.Count > 0 then
  begin
    for i := 0 to ProtocolEventConsumed.AutoGenerate.Count - 1 do
    begin
      NodeIDToEventID(NodeID, ProtocolEventConsumed.AutoGenerate.StartIndex + i, TempEventID);
      ProtocolEventConsumed.Add(TempEventID, ProtocolEventConsumed.AutoGenerate.DefaultState);
    end;
    ProtocolEventConsumed.Valid := True;
  end;

  if ProtocolEventsProduced.AutoGenerate.Count > 0 then
  begin
    for i := 0 to ProtocolEventsProduced.AutoGenerate.Count - 1 do
    begin
      NodeIDToEventID(NodeID, ProtocolEventsProduced.AutoGenerate.StartIndex + i, TempEventID);
      ProtocolEventsProduced.Add(TempEventID, ProtocolEventsProduced.AutoGenerate.DefaultState);
    end;
    ProtocolEventsProduced.Valid := True;
  end;
end;

destructor TLccNode.Destroy;
begin
  _800msTimer.Enabled := False;
  _800msTimer.Free;
  FProtocolSupportedProtocols.Free;
  FProtocolSimpleNodeInfo.Free;
  FTProtocolMemoryConfigurationDefinitionInfo.Free;
 //JDK FConfigurationMem.Free;
  FProtocolEventConsumed.Free;
  FProtocolEventsProduced.Free;
  FProtocolMemoryOptions.Free;
  FProtocolMemoryInfo.Free;
  FACDIMfg.Free;
  FACDIUser.Free;
  FMemoryConfiguration.Free;
  FDatagramResendQueue.Free;
  FDatagramWorkerMessage.Free;
  inherited;
end;

function TLccNode.GetAlias: Word;
begin
  Result := 0;
end;

procedure TLccNode.Login(ANodeID: TNodeID);
begin
  FInitialized := True;
  SendInitializeComplete;
  AutoGenerateEvents;
  SendEvents;
end;

procedure TLccNode.Logout;
begin
 FInitialized := False;
  _800msTimer.Enabled := False;
  DatagramResendQueue.Clear;
end;

procedure TLccNode.On_800msTimer(Sender: TObject);
begin
  DatagramResendQueue.TickTimeout;
end;

function TLccNode.ProcessMessage(SourceLccMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
  Temp: TEventID;
begin
  Result := False;

  TestNodeID[0] := 0;
  TestNodeID[1] := 0;

 // if Initialized then;
  begin
    // First look for a duplicate NodeID
    if EqualNodeID(NodeID, SourceLccMessage.SourceID, False) then
    begin
      Logout;
      Exit;
    end;


    // Next look to see if it is an addressed message and if not for use just exit


    if SourceLccMessage.HasDestination then
    begin
      if not IsDestinationEqual(SourceLccMessage) then
        Exit;
    end;

    case SourceLccMessage.MTI of
      MTI_OPTIONAL_INTERACTION_REJECTED :
          begin
            // TODO need a call back handler
          end;
      MTI_VERIFY_NODE_ID_NUMBER      :
          begin
            if SourceLccMessage.DataCount = 6 then
            begin
              SourceLccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
              if EqualNodeID(TestNodeID, NodeID, False) then
              begin
                WorkerMessage.LoadVerifiedNodeID(NodeID, GetAlias);
                SendMessageFunc(WorkerMessage);
              end
            end else
            begin
              WorkerMessage.LoadVerifiedNodeID(NodeID, GetAlias);
              SendMessageFunc(WorkerMessage);
            end;
            Result := True;
          end;
      MTI_VERIFY_NODE_ID_NUMBER_DEST :
          begin
            WorkerMessage.LoadVerifiedNodeID(NodeID, GetAlias);
            SendMessageFunc(WorkerMessage);
            Result := True;
          end;
      MTI_VERIFIED_NODE_ID_NUMBER :
          begin
             // TODO need a call back handler
          end;
      MTI_SIMPLE_NODE_INFO_REQUEST :
          begin
            WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, ProtocolSimpleNodeInfo.PackedFormat);
            SendMessageFunc(WorkerMessage);
            Result := True;
          end;
      MTI_SIMPLE_NODE_INFO_REPLY :
          begin  // Called if I send a SNIP Request and the other node replies
            // TODO need a call back handler
            Result := True;
          end;
      MTI_PROTOCOL_SUPPORT_INQUIRY :
          begin
            WorkerMessage.LoadProtocolIdentifyReply(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, ProtocolSupportedProtocols.EncodeFlags);
            SendMessageFunc(WorkerMessage);
            Result := True;
          end;
      MTI_PROTOCOL_SUPPORT_REPLY :
          begin   // Called if I send a Protocol Support and loads the ProtocolSupportedProtocols with the data
            // TODO need a call back handler
            Result := True;
          end;
      MTI_EVENTS_IDENTIFY :
          begin
            SendConsumedEvents;
            SendProducedEvents;
            Result := True;
          end;
      MTI_EVENTS_IDENTIFY_DEST :
          begin
            SendConsumedEvents;  // already known the destination is us
            SendProducedEvents;
            Result := True;
          end;
      MTI_PRODUCER_IDENDIFY :
          begin
            Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
            SendProducerIdentify(Temp);         // Compatible with Smart Pascal
            Result := True;
          end;
      MTI_CONSUMER_IDENTIFY :
          begin
            Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
            SendConsumerIdentify(Temp);        // Compatible with Smart Pascal
            Result := True;
          end;
       MTI_CONSUMER_IDENTIFIED_CLEAR :
          begin
            // TODO need a call back handler
          end;
       MTI_CONSUMER_IDENTIFIED_SET :
          begin
           // TODO need a call back handler
          end;
       MTI_CONSUMER_IDENTIFIED_UNKNOWN :
          begin
            // TODO need a call back handler
          end;
       MTI_PRODUCER_IDENTIFIED_CLEAR :
          begin
            // TODO need a call back handler
          end;
       MTI_PRODUCER_IDENTIFIED_SET :
          begin
            // TODO need a call back handler
          end;
       MTI_PRODUCER_IDENTIFIED_UNKNOWN :
          begin
            // TODO need a call back handler
          end;
       MTI_DATAGRAM_REJECTED_REPLY :
         begin
           DatagramResendQueue.Resend(SourceLccMessage);
         end;
       MTI_DATAGRAM_OK_REPLY :
         begin
           DatagramResendQueue.Remove(SourceLccMessage);
         end;
       MTI_DATAGRAM :
         begin
           case SourceLccMessage.DataArrayIndexer[0] of
             DATAGRAM_PROTOCOL_CONFIGURATION :
               begin
                 case SourceLccMessage.DataArrayIndexer[1] and $F0 of
                   MCP_WRITE :
                     begin
                       case SourceLccMessage.DataArrayIndexer[1] and $03 of
                         MCP_NONE :
                             begin
                               case SourceLccMessage.DataArrayIndexer[6] of
                                 MSI_CDI             :
                                     begin
                                     end;  // Not writeable
                                 MSI_ALL             :
                                     begin
                                     end;  // Not writeable
                                 MSI_CONFIG          :
                                     begin
                                       SendDatagramAckReply(SourceLccMessage, False, 0);     // We will be sending a Write Reply
                                       ProtocolMemoryConfiguration.WriteRequest(SourceLccMessage);
                                       Result := True;
                                     end;
                                 MSI_ACDI_MFG        :
                                     begin
                                     end;  // Not writeable
                                 MSI_ACDI_USER       :
                                     begin
                                       SendDatagramAckReply(SourceLccMessage, False, 0);     // We will be sending a Write Reply
                                       ACDIUser.WriteRequest(SourceLccMessage);
                                       Result := True;
                                     end;
                                 MSI_FDI             :
                                     begin
                                     end;  // Not writeable
                                 MSI_FUNCTION_CONFIG :
                                     begin
                                     end;
                               end
                             end;
                         MCP_CONFIGURATION :
                             begin
                               SendDatagramAckReply(SourceLccMessage, False, 0);             // We will be sending a Write Reply
                               ProtocolMemoryConfiguration.WriteRequest(SourceLccMessage);
                               Result := True;
                             end;
                         MCP_ALL           :
                             begin
                             end; // Not writeable
                         MCP_CDI           :
                             begin
                             end; // Not writeable
                       end;
                     end;
                   MCP_WRITE_STREAM :
                       begin
                       end;
                   MCP_READ :
                       begin
                         case SourceLccMessage.DataArrayIndexer[1] and $03 of
                           MCP_NONE :
                               begin
                                 case SourceLccMessage.DataArrayIndexer[6] of
                                   MSI_CDI             :
                                       begin
                                         WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                                    SourceLccMessage.CAN.SourceAlias);
                                         ProtocolConfigurationDefinitionInfo.LoadReply(SourceLccMessage, WorkerMessage);
                                         SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                                         Result := True;
                                       end;
                                   MSI_ALL             :
                                       begin
                                         SendDatagramAckReply(SourceLccMessage, False, 0);   // We won't be sending a Read Reply
                                       end;
                                   MSI_CONFIG          :
                                       begin
                                         WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                                    SourceLccMessage.CAN.SourceAlias);
                                         ProtocolMemoryConfiguration.LoadReply(SourceLccMessage, WorkerMessage);
                                         SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                                         Result := True;
                                       end;
                                   MSI_ACDI_MFG        :
                                       begin
                                         WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                                    SourceLccMessage.CAN.SourceAlias);
                                         ACDIMfg.LoadReply(SourceLccMessage, WorkerMessage);
                                         SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                                         Result := True;
                                       end;
                                   MSI_ACDI_USER       :
                                       begin
                                         WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                                    SourceLccMessage.CAN.SourceAlias);
                                         ACDIUser.LoadReply(SourceLccMessage, WorkerMessage);
                                         SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                                         Result := True;
                                       end;
                                   MSI_FDI             :
                                        begin
                                        end;
                                   MSI_FUNCTION_CONFIG :
                                        begin
                                        end;
                                 end
                               end;
                           MCP_CONFIGURATION : begin
                                                 WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                                            SourceLccMessage.CAN.SourceAlias);
                                                 ProtocolMemoryConfiguration.LoadReply(SourceLccMessage, WorkerMessage);
                                                 SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                                                 Result := True;
                                               end;
                           MCP_ALL           : begin  end;
                           MCP_CDI           : begin
                                                 WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                                            SourceLccMessage.CAN.SourceAlias);
                                                 ProtocolConfigurationDefinitionInfo.LoadReply(SourceLccMessage, WorkerMessage);
                                                 SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                                                 Result := True;
                                               end;
                         end;
                       end;
                   MCP_READ_STREAM :
                       begin
                       end;
                   MCP_OPERATION :
                       begin
                         case SourceLccMessage.DataArrayIndexer[1] of
                           MCP_OP_GET_CONFIG :
                               begin
                                 WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                            SourceLccMessage.CAN.SourceAlias);
                                 ProtocolMemoryOptions.LoadReply(WorkerMessage);
                                 SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                                 Result := True;
                               end;
                           MCP_OP_GET_ADD_SPACE_INFO :
                               begin
                                 WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                            SourceLccMessage.CAN.SourceAlias);
                                 ProtocolMemoryInfo.LoadReply(SourceLccMessage, WorkerMessage);
                                 SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
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
                         end // case
                       end
                 end; // case
               end
           else begin
               // Unknown Datagram Type
               WorkerMessage.LoadDatagramRejected(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                  SourceLccMessage.CAN.SourceAlias,
                                                  REJECTED_DATAGRAMS_NOT_ACCEPTED);
               SendMessageFunc(WorkerMessage);
               Result := True;
             end;
           end;  // case
         end;
    else begin
        if SourceLccMessage.HasDestination then
        begin
          WorkerMessage.LoadOptionalInteractionRejected(NodeID, GetAlias, SourceLccMessage.SourceID,
                                                        SourceLccMessage.CAN.SourceAlias,
                                                        REJECTED_BUFFER_FULL, SourceLccMessage.MTI);
          SendMessageFunc(WorkerMessage);
          Result := True;
        end;
      end;
    end; // case
  end;
end;

procedure TLccNode.SendDatagramAckReply(SourceLccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  // Only Ack if we accept the datagram
  DatagramWorkerMessage.LoadDatagramAck(SourceLccMessage.DestID, SourceLccMessage.CAN.DestAlias,
                                        SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias,
                                        True, ReplyPending, TimeOutValueN);
  SendMessageFunc(DatagramWorkerMessage);
end;

procedure TLccNode.SendConsumedEvents;
var
  i: Integer;
  Temp: TEventID;
begin
  for i := 0 to ProtocolEventConsumed.Count - 1 do
  begin
    Temp := ProtocolEventConsumed.Event[i].ID;
    WorkerMessage.LoadConsumerIdentified(NodeID, GetAlias, Temp, ProtocolEventConsumed.Event[i].State);
    SendMessageFunc(WorkerMessage);
  end;
end;

procedure TLccNode.SendConsumerIdentify(var Event: TEventID);
var
  EventObj: TLccEvent;
  Temp: TEventID;
begin
  EventObj := ProtocolEventConsumed.Supports(Event);
  if Assigned(EventObj) then
  begin
    Temp := EventObj.ID;
    WorkerMessage.LoadConsumerIdentified(NodeID, GetAlias, Temp, EventObj.State);
    SendMessageFunc(WorkerMessage);
  end;
end;

procedure TLccNode.SendDatagramRejectedReply(SourceLccMessage: TLccMessage; Reason: Word);
begin
  DatagramWorkerMessage.LoadDatagramRejected(SourceLccMessage.DestID, SourceLccMessage.CAN.DestAlias,
                                             SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias,
                                             Reason);
  SendMessageFunc(DatagramWorkerMessage);
end;

procedure TLccNode.SendDatagramRequiredReply(SourceLccMessage, ReplyLccMessage: TLccMessage);
begin
 if ReplyLccMessage.UserValid then
  begin
    if DatagramResendQueue.Add(ReplyLccMessage.Clone) then     // Waiting for an ACK
    begin
      SendDatagramAckReply(SourceLccMessage, False, 0);   // We will be sending a Read Reply
      SendMessageFunc(ReplyLccMessage);
    end else
      SendDatagramRejectedReply(SourceLccMessage, REJECTED_BUFFER_FULL)
   end;
end;

procedure TLccNode.SendEvents;
begin
  SendConsumedEvents;
  SendProducedEvents;
end;

procedure TLccNode.SendInitializeComplete;
begin
  WorkerMessage.LoadInitializationComplete(NodeID, GetAlias);
  SendMessageFunc(WorkerMessage);
end;

procedure TLccNode.SendProducedEvents;
var
  i: Integer;
  Temp: TEventID;
begin
  for i := 0 to ProtocolEventsProduced.Count - 1 do
  begin
    Temp := ProtocolEventsProduced.Event[i].ID;
    WorkerMessage.LoadProducerIdentified(NodeID, GetAlias, Temp, ProtocolEventsProduced.Event[i].State);
    SendMessageFunc(WorkerMessage);
  end;
end;

procedure TLccNode.SendProducerIdentify(var Event: TEventID);
var
  EventObj: TLccEvent;
  Temp: TEventID;
begin
  EventObj := ProtocolEventsProduced.Supports(Event);
  if Assigned(EventObj) then
  begin
    Temp := EventObj.ID;
    WorkerMessage.LoadProducerIdentified(NodeID, GetAlias, Temp, EventObj.State);
    SendMessageFunc(WorkerMessage);
  end;
end;


initialization

finalization

end.

