unit lcc_node;

interface

{$IFNDEF DWSCRIPT}
{$I lcc_compilers.inc}
{$ENDIF}

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
  {$IFNDEF ULTIBO}
    {$IFDEF FPC}
      ExtCtrls,
    {$ELSE}
      System.Types,
      FMX.Types,
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF DWSCRIPT}
    {$IFDEF FPC}
      contnrs,
    {$ELSE}
      System.Generics.Collections,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
  lcc_protocol_traction,
  lcc_protocol_traction_simpletrainnodeinfo,
  lcc_protocol_traction_configuation_functiondefinitioninfo,
  lcc_protocol_traction_configuration_functions,
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
TLccNodeClass = class of TLccNode;

TLccNode = class(TObject)
private
  FWorkerMessageDatagram: TLccMessage;
  FInitialized: Boolean;
  FNodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF};
  FSendMessageFunc: TLccSendMessageFunc;
  FStreamManufacturerData: TMemoryStream;        // Stream containing the Manufacturer Data stored like the User data with Fixed Offsets for read only data
                                                 // SNIP uses this structure to create a packed version of this information (null separated strings) +
                                                 // the user name and user description which it pulls out of the Configuration Stream
                                                 // Address 0 = Version
                                                 // Address 1 = Manufacturer
                                                 // Address 42 = Model
                                                 // Address 83 = Hardware Version
                                                 // Address 104 = Software Version
  FStreamCdi: TMemoryStream;                     // Stream containing the XML string for the CDI (Configuration Definition Info)
  FStreamConfig: TMemoryStream;                  // Stream containing the writable configuration memory where the Address = Offset in the stream
                                                 // and the following MUST be true
                                                 // Address 0 = User info Version number
                                                 // Address 1 = User Defined name (ACDI/SNIP)
                                                 // Address 64 = User defined description  (ACDI/SNIP)
                                                 // Address 128 = Node specific persistent data
  FStreamTractionConfig: TMemoryStream;          // Stream containing the writable configuration memory for a Traction node where the Address = Offset in the stream
  FStreamTractionFdi: TMemoryStream;             // Stream containing the XML string for the FDI (Function Definition Info)
  FTProtocolMemoryConfigurationDefinitionInfo: TProtocolMemoryConfigurationDefinitionInfo;
  FProtocolMemoryOptions: TProtocolMemoryOptions;
  FProtocolMemoryConfiguration: TProtocolMemoryConfiguration;
  FProtocolEventConsumed: TProtocolEvents;
  FProtocolEventsProduced: TProtocolEvents;
  FProtocolSupportedProtocols: TProtocolSupportedProtocols;
  FProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo;
  FProtocolMemoryInfo: TProtocolMemoryInfo;
  FProtocolTractionSimpleTrainNodeInfo: TTractionProtocolSimpleTrainNodeInfo;
  FProtocolTraction: TProtocolTraction;
  FProtocolTractionFunctionDefinitionInfo: TTractionFunctionDefinitionInfo;
  FProtocolTractionMemoryFunctionConfiguration: TTractionFunctionConfiguration;
  FACDIMfg: TACDIMfg;
  FACDIUser: TACDIUser;
  FDatagramResendQueue: TDatagramQueue;
  FWorkerMessage: TLccMessage;
  F_800msTimer: TLccTimer;

  function GetNodeIDStr: String;
protected
  FNodeID: TNodeID;

  property NodeManager:{$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF} read FNodeManager write FNodeManager;
  property SendMessageFunc: TLccSendMessageFunc read FSendMessageFunc;
  property StreamCdi: TMemoryStream read FStreamCdi write FStreamCdi;
  property StreamConfig: TMemoryStream read FStreamConfig write FStreamConfig;
  property StreamManufacturerData: TMemoryStream read FStreamManufacturerData write FStreamManufacturerData;
  property StreamTractionFdi: TMemoryStream read FStreamTractionFdi write FStreamTractionFdi;
  property StreamTractionConfig: TMemoryStream read FStreamTractionConfig write FStreamTractionConfig;

  property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  property WorkerMessageDatagram: TLccMessage read FWorkerMessageDatagram write FWorkerMessageDatagram;
  property _800msTimer: TLccTimer read F_800msTimer write F_800msTimer;

  procedure CreateNodeID(var Seed: TNodeID);
  function GetAlias: Word; virtual;
  function FindCdiElement(TestXML, Element: string; var Offset: Integer; var ALength: Integer): Boolean;
  function IsDestinationEqual(LccMessage: TLccMessage): Boolean; virtual;
  function LoadManufacturerDataStream(ACdi: string): Boolean;
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
  property ProtocolMemoryConfiguration: TProtocolMemoryConfiguration read FProtocolMemoryConfiguration write FProtocolMemoryConfiguration;
  property ProtocolConfigurationDefinitionInfo: TProtocolMemoryConfigurationDefinitionInfo read FTProtocolMemoryConfigurationDefinitionInfo write FTProtocolMemoryConfigurationDefinitionInfo;
  property ProtocolMemoryOptions: TProtocolMemoryOptions read FProtocolMemoryOptions write FProtocolMemoryOptions;
  property ProtocolMemoryInfo: TProtocolMemoryInfo read FProtocolMemoryInfo write FProtocolMemoryInfo;
  property ProtocolEventConsumed: TProtocolEvents read FProtocolEventConsumed write FProtocolEventConsumed;
  property ProtocolEventsProduced: TProtocolEvents read FProtocolEventsProduced write FProtocolEventsProduced;
  property ProtocolSupportedProtocols: TProtocolSupportedProtocols read FProtocolSupportedProtocols write FProtocolSupportedProtocols;
  property ProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo read FProtocolSimpleNodeInfo write FProtocolSimpleNodeInfo;
  property ProtocolTraction: TProtocolTraction read FProtocolTraction write FProtocolTraction;
  property ProtocolTractionFunctionDefinitionInfo: TTractionFunctionDefinitionInfo read FProtocolTractionFunctionDefinitionInfo write FProtocolTractionFunctionDefinitionInfo;
  property ProtocolTractionMemoryFunctionConfiguration: TTractionFunctionConfiguration read FProtocolTractionMemoryFunctionConfiguration write FProtocolTractionMemoryFunctionConfiguration;
  property ProtocolTractionSimpleTrainNodeInfo: TTractionProtocolSimpleTrainNodeInfo read FProtocolTractionSimpleTrainNodeInfo write FProtocolTractionSimpleTrainNodeInfo;

  constructor Create(ASendMessageFunc: TLccSendMessageFunc; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string); virtual;
  destructor Destroy; override;

  function IsNode(ALccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean; virtual;
  procedure Login(ANodeID: TNodeID); virtual;
  procedure Logout; virtual;
  function ProcessMessage(SourceLccMessage: TLccMessage): Boolean; virtual;
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
  {$IFDEF DELPHI}
  FInProcessMultiFrameMessage: TObjectList<TLccMessage>;
  {$ELSE}
  FInProcessMultiFrameMessage: TObjectList;
  {$ENDIF}
  FSeedNodeID: TNodeID;
  FPermitted: Boolean;

  function GetAliasIDStr: String;
protected
  property DuplicateAliasDetected: Boolean read FDuplicateAliasDetected write FDuplicateAliasDetected;
  {$IFDEF DELPHI}
  property InProcessMultiFrameMessage: TObjectList<TLccMessage> read FInProcessMultiFrameMessage write FInProcessMultiFrameMessage;
  {$ELSE}
  property InProcessMultiFrameMessage: TObjectList read FInProcessMultiFrameMessage write FInProcessMultiFrameMessage;
  {$ENDIF}
  property SeedNodeID: TNodeID read FSeedNodeID write FSeedNodeID;

  procedure Creating; virtual;
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

   constructor Create(ASendMessageFunc: TLccSendMessageFunc; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string); reintroduce;
   destructor Destroy; override;
   function IsNode(ALccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean; override;
   procedure Login(ANodeID: TNodeID); override;
   procedure Logout; override;
   function ProcessMessage(SourceLccMessage: TLccMessage): Boolean; override;
end;

var
  InprocessMessageAllocated: Integer = 0;

implementation

uses
  lcc_node_manager;

{ TLccCanNode }

constructor TLccCanNode.Create(ASendMessageFunc: TLccSendMessageFunc; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string);
begin
  inherited Create(ASendMessageFunc, ANodeManager, CdiXML);
  {$IFDEF DELPHI}
  FInProcessMultiFrameMessage := TObjectList<TLccMessage>.Create;
  {$ELSE}
  FInProcessMultiFrameMessage := TObjectList.Create;
  {$ENDIF}
  {$IFNDEF DWSCRIPT}
  InProcessMultiFrameMessage.OwnsObjects := False
  {$ENDIF};
  Creating;
end;

procedure TLccCanNode.Creating;
begin

end;

destructor TLccCanNode.Destroy;
begin
  InProcessMessageClear;
  InProcessMultiFrameMessage.Free;
  FAliasID := 0;
  (NodeManager as INodeManagerCallbacks).DoAliasIDChanged(Self);
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
{$IFDEF DWSCRIPT}
var
  i: Integer;
{$ENDIF}
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
  (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);
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
      DuplicateAliasDetected := False;  // Reset
      Temp := FSeedNodeID;
      GenerateNewSeed(Temp);
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
      (NodeManager as INodeManagerCallbacks).DoAliasIDChanged(Self);
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
           //       SendDatagramRejectedReply(SourceLccMessage, REJECTED_BUFFER_FULL);        // See below note in the END FRAME
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
  GenerateNewSeed(Temp);
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
    (NodeManager as INodeManagerCallbacks).DoCANAliasMapReset(Self);
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

function TLccNode.LoadManufacturerDataStream(ACdi: string): Boolean;
var
  AnOffset, ALength, i: Integer;
begin
  Result := False;

  StreamManufacturerData.Size := LEN_MANUFACTURER_INFO;
  for i := 0 to StreamManufacturerData.Size - 1 do
    StreamWriteByte(StreamManufacturerData, 0);

  StreamManufacturerData.Position := ADDRESS_VERSION;
  StreamWriteByte(StreamManufacturerData, 1);

  AnOffset := 0;
  ALength := 0;
  if FindCdiElement(ACdi, '<manufacturer>', AnOffset, ALength) then
  begin
    if ALength < LEN_MFG_NAME then
    begin
      StreamManufacturerData.Position := ADDRESS_MFG_NAME;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<model>', AnOffset, ALength) then
  begin
    if ALength < LEN_MODEL_NAME then
    begin
      StreamManufacturerData.Position := ADDRESS_MODEL_NAME;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<hardwareVersion>', AnOffset, ALength) then
  begin
    if ALength < LEN_HARDWARE_VERSION then
    begin
      StreamManufacturerData.Position := ADDRESS_HARDWARE_VERSION;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<softwareVersion>', AnOffset, ALength) then
  begin
    if ALength < LEN_SOFTWARE_VERSION then
    begin
      StreamManufacturerData.Position := ADDRESS_SOFTWARE_VERSION;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  Result := True;
end;

constructor TLccNode.Create(ASendMessageFunc: TLccSendMessageFunc;
  ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string);
var
  i, Counter: Integer;
begin
  inherited Create;
  FProtocolSupportedProtocols := TProtocolSupportedProtocols.Create(ASendMessageFunc);
  FProtocolSimpleNodeInfo := TProtocolSimpleNodeInfo.Create(ASendMessageFunc);
  FTProtocolMemoryConfigurationDefinitionInfo := TProtocolMemoryConfigurationDefinitionInfo.Create(ASendMessageFunc);
  FProtocolMemoryOptions := TProtocolMemoryOptions.Create(ASendMessageFunc);
  FProtocolMemoryConfiguration := TProtocolMemoryConfiguration.Create(SendMessageFunc);
  FProtocolMemoryInfo := TProtocolMemoryInfo.Create(ASendMessageFunc);
  FProtocolEventConsumed := TProtocolEvents.Create(ASendMessageFunc);
  FProtocolEventsProduced := TProtocolEvents.Create(ASendMessageFunc);

  FProtocolTraction := TProtocolTraction.Create(ASendMessageFunc);
  FProtocolTractionFunctionDefinitionInfo := TTractionFunctionDefinitionInfo.Create(ASendMessageFunc);
  FProtocolTractionMemoryFunctionConfiguration := TTractionFunctionConfiguration.Create(ASendMessageFunc);
  FProtocolTractionSimpleTrainNodeInfo := TTractionProtocolSimpleTrainNodeInfo.Create(ASendMessageFunc);

  FACDIMfg := TACDIMfg.Create(ASendMessageFunc);
  FACDIUser := TACDIUser.Create(ASendMessageFunc);
  FStreamCdi := TMemoryStream.Create;
  FStreamConfig := TMemoryStream.Create;
  FStreamManufacturerData := TMemoryStream.Create;
  FStreamTractionConfig := TMemoryStream.Create;
  FStreamTractionFdi := TMemoryStream.Create;

  FDatagramResendQueue := TDatagramQueue.Create(ASendMessageFunc);
  FWorkerMessageDatagram := TLccMessage.Create;
  FWorkerMessage := TLccMessage.Create;
  FSendMessageFunc := ASendMessageFunc;
  FNodeManager := ANodeManager;

  _800msTimer := TLccTimer.Create(nil);
  _800msTimer.Enabled := False;
  {$IFDEF DWSCRIPT}
  _800msTimer.OnTime := @On_800msTimer;
  _800msTimer.Delay := 800;
  {$ELSE}
  _800msTimer.OnTimer := {$IFNDEF DELPHI}@{$ENDIF}On_800msTimer;
  _800msTimer.Interval := 800;
  {$ENDIF}

  // Setup the Cdi Stream
  StreamCdi.Size := Int64( Length(CdiXML) + 1);   // Need the null
  i := Low(CdiXML);
  for Counter := 0 to Length(CdiXML) - 1 do       // ios/android compatible
  begin
    StreamWriteByte(StreamCdi, Ord(CdiXML[i]));
    Inc(i);
  end;
  StreamWriteByte(StreamCdi, 0);

  // Setup the Manufacturer Data Stream from the XML to allow access for ACDI and SNIP
  LoadManufacturerDataStream(CdiXML);

  // Setup the Configuration Memory Stream
  StreamConfig.Size := LEN_USER_MANUFACTURER_INFO;
  StreamConfig.Position := 0;
  StreamWriteByte(StreamConfig, USER_MFG_INFO_VERSION_ID);
  while StreamConfig.Position < StreamConfig.Size do
    StreamWriteByte(StreamConfig, 0);

  // Setup the Fdi Stream

  // Setup the Function Configuration Memory Stream
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

procedure TLccNode.CreateNodeID(var Seed: TNodeID);
begin
  Seed[1] := StrToInt('0x020112');
  {$IFDEF DWSCRIPT}
  Seed[0] := RandomInt($FFFFFF);
  {$ELSE}
  Seed[0] := Random($FFFFFF);
  {$ENDIF}
  (NodeManager as INodeManagerCallbacks).DoCreateLccNode(Self);
end;

destructor TLccNode.Destroy;
begin
  FNodeID[0] := 0;
  FNodeID[1] := 0;
  (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);
  (NodeManager as INodeManagerCallbacks).DoDestroyLccNode(Self);
  _800msTimer.Enabled := False;
  _800msTimer.Free;
  FProtocolSupportedProtocols.Free;
  FProtocolSimpleNodeInfo.Free;
  FTProtocolMemoryConfigurationDefinitionInfo.Free;
  FProtocolEventConsumed.Free;
  FProtocolEventsProduced.Free;
  FProtocolMemoryOptions.Free;
  FProtocolMemoryInfo.Free;
  FProtocolTraction.Free;
  FProtocolTractionFunctionDefinitionInfo.Free;
  FProtocolTractionMemoryFunctionConfiguration.Free;
  FProtocolTractionSimpleTrainNodeInfo.Free;
  FACDIMfg.Free;
  FACDIUser.Free;
  FProtocolMemoryConfiguration.Free;
  FDatagramResendQueue.Free;
  FWorkerMessageDatagram.Free;
  FWorkerMessage.Free;
  FStreamCdi.Free;
  FStreamConfig.Free;
  FStreamManufacturerData.Free;
  FStreamTractionConfig.Free;
  FStreamTractionFdi.Free;
  inherited;
end;

function TLccNode.FindCdiElement(TestXML, Element: string; var Offset: Integer; var ALength: Integer): Boolean;
var
  OffsetEnd: Integer;
begin
  Result := False;
  TestXML := LowerCase(TestXML);
  Element := LowerCase(Element);
  Offset := Pos(Element, TestXML);
  if Offset > -1 then
  begin
    Inc(Offset, Length(Element));
    Element := StringReplace(Element, '<', '</', [rfReplaceAll]);
    OffsetEnd := Pos(Element, TestXML);
    if (OffsetEnd > -1) and (OffsetEnd > Offset) then
    begin
      ALength := OffsetEnd - Offset;
      Result := True;
      OffsetEnd := Low(TestXML);  // The "Low" would not work in the following if statement directly in Delphi
      if OffsetEnd = 0 then   // Mobile
        Dec(Offset, 1);
    end else
    Exit;
  end
end;

function TLccNode.GetAlias: Word;
begin
  Result := 0;
end;

procedure TLccNode.Login(ANodeID: TNodeID);
begin
  if NullNodeID(ANodeID) then
    CreateNodeID(ANodeID);
  FNodeID := ANodeID;
  (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);
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
  AddressSpace, OperationType, TractionCode: Byte;
  DoDefault: Boolean;
begin

  // By the time a messages drops into this method it is a fully qualified OpenLCB
  // message.  Any CAN messages that are sent as multi frames have been combined
  // into a full OpenLCB message.

  Result := False;

  TestNodeID[0] := 0;
  TestNodeID[1] := 0;

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

    // *************************************************************************
    // *************************************************************************
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

    // *************************************************************************
    // *************************************************************************
    MTI_SIMPLE_NODE_INFO_REQUEST :
        begin
          WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, ProtocolSimpleNodeInfo.PackedFormat(StreamManufacturerData, StreamConfig));
          SendMessageFunc(WorkerMessage);
          Result := True;
        end;
    MTI_SIMPLE_NODE_INFO_REPLY :
        begin  // Called if I send a SNIP Request and the other node replies
          // TODO need a call back handler
          Result := True;
        end;

    // *************************************************************************
    // *************************************************************************
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

    // *************************************************************************
    // Producer/Consumer tell me what events do you care about (for routers, getting mass
    // results for the state of the layout
    // *************************************************************************
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

    // *************************************************************************
    // General Producer/Consumer Queries
    // *************************************************************************
    MTI_PRODUCER_IDENDIFY :
        begin
          // First see if we have any built in producers we can to reply automatically
          // Note that the expectation is the app is maintaining the state of the event
          // objects in parallel through the TProtocolEventsProduced object (Clear/Set/Unkown)

          // Let the application have a crack
          DoDefault := True;
           (NodeManager as INodeManagerCallbacks).DoProducerIdentify(Self, SourceLccMessage, DoDefault);
          if DoDefault then
          begin
            Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
            SendProducerIdentify(Temp);         // Compatible with Smart Pascal
          end;
          Result := True;
        end;
    MTI_CONSUMER_IDENTIFY :
        begin
          // First see if we have any preregistred consumers that we use that
          // we can to reply automatically, we are not the producers so we
          // don't need to keep the state upto date

          // Let the application have a crack
          DoDefault := True;
           (NodeManager as INodeManagerCallbacks).DoProducerIdentify(Self, SourceLccMessage, DoDefault);
          if DoDefault then
          begin
            Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
            SendConsumerIdentify(Temp);        // Compatible with Smart Pascal
          end;
          Result := True;
        end;

    // *************************************************************************
     // This block of messages is if we sent at "Producer" or "Consumer" Identify
     // and these are the results coming back... I am not sure what "Consumer" Identify
     // needs different states as the replying node is not in control of the state only
     // the "Producer" is in control
     // *************************************************************************
     MTI_CONSUMER_IDENTIFIED_CLEAR :
        begin
          Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceLccMessage, Temp, evs_InValid);
        end;
     MTI_CONSUMER_IDENTIFIED_SET :
        begin
         Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceLccMessage, Temp, evs_Valid);
        end;
     MTI_CONSUMER_IDENTIFIED_UNKNOWN :
        begin
          Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceLccMessage, Temp, evs_Unknown);
        end;
     MTI_PRODUCER_IDENTIFIED_CLEAR :
        begin
          Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceLccMessage, Temp, evs_inValid);
        end;
     MTI_PRODUCER_IDENTIFIED_SET :
        begin
          Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceLccMessage, Temp, evs_Valid);
        end;
     MTI_PRODUCER_IDENTIFIED_UNKNOWN :
        begin
          Temp := SourceLccMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceLccMessage, Temp, evs_Unknown);
        end;

     // *************************************************************************
     // Traction Messages
     // *************************************************************************
     MTI_TRACTION_SIMPLE_TRAIN_INFO_REQUEST :
        begin
          Result := True;
        end;
    MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY :
        begin
          Result := True;
        end;
    MTI_TRACTION_REQUEST :
        begin
          TractionCode := SourceLccMessage.DataArrayIndexer[0];
          case TractionCode of
            TRACTION_SPEED_DIR :
               begin
                // ProtocolTraction.SetSpeedDir(SourceLccMessage);
                 (NodeManager as INodeManagerCallbacks).DoTractionSpeedSet(Self, SourceLccMessage, False);
               end;
             TRACTION_FUNCTION :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionFunctionSet(Self, SourceLccMessage, False);
               end;
             TRACTION_E_STOP :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionEmergencyStop(Self, SourceLccMessage, False);
               end;
             TRACTION_QUERY_SPEED :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionQuerySpeed(Self, SourceLccMessage, False);
               end;
             TRACTION_QUERY_FUNCTION :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionQueryFunction(Self, SourceLccMessage, False);
               end;
             TRACTION_CONTROLLER_CONFIG :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionControllerConfig(Self, SourceLccMessage, False);
               end;
             TRACTION_LISTENER :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionListenerConfig(Self, SourceLccMessage, False);
               end;
             TRACTION_MANAGE :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionManage(Self, SourceLccMessage, False);
               end;
          end;
          Result := True;
        end;
    MTI_TRACTION_REPLY :
        begin
          TractionCode := SourceLccMessage.DataArrayIndexer[0];
          case TractionCode of
            TRACTION_QUERY_SPEED :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionQuerySpeed(Self, SourceLccMessage, True);
               end;
             TRACTION_QUERY_FUNCTION :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionQueryFunction(Self, SourceLccMessage, True);
               end;
             TRACTION_CONTROLLER_CONFIG :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionControllerConfig(Self, SourceLccMessage, True);
               end;
             TRACTION_LISTENER :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionListenerConfig(Self, SourceLccMessage, True);
               end;
             TRACTION_MANAGE :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionManage(Self, SourceLccMessage, True);
               end;
          end;
          Result := True;
        end;

    // *************************************************************************
    // Datagram Messages
    // *************************************************************************
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
           DATAGRAM_PROTOCOL_CONFIGURATION :     {0x20}
             begin
               AddressSpace := 0;

               // Figure out where the Memory space to work on is located, encoded in the header or in the first databyte slot.
               case SourceLccMessage.DataArrayIndexer[1] and $03 of
                 MCP_NONE          : AddressSpace := SourceLccMessage.DataArrayIndexer[6];
                 MCP_CDI           : AddressSpace := MSI_CDI;
                 MCP_ALL           : AddressSpace := MSI_ALL;
                 MCP_CONFIGURATION : AddressSpace := MSI_CONFIG;
               end;

               case SourceLccMessage.DataArrayIndexer[1] and $F0 of
                 MCP_WRITE :
                   begin
                     case AddressSpace of
                       MSI_CDI       : begin end; // Can't write to the CDI
                       MSI_ALL       : begin end; // Can't write to the program area
                       MSI_CONFIG    :            // Needs access to the Configuration Memory Information
                         begin
                           SendDatagramAckReply(SourceLccMessage, False, 0);     // We will be sending a Write Reply
                           ProtocolMemoryConfiguration.DatagramWriteRequest(SourceLccMessage, StreamConfig);
                           Result := True;
                         end;
                       MSI_ACDI_MFG  : begin end; // Can't write to the Manufacturers area
                       MSI_ACDI_USER :            // Needs access to the Configuration Memory Information
                         begin
                           SendDatagramAckReply(SourceLccMessage, False, 0);     // We will be sending a Write Reply
                           ACDIUser.DatagramWriteRequest(SourceLccMessage, StreamConfig);
                           Result := True;
                         end;
                       MSI_TRACTION_FDI       : begin end; // Can't write to the FDI area
                       MSI_TRACTION_FUNCTION_CONFIG :
                         begin
                           SendDatagramAckReply(SourceLccMessage, False, 0);     // We will be sending a Write Reply
                           ProtocolMemoryConfiguration.DatagramWriteRequest(SourceLccMessage, StreamTractionConfig);
                           Result := True;
                         end;
                     end;
                   end;
                 MCP_READ :
                   begin
                     case AddressSpace of
                       MSI_CDI :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias);
                           ProtocolConfigurationDefinitionInfo.DatagramReadRequest(SourceLccMessage, WorkerMessage, StreamCdi);
                           SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_ALL       :
                           begin  // Can't read from the program area
                             SendDatagramAckReply(SourceLccMessage, False, 0);
                           end;
                       MSI_CONFIG :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias);
                           ProtocolMemoryConfiguration.DatagramReadRequest(SourceLccMessage, WorkerMessage, StreamConfig);
                           SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_ACDI_MFG :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias);
                           ACDIMfg.DatagramReadRequest(SourceLccMessage, WorkerMessage, StreamManufacturerData);
                           SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_ACDI_USER :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias);
                           ACDIUser.DatagramReadRequest(SourceLccMessage, WorkerMessage, StreamConfig);
                           SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_TRACTION_FDI :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias);
                           ProtocolConfigurationDefinitionInfo.DatagramReadRequest(SourceLccMessage, WorkerMessage, StreamTractionFdi);
                           SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_TRACTION_FUNCTION_CONFIG :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias);
                           ProtocolMemoryConfiguration.DatagramReadRequest(SourceLccMessage, WorkerMessage, StreamTractionConfig);
                           SendDatagramRequiredReply(SourceLccMessage, WorkerMessage);
                           Result := True;
                         end;
                     end;
                   end;
                 MCP_WRITE_STREAM :
                   begin
                   end;
                 MCP_READ_STREAM :
                   begin
                   end;
                 MCP_OPERATION :
                   begin
                     OperationType := SourceLccMessage.DataArrayIndexer[1];
                     case OperationType of
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
                   end;
               end
             end
         else begin {case else}
             // Unknown Datagram Type
             WorkerMessage.LoadDatagramRejected(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, REJECTED_DATAGRAMS_NOT_ACCEPTED);
             SendMessageFunc(WorkerMessage);
             Result := True;
           end;
         end;  // case
       end;
  else begin
      if SourceLccMessage.HasDestination then
      begin
        WorkerMessage.LoadOptionalInteractionRejected(NodeID, GetAlias, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, REJECTED_BUFFER_FULL, SourceLccMessage.MTI);
        SendMessageFunc(WorkerMessage);
        Result := True;
      end;
    end;
  end; // case
end;

procedure TLccNode.SendDatagramAckReply(SourceLccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  // Only Ack if we accept the datagram
  WorkerMessageDatagram.LoadDatagramAck(SourceLccMessage.DestID, SourceLccMessage.CAN.DestAlias,
                                        SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias,
                                        True, ReplyPending, TimeOutValueN);
  SendMessageFunc(WorkerMessageDatagram);
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
  WorkerMessageDatagram.LoadDatagramRejected(SourceLccMessage.DestID, SourceLccMessage.CAN.DestAlias,
                                             SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias,
                                             Reason);
  SendMessageFunc(WorkerMessageDatagram);
end;

procedure TLccNode.SendDatagramRequiredReply(SourceLccMessage, ReplyLccMessage: TLccMessage);
begin
  if DatagramResendQueue.Add(ReplyLccMessage.Clone) then     // Waiting for an ACK
  begin
    SendDatagramAckReply(SourceLccMessage, False, 0);   // We will be sending a Read Reply
    SendMessageFunc(ReplyLccMessage);
  end else
    SendDatagramRejectedReply(SourceLccMessage, REJECTED_BUFFER_FULL)
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
  (NodeManager as INodeManagerCallbacks).DoInitializationComplete(Self);
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
  {$IFNDEF DWSCRIPT}
  Randomize;
  {$ENDIF}

finalization

end.

