unit lcc_node_manager;

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
  contnrs,
  ExtCtrls,
  lcc_common_classes,
{$ENDIF}
  lcc_node,
  lcc_defines,
  lcc_node_messages,
  lcc_utilities;

type

  TOnLccNodeMessage = procedure(Sender: TObject; LccSourceNode: TLccNode) of object;
  TOnLccNodeMessageWithDest = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode) of object;
  TOnLccNodeEventIdentified = procedure(Sender: TObject; LccSourceNode: TLccNode; var Event: TEventID; State: TEventState) of object;
  TOnLccNodeMessageResultCode = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; ResultCode: Byte) of object;
  TOnLccNodeTractionControllerQuery = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; ActiveControllerNodeID: TNodeID; ActiveControllerAlias: Word) of object;
  TOnLccNodeTractionControllerChangeNotify = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; NewRequestingNode: TNodeID; NewRequestingNodeAlias: Word; var Allow: Boolean) of object;
  TOnLccNodeConfigMem = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode) of object;
  TOnLccNodeConfigMemAddressSpace = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; AddressSpace: Byte) of object;

  { TLccNodeManager }

  TLccNodeManager = class(TComponent)
  private
    {$IFNDEF DWSCRIPT}
    FCdiParser: TLccCdiParserBase;
    FHardwareConnection: TLccHardwareConnectionManager;
    {$ENDIF}
    FOnAliasIDChanged: TOnLccNodeMessage;
    FOnLccMessageReceive: TOnMessageEvent;
    FOnLccNodeConfigMemAddressSpaceInfoReply: TOnLccNodeConfigMemAddressSpace;
    FOnLccNodeConfigMemOptionsReply: TOnLccNodeConfigMem;
    FOnNodeIDChanged: TOnLccNodeMessage;
    FOnLccCANAliasMapReset: TOnLccNodeMessage;
    FOnLccNodeCDI: TOnLccNodeMessageWithDest;
    FOnLccNodeConfigMemReadReply: TOnLccNodeConfigMem;
    FOnLccNodeConfigMemWriteReply: TOnLccNodeConfigMem;
    FOnLccNodeConsumerIdentified: TOnLccNodeEventIdentified;
    FOnLccNodeCreate: TOnLccNodeMessage;
    FOnLccNodeDatagramReply: TOnLccNodeMessageWithDest;
    FOnLccNodeDestroy: TOnLccNodeMessage;
    FOnLccNodeFDI: TOnLccNodeMessageWithDest;
    FOnLccNodeFunctionConfiguration: TOnLccNodeMessageWithDest;
    FOnLccNodeInitializationComplete: TOnLccNodeMessage;
    FOnLccNodeOptionalInteractionRejected: TOnLccNodeMessageWithDest;
    FOnLccNodeProducerIdentified: TOnLccNodeEventIdentified;
    FOnLccNodeProtocolIdentifyReply: TOnLccNodeMessageWithDest;
    FOnLccNodeRemoteButtonReply: TOnLccNodeMessageWithDest;
    FOnLccNodeSimpleNodeIdentReply: TOnLccNodeMessageWithDest;
    FOnLccNodeSimpleTrainNodeIdentReply: TOnLccNodeMessageWithDest;
    FOnLccNodeTractionControllerChangeNotify: TOnLccNodeTractionControllerChangeNotify;
    FOnLccNodeTractionReplyControllerAssign: TOnLccNodeMessageResultCode;
    FOnLccNodeTractionReplyControllerChangeNotify: TOnLccNodeMessageResultCode;
    FOnLccNodeTractionReplyControllerQuery: TOnLccNodeTractionControllerQuery;
    FOnLccNodeTractionReplyManage: TOnLccNodeMessageResultCode;
    FOnLccNodeTractionReplyQueryFunction: TOnLccNodeMessageWithDest;
    FOnLccNodeTractionReplyQuerySpeed: TOnLccNodeMessageWithDest;
    FOnLccNodeVerifiedNodeID: TOnLccNodeMessage;
    FOnLccMessageSend: TOnMessageEvent;

    FNodes: TObjectList;
  protected
    procedure DoAliasIDChanged(LccNode: TLccNode); virtual;
    procedure DoCANAliasMapReset(LccNode: TLccNode); virtual;
    procedure DoCDI(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoConfigMemAddressSpaceInfoReply(SourceLccNode, DesTLccNode: TLccNode; AddressSpace: Byte); virtual;
    procedure DoConfigMemOptionsReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoConfigMemReadReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoConfigMemWriteReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoCreateLccNode(SourceLccNode: TLccNode); virtual;
    procedure DoConsumerIdentified(SourceLccNode: TLccNode; var Event: TEventID; State: TEventState); virtual;
    procedure DoDatagramReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoDestroyLccNode(LccNode: TLccNode); virtual;
    procedure DoFDI(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoFunctionConfiguration(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoInitializationComplete(SourceLccNode: TLccNode); virtual;
    procedure DoNodeIDChanged(LccNode: TLccNode); virtual;
    procedure DoOptionalInteractionRejected(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoProducerIdentified(SourceLccNode: TLccNode; var Event: TEventID; State: TEventState); virtual;
    procedure DoProtocolIdentifyReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoRemoteButtonReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoSimpleNodeIdentReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoSimpleTrainNodeIdentReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoTractionControllerChangeNotify(SourceLccNode, DesTLccNode: TLccNode; NewRequestingNode: TNodeID; NewRequestingNodeAlias: Word; var Allow: Boolean); virtual;
    procedure DoTractionReplyQuerySpeed(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoTractionReplyQueryFunction(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoTractionReplyControllerAssign(SourceLccNode, DesTLccNode: TLccNode; ResultCode: Byte); virtual;
    procedure DoTractionReplyControllerQuery(SourceLccNode, DesTLccNode: TLccNode; ActiveControllerNodeID: TNodeID; ActiveControllerAlias: Word); virtual;
    procedure DoTractionReplyControllerChangeNotify(SourceLccNode, DesTLccNode: TLccNode; ResultCode: Byte); virtual;
    procedure DoTractionReplyManage(SourceLccNode, DesTLccNode: TLccNode; ResultCode: Byte); virtual;
    procedure DoVerifiedNodeID(SourceLccNode: TLccNode); virtual;

    procedure DoLccMessageSend(Message: TLccMessage); virtual;
    procedure DoLccMessageReceive(Message: TLccMessage); virtual;

  public
    property Nodes: TOBjectList read FNodes write FNodes;

    constructor Create(AnOwner: TComponent); {$IFNDEF DWSCRIPT} override;  {$ENDIF}
    destructor Destroy; override;

    procedure Clear;
    function AddNode: TLccNode; virtual;
    function FindOwnedNodeByDestID(LccMessage: TLccMessage): TLccNode;
    function FindOwnedNodeBySourceID(LccMessage: TLccMessage): TLccNode;
    procedure LogoutAll;
    procedure ProcessMessage(LccMessage: TLccMessage);

    procedure LccMessageReceive(LccMessage: TLccMessage);
    procedure LccMessageSend(LccMessage: TLccMessage);

  published
    {$IFNDEF DWSCRIPT}
    property CdiParser: TLccCdiParserBase read FCdiParser write FCdiParser;
    property HardwareConnection: TLccHardwareConnectionManager read FHardwareConnection write FHardwareConnection;
    {$ENDIF}
    property OnAliasIDChanged: TOnLccNodeMessage read FOnAliasIDChanged write FOnAliasIDChanged;
    property OnLccCANAliasMapReset: TOnLccNodeMessage read FOnLccCANAliasMapReset write FOnLccCANAliasMapReset;
    property OnLccNodeCDI: TOnLccNodeMessageWithDest read FOnLccNodeCDI write FOnLccNodeCDI;
    property OnLccNodeConfigMemAddressSpaceInfoReply: TOnLccNodeConfigMemAddressSpace read FOnLccNodeConfigMemAddressSpaceInfoReply write FOnLccNodeConfigMemAddressSpaceInfoReply;
    property OnLccNodeConfigMemOptionsReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemOptionsReply write FOnLccNodeConfigMemOptionsReply;
    property OnLccNodeConfigMemReadReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemReadReply write FOnLccNodeConfigMemReadReply;
    property OnLccNodeConfigMemWriteReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemWriteReply write FOnLccNodeConfigMemWriteReply;
    property OnLccNodeConsumerIdentified: TOnLccNodeEventIdentified read FOnLccNodeConsumerIdentified write FOnLccNodeConsumerIdentified;
    property OnLccNodeCreate: TOnLccNodeMessage read FOnLccNodeCreate write FOnLccNodeCreate;
    property OnLccNodeDatagramReply: TOnLccNodeMessageWithDest read FOnLccNodeDatagramReply write FOnLccNodeDatagramReply;
    property OnLccNodeDestroy: TOnLccNodeMessage read FOnLccNodeDestroy write FOnLccNodeDestroy;
    property OnLccNodeFDI: TOnLccNodeMessageWithDest read FOnLccNodeFDI write FOnLccNodeFDI;
    property OnLccNodeFunctionConfiguration: TOnLccNodeMessageWithDest read FOnLccNodeFunctionConfiguration write FOnLccNodeFunctionConfiguration;
    property OnNodeIDChanged: TOnLccNodeMessage read FOnNodeIDChanged write FOnNodeIDChanged;
    property OnLccNodeInitializationComplete: TOnLccNodeMessage read FOnLccNodeInitializationComplete write FOnLccNodeInitializationComplete;
    property OnLccNodeOptionalInteractionRejected: TOnLccNodeMessageWithDest read FOnLccNodeOptionalInteractionRejected write FOnLccNodeOptionalInteractionRejected;
    property OnLccNodeProducerIdentified: TOnLccNodeEventIdentified read FOnLccNodeProducerIdentified write FOnLccNodeProducerIdentified;
    property OnLccNodeProtocolIdentifyReply: TOnLccNodeMessageWithDest read FOnLccNodeProtocolIdentifyReply write FOnLccNodeProtocolIdentifyReply;
    property OnLccNodeRemoteButtonReply: TOnLccNodeMessageWithDest read FOnLccNodeRemoteButtonReply write FOnLccNodeRemoteButtonReply;
    property OnLccNodeSimpleNodeIdentReply: TOnLccNodeMessageWithDest read FOnLccNodeSimpleNodeIdentReply write FOnLccNodeSimpleNodeIdentReply;
    property OnLccNodeSimpleTrainNodeIdentReply: TOnLccNodeMessageWithDest read FOnLccNodeSimpleTrainNodeIdentReply write FOnLccNodeSimpleTrainNodeIdentReply;
    property OnLccNodeTractionControllerChangeNotify: TOnLccNodeTractionControllerChangeNotify read FOnLccNodeTractionControllerChangeNotify write FOnLccNodeTractionControllerChangeNotify;
    property OnLccNodeTractionReplyQuerySpeed: TOnLccNodeMessageWithDest read FOnLccNodeTractionReplyQuerySpeed write FOnLccNodeTractionReplyQuerySpeed;
    property OnLccNodeTractionReplyQueryFunction: TOnLccNodeMessageWithDest read FOnLccNodeTractionReplyQueryFunction write FOnLccNodeTractionReplyQueryFunction;
    property OnLccNodeTractionReplyControllerAssign: TOnLccNodeMessageResultCode read FOnLccNodeTractionReplyControllerAssign write FOnLccNodeTractionReplyControllerAssign;
    property OnLccNodeTractionReplyControllerQuery: TOnLccNodeTractionControllerQuery read FOnLccNodeTractionReplyControllerQuery write FOnLccNodeTractionReplyControllerQuery;
    property OnLccNodeTractionReplyControllerChangeNotify: TOnLccNodeMessageResultCode read FOnLccNodeTractionReplyControllerChangeNotify write FOnLccNodeTractionReplyControllerChangeNotify;
    property OnLccNodeTractionReplyManage: TOnLccNodeMessageResultCode read FOnLccNodeTractionReplyManage write FOnLccNodeTractionReplyManage;
    property OnLccNodeVerifiedNodeID: TOnLccNodeMessage read FOnLccNodeVerifiedNodeID write FOnLccNodeVerifiedNodeID;

    property OnLccMessageReceive: TOnMessageEvent read FOnLccMessageReceive write FOnLccMessageReceive;
    property OnLccMessageSend: TOnMessageEvent read FOnLccMessageSend write FOnLccMessageSend;
  end;


  { TLccCanNodeManager }

  TLccCanNodeManager = class(TLccNodeManager)
  public
    constructor Create(AnOwner: TComponent); {$IFNDEF DWSCRIPT} override;  {$ENDIF}
    destructor Destroy; override;

    function AddNode: TLccCanNode; override;
  end;

implementation

{ TLccCanNodeManager }

function TLccCanNodeManager.AddNode: TLccCanNode;
begin
  Result := TLccCanNode.Create(@DoLccMessageSend);
  Nodes.Add(Result);
end;

constructor TLccCanNodeManager.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
end;

destructor TLccCanNodeManager.Destroy;
begin
  inherited;
end;




{ TLccNodeManager }

procedure TLccNodeManager.DoAliasIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnAliasIDChanged) then
    OnAliasIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoCANAliasMapReset(LccNode: TLccNode);
begin
   if Assigned(FOnLccCANAliasMapReset) then
     FOnLccCANAliasMapReset(Self, LccNode);
end;

procedure TLccNodeManager.DoCDI(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeCDI) then
    OnLccNodeCDI(Self, SourceLccNode, DesTLccNode)
end;

procedure TLccNodeManager.DoConfigMemAddressSpaceInfoReply(SourceLccNode,
  DesTLccNode: TLccNode; AddressSpace: Byte);
begin
 if Assigned(OnLccNodeConfigMemAddressSpaceInfoReply) then
   OnLccNodeConfigMemAddressSpaceInfoReply(Self, SourceLccNode, DesTLccNode, AddressSpace);
end;

procedure TLccNodeManager.DoConfigMemOptionsReply(SourceLccNode,
  DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemOptionsReply) then
    OnLccNodeConfigMemOptionsReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoConfigMemReadReply(SourceLccNode,
  DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemReadReply) then
    OnLccNodeConfigMemReadReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoConfigMemWriteReply(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemWriteReply) then
    OnLccNodeConfigMemWriteReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoCreateLccNode(SourceLccNode: TLccNode);
begin
  if Assigned(OnLccNodeCreate) then
    OnLccNodeCreate(Self, SourceLccNode)
end;

procedure TLccNodeManager.DoConsumerIdentified(SourceLccNode: TLccNode; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeConsumerIdentified) then
    OnLccNodeConsumerIdentified(Self, SourceLccNode, Event, State);
end;

procedure TLccNodeManager.DoDatagramReply(SourceLccNode,
  DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeDatagramReply) then
    OnLccNodeDatagramReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoDestroyLccNode(LccNode: TLccNode);
begin
  {$IFNDEF DWSCRIPT}
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(CdiParser) then
      CdiParser.NotifyLccNodeDestroy(LccNode);
  end;
  {$ENDIF}
  if Assigned(OnLccNodeDestroy) then
    OnLccNodeDestroy(Self, LccNode);
end;

procedure TLccNodeManager.DoFDI(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeFDI) then
    OnLccNodeFDI(Self, SourceLccNode, DesTLccNode)
end;

procedure TLccNodeManager.DoFunctionConfiguration(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeFunctionConfiguration) then
    OnLccNodeFunctionConfiguration(Self, SourceLccNode, DesTLccNode)
end;

procedure TLccNodeManager.DoInitializationComplete(SourceLccNode: TLccNode);
begin
  if Assigned(OnLccNodeInitializationComplete) then
    OnLccNodeInitializationComplete(Self, SourceLccNode);
end;

procedure TLccNodeManager.DoLccMessageReceive(Message: TLccMessage);
begin
  {$IFNDEF DWSCRIPT}
  if Assigned(HardwareConnection) then
    HardwareConnection.SendMessage(Message);
  {$ENDIF}
  if Assigned(OnLccMessageReceive) then
    OnLccMessageReceive(Self, Message);
end;

procedure TLccNodeManager.DoLccMessageSend(Message: TLccMessage);
begin
  {$IFNDEF DWSCRIPT}
  if Assigned(HardwareConnection) then
    HardwareConnection.SendMessage(Message);
  {$ENDIF}
  if Assigned(OnLccMessageSend) then
    OnLccMessageSend(Self, Message);
end;

procedure TLccNodeManager.DoNodeIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnNodeIDChanged) then
    OnNodeIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoOptionalInteractionRejected(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeOptionalInteractionRejected) then
    OnLccNodeOptionalInteractionRejected(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoProducerIdentified(SourceLccNode: TLccNode; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeProducerIdentified) then
    OnLccNodeProducerIdentified(Self, SourceLccNode, Event, State);
end;

procedure TLccNodeManager.DoProtocolIdentifyReply(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeProtocolIdentifyReply) then
    OnLccNodeProtocolIdentifyReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoRemoteButtonReply(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeRemoteButtonReply) then
    OnLccNodeRemoteButtonReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoSimpleNodeIdentReply(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeSimpleNodeIdentReply) then
    OnLccNodeSimpleNodeIdentReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoSimpleTrainNodeIdentReply(SourceLccNode,
  DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeSimpleTrainNodeIdentReply) then
    OnLccNodeSimpleTrainNodeIdentReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoTractionControllerChangeNotify(SourceLccNode,
  DesTLccNode: TLccNode; NewRequestingNode: TNodeID;
  NewRequestingNodeAlias: Word; var Allow: Boolean);
begin
  if Assigned(OnLccNodeTractionControllerChangeNotify) then
    OnLccNodeTractionControllerChangeNotify(Self, SourceLccNode, DesTLccNode, NewRequestingNode, NewRequestingNodeAlias, Allow);
end;

procedure TLccNodeManager.DoTractionReplyQuerySpeed(SourceLccNode,
  DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeTractionReplyQuerySpeed) then
    OnLccNodeTractionReplyQuerySpeed(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoTractionReplyQueryFunction(SourceLccNode,
  DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeTractionReplyQueryFunction) then
    OnLccNodeTractionReplyQueryFunction(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoTractionReplyControllerAssign(SourceLccNode,
  DesTLccNode: TLccNode; ResultCode: Byte);
begin
  if Assigned(OnLccNodeTractionReplyControllerAssign) then
    OnLccNodeTractionReplyControllerAssign(Self, SourceLccNode, DesTLccNode, ResultCode);
end;

procedure TLccNodeManager.DoTractionReplyControllerQuery(SourceLccNode,
  DesTLccNode: TLccNode; ActiveControllerNodeID: TNodeID;
  ActiveControllerAlias: Word);
begin
  if Assigned(OnLccNodeTractionReplyControllerQuery) then
    OnLccNodeTractionReplyControllerQuery(Self, SourceLccNode, DesTLccNode, ActiveControllerNodeID, ActiveControllerAlias);
end;

procedure TLccNodeManager.DoTractionReplyControllerChangeNotify(
  SourceLccNode, DesTLccNode: TLccNode; ResultCode: Byte);
begin
  if Assigned(OnLccNodeTractionReplyControllerChangeNotify) then
    OnLccNodeTractionReplyControllerChangeNotify(Self, SourceLccNode, DesTLccNode, ResultCode);
end;

procedure TLccNodeManager.DoTractionReplyManage(SourceLccNode,
  DesTLccNode: TLccNode; ResultCode: Byte);
begin
  if Assigned(OnLccNodeTractionReplyManage) then
    OnLccNodeTractionReplyManage(Self, SourceLccNode, DesTLccNode, ResultCode);
end;

procedure TLccNodeManager.DoVerifiedNodeID(SourceLccNode: TLccNode);
begin
  if Assigned(OnLccNodeVerifiedNodeID) then
    OnLccNodeVerifiedNodeID(Self, SourceLccNode);
end;

constructor TLccNodeManager.Create(AnOwner: TComponent);
begin
  {$IFDEF DWSCRIPT}
  inherited Create;
  FNodes := TObjectList.Create;
  {$ELSE}
  inherited Create(AnOwner);
  FNodes := TObjectList.Create;
  FNodes.OwnsObjects := False;
  {$ENDIF}
end;

function TLccNodeManager.AddNode: TLccNode;
begin
  Result := TLccNode.Create(@DoLccMessageSend);
  Nodes.Add(Result);
end;

destructor TLccNodeManager.Destroy;
begin
  LogoutAll;
  Clear;
  FNodes.Free;
  inherited Destroy;
end;

procedure TLccNodeManager.Clear;
var
  i: Integer;
begin
  try
    LogoutAll;
    for i := 0 to FNodes.Count - 1 do
      TObject( FNodes[i]).Free;
  finally
    Nodes.Clear;
  end;
end;

function TLccNodeManager.FindOwnedNodeByDestID(LccMessage: TLccMessage): TLccNode;
var
  i: Integer;
begin
  Result := nil;
  i := 0;     // Cheap, slow linear search for now
  while i < Nodes.Count do
  begin
    if TLccNode(Nodes[i]).IsNode(LccMessage, ntt_Dest) then
    begin
      Result := TLccNode(Nodes[i]);
      Break;
    end;
    Inc(i)
  end;
end;

function TLccNodeManager.FindOwnedNodeBySourceID(LccMessage: TLccMessage): TLccNode;
var
  i: Integer;
begin
  Result := nil;
  i := 0;     // Cheap, slow linear search for now
  while i < Nodes.Count do
  begin
    if TLccNode(Nodes[i]).IsNode(LccMessage, ntt_Source) then
    begin
      Result := TLccNode(Nodes[i]);
      Break;
    end;
    Inc(i)
  end;
end;

procedure TLccNodeManager.LccMessageReceive(LccMessage: TLccMessage);
begin
  DoLccMessageReceive(LccMessage);
end;

procedure TLccNodeManager.LogoutAll;
var
  i: Integer;
begin
  for i := 0 to Nodes.Count - 1 do
    TLccNode( Nodes[i]).Logout;
end;

procedure TLccNodeManager.ProcessMessage(LccMessage: TLccMessage);
var
  i: Integer;
begin
  for i := 0 to Nodes.Count - 1 do
    TLccNode( Nodes[i]).ProcessMessage(LccMessage);
  DoLccMessageReceive(LccMessage);
end;

procedure TLccNodeManager.LccMessageSend(LccMessage: TLccMessage);
begin
  DoLccMessageSend(LccMessage);
end;



initialization
//  RegisterClass(TLccNodeManager);

finalization

end.

