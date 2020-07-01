unit lcc_node_manager;

{$IFNDEF DWSCRIPT}
{$I lcc_compilers.inc}
{$ENDIF}

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
  {$IFDEF DELPHI}
  System.Generics.Collections,
  {$ELSE}
  contnrs,
  {$ENDIF}
  {$IFNDEF ULTIBO}
    {$IFDEF FPC}
      ExtCtrls,
    {$ELSE}
      FMX.Types,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  lcc_node,
  lcc_defines,
  lcc_node_messages;

type
  INodeManagerCallbacks = interface
    ['{C6920bCA-08BC-4D45-B27C-174640FA3106}']
    procedure DoAliasIDChanged(LccNode: TLccNode);               //*
    procedure DoCANAliasMapReset(LccNode: TLccNode);             //*
    procedure DoCDIRead(SourceLccNode, DestLccNode: TLccNode);
    procedure DoConfigMemAddressSpaceInfoReply(SourceLccNode, DesTLccNode: TLccNode; AddressSpace: Byte);
    procedure DoConfigMemOptionsReply(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoConfigMemReadReply(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoConfigMemWriteReply(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoCreateLccNode(SourceLccNode: TLccNode);     //*
    procedure DoConsumerIdentify(SourceLccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoConsumerIdentified(SourceLccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
    procedure DoDatagramReply(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoDestroyLccNode(LccNode: TLccNode);   //*
    procedure DoFDI(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoFunctionConfiguration(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoInitializationComplete(SourceLccNode: TLccNode);   //*
    procedure DoNodeIDChanged(LccNode: TLccNode);                  //*
    procedure DoOptionalInteractionRejected(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoProducerIdentify(SourceLccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoProducerIdentified(SourceLccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
    procedure DoProtocolIdentifyReply(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoRemoteButtonReply(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoSimpleNodeIdentReply(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoSimpleTrainNodeIdentReply(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoTractionControllerChangeNotify(SourceLccNode, DesTLccNode: TLccNode; NewRequestingNode: TNodeID; NewRequestingNodeAlias: Word; var Allow: Boolean);
    procedure DoTractionReplyQuerySpeed(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoTractionReplyQueryFunction(SourceLccNode, DesTLccNode: TLccNode);
    procedure DoTractionReplyControllerAssign(SourceLccNode, DesTLccNode: TLccNode; ResultCode: Byte);
    procedure DoTractionReplyControllerQuery(SourceLccNode, DesTLccNode: TLccNode; ActiveControllerNodeID: TNodeID; ActiveControllerAlias: Word);
    procedure DoTractionReplyControllerChangeNotify(SourceLccNode, DesTLccNode: TLccNode; ResultCode: Byte);
    procedure DoTractionReplyManage(SourceLccNode, DesTLccNode: TLccNode; ResultCode: Byte);
    procedure DoVerifiedNodeID(SourceLccNode: TLccNode);
  end;

type

  TOnLccNodeMessage = procedure(Sender: TObject; LccSourceNode: TLccNode) of object;
  TOnLccNodeMessageWithDest = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode) of object;
  TOnLccNodeEventIdentify = procedure(Sender: TObject; LccSourceNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean) of object;
  TOnLccNodeEventIdentified = procedure(Sender: TObject; LccSourceNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState) of object;
  TOnLccNodeMessageResultCode = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; ResultCode: Byte) of object;
  TOnLccNodeTractionControllerQuery = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; ActiveControllerNodeID: TNodeID; ActiveControllerAlias: Word) of object;
  TOnLccNodeTractionControllerChangeNotify = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; NewRequestingNode: TNodeID; NewRequestingNodeAlias: Word; var Allow: Boolean) of object;
  TOnLccNodeConfigMem = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode) of object;
  TOnLccNodeConfigMemAddressSpace = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; AddressSpace: Byte) of object;

  { TLccNodeManager }

  TLccNodeManager = class(TComponent, INodeManagerCallbacks)
  private
    FOnLccNodeAliasIDChanged: TOnLccNodeMessage;
    FOnLccMessageReceive: TOnMessageEvent;
    FOnLccNodeConfigMemAddressSpaceInfoReply: TOnLccNodeConfigMemAddressSpace;
    FOnLccNodeConfigMemOptionsReply: TOnLccNodeConfigMem;
    FOnLccNodeConsumerIdentify: TOnLccNodeEventIdentify;
    FOnLccNodeIDChanged: TOnLccNodeMessage;
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
    FOnLccNodeProducerIdentify: TOnLccNodeEventIdentify;
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

    {$IFDEF DELPHI}
    FNodes: TObjectList<TLccNode>;
    {$ELSE}
    FNodes: TObjectList;
    {$ENDIF}
    function GetNode(Index: Integer): TLccNode;
  protected
    procedure DoAliasIDChanged(LccNode: TLccNode); virtual;               //*
    procedure DoCANAliasMapReset(LccNode: TLccNode); virtual;             //*
    procedure DoCDIRead(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoConfigMemAddressSpaceInfoReply(SourceLccNode, DesTLccNode: TLccNode; AddressSpace: Byte); virtual;
    procedure DoConfigMemOptionsReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoConfigMemReadReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoConfigMemWriteReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoCreateLccNode(SourceLccNode: TLccNode); virtual;     //*
    procedure DoConsumerIdentify(SourceLccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoConsumerIdentified(SourceLccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState); virtual;
    procedure DoDatagramReply(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoDestroyLccNode(LccNode: TLccNode); virtual;   //*
    procedure DoFDI(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoFunctionConfiguration(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoInitializationComplete(SourceLccNode: TLccNode); virtual;   //*
    procedure DoNodeIDChanged(LccNode: TLccNode); virtual;                  //*
    procedure DoOptionalInteractionRejected(SourceLccNode, DesTLccNode: TLccNode); virtual;
    procedure DoProducerIdentify(SourceLccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoProducerIdentified(SourceLccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState); virtual;
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

    procedure LccMessageSendCallback(LccMessage: TLccMessage); //  The Callback function for all Nodes use to reply to message they can automaticallly

  public
    {$IFDEF DELPHI}
    property Nodes: TOBjectList<TLccNode> read FNodes write FNodes;
    {$ELSE}
    property Nodes: TOBjectList read FNodes write FNodes;
    {$ENDIF}
    property Node[Index: Integer]: TLccNode read GetNode;

    constructor Create(AnOwner: TComponent); {$IFNDEF DWSCRIPT} override;  {$ENDIF}
    destructor Destroy; override;

    procedure Clear;
    function AddNode(CdiXML: string): TLccNode; virtual;
    procedure LogoutAll;

    function FindOwnedNodeByDestID(LccMessage: TLccMessage): TLccNode;
    function FindOwnedNodeBySourceID(LccMessage: TLccMessage): TLccNode;

    procedure ProcessMessage(LccMessage: TLccMessage);  // Takes incoming messages and dispatches them to the nodes
    procedure SendMessage(LccMessage: TLccMessage);

  published
    // Node Management
    property OnLccNodeCreate: TOnLccNodeMessage read FOnLccNodeCreate write FOnLccNodeCreate;
    property OnLccNodeDestroy: TOnLccNodeMessage read FOnLccNodeDestroy write FOnLccNodeDestroy;
    property OnLccNodeIDChanged: TOnLccNodeMessage read FOnLccNodeIDChanged write FOnLccNodeIDChanged;
    property OnLccNodeInitializationComplete: TOnLccNodeMessage read FOnLccNodeInitializationComplete write FOnLccNodeInitializationComplete;
    property OnLccNodeVerifiedNodeID: TOnLccNodeMessage read FOnLccNodeVerifiedNodeID write FOnLccNodeVerifiedNodeID;
    property OnLccNodeProtocolIdentifyReply: TOnLccNodeMessageWithDest read FOnLccNodeProtocolIdentifyReply write FOnLccNodeProtocolIdentifyReply;

    // CAN Node Management
    property OnLccNodeAliasIDChanged: TOnLccNodeMessage read FOnLccNodeAliasIDChanged write FOnLccNodeAliasIDChanged;
    property OnLccCANAliasMapReset: TOnLccNodeMessage read FOnLccCANAliasMapReset write FOnLccCANAliasMapReset;

    // Configuration Memory Information
    property OnLccNodeConfigMemAddressSpaceInfoReply: TOnLccNodeConfigMemAddressSpace read FOnLccNodeConfigMemAddressSpaceInfoReply write FOnLccNodeConfigMemAddressSpaceInfoReply;
    property OnLccNodeConfigMemOptionsReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemOptionsReply write FOnLccNodeConfigMemOptionsReply;

    // Configuration Memory Access
    property OnLccNodeCDI: TOnLccNodeMessageWithDest read FOnLccNodeCDI write FOnLccNodeCDI;
    property OnLccNodeConfigMemReadReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemReadReply write FOnLccNodeConfigMemReadReply;
    property OnLccNodeConfigMemWriteReply: TOnLccNodeConfigMem read FOnLccNodeConfigMemWriteReply write FOnLccNodeConfigMemWriteReply;

    // Events
    property OnLccNodeConsumerIdentify: TOnLccNodeEventIdentify read FOnLccNodeConsumerIdentify write FOnLccNodeConsumerIdentify;
    property OnLccNodeConsumerIdentified: TOnLccNodeEventIdentified read FOnLccNodeConsumerIdentified write FOnLccNodeConsumerIdentified;
    property OnLccNodeProducerIdentify: TOnLccNodeEventIdentify read FOnLccNodeProducerIdentify write FOnLccNodeProducerIdentify;
    property OnLccNodeProducerIdentified: TOnLccNodeEventIdentified read FOnLccNodeProducerIdentified write FOnLccNodeProducerIdentified;

    // SNIP
    property OnLccNodeSimpleNodeIdentReply: TOnLccNodeMessageWithDest read FOnLccNodeSimpleNodeIdentReply write FOnLccNodeSimpleNodeIdentReply;
    property OnLccNodeSimpleTrainNodeIdentReply: TOnLccNodeMessageWithDest read FOnLccNodeSimpleTrainNodeIdentReply write FOnLccNodeSimpleTrainNodeIdentReply;

    // Datagrams
    property OnLccNodeDatagramReply: TOnLccNodeMessageWithDest read FOnLccNodeDatagramReply write FOnLccNodeDatagramReply;

    // Traction
    property OnLccNodeTractionControllerChangeNotify: TOnLccNodeTractionControllerChangeNotify read FOnLccNodeTractionControllerChangeNotify write FOnLccNodeTractionControllerChangeNotify;
    property OnLccNodeTractionReplyQuerySpeed: TOnLccNodeMessageWithDest read FOnLccNodeTractionReplyQuerySpeed write FOnLccNodeTractionReplyQuerySpeed;
    property OnLccNodeTractionReplyQueryFunction: TOnLccNodeMessageWithDest read FOnLccNodeTractionReplyQueryFunction write FOnLccNodeTractionReplyQueryFunction;
    property OnLccNodeTractionReplyControllerAssign: TOnLccNodeMessageResultCode read FOnLccNodeTractionReplyControllerAssign write FOnLccNodeTractionReplyControllerAssign;
    property OnLccNodeTractionReplyControllerQuery: TOnLccNodeTractionControllerQuery read FOnLccNodeTractionReplyControllerQuery write FOnLccNodeTractionReplyControllerQuery;
    property OnLccNodeTractionReplyControllerChangeNotify: TOnLccNodeMessageResultCode read FOnLccNodeTractionReplyControllerChangeNotify write FOnLccNodeTractionReplyControllerChangeNotify;
    property OnLccNodeTractionReplyManage: TOnLccNodeMessageResultCode read FOnLccNodeTractionReplyManage write FOnLccNodeTractionReplyManage;

    // Traction DCC Functions
    property OnLccNodeFDI: TOnLccNodeMessageWithDest read FOnLccNodeFDI write FOnLccNodeFDI;
    property OnLccNodeFunctionConfiguration: TOnLccNodeMessageWithDest read FOnLccNodeFunctionConfiguration write FOnLccNodeFunctionConfiguration;


    // Other stuff that may not be useful
    property OnLccNodeOptionalInteractionRejected: TOnLccNodeMessageWithDest read FOnLccNodeOptionalInteractionRejected write FOnLccNodeOptionalInteractionRejected;
    property OnLccNodeRemoteButtonReply: TOnLccNodeMessageWithDest read FOnLccNodeRemoteButtonReply write FOnLccNodeRemoteButtonReply;

    // Message Management
    property OnLccMessageReceive: TOnMessageEvent read FOnLccMessageReceive write FOnLccMessageReceive;
    property OnLccMessageSend: TOnMessageEvent read FOnLccMessageSend write FOnLccMessageSend;
  end;


  { TLccCanNodeManager }

  TLccCanNodeManager = class(TLccNodeManager)
  private
    function GetCanNode(Index: Integer): TLccCanNode;
  public
    property CanNode[Index: Integer]: TLccCanNode read GetCanNode;

    constructor Create(AnOwner: TComponent); {$IFNDEF DWSCRIPT} override;  {$ENDIF}
    destructor Destroy; override;

    function AddNode(CdiXML: string): TLccCanNode; reintroduce;
  end;

implementation

{ TLccCanNodeManager }

function TLccCanNodeManager.AddNode(CdiXML: string): TLccCanNode;
begin
  Result := TLccCanNode.Create({$IFDEF FPC}@{$ENDIF}LccMessageSendCallback, Self, CdiXML);
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

function TLccCanNodeManager.GetCanNode(Index: Integer): TLccCanNode;
begin
  if Index < Nodes.Count then
    Result := Nodes[Index] as TLccCanNode
  else
    Result := nil;
end;

{ TLccNodeManager }

procedure TLccNodeManager.DoAliasIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeAliasIDChanged) then
    OnLccNodeAliasIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoCANAliasMapReset(LccNode: TLccNode);
begin
   if Assigned(FOnLccCANAliasMapReset) then
     FOnLccCANAliasMapReset(Self, LccNode);
end;

procedure TLccNodeManager.DoCDIRead(SourceLccNode, DestLccNode: TLccNode);
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

procedure TLccNodeManager.DoConsumerIdentified(SourceLccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeConsumerIdentified) then
    OnLccNodeConsumerIdentified(Self, SourceLccNode, LccMessage, Event, State);
end;

procedure TLccNodeManager.DoConsumerIdentify(SourceLccNode: TLccNode;
  LccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnLccNodeConsumerIdentify) then
    OnLccNodeConsumerIdentify(Self, SourceLccNode, LccMessage, DoDefault);
end;

procedure TLccNodeManager.DoDatagramReply(SourceLccNode,
  DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeDatagramReply) then
    OnLccNodeDatagramReply(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoDestroyLccNode(LccNode: TLccNode);
begin
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
  if Assigned(OnLccMessageReceive) then
    OnLccMessageReceive(Self, Message);
end;

procedure TLccNodeManager.DoLccMessageSend(Message: TLccMessage);
begin
  if Assigned(OnLccMessageSend) then
    OnLccMessageSend(Self, Message);
end;

procedure TLccNodeManager.DoNodeIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeIDChanged) then
    OnLccNodeIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoOptionalInteractionRejected(SourceLccNode, DesTLccNode: TLccNode);
begin
  if Assigned(OnLccNodeOptionalInteractionRejected) then
    OnLccNodeOptionalInteractionRejected(Self, SourceLccNode, DesTLccNode);
end;

procedure TLccNodeManager.DoProducerIdentified(SourceLccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeProducerIdentified) then
    OnLccNodeProducerIdentified(Self, SourceLccNode, LccMessage, Event, State);
end;

procedure TLccNodeManager.DoProducerIdentify(SourceLccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnLccNodeProducerIdentify) then
    OnLccNodeProducerIdentify(Self, SourceLccNode, LccMessage, DoDefault);
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
  {$IFDEF DELPHI}
  FNodes := TObjectList<TLccNode>.Create;
  {$ELSE}
  FNodes := TObjectList.Create;
  {$ENDIF}
  FNodes.OwnsObjects := False;
  {$ENDIF}
end;

function TLccNodeManager.AddNode(CdiXML: string): TLccNode;
begin
  Result := TLccNode.Create({$IFDEF FPC}@{$ENDIF}LccMessageSendCallback, Self, CdiXML);
  Nodes.Add(Result);
  DoCreateLccNode(Result);
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

function TLccNodeManager.GetNode(Index: Integer): TLccNode;
begin
  if Index < Nodes.Count then
    Result := Nodes[Index] as TLccNode
  else
    Result := nil;
end;

procedure TLccNodeManager.LogoutAll;
var
  i: Integer;
begin
  for i := 0 to Nodes.Count - 1 do
  begin
    // Maybe an OnLogout property?
    TLccNode( Nodes[i]).Logout;
  end;
end;

procedure TLccNodeManager.ProcessMessage(LccMessage: TLccMessage);
var
  i: Integer;
begin
  DoLccMessageReceive(LccMessage);
  for i := 0 to Nodes.Count - 1 do
    TLccNode( Nodes[i]).ProcessMessage(LccMessage);
end;

procedure TLccNodeManager.SendMessage(LccMessage: TLccMessage);
begin
  DoLccMessageSend(LccMessage);
end;

procedure TLccNodeManager.LccMessageSendCallback(LccMessage: TLccMessage);
begin
  DoLccMessageSend(LccMessage);
end;



initialization
{$IFNDEF DWSCRIPT}
   RegisterClass(TLccNodeManager);
{$ENDIF}

finalization

end.

