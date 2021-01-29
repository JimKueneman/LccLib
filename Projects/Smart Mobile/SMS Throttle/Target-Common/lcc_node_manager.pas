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
  lcc_node_train,
  lcc_node_controller,
  lcc_defines,
  lcc_utilities,
  lcc_node_messages;


const
  MAX_HARDWARE_CONNECTIONS = 10;  // Lazy here to make it dynamic to use with SMS should never ever need more than 10

type

  // This is how the Node Manager has access to the Connection Links with needing
  // to have access the objects associated with the Links.  The Links have a
  // pointer to TLccNodeManager so this allows us to not have to have a single unit
  // to have visibility back and forth.
  IHardwareConnectionManagerLink = interface
    ['{619C8E64-69C3-94A6-B6FE-B16B6CB57A45}']
    procedure SendMessage(AMessage: TLccMessage);
    function IsLccLink: Boolean;
    function GetConnected: Boolean;
//    if the node manager has no more active threads in its links then it should free all nodes and alias maps since it is no longer connected to any LCC networks and everythinbg is stale.
  end;

  INodeManager = interface
    ['{0FE72011-CBDB-9EDC-0C36-C5031A63F27F}']
    procedure Clear;
    function AddNode(CdiXML: string; AutoLogin: Boolean): TLccNode;
    function AddNodeByClass(CdiXML: string; NodeClass: TLccNodeClass; AutoLogin: Boolean): TLccNode;
    procedure LogoutAll;
    function GetNode(Index: Integer): TLccNode;
    function GetNodeCount: Integer;
    function ExtractNode(Index: Integer): TLccNode;
  end;


  INodeManagerCallbacks = interface
    ['{C6920bCA-08BC-4D45-B27C-174640FA3106}']
    procedure DoAliasIDChanged(LccNode: TLccNode);               //*
    procedure DoCANAliasMapReset(LccNode: TLccNode);             //*
    procedure DoCDIRead(LccNode: TLccNode);
    procedure DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode; AddressSpace: Byte);
    procedure DoConfigMemOptionsReply(LccNode: TLccNode);
    procedure DoConfigMemReadReply(LccNode: TLccNode);
    procedure DoConfigMemWriteReply(LccNode: TLccNode);
    procedure DoCreateLccNode(LccNode: TLccNode);     //*
    procedure DoConsumerIdentify(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
    procedure DoDatagramReply(LccNode: TLccNode);
    procedure DoDestroyLccNode(LccNode: TLccNode);   //*
    procedure DoLogInNode(LccNode: TLccNode);
    procedure DoLogOutNode(LccNode: TLccNode);
    procedure DoFDI(LccNode: TLccNode);
    procedure DoFunctionConfiguration(LccNode: TLccNode);
    procedure DoInitializationComplete(LccNode: TLccNode);   //*
    procedure DoNodeIDChanged(LccNode: TLccNode);                  //*
    procedure DoOptionalInteractionRejected(LccNode: TLccNode);
    procedure DoProducerIdentify(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
    procedure DoProtocolIdentifyReply(LccNode: TLccNode);
    procedure DoRemoteButtonReply(LccNode: TLccNode);
    procedure DoSimpleNodeIdentReply(LccNode: TLccNode);
    procedure DoSimpleTrainNodeIdentReply(LccNode: TLccNode);
    procedure DoTractionEmergencyStop(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionSpeedSet(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionFunctionSet(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionQuerySpeed(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionQueryFunction(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionControllerConfig(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionListenerConfig(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoTractionManage(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure DoVerifiedNodeID(LccNode: TLccNode);
  end;

type

  TOnLccNodeMessage = procedure(Sender: TObject; LccSourceNode: TLccNode) of object;
  TOnLccNodeMessageWithDest = procedure(Sender: TObject; LccNode: TLccNode) of object;
  TOnLccNodeEventIdentify = procedure(Sender: TObject; LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean) of object;
  TOnLccNodeEventIdentified = procedure(Sender: TObject; Lccnode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState) of object;
  TOnLccNodeMessageResultCode = procedure(Sender: TObject; LccNode: TLccNode; LccMessage: TLccMessage; ResultCode: Byte) of object;
  TOnLccNodeConfigMem = procedure(Sender: TObject; LccNode: TLccNode) of object;
  TOnLccNodeConfigMemAddressSpace = procedure(Sender: TObject; LccNode: TLccNode; AddressSpace: Byte) of object;
  TOnLccNodeMessageWithReply = procedure(Sender: TObject; LccSourceNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean) of object;

  { TLccNodeManager }

  TLccNodeManager = class(TComponent, INodeManagerCallbacks, INodeManager)
  private
    FGridConnect: Boolean;
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
    FOnLccNodeLogin: TOnLccNodeMessage;
    FOnLccNodeLogout: TOnLccNodeMessage;
    FOnLccNodeOptionalInteractionRejected: TOnLccNodeMessageWithDest;
    FOnLccNodeProducerIdentified: TOnLccNodeEventIdentified;
    FOnLccNodeProducerIdentify: TOnLccNodeEventIdentify;
    FOnLccNodeProtocolIdentifyReply: TOnLccNodeMessageWithDest;
    FOnLccNodeRemoteButtonReply: TOnLccNodeMessageWithDest;
    FOnLccNodeSimpleNodeIdentReply: TOnLccNodeMessageWithDest;
    FOnLccNodeSimpleTrainNodeIdentReply: TOnLccNodeMessageWithDest;
    FOnLccNodeTractionControllerConfig: TOnLccNodeMessageWithReply;
    FOnLccNodeTractionEmergencyStop: TOnLccNodeMessageWithReply;
    FOnLccNodeTractionFunctionSet: TOnLccNodeMessageWithReply;
    FOnLccNodeTractionListenerConfig: TOnLccNodeMessageWithReply;
    FOnLccNodeTractionManage: TOnLccNodeMessageWithReply;
    FOnLccNodeTractionQueryFunction: TOnLccNodeMessageWithReply;
    FOnLccNodeTractionQuerySpeed: TOnLccNodeMessageWithReply;
    FOnLccNodeTractionSpeedSet: TOnLccNodeMessageWithReply;
    FOnLccNodeVerifiedNodeID: TOnLccNodeMessage;
    FOnLccMessageSend: TOnMessageEvent;

    {$IFDEF DELPHI}
    FNodes: TObjectList<TLccNode>;
    {$ELSE}
    FNodes: TObjectList;
    {$ENDIF}
    FWorkerMessage: TLccMessage;
    function GetNode(Index: Integer): TLccNode;
  protected
    HardwareConnectionLinkArray: array[0..MAX_HARDWARE_CONNECTIONS] of IHardwareConnectionManagerLink;
    HardwareConnectionLinkCount: Integer;
    HardwareConnectionConnectedCount: Integer;

    procedure DoAliasIDChanged(LccNode: TLccNode); virtual;               //*
    procedure DoCANAliasMapReset(LccNode: TLccNode); virtual;             //*
    procedure DoCDIRead(LccNode: TLccNode); virtual;
    procedure DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode; AddressSpace: Byte); virtual;
    procedure DoConfigMemOptionsReply(LccNode: TLccNode); virtual;
    procedure DoConfigMemReadReply(LccNode: TLccNode); virtual;
    procedure DoConfigMemWriteReply(LccNode: TLccNode); virtual;
    procedure DoCreateLccNode(LccNode: TLccNode); virtual;     //*
    procedure DoLogInNode(LccNode: TLccNode); virtual;         //*
    procedure DoLogOutNode(LccNode: TLccNode); virtual;
    procedure DoConsumerIdentify(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoConsumerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState); virtual;
    procedure DoDatagramReply(LccNode: TLccNode); virtual;
    procedure DoDestroyLccNode(LccNode: TLccNode); virtual;   //*
    procedure DoFDI(LccNode: TLccNode); virtual;
    procedure DoFunctionConfiguration(LccNode: TLccNode); virtual;
    procedure DoInitializationComplete(LccNode: TLccNode); virtual;   //*
    procedure DoNodeIDChanged(LccNode: TLccNode); virtual;                  //*
    procedure DoOptionalInteractionRejected(LccNode: TLccNode); virtual;
    procedure DoProducerIdentify(LccNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure DoProducerIdentified(LccNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState); virtual;
    procedure DoProtocolIdentifyReply(LccNode: TLccNode); virtual;
    procedure DoRemoteButtonReply(LccNode: TLccNode); virtual;
    procedure DoSimpleNodeIdentReply(LccNode: TLccNode); virtual;
    procedure DoSimpleTrainNodeIdentReply(LccNode: TLccNode); virtual;
    procedure DoTractionEmergencyStop(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoTractionSpeedSet(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoTractionFunctionSet(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoTractionQuerySpeed(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoTractionQueryFunction(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoTractionControllerConfig(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoTractionListenerConfig(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoTractionManage(LccNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean); virtual;
    procedure DoVerifiedNodeID(LccNode: TLccNode); virtual;

    procedure DoLccMessageSend(Sender: TObject; Message: TLccMessage); virtual;
    procedure DoLccMessageReceive(Message: TLccMessage); virtual;

    procedure LccMessageSendCallback(Sender: TObject; LccMessage: TLccMessage); //  The Callback function for all Nodes use to reply to message they can automaticallly

  public
    // Connection Manager

    property GridConnect: Boolean read FGridConnect;
    {$IFDEF DELPHI}
    property Nodes: TOBjectList<TLccNode> read FNodes write FNodes;
    {$ELSE}
    property Nodes: TOBjectList read FNodes write FNodes;
    {$ENDIF}
    property Node[Index: Integer]: TLccNode read GetNode;

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    constructor Create(AnOwner: TComponent; GridConnectLink: Boolean); {$IFNDEF DWSCRIPT} reintroduce; virtual; {$ENDIF}
    destructor Destroy; override;

    procedure Clear;
    function AddNode(CdiXML: string; AutoLogin: Boolean): TLccNode; virtual;
    function AddNodeByClass(CdiXML: string; NodeClass: TLccNodeClass; AutoLogin: Boolean): TLccNode; virtual;
    function GetNodeCount: Integer;
    function ExtractNode(Index: Integer): TLccNode;

    procedure LogoutAll;

    function FindConnectedLink: IHardwareConnectionManagerLink;

    procedure ProcessMessage(LccMessage: TLccMessage);  // Takes incoming messages and dispatches them to the nodes
    procedure SendMessage(Sender: TObject; LccMessage: TLccMessage); // Outgoing messages are passed through this method, its address is given to Nodes and other objects that need to send messages
    procedure ReceiveMessage(ConnectionManager: IHardwareConnectionManagerLink; ALccMessage: TLccMessage);  // Takes all incoming messages from all Connection object and dispatches them to the nodes and dispaches them to other Connections that are registered

    procedure RegisterHardwareConnectionLink(AConnectionManagerLink: IHardwareConnectionManagerLink); // Any Connection that needs to send/receive OpenLCB message must register with the NodeManage here
    procedure UnRegisterHardwareConnectionLink(AConnectionManagerLink: IHardwareConnectionManagerLink);

  published

    // Node Management
    property OnLccNodeCreate: TOnLccNodeMessage read FOnLccNodeCreate write FOnLccNodeCreate;
    property OnLccNodeDestroy: TOnLccNodeMessage read FOnLccNodeDestroy write FOnLccNodeDestroy;
    property OnLccNodeLogin: TOnLccNodeMessage read FOnLccNodeLogin write FOnLccNodeLogin;
    property OnLccNodeLogout: TOnLccNodeMessage read FOnLccNodeLogout write FOnLccNodeLogout;
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
    property OnLccNodeTractionEmergencyStop: TOnLccNodeMessageWithReply read FOnLccNodeTractionEmergencyStop write FOnLccNodeTractionEmergencyStop;
    property OnLccNodeTractionSpeedSet: TOnLccNodeMessageWithReply read FOnLccNodeTractionSpeedSet write FOnLccNodeTractionSpeedSet;
    property OnLccNodeTractionFunctionSet: TOnLccNodeMessageWithReply read FOnLccNodeTractionFunctionSet write FOnLccNodeTractionFunctionSet;
    property OnLccNodeTractionQuerySpeed: TOnLccNodeMessageWithReply read FOnLccNodeTractionQuerySpeed write FOnLccNodeTractionQuerySpeed;
    property OnLccNodeTractionQueryFunction: TOnLccNodeMessageWithReply read FOnLccNodeTractionQueryFunction write FOnLccNodeTractionQueryFunction;
    property OnLccNodeTractionControllerConfig: TOnLccNodeMessageWithReply read FOnLccNodeTractionControllerConfig write FOnLccNodeTractionControllerConfig;
    property OnLccNodeTractionListenerConfig: TOnLccNodeMessageWithReply read FOnLccNodeTractionListenerConfig write FOnLccNodeTractionListenerConfig;
    property OnLccNodeTractionManage: TOnLccNodeMessageWithReply read FOnLccNodeTractionManage write FOnLccNodeTractionManage;

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


implementation

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

procedure TLccNodeManager.DoCDIRead(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeCDI) then
    OnLccNodeCDI(Self, LccNode)
end;

procedure TLccNodeManager.DoConfigMemAddressSpaceInfoReply(LccNode: TLccNode;
  AddressSpace: Byte);
begin
 if Assigned(OnLccNodeConfigMemAddressSpaceInfoReply) then
   OnLccNodeConfigMemAddressSpaceInfoReply(Self, LccNode, AddressSpace);
end;

procedure TLccNodeManager.DoConfigMemOptionsReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemOptionsReply) then
    OnLccNodeConfigMemOptionsReply(Self, LccNode);
end;

procedure TLccNodeManager.DoConfigMemReadReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemReadReply) then
    OnLccNodeConfigMemReadReply(Self, LccNode);
end;

procedure TLccNodeManager.DoConfigMemWriteReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemWriteReply) then
    OnLccNodeConfigMemWriteReply(Self, LccNode);
end;

procedure TLccNodeManager.DoCreateLccNode(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeCreate) then
    OnLccNodeCreate(Self, LccNode)
end;

procedure TLccNodeManager.DoConsumerIdentified(LccNode: TLccNode;
  LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeConsumerIdentified) then
    OnLccNodeConsumerIdentified(Self, LccNode, LccMessage, Event, State);
end;

procedure TLccNodeManager.DoConsumerIdentify(LccNode: TLccNode;
  LccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnLccNodeConsumerIdentify) then
    OnLccNodeConsumerIdentify(Self, LccNode, LccMessage, DoDefault);
end;

procedure TLccNodeManager.DoDatagramReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeDatagramReply) then
    OnLccNodeDatagramReply(Self, LccNode);
end;

procedure TLccNodeManager.DoDestroyLccNode(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeDestroy) then
    OnLccNodeDestroy(Self, LccNode);
end;

procedure TLccNodeManager.DoFDI(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeFDI) then
    OnLccNodeFDI(Self, LccNode)
end;

procedure TLccNodeManager.DoFunctionConfiguration(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeFunctionConfiguration) then
    OnLccNodeFunctionConfiguration(Self, LccNode)
end;

procedure TLccNodeManager.DoInitializationComplete(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeInitializationComplete) then
    OnLccNodeInitializationComplete(Self, LccNode);
end;

procedure TLccNodeManager.DoLccMessageReceive(Message: TLccMessage);
begin
  if Assigned(OnLccMessageReceive) then
    OnLccMessageReceive(Self, Message);
end;

procedure TLccNodeManager.DoLccMessageSend(Sender: TObject; Message: TLccMessage);
begin
  if Assigned(OnLccMessageSend) then
    OnLccMessageSend(Sender, Message);
end;

procedure TLccNodeManager.DoLogInNode(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeLogin) then
    OnLccNodeLogin(Self, LccNode);
end;

procedure TLccNodeManager.DoLogOutNode(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeLogout) then
    OnLccNodeLogout(Self, LccNode);
end;

procedure TLccNodeManager.DoNodeIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeIDChanged) then
    OnLccNodeIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoOptionalInteractionRejected(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeOptionalInteractionRejected) then
    OnLccNodeOptionalInteractionRejected(Self, LccNode);
end;

procedure TLccNodeManager.DoProducerIdentified(LccNode: TLccNode;
  LccMessage: TLccMessage; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeProducerIdentified) then
    OnLccNodeProducerIdentified(Self, LccNode, LccMessage, Event, State);
end;

procedure TLccNodeManager.DoProducerIdentify(LccNode: TLccNode;
  LccMessage: TLccMessage; var DoDefault: Boolean);
begin
  if Assigned(OnLccNodeProducerIdentify) then
    OnLccNodeProducerIdentify(Self, LccNode, LccMessage, DoDefault);
end;

procedure TLccNodeManager.DoProtocolIdentifyReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeProtocolIdentifyReply) then
    OnLccNodeProtocolIdentifyReply(Self, LccNode);
end;

procedure TLccNodeManager.DoRemoteButtonReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeRemoteButtonReply) then
    OnLccNodeRemoteButtonReply(Self, LccNode);
end;

procedure TLccNodeManager.DoSimpleNodeIdentReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeSimpleNodeIdentReply) then
    OnLccNodeSimpleNodeIdentReply(Self, LccNode);
end;

procedure TLccNodeManager.DoSimpleTrainNodeIdentReply(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeSimpleTrainNodeIdentReply) then
    OnLccNodeSimpleTrainNodeIdentReply(Self, LccNode);
end;

procedure TLccNodeManager.DoTractionQuerySpeed(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionQuerySpeed) then
    OnLccNodeTractionQuerySpeed(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionSpeedSet(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionSpeedSet) then
    OnLccNodeTractionSpeedSet(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionQueryFunction(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionQueryFunction) then
    OnLccNodeTractionQueryFunction(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionControllerConfig(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionControllerConfig) then
    OnLccNodeTractionControllerConfig(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionEmergencyStop(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionEmergencyStop) then
    OnLccNodeTractionEmergencyStop(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionFunctionSet(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionFunctionSet) then
    OnLccNodeTractionFunctionSet(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionListenerConfig(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionListenerConfig) then
    OnLccNodeTractionListenerConfig(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoTractionManage(LccNode: TLccNode;
  LccMessage: TLccMessage; IsReply: Boolean);
begin
  if Assigned(OnLccNodeTractionManage) then
    OnLccNodeTractionManage(Self, LccNode, LccMessage, IsReply);
end;

procedure TLccNodeManager.DoVerifiedNodeID(LccNode: TLccNode);
begin
  if Assigned(OnLccNodeVerifiedNodeID) then
    OnLccNodeVerifiedNodeID(Self, LccNode);
end;

function TLccNodeManager.ExtractNode(Index: Integer): TLccNode;
begin
  Result := nil;
  if Index < Nodes.Count then
  begin
    Result := Nodes[Index] as TLccNode;
    {$IFDEF DWSCRIPT}
    Nodes.Remove(Index);
    {$ELSE}
    Nodes.Delete(Index);
    {$ENDIF}
  end;
end;

constructor TLccNodeManager.Create(AnOwner: TComponent; GridConnectLink: Boolean);
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
  FGridConnect := GridConnectLink;
  FWorkerMessage := TLccMessage.Create;
end;

function TLccNodeManager.AddNode(CdiXML: string; AutoLogin: Boolean): TLccNode;
begin
  Result := TLccNode.Create({$IFDEF FPC}@{$ENDIF}LccMessageSendCallback, Self, CdiXML, GridConnect);
  Nodes.Add(Result);
  DoCreateLccNode(Result);
  if AutoLogin then
    Result.Login(NULL_NODE_ID);
end;

function TLccNodeManager.AddNodeByClass(CdiXML: string; NodeClass: TLccNodeClass; AutoLogin: Boolean): TLccNode;
begin
  Result := nil;
  if Assigned(NodeClass) then
  begin
    Result := NodeClass.Create({$IFDEF FPC}@{$ENDIF}LccMessageSendCallback, Self, CdiXML, GridConnect);
    Nodes.Add(Result);
    DoCreateLccNode(Result);
    if AutoLogin then
      Result.Login(NULL_NODE_ID);
  end;
end;

destructor TLccNodeManager.Destroy;
begin
  LogoutAll;
  Clear;
  FNodes.Free;
  FreeAndNil(FWorkerMessage);
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

function TLccNodeManager.GetNode(Index: Integer): TLccNode;
begin
  if Index < Nodes.Count then
    Result := Nodes[Index] as TLccNode
  else
    Result := nil;
end;

function TLccNodeManager.GetNodeCount: Integer;
begin
  Result := Nodes.Count;
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

function TLccNodeManager.FindConnectedLink: IHardwareConnectionManagerLink;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while (i < HardwareConnectionConnectedCount) and not Assigned(Result) do
  begin
    if HardwareConnectionLinkArray[i].IsLccLink and HardwareConnectionLinkArray[i].GetConnected then
      Result := HardwareConnectionLinkArray[i];
    Inc(i);
  end;
end;

procedure TLccNodeManager.ProcessMessage(LccMessage: TLccMessage);
var
  i: Integer;
begin
  DoLccMessageReceive(LccMessage);
  for i := 0 to Nodes.Count - 1 do
    ( Nodes[i] as TLccNode).ProcessMessage(LccMessage);
end;

procedure TLccNodeManager.SendMessage(Sender: TObject; LccMessage: TLccMessage);
var
  i: Integer;
begin
  // Send the message to the wire

  // Emumerate all Hardware Connections and pass on the message to send
  for i := 0 to HardwareConnectionLinkCount - 1 do
    HardwareConnectionLinkArray[i].SendMessage(LccMessage);

  // Send the messages to all the other virtual nodes.
  if Sender is TLccNode then
  begin
    for i := 0 to Nodes.Count - 1 do
    begin

 //     Assert(not NullNodeID( (Node[i] as TLccNode).NodeID ));
 //     Assert(not NullNodeID( LccMessage.SourceID ));

      // don't sent it back to itself but deliver it to all the other virtual owned nodes
      if not EqualNode(Node[i].NodeID, Node[i].AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, True) then
        Node[i].ProcessMessage(LccMessage);
    end;
  end;

  // Allow app to see it
  DoLccMessageSend(Sender, LccMessage);
end;

procedure TLccNodeManager.ReceiveMessage(ConnectionManager: IHardwareConnectionManagerLink; ALccMessage: TLccMessage);
var
  i: Integer;
begin
  // This message came in through a hardware connection so relay it out to the other connections
  for i := 0 to HardwareConnectionLinkCount - 1 do
  begin
    if (HardwareConnectionLinkArray[i].IsLccLink) and (HardwareConnectionLinkArray[i] <> ConnectionManager)  then
      HardwareConnectionLinkArray[i].SendMessage(ALccMessage);
  end;

  ProcessMessage(ALccMessage);
end;

procedure TLccNodeManager.LccMessageSendCallback(Sender: TObject; LccMessage: TLccMessage);
begin
  SendMessage(Sender, LccMessage);
end;

procedure TLccNodeManager.RegisterHardwareConnectionLink(AConnectionManagerLink: IHardwareConnectionManagerLink);
begin
  HardwareConnectionLinkArray[HardwareConnectionLinkCount] := AConnectionManagerLink;
  Inc(HardwareConnectionLinkCount);
end;

procedure TLccNodeManager.UnRegisterHardwareConnectionLink(AConnectionManagerLink: IHardwareConnectionManagerLink);
var
  i, j: Integer;
begin
  for i := 0 to HardwareConnectionLinkCount - 1 do
  begin  // Find the index of the link
    if HardwareConnectionLinkArray[i] = AConnectionManagerLink then
    begin  // remove it by sliding the rest of the links down in the array then decrementing the index
      for j := i to HardwareConnectionLinkCount - 1 do
        HardwareConnectionLinkArray[j] := HardwareConnectionLinkArray[j+1];
      Dec(HardwareConnectionLinkCount);
    end;
  end;
end;


initialization
{$IFNDEF DWSCRIPT}
   RegisterClass(TLccNodeManager);
{$ENDIF}

finalization
end.

