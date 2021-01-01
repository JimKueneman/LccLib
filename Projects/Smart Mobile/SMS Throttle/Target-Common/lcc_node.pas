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

  TIMEOUT_CONTROLLER_NOTIFY_WAIT = 2500;

const

 CDI_XML: string = (
'<?xml version="1.0" encoding="utf-8"?>'+
'<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
'<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
	'<identification>'+
		'<manufacturer>Mustangpeak</manufacturer>'+
		'<model>TC1000</model>'+
		'<hardwareVersion>1.0.0.0</hardwareVersion>'+
		'<softwareVersion>1.0.0.0</softwareVersion>'+
	'</identification>'+
	'<segment origin="1" space="253">'+
		'<name>User</name>'+
		'<description>User defined information</description>'+
		'<group>'+
			'<name>User Data</name>'+
			'<description>Add your own unique node info here</description>'+
			'<string size="63">'+
				'<name>User Name</name>'+
			'</string>'+
			'<string size="64">'+
				'<name>User Description</name>'+
			'</string>'+
		'</group>'+
	'</segment>'+
'</cdi>');


type
  TOnActionCompleteCallback = procedure(LccAction: TObject) of object;

  TLccNode = class;
  TLccAction = class;
  TLccActionHub = class;

  TOnActionTimoutExpired = procedure(Sender: TLccAction) of object;

 { TLccAction }

 TLccAction = class(TObject)
 private
   FActionHub: TLccActionHub;
   FActionStateIndex: Integer;
   FAliasID: Word;
   FCancel: Boolean;       // Cancels the action, mimics a Timeout in a state which jumps to last state to clean up
   FIgnoreTimer: Boolean;
   FNodeID: TNodeID;
   FOnTimeoutExpired: TOnActionTimoutExpired;
   FOwner: TLccNode;
   FSendMessage: TOnMessageEvent;
   FStates: TOnMessageEventArray;
   FTargetAliasID: Word;
   FTargetNodeID: TNodeID;
   FTimeoutCounts: Integer;
   FTimeoutCountThreshold: Integer;
   FWorkerMessage: TLccMessage;
 protected
   property ActionHub: TLccActionHub read FActionHub write FActionHub;
   property ActionStateIndex: Integer read FActionStateIndex write FActionStateIndex;
   property States: TOnMessageEventArray read FStates write FStates;
   property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
   property TimeoutCounts: Integer read FTimeoutCounts write FTimeoutCounts;
   property TimeoutCountThreshold: Integer read FTimeoutCountThreshold write FTimeoutCountThreshold;

   function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; virtual;
   function _NFinalStateCleanup(Sender: TObject; SourceMessage: TLccMessage): Boolean; virtual;

   procedure LoadStateArray; virtual;   // Assign your state functions to the array to get called in order
   function ProcessMessage(SourceMessage: TLccMessage): Boolean; virtual;
   procedure TimeTick; virtual;    // 800ms Clock

   procedure DoTimeoutExpired; virtual;
   procedure UnRegisterSelf;

 public
   property Cancel: Boolean read FCancel write FCancel;

   property AliasID: Word read FAliasID;
   property NodeID: TNodeID read FNodeID;
   property TargetNodeID: TNodeID read FTargetNodeID write FTargetNodeID;
   property TargetAliasID: Word read FTargetAliasID write FTargetAliasID;

   property Owner: TLccNode read FOwner;

   property SendMessage: TOnMessageEvent read FSendMessage write FSendMessage;

   // Disables the timer from running any code.  Does not physically disable the timer but just short circuits TimeTick
   property IgnoreTimer: Boolean read FIgnoreTimer write FIgnoreTimer;
   property OnTimeoutExpired: TOnActionTimoutExpired read FOnTimeoutExpired write FOnTimeoutExpired;

   constructor Create(AnOwner: TLccNode; ANodeID: TNodeID; AnAliasID: Word);
   destructor Destroy; override;

   // Set the index to the next function that will be pointed to in the States array
   function AdvanceToNextState(JumpCount: Integer = 1): Integer;
   // Each decendant must set the number of state functions it implements
   procedure SetStateArrayLength(NewLength: Integer);
   // Sets the number of ms before any wait state is declared hung or complete
   procedure SetTimoutCountThreshold(NewThreshold_ms: Integer; ResetCounter: Boolean = True);  // 800ms counts
   // set the timer counter that starts counting toward the CountThreshold
   procedure ResetTimeoutCounter;
   // Compares CountThreshold and Counter to see if the timeout timer has expired
   function TimeoutExpired: Boolean;
   // Assign the other end of the communcation this Action has the pipe open to
   procedure AssignTargetNode(ATargetNodeID: TNodeID; ATargetAliasID: Word);
 end;

 { TLccActionHub }

 TLccActionHub = class(TObject)
 private
   {$IFDEF DELPHI}
    FLccActiveActions: TObjectList<TLccAction>;
    FLccCompletedActions: TObjectList<TLccAction>;
   {$ELSE}
    FLccActiveActions: TObjectList;
    FLccCompletedActions: TObjectList;
   {$ENDIF}
    FOwner: TLccNode;
    FSendMessageFunc: TOnMessageEvent;
    FWorkerMessage: TLccMessage;
 protected
   {$IFDEF DELPHI}
   property LccActiveActions: TObjectList<TLccAction> read FLccActiveActions write FLccActiveActions;
   property LccCompletedActions: TObjectList<TLccAction> read FLccCompletedActions write FLccCompletedActions;
   {$ELSE}
   property LccActiveActions: TObjectList read FLccActiveActions write FLccActiveActions;
   property LccCompletedActions: TObjectList read FLccCompletedActions write FLccCompletedActions;
   {$ENDIF}
   property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
   property SendMessageFunc: TOnMessageEvent read FSendMessageFunc write FSendMessageFunc;
   procedure TimeTick;
 public
   property Owner: TLccNode read FOwner write FOwner;

   constructor Create(AnOwner: TLccNode; ASendMessageFunc: TOnMessageEvent);
   destructor Destory;

   procedure ClearActions;
   procedure ClearCompletedActions;
   function ProcessMessage(SourceMessage: TLccMessage): Boolean;
   function RegisterAction(ANode: TLccNode; SourceMessage: TLccMessage; AnAction: TLccAction): Boolean; overload;
   function RegisterAction(ANode: TLccNode; ATargetNodeID: TNodeID; ATargetAliasID: Word; AnAction: TLccAction): Boolean; overload;
   procedure UnregisterActionAndMarkForFree(AnAction: TLccAction);
 end;


  { TLccNode }

  TLccNode = class(TObject)
  private
    FLccActions: TLccActionHub;
    FWorkerMessageDatagram: TLccMessage;
    FInitialized: Boolean;
    FNodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF};
    FSendMessageFunc: TOnMessageEvent;
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
    procedure SendDatagramAckReply(SourceMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
    procedure SendDatagramRejectedReply(SourceMessage: TLccMessage; Reason: Word);
    procedure SendDatagramRequiredReply(SourceMessage, ReplyLccMessage: TLccMessage);
    procedure On_800msTimer(Sender: TObject);  virtual;
    function GetCdiFile: string; virtual;
    procedure BeforeLogin; virtual;
  public
    property DatagramResendQueue: TDatagramQueue read FDatagramResendQueue;
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr: String read GetNodeIDStr;
    property Initialized: Boolean read FInitialized;
    property LccActions: TLccActionHub read FLccActions write FLccActions;
    property SendMessageFunc: TOnMessageEvent read FSendMessageFunc;

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

    constructor Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string); virtual;
    destructor Destroy; override;

    function IsNode(ALccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean; virtual;
    procedure Login(ANodeID: TNodeID); virtual;
    procedure Logout; virtual;
    function ProcessMessage(SourceMessage: TLccMessage): Boolean; virtual;
    procedure SendEvents;
    procedure SendConsumedEvents;
    procedure SendConsumerIdentify(var Event: TEventID);
    procedure SendProducedEvents;
    procedure SendProducerIdentify(var Event: TEventID);
    procedure SendInitializeComplete;
  end;

  TLccNodeClass = class of TLccNode;

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

     constructor Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string); override;
     destructor Destroy; override;
     function IsNode(ALccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean; override;
     procedure Login(ANodeID: TNodeID); override;
     procedure Logout; override;
     function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
     procedure SendGlobalAME;
  end;

  TLccCanNodeClass = class of TLccCanNode;



var
  InprocessMessageAllocated: Integer = 0;
  ActionObjectsAllocated: Integer = 0;

implementation

uses
  lcc_node_manager;

{ TLccAction }

constructor TLccAction.Create(AnOwner: TLccNode; ANodeID: TNodeID;
  AnAliasID: Word);
begin
  Inc(ActionObjectsAllocated);
  FOwner := AnOwner;;
  WorkerMessage := TLccMessage.Create;
  FNodeID := ANodeID;
  FAliasID := AnAliasID;
  SetTimoutCountThreshold(5000);  // Default 5 seconds
  LoadStateArray;
end;

function TLccAction.AdvanceToNextState(JumpCount: Integer): Integer;
begin
  Inc(FActionStateIndex, JumpCount);
  Result := FActionStateIndex;
end;

destructor TLccAction.Destroy;
begin
  Dec(ActionObjectsAllocated);
  {$IFDEF DWSCRIPT}
  WorkerMessage.Free;
  {$ELSE}
  FreeAndNil(FWorkerMessage);
  {$ENDIF}
  inherited Destroy;
end;

procedure TLccAction.DoTimeoutExpired;
begin
  if Assigned(OnTimeoutExpired) then
    OnTimeoutExpired(Self)
end;

function TLccAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  FActionStateIndex := 0;
  if Assigned(SourceMessage) then
    AssignTargetNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias)
end;

function TLccAction._NFinalStateCleanup(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  FActionStateIndex := Length(FStates) - 1;
  UnRegisterSelf;
end;

procedure TLccAction.LoadStateArray;
begin
  SetStateArrayLength(2);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup
end;

function TLccAction.ProcessMessage(SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  // Only send to the active State in the Action
  if (ActionStateIndex > -1) and (ActionStateIndex < Length(States)) then
    Result := States[ActionStateIndex](Owner, SourceMessage);
end;

procedure TLccAction.ResetTimeoutCounter;
begin
  FTimeoutCounts := 0;
end;

procedure TLccAction.SetStateArrayLength(NewLength: Integer);
begin
  {$IFDEF DWSCRIPT}
  FStates.SetLength(NewLength);
  {$ELSE}
  SetLength(FStates, NewLength);
  {$ENDIF}
end;

procedure TLccAction.SetTimoutCountThreshold(NewThreshold_ms: Integer; ResetCounter: Boolean);
begin
  TimeoutCountThreshold := Trunc(NewThreshold_ms/800) + 1;
  if ResetCounter then
    TimeoutCounts := 0;
end;

procedure TLccAction.TimeTick;
begin
  if not IgnoreTimer then
  begin
    Inc(FTimeoutCounts);
    ProcessMessage(nil);    // Force a clock tick in the state machine
  end
end;

procedure TLccAction.UnRegisterSelf;
begin
  if Assigned(ActionHub) then
    ActionHub.UnregisterActionAndMarkForFree(Self);
end;

function TLccAction.TimeoutExpired: Boolean;
begin
  Result := TimeoutCounts > TimeoutCountThreshold ;
end;

procedure TLccAction.AssignTargetNode(ATargetNodeID: TNodeID;
  ATargetAliasID: Word);
begin
  FTargetNodeID := ATargetNodeID;
  FTargetAliasID := ATargetAliasID;
end;

{ TLccActionHub }

constructor TLccActionHub.Create(AnOwner: TLccNode;
  ASendMessageFunc: TOnMessageEvent);
begin
  {$IFDEF DELPHI}
  LccActiveActions := TObjectList<TLccAction>.Create(False);
  LccCompletedActions := TObjectList<TLccAction>.Create(False);
  {$ELSE}
   LccActiveActions := TObjectList.Create;
   LccCompletedActions := TObjectList.Create;
   {$IFNDEF DWSCRIPT}
   LccActiveActions.OwnsObjects := False;
   LccCompletedActions.OwnsObjects := False;
   {$ENDIF}
  {$ENDIF}

  FWorkerMessage := TLccMessage.Create;
  FOwner := AnOwner;
  FSendMessageFunc := ASendMessageFunc;
end;

procedure TLccActionHub.ClearActions;
var
  i: Integer;
begin
  try
    for i := 0 to LccActiveActions.Count - 1 do
       LccActiveActions[I].Free;
  finally
    LccActiveActions.Clear;
  end;
end;

procedure TLccActionHub.ClearCompletedActions;
var
  i: Integer;
begin
  try
    for i := 0 to LccCompletedActions.Count - 1 do
       LccCompletedActions[I].Free;
  finally
    LccCompletedActions.Clear;
  end;
end;

destructor TLccActionHub.Destory;
begin
  ClearActions;
  ClearCompletedActions;
  {$IFNDEF DWSCRIPT}
  FreeAndNil(FLccActiveActions);
  FreeAndNil(FLccCompletedActions);
  FreeAndNil(FWorkerMessage);
  {$ELSE}
  LccActiveActions.Free;
  LccCompletedActions.Free;
  WorkerMessage.Free;
  {$ENDIF}
end;

function TLccActionHub.ProcessMessage(SourceMessage: TLccMessage): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := LccActiveActions.Count - 1 downto 0 do     // May removed some items during the call
    Result := (LccActiveActions[i] as TLccAction).ProcessMessage(SourceMessage);;
end;

function TLccActionHub.RegisterAction(ANode: TLccNode; SourceMessage: TLccMessage; AnAction: TLccAction): Boolean;
begin
  LccActiveActions.Add(AnAction);
  AnAction.FNodeID := ANode.NodeID;
  AnAction.FAliasID := (ANode as TLccCanNode).AliasID;
  AnAction.FOwner := ANode;
  AnAction.SendMessage := ANode.SendMessageFunc;
  AnAction.ActionHub := Self;
  Result := AnAction._0ReceiveFirstMessage(ANode, SourceMessage);
end;

function TLccActionHub.RegisterAction(ANode: TLccNode; ATargetNodeID: TNodeID; ATargetAliasID: Word; AnAction: TLccAction): Boolean;
begin
  Result := True;
  // Dummy message just to load the NodeIDs
  AnAction.TargetNodeID := ATargetNodeID;
  AnAction.TargetAliasID := ATargetAliasID;
  RegisterAction(ANode, nil, AnAction);
end;

procedure TLccActionHub.TimeTick;
var
  i: Integer;
begin
  for i := LccActiveActions.Count - 1 downto 0  do
    (LccActiveActions[i] as TLccAction).TimeTick;
  ClearCompletedActions;
end;

procedure TLccActionHub.UnregisterActionAndMarkForFree(AnAction: TLccAction);
var
  i: Integer;
begin
  i := LccActiveActions.IndexOf(AnAction);
  if i > -1 then
  begin
    // Move to the Completed pile to be freed
    LccCompletedActions.Add( LccActiveActions[i]);
    // Take it out of the Active Pile
    {$IFDEF DWSCRIPT}
    LccActiveActions.Remove(i);
    {$ELSE}
    LccActiveActions.Delete(i);
    {$ENDIF}
  end;
end;

{ TLccCanNode }

constructor TLccCanNode.Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string);
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
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 1);
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 2);
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 3);
  SendMessageFunc(Self, WorkerMessage);

  _800msTimer.Enabled := True;  //  Next state is in the event handler to see if anyone objects tor our Alias
end;

procedure TLccCanNode.Logout;
begin
  (NodeManager as INodeManagerCallbacks).DoLogOutNode(Self);
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
      SendMessageFunc(Self, WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 1);
      SendMessageFunc(Self, WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 2);
      SendMessageFunc(Self, WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 3);
      SendMessageFunc(Self, WorkerMessage);
    end else
    begin
      FPermitted := True;
      WorkerMessage.LoadRID(NodeID, AliasID);
      SendMessageFunc(Self, WorkerMessage);
      WorkerMessage.LoadAMD(NodeID, AliasID);
      SendMessageFunc(Self, WorkerMessage);
      (NodeManager as INodeManagerCallbacks).DoAliasIDChanged(Self);
      inherited Login(NodeID);
    end
  end;
  if Permitted then
    inherited On_800msTimer(Sender);
end;

function TLccCanNode.ProcessMessage(SourceMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
begin
  Result := False;

  // Check for a message with the Alias equal to our own.
  if (AliasID <> 0) and (SourceMessage.CAN.SourceAlias = AliasID) then
  begin
    // Check if it is a Check ID message for a node trying to use our Alias and if so tell them no.
    if ((SourceMessage.CAN.MTI and $0F000000) >= MTI_CAN_CID6) and ((SourceMessage.CAN.MTI and $0F000000) <= MTI_CAN_CID0) then
    begin
      WorkerMessage.LoadRID(NodeID, AliasID);                   // sorry charlie this is mine
      SendMessageFunc(Self, WorkerMessage);
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
    if SourceMessage.CAN.SourceAlias = AliasID then
      DuplicateAliasDetected := True;
  end else
  begin
    // Normal message loop once successfully allocating an Alias

    TestNodeID[0] := 0;
    TestNodeID[1] := 0;
    if SourceMessage.IsCAN then
    begin
      case SourceMessage.CAN.MTI of
        MTI_CAN_AME :          // Alias Map Enquiry
          begin
            if SourceMessage.DataCount = 6 then
            begin
              SourceMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
              if EqualNodeID(TestNodeID, NodeID, False) then
              begin
                WorkerMessage.LoadAMD(NodeID, AliasID);
                SendMessageFunc(Self, WorkerMessage);
              end
            end else
            begin
              WorkerMessage.LoadAMD(NodeID, AliasID);
              SendMessageFunc(Self, WorkerMessage);
            end;
            Result := True;
          end;
        MTI_CAN_AMR : InProcessMessageFlushBySourceAlias(SourceMessage); // If the Alias is being reset flush all messages associated with it
        MTI_CAN_AMD : InProcessMessageFlushBySourceAlias(SourceMessage); // If the Alias now coming on line, any old messages for this Alias are out dated
      end
    end;
    if not Result then
      Result := inherited ProcessMessage(SourceMessage);
  end;
end;

procedure TLccCanNode.SendGlobalAME;
begin
  if Permitted then
  begin
    WorkerMessage.LoadAME(NodeID, AliasID, NULL_NODE_ID);
    SendMessageFunc(Self, WorkerMessage);
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
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 1);
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 2);
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 3);
  SendMessageFunc(Self, WorkerMessage);

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
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccCanNode.SendAMR;
begin
  if Permitted then
  begin
    FPermitted := False;
    WorkerMessage.LoadAMR(NodeID, AliasID);
    SendMessageFunc(Self, WorkerMessage);
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

constructor TLccNode.Create(ASendMessageFunc: TOnMessageEvent;
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

  if CdiXML = '' then
    CdiXML := GetCdiFile;

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

  FLccActions := TLccActionHub.Create(Self, ASendMessageFunc);

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

procedure TLccNode.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;
  ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := True;
  ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := True;
  ProtocolSupportedProtocols.TractionFunctionConfiguration := True;

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_TRACTION_FDI, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_TRACTION_FUNCTION_CONFIG, True, False, True, 0, $FFFFFFFF);

  ProtocolMemoryOptions.WriteUnderMask := True;
  ProtocolMemoryOptions.UnAlignedReads := True;
  ProtocolMemoryOptions.UnAlignedWrites := True;
  ProtocolMemoryOptions.SupportACDIMfgRead := True;
  ProtocolMemoryOptions.SupportACDIUserRead := True;
  ProtocolMemoryOptions.SupportACDIUserWrite := True;
  ProtocolMemoryOptions.WriteLenOneByte := True;
  ProtocolMemoryOptions.WriteLenTwoBytes := True;
  ProtocolMemoryOptions.WriteLenFourBytes := True;
  ProtocolMemoryOptions.WriteLenSixyFourBytes := True;
  ProtocolMemoryOptions.WriteArbitraryBytes := True;
  ProtocolMemoryOptions.WriteStream := False;
  ProtocolMemoryOptions.HighSpace := MSI_CDI;
  ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;

  // Create a few events for fun
  ProtocolEventConsumed.AutoGenerate.Count := 5;
  ProtocolEventConsumed.AutoGenerate.StartIndex := 0;
  ProtocolEventsProduced.AutoGenerate.Count := 5;
  ProtocolEventsProduced.AutoGenerate.StartIndex := 0;
end;

procedure TLccNode.CreateNodeID(var Seed: TNodeID);
begin
  Seed[1] := StrToInt('0x020112');
  {$IFDEF DWSCRIPT}
  Seed[0] := RandomInt($FFFFFF);
  {$ELSE}
  Seed[0] := Random($FFFFFF);
  {$ENDIF}
  (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);
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
  FLccActions.Free;
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

function TLccNode.GetCdiFile: string;
begin
  Result := CDI_XML;
end;

procedure TLccNode.Login(ANodeID: TNodeID);
begin
  BeforeLogin;
  if NullNodeID(ANodeID) then
    CreateNodeID(ANodeID);
  FNodeID := ANodeID;
  (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);
  FInitialized := True;
  SendInitializeComplete;
  AutoGenerateEvents;
  SendEvents;
  (NodeManager as INodeManagerCallbacks).DoLogInNode(Self);
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
  LccActions.TimeTick;
end;

function TLccNode.ProcessMessage(SourceMessage: TLccMessage): Boolean;
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
  if EqualNodeID(NodeID, SourceMessage.SourceID, False) then
  begin
    Logout;
    Exit;
  end;

  LccActions.ProcessMessage(SourceMessage);


  // Next look to see if it is an addressed message and if not for use just exit


  if SourceMessage.HasDestination then
  begin
    if not IsDestinationEqual(SourceMessage) then
      Exit;
  end;

  case SourceMessage.MTI of
    MTI_OPTIONAL_INTERACTION_REJECTED :
        begin
          // TODO need a call back handler
        end;

    // *************************************************************************
    // *************************************************************************
    MTI_VERIFY_NODE_ID_NUMBER      :
        begin
          if SourceMessage.DataCount = 6 then
          begin
            SourceMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
            if EqualNodeID(TestNodeID, NodeID, False) then
            begin
              WorkerMessage.LoadVerifiedNodeID(NodeID, GetAlias);
              SendMessageFunc(Self, WorkerMessage);
            end
          end else
          begin
            WorkerMessage.LoadVerifiedNodeID(NodeID, GetAlias);
            SendMessageFunc(Self, WorkerMessage);
          end;
          Result := True;
        end;
    MTI_VERIFY_NODE_ID_NUMBER_DEST :
        begin
          WorkerMessage.LoadVerifiedNodeID(NodeID, GetAlias);
          SendMessageFunc(Self, WorkerMessage);
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
          WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ProtocolSimpleNodeInfo.PackedFormat(StreamManufacturerData, StreamConfig));
          SendMessageFunc(Self, WorkerMessage);
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
          WorkerMessage.LoadProtocolIdentifyReply(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ProtocolSupportedProtocols.EncodeFlags);
          SendMessageFunc(Self, WorkerMessage);
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
           (NodeManager as INodeManagerCallbacks).DoProducerIdentify(Self, SourceMessage, DoDefault);
          if DoDefault then
          begin
            Temp := SourceMessage.ExtractDataBytesAsEventID(0);
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
           (NodeManager as INodeManagerCallbacks).DoProducerIdentify(Self, SourceMessage, DoDefault);
          if DoDefault then
          begin
            Temp := SourceMessage.ExtractDataBytesAsEventID(0);
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
          Temp := SourceMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage, Temp, evs_InValid);
        end;
     MTI_CONSUMER_IDENTIFIED_SET :
        begin
         Temp := SourceMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage, Temp, evs_Valid);
        end;
     MTI_CONSUMER_IDENTIFIED_UNKNOWN :
        begin
          Temp := SourceMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage, Temp, evs_Unknown);
        end;
     MTI_PRODUCER_IDENTIFIED_CLEAR :
        begin
          Temp := SourceMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage, Temp, evs_inValid);
        end;
     MTI_PRODUCER_IDENTIFIED_SET :
        begin
          Temp := SourceMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage, Temp, evs_Valid);
        end;
     MTI_PRODUCER_IDENTIFIED_UNKNOWN :
        begin
          Temp := SourceMessage.ExtractDataBytesAsEventID(0);
          (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage, Temp, evs_Unknown);
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
          TractionCode := SourceMessage.DataArrayIndexer[0];
          case TractionCode of
            TRACTION_SPEED_DIR :
               begin
                // ProtocolTraction.SetSpeedDir(SourceMessage);
                 (NodeManager as INodeManagerCallbacks).DoTractionSpeedSet(Self, SourceMessage, False);
               end;
             TRACTION_FUNCTION :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionFunctionSet(Self, SourceMessage, False);
               end;
             TRACTION_E_STOP :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionEmergencyStop(Self, SourceMessage, False);
               end;
             TRACTION_QUERY_SPEED :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionQuerySpeed(Self, SourceMessage, False);
               end;
             TRACTION_QUERY_FUNCTION :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionQueryFunction(Self, SourceMessage, False);
               end;
             TRACTION_CONTROLLER_CONFIG :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionControllerConfig(Self, SourceMessage, False);
               end;
             TRACTION_LISTENER :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionListenerConfig(Self, SourceMessage, False);
               end;
             TRACTION_MANAGE :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionManage(Self, SourceMessage, False);
               end;
          end;
          Result := True;
        end;
    MTI_TRACTION_REPLY :
        begin
          TractionCode := SourceMessage.DataArrayIndexer[0];
          case TractionCode of
            TRACTION_QUERY_SPEED :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionQuerySpeed(Self, SourceMessage, True);
               end;
             TRACTION_QUERY_FUNCTION :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionQueryFunction(Self, SourceMessage, True);
               end;
             TRACTION_CONTROLLER_CONFIG :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionControllerConfig(Self, SourceMessage, True);
               end;
             TRACTION_LISTENER :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionListenerConfig(Self, SourceMessage, True);
               end;
             TRACTION_MANAGE :
               begin
                 (NodeManager as INodeManagerCallbacks).DoTractionManage(Self, SourceMessage, True);
               end;
          end;
          Result := True;
        end;

    // *************************************************************************
    // Datagram Messages
    // *************************************************************************
     MTI_DATAGRAM_REJECTED_REPLY :
       begin
         DatagramResendQueue.Resend(SourceMessage);
       end;
     MTI_DATAGRAM_OK_REPLY :
       begin
         DatagramResendQueue.Remove(SourceMessage);
       end;
     MTI_DATAGRAM :
       begin
         case SourceMessage.DataArrayIndexer[0] of
           DATAGRAM_PROTOCOL_CONFIGURATION :     {0x20}
             begin
               AddressSpace := 0;

               // Figure out where the Memory space to work on is located, encoded in the header or in the first databyte slot.
               case SourceMessage.DataArrayIndexer[1] and $03 of
                 MCP_NONE          : AddressSpace := SourceMessage.DataArrayIndexer[6];
                 MCP_CDI           : AddressSpace := MSI_CDI;
                 MCP_ALL           : AddressSpace := MSI_ALL;
                 MCP_CONFIGURATION : AddressSpace := MSI_CONFIG;
               end;

               case SourceMessage.DataArrayIndexer[1] and $F0 of
                 MCP_WRITE :
                   begin
                     case AddressSpace of
                       MSI_CDI       : begin end; // Can't write to the CDI
                       MSI_ALL       : begin end; // Can't write to the program area
                       MSI_CONFIG    :            // Needs access to the Configuration Memory Information
                         begin
                           SendDatagramAckReply(SourceMessage, False, 0);     // We will be sending a Write Reply
                           ProtocolMemoryConfiguration.DatagramWriteRequest(SourceMessage, StreamConfig);
                           Result := True;
                         end;
                       MSI_ACDI_MFG  : begin end; // Can't write to the Manufacturers area
                       MSI_ACDI_USER :            // Needs access to the Configuration Memory Information
                         begin
                           SendDatagramAckReply(SourceMessage, False, 0);     // We will be sending a Write Reply
                           ACDIUser.DatagramWriteRequest(SourceMessage, StreamConfig);
                           Result := True;
                         end;
                       MSI_TRACTION_FDI       : begin end; // Can't write to the FDI area
                       MSI_TRACTION_FUNCTION_CONFIG :
                         begin
                           SendDatagramAckReply(SourceMessage, False, 0);     // We will be sending a Write Reply
                           ProtocolMemoryConfiguration.DatagramWriteRequest(SourceMessage, StreamTractionConfig);
                           Result := True;
                         end;
                     end;
                   end;
                 MCP_READ :
                   begin
                     case AddressSpace of
                       MSI_CDI :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ProtocolConfigurationDefinitionInfo.DatagramReadRequest(SourceMessage, WorkerMessage, StreamCdi);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_ALL       :
                           begin  // Can't read from the program area
                             SendDatagramAckReply(SourceMessage, False, 0);
                           end;
                       MSI_CONFIG :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ProtocolMemoryConfiguration.DatagramReadRequest(SourceMessage, WorkerMessage, StreamConfig);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_ACDI_MFG :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ACDIMfg.DatagramReadRequest(SourceMessage, WorkerMessage, StreamManufacturerData);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_ACDI_USER :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ACDIUser.DatagramReadRequest(SourceMessage, WorkerMessage, StreamConfig);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_TRACTION_FDI :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ProtocolConfigurationDefinitionInfo.DatagramReadRequest(SourceMessage, WorkerMessage, StreamTractionFdi);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_TRACTION_FUNCTION_CONFIG :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ProtocolMemoryConfiguration.DatagramReadRequest(SourceMessage, WorkerMessage, StreamTractionConfig);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
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
                     OperationType := SourceMessage.DataArrayIndexer[1];
                     case OperationType of
                       MCP_OP_GET_CONFIG :
                           begin
                             WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceMessage.SourceID,
                                                        SourceMessage.CAN.SourceAlias);
                             ProtocolMemoryOptions.LoadReply(WorkerMessage);
                             SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                             Result := True;
                           end;
                       MCP_OP_GET_ADD_SPACE_INFO :
                           begin
                             WorkerMessage.LoadDatagram(NodeID, GetAlias, SourceMessage.SourceID,
                                                        SourceMessage.CAN.SourceAlias);
                             ProtocolMemoryInfo.LoadReply(SourceMessage, WorkerMessage);
                             SendDatagramRequiredReply(SourceMessage, WorkerMessage);
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
             WorkerMessage.LoadDatagramRejected(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ERROR_PERMANENT or ERROR_NOT_IMPLEMENTED or ERROR_TYPE);
             SendMessageFunc(Self, WorkerMessage);
             Result := True;
           end;
         end;  // case
       end;
  else begin
      if SourceMessage.HasDestination then
      begin
        WorkerMessage.LoadOptionalInteractionRejected(NodeID, GetAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ERROR_PERMANENT or ERROR_NOT_IMPLEMENTED or ERROR_MTI, SourceMessage.MTI);
        SendMessageFunc(Self, WorkerMessage);
        Result := True;
      end;
    end;
  end; // case
end;

procedure TLccNode.SendDatagramAckReply(SourceMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  // Only Ack if we accept the datagram
  WorkerMessageDatagram.LoadDatagramAck(NodeID, GetAlias,
                                        SourceMessage.SourceID, SourceMessage.CAN.SourceAlias,
                                        True, ReplyPending, TimeOutValueN);
  SendMessageFunc(Self, WorkerMessageDatagram);
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
    SendMessageFunc(Self, WorkerMessage);
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
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccNode.SendDatagramRejectedReply(SourceMessage: TLccMessage; Reason: Word);
begin
  WorkerMessageDatagram.LoadDatagramRejected(NodeID, GetAlias,
                                             SourceMessage.SourceID, SourceMessage.CAN.SourceAlias,
                                             Reason);
  SendMessageFunc(Self, WorkerMessageDatagram);
end;

procedure TLccNode.SendDatagramRequiredReply(SourceMessage, ReplyLccMessage: TLccMessage);
begin
  if DatagramResendQueue.Add(ReplyLccMessage.Clone) then     // Waiting for an ACK
  begin
    SendDatagramAckReply(SourceMessage, False, 0);   // We will be sending a Read Reply
    SendMessageFunc(Self, ReplyLccMessage);
  end else
    SendDatagramRejectedReply(SourceMessage, ERROR_TEMPORARY or ERROR_BUFFER_UNAVAILABLE)
end;

procedure TLccNode.SendEvents;
begin
  SendConsumedEvents;
  SendProducedEvents;
end;

procedure TLccNode.SendInitializeComplete;
begin
  WorkerMessage.LoadInitializationComplete(NodeID, GetAlias);
  SendMessageFunc(Self, WorkerMessage);
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
    SendMessageFunc(Self, WorkerMessage);
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
    SendMessageFunc(Self, WorkerMessage);
  end;
end;


initialization
  {$IFNDEF DWSCRIPT}
  Randomize;
  {$ENDIF}

finalization

end.

