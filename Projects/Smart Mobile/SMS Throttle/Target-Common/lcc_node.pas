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
      {$IFNDEF FPC_CONSOLE_APP}
        ExtCtrls,
      {$ENDIF}
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
  lcc_protocol_base,
  lcc_alias_server,
  lcc_train_server;

const
  ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH = $0001;

  TIMEOUT_TIME = 100; // milli seconds
  TIMEOUT_CONTROLLER_NOTIFY_WAIT = 5000;  // 5 seconds
  TIMEOUT_CONTROLLER_RESERVE_WAIT = 5000;
  TIMEOUT_NODE_VERIFIED_WAIT = 800;       // 800ms
  TIMEOUT_NODE_ALIAS_MAPPING_WAIT = 1000;       // 800ms
  TIMEOUT_CREATE_TRAIN_WAIT = 1000;       // 1000ms
  TIMEOUT_SNIP_REPONSE_WAIT = 500;
  TIMEOUT_LISTENER_ATTACH_TRAIN_WAIT = 5000;       // per listener, will have to map CAN Alias if on CAN so may take a bit...

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
  TLccNode = class;
  TLccAction = class;
  TLccActionHub = class;
  TLccActionTrainInfoList = class;

  TOnActionTimoutExpired = procedure(Sender: TLccAction) of object;
  TOnActionCompleteCallback = procedure(SourceAction: TLccAction) of object;


  { TLccActionTrainSnip }

  TLccActionTrainSnip = class
  private
    FManufacturer: string;
    FOwner: string;
    FRoadName: string;
    FRoadNumber: string;
    FTrainClass: string;
    FTrainName: string;
    FValid: Boolean;
    FVersion: Byte;
  public
    property Manufacturer: string read FManufacturer write FManufacturer;
    property Owner: string read FOwner write FOwner;
    property RoadName: string read FRoadName write FRoadName;
    property TrainName: string read FTrainName write FTrainName;
    property TrainClass: string read FTrainClass write FTrainClass;
    property RoadNumber: string read FRoadNumber write FRoadNumber;
    property Version: Byte read FVersion write FVersion;
    property Valid: Boolean read FValid write FValid;
  end;

  { TLccActionSnipRec }

  TLccActionSnipRec = class
  private
    FHardwareVersion: string;
    FManufacturer: string;
    FModel: string;
    FSoftwareVersion: string;
    FUserDecription: string;
    FUserDescription: string;
    FUserName: string;
    FUserVersion: Byte;
    FValid: Boolean;
    FVersion: Byte;
  public
    property Version: Byte read FVersion write FVersion;
    property Manufacturer: string read FManufacturer write FManufacturer;
    property Model: string read FModel write FModel;
    property HardwareVersion: string read FHardwareVersion write FHardwareVersion;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;
    property UserVersion: Byte read FUserVersion write FUserVersion;
    property UserName: string read FUserName write FUserName;
    property UserDescription: string read FUserDescription write FUserDecription;

    property Valid: Boolean read FValid write FValid;
  end;

  { TLccListenerInfo }

  TLccListenerInfo = class
  private
    FFlags: Byte;
    FCount: Integer;
    FList: TLccActionTrainInfoList;
    FNodeIndex: Byte;
  public
    property NodeIndex: Byte read FNodeIndex write FNodeIndex;
    property Flags: Byte read FFlags write FFlags;
    property Count: Integer read FCount write FCount;

    property List: TLccActionTrainInfoList read FList write FList;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLccActionTrainInfo }

  TLccActionTrainInfo = class
  private
    FAliasID: Word;
    FIsReserved: Boolean;            // The Train has its Reserved flag set for our operation (we reserved it)
    FListener: TLccListenerInfo;
    FNodeID: TNodeID;
    FSearchCriteria: DWORD;          // Search Critera Data
    FSearchCriteriaFound: Boolean;   // This NodeID/AliasID Train responded that it has this Search Criteria
    FSearchCriteriaValid: Boolean;   // The Search Critera Data is valid information and can be used
    FSNIP: TLccActionSnipRec;
    FTrainSNIP: TLccActionTrainSnip;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property AliasID: Word read FAliasID write FAliasID;
    property SearchCriteria: DWORD read FSearchCriteria write FSearchCriteria;
    property SearchCriteriaValid: Boolean read FSearchCriteriaValid write FSearchCriteriaValid;
    property SearchCriteriaFound: Boolean read FSearchCriteriaFound write FSearchCriteriaFound;

    property IsReserved: Boolean read FIsReserved write FIsReserved;
    property TrainSNIP: TLccActionTrainSnip read FTrainSNIP write FTrainSNIP;
    property SNIP: TLccActionSnipRec read FSNIP write FSNIP;
    property Listener: TLccListenerInfo read FListener write FListener;


    constructor Create; virtual;
    destructor Destroy; override;

    function Clone: TLccActionTrainInfo;
  end;

  { TLccActionTrainInfoList }

  TLccActionTrainInfoList = class
  private
    {$IFDEF DELPHI}
    FTrainList: TObjectList<TLccActionTrainInfo>;
    FConsistList: TObjectList<TLccActionTrainInfo>;
    {$ELSE}
    FTrainList: TObjectList;
    FConsistList: TObjectList;
    {$ENDIF}
    function GetConsits(Index: Integer): TLccActionTrainInfo;
    function GetCount: Integer;
    function GetTrains(Index: Integer): TLccActionTrainInfo;
    procedure SetConsits(Index: Integer; AValue: TLccActionTrainInfo);
    procedure SetTrains(Index: Integer; AValue: TLccActionTrainInfo);
  protected
  {$IFDEF DELPHI}
    property TrainList: TObjectList<TLccActionTrainInfo> read FTrainList write FTrainList;
    property ConsistList: TObjectList<TLccActionTrainInfo> read FConsistList write FConsistList;
    {$ELSE}
    property TrainList: TObjectList read FTrainList write FTrainList;
    property ConsistList: TObjectList read FConsistList write FConsistList;
    {$ENDIF}
  public
    property Consits[Index: Integer]: TLccActionTrainInfo read GetConsits write SetConsits;
    property Trains[Index: Integer]: TLccActionTrainInfo read GetTrains write SetTrains; default;
    property Count: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;

    function CreateNew(ANodeID: TNodeID; AnAliasID: Word): TLccActionTrainInfo;
    procedure Add(ATrain: TLccActionTrainInfo);
    function IndexOf(ATrain: TLccActionTrainInfo): Integer;
    procedure Remove(ATrain: TLccActionTrainInfo);
    procedure Clear();
    function MatchingAlias(TestAlias: Word): TLccActionTrainInfo;

    function MatchingSearchCriteria(TestSearchCriteria: DWord): TLccActionTrainInfo;
    function MatchingNodeAndSearchCriteria(TestNodeID: TNodeID; TestAliasID: Word): TLccActionTrainInfo;
  end;


  TLccActionErrorCode = (laecOk, laecTimedOut, laecTerminated, laecNoTrainFound, laecReservedFailed, laecListenerAttachFailed, laecUnableToCreateAliasMapping);

  { TLccAction }

  TLccAction = class(TObject)
  private
    FActionHub: TLccActionHub;
    FActionStateIndex: Integer;
    FAliasMappingAlias: Word;
    FAliasMappingNodeID: TNodeID;
    FErrorCode: TLccActionErrorCode;
    FFreezeTimer: Boolean;
    FOnCompleteCallback: TOnActionCompleteCallback;
    FOnTimeoutExpired: TOnActionTimoutExpired;
    FOwner: TLccNode;
    FSendMessage: TOnMessageEvent;
    FStates: TOnMessageEventArray;
    FTimeoutCounts: Integer;
    FTimeoutCountThreshold: Integer;
    FUniqueID: Integer;
    FWaitingForFree: Boolean;
    FWorkerMessage: TLccMessage;
  protected
    FSourceAliasID: Word;
    FSourceNodeID: TNodeID;
    FDestAliasID: Word;
    FDestNodeID: TNodeID;

    property AliasMappingAlias: Word read FAliasMappingAlias write FAliasMappingAlias;
    property AliasMappingNodeID: TNodeID read FAliasMappingNodeID write FAliasMappingNodeID;
    property ActionHub: TLccActionHub read FActionHub write FActionHub;
    property ActionStateIndex: Integer read FActionStateIndex write FActionStateIndex;
    property States: TOnMessageEventArray read FStates write FStates;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property TimeoutCounts: Integer read FTimeoutCounts write FTimeoutCounts;
    property TimeoutCountThreshold: Integer read FTimeoutCountThreshold write FTimeoutCountThreshold;
    property OnCompleteCallback: TOnActionCompleteCallback read FOnCompleteCallback write FOnCompleteCallback;
    property WaitingForFree: Boolean read FWaitingForFree write FWaitingForFree;

    // This message never gets a timer tick so never try to create a wait state here.  The ActionHub calls this off in its KickOff call
    // Any state after this can be called from a nodes ProcessMessage loop, with that SourceMessage, or from the Timer with nil
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; virtual;
    function _NFinalStateCleanup(Sender: TObject; SourceMessage: TLccMessage): Boolean; virtual;

    procedure LoadStateArray; virtual;   // Assign your state functions to the array to get called in order
    function ProcessMessage(SourceMessage: TLccMessage): Boolean; virtual;
    procedure TimeTick; virtual;    // 800ms Clock

    procedure CompleteCallback(SourceAction: TLccAction); virtual; // override to do something
    procedure DoTimeoutExpired; virtual;
    procedure UnRegisterSelf;
    function ValidateAliasMapping(AnAliasToMap: Word; SendAME: Boolean): TLccAliasMapping;
    function ValidateAliasMappingWait: TLccAliasMapping;
    function ValidateNodeIDMapping(ANodeIDToMap: TNodeID; SendVerify: Boolean): TLccAliasMapping;
    function ValidateNodeIDMappingWait(ASourceMessage: TLccMessage): TLccAliasMapping;

  public
   //  property Cancel: Boolean read FCancel write FCancel;

    property SourceAliasID: Word read FSourceAliasID;
    property SourceNodeID: TNodeID read FSourceNodeID;
    property DestAliasID: Word read FDestAliasID;
    property DestNodeID: TNodeID read FDestNodeID;


    property Owner: TLccNode read FOwner;

    property SendMessage: TOnMessageEvent read FSendMessage write FSendMessage;

    // Disables the timer from running any code.  Does not physically disable the timer but just short circuits TimeTick
    property FreezeTimer: Boolean read FFreezeTimer write FFreezeTimer;
   // Check to see if the timer that was set previousely through SetTimoutCountThreshold
    property OnTimeoutExpired: TOnActionTimoutExpired read FOnTimeoutExpired write FOnTimeoutExpired;
    // User property that can be used to flag the statemachine should skip through states to the end
    property ErrorCode: TLccActionErrorCode read FErrorCode write FErrorCode;
    // Unique ID to allow users to identify which Action is in callbacks.
    property UniqueID: Integer read FUniqueID;

    constructor Create(AnOwner: TLccNode; ASourceNodeID: TNodeID; ASourceAliasID: Word; ADestNodeID: TNodeID; ADestAliasID: Word; AnUniqueID: Integer); virtual;
    destructor Destroy; override;

    // Set the index to the last exit function that will be pointed to in the States array
    function AdvanceToLastState: Integer;
    // Set the index to the next function that will be pointed to in the States array
    function AdvanceToNextState(JumpCount: Integer = 1): Integer;
    // Each decendant must set the number of state functions it implements
    procedure SetStateArrayLength(NewLength: Integer);
    // Sets the number of ms before any wait state is declared hung or complete
    procedure SetTimoutCountThreshold(NewThreshold_ms: Integer; ResetCounter: Boolean = True);  // 100ms counts
    // set the timer counter that starts counting toward the CountThreshold
    procedure ResetTimeoutCounter;
    // Compares CountThreshold and Counter to see if the timeout timer has expired
    function TimeoutExpired: Boolean;
    // Sets the OnCompleteCallback property to the CompleteCallack method of the passed function
    procedure RegisterCallBackOnExit(AnLccAction: TLccAction);
  end;


  { TLccActionTrain }

  TLccActionTrain = class(TLccAction)
  private
    FReleaseTrain: Boolean;
    FTrains: TLccActionTrainInfoList;
    function GetTrain: TLccActionTrainInfo;
  public
    // We may not want to Release the train lock if we are Assigning and Creating a Consist.  Must mainually release the trains when done
    property ReleaseTrain: Boolean read FReleaseTrain write FReleaseTrain;
    // User property hold trains nodes that are being working on in the task
    property Trains: TLccActionTrainInfoList read FTrains write FTrains;
    property Train: TLccActionTrainInfo read GetTrain;

    constructor Create(AnOwner: TLccNode; ASourceNodeID: TNodeID; ASourceAliasID: Word; ADestNodeID: TNodeID; ADestAliasID: Word; AnUniqueID: Integer); override;
    destructor Destroy; override;
  end;

 { TLccActionHub }

  TLccActionHub = class(TObject)
  private
    FActionInSendMessage: TLccAction;
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
    property ActionInSendMessage: TLccAction read FActionInSendMessage write FActionInSendMessage;
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
    function RegisterAndKickOffAction(AnAction: TLccAction; KickOffMessage: TLccMessage): Boolean;
    procedure UnregisterActionAndMarkForFree(AnAction: TLccAction);
  end;


  { TLccNode }

  TLccNode = class(TObject)
  private
    FAliasID: Word;
    FAliasServer: TLccAliasServer;
    FDuplicateAliasDetected: Boolean;
    FGridConnect: Boolean;
    FLccActions: TLccActionHub;
    FLoginTimoutCounter: Integer;
    FPermitted: Boolean;
    FSeedNodeID: TNodeID;
    FTrainServer: TLccTrainServer;
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
    F_100msTimer: TLccTimer;

    function GetAliasIDStr: String;
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
    property _100msTimer: TLccTimer read F_100msTimer write F_100msTimer;

    // GridConnect Helpers
    property DuplicateAliasDetected: Boolean read FDuplicateAliasDetected write FDuplicateAliasDetected;
    property SeedNodeID: TNodeID read FSeedNodeID write FSeedNodeID;
    property LoginTimoutCounter: Integer read FLoginTimoutCounter write FLoginTimoutCounter;

    procedure CreateNodeID(var Seed: TNodeID);
    function FindCdiElement(TestXML, Element: string; var Offset: Integer; var ALength: Integer): Boolean;
    function LoadManufacturerDataStream(ACdi: string): Boolean;
    procedure AutoGenerateEvents;
    procedure SendDatagramAckReply(SourceMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
    procedure SendDatagramRejectedReply(SourceMessage: TLccMessage; Reason: Word);
    procedure SendDatagramRequiredReply(SourceMessage, ReplyLccMessage: TLccMessage);
    procedure On_100msTimer(Sender: TObject);  virtual;
    function GetCdiFile: string; virtual;
    procedure BeforeLogin; virtual;
    procedure LogInLCC(ANodeID: TNodeID);

    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;
    function ProcessMessageGridConnect(SourceMessage: TLccMessage): Boolean;

    // GridConnect Helpers
    function GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
    procedure GenerateNewSeed(var Seed: TNodeID);
    procedure Relogin;

  public
    property AliasServer: TLccAliasServer read FAliasServer;
    property TrainServer: TLccTrainServer read FTrainServer;
    property DatagramResendQueue: TDatagramQueue read FDatagramResendQueue;
    property GridConnect: Boolean read FGridConnect;
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr: String read GetNodeIDStr;
    property Initialized: Boolean read FInitialized;
    property LccActions: TLccActionHub read FLccActions write FLccActions;
    property SendMessageFunc: TOnMessageEvent read FSendMessageFunc;

    // GridConnect Helpers
    property AliasID: Word read FAliasID;
    property AliasIDStr: String read GetAliasIDStr;
    property Permitted: Boolean read FPermitted;

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

    constructor Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); virtual;
    destructor Destroy; override;

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


var
  InprocessMessageAllocated: Integer = 0;
  ActionObjectsAllocated: Integer = 0;

implementation

uses
  lcc_node_manager;

{ TLccListenerInfo }

constructor TLccListenerInfo.Create;
begin
  FList := TLccActionTrainInfoList.Create;
end;

destructor TLccListenerInfo.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

{ TLccActionTrainInfo }

constructor TLccActionTrainInfo.Create;
begin
  inherited Create;
  FTrainSNIP := TLccActionTrainSnip.Create;
  FSNIP := TLccActionSnipRec.Create;
  FListener := TLccListenerInfo.Create;
end;

destructor TLccActionTrainInfo.Destroy;
begin
  FreeAndNil(FTrainSNIP);
  FreeAndNil(FSNIP);
  FreeAndNil(FListener);
  inherited Destroy;
end;

function TLccActionTrainInfo.Clone: TLccActionTrainInfo;

   procedure BuildListeners(TargetListenerList, SourceListenerList: TLccActionTrainInfoList);
   var
     i: Integer;
     LocalListener, NewListener: TLccListenerInfo;
   begin
     for i := 0 to SourceListenerList.Count - 1 do
     begin
        NewListener := TLccListenerInfo.Create;
        LocalListener := (SourceListenerList[i].Listener as TLccListenerInfo);

        NewListener.Count := LocalListener.Count;
        NewListener.Flags := LocalListener.Flags;
        NewListener.NodeIndex := LocalListener.NodeIndex;
     end;
   end;

begin
  Result := TLccActionTrainInfo.Create;
  Result.NodeID := NodeID;
  Result.AliasID := AliasID;
  Result.SearchCriteria := SearchCriteria;
  Result.SearchCriteriaValid := SearchCriteriaValid;
  Result.SearchCriteriaFound := SearchCriteriaFound;

  Result.TrainSNIP.Manufacturer := TrainSNIP.Manufacturer;
  Result.TrainSNIP.Owner := TrainSNIP.Owner;
  Result.TrainSNIP.RoadName := TrainSNIP.RoadName;
  Result.TrainSNIP.TrainName := TrainSNIP.TrainName;
  Result.TrainSNIP.TrainClass := TrainSNIP.TrainClass;
  Result.TrainSNIP.RoadNumber := TrainSNIP.RoadNumber;
  Result.TrainSNIP.Version := TrainSNIP.Version;
  Result.TrainSNIP.Valid := TrainSNIP.Valid;

  Result.SNIP.Version := SNIP.Version;
  Result.SNIP.Manufacturer := SNIP.Manufacturer;
  Result.SNIP.Model := SNIP.Model;
  Result.SNIP.HardwareVersion := SNIP.HardwareVersion;
  Result.SNIP.SoftwareVersion := SNIP.SoftwareVersion;
  Result.SNIP.UserVersion := SNIP.UserVersion;
  Result.SNIP.UserName := SNIP.UserName;
  Result.SNIP.UserDescription := SNIP.UserDescription;

  Result.SNIP.Valid := SNIP.Valid;

  Result.Listener.Count := Listener.Count;
  Result.Listener.NodeIndex := Listener.NodeIndex;
  Result.Listener.Flags := Listener.Flags;

  if Listener.List.Count > 1 then
    BuildListeners(Result.Listener.List, Listener.List);

  Result.IsReserved := IsReserved;
end;

{ TLccActionTrain }

function TLccActionTrain.GetTrain: TLccActionTrainInfo;
begin
  Result := nil;
  if Trains.Count > 0 then
    Result := Trains[0];
end;

constructor TLccActionTrain.Create(AnOwner: TLccNode; ASourceNodeID: TNodeID;
  ASourceAliasID: Word; ADestNodeID: TNodeID; ADestAliasID: Word;
  AnUniqueID: Integer);
begin
  inherited Create(AnOwner, ASourceNodeID, ASourceAliasID, ADestNodeID, ADestAliasID, AnUniqueID);
  FTrains := TLccActionTrainInfoList.Create;
  FReleaseTrain := True;
end;

destructor TLccActionTrain.Destroy;
begin
  FreeAndNil(FTrains);
  inherited Destroy;
end;

{ TLccActionTrainInfoList }

function TLccActionTrainInfoList.GetTrains(Index: Integer): TLccActionTrainInfo;
begin
  Result := TrainList[Index] as TLccActionTrainInfo;
end;

procedure TLccActionTrainInfoList.SetConsits(Index: Integer;
  AValue: TLccActionTrainInfo);
begin
  ConsistList[Index] := AValue;
end;

function TLccActionTrainInfoList.GetCount: Integer;
begin
  Result := TrainList.Count;
end;

function TLccActionTrainInfoList.GetConsits(Index: Integer): TLccActionTrainInfo;
begin
  Result := ConsistList[Index] as TLccActionTrainInfo;
end;

procedure TLccActionTrainInfoList.SetTrains(Index: Integer; AValue: TLccActionTrainInfo);
begin
  TrainList[Index] := AValue;
end;

constructor TLccActionTrainInfoList.Create;
begin
  {$IFDEF DELPHI}
    FTrainList := TObjectList<TLccActionTrainInfo>.Create(False);
    FConsistList := TObjectList<TLccActionTrainInfo>.Create(False);
  {$ELSE}
    FTrainList := TObjectList.Create;
    FConsistList := TObjectList.Create;
    {$IFNDEF DWSCRIPT}
      TrainList.OwnsObjects := True;
      ConsistList.OwnsObjects := True;
    {$ENDIF}
  {$ENDIF}
end;

destructor TLccActionTrainInfoList.Destroy;
begin
  FreeAndNil(FTrainList);
  FreeAndNil(FConsistList);
  inherited Destroy;
end;

function TLccActionTrainInfoList.CreateNew(ANodeID: TNodeID; AnAliasID: Word): TLccActionTrainInfo;
begin
  Result := TLccActionTrainInfo.Create;
  Add(Result);
  Result.NodeID := ANodeID;
  Result.AliasID := AnAliasID;
end;

procedure TLccActionTrainInfoList.Add(ATrain: TLccActionTrainInfo);
begin
  TrainList.Add(ATrain);
end;

function TLccActionTrainInfoList.IndexOf(ATrain: TLccActionTrainInfo): Integer;
begin
  Result := TrainList.IndexOf(ATrain);
end;

procedure TLccActionTrainInfoList.Remove(ATrain: TLccActionTrainInfo);
{$IFDEF DWSCRIPT}
 var
   i: Integer;
{$ENDIF}
begin
  {$IFDEF DWSCRIPT}
  i := TrainList.IndexOf(ATrain);
  if i > -1 then
    TrainList.Remove(i);
  {$ELSE}
  TrainList.Remove(ATrain);
  {$ENDIF}
end;

procedure TLccActionTrainInfoList.Clear();
begin
  TrainList.Clear;
end;

function TLccActionTrainInfoList.MatchingAlias(TestAlias: Word): TLccActionTrainInfo;
var
  i: Integer;
  LocalTrain: TLccActionTrainInfo;
begin
  Result := nil;
  i := 0;
  while i < TrainList.Count do
  begin
    LocalTrain := Trains[i];
    if LocalTrain.AliasID = TestAlias then
    begin
      Result := LocalTrain;
      i := TrainList.Count;
    end;
    Inc(i);
  end;

end;

function TLccActionTrainInfoList.MatchingSearchCriteria(TestSearchCriteria: DWord): TLccActionTrainInfo;
var
  i: Integer;
  LocalTrain: TLccActionTrainInfo;
begin
  Result := nil;
  i := 0;
  while i < TrainList.Count do
  begin
    LocalTrain := Trains[i];
    if LocalTrain.SearchCriteriaValid then
      if LocalTrain.SearchCriteria = TestSearchCriteria then
      begin
        Result := LocalTrain;
        i := TrainList.Count;
      end;
    Inc(i);
  end;
end;

function TLccActionTrainInfoList.MatchingNodeAndSearchCriteria(TestNodeID: TNodeID;TestAliasID: Word): TLccActionTrainInfo;
var
  i: Integer;
  LocalTrain: TLccActionTrainInfo;
begin
  Result := nil;
  i := 0;
  while i < TrainList.Count do
  begin
    LocalTrain := Trains[i];
    if EqualNode(LocalTrain.NodeID, LocalTrain.AliasID, TestNodeID, TestAliasID, True) then
    begin
      Result := LocalTrain;
      i := TrainList.Count;
    end;
    Inc(i);
  end;
end;

{ TLccAction }

constructor TLccAction.Create(AnOwner: TLccNode; ASourceNodeID: TNodeID;
  ASourceAliasID: Word; ADestNodeID: TNodeID; ADestAliasID: Word;
  AnUniqueID: Integer);
begin
  Inc(ActionObjectsAllocated);
  FUniqueID := AnUniqueID;
  FOwner := AnOwner;;
  WorkerMessage := TLccMessage.Create;
  FSourceNodeID := ASourceNodeID;
  FSourceAliasID := ASourceAliasID;
  FDestNodeID := ADestNodeID;
  FDestAliasID := ADestAliasID;
  FSendMessage := AnOwner.SendMessageFunc;
  SetTimoutCountThreshold(5000);  // Default 5 seconds
  LoadStateArray;
end;

function TLccAction.AdvanceToNextState(JumpCount: Integer): Integer;
begin
  Inc(FActionStateIndex, JumpCount);
  if (FActionStateIndex > Length(States) - 1) or (FActionStateIndex < 0) then
    FActionStateIndex := Length(States) - 1;     // Error just run the last state to exit
  Result := FActionStateIndex;
end;

procedure TLccAction.CompleteCallback(SourceAction: TLccAction);
begin
  FreezeTimer := True;
  try
    if Assigned(SourceAction) then
      OnCompleteCallback := {$IFNDEF DELPHI}@{$ENDIF}CompleteCallback;
  finally
    FreezeTimer := False;
  end;
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

function TLccAction.AdvanceToLastState: Integer;
begin
  FActionStateIndex := Length(States) - 1;     // Error just run the last state to exit
  Result := FActionStateIndex;
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
end;

function TLccAction._NFinalStateCleanup(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  if not WaitingForFree then  // Only allow this to be called once, may get called after the first time through
  begin
 //   SourceMessage := SourceMessage;
    FActionStateIndex := Length(FStates) - 1;
    if Assigned(OnCompleteCallback) then
      OnCompleteCallback(Self);
    UnRegisterSelf;
    WaitingForFree := True;
  end;
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
  // Never send a TimerTick call to State 0 though.
  if (ActionStateIndex > 0) and (ActionStateIndex < Length(States)) then
    Result := States[ActionStateIndex](Owner, SourceMessage);
end;

procedure TLccAction.RegisterCallBackOnExit(AnLccAction: TLccAction);
begin
  if Assigned(AnLccAction) then
    OnCompleteCallback := {$IFNDEF DELPHI}@{$ENDIF}AnLccAction.CompleteCallback
  else
    OnCompleteCallback := nil;
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
  TimeoutCountThreshold := Trunc(NewThreshold_ms/TIMEOUT_TIME) + 1;
  if ResetCounter then
    TimeoutCounts := 0;
end;

procedure TLccAction.TimeTick;
begin
  if not FreezeTimer then
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

function TLccAction.ValidateAliasMapping(AnAliasToMap: Word; SendAME: Boolean): TLccAliasMapping;
begin
  AliasMappingAlias := AnAliasToMap;
  Result := Owner.AliasServer.FindMapping(AliasMappingAlias);
  if not Assigned(Result) and SendAME then
  begin
    WorkerMessage.LoadAME(SourceNodeID, SourceAliasID, NULL_NODE_ID);
    SendMessage(Owner, WorkerMessage);
    SetTimoutCountThreshold(TIMEOUT_NODE_ALIAS_MAPPING_WAIT, True);
  end;
end;

function TLccAction.ValidateAliasMappingWait: TLccAliasMapping;
begin
  // Only way to get here is if we were GridConnect
  Result := Owner.AliasServer.FindMapping(AliasMappingAlias);
  if not Assigned(Result) then
  begin
    if TimeoutExpired then
    begin
      // Can't get the Alias.
      ErrorCode := laecTimedOut;
      AdvanceToLastState;
    end;
  end;
end;

function TLccAction.ValidateNodeIDMapping(ANodeIDToMap: TNodeID; SendVerify: Boolean): TLccAliasMapping;
begin
  AliasMappingNodeID := ANodeIDToMap;
  Result := Owner.AliasServer.FindMapping(AliasMappingNodeID);
  if not Assigned(Result) and SendVerify then
  begin
    WorkerMessage.LoadVerifyNodeID(SourceNodeID, SourceAliasID, AliasMappingNodeID);
    SendMessage(Owner, WorkerMessage);
    SetTimoutCountThreshold(TIMEOUT_NODE_ALIAS_MAPPING_WAIT, True);
  end;
end;

function TLccAction.ValidateNodeIDMappingWait(ASourceMessage: TLccMessage): TLccAliasMapping;
var
  TempNodeID: TNodeID;
begin
  Result := nil;
  if Assigned(ASourceMessage) then   // Could be time tick calling and we need to wait for a real message
  begin
    case ASourceMessage.MTI of
      MTI_VERIFIED_NODE_ID_NUMBER :
        begin
          TempNodeID := NULL_NODE_ID;
          if EqualNodeID(ASourceMessage.ExtractDataBytesAsNodeID(0, TempNodeID), AliasMappingNodeID, False) then
          begin
            Result := Owner.AliasServer.AddMapping(ASourceMessage.CAN.SourceAlias, AliasMappingNodeID);
          end;
        end;
    end;
  end;

  if not Assigned(Result) then
  begin
    if TimeoutExpired then
    begin
      // Can't get the Alias.
      ErrorCode := laecTimedOut;
      AdvanceToLastState;
    end;
  end;
end;

function TLccAction.TimeoutExpired: Boolean;
begin
  Result := TimeoutCounts > TimeoutCountThreshold ;
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
  begin
    // Do not allow reentrancy from a SendMessage call from an action that could wind up back here
    if LccActiveActions[i] <> ActionInSendMessage then
    begin
      ActionInSendMessage := (LccActiveActions[i] as TLccAction);
      try
        Result := ActionInSendMessage.ProcessMessage(SourceMessage);
      finally
        ActionInSendMessage := nil;
      end;
    end;
  end;
end;

function TLccActionHub.RegisterAndKickOffAction(AnAction: TLccAction; KickOffMessage: TLccMessage): Boolean;
begin
  AnAction.ActionHub := Self;
  LccActiveActions.Add(AnAction);
  Result := AnAction._0ReceiveFirstMessage(AnAction.Owner, KickOffMessage);
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

{TLccNode }

function TLccNode.GetNodeIDStr: String;
begin
  Result := NodeIDToString(NodeID, False);
{ Result := IntToHex(NodeID[1], 6);
 Result := Result + IntToHex(NodeID[0], 6);
 Result := '0x' + Result   }
end;

function TLccNode.GetAliasIDStr: String;
begin
  Result := NodeAliasToString(AliasID);
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

constructor TLccNode.Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean);
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
  FGridConnect := GridConnectLink;
  FAliasServer := TLccAliasServer.Create;
  FTrainServer := TLccTrainServer.Create;

  _100msTimer := TLccTimer.Create(nil);
  _100msTimer.Enabled := False;
  {$IFDEF DWSCRIPT}
  _100msTimer.OnTime := @On_100msTimer;
  _100msTimer.Delay := 100;
  {$ELSE}
  _100msTimer.OnTimer := {$IFNDEF DELPHI}@{$ENDIF}On_100msTimer;
  _100msTimer.Interval := 100;
  {$ENDIF}

  if CdiXML = '' then
    CdiXML := GetCdiFile;

  // Setup the Cdi Stream
  StreamCdi.Size := Int64( Length(CdiXML)) + 1;   // Need the null
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

procedure TLccNode.LogInLCC(ANodeID: TNodeID);
var
  AMapping: TLccAliasMapping;
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

  if GridConnect then
  begin
    AMapping := AliasServer.AddMapping(AliasID, NodeID);
    if Assigned(AMapping) then
      (NodeManager as INodeManagerCallbacks).DoAliasMappingChange(Self, AMapping, True);
  end;
end;

function TLccNode.ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;
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


  // Train Roster Updates ******************************************************
  case SourceMessage.MTI of
    MTI_PRODUCER_IDENTIFIED_CLEAR,
    MTI_PRODUCER_IDENTIFIED_SET,
    MTI_PRODUCER_IDENTIFIED_UNKNOWN :
      begin
        if SourceMessage.IsEqualEventID(EVENT_IS_TRAIN) then
        begin
          // Interesting.... need both?
     //     TrainServer.AddTrainObject();
        end;
      end;
  end;
  // END: Train Roster Updates *************************************************


  // Next look to see if it is an addressed message and if not for use just exit


  if SourceMessage.HasDestination then
  begin
    if not EqualNode(NodeID,  AliasID, SourceMessage.DestID, SourceMessage.CAN.DestAlias, True) then
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
              WorkerMessage.LoadVerifiedNodeID(NodeID, FAliasID);
              SendMessageFunc(Self, WorkerMessage);
            end
          end else
          begin
            WorkerMessage.LoadVerifiedNodeID(NodeID, FAliasID);
            SendMessageFunc(Self, WorkerMessage);
          end;
          Result := True;
        end;
    MTI_VERIFY_NODE_ID_NUMBER_DEST :
        begin
          WorkerMessage.LoadVerifiedNodeID(NodeID, FAliasID);
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
          WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ProtocolSimpleNodeInfo.PackedFormat(StreamManufacturerData, StreamConfig));
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
          WorkerMessage.LoadProtocolIdentifyReply(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ProtocolSupportedProtocols.EncodeFlags);
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
           DATAGRAM_PROTOCOL_LOGREQUEST : {0x01}  // Makes the Python Script Happy
             begin
               SendDatagramAckReply(SourceMessage, False, 0);
             end;
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
                           WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
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
                           WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ProtocolMemoryConfiguration.DatagramReadRequest(SourceMessage, WorkerMessage, StreamConfig);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_ACDI_MFG :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ACDIMfg.DatagramReadRequest(SourceMessage, WorkerMessage, StreamManufacturerData);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_ACDI_USER :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ACDIUser.DatagramReadRequest(SourceMessage, WorkerMessage, StreamConfig);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_TRACTION_FDI :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           ProtocolConfigurationDefinitionInfo.DatagramReadRequest(SourceMessage, WorkerMessage, StreamTractionFdi);
                           SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                           Result := True;
                         end;
                       MSI_TRACTION_FUNCTION_CONFIG :
                         begin
                           WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
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
                             WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID,
                                                        SourceMessage.CAN.SourceAlias);
                             ProtocolMemoryOptions.LoadReply(WorkerMessage);
                             SendDatagramRequiredReply(SourceMessage, WorkerMessage);
                             Result := True;
                           end;
                       MCP_OP_GET_ADD_SPACE_INFO :
                           begin
                             WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID,
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
             WorkerMessage.LoadDatagramRejected(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ERROR_PERMANENT_NOT_IMPLEMENTED or ERROR_TYPE);
             SendMessageFunc(Self, WorkerMessage);
             Result := True;
           end;
         end;  // case
       end;
  else begin
      if SourceMessage.HasDestination then
      begin
        WorkerMessage.LoadOptionalInteractionRejected(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ERROR_PERMANENT_NOT_IMPLEMENTED or ERROR_MTI, SourceMessage.MTI);
        SendMessageFunc(Self, WorkerMessage);
        Result := True;
      end;
    end;
  end; // case
end;

function TLccNode.ProcessMessageGridConnect(SourceMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
  AMapping: TLccAliasMapping;
  DelayedMessage: TLccMessage;
begin
  Result := False;

  // Alias Mapping Updates ******************************************************
  // Keep the Alias Maps up to date regardless of our state
  case SourceMessage.CAN.MTI of
    MTI_CAN_AMR :
      begin   // Alias going away clear it from the cache
        AMapping := AliasServer.RemoveMapping(SourceMessage.CAN.SourceAlias, False);
        try
          (NodeManager as INodeManagerCallbacks).DoAliasMappingChange(Self, AMapping, False);
        finally
          AliasServer.FlushDelayedMessagesByAlias(AMapping.NodeAlias);
          AMapping.Free;
        end;
      end;
    MTI_CAN_AMD :
      begin  // Alias coming on line save a copy of this mapping for future use
        TestNodeID := NULL_NODE_ID;
        SourceMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
        AMapping := AliasServer.AddMapping(SourceMessage.CAN.SourceAlias, TestNodeID);
        if Assigned(AMapping) then
          (NodeManager as INodeManagerCallbacks).DoAliasMappingChange(Self, AMapping, True);

        // Below if we encounted a message with a SourceAlias we did not have a mapping
        // we delayed the message and sent and AMR so this is the reply (or not)
        DelayedMessage := AliasServer.PopDelayedMessageByAlias(SourceMessage.CAN.SourceAlias);
        while Assigned(DelayedMessage) do
        begin
          ProcessMessageLCC(DelayedMessage);
          DelayedMessage.Free;
          DelayedMessage := AliasServer.PopDelayedMessageByAlias(SourceMessage.CAN.SourceAlias);
        end
      end;
  end;
  // END: Alias Mapping Updates ************************************************

  // Alias Allocation, duplicate checking after allocation**********************
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
  // END: Alias Allocation, duplicate checking after allocation******************

  if not Permitted then
  begin
    // We are still trying to allocate a new Alias, someone else is using this alias
    if SourceMessage.CAN.SourceAlias = AliasID then
      DuplicateAliasDetected := True;
  end else
  begin
    // Normal message loop once successfully allocating an Alias

    // Do this after the updates to the Alias Maps so we don't call unnecessarily
    if not SourceMessage.IsCAN then // Don't interfer with CAN log in messages...
      if not Assigned(AliasServer.FindMapping(SourceMessage.CAN.SourceAlias)) then
      begin
        // Sucks to have to make this a global call but once done everyone will be updated.
        // Optimization, don't send more AMEs if we have an Delayed Message for this Alias,
        // already has been done.
        if not AliasServer.HasDelayedMessageByAlias(SourceMessage.CAN.SourceAlias) then
        begin
          WorkerMessage.LoadAME(NodeID, AliasID, NULL_NODE_ID);
          SendMessageFunc(Self, WorkerMessage);
        end;
        AliasServer.AddDelayedMessage(SourceMessage);
        Result := True;    // Wait for the mapping to be valid before processing in the 100ms timer
      end;


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
      end
    end;
    if not Result then
      Result := ProcessMessageLCC(SourceMessage);
  end;
end;

function TLccNode.GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
begin
  Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
end;

procedure TLccNode.GenerateNewSeed(var Seed: TNodeID);
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

procedure TLccNode.Relogin;
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

  LoginTimoutCounter := 0;
  _100msTimer.Enabled := True;  //  Next state is in the event handler to see if anyone objects tor our Alias
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
  _100msTimer.Enabled := False;

  // No reason to check for GridConnect, just do it anyway
  FAliasID := 0;
  (NodeManager as INodeManagerCallbacks).DoAliasIDChanged(Self);

  FNodeID[0] := 0;
  FNodeID[1] := 0;
  (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);

  (NodeManager as INodeManagerCallbacks).DoDestroyLccNode(Self);
  _100msTimer.Free;
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
  FAliasServer.Free;
  FTrainServer.Free;
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

function TLccNode.GetCdiFile: string;
begin
  Result := CDI_XML;
end;

procedure TLccNode.Login(ANodeID: TNodeID);
var
  Temp: TNodeID;
begin
  if GridConnect then
  begin
    BeforeLogin;
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

    LoginTimoutCounter := 0;
    _100msTimer.Enabled := True;  //  Next state is in the event handler to see if anyone objects tor our Alias
  end else
    LoginLCC(ANodeID);
end;

procedure TLccNode.Logout;
var
  AMapping: TLccAliasMapping;
begin
  (NodeManager as INodeManagerCallbacks).DoLogOutNode(Self);
  if GridConnect then
  begin
    FPermitted := False;
    WorkerMessage.LoadAMR(NodeID, AliasID);
    SendMessageFunc(Self, WorkerMessage);
    (NodeManager as INodeManagerCallbacks).DoCANAliasMapReset(Self);
  end;
  // TODO Is there a message to take a non CAN node off line??????
  FInitialized := False;
  _100msTimer.Enabled := False;
  DatagramResendQueue.Clear;
  if GridConnect then
  begin
    AliasServer.FlushDelayedMessages;
    AMapping := AliasServer.RemoveMapping(AliasID, False);
    try
      (NodeManager as INodeManagerCallbacks).DoAliasMappingChange(Self, AMapping, False);
    finally
      AMapping.Free;
    end;
  end;
end;

procedure TLccNode.On_100msTimer(Sender: TObject);
var
  Temp: TNodeID;
begin
  if GridConnect then
  begin
    if not Permitted then
    begin
      Inc(FLoginTimoutCounter);
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
        LoginTimoutCounter := 0;
      end else
      begin
        if LoginTimoutCounter > 7 then
        begin
          FPermitted := True;
          WorkerMessage.LoadRID(NodeID, AliasID);
          SendMessageFunc(Self, WorkerMessage);
          WorkerMessage.LoadAMD(NodeID, AliasID);
          SendMessageFunc(Self, WorkerMessage);
          (NodeManager as INodeManagerCallbacks).DoAliasIDChanged(Self);
          LogInLCC(NodeID);
        end;
      end
    end else
    begin
      DatagramResendQueue.TickTimeout;
      LccActions.TimeTick;
    end
  end else
  begin
    DatagramResendQueue.TickTimeout;
    LccActions.TimeTick;
  end;
end;

function TLccNode.ProcessMessage(SourceMessage: TLccMessage): Boolean;
begin
  if GridConnect then
    Result := ProcessMessageGridConnect(SourceMessage)   // When necessary ProcessMessageGridConnect drops the message into ProcessMessageLCC
  else
    Result := ProcessMessageLCC(SourceMessage);
end;

procedure TLccNode.SendDatagramAckReply(SourceMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  // Only Ack if we accept the datagram
  WorkerMessageDatagram.LoadDatagramAck(NodeID, FAliasID,
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
    WorkerMessage.LoadConsumerIdentified(NodeID, FAliasID, Temp, ProtocolEventConsumed.Event[i].State);
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
    WorkerMessage.LoadConsumerIdentified(NodeID, FAliasID, Temp, EventObj.State);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccNode.SendDatagramRejectedReply(SourceMessage: TLccMessage; Reason: Word);
begin
  WorkerMessageDatagram.LoadDatagramRejected(NodeID, FAliasID,
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
    SendDatagramRejectedReply(SourceMessage, ERROR_TEMPORARY_BUFFER_UNAVAILABLE)
end;

procedure TLccNode.SendEvents;
begin
  SendConsumedEvents;
  SendProducedEvents;
end;

procedure TLccNode.SendInitializeComplete;
begin
  WorkerMessage.LoadInitializationComplete(NodeID, FAliasID);
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
    WorkerMessage.LoadProducerIdentified(NodeID, FAliasID, Temp, ProtocolEventsProduced.Event[i].State);
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
    WorkerMessage.LoadProducerIdentified(NodeID, FAliasID, Temp, EventObj.State);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;


initialization
  {$IFNDEF DWSCRIPT}
  Randomize;
  {$ENDIF}

finalization

end.

