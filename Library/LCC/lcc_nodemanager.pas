unit lcc_nodemanager;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, lcc_defines,
  {$IFDEF FPC}
  laz2_DOM, laz2_XMLRead, LResources, ExtCtrls,
  {$ENDIF}
  {$IFNDEF FPC}
  Types,
  FMX.Types,
  System.Generics.Collections,
  {$ENDIF}
  lcc_utilities, lcc_math_float16, lcc_messages, lcc_app_common_settings,
  lcc_common_classes, lcc_rootnode;

const
  ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH = $0001;


type
  TLccNode = class;
  TLccNodeManager = class;
  TTraction = class;
  TConfigurationMemory = class;
  TLccOwnedNode = class;
  TLccOwnedNodeClass = class of TLccOwnedNode;

  TOnLccNodeMessage = procedure(Sender: TObject; LccSourceNode: TLccNode) of object;
  TOnLccNodeMessageWithDest = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode) of object;
  TOnLccNodeEventIdentified = procedure(Sender: TObject; LccSourceNode: TLccNode; var Event: TEventID; State: TEventState) of object;
  TOnLccNodeTractionProxyReplyAllocate = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; LegacyTechnology: Byte; TrainID: Word; var TrainNode: TNodeID; TrainAlias: Word) of object;
  TOnLccNodeTractionProxyReplyAttach = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; ReplyCode: Byte) of object;
  TOnLccNodeMessageResultCode = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; ResultCode: Byte) of object;
  TOnLccNodeTractionControllerQuery = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; ActiveControllerNodeID: TNodeID; ActiveControllerAlias: Word) of object;
  TOnLccNodeTractionControllerChangeNotify = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; NewRequestingNode: TNodeID; NewRequestingNodeAlias: Word; var Allow: Boolean) of object;
  TOnLccNodeConfigMem = procedure(Sender: TObject; LccSourceNode, LccDestNode: TLccNode) of object;
  TOnLccGetRootNodeClass = procedure(Sender: TObject; var NodeClass: TLccOwnedNodeClass) of object;

  { TNodeProtocolBase }

  TNodeProtocolBase = class(TComponent)
  private
    FCreateTime: DWord;
    FErrorCode: Word;
    FNext: TNodeProtocolBase;
    FOwnerManager: TLccNodeManager;
    FValid: Boolean;
    FWorkerMessage: TLccMessage;
    procedure SetOwnerManager(AValue: TLccNodeManager); virtual;
    procedure SetValid(AValue: Boolean); virtual;
  protected
    property CreateTime: DWord read FCreateTime write FCreateTime;
    property OwnerManager: TLccNodeManager read FOwnerManager write SetOwnerManager;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  public
    property ErrorCode: Word read FErrorCode write FErrorCode;
    property Valid: Boolean read FValid write SetValid;
    property Next: TNodeProtocolBase read FNext write FNext;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; virtual; abstract;
  end;

  { TProtocolSupport }

  TProtocolSupport = class(TNodeProtocolBase)
  private
    FACDI: Boolean;
    FCDI: Boolean;
    FDatagram: Boolean;
    FDisplay: Boolean;
    FEventExchange: Boolean;
    FFDI: Boolean;
    FFunctionConfiguration: Boolean;
    FIdentification: Boolean;
    FMemConfig: Boolean;
    FRemoteButton: Boolean;
    FReservation: Boolean;
    FSimpleNodeInfo: Boolean;
    FSimpleTrainNodeInfo: Boolean;
    FStream: Boolean;
    FTeach_Learn: Boolean;
    FTractionControl: Boolean;
    FTractionProxy: Boolean;
  protected
    Flags: array of QWord;
    procedure DecodeFlags;
    function EncodeFlags: QWord;
  public
    property Datagram: Boolean read FDatagram write FDatagram;
    property Stream: Boolean read FStream write FStream;
    property MemConfig: Boolean read FMemConfig write FMemConfig;
    property Reservation: Boolean read FReservation write FReservation;
    property EventExchange: Boolean read FEventExchange write FEventExchange;
    property Identification: Boolean read FIdentification write FIdentification;
    property Teach_Learn: Boolean read FTeach_Learn write FTeach_Learn;
    property RemoteButton: Boolean read FRemoteButton write FRemoteButton;
    property ACDI: Boolean read FACDI write FACDI;
    property Display: Boolean read FDisplay write FDisplay;
    property SimpleNodeInfo: Boolean read FSimpleNodeInfo write FSimpleNodeInfo;
    property CDI: Boolean read FCDI write FCDI;
    property TractionControl: Boolean read FTractionControl write FTractionControl;
    property FDI: Boolean read FFDI write FFDI;
    property FunctionConfiguration: Boolean read FFunctionConfiguration write FFunctionConfiguration;
    property TractionProxy: Boolean read FTractionProxy write FTractionProxy;
    property SimpleTrainNodeInfo: Boolean read FSimpleTrainNodeInfo write FSimpleTrainNodeInfo;

    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TSimpleNodeInfo }

  TSimpleNodeInfo = class(TNodeProtocolBase)
  private
    FHardwareVersion: string;
    FManufacturer: string;
    FModel: string;
    FSoftwareVersion: string;
    FPackedInfo: TSimpleNodeInfoPacked;
    FUserDescription: string;
    FUserName: string;
    FUserVersion: Word;
    FVersion: Word;

    function GetPackedFormat: TSimpleNodeInfoPacked;
  public
    property Version: Word read FVersion write FVersion;
    property Manufacturer: string read FManufacturer write FManufacturer;
    property Model: string read FModel write FModel;
    property HardwareVersion: string read FHardwareVersion write FHardwareVersion;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;
    property UserVersion: Word read FUserVersion write FUserVersion;
    property UserName: string read FUserName write FUserName;
    property UserDescription: string read FUserDescription write FUserDescription;

    property PackedFormat: TSimpleNodeInfoPacked read GetPackedFormat;

    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TSimpleTrainNodeInfo }

  TSimpleTrainNodeInfo = class(TNodeProtocolBase)
  private
    FManufacturer: string;
    FOwner: string;
    FRoadname: string;
    FRoadNumber: string;
    FTrainClass: string;
    FTrainName: string;
    FVersion: Word;
  public
    property Version: Word read FVersion;
    property Roadname: string read FRoadname;
    property TrainClass: string read FTrainClass;
    property RoadNumber: string read FRoadNumber;
    property TrainName: string read FTrainName;
    property Manufacturer: string read FManufacturer;
    property Owner: string read FOwner;

    function ProcessMessage(LccMessage: TLccMessage; Traction: TTraction): Boolean; reintroduce; virtual;
  end;

  { TLccEvent }

  TLccEvent = class
  private
    FID: TEventID;
    FState: TEventState;
  public
    property ID: TEventID read FID write FID;
    property State: TEventState read FState write FState;
  end;

  { TLccEvents }

  TLccEvents = class(TNodeProtocolBase)
  private
    {$IFDEF FPC}
    FEventList: TList;
    {$ELSE}
    FEventList: TObjectList<TLccEvent>;
    {$ENDIF}
    function GetEvent(Index: Integer): TLccEvent;
  protected
    {$IFDEF FPC}
      property EventList: TList read FEventList write FEventList;
    {$ELSE}
      property EventList: TObjectList<TLccEvent> read FEventList write FEventList;
    {$ENDIF}
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(Event: TEventID; State: TEventState);
    procedure Clear;
    function Supports(Event: TEventID): TLccEvent;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
    property Event[Index: Integer]: TLccEvent read GetEvent;

  end;

  { TStreamBasedProtocol }

  TStreamBasedProtocol = class(TNodeProtocolBase)
  private
    FInProcessAddress: DWord;
    FStream: TMemoryStream;
    FAddressSpace: Byte;
  protected
    procedure SetValid(AValue: Boolean); override;
    procedure DoLoadComplete(LccMessage: TLccMessage); virtual; abstract;

    property InProcessAddress: DWord read FInProcessAddress write FInProcessAddress;
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;
  public
    property AStream: TMemoryStream read FStream write FStream;

    constructor Create(AnOwner: TComponent; AnAddressSpace: Byte); reintroduce; virtual;
    destructor Destroy; override;
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage); virtual;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TFDI }

  TFDI = class(TStreamBasedProtocol)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  end;

  { TFunctionConfiguration }

  TFunctionConfiguration = class(TNodeProtocolBase)
  private
    FFunctionStatesArray: TFunctionStatesArray;
    function GetFunctionStates(iIndex: Integer): Boolean;
  protected
    property FunctionStatesArray: TFunctionStatesArray read FFunctionStatesArray write FFunctionStatesArray;
  public
    property FunctionStates[iIndex: Integer]: Boolean read GetFunctionStates;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TCDI }

  TCDI = class(TStreamBasedProtocol)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  end;

  { TACDIMfg }

  TACDIMfg = class(TStreamBasedProtocol)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  end;

  { TConfiguration }

  TConfiguration = class(TStreamBasedProtocol)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  end;


  { TConfigurationMemory }

  TConfigurationMemory = class(TNodeProtocolBase)
  private
    FAddress: DWord;
    FAddressSpace: Byte;
    FDataCount: Integer;
    FDataRaw: TDatagramArray;
    FDataType: TLccConfigDataType;
    FDataTypeBit: Byte;
    FDataTypeEvent: TEventID;
    FDataTypeInteger: Integer;
    FDataTypeString: string;
    FInProcessAddress: DWord;
    function GetDataRawIndexer(iIndex: Word): Byte;

    procedure SetDataRawIndexer(iIndex: Word; const Value: Byte);
  protected
    property InProcessAddress: DWord read FInProcessAddress write FInProcessAddress;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Address: DWord read FAddress write FAddress;
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;
    property DataCount: Integer read FDataCount write FDataCount;
    property DataRaw: TDatagramArray read FDataRaw write FDataRaw;
    property DataRawIndexer[iIndex: Word]: Byte read GetDataRawIndexer write SetDataRawIndexer;
    property DataType: TLccConfigDataType read FDataType write FDataType;
    property DataTypeInteger: Integer read FDataTypeInteger;
    property DataTypeEvent: TEventID read FDataTypeEvent;
    property DataTypeBit: Byte read FDataTypeBit;
    property DataTypeString: string read FDataTypeString;
    procedure Initialize(AnAddress: DWord; AnAddressSpace: Byte; DataSize: Integer; ADataType: TLccConfigDataType);
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TConfigMemAddressSpaceInfoObject }

  TConfigMemAddressSpaceInfoObject = class
  private
    FHighAddress: DWord;
    FIsReadOnly: Boolean;
    FImpliedZeroLowAddress: Boolean;
    FLowAddress: DWord;
    FIsPresent: Boolean;
    FAddressSpace: Byte;
  public
    property AddressSpace: Byte read FAddressSpace;
    property IsPresent: Boolean read FIsPresent;
    property IsReadOnly: Boolean read FIsReadOnly;
    property ImpliedZeroLowAddress: Boolean read FImpliedZeroLowAddress;
    property LowAddress: DWord read FLowAddress;
    property HighAddress: DWord read FHighAddress;
  end;

  { TConfigMemAddressSpaceInfo }

  TConfigMemAddressSpaceInfo = class(TNodeProtocolBase)
  private
    FList: TList;
    function GetAddressSpace(Index: Integer): TConfigMemAddressSpaceInfoObject;
    function GetCount: Integer;
  protected
    property List: TList read FList write FList;
  public
    property AddressSpace[Index: Integer]: TConfigMemAddressSpaceInfoObject read GetAddressSpace;
    property Count: Integer read GetCount;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(_Space: Byte; _IsPresent, _IsReadOnly, _ImpliedZeroLowAddress: Boolean; _LowAddress, _HighAddress: DWord);
    procedure Clear;
    function FindByAddressSpace(Space: Byte): TConfigMemAddressSpaceInfoObject;
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TTraction }

  TTraction = class(TNodeProtocolBase)
  private
    FLegacySpeedSteps: Byte;
    FLegacyTechnology: Byte;
    FLegacyTrainID: Word;
    FLinkedNode: TLccNode;                 // depends on the Node: Throttle Node = Linked Train Node, Train Node = Linked Throttle Node
    FScratchNode: TLccNode;
    FSpeed: THalfFloat;
    FSpeedActual: THalfFloat;
    FSpeedCommanded: THalfFloat;
    procedure SetFunctions(Index: DWord; AValue: Word);
    function GetFunctions(Index: DWord): Word;
  protected
    FunctionArray: array of Word;
    procedure GrowArray(NewSize: DWord);
  public
    property Speed: THalfFloat read FSpeed;
    property SpeedActual: THalfFloat read FSpeedActual;
    property SpeedCommanded: THalfFloat read FSpeedCommanded;
    property Functions[Index: DWord]: Word read GetFunctions;
    property LinkedNode: TLccNode read FLinkedNode write FLinkedNode;
    property LegacyTechnology: Byte read FLegacyTechnology write FLegacyTechnology;
    property LegacyTrainID: Word read FLegacyTrainID write FLegacyTrainID;
    property LegacySpeedSteps: Byte read FLegacySpeedSteps write FLegacySpeedSteps;
    property ScratchNode: TLccNode read FScratchNode write FScratchNode;

    function IsLinked: Boolean;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TConfigurationMemOptions }

  TConfigurationMemOptions = class(TNodeProtocolBase)
  private
    FHighSpace: Byte;
    FLowSpace: Byte;
    FSupportACDIMfgRead: Boolean;
    FSupportACDIUserRead: Boolean;
    FSupportACDIUserWrite: Boolean;
    FUnAlignedReads: Boolean;
    FUnAlignedWrites: Boolean;
    FWriteArbitraryBytes: Boolean;
    FWriteLenFourBytes: Boolean;
    FWriteLenOneByte: Boolean;
    FWriteLenSixyFourBytes: Boolean;
    FWriteLenTwoBytes: Boolean;
    FWriteStream: Boolean;
    FWriteUnderMask: Boolean;
  public
    property WriteUnderMask: Boolean read FWriteUnderMask write FWriteUnderMask;
    property UnAlignedReads: Boolean read FUnAlignedReads write FUnAlignedReads;
    property UnAlignedWrites: Boolean read FUnAlignedWrites write FUnAlignedWrites;
    property SupportACDIMfgRead: Boolean read FSupportACDIMfgRead write FSupportACDIMfgRead;
    property SupportACDIUserRead: Boolean read FSupportACDIUserRead write FSupportACDIUserRead;
    property SupportACDIUserWrite: Boolean read FSupportACDIUserWrite write FSupportACDIUserWrite;
    property WriteLenOneByte: Boolean read FWriteLenOneByte write FWriteLenOneByte;
    property WriteLenTwoBytes: Boolean read FWriteLenTwoBytes write FWriteLenTwoBytes;
    property WriteLenFourBytes: Boolean read FWriteLenFourBytes write FWriteLenFourBytes;
    property WriteLenSixyFourBytes: Boolean read FWriteLenSixyFourBytes write FWriteLenSixyFourBytes;
    property WriteArbitraryBytes: Boolean read FWriteArbitraryBytes write FWriteArbitraryBytes;
    property WriteStream: Boolean read FWriteStream write FWriteStream;
    property HighSpace: Byte read FHighSpace write FHighSpace;
    property LowSpace: Byte read FLowSpace write FLowSpace;

    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
    procedure LoadReply(LccMessage: TLccMessage);
  end;

  { TLccNode }

  TLccNode = class(TNodeProtocolBase)
  private
    FAliasID: Word;
    FCDI: TCDI;
    FConfigurationMem: TConfigurationMemory;
    FConfigurationMemOptions: TConfigurationMemOptions;
    FEventsConsumed: TLccEvents;
    FEventsProduced: TLccEvents;
    FFDI: TFDI;
    FFunctionConfiguration: TFunctionConfiguration;
    FiStartupSequence: Word;
    FNodeID: TNodeID;
    FNodeIDStr: string;
    FProtocolSupport: TProtocolSupport;
    FSimpleNodeInfo: TSimpleNodeInfo;
    FSimpleTrainNodeInfo: TSimpleTrainNodeInfo;
    FTraction: TTraction;
    FConfigMemAddressSpaceInfo: TConfigMemAddressSpaceInfo;
    procedure SetOwnerManager(AValue: TLccNodeManager); override;
  protected

    function ExtractAddressSpace(LccMessage: TLccMessage): Byte;
  public
    property AliasID: Word read FAliasID;
    property CDI: TCDI read FCDI write FCDI;
    property ConfigurationMem: TConfigurationMemory read FConfigurationMem write FConfigurationMem;
    property ConfigurationMemOptions: TConfigurationMemOptions read FConfigurationMemOptions write FConfigurationMemOptions;
    property ConfigMemAddressSpaceInfo: TConfigMemAddressSpaceInfo read FConfigMemAddressSpaceInfo write FConfigMemAddressSpaceInfo;
    property EventsConsumed: TLccEvents read FEventsConsumed write FEventsConsumed;
    property EventsProduced: TLccEvents read FEventsProduced write FEventsProduced;
    property FDI: TFDI read FFDI write FFDI;
    property FunctionConfiguration: TFunctionConfiguration read FFunctionConfiguration write FFunctionConfiguration;
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr: string read FNodeIDStr;
    property ProtocolSupport: TProtocolSupport read FProtocolSupport;
    property iStartupSequence: Word read FiStartupSequence write FiStartupSequence;
    property SimpleNodeInfo: TSimpleNodeInfo read FSimpleNodeInfo;
    property SimpleTrainNodeInfo: TSimpleTrainNodeInfo read FSimpleTrainNodeInfo;
    property Traction: TTraction read FTraction write FTraction;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    function IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TLccOwnedNode }

  TLccOwnedNode = class(TLccNode)
  private
    FACDIMfg: TACDIMfg;
    FConfiguration: TConfiguration;
    FDuplicateAliasDetected: Boolean;
    FInitialized: Boolean;
    FLogInAliasID: Word;
    FLoginTimer: TTimer;
    FPermitted: Boolean;
    FSeedNodeID: TNodeID;
  protected
    property DuplicateAliasDetected: Boolean read FDuplicateAliasDetected write FDuplicateAliasDetected;
    property LogInAliasID: Word read FLogInAliasID write FLogInAliasID;
    property LoginTimer: TTimer read FLoginTimer write FLoginTimer;
    property SeedNodeID: TNodeID read FSeedNodeID write FSeedNodeID;

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
  public
    property ACDIMfg: TACDIMfg read FACDIMfg write FACDIMfg;
    property Configuration: TConfiguration read FConfiguration write FConfiguration;
    property Initialized: Boolean read FInitialized;
    property Permitted: Boolean read FPermitted;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Login(NewNodeID, RegenerateAliasSeed: Boolean);

    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TLccDefaultRootNode }

  TLccDefaultRootNode = class(TLccOwnedNode)
  public
   constructor Create(AnOwner: TComponent); override;
  end;


  { TLccNodeManager }

  TLccNodeManager = class(TComponent)
  private
    FAutoInterrogateDiscoveredNodes: Boolean;
    FCAN: Boolean;
    FCdiParser: TLccCdiParserBase;
    FEnabled: Boolean;
    FHardwareConnection: TLccHardwareConnectionManager;
    FLccSettings: TLccSettings;
    FNodeList: TList;
    FOnAliasIDChanged: TOnLccNodeMessage;
    FOnNodeIDChanged: TOnLccNodeMessage;
    FOnLccGetRootNodeClass: TOnLccGetRootNodeClass;
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
    FOnLccNodeTractionProxyReplyAllocate: TOnLccNodeTractionProxyReplyAllocate;
    FOnLccNodeTractionProxyReplyAttach: TOnLccNodeTractionProxyReplyAttach;
    FOnLccNodeTractionProxyReplyManage: TOnLccNodeMessageResultCode;
    FOnLccNodeTractionReplyControllerAssign: TOnLccNodeMessageResultCode;
    FOnLccNodeTractionReplyControllerChangeNotify: TOnLccNodeMessageResultCode;
    FOnLccNodeTractionReplyControllerQuery: TOnLccNodeTractionControllerQuery;
    FOnLccNodeTractionReplyManage: TOnLccNodeMessageResultCode;
    FOnLccNodeTractionReplyQueryFunction: TOnLccNodeMessageWithDest;
    FOnLccNodeTractionReplyQuerySpeed: TOnLccNodeMessageWithDest;
    FOnLccNodeVerifiedNodeID: TOnLccNodeMessage;
    FOnRequestMessageSend: TOnMessageEvent;
    FOwnedNodeList: TList;
    FRootNode: TLccOwnedNode;
    FWorkerMessage: TLccMessage;
    FAutoSendVerifyNodesOnStart: Boolean;
    function GetNodes(Index: Integer): TLccNode;
    function GetOwnedNodes(Index: Integer): TLccOwnedNode;
    procedure SetCAN(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetNodes(Index: Integer; AValue: TLccNode);
    procedure SetOwnedNodes(Index: Integer; AValue: TLccOwnedNode);
  protected
    property NodeList: TList read FNodeList write FNodeList;
    property OwnedNodeList: TList read FOwnedNodeList write FOwnedNodeList;

    procedure DoAliasIDChanged(LccNode: TLccNode); virtual;
    procedure DoCDI(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoConfigMemReadReply(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoConfigMemWriteReply(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoCreateLccNode(SourceLccNode: TLccNode); virtual;
    procedure DoConsumerIdentified(SourceLccNode: TLccNode; var Event: TEventID; State: TEventState); virtual;
    procedure DoDatagramReply(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoDestroyLccNode(LccNode: TLccNode); virtual;
    procedure DoFDI(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoFunctionConfiguration(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoGetRootNodeClass(var RootNodeClass: TLccOwnedNodeClass); virtual;
    procedure DoInitializationComplete(SourceLccNode: TLccNode); virtual;
    procedure DoNodeIDChanged(LccNode: TLccNode); virtual;
    procedure DoOptionalInteractionRejected(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoProducerIdentified(SourceLccNode: TLccNode; var Event: TEventID; State: TEventState); virtual;
    procedure DoProtocolIdentifyReply(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoRemoteButtonReply(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoRequestMessageSend(Message: TLccMessage); virtual;
    procedure DoSimpleNodeIdentReply(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoSimpleTrainNodeIdentReply(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoTractionControllerChangeNotify(SourceLccNode, DestLccNode: TLccNode; NewRequestingNode: TNodeID; NewRequestingNodeAlias: Word; var Allow: Boolean); virtual;
    procedure DoTractionProxyReplyAllocate(SourceLccNode, DestLccNode: TLccNode; LegacyTechnology: Byte; TrainID: Word; var TrainNode: TNodeID; TrainAlias: Word); virtual;
    procedure DoTractionProxyReplyAttach(SourceLccNode, DestLccNode: TLccNode; ReplyCode: Byte); virtual;
    procedure DoTractionProxyReplyManage(SourceLccNode, DestLccNode: TLccNode; ResultCode: Byte); virtual;
    procedure DoTractionReplyQuerySpeed(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoTractionReplyQueryFunction(SourceLccNode, DestLccNode: TLccNode); virtual;
    procedure DoTractionReplyControllerAssign(SourceLccNode, DestLccNode: TLccNode; ResultCode: Byte); virtual;
    procedure DoTractionReplyControllerQuery(SourceLccNode, DestLccNode: TLccNode; ActiveControllerNodeID: TNodeID; ActiveControllerAlias: Word); virtual;
    procedure DoTractionReplyControllerChangeNotify(SourceLccNode, DestLccNode: TLccNode; ResultCode: Byte); virtual;
    procedure DoTractionReplyManage(SourceLccNode, DestLccNode: TLccNode; ResultCode: Byte); virtual;
    procedure DoVerifiedNodeID(SourceLccNode: TLccNode); virtual;

    function FindSourceNode(LccMessage: TLccMessage): TLccNode;
    function FindDestNode(LccMessage: TLccMessage): TLccNode;
    function FindOwnedDestNode(LccMessage: TLccMessage): TLccOwnedNode;
    function FindOwnedSourceNode(LccMessage: TLccMessage): TLccOwnedNode;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  public
    property Nodes[Index: Integer]: TLccNode read GetNodes write SetNodes;
    property OwnedNodes[Index: Integer]: TLccOwnedNode read GetOwnedNodes write SetOwnedNodes;
    property RootNode: TLccOwnedNode read FRootNode write FRootNode;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearOwned;
    function CreateNodeBySourceMessage(LccMessage: TLccMessage): TLccNode;
    function CreateNodeByDestMessage(LccMessage: TLccMessage): TLccNode;
    function CreateOwnedNode: TLccOwnedNode;
    function EqualEventID(var Event1, Event2: TEventID): Boolean;
    function FindNode(ANodeID: TNodeID; ANodeAlias: Word): TLccNode;
    function IsManagerNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
    procedure NodeIDStringToNodeID(ANodeIDStr: string; var ANodeID: TNodeID);
    function NodeIDToNodeIDStr(ANodeID: TNodeID): string;
    function ProcessMessage(LccMessage: TLccMessage): Boolean;

  published
    property AutoInterrogateDiscoveredNodes: Boolean read FAutoInterrogateDiscoveredNodes write FAutoInterrogateDiscoveredNodes;
    property AutoSendVerifyNodesOnStart: Boolean read FAutoSendVerifyNodesOnStart write FAutoSendVerifyNodesOnStart;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property CAN: Boolean read FCAN write SetCAN;
    property CdiParser: TLccCdiParserBase read FCdiParser write FCdiParser;
    property HardwareConnection: TLccHardwareConnectionManager read FHardwareConnection write FHardwareConnection;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property OnAliasIDChanged: TOnLccNodeMessage read FOnAliasIDChanged write FOnAliasIDChanged;
    property OnLccGetRootNodeClass: TOnLccGetRootNodeClass read FOnLccGetRootNodeClass write FOnLccGetRootNodeClass;
    property OnLccNodeCDI: TOnLccNodeMessageWithDest read FOnLccNodeCDI write FOnLccNodeCDI;
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
    property OnLccNodeTractionProxyReplyAllocate: TOnLccNodeTractionProxyReplyAllocate read FOnLccNodeTractionProxyReplyAllocate write FOnLccNodeTractionProxyReplyAllocate;
    property OnLccNodeTractionProxyReplyAttach: TOnLccNodeTractionProxyReplyAttach read FOnLccNodeTractionProxyReplyAttach write FOnLccNodeTractionProxyReplyAttach;
    property OnLccNodeTractionProxyReplyManage: TOnLccNodeMessageResultCode read FOnLccNodeTractionProxyReplyManage write FOnLccNodeTractionProxyReplyManage;
    property OnLccNodeVerifiedNodeID: TOnLccNodeMessage read FOnLccNodeVerifiedNodeID write FOnLccNodeVerifiedNodeID;
    property OnRequestMessageSend: TOnMessageEvent read FOnRequestMessageSend write FOnRequestMessageSend;
  end;

var
  TotalSNIPMessages: DWord;
  TotalSTNIPMessage: DWord;

procedure Register;

// ******************************************************************************
// ******************************************************************************
// ******************************************************************************
// ******************************************************************************
// ******************************************************************************
// ******************************************************************************
// ******************************************************************************
// ******************************************************************************

implementation

procedure Register;
begin
  {$IFDEF FPC}
  {$I TLccNodeManager.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccNodeManager]);
end;

{ TLccDefaultRootNode }

constructor TLccDefaultRootNode.Create(AnOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AnOwner);
  // Common Protocols
  ProtocolSupport.Datagram := True;      // We support CDI so we must support datagrams
  ProtocolSupport.MemConfig := True;   // We support CDI so we must support datagrams
  ProtocolSupport.CDI := True;         // We Support CDI
  ProtocolSupport.EventExchange := True;  // We support Events
  ProtocolSupport.SimpleNodeInfo := True;  // We Support SNIP

  // Setup the SNIP constants, this information MUST be idential to the information
  // in the  <identification> tag of the CDI to comply with the LCC specs
  SimpleNodeInfo.Version := SNIP_VER;
  SimpleNodeInfo.Manufacturer := SNIP_MFG;
  SimpleNodeInfo.Model := SNIP_MODEL;
  SimpleNodeInfo.SoftwareVersion := SNIP_SW_VER;
  SimpleNodeInfo.HardwareVersion := SNIP_HW_VER;
  SimpleNodeInfo.UserVersion := SNIP_USER_VER;
  SimpleNodeInfo.UserDescription := SNIP_USER_DESC;
  SimpleNodeInfo.UserName := SNIP_USER_NAME;

  CDI.AStream.Clear;
  for i := 0 to MAX_CDI_ARRAY - 1 do
    CDI.AStream.WriteByte(CDI_ARRAY[i]);
  CDI.Valid := True;
end;

{ TConfigurationMemOptions }

procedure TConfigurationMemOptions.LoadReply(LccMessage: TLccMessage);
var
  OpsMask: Word;
begin
  LccMessage.DataArrayIndexer[0] := $20;
  LccMessage.DataArrayIndexer[1] := MCP_OP_GET_CONFIG_REPLY;
  LccMessage.DataArrayIndexer[5] := FHighSpace;
  LccMessage.DataArrayIndexer[6] := FLowSpace;
  LccMessage.DataArrayIndexer[4] := 0;
  if WriteLenOneByte then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_ONE_BYTE;
  if WriteLenTwoBytes then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_TWO_BYTE;
  if WriteLenFourBytes then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_FOUR_BYTE;
  if WriteLenSixyFourBytes then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_64_BYTE;
  if WriteArbitraryBytes then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_ARBITRARY_BYTE;
  if WriteStream then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_STREAM_WRITE_SUPPORTED;
  OpsMask := 0;
  if WriteUnderMask then
    OpsMask := OpsMask or MCO_WRITE_UNDER_MASK;
  if UnAlignedReads then
    OpsMask := OpsMask or MCO_UNALIGNED_READS;
  if UnAlignedWrites then
    OpsMask := OpsMask or MCO_UNALIGNED_WRITES;
  if SupportACDIMfgRead then
    OpsMask := OpsMask or MCO_ACDI_MFG_READS;
  if SupportACDIUserRead then
    OpsMask := OpsMask or MCO_ACDI_USER_READS;
  if SupportACDIUserWrite then
    OpsMask := OpsMask or MCO_ACDI_USER_WRITES;
  LccMessage.DataArrayIndexer[2] := _Hi(OpsMask);
  LccMessage.DataArrayIndexer[3] := _Lo(OpsMask);
  LccMessage.DataCount := 7;
  LccMessage.UserValid := True;
end;

function TConfigurationMemOptions.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  OpsMask: Word;
begin
  Result := True;
  if LccMessage.MTI = MTI_DATAGRAM then
  begin
    case LccMessage.DataArrayIndexer[1] of
      MCP_OP_GET_CONFIG_REPLY :
        begin
          FHighSpace := LccMessage.DataArrayIndexer[5];
          FLowSpace := LccMessage.DataArrayIndexer[6];
          FWriteLenOneByte := LccMessage.DataArrayIndexer[4] and MCWL_ONE_BYTE <> 0;
          FWriteLenTwoBytes := LccMessage.DataArrayIndexer[4] and MCWL_TWO_BYTE <> 0;
          FWriteLenFourBytes := LccMessage.DataArrayIndexer[4] and MCWL_FOUR_BYTE <> 0;
          FWriteLenSixyFourBytes := LccMessage.DataArrayIndexer[4] and MCWL_64_BYTE <> 0;
          FWriteArbitraryBytes := LccMessage.DataArrayIndexer[4] and MCWL_ARBITRARY_BYTE <> 0;
          FWriteStream := LccMessage.DataArrayIndexer[4] and MCWL_STREAM_WRITE_SUPPORTED <> 0;
          OpsMask := LccMessage.ExtractDataBytesAsInt(2, 3);
          FWriteUnderMask := OpsMask and MCO_WRITE_UNDER_MASK <> 0;
          FUnAlignedReads := OpsMask and MCO_UNALIGNED_READS <> 0;
          FUnAlignedWrites := OpsMask and MCO_UNALIGNED_WRITES <> 0;
          SupportACDIMfgRead := OpsMask and MCO_ACDI_MFG_READS <> 0;
          SupportACDIUserRead := OpsMask and MCO_ACDI_USER_READS <> 0;
          SupportACDIUserWrite := OpsMask and MCO_ACDI_USER_WRITES <> 0;
        end;
    end
  end;
end;

{ TLccOwnedNode }

constructor TLccOwnedNode.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  LoginTimer := TTimer.Create(Self);
  LoginTimer.Enabled := False;
  LoginTimer.Interval := 800;
  LoginTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}OnLoginTimer;
  LogInAliasID := 0;
  FACDIMfg := TACDIMfg.Create(Self, MSI_ACDI_MFG);
  FConfiguration := TConfiguration.Create(Self, MSI_CONFIG);
end;

function TLccOwnedNode.CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
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

destructor TLccOwnedNode.Destroy;
begin
  if Permitted and Assigned(OwnerManager) then
  begin
    WorkerMessage.LoadAMR(NodeID, AliasID);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
  end;
  FPermitted := False;
  FreeAndNil(FACDIMfg);
  FreeAndNil(FConfiguration);
  inherited Destroy;
end;

function TLccOwnedNode.GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
begin
  Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
end;

procedure TLccOwnedNode.GenerateNewNodeID;
begin
  FNodeIDStr := '0x020112';
  FNodeID[1] := StrToInt(FNodeIDStr);
  FNodeID[0] := Random($FFFFFF);
  FNodeIDStr := FNodeIDStr + IntToHex(FNodeID[0], 6);
  FSeedNodeID[0] := FNodeID[0];
  FSeedNodeID[1] := FNodeID[1];
end;

procedure TLccOwnedNode.Login(NewNodeID, RegenerateAliasSeed: Boolean);
var
  TempNodeID: TNodeID;
  TempID, TempID1, TempID2: QWord;
begin
  if Assigned(OwnerManager.LccSettings) then
  begin
    if OwnerManager.LccSettings.General.NodeIDAsVal = 0 then
    begin
      GenerateNewNodeID;
      OwnerManager.LccSettings.General.NodeIDAsTNodeID(TempNodeID);
      if not EqualNodeID(TempNodeID, NodeID, True) then
      begin
         TempID1 := QWord(NodeID[0]);
         TempID2 := QWord(NodeID[1]);
         TempID2 := TempID2 shl 24;
         TempID := TempID1 or TempID2;
         OwnerManager.LccSettings.General.NodeID := '0x'+IntToHex(TempID, 12);
         OwnerManager.LccSettings.SaveToFile;
      end;
    end else
    begin
      OwnerManager.LccSettings.General.NodeIDAsTNodeID(TempNodeID);
      FNodeID[0] := TempNodeID[0];
      FNodeID[1] := TempNodeID[1];
    end;
  end else
  begin
    if NewNodeID then
      GenerateNewNodeID;
  end;
  if Assigned(OwnerManager) then
    OwnerManager.DoNodeIDChanged(Self);
  LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
  SendAliasLoginRequest;
  DuplicateAliasDetected := False;
  LoginTimer.Enabled := True;
end;

procedure TLccOwnedNode.OnLoginTimer(Sender: TObject);
begin
  LoginTimer.Enabled := False;
  FAliasID := LoginAliasID;
  LogInAliasID := 0;
  if Assigned(OwnerManager) then
    OwnerManager.DoAliasIDChanged(Self);
  SendAliasLogin;
  SendEvents;
  if OwnerManager.RootNode = Self then
  begin
    if OwnerManager.AutoSendVerifyNodesOnStart then
    begin
      WorkerMessage.LoadVerifyNodeID(NodeID, AliasID);
      OwnerManager.DoRequestMessageSend(WorkerMessage);
    end;
  end;
end;

function TLccOwnedNode.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
  Event: TLccEvent;
begin
  Result := False;
  TestNodeID[0] := 0;
  TestNodeID[1] := 0;

  if LogInAliasID <> 0 then
  begin
    if LccMessage.CAN.SourceAlias = LogInAliasID then
    begin
      LogInAliasID := CreateAliasID(FNodeID, True);
      SendAliasLoginRequest;
      LoginTimer.Enabled := True;
      Exit;
    end;
  end;

  if Permitted then
  begin
    if LccMessage.CAN.SourceAlias = AliasID then
    begin
      if ((LccMessage.CAN.MTI and $0F000000) >= MTI_CAN_CID6) and ((LccMessage.CAN.MTI and $0F000000) <= MTI_CAN_CID0) then
      begin
        WorkerMessage.LoadRID(AliasID);                   // sorry charlie this is mine
        OwnerManager.DoRequestMessageSend(WorkerMessage);
        Exit;
      end else
      begin
        WorkerMessage.LoadAMR(NodeID, AliasID);          // You used my Alias you dog......
        OwnerManager.DoRequestMessageSend(WorkerMessage);
        FPermitted := False;
        Login(False, True);
        Exit;
      end;
    end;
  end;

  if LccMessage.HasDestination then
  begin
    if (LccMessage.CAN.DestAlias > 0) and (AliasID > 0) then
    begin
      if LccMessage.CAN.DestAlias <> AliasID then
        Exit;
    end else
    begin
      if not EqualNodeID(LccMessage.DestID, NodeID, False) then
        Exit;
    end;
  end;

  if Permitted and Initialized then
  begin
    if LccMessage.CANOnly then
    begin
      case LccMessage.CAN.MTI of
        MTI_CAN_AME  :
            begin
              if LccMessage.DataCount = 6 then
              begin
                LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
                if EqualNodeID(TestNodeID, NodeID, False) then
                begin
                  WorkerMessage.LoadAMD(NodeID, AliasID);
                  OwnerManager.DoRequestMessageSend(WorkerMessage);
                end
              end else
              begin
                WorkerMessage.LoadAMD(NodeID, AliasID);
                OwnerManager.DoRequestMessageSend(WorkerMessage);
              end;
            end;
        MTI_CAN_AMD  :
            begin
              if LccMessage.DataCount = 6 then
              begin
                LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
                if EqualNodeID(TestNodeID, NodeID, False) then                  // some Dog has my Node ID!
                begin
                  WorkerMessage.LoadPCER(NodeID, AliasID, @EVENT_DUPLICATE_ID_DETECTED);
                  OwnerManager.DoRequestMessageSend(WorkerMessage);
                end
              end else
            end;
        MTI_CAN_RID  : begin end;
      end;
    end else
    begin
      case LccMessage.MTI of
        MTI_VERIFY_NODE_ID_NUMBER      :
            begin
              if LccMessage.DataCount = 6 then
              begin
                LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
                if EqualNodeID(TestNodeID, NodeID, False) then
                begin
                  WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
                  OwnerManager.DoRequestMessageSend(WorkerMessage);
                end
              end else
              begin
                WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
                OwnerManager.DoRequestMessageSend(WorkerMessage);
              end;
            end;
        MTI_VERIFY_NODE_ID_NUMBER_DEST :
            begin
              WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
              OwnerManager.DoRequestMessageSend(WorkerMessage);
            end;
        MTI_SIMPLE_NODE_INFO_REQUEST :
            begin
              WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, SimpleNodeInfo.PackedFormat);
              OwnerManager.DoRequestMessageSend(WorkerMessage);
            end;
        MTI_PROTOCOL_SUPPORT_INQUIRY :
            begin
              WorkerMessage.LoadProtocolIdentifyReply(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ProtocolSupport.EncodeFlags);
              OwnerManager.DoRequestMessageSend(WorkerMessage);
            end;
        MTI_EVENTS_IDENTIFY :
            begin
              SendConsumedEvents;
              SendProducedEvents;
            end;
        MTI_EVENTS_IDENTIFY_DEST :
            begin
              if AliasID = LccMessage.CAN.DestAlias then
              begin
                SendConsumedEvents;
                SendProducedEvents;
              end;
            end;
        MTI_PRODUCER_IDENDIFY :
            begin
              Event := EventsProduced.Supports(LccMessage.ExtractDataBytesAsEventID(0)^);
              if Assigned(Event) then
              begin
                WorkerMessage.LoadProducerIdentified(NodeID, AliasID, Event.FID, Event.State);
                OwnerManager.DoRequestMessageSend(WorkerMessage);
              end
            end;
        MTI_CONSUMER_IDENTIFY :
            begin
              Event := EventsConsumed.Supports(LccMessage.ExtractDataBytesAsEventID(0)^);
              if Assigned(Event) then
              begin
                WorkerMessage.LoadConsumerIdentified(NodeID, AliasID, Event.FID, Event.State);
                OwnerManager.DoRequestMessageSend(WorkerMessage);
              end
            end;
         MTI_DATAGRAM_REJECTED_REPLY :
           begin
             // This is passed by the assembler/disassembler if something went wrong that needs to
             // get passed on
             LccMessage.SwapDestAndSourceIDs;
             OwnerManager.DoRequestMessageSend(LccMessage);
           end;
         MTI_DATAGRAM :
           begin
             case LccMessage.DataArrayIndexer[0] of
               DATAGRAM_PROTOCOL_CONFIGURATION :
                 begin
                   // Only Ack if we accept the datagram
                   WorkerMessage.LoadDatagramAck(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, True);
                   OwnerManager.DoRequestMessageSend(WorkerMessage);

                   case LccMessage.DataArrayIndexer[1] and $F0 of
                     MCP_WRITE :
                       begin
                         WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                    //     CDI.LoadReply(WorkerMessage);
                    //     OwnerManager.DoRequestMessageSend(WorkerMessage);
                       end;
                     MCP_WRITE_STREAM :
                       begin
                       end;
                     MCP_READ :
                       begin
                         WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                         case LccMessage.DataArrayIndexer[1] and $0F of
                           MCP_NONE :
                               begin
                                 case LccMessage.DataArrayIndexer[1] of
                                   MSI_CDI             : CDI.LoadReply(LccMessage, WorkerMessage);
                                   MSI_ALL             : begin end;
                                   MSI_CONFIG          : Configuration.LoadReply(LccMessage, WorkerMessage);
                                   MSI_ACDI_MFG        : ACDIMfg.LoadReply(LccMessage, WorkerMessage);
                                   MSI_ACDI_USER       : begin end;
                                   MSI_FDI             : begin end;
                                   MSI_FUNCTION_CONFIG : begin end;
                                 end
                               end;
                           MCP_CONFIGURATION : Configuration.LoadReply(LccMessage, WorkerMessage);
                           MCP_ALL           : begin  end;
                           MCP_CDI           : CDI.LoadReply(LccMessage, WorkerMessage);
                         end;
                         if WorkerMessage.UserValid then
                           OwnerManager.DoRequestMessageSend(WorkerMessage);
                       end;
                     MCP_READ_STREAM :
                       begin
                       end;
                     MCP_OP_GET_CONFIG :
                       begin
                         case LccMessage.DataArrayIndexer[1] of
                           MCP_OP_GET_CONFIG :
                             begin
                               WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                               ConfigurationMemOptions.LoadReply(WorkerMessage);
                               if WorkerMessage.UserValid then;
                                 OwnerManager.DoRequestMessageSend(WorkerMessage);
                             end;
                           MCP_OP_GET_ADD_SPACE_INFO :
                             begin
                               WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                               ConfigMemAddressSpaceInfo.LoadReply(LccMessage, WorkerMessage);
                               if WorkerMessage.UserValid then
                                 OwnerManager.DoRequestMessageSend(WorkerMessage);
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
                 // Undknown Datagram Type
                 WorkerMessage.LoadDatagramRejected(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, REJECTED_DATAGRAMS_NOT_ACCEPTED);
                 OwnerManager.DoRequestMessageSend(WorkerMessage);
               end;
             end;  // case
           end;
      else begin
          if LccMessage.HasDestination then
          begin
            WorkerMessage.LoadOptionalInteractionRejected(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, REJECTED_BUFFER_FULL, LccMessage.MTI);
            OwnerManager.DoRequestMessageSend(WorkerMessage);
          end;
        end;
      end; // case
    end;
  end;
end;

procedure TLccOwnedNode.PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
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

procedure TLccOwnedNode.SendAliasLogin;
begin
  if Assigned(OwnerManager) then
  begin
    WorkerMessage.LoadRID(AliasID);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
    WorkerMessage.LoadAMD(NodeID, AliasID);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
    FPermitted := True;
    WorkerMessage.LoadInitializationComplete(NodeID, AliasID);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
    FInitialized := True;
  end;
end;

procedure TLccOwnedNode.SendAliasLoginRequest;
begin
  if Assigned(OwnerManager) then
  begin
    WorkerMessage.LoadCID(NodeID, LoginAliasID, 0);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
    WorkerMessage.LoadCID(NodeID, LoginAliasID, 1);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
    WorkerMessage.LoadCID(NodeID, LoginAliasID, 2);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
    WorkerMessage.LoadCID(NodeID, LoginAliasID, 3);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
  end;
end;

procedure TLccOwnedNode.SendAMR;
begin
   WorkerMessage.LoadAMR(NodeID, AliasID);
   OwnerManager.DoRequestMessageSend(WorkerMessage);
end;

procedure TLccOwnedNode.SendConsumedEvents;
var
  i: Integer;
begin
  for i := 0 to EventsConsumed.EventList.Count - 1 do
  begin
    WorkerMessage.LoadConsumerIdentified(NodeID, AliasID, EventsConsumed.Event[i].FID, EventsConsumed.Event[i].State);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
  end;
end;

procedure TLccOwnedNode.SendEvents;
begin
  SendConsumedEvents;
  SendProducedEvents;
end;

procedure TLccOwnedNode.SendProducedEvents;
var
  i: Integer;
begin
  for i := 0 to EventsProduced.EventList.Count - 1 do
  begin
    WorkerMessage.LoadProducerIdentified(NodeID, AliasID, EventsProduced.Event[i].FID , EventsProduced.Event[i].State);
    OwnerManager.DoRequestMessageSend(WorkerMessage);
  end;
end;

{ TFunctionConfiguration }

function TFunctionConfiguration.GetFunctionStates(iIndex: Integer): Boolean;
begin
  if (iIndex > -1) and (iIndex < 30) then
    Result := FFunctionStatesArray[iIndex] = 1
  else
    Result := False;
end;

function TFunctionConfiguration.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  SourceNode, DestNode: TLccNode;
  FunctionAddress: DWord;
  i: Integer;
begin
  Result := False;
  FunctionAddress := LccMessage.ExtractDataBytesAsInt(2, 5);
  FunctionAddress := FunctionAddress and $000000FF;
  i := 7;
  if (LccMessage.DataCount - i) mod 2 = 0 then   // Words are 2 bytes so make sure we are on even boundy of words
  begin
    while i < LccMessage.DataCount do
    begin
      FFunctionStatesArray[FunctionAddress] := (LccMessage.DataArrayIndexer[i+1] shl 8) or LccMessage.DataArrayIndexer[i]; // Little
      Inc(FunctionAddress);
      Inc(i, 2);
    end;
    Valid := True;
    if Assigned(OwnerManager) then
    begin
      SourceNode := OwnerManager.FindSourceNode(LccMessage);
      DestNode := OwnerManager.FindDestNode(LccMessage);
      if Assigned(SourceNode) and Assigned(DestNode) then
        OwnerManager.DoFunctionConfiguration(SourceNode, DestNode);
    end;
  end;
end;

{ TLccEvents }

constructor TLccEvents.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  {$IFDEF FPC}
    FEventList := TList.Create;
  {$ELSE}
    FEventList := TObjectList<TLccEvent>.Create;
    EventList.OwnsObjects := False;
  {$ENDIF}
end;

destructor TLccEvents.Destroy;
begin
  Clear;
  FreeAndNil(FEventList);
  inherited Destroy;
end;

function TLccEvents.GetEvent(Index: Integer): TLccEvent;
begin
  {$IFDEF FPC}
    Result := TLccEvent( EventList[Index])
  {$ELSE}
    Result := EventList[Index]
  {$ENDIF}
end;

function TLccEvents.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := False;
end;

procedure TLccEvents.Add(Event: TEventID; State: TEventState);
var
  LccEvent: TLccEvent;
begin
  LccEvent := Supports(Event);
  if Assigned(LccEvent) then
    LccEvent.State := State
  else begin
    LccEvent := TLccEvent.Create;
    LccEvent.ID := Event;
    LccEvent.State := State;
    EventList.Add(LccEvent);
  end;
end;

procedure TLccEvents.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to EventList.Count - 1 do
    {$IFDEF FPC}
      TObject( EventList[i]).Free;
    {$ELSE}
      EventList[i].Free;
    {$ENDIF}
  finally
    EventList.Clear
  end;
end;

function TLccEvents.Supports(Event: TEventID): TLccEvent;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while not Assigned(Result) and (i < EventList.Count) do
  begin
    if EqualEventID(Event, TLccEvent( EventList[i]).ID) then
      Result := TLccEvent( EventList[i]);
    Inc(i);
  end;

end;

{ TConfigurationMemory }

constructor TConfigurationMemory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TConfigurationMemory.Destroy;
begin
  inherited Destroy;
end;

function TConfigurationMemory.GetDataRawIndexer(iIndex: Word): Byte;
begin
  Result := FDataRaw[iIndex]
end;

procedure TConfigurationMemory.Initialize(AnAddress: DWord;
  AnAddressSpace: Byte; DataSize: Integer; ADataType: TLccConfigDataType);
begin
  ErrorCode := 0;
  Address := AnAddress;
  DataCount := DataSize;
  AddressSpace := AnAddressSpace;
  InProcessAddress := AnAddress;
  DataType := ADataType;
  Valid := False;
  FDataTypeInteger := 0;
  FDataTypeEvent[0] := 0;
  FDataTypeEvent[1] := 0;
  FDataTypeEvent[2] := 0;
  FDataTypeEvent[3] := 0;
  FDataTypeEvent[4] := 0;
  FDataTypeEvent[5] := 0;
  FDataTypeEvent[6] := 0;
  FDataTypeEvent[7] := 0;
  FDataTypeBit := 0;
  FDataTypeString := '';
end;

function TConfigurationMemory.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  iStart, i, RemainingCount: Integer;

  LocalAddressSpace: Byte;
  SourceNode: TLccNode;
begin
  Result := True;
  LocalAddressSpace := 0;
  RemainingCount := 0;
  if LccMessage.DataArrayIndexer[1] and MCP_READ_REPLY = MCP_READ_REPLY then
  begin
    // First Block of Data
    if InProcessAddress = Address then
    begin
      if LccMessage.DataArrayIndexer[1] and $03 <> 0 then
      begin
         case LccMessage.DataArrayIndexer[1] and $03 of
           MCP_CDI           : LocalAddressSpace := MSI_CDI;
           MCP_ALL           : LocalAddressSpace := MSI_ALL;
           MCP_CONFIGURATION : LocalAddressSpace := MSI_CONFIG;
         end;
         iStart := 6;
      end else
      begin
         LocalAddressSpace := LccMessage.DataArrayIndexer[6];
         iStart := 7
      end;
      if LocalAddressSpace <> AddressSpace then
        ErrorCode := ErrorCode or ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH;
    end else
    begin
      // Subsequent Blocks of Data
      if LccMessage.DataArrayIndexer[1] and $03 <> 0 then
        iStart := 6
      else
        iStart := 7
    end;

    DataCount := 0;
    if ErrorCode = 0 then
    begin
      DataCount := LccMessage.DataCount - iStart;
      for i := 0 to DataCount - 1 do
        DataRawIndexer[i] := LccMessage.DataArrayIndexer[i + iStart];
      case DataType of
        cdt_String :
          begin
            InProcessAddress := InProcessAddress + DWord((LccMessage.DataCount - iStart));
            for i := 0 to LccMessage.DataCount - iStart - 1 do
              FDataTypeString := FDataTypeString + Char( LccMessage.DataArrayIndexer[i+iStart]);

            RemainingCount := DataCount - Length(FDataTypeString);           // Strings are 1 indexed
            if RemainingCount > 64 then
              RemainingCount := 64;
            if RemainingCount > 0 then
            begin
              WorkerMessage.LoadConfigMemRead(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, MSI_CONFIG, InProcessAddress, RemainingCount);
              OwnerManager.DoRequestMessageSend(WorkerMessage);
            end
          end;
        cdt_Int :
          begin
            FDataTypeInteger := LccMessage.ExtractDataBytesAsInt(iStart, LccMessage.DataCount-1);
            RemainingCount := 0;
          end;
        cdt_EventID :
          begin
            FDataTypeEvent := LccMessage.ExtractDataBytesAsEventID(iStart)^;
            RemainingCount := 0;
          end;
        cdt_Bit :
          begin
            // ToDo
          end;
       end
    end;

    if (ErrorCode = 0) or (RemainingCount <= 0) then
    begin
      Valid := ErrorCode = 0;
      SourceNode := OwnerManager.FindSourceNode(LccMessage);
      if Assigned(OwnerManager.CdiParser) then    // Callback on the CDI Parser if available
        OwnerManager.CdiParser.DoConfigMemReadReply(SourceNode);
      OwnerManager.DoConfigMemReadReply(SourceNode, OwnerManager.FindDestNode(LccMessage));
    end;
  end else
  if LccMessage.DataArrayIndexer[1] and MCP_WRITE_REPLY <> 0 then
  begin
    ErrorCode := 0;

    if ErrorCode = 0 then
    begin
      SourceNode := OwnerManager.FindSourceNode(LccMessage);
      if Assigned(OwnerManager.CdiParser) then    // Callback on the CDI Parser if available
        OwnerManager.CdiParser.DoConfigMemWriteReply(SourceNode);
      OwnerManager.DoConfigMemWriteReply(SourceNode, OwnerManager.FindDestNode(LccMessage));
    end;
  end;
end;

procedure TConfigurationMemory.SetDataRawIndexer(iIndex: Word; const Value: Byte);
begin
  FDataRaw[iIndex] := Value
end;

procedure TCDI.DoLoadComplete(LccMessage: TLccMessage);
var
  SourceNode, DestNode: TLccNode;
begin
  if Assigned(OwnerManager) then
  begin
    SourceNode := OwnerManager.FindSourceNode(LccMessage);
    DestNode := OwnerManager.FindDestNode(LccMessage);
    if Assigned(SourceNode) and Assigned(DestNode) then
      OwnerManager.DoCDI(SourceNode, DestNode);
  end;
end;

{ TFDI }

procedure TFDI.DoLoadComplete(LccMessage: TLccMessage);
var
  SourceNode, DestNode: TLccNode;
begin
  if Assigned(OwnerManager) then
  begin
    SourceNode := OwnerManager.FindSourceNode(LccMessage);
    DestNode := OwnerManager.FindDestNode(LccMessage);
    if Assigned(SourceNode) and Assigned(DestNode) then
      OwnerManager.DoFDI(SourceNode, DestNode);
  end;
end;

{ TTraction }

procedure TTraction.SetFunctions(Index: DWord; AValue: Word);
begin
  GrowArray(Index + 1);
  FunctionArray[Index] := AValue
end;

function TTraction.GetFunctions(Index: DWord): Word;
begin
  GrowArray(Index + 1);
  Result := FunctionArray[Index];
end;

procedure TTraction.GrowArray(NewSize: DWord);
var
  OldSize, i: DWord;
begin
  OldSize := Length(FunctionArray);
  if NewSize > OldSize then
  begin
    SetLength(FunctionArray, NewSize);
    i := OldSize;
    while i < NewSize do
    begin
      FunctionArray[i] := 0;
      Inc(i)
    end
  end;
end;

function TTraction.IsLinked: Boolean;
begin
  Result := Assigned(LinkedNode)
end;

function TTraction.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[0] of
    TRACTION_QUERY_SPEED :
        begin
          FSpeed := LccMessage.ExtractDataBytesAsInt(1, 2);
          FSpeedCommanded := LccMessage.ExtractDataBytesAsInt(4, 5);
          FSpeedActual := LccMessage.ExtractDataBytesAsInt(6, 7); ;
        end;
    TRACTION_QUERY_FUNCTION :
        begin
          SetFunctions(LccMessage.ExtractDataBytesAsInt(1, 3), LccMessage.ExtractDataBytesAsInt(4,5))
        end;
    TRACTION_CONTROLLER_CONFIG :
        begin
          case LccMessage.DataArrayIndexer[1] of
            TRACTION_CONTROLLER_CONFIG_ASSIGN :
                begin

                end;
            TRACTION_CONTROLLER_CONFIG_QUERY :
                begin

                end;
            TRACTION_CONTROLLER_CONFIG_NOTIFY :
                begin

                end;
          end;
        end;
    TRACTION_CONSIST :
        begin

        end;
    TRACTION_MANAGE :
        begin

        end;
  end;
end;

{ TNodeProtocolBase }

procedure TNodeProtocolBase.SetOwnerManager(AValue: TLccNodeManager);
begin
  if FOwnerManager=AValue then Exit;
  FOwnerManager:=AValue;
end;

procedure TNodeProtocolBase.SetValid(AValue: Boolean);
begin
  if FValid=AValue then Exit;
  FValid:=AValue;
end;

constructor TNodeProtocolBase.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FCreateTime := GetTickCount;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TNodeProtocolBase.Destroy;
begin
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

{ TLccNodeManager }

function TLccNodeManager.GetNodes(Index: Integer): TLccNode;
begin
  Result := TLccNode( NodeList[Index]);
end;

function TLccNodeManager.GetOwnedNodes(Index: Integer): TLccOwnedNode;
begin
  Result := TLccOwnedNode( OwnedNodeList[Index]);
end;

procedure TLccNodeManager.SetCAN(AValue: Boolean);
begin
  if AValue <> FCAN then
  begin
    FCAN:=AValue;
    if Enabled then
    begin
      Enabled := False;                                                         // ReEnable if the CAN is set while enabled
      Enabled := True;
    end;
  end;
end;

procedure TLccNodeManager.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    if FEnabled then
    begin
      if Assigned(RootNode) then
        RootNode.Login(True, False);
    end else
    begin
      Clear;
      ClearOwned;
      RootNode.SendAMR
    end
  end
end;

procedure TLccNodeManager.SetNodes(Index: Integer; AValue: TLccNode);
begin
  NodeList[Index] := AValue
end;

procedure TLccNodeManager.SetOwnedNodes(Index: Integer; AValue: TLccOwnedNode);
begin
  OwnedNodeList[Index] := AValue
end;

procedure TLccNodeManager.DoAliasIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnAliasIDChanged) then
    OnAliasIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoCDI(SourceLccNode, DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeCDI) then
    OnLccNodeCDI(Self, SourceLccNode, DestLccNode)
end;

procedure TLccNodeManager.DoConfigMemReadReply(SourceLccNode,
  DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemReadReply) then
    OnLccNodeConfigMemReadReply(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoConfigMemWriteReply(SourceLccNode, DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeConfigMemWriteReply) then
    OnLccNodeConfigMemWriteReply(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoCreateLccNode(SourceLccNode: TLccNode);
begin
  if Assigned(OnLccNodeCreate) then
    OnLccNodeCreate(Self, SourceLccNode)
end;

procedure TLccNodeManager.DoConsumerIdentified(SourceLccNode: TLccNode;
  var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeConsumerIdentified) then
    OnLccNodeConsumerIdentified(Self, SourceLccNode, Event, State);
end;

procedure TLccNodeManager.DoDatagramReply(SourceLccNode,
  DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeDatagramReply) then
    OnLccNodeDatagramReply(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoDestroyLccNode(LccNode: TLccNode);
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(CdiParser) then
      CdiParser.NotifyLccNodeDestroy(LccNode);
  end;
  if Assigned(OnLccNodeDestroy) then
    OnLccNodeDestroy(Self, LccNode);
end;

procedure TLccNodeManager.DoFDI(SourceLccNode, DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeFDI) then
    OnLccNodeFDI(Self, SourceLccNode, DestLccNode)
end;

procedure TLccNodeManager.DoFunctionConfiguration(SourceLccNode, DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeFunctionConfiguration) then
    OnLccNodeFunctionConfiguration(Self, SourceLccNode, DestLccNode)
end;

procedure TLccNodeManager.DoGetRootNodeClass( var RootNodeClass: TLccOwnedNodeClass);
begin
  RootNodeClass := TLccDefaultRootNode;
  if Assigned(OnLccGetRootNodeClass) then
    OnLccGetRootNodeClass(Self, RootNodeClass);
end;

procedure TLccNodeManager.DoInitializationComplete(SourceLccNode: TLccNode);
begin
  if Assigned(OnLccNodeInitializationComplete) then
    OnLccNodeInitializationComplete(Self, SourceLccNode);
end;

procedure TLccNodeManager.DoNodeIDChanged(LccNode: TLccNode);
begin
  if Assigned(OnNodeIDChanged) then
    OnNodeIDChanged(Self, LccNode);
end;

procedure TLccNodeManager.DoOptionalInteractionRejected(SourceLccNode,
  DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeOptionalInteractionRejected) then
    OnLccNodeOptionalInteractionRejected(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoProducerIdentified(SourceLccNode: TLccNode; var Event: TEventID; State: TEventState);
begin
  if Assigned(OnLccNodeProducerIdentified) then
    OnLccNodeProducerIdentified(Self, SourceLccNode, Event, State);
end;

procedure TLccNodeManager.DoProtocolIdentifyReply(SourceLccNode, DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeProtocolIdentifyReply) then
    OnLccNodeProtocolIdentifyReply(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoRemoteButtonReply(SourceLccNode, DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeRemoteButtonReply) then
    OnLccNodeRemoteButtonReply(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoRequestMessageSend(Message: TLccMessage);
begin
  if Assigned(HardwareConnection) then
    HardwareConnection.SendMessage(Message);
  if Assigned(OnRequestMessageSend) then
    OnRequestMessageSend(Self, Message);
end;

procedure TLccNodeManager.DoSimpleNodeIdentReply(SourceLccNode,
  DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeSimpleNodeIdentReply) then
    OnLccNodeSimpleNodeIdentReply(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoSimpleTrainNodeIdentReply(SourceLccNode,
  DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeSimpleTrainNodeIdentReply) then
    OnLccNodeSimpleTrainNodeIdentReply(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoTractionControllerChangeNotify(SourceLccNode,
  DestLccNode: TLccNode; NewRequestingNode: TNodeID;
  NewRequestingNodeAlias: Word; var Allow: Boolean);
begin
  if Assigned(OnLccNodeTractionControllerChangeNotify) then
    OnLccNodeTractionControllerChangeNotify(Self, SourceLccNode, DestLccNode, NewRequestingNode, NewRequestingNodeAlias, Allow);
end;

procedure TLccNodeManager.DoTractionProxyReplyAllocate(SourceLccNode,
  DestLccNode: TLccNode; LegacyTechnology: Byte; TrainID: Word;
  var TrainNode: TNodeID; TrainAlias: Word);
begin
  if Assigned(OnLccNodeTractionProxyReplyAllocate) then
    OnLccNodeTractionProxyReplyAllocate(Self, SourceLccNode, DestLccNode, LegacyTechnology, TrainID, TrainNode, TrainAlias);
end;

procedure TLccNodeManager.DoTractionProxyReplyAttach(SourceLccNode,
  DestLccNode: TLccNode; ReplyCode: Byte);
begin
  if Assigned(OnLccNodeTractionProxyReplyAttach) then
    OnLccNodeTractionProxyReplyAttach(Self, SourceLccNode, DestLccNode, ReplyCode);
end;

procedure TLccNodeManager.DoTractionProxyReplyManage(SourceLccNode,
  DestLccNode: TLccNode; ResultCode: Byte);
begin
  if Assigned(OnLccNodeTractionProxyReplyManage) then
    OnLccNodeTractionProxyReplyManage(Self, SourceLccNode, DestLccNode, ResultCode);
end;

procedure TLccNodeManager.DoTractionReplyQuerySpeed(SourceLccNode,
  DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeTractionReplyQuerySpeed) then
    OnLccNodeTractionReplyQuerySpeed(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoTractionReplyQueryFunction(SourceLccNode,
  DestLccNode: TLccNode);
begin
  if Assigned(OnLccNodeTractionReplyQueryFunction) then
    OnLccNodeTractionReplyQueryFunction(Self, SourceLccNode, DestLccNode);
end;

procedure TLccNodeManager.DoTractionReplyControllerAssign(SourceLccNode,
  DestLccNode: TLccNode; ResultCode: Byte);
begin
  if Assigned(OnLccNodeTractionReplyControllerAssign) then
    OnLccNodeTractionReplyControllerAssign(Self, SourceLccNode, DestLccNode, ResultCode);
end;

procedure TLccNodeManager.DoTractionReplyControllerQuery(SourceLccNode,
  DestLccNode: TLccNode; ActiveControllerNodeID: TNodeID;
  ActiveControllerAlias: Word);
begin
  if Assigned(OnLccNodeTractionReplyControllerQuery) then
    OnLccNodeTractionReplyControllerQuery(Self, SourceLccNode, DestLccNode, ActiveControllerNodeID, ActiveControllerAlias);
end;

procedure TLccNodeManager.DoTractionReplyControllerChangeNotify(
  SourceLccNode, DestLccNode: TLccNode; ResultCode: Byte);
begin
  if Assigned(OnLccNodeTractionReplyControllerChangeNotify) then
    OnLccNodeTractionReplyControllerChangeNotify(Self, SourceLccNode, DestLccNode, ResultCode);
end;

procedure TLccNodeManager.DoTractionReplyManage(SourceLccNode,
  DestLccNode: TLccNode; ResultCode: Byte);
begin
  if Assigned(OnLccNodeTractionReplyManage) then
    OnLccNodeTractionReplyManage(Self, SourceLccNode, DestLccNode, ResultCode);
end;

procedure TLccNodeManager.DoVerifiedNodeID(SourceLccNode: TLccNode);
begin
  if Assigned(OnLccNodeVerifiedNodeID) then
    OnLccNodeVerifiedNodeID(Self, SourceLccNode);
end;

function TLccNodeManager.FindSourceNode(LccMessage: TLccMessage): TLccNode;
var
  i: Integer;
begin
  Result := nil;
  i := 0;     // Cheap, slow linear search for now
  while i < NodeList.Count do
  begin
    if Nodes[i].IsNode(LccMessage, ntt_Source) then
    begin
      Result := Nodes[i];
      Break;
    end;
    Inc(i);
  end;
end;

function TLccNodeManager.FindDestNode(LccMessage: TLccMessage): TLccNode;
var
  i: Integer;
begin
  Result := nil;
  i := 0;     // Cheap, slow linear search for now
  while i < NodeList.Count do
  begin
    if Nodes[i].IsNode(LccMessage, ntt_Dest) then
    begin
      Result := Nodes[i];
      Break;
    end;
    Inc(i);
  end;
end;

procedure TLccNodeManager.NodeIDStringToNodeID(ANodeIDStr: string; var ANodeID: TNodeID);
var
  TempStr: string;
  TempNodeID: QWord;
begin
  ANodeIDStr := Trim(ANodeIDStr);
  TempStr := StringReplace(ANodeIDStr, '0x', '', [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(TempStr, '$', '', [rfReplaceAll, rfIgnoreCase]);
  try
    TempNodeID := StrToInt64('$' + TempStr);
    ANodeID[0] := DWord( TempNodeID and $0000000000FFFFFF);
    ANodeID[1] := DWord( (TempNodeID shr 24) and $0000000000FFFFFF);
  except
    ANodeID[0] := 0;
    ANodeID[1]  := 0;
  end;
end;

function TLccNodeManager.NodeIDToNodeIDStr(ANodeID: TNodeID): string;
begin
  Result := IntToHex(ANodeID[1], 6);
  Result := Result + IntToHex(ANodeID[0], 6);
end;

procedure TLccNodeManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TLccCdiParserBase then
  begin
     case Operation of
       opInsert : TLccCdiParserBase(AComponent).SetNodeManager(Self);
       opRemove : TLccCdiParserBase(AComponent).SetNodeManager(nil);
     end;
  end;
end;

constructor TLccNodeManager.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FNodeList := TList.Create;
  FOwnedNodeList := TList.Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TLccNodeManager.Destroy;
begin
  Clear;
  ClearOwned;
  FreeAndNil(FNodeList);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FOwnedNodeList);
  FreeAndNil(FRootNode);
  inherited Destroy;
end;

procedure TLccNodeManager.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to FNodeList.Count - 1 do
      TObject( FNodeList[i]).Free;
  finally
    NodeList.Clear;
  end;
end;

procedure TLccNodeManager.ClearOwned;
var
  i: Integer;
begin
  try
    for i := 0 to FOwnedNodeList.Count - 1 do
      TObject( FOwnedNodeList[i]).Free;
  finally
    OwnedNodeList.Clear;
  end;
end;

function TLccNodeManager.CreateNodeBySourceMessage(LccMessage: TLccMessage): TLccNode;
begin
  Result := TLccNode.Create(Self);
  Result.OwnerManager := Self;
  Result.FAliasID := LccMessage.CAN.SourceAlias;
  Result.FNodeID[0] := LccMessage.SourceID[0];
  Result.FNodeID[1] := LccMessage.SourceID[1];
  NodeList.Add(Result);
end;

function TLccNodeManager.CreateNodeByDestMessage(LccMessage: TLccMessage): TLccNode;
begin
  Result := TLccNode.Create(Self);
  Result.OwnerManager := Self;
  Result.FAliasID := LccMessage.CAN.DestAlias;
  Result.FNodeID[0] := LccMessage.DestID[0];
  Result.FNodeID[1] := LccMessage.DestID[1];
  NodeList.Add(Result);
end;

function TLccNodeManager.CreateOwnedNode: TLccOwnedNode;
begin
  Result := TLccOwnedNode.Create(Self);
  Result.OwnerManager := Self;
  OwnedNodeList.Add(Result);
end;

function TLccNodeManager.EqualEventID(var Event1, Event2: TEventID): Boolean;
var
  i: Integer;
begin
  Result := True;
  i := 0;
  while (i < 8) and Result do
  begin
    if Event1[i] <> Event2[i] then
    begin
      Result := False;
      Break
    end;
    Inc(i);
  end;
end;

function TLccNodeManager.FindNode(ANodeID: TNodeID; ANodeAlias: Word): TLccNode;
var
  i: Integer;
  LccNode: TLccNode;
begin
  Result := nil;
  for i := 0 to NodeList.Count - 1 do
  begin
    LccNode := TLccNode( NodeList[i]);
    if EqualNodeID(ANodeID, LccNode.NodeID, False) or (ANodeAlias = LccNode.AliasID) then
    begin
      Result := LccNode;
      Break
    end;
  end;
end;

function TLccNodeManager.FindOwnedDestNode(LccMessage: TLccMessage): TLccOwnedNode;
var
  i: Integer;
begin
  Result := nil;
  if RootNode.IsNode(LccMessage, ntt_Dest) then
    Result := RootNode
  else begin
    i := 0;     // Cheap, slow linear search for now
    while i < OwnedNodeList.Count do
    begin
      if OwnedNodes[i].IsNode(LccMessage, ntt_Dest) then
      begin
        Result := OwnedNodes[i];
        Break;
      end
    end;
  end;
end;

function TLccNodeManager.FindOwnedSourceNode(LccMessage: TLccMessage): TLccOwnedNode;
var
  i: Integer;
begin
  Result := nil;
  if RootNode.IsNode(LccMessage, ntt_Source) then
    Result := RootNode
  else begin
    i := 0;     // Cheap, slow linear search for now
    while i < OwnedNodeList.Count do
    begin
      if OwnedNodes[i].IsNode(LccMessage, ntt_Source) then
      begin
        Result := OwnedNodes[i];
        Break;
      end
    end;
  end;
end;

function TLccNodeManager.IsManagerNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
begin
  Result := False;
  if Assigned(RootNode) then
  begin
    if TestType = ntt_Dest then
    begin
      if LccMessage.HasDestNodeID and not NullNodeID(RootNode.NodeID) then
        Result := ((RootNode.NodeID[0] = LccMessage.DestID[0]) and (RootNode.NodeID[1] = LccMessage.DestID[1])) or (RootNode.AliasID = LccMessage.CAN.DestAlias)
      else
      if (RootNode.AliasID <> 0) and (LccMessage.CAN.DestAlias <> 0) then
        Result := RootNode.AliasID = LccMessage.CAN.DestAlias
    end else
    if TestType = ntt_Source then
    begin
      if LccMessage.HasSourceNodeID and not NullNodeID(RootNode.NodeID) then
        Result := ((RootNode.NodeID[0] = LccMessage.SourceID[0]) and (RootNode.NodeID[1] = LccMessage.SourceID[1])) or (RootNode.AliasID = LccMessage.CAN.SourceAlias)
      else
      if (RootNode.AliasID <> 0) and (LccMessage.CAN.SourceAlias <> 0) then
        Result := RootNode.AliasID = LccMessage.CAN.SourceAlias
    end;
  end
end;

procedure TLccNodeManager.Loaded;
var
  RootNodeClass: TLccOwnedNodeClass;
begin
  inherited Loaded;
  RootNodeClass := nil;
  DoGetRootNodeClass(RootNodeClass);
  FRootNode := RootNodeClass.Create(nil);
  FRootNode.OwnerManager := Self;
  DoCreateLccNode(FRootNode);
end;

function TLccNodeManager.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  LccSourceNode: TLccNode;
  LccOwnedNode: TLccOwnedNode;
  i: Integer;
begin
  Result := True;
  if Enabled then
  begin
    // First see if we have the node on the network that sent this message and allow
    // the NodeList to update those objects that are images of those nodes.
    LccSourceNode := FindSourceNode(LccMessage);
    if not Assigned(LccSourceNode) and (RootNode.Initialized) then
    begin                                                                         // We don't have an image of this node in our database
      LccSourceNode := CreateNodeBySourceMessage(LccMessage);
      DoCreateLccNode(LccSourceNode);
      LccSourceNode.ProcessMessage(LccMessage);
      if LccMessage.MTI = MTI_VERIFIED_NODE_ID_NUMBER then
      begin
        if AutoInterrogateDiscoveredNodes then
        begin
          // Most likely reply from booting up so we need to get the events
          WorkerMessage.LoadIdentifyEventsAddressed(RootNode.NodeID, RootNode.AliasID, LccSourceNode.NodeID, LccSourceNode.AliasID);
          DoRequestMessageSend(WorkerMessage);
        end;
      end else
      if LccMessage.MTI = MTI_INITIALIZATION_COMPLETE then
      begin
        // Events will be coming as part of the initialization sequence
      end else
      begin
        if AutoInterrogateDiscoveredNodes then
        begin
          // Found a node some other way so need to get it information (NodeID and Events)
          WorkerMessage.LoadVerifyNodeIDAddressed(RootNode.NodeID, RootNode.AliasID, LccSourceNode.NodeID, LccSourceNode.AliasID);
          DoRequestMessageSend(WorkerMessage);
          WorkerMessage.LoadIdentifyEventsAddressed(RootNode.NodeID, RootNode.AliasID, LccSourceNode.NodeID, LccSourceNode.AliasID);
          DoRequestMessageSend(WorkerMessage);
        end
      end;
    end else
      LccSourceNode.ProcessMessage(LccMessage);

    // Now handle messages that are directed to our internally created and mananged nodes
    if LccMessage.HasDestination then
    begin
      LccOwnedNode := FindOwnedDestNode(LccMessage);
      if Assigned(LccOwnedNode) then
        LccOwnedNode.ProcessMessage(LccMessage)
      else begin
        LccOwnedNode := FindOwnedSourceNode(LccMessage);
        if Assigned(LccOwnedNode) then
          LccOwnedNode.ProcessMessage(LccMessage)   // this will throw an error and reallocate the alias
      end;
    end else
    begin
      RootNode.ProcessMessage(LccMessage);
      for i := 0 to OwnedNodeList.Count - 1 do
        TLccOwnedNode( OwnedNodeList[i]).ProcessMessage(LccMessage);
    end;
  end
end;

{ TSimpleTrainNodeInfo }

function TSimpleTrainNodeInfo.ProcessMessage(LccMessage: TLccMessage;
  Traction: TTraction): Boolean;

    function NextString(AStrPtr: PChar): PChar;
    begin
      Result := AStrPtr;
      while Result^ <> #0 do
        Inc(Result);
      Inc(Result);
    end;

var
  StrPtr: PChar;
begin
  Result := True;
  StrPtr := @LccMessage.DataArray[0];

  FVersion := Ord( StrPtr^);
  Inc(StrPtr);
  FRoadname := StrPtr;
  StrPtr := NextString(StrPtr);
  FTrainClass := StrPtr;
  StrPtr := NextString(StrPtr);
  FRoadNumber := StrPtr;
  StrPtr := NextString(StrPtr);
  FTrainName := StrPtr;
  StrPtr := NextString(StrPtr);
  FManufacturer := StrPtr;
  StrPtr := NextString(StrPtr);
  FOwner := StrPtr;
  StrPtr := NextString(StrPtr);
  Traction.LegacyTechnology := Ord(StrPtr^);
  Inc(StrPtr);
  Traction.LegacyTrainID := Ord( StrPtr^) shl 8;
  Inc(StrPtr);
  Traction.LegacyTrainID := Ord(StrPtr^) or Traction.LegacyTrainID;
  Inc(StrPtr);
  if Ord( StrPtr^) > 0 then
    Traction.LegacyTrainID := Traction.LegacyTrainID or $C000;
  Inc(StrPtr);
  case Ord(StrPtr^) of
    0 : Traction.LegacySpeedSteps := 14;
    1 : Traction.LegacySpeedSteps := 28;
    2 : Traction.LegacySpeedSteps := 128
  else
    Traction.LegacySpeedSteps := 28;
  end;
  Valid := True;
end;

{ TSimpleNodeInfo }

function TSimpleNodeInfo.GetPackedFormat: TSimpleNodeInfoPacked;
const
  NULL_COUNT = 6;
  VERSION_COUNT = 2;
var
  iArray, i: Integer;
begin
  i :=  Length(FManufacturer) + Length(FModel) + Length(FHardwareVersion) + Length(FSoftwareVersion) + Length(UserName) + Length(UserDescription);
  i := i + NULL_COUNT + VERSION_COUNT;
  SetLength(FPackedInfo, i);
  iArray := 0;

  FPackedInfo[iArray] := Version;      // 4 Items follow
  Inc(iArray);
  StringToNullArray(FManufacturer, FPackedInfo, iArray);
  StringToNullArray(FModel, FPackedInfo, iArray);
  StringToNullArray(FHardwareVersion, FPackedInfo, iArray);
  StringToNullArray(FSoftwareVersion, FPackedInfo, iArray);

  FPackedInfo[iArray] := UserVersion;  // 2 items follow
  Inc(iArray);
  StringToNullArray(FUserName, FPackedInfo, iArray);
  StringToNullArray(FUserDescription, FPackedInfo, iArray);

  Result := FPackedInfo;
end;

function TSimpleNodeInfo.ProcessMessage(LccMessage: TLccMessage): Boolean;

  function NextString(AStrPtr: PChar): PChar;
  begin
    Result := AStrPtr;
    while Result^ <> #0 do
      Inc(Result);
    Inc(Result);
  end;

var
  StrPtr: PChar;
begin
  Result := True;
  StrPtr := @LccMessage.DataArray[0];
  FVersion := Ord(StrPtr^);
  Inc(StrPtr);
  FManufacturer := StrPtr;
  StrPtr := NextString(StrPtr);
  FModel := StrPtr;
  StrPtr := NextString(StrPtr);
  FHardwareVersion := StrPtr;
  StrPtr := NextString(StrPtr);
  FSoftwareVersion := StrPtr;
  StrPtr := NextString(StrPtr);
  FUserVersion := Ord(StrPtr^);
  Inc(StrPtr);
  FUserName := StrPtr;
  StrPtr := NextString(StrPtr);
  FUserDescription := StrPtr;
  Valid := True;
end;

{ TSimpleNodeInfo }

{function TSimpleNodeInfo.GetPackedFormat: TSimpleNodeInfoPacked;
begin

end;

 function TSimpleNodeInfo.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin

end;

TProtocolSupport }

procedure TProtocolSupport.DecodeFlags;
begin
  if Length(Flags) > 0 then
  begin
    FACDI := Flags[0] and PIP_ABBREVIATED_CDI <> 0;
    FCDI := Flags[0] and PIP_CDI <> 0;
    FDatagram := Flags[0] and PIP_DATAGRAM <> 0;
    FDisplay := Flags[0] and PIP_DISPLAY <> 0;
    FEventExchange := Flags[0] and PIP_EVENT_EXCHANGE <> 0;
    FFDI := Flags[0] and PIP_FDI <> 0;
    FIdentification := Flags[0] and PIP_PIP <> 0;
    FMemConfig := Flags[0] and PIP_MEMORY_CONFIG <> 0;
    FRemoteButton := Flags[0] and PIP_REMOTE_BUTTON <> 0;
    FReservation := Flags[0] and PIP_RESERVATION <> 0;
    FSimpleNodeInfo := Flags[0] and PIP_SIMPLE_NODE_INFO <> 0;
    FSimpleTrainNodeInfo := Flags[0] and PIP_SIMPLE_TRAIN_NODE_INFO <> 0;
    FStream := Flags[0] and PIP_STREAM <> 0;
    FTeach_Learn := Flags[0] and PIP_TEACH_LEARN <> 0;
    FTractionControl := Flags[0] and PIP_TRACTION <> 0;
    FTractionProxy := Flags[0] and PIP_TRACTION_PROXY <> 0;
    FFunctionConfiguration := Flags[0] and PIP_FUNCTION_CONFIGURATION <> 0;
    Valid := True;
  end;
end;

function TProtocolSupport.EncodeFlags: QWord;
begin
  Result := 0;
  if ACDI then Result := Result or PIP_ABBREVIATED_CDI;
  if CDI then Result := Result or PIP_CDI;
  if Datagram then Result := Result or PIP_DATAGRAM;
  if Display then Result := Result or PIP_DISPLAY;
  if EventExchange then Result := Result or PIP_EVENT_EXCHANGE;
  if FDI then Result := Result or PIP_FDI;
  if Identification then Result := Result or PIP_PIP;
  if MemConfig then Result := Result or PIP_MEMORY_CONFIG;
  if RemoteButton then Result := Result or PIP_REMOTE_BUTTON;
  if Reservation then Result := Result or PIP_RESERVATION;
  if SimpleNodeInfo then Result := Result or PIP_SIMPLE_NODE_INFO;
  if SimpleTrainNodeInfo then Result := Result or PIP_SIMPLE_TRAIN_NODE_INFO;
  if Stream then Result := Result or PIP_STREAM;
  if Teach_Learn then Result := Result or PIP_TEACH_LEARN;
  if TractionControl then Result := Result or PIP_TRACTION;
  if TractionProxy then Result := Result or PIP_TRACTION_PROXY;
  if FunctionConfiguration then Result := Result or PIP_FUNCTION_CONFIGURATION;
end;

function TProtocolSupport.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  i, FlagBlocks, Offset: Integer;
begin
  Result := True;
  FlagBlocks := LccMessage.DataCount div 6;
  SetLength(Flags, FlagBlocks);
  Offset := 0;
  for i := 0 to FlagBlocks - 1 do
  begin
    Flags[i] := LccMessage.ExtractDataBytesAsInt(Offset, 5);     // Protocol uses 6 byte chunks due to needing to use 2 in the CAN for the destination
    Offset := Offset + 6;
  end;
  DecodeFlags;
end;

{ TStreamBasedProtocol }

procedure TStreamBasedProtocol.SetValid(AValue: Boolean);
begin
  inherited SetValid(AValue);
  if not AValue then
  begin
    AStream.Size := 0;
    InProcessAddress := 0;
  end
end;

constructor TStreamBasedProtocol.Create(AnOwner: TComponent; AnAddressSpace: Byte);
begin
  inherited Create(AnOwner);
  FStream := TMemoryStream.Create;
  FAddressSpace := AnAddressSpace;
end;

destructor TStreamBasedProtocol.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TStreamBasedProtocol.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
var
  i: Integer;
  iStart, ReadCount: Integer;
  AByte: Byte;
begin
  // Assumption is this is a datagram message
  if LccMessage.DataArrayIndexer[1] and $03 = 0 then
    iStart := 7
  else
    iStart := 6;
  ReadCount := LccMessage.DataArrayIndexer[iStart];
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArrayIndexer[0];
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArrayIndexer[1] or $10;   // Make it a reply
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];    // Copy the address
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArrayIndexer[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArrayIndexer[5];

  if AStream.Size = 0 then
  begin
    OutMessage.DataCount := iStart + 1;
    OutMessage.DataArrayIndexer[iStart] := Ord(#0);
  end else
  begin
    AStream.Position := LccMessage.ExtractDataBytesAsInt(2, 5);
    i := 0;
    while (AStream.Position < AStream.Size) and (i < ReadCount) do
    begin
      AByte := 0;
      AStream.Read(AByte, 1);
      OutMessage.DataArrayIndexer[iStart + i] := AByte;
      Inc(i);
    end;
    OutMessage.DataCount := iStart + i;

    if AStream.Position = AStream.Size then
    begin
      OutMessage.DataArrayIndexer[OutMessage.DataCount] := Ord(#0);
      OutMessage.DataCount := OutMessage.DataCount + 1
    end;
  end;
  OutMessage.UserValid := True;
end;

function TStreamBasedProtocol.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  NullFound: Boolean;
  i: Integer;
  iStart: Integer;
  AByte: Byte;
begin
  Result := True;
  if not Valid then
  begin
    NullFound := False;
    if LccMessage.DataArrayIndexer[1] and $03 = 0 then
      iStart := 7
    else
      iStart := 6;
    for i := iStart to LccMessage.DataCount - 1 do
    begin
      NullFound := LccMessage.DataArrayIndexer[i] = Ord(#0);
      AByte := LccMessage.DataArrayIndexer[i];
      AStream.WriteBuffer(AByte, 1);
      if NullFound then
        Break
    end;

    if NullFound then
    begin
      AStream.Position := 0;
      FValid := True;
      DoLoadComplete(LccMessage);
    end else
    begin
      WorkerMessage.CANOnly := False;
      WorkerMessage.SourceID := LccMessage.DestID;
      WorkerMessage.CAN.SourceAlias := LccMessage.CAN.DestAlias;
      WorkerMessage.DestID := LccMessage.SourceID;
      WorkerMessage.CAN.DestAlias := LccMessage.CAN.SourceAlias;
      WorkerMessage.DataCount := 0;
      WorkerMessage.DataArrayIndexer[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
      WorkerMessage.DataArrayIndexer[1] := MCP_READ;
      InProcessAddress := InProcessAddress + 64 {- iStart};
      WorkerMessage.InsertDWordAsDataBytes(InProcessAddress, 2);
      WorkerMessage.DataArrayIndexer[6] := AddressSpace;
      WorkerMessage.DataArrayIndexer[7] := 64;                     // Read until the end.....
      WorkerMessage.DataCount := 8;
      WorkerMessage.MTI := MTI_DATAGRAM;
      OwnerManager.DoRequestMessageSend(WorkerMessage);
    end;
  end;
end;

{ TLccNode }

procedure TLccNode.SetOwnerManager(AValue: TLccNodeManager);
begin
  inherited SetOwnerManager(AValue);
  ProtocolSupport.OwnerManager := AValue;
  SimpleTrainNodeInfo.OwnerManager := AValue;
  SimpleTrainNodeInfo.OwnerManager := AValue;
  FDI.OwnerManager := AValue;
  CDI.OwnerManager := AValue;
  ConfigurationMem.OwnerManager := AValue;
  FunctionConfiguration.OwnerManager := AValue;
end;

function TLccNode.ExtractAddressSpace(LccMessage: TLccMessage): Byte;
begin
  Result := 0;
  case LccMessage.DataArrayIndexer[1] and $03 of
    0 : Result := LccMessage.DataArrayIndexer[6];
    1 : Result := MSI_CONFIG;
    2 : Result := MSI_ALL;
    3 : Result := MSI_CDI;
  end;
end;

constructor TLccNode.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FProtocolSupport := TProtocolSupport.Create(AnOwner);
  FSimpleNodeInfo := TSimpleNodeInfo.Create(AnOwner);
  FSimpleTrainNodeInfo := TSimpleTrainNodeInfo.Create(AnOwner);
  FCDI := TCDI.Create(AnOwner, MSI_CDI);
  FFDI := TFDI.Create(AnOwner, MSI_FDI);
  FTraction := TTraction.Create(AnOwner);
  FConfigurationMem := TConfigurationMemory.Create(AnOwner);
  FFunctionConfiguration := TFunctionConfiguration.Create(AnOwner);
  FiStartupSequence := 0;
  FEventsConsumed := TLccEvents.Create(AnOwner);
  FEventsProduced := TLccEvents.Create(AnOwner);
  FConfigurationMemOptions := TConfigurationMemOptions.Create(AnOwner);
  FConfigMemAddressSpaceInfo := TConfigMemAddressSpaceInfo.Create(AnOwner);
end;

destructor TLccNode.Destroy;
begin
  FreeAndNil(FProtocolSupport);
  FreeAndNil(FSimpleNodeInfo);
  FreeAndNil(FSimpleTrainNodeInfo);
  FreeAndNil(FFDI);
  FreeAndNil(FFunctionConfiguration);
  FreeAndNil(FCDI);
  FreeAndNil(FTraction);
  FreeAndNil(FConfigurationMem);
  FreeAndNil(FEventsConsumed);
  FreeAndNil(FEventsProduced);
  FreeAndNil(FConfigurationMemOptions);
  FreeAndNil(FConfigMemAddressSpaceInfo);
  if Assigned(OwnerManager) then
    OwnerManager.DoDestroyLccNode(Self);
  inherited;
end;

function TLccNode.IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
begin
  Result := False;
  if TestType = ntt_Dest then
  begin
    if LccMessage.HasDestNodeID and not NullNodeID(NodeID) then
      Result := ((NodeID[0] = LccMessage.DestID[0]) and (NodeID[1] = LccMessage.DestID[1])) or (AliasID = LccMessage.CAN.DestAlias)
    else
    if (AliasID <> 0) and (LccMessage.CAN.DestAlias <> 0) then
      Result := AliasID = LccMessage.CAN.DestAlias
  end else
  if TestType = ntt_Source then
  begin
    if LccMessage.HasSourceNodeID and not NullNodeID(NodeID) then
      Result := ((NodeID[0] = LccMessage.SourceID[0]) and (NodeID[1] = LccMessage.SourceID[1])) or (AliasID = LccMessage.CAN.SourceAlias)
    else
    if (AliasID <> 0) and (LccMessage.CAN.SourceAlias <> 0) then
      Result := AliasID = LccMessage.CAN.SourceAlias
  end;
end;

function TLccNode.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  ANodeID: TNodeID;
  Allow: Boolean;
  LccDestNode: TLccNode;
  EventPtr: PEventID;
begin
  Result := True;
  ANodeID[0] := 0;
  ANodeID[1] := 0;
  LccDestNode := nil;

  if Assigned(OwnerManager) then
  begin
    if LccMessage.HasDestination then
  //    if not OwnerManager.IsManagerNode(LccMessage) then
      begin
        LccDestNode := OwnerManager.FindDestNode(LccMessage);
        if not Assigned(LccDestNode) then
        begin
          LccDestNode := OwnerManager.CreateNodeByDestMessage(LccMessage);
          if Assigned(OwnerManager) and OwnerManager.AutoInterrogateDiscoveredNodes then
          begin
            // Have other nodes send out Verified
            WorkerMessage.LoadVerifyNodeIDAddressed(NodeID, AliasID, LccDestNode.NodeID, LccDestNode.AliasID);
            OwnerManager.DoRequestMessageSend(WorkerMessage);
          end;
        end;
      end;
  end;

  case LccMessage.MTI of
    MTI_INITIALIZATION_COMPLETE :
        begin
          if Assigned(OwnerManager) then
          begin
            FAliasID := LccMessage.CAN.SourceAlias;
            LccMessage.ExtractDataBytesAsNodeID(0, FNodeID);
            OwnerManager.DoInitializationComplete(Self);
          end;
        end;
    MTI_PROTOCOL_SUPPORT_REPLY :
        begin
          ProtocolSupport.ProcessMessage(LccMessage);
          if Assigned(OwnerManager) then
            OwnerManager.DoProtocolIdentifyReply(Self, LccDestNode);
        end;
    MTI_VERIFY_NODE_ID_NUMBER :
        begin
        end;
    MTI_VERIFY_NODE_ID_NUMBER_DEST :
        begin
        end;
    MTI_VERIFIED_NODE_ID_NUMBER :
        begin
          FAliasID := LccMessage.CAN.SourceAlias;
          LccMessage.ExtractDataBytesAsNodeID(0, FNodeID);
          if Assigned(OwnerManager) then
            OwnerManager.DoVerifiedNodeID(Self);
        end;
    MTI_SIMPLE_NODE_INFO_REPLY  :
        begin
          Inc(TotalSNIPMessages);
          SimpleNodeInfo.ProcessMessage(LccMessage);
          if Assigned(OwnerManager) then
            OwnerManager.DoSimpleNodeIdentReply(Self, LccDestNode);
        end;
    MTI_SIMPLE_TRAIN_INFO_REPLY :
        begin
          Inc(TotalSTNIPMessage);
          SimpleTrainNodeInfo.ProcessMessage(LccMessage, Traction);
          if Assigned(OwnerManager) then
            OwnerManager.DoSimpleTrainNodeIdentReply(Self, LccDestNode);
        end;
    MTI_PRODUCER_IDENTIFIED_SET :
        begin
          EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
          EventsProduced.Add(EventPtr^, evs_Valid);
          if Assigned(OwnerManager) then
            OwnerManager.DoProducerIdentified(Self, EventPtr^ , evs_Valid);
        end;
    MTI_PRODUCER_IDENTIFIED_CLEAR :
        begin
          EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
          EventsProduced.Add(EventPtr^, evs_InValid);
          if Assigned(OwnerManager) then
            OwnerManager.DoProducerIdentified(Self, EventPtr^, evs_InValid);
        end;
    MTI_PRODUCER_IDENTIFIED_UNKNOWN :
        begin
          EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
          EventsProduced.Add(EventPtr^, evs_Unknown);
          if Assigned(OwnerManager) then
            OwnerManager.DoProducerIdentified(Self, EventPtr^, evs_Unknown);
        end;
    MTI_CONSUMER_IDENTIFIED_SET :
        begin
          EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
          EventsConsumed.Add(EventPtr^, evs_Valid);
          if Assigned(OwnerManager) then
            OwnerManager.DoConsumerIdentified(Self, EventPtr^, evs_Valid);
        end;
    MTI_CONSUMER_IDENTIFIED_CLEAR :
        begin
          EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
          EventsConsumed.Add(EventPtr^, evs_InValid);
          if Assigned(OwnerManager) then
            OwnerManager.DoConsumerIdentified(Self, EventPtr^, evs_InValid);
        end;
    MTI_CONSUMER_IDENTIFIED_UNKNOWN :
        begin
          EventPtr := LccMessage.ExtractDataBytesAsEventID(0);
          EventsConsumed.Add(EventPtr^, evs_Unknown);
          if Assigned(OwnerManager) then
            OwnerManager.DoConsumerIdentified(Self, EventPtr^, evs_Unknown);
        end;
    MTI_TRACTION_PROTOCOL :
        begin
          if Assigned(OwnerManager) then
          begin
            case LccMessage.DataArrayIndexer[0] of
              TRACTION_CONTROLLER_CONFIG :
                  begin
                    case LccMessage.DataArrayIndexer[1] of
                        TRACTION_CONTROLLER_CONFIG_NOTIFY :
                          begin
                            Allow := True;
                            OwnerManager.DoTractionControllerChangeNotify(Self, LccDestNode, LccMessage.ExtractDataBytesAsNodeID(3, ANodeID)^, LccMessage.ExtractDataBytesAsInt(9, 10), Allow);
                            WorkerMessage.LoadTractionControllerChangeNotifyReply(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, Allow);
                            OwnerManager.DoRequestMessageSend(WorkerMessage);
                          end;
                    end;
                  end;
            end
          end
        end;
    MTI_TRACTION_REPLY :
        begin
          ANodeID := NULL_NODE_ID;
          Traction.ProcessMessage(LccMessage);
          if Assigned(OwnerManager) then
          begin
            case LccMessage.DataArrayIndexer[0] of
              TRACTION_QUERY_SPEED : OwnerManager.DoTractionReplyQuerySpeed(Self, LccDestNode);
              TRACTION_QUERY_FUNCTION : OwnerManager.DoTractionReplyQueryFunction(Self, LccDestNode);
              TRACTION_CONTROLLER_CONFIG :
                  begin
                    case LccMessage.DataArrayIndexer[1] of
                      TRACTION_CONTROLLER_CONFIG_ASSIGN :
                          begin
                            OwnerManager.DoTractionReplyControllerAssign(Self, LccDestNode, LccMessage.DataArrayIndexer[2]);
                          end;
                      TRACTION_CONTROLLER_CONFIG_QUERY  :
                          begin
                            if LccMessage.DataArrayIndexer[2] and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                              OwnerManager.DoTractionReplyControllerQuery(Self, LccDestNode, LccMessage.ExtractDataBytesAsNodeID(3, ANodeID)^, LccMessage.ExtractDataBytesAsInt(9, 10))
                            else
                              OwnerManager.DoTractionReplyControllerQuery(Self, LccDestNode, LccMessage.ExtractDataBytesAsNodeID(3, ANodeID)^, 0);
                          end;
                      TRACTION_CONTROLLER_CONFIG_NOTIFY :
                          begin
                            OwnerManager.DoTractionReplyControllerChangeNotify(Self, LccDestNode, LccMessage.DataArrayIndexer[2]);
                          end;
                    end;
                  end;
              TRACTION_MANAGE : OwnerManager.DoTractionReplyManage(Self, LccDestNode, LccMessage.DataArrayIndexer[2]);
            end;
          end;
        end;
    MTI_TRACTION_PROXY_REPLY :
        begin
          if Assigned(OwnerManager) then
          begin
            case LccMessage.DataArrayIndexer[0] of
              TRACTION_PROXY_MANAGE   :
                  begin
                    OwnerManager.DoTractionProxyReplyManage(Self, LccDestNode, LccMessage.DataArrayIndexer[2]);
                  end;
              TRACTION_PROXY_ALLOCATE :
                  begin
                    if LccMessage.DataArrayIndexer[1] and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                      OwnerManager.DoTractionProxyReplyAllocate(Self, LccDestNode, LccMessage.DataArrayIndexer[1], LccMessage.ExtractDataBytesAsInt(3, 4), LccMessage.ExtractDataBytesAsNodeID(5, ANodeID)^, LccMessage.ExtractDataBytesAsInt(11, 12))
                    else
                      OwnerManager.DoTractionProxyReplyAllocate(Self, LccDestNode, LccMessage.DataArrayIndexer[1], LccMessage.ExtractDataBytesAsInt(3, 4), LccMessage.ExtractDataBytesAsNodeID(5, ANodeID)^, 0);
                  end;
              TRACTION_PROXY_ATTACH   :
                  begin
                    OwnerManager.DoTractionProxyReplyAttach(Self, LccDestNode, LccMessage.DataArrayIndexer[1]);
                  end;
              else begin
                // Something is broken but don't allow the Reservation to be stuck Reserved, Releasing it will not hurt
                  WorkerMessage.LoadTractionProxyManage(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, False);
                  OwnerManager.DoRequestMessageSend(WorkerMessage);
                end
              end; // Case
            end;
          end;
      MTI_DATAGRAM :
          begin
            case LccMessage.DataArrayIndexer[0] of
              DATAGRAM_PROTOCOL_CONFIGURATION :
                  begin
                    case LccMessage.DataArrayIndexer[1] and $F0 of
                      MCP_READ              : begin end;
                      MCP_READ_STREAM       : begin end;
                      MCP_READ_REPLY        :
                          begin
                            if LccMessage.DataArrayIndexer[1] and $08 = 0 then
                            begin
                              // Ok
                              case ExtractAddressSpace(LccMessage) of
                                MSI_CDI             : CDI.ProcessMessage(LccMessage);
                                MSI_ALL             : begin end;
                                MSI_CONFIG          : ConfigurationMem.ProcessMessage(LccMessage);
                                MSI_ACDI_MFG        : begin end;
                                MSI_ACDI_USER       : begin end;
                                MSI_FDI             : FDI.ProcessMessage(LccMessage);
                                MSI_FUNCTION_CONFIG : FunctionConfiguration.ProcessMessage(LccMessage);
                              end;
                            end else
                            begin
                              // Failure
                            end;
                          end;
                      MCP_READ_STREAM_REPLY : begin end;
                      MCP_WRITE             : begin end;
                      MCP_WRITE_STREAM      : begin end;
                      MCP_WRITE_REPLY       :
                          begin
                            if LccMessage.DataArrayIndexer[1] and $08 = 0 then
                            begin
                              // Ok
                              case ExtractAddressSpace(LccMessage) of
                                MSI_CDI              : begin end; // Not writable
                                MSI_ALL              : begin end; // Not writeable
                                MSI_CONFIG           : ConfigurationMem.ProcessMessage(LccMessage);
                                MSI_ACDI_MFG         : begin end;
                                MSI_ACDI_USER        : begin end;
                                MSI_FDI              : begin end; // Not writeable
                                MSI_FUNCTION_CONFIG  : FunctionConfiguration.ProcessMessage(LccMessage);
                              end;
                            end else
                            begin
                              // Failure
                            end;
                          end;
                      MCP_OPERATION         :
                          begin
                            ConfigurationMemOptions.ProcessMessage(LccMessage);
                          end;
                  end;
            end;
        end;
      end;
  end;
end;


{ TConfigMemAddressSpaceInfo }

procedure TConfigMemAddressSpaceInfo.Add(_Space: Byte; _IsPresent, _IsReadOnly, _ImpliedZeroLowAddress: Boolean; _LowAddress, _HighAddress: DWord);
var
  Info: TConfigMemAddressSpaceInfoObject;
begin
  Info := TConfigMemAddressSpaceInfoObject.Create;
  Info.FAddressSpace := _Space;
  Info.FIsPresent := _IsPresent;
  Info.FIsReadOnly := _IsReadOnly;
  Info.FImpliedZeroLowAddress := _ImpliedZeroLowAddress;
  Info.FLowAddress := _LowAddress;
  Info.FHighAddress := _HighAddress;
  List.Add(Info);
end;

procedure TConfigMemAddressSpaceInfo.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject(List[i]).Free;
  finally
    List.Clear
  end;
end;

constructor TConfigMemAddressSpaceInfo.Create(AnOwner: TComponent);
begin
  inherited;
  List := TList.Create;
end;

destructor TConfigMemAddressSpaceInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TConfigMemAddressSpaceInfo.FindByAddressSpace(Space: Byte): TConfigMemAddressSpaceInfoObject;
var
  i: Integer;
begin
  i := 0;
  Result := nil;
  while (i < Count) and not Assigned(Result) do
  begin
    if AddressSpace[i].AddressSpace = Space then
      Result := AddressSpace[i];
    Inc(i);
  end;
end;

function TConfigMemAddressSpaceInfo.GetAddressSpace(Index: Integer): TConfigMemAddressSpaceInfoObject;
begin
  Result := TConfigMemAddressSpaceInfoObject( List[Index])
end;

function TConfigMemAddressSpaceInfo.GetCount: Integer;
begin
  Result := List.Count
end;

procedure TConfigMemAddressSpaceInfo.LoadReply(LccMessage, OutMessage: TLccMessage);
var
  Info: TConfigMemAddressSpaceInfoObject;
begin
   // Decode the LccMessage
  Info := FindByAddressSpace( LccMessage.DataArrayIndexer[2]);
  if Assigned(Info) then
  begin
    OutMessage.DataArrayIndexer[1] := MCP_OP_GET_ADD_SPACE_INFO_REPLY;
    if Info.IsPresent then
      OutMessage.DataArrayIndexer[1] := OutMessage.DataArrayIndexer[1] or MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT;
    OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];
    OutMessage.DataArrayIndexer[3] := _Highest(Info.FHighAddress);
    OutMessage.DataArrayIndexer[4] := _Higher(Info.FHighAddress);
    OutMessage.DataArrayIndexer[5] := _Hi(Info.FHighAddress);
    OutMessage.DataArrayIndexer[6] := _Lo(Info.FHighAddress);
    OutMessage.DataArrayIndexer[7] := 0;
    if Info.IsReadOnly then
      OutMessage.DataArrayIndexer[7] := OutMessage.DataArrayIndexer[7] or $01;
    OutMessage.DataCount := 8;
    if not Info.ImpliedZeroLowAddress then
    begin
      OutMessage.DataArrayIndexer[8] := _Highest(Info.FLowAddress);
      OutMessage.DataArrayIndexer[9] := _Higher(Info.FLowAddress);
      OutMessage.DataArrayIndexer[10] := _Hi(Info.FLowAddress);
      OutMessage.DataArrayIndexer[11] := _Lo(Info.FLowAddress);
      OutMessage.DataCount := 12;
    end;
  end else
  begin
    OutMessage.DataArrayIndexer[1] := MCP_OP_GET_ADD_SPACE_INFO_REPLY;  // Not present
    OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];
    OutMessage.DataArrayIndexer[3] := 0;
    OutMessage.DataArrayIndexer[4] := 0;
    OutMessage.DataArrayIndexer[5] := 0;
    OutMessage.DataArrayIndexer[6] := 0;
    OutMessage.DataArrayIndexer[7] := $01;
    OutMessage.DataCount := 8;
  end;
  OutMessage.UserValid := True;
end;

function TConfigMemAddressSpaceInfo.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := True
end;

{ TACDIMfg }

procedure TACDIMfg.DoLoadComplete(LccMessage: TLccMessage);
begin

end;

{ TConfiguration }

procedure TConfiguration.DoLoadComplete(LccMessage: TLccMessage);
begin

end;

initialization
  TotalSNIPMessages := 0;
  TotalSTNIPMessage := 0;
  RegisterClass(TLccNodeManager);

finalization

end.


