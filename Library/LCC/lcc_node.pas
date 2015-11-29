unit lcc_node;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
  laz2_DOM,
  laz2_XMLRead,
  fptimer,
  contnrs,
  {$ELSE}
  FMX.Types,
  Generics.Collections,
  {$ENDIF}
  lcc_app_common_settings,
  lcc_node_protocol_helpers,
  lcc_math_float16,
  lcc_messages,
  lcc_defines,
  lcc_utilities;

const
  ABANDON_DATAGRAM_TIMEOUT = 5;   // 5 seconds and a queued datagram is flagged as abandon and will be freed
  ABANDON_DATAGRAM_RETRYS = 5;       // Will try a max of 5 times to send a datagram, if rejects 5 time we will give up

type
  { TLccCoreNode }

  TLccCoreNode = class(TBaseLccNode)
  private
    FACDIMfg: TLccACDIMfg;
    FACDIUser: TLccACDIUser;
    FAckWorkerMessage: TLccMessage;
    FCDI: TLccCDI;
    FConfiguration: TLccConfiguration;
    FConfigurationMem: TLccConfigurationMemory;
    FConfigMemOptions: TLccConfigurationMemOptions;
    FEnabled: Boolean;
    FEventsConsumed: TLccEvents;
    FEventsProduced: TLccEvents;
    FFDI: TLccFDI;
    FFunctionConfiguration: TLccFunctionConfiguration;
    FOnRequestSendMessage: TOnMessageEvent;
    FProtocolSupport: TLccProtocolSupport;
    FSimpleNodeInfo: TLccSimpleNodeInfo;
    FSimpleTrainNodeInfo: TLccSimpleTrainNodeInfo;
    FTraction: TLccTraction;
    FConfigMemAddressSpaceInfo: TLccConfigMemAddressSpaceInfo;

    function GetAliasIDStr: String;
    function GetEnabled: Boolean; virtual;
    function GetNodeIDStr: String;
    procedure SetEnabled(AValue: Boolean); virtual;
  protected
    FAliasID: Word;
    FInitialized: Boolean;
    FNodeID: TNodeID;
    FPermitted: Boolean;

    function GetInitialized: Boolean; virtual;
    function GetPermitted: Boolean; virtual;

    property AckWorkerMessage: TLccMessage read FAckWorkerMessage write FAckWorkerMessage;
    property Initialized: Boolean read GetInitialized;
    property Permitted: Boolean read GetPermitted;

    function DoCanAMR(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoCanAME(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoCanAMD(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoCanRID(LccMessage: TLccMessage): Boolean; virtual; abstract;
    procedure DoCanDuplicatedAlias(LccMessage: TLccMessage); virtual; abstract;
    procedure DoCanDuplicatedLoginAlias(LccMessage: TLccMessage); virtual; abstract;

    function DoInitializatinComplete(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoOptionalInteractionRejected(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProtocolSupportInquiry(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProtocolSupportReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoSimpleNodeInfoRequest(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoSimpleTrainInfoRequest(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoEventsIdentify(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoEventsIdentifyDest(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProducersIdentify(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoConsumersIdentify(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProducerIdentifiedSet(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProducerIdentifiedClear(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProducerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoConsumerIdentifiedSet(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DConsumerIdentifiedClear(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoConsumerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoTractionProtocol(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoTractionProtocolReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramRejectedReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramOkReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramConfigurationRead(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramConfigruationReadStream(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramConfigruationReadReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramConfigruationReadStreamReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramConfigruationWrite(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramConfigruationWriteStream(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramConfigruationWriteReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoDatagramConfigruationOperation(LccMessage: TLccMessage): Boolean; virtual; abstract;
    procedure DoUnknownLccCanMessage(LccMessage: TLccMessage); virtual; abstract;
    procedure DoUnknownLccDatagramConfigruationMessage(LccMessage: TLccMessage); virtual; abstract;
    procedure DoUnknownLccDatagramMessage(LccMessage: TLccMessage); virtual; abstract;
    procedure DoUnknownLccMessge(LccMessage: TLccMessage); virtual; abstract;
    function IsMessageForThisNode(LccMessage: TLccMessage): Boolean; virtual;
    function IsMessageSourceUsingThisAlias(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function IsMessageSourceUsingThisLoginAlias(LccMessage: TLccMessage): Boolean; virtual; abstract;

    function DoDatagram(LccMessage: TLccMessage): Boolean;
    function DoDatagramConfiguration(LccMessage: TLccMessage): Boolean;
    procedure DoRequestSendMessage(LccMessage: TLccMessage; QueueDatagramMessage: Boolean); virtual;

    function ExtractAddressSpaceFromDatagramConfigurationMessage(LccMessage: TLccMessage): Byte;
    procedure SendAckReply(LccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
  public
    property ACDIMfg: TLccACDIMfg read FACDIMfg write FACDIMfg;
    property ACDIUser: TLccACDIUser read FACDIUser write FACDIUser;
    property Configuration: TLccConfiguration read FConfiguration write FConfiguration;
    property CDI: TLccCDI read FCDI write FCDI;
    property ConfigurationMem: TLccConfigurationMemory read FConfigurationMem write FConfigurationMem;
    property ConfigMemOptions: TLccConfigurationMemOptions read FConfigMemOptions write FConfigMemOptions;
    property ConfigMemAddressSpaceInfo: TLccConfigMemAddressSpaceInfo read FConfigMemAddressSpaceInfo write FConfigMemAddressSpaceInfo;
    property EventsConsumed: TLccEvents read FEventsConsumed write FEventsConsumed;
    property EventsProduced: TLccEvents read FEventsProduced write FEventsProduced;
    property FDI: TLccFDI read FFDI write FFDI;
    property FunctionConfiguration: TLccFunctionConfiguration read FFunctionConfiguration write FFunctionConfiguration;
    property ProtocolSupport: TLccProtocolSupport read FProtocolSupport;
    property SimpleNodeInfo: TLccSimpleNodeInfo read FSimpleNodeInfo;
    property SimpleTrainNodeInfo: TLccSimpleTrainNodeInfo read FSimpleTrainNodeInfo;
    property Traction: TLccTraction read FTraction write FTraction;

    property AliasID: Word read FAliasID;
    property AliasIDStr: String read GetAliasIDStr;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr: String read GetNodeIDStr;

    property OnRequestSendMessage: TOnMessageEvent read FOnRequestSendMessage write FOnRequestSendMessage;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    function IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;

    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  // The Database Node is a container that mirrors some node on the network that
  // this program does not maintain.  It mainly handles reply and information broadcast
  // by those nodes.  It should NEVER send out a message with the stored NodeID or Alias

  { TLccDatabaseNode }

  TLccDatabaseNode = class(TLccCoreNode)
  protected
    function GetInitialized: Boolean; override;
    function GetPermitted: Boolean; override;

    function DoCanAMR(LccMessage: TLccMessage): Boolean; override;
    function DoCanAME(LccMessage: TLccMessage): Boolean; override;
    function DoCanAMD(LccMessage: TLccMessage): Boolean; override;
    function DoCanRID(LccMessage: TLccMessage): Boolean; override;
    procedure DoCanDuplicatedAlias(LccMessage: TLccMessage); override;
    procedure DoCanDuplicatedLoginAlias(LccMessage: TLccMessage); override;

    function DoInitializatinComplete(LccMessage: TLccMessage): Boolean; override;
    function DoOptionalInteractionRejected(LccMessage: TLccMessage): Boolean; override;
    function DoProtocolSupportInquiry(LccMessage: TLccMessage): Boolean; override;
    function DoProtocolSupportReply(LccMessage: TLccMessage): Boolean; override;
    function DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean; override;
    function DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean; override;
    function DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleNodeInfoRequest(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleTrainInfoRequest(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean; override;
    function DoEventsIdentify(LccMessage: TLccMessage): Boolean; override;
    function DoEventsIdentifyDest(LccMessage: TLccMessage): Boolean; override;
    function DoProducersIdentify(LccMessage: TLccMessage): Boolean; override;
    function DoConsumersIdentify(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedSet(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedClear(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; override;
    function DoConsumerIdentifiedSet(LccMessage: TLccMessage): Boolean; override;
    function DConsumerIdentifiedClear(LccMessage: TLccMessage): Boolean; override;
    function DoConsumerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; override;
    function DoTractionProtocol(LccMessage: TLccMessage): Boolean; override;
    function DoTractionProtocolReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramRejectedReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramOkReply(LccMessage: TLccMessage): Boolean; override;
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
    function IsMessageSourceUsingThisAlias(LccMessage: TLccMessage): Boolean; override;
    function IsMessageSourceUsingThisLoginAlias(LccMessage: TLccMessage): Boolean; override;
  end;

  { TLccVirtualNode }

  TLccVirtualNode = class(TLccCoreNode)
  private
    FLoggedIn: Boolean;
    FLogInAliasID: Word;
    {$IFDEF FPC} FLoginTimer: TFPTimer; {$ELSE}
                 FLoginTimer: TTimer; {$ENDIF}
    FSeedNodeID: TNodeID;
    function GetEnabled: Boolean; override;
    procedure SetEnabled(AValue: Boolean); override;
  protected
    function DoCanAMR(LccMessage: TLccMessage): Boolean; override;
    function DoCanAME(LccMessage: TLccMessage): Boolean; override;
    function DoCanAMD(LccMessage: TLccMessage): Boolean; override;
    function DoCanRID(LccMessage: TLccMessage): Boolean; override;
    procedure DoCanDuplicatedAlias(LccMessage: TLccMessage); override;
    procedure DoCanDuplicatedLoginAlias(LccMessage: TLccMessage); override;

    function DoInitializatinComplete(LccMessage: TLccMessage): Boolean; override;
    function DoOptionalInteractionRejected(LccMessage: TLccMessage): Boolean; override;
    function DoProtocolSupportInquiry(LccMessage: TLccMessage): Boolean; override;
    function DoProtocolSupportReply(LccMessage: TLccMessage): Boolean; override;
    function DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean; override;
    function DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean; override;
    function DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleNodeInfoRequest(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleTrainInfoRequest(LccMessage: TLccMessage): Boolean; override;
    function DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean; override;
    function DoEventsIdentify(LccMessage: TLccMessage): Boolean; override;
    function DoEventsIdentifyDest(LccMessage: TLccMessage): Boolean; override;
    function DoProducersIdentify(LccMessage: TLccMessage): Boolean; override;
    function DoConsumersIdentify(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedSet(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedClear(LccMessage: TLccMessage): Boolean; override;
    function DoProducerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; override;
    function DoConsumerIdentifiedSet(LccMessage: TLccMessage): Boolean; override;
    function DConsumerIdentifiedClear(LccMessage: TLccMessage): Boolean; override;
    function DoConsumerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; override;
    function DoTractionProtocol(LccMessage: TLccMessage): Boolean; override;
    function DoTractionProtocolReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramRejectedReply(LccMessage: TLccMessage): Boolean; override;
    function DoDatagramOkReply(LccMessage: TLccMessage): Boolean; override;
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
    function IsMessageSourceUsingThisAlias(LccMessage: TLccMessage): Boolean; override;
    function IsMessageSourceUsingThisLoginAlias(LccMessage: TLccMessage): Boolean; override;

    procedure CheckForAutoGeneratedEvents;
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

    property LogInAliasID: Word read FLogInAliasID write FLogInAliasID;
    {$IFDEF FPC}
      property LoginTimer: TFPTimer read FLoginTimer write FLoginTimer;
    {$ELSE}
      property LoginTimer: TTimer read FLoginTimer write FLoginTimer;
    {$ENDIF}
    property SeedNodeID: TNodeID read FSeedNodeID write FSeedNodeID;
  public
    property Initialized;
    property LoggedIn: Boolean read FLoggedIn;
    property Permitted;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Login(NewNodeID, RegenerateAliasSeed: Boolean);
    procedure Logout;
    procedure LoginWithLccSettings(RegenerateAliasSeed: Boolean; LccSettings: TLccSettings);
    procedure LoginWithNodeID(ANodeID: TNodeId; RegenerateAliasSeed: Boolean);
  end;

  { TLccMessageQueue }

  TLccMessageQueue = class
  private
    {$IFDEF FPC}
    FQueue: TObjectList;
    FTimer: TFPTimer;
    {$ELSE}
    FQueue: TObjectList<TLccMessage>;
    FTimer: TTimer;
    {$ENDIF}
    function GetLccMessage(iIndex: Integer): TLccMessage;
    procedure SetLccMessage(iIndex: Integer; AValue: TLccMessage);
  protected
    {$IFDEF FPC}
    property Queue: TObjectList read FQueue write FQueue;
    property Timer: TFPTimer read FTimer write FTimer;
    {$ELSE}
    property Queue: TObjectList<TLccMessage> read FQueue write FQueue;
    property Timer: TTimer read FTimer write FTimer;
    {$ENDIF}

    procedure OnTimer(Sender: TObject);
  public
    property LccMessage[iIndex: Integer]: TLccMessage read GetLccMessage write SetLccMessage;

    constructor Create;
    destructor Destroy; override;

    procedure Add(ALccMessage: TLccMessage);
    procedure Delete(Index: Integer);
    function FindMessageByAlias(SourceAlias, DestAlias: Word; MTI: DWord): TLccMessage;
    procedure FlushMessagesByAlias(SourceAlias, DestAlias: Word; MTI: DWord);
    procedure Remove(ALccMessage: TLccMessage);
  end;


implementation

const
  // These must be IDENTICAL to the values in the CDI file below
  SNIP_VER = 1;
  SNIP_MFG = 'Mustangpeak';
  SNIP_MODEL = 'SW100';
  SNIP_HW_VER = '1.0.0.0';
  SNIP_SW_VER = '1.0.0.0';
  SNIP_USER_VER = 1;
  SNIP_USER_NAME = '';
  SNIP_USER_DESC = '';


const
  MAX_CDI_ARRAY = 766;
  CDI_ARRAY: array[0..MAX_CDI_ARRAY-1] of byte = (
    $3C, $3F, $78, $6D, $6C, $20, $76, $65, $72, $73, $69, $6F, $6E, $3D, $22, $31, $2E, $30, $22, $20, $65, $6E, $63, $6F, $64, $69, $6E, $67, $3D, $22, $75, $74, $66, $2D, $38, $22, $3F, $3E,    // <?xml version="1.0" encoding="utf-8"?>
    $3C, $3F, $78, $6D, $6C, $2D, $73, $74, $79, $6C, $65, $73, $68, $65, $65, $74, $20, $74, $79, $70, $65, $3D, $22, $74, $65, $78, $74, $2F, $78, $73, $6C, $22, $20, $68, $72, $65, $66, $3D, $22, $68, $74, $74, $70, $3A, $2F, $2F, $6F, $70, $65, $6E, $6C, $63, $62, $2E, $6F, $72, $67, $2F, $74, $72, $75, $6E, $6B, $2F, $70, $72, $6F, $74, $6F, $74, $79, $70, $65, $73, $2F, $78, $6D, $6C, $2F, $78, $73, $6C, $74, $2F, $63, $64, $69, $2E, $78, $73, $6C, $22, $3F, $3E,    // <?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>
    $3C, $63, $64, $69, $20, $78, $6D, $6C, $6E, $73, $3A, $78, $73, $69, $3D, $22, $68, $74, $74, $70, $3A, $2F, $2F, $77, $77, $77, $2E, $77, $33, $2E, $6F, $72, $67, $2F, $32, $30, $30, $31, $2F, $58, $4D, $4C, $53, $63, $68, $65, $6D, $61, $2D, $69, $6E, $73, $74, $61, $6E, $63, $65, $22, $20, $78, $73, $69, $3A, $6E, $6F, $4E, $61, $6D, $65, $73, $70, $61, $63, $65, $53, $63, $68, $65, $6D, $61, $4C, $6F, $63, $61, $74, $69, $6F, $6E, $3D, $22, $68, $74, $74, $70, $3A, $2F, $2F, $6F, $70, $65, $6E, $6C, $63, $62, $2E, $6F, $72, $67, $2F, $74, $72, $75, $6E, $6B, $2F, $73, $70, $65, $63, $73, $2F, $73, $63, $68, $65, $6D, $61, $2F, $63, $64, $69, $2E, $78, $73, $64, $22, $3E,    // <cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">
    $3C, $69, $64, $65, $6E, $74, $69, $66, $69, $63, $61, $74, $69, $6F, $6E, $3E,    // <identification>
    $3C, $6D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $3E, $4D, $75, $73, $74, $61, $6E, $67, $70, $65, $61, $6B, $3C, $2F, $6D, $61, $6E, $75, $66, $61, $63, $74, $75, $72, $65, $72, $3E,    // <manufacturer>Mustangpeak</manufacturer>
    $3C, $6D, $6F, $64, $65, $6C, $3E, $53, $57, $31, $30, $30, $3C, $2F, $6D, $6F, $64, $65, $6C, $3E,    // <model>SW100</model>
    $3C, $68, $61, $72, $64, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E, $31, $2E, $30, $2E, $30, $2E, $30, $3C, $2F, $68, $61, $72, $64, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E,    // <hardwareVersion>1.0.0.0</hardwareVersion>
    $3C, $73, $6F, $66, $74, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E, $31, $2E, $30, $2E, $30, $2E, $30, $3C, $2F, $73, $6F, $66, $74, $77, $61, $72, $65, $56, $65, $72, $73, $69, $6F, $6E, $3E,    // <softwareVersion>1.0.0.0</softwareVersion>
    $3C, $2F, $69, $64, $65, $6E, $74, $69, $66, $69, $63, $61, $74, $69, $6F, $6E, $3E,    // </identification>
    $3C, $73, $65, $67, $6D, $65, $6E, $74, $20, $6F, $72, $69, $67, $69, $6E, $3D, $22, $31, $22, $20, $73, $70, $61, $63, $65, $3D, $22, $32, $35, $33, $22, $3E,    // <segment origin="1" space="253">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $55, $73, $65, $72, $20, $64, $65, $66, $69, $6E, $65, $64, $20, $69, $6E, $66, $6F, $72, $6D, $61, $74, $69, $6F, $6E, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>User defined information</description>
    $3C, $67, $72, $6F, $75, $70, $3E,    // <group>
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $44, $61, $74, $61, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Data</name>
    $3C, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E, $41, $64, $64, $20, $79, $6F, $75, $72, $20, $6F, $77, $6E, $20, $75, $6E, $69, $71, $75, $65, $20, $6E, $6F, $64, $65, $20, $69, $6E, $66, $6F, $20, $68, $65, $72, $65, $3C, $2F, $64, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3E,    // <description>Add your own unique node info here</description>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $36, $33, $22, $3E,    // <string size="63">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $4E, $61, $6D, $65, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Name</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $73, $74, $72, $69, $6E, $67, $20, $73, $69, $7A, $65, $3D, $22, $36, $34, $22, $3E,    // <string size="64">
    $3C, $6E, $61, $6D, $65, $3E, $55, $73, $65, $72, $20, $44, $65, $73, $63, $72, $69, $70, $74, $69, $6F, $6E, $3C, $2F, $6E, $61, $6D, $65, $3E,    // <name>User Description</name>
    $3C, $2F, $73, $74, $72, $69, $6E, $67, $3E,    // </string>
    $3C, $2F, $67, $72, $6F, $75, $70, $3E,    // </group>
    $3C, $2F, $73, $65, $67, $6D, $65, $6E, $74, $3E,    // </segment>
    $3C, $2F, $63, $64, $69, $3E, $00   // </cdi>
  );

type
  TLccEventHack = class(TLccEvent)
  end;

var
  DatagramReplyQueue: TLccMessageQueue;

{ TLccMessageQueue }

function TLccMessageQueue.GetLccMessage(iIndex: Integer): TLccMessage;
begin
  Result := Queue[iIndex] as TLccMessage;
end;

procedure TLccMessageQueue.SetLccMessage(iIndex: Integer; AValue: TLccMessage);
begin
  Queue[iIndex] := AValue
end;

procedure TLccMessageQueue.OnTimer(Sender: TObject);
var
  LocalMessage: TLccMessage;
  i: Integer;
begin
  for i := Queue.Count - 1 downto 0 do
  begin
    LocalMessage := LccMessage[i];
    LocalMessage.AbandonTimeout := LocalMessage.AbandonTimeout + 1;
    if (LocalMessage.AbandonTimeout > ABANDON_DATAGRAM_TIMEOUT) or (LocalMessage.RetryAttempts > ABANDON_DATAGRAM_RETRYS) then
      Queue.Delete(i);
  end;
end;

constructor TLccMessageQueue.Create;
begin
  inherited;
  {$IFDEF FPC}
  Timer := TFPTimer.Create(nil);
  Queue := TObjectList.Create;
  {$ELSE}
  Timer := TTimer.Create(nil);
  Queue := TObjectList<TLccMessage>.Create;
  {$ENDIF}

  Timer.OnTimer := {$IFDEF FPC}@{$ENDIF}OnTimer;
  Timer.Interval := 1000;  // 1sec
  {$IFDEF FPC}
  Timer.StartTimer;
  {$ELSE}
  Timer.Enabled := True;
  {$ENDIF}
  Queue.OwnsObjects := True;
end;

destructor TLccMessageQueue.Destroy;
begin
  {$IFDEF FPC}
  Timer.StopTimer;
  {$ELSE}
  Timer.Enabled := False;
  {$ENDIF}
  FreeAndNil(FTimer);
  FreeAndNil(FQueue);
  inherited Destroy;
end;

procedure TLccMessageQueue.Add(ALccMessage: TLccMessage);
begin
  Queue.Add(ALccMessage);
end;

procedure TLccMessageQueue.Delete(Index: Integer);
begin
  Queue.Delete(Index);
end;

function TLccMessageQueue.FindMessageByAlias(SourceAlias, DestAlias: Word; MTI: DWord): TLccMessage;
var
  i: Integer;
  AMessage: TLccMessage;
begin
  Result := nil;
  i := 0;
  while i < Queue.Count do
  begin
    AMessage := LccMessage[i];
    if (AMessage.CAN.SourceAlias = SourceAlias) and (AMessage.CAN.DestAlias = DestAlias) and ((AMessage.MTI = MTI) or (MTI = $00000000)) then
    begin
      Result := AMessage;
      Break;
    end;
    Inc(i);
  end;
end;

procedure TLccMessageQueue.FlushMessagesByAlias(SourceAlias, DestAlias: Word; MTI: DWord);
var
  QueuedMessage: TLccMessage;
begin
  QueuedMessage := DatagramReplyQueue.FindMessageByAlias(SourceAlias, DestAlias, MTI);
  while Assigned(QueuedMessage) do
  begin
    DatagramReplyQueue.Remove(QueuedMessage);
    QueuedMessage := DatagramReplyQueue.FindMessageByAlias(SourceAlias, DestAlias, MTI);
  end;
end;

procedure TLccMessageQueue.Remove(ALccMessage: TLccMessage);
begin
  Queue.Remove(ALccMessage);
end;

{ TLccCoreNode }

function TLccCoreNode.GetNodeIDStr: String;
begin
  Result := IntToHex(NodeID[1], 6);
  Result := Result + IntToHex(NodeID[0], 6);
  Result := '0x' + Result
end;

procedure TLccCoreNode.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
end;

function TLccCoreNode.GetPermitted: Boolean;
begin
  Result := FPermitted;
end;

function TLccCoreNode.IsMessageForThisNode(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  if LccMessage.HasDestination then
  begin
    if (LccMessage.CAN.DestAlias > 0) and (AliasID > 0) then
      Result := LccMessage.CAN.DestAlias = AliasID
    else
      Result := EqualNodeID(LccMessage.DestID, NodeID, False);
  end;
end;

function TLccCoreNode.DoDatagram(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[0] of
    DATAGRAM_PROTOCOL_CONFIGURATION : Result := DoDatagramConfiguration(LccMessage);
  else
    DoUnknownLccDatagramMessage(LccMessage);
  end;
end;

function TLccCoreNode.DoDatagramConfiguration(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[1] and $F0 of
    MCP_READ              : Result := DoDatagramConfigurationRead(LccMessage);
    MCP_READ_STREAM       : Result := DoDatagramConfigruationReadStream(LccMessage);
    MCP_READ_REPLY        : Result := DoDatagramConfigruationReadReply(LccMessage);
    MCP_READ_STREAM_REPLY : Result := DoDatagramConfigruationReadStreamReply(LccMessage);
    MCP_WRITE             : Result := DoDatagramConfigruationWrite(LccMessage);
    MCP_WRITE_STREAM      : Result := DoDatagramConfigruationWriteStream(LccMessage);
    MCP_WRITE_REPLY       : Result := DoDatagramConfigruationWriteReply(LccMessage);
    MCP_OPERATION         : Result := DoDatagramConfigruationOperation(LccMessage);
  else
    DoUnknownLccDatagramConfigruationMessage(LccMessage)
  end;
end;

procedure TLccCoreNode.DoRequestSendMessage(LccMessage: TLccMessage; QueueDatagramMessage: Boolean);
begin
  if Assigned(OnRequestSendMessage) then
  begin
    if QueueDatagramMessage then
      DatagramReplyQueue.Add(LccMessage.Clone);
    OnRequestSendMessage(Self, LccMessage);
  end;
end;

function TLccCoreNode.GetAliasIDStr: String;
begin
  Result := '0x' + IntToHex(FAliasID, 4);
end;

function TLccCoreNode.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TLccCoreNode.GetInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TLccCoreNode.ExtractAddressSpaceFromDatagramConfigurationMessage(LccMessage: TLccMessage): Byte;
begin
  Result := 0;
  case LccMessage.DataArrayIndexer[1] and $03 of
    0 : Result := LccMessage.DataArrayIndexer[6];
    1 : Result := MSI_CONFIG;
    2 : Result := MSI_ALL;
    3 : Result := MSI_CDI;
  end;
end;

constructor TLccCoreNode.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FConfiguration := TLccConfiguration.Create(Self, MSI_CONFIG);
  FSimpleNodeInfo := TLccSimpleNodeInfo.Create(Self, Configuration);
  FACDIMfg := TLccACDIMfg.Create(Self, MSI_ACDI_MFG, SimpleNodeInfo);
  FACDIUser := TLccACDIUser.Create(Self, MSI_ACDI_USER, SimpleNodeInfo, Configuration);
  FProtocolSupport := TLccProtocolSupport.Create(Self);
  FCDI := TLccCDI.Create(Self, MSI_CDI);
  FSimpleTrainNodeInfo := TLccSimpleTrainNodeInfo.Create(Self);
  FFDI := TLccFDI.Create(Self, MSI_FDI);
  FTraction := TLccTraction.Create(Self);
  FFunctionConfiguration := TLccFunctionConfiguration.Create(Self);
  FConfigurationMem := TLccConfigurationMemory.Create(Self);
  FEventsConsumed := TLccEvents.Create(Self);
  FEventsProduced := TLccEvents.Create(Self);
  FConfigMemOptions := TLccConfigurationMemOptions.Create(Self);
  FConfigMemAddressSpaceInfo := TLccConfigMemAddressSpaceInfo.Create(Self);
  FAckWorkerMessage := TLccMessage.Create;
  FEnabled := True;
end;

destructor TLccCoreNode.Destroy;
begin
  FreeAndNil(FProtocolSupport);
  FreeAndNil(FSimpleNodeInfo);
  FreeAndNil(FSimpleTrainNodeInfo);
  FreeAndNil(FFDI);
  FreeAndNil(FTraction);
  FreeAndNil(FFunctionConfiguration);
  FreeAndNil(FCDI);
  FreeAndNil(FConfigurationMem);
  FreeAndNil(FEventsConsumed);
  FreeAndNil(FEventsProduced);
  FreeAndNil(FConfigMemOptions);
  FreeAndNil(FConfigMemAddressSpaceInfo);
   FreeAndNil(FACDIMfg);
  FreeAndNil(FACDIUser);
  FreeAndNil(FConfiguration);
  FreeAndNil(FAckWorkerMessage);

 // if Assigned(OwnerManager) then
 //   OwnerManager.DoDestroyLccNode(Self);
  inherited;
end;

function TLccCoreNode.IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
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

function TLccCoreNode.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := False;
  if Enabled then
  begin
    if IsMessageSourceUsingThisLoginAlias(LccMessage) then
    begin
      DoCanDuplicatedLoginAlias(LccMessage);
      Exit;
    end;

    if IsMessageSourceUsingThisAlias(LccMessage) then
    begin
      DoCanDuplicatedAlias(LccMessage);
      Exit;
    end;

    if IsMessageForThisNode(LccMessage) then
    begin
      if LccMessage.IsCAN then
      begin
        case LccMessage.CAN.MTI of
          MTI_CAN_AMR  : Result := DoCanAMR(LccMessage);
          MTI_CAN_AME  : Result := DoCanAME(LccMessage);
          MTI_CAN_AMD  : Result := DoCanAMD(LccMessage);
          MTI_CAN_RID  : Result := DoCanRID(LccMessage);
        else
          DoUnknownLccCanMessage(LccMessage);
        end;
      end else
      begin
        if Permitted and Initialized then
        begin
          case LccMessage.MTI of
            MTI_INITIALIZATION_COMPLETE       : Result := DoInitializatinComplete(LccMessage);        // [IN]
            MTI_OPTIONAL_INTERACTION_REJECTED : Result := DoOptionalInteractionRejected(LccMessage);  // [IN]
            MTI_PROTOCOL_SUPPORT_INQUIRY      : Result := DoProtocolSupportInquiry(LccMessage);       // [OUT]
            MTI_PROTOCOL_SUPPORT_REPLY        : Result := DoProtocolSupportReply(LccMessage);         // [IN]
            MTI_VERIFY_NODE_ID_NUMBER         : Result := DoVerifyNodeIdNumber(LccMessage);           // [OUT]
            MTI_VERIFY_NODE_ID_NUMBER_DEST    : Result := DoVerifyNodeIdNumberDest(LccMessage);       // [OUT]
            MTI_VERIFIED_NODE_ID_NUMBER       : Result := DoVerifiedNodeIDNumber(LccMessage);         // [IN]
            MTI_SIMPLE_NODE_INFO_REQUEST      : Result := DoSimpleNodeInfoRequest(LccMessage);        // [OUT]
            MTI_SIMPLE_NODE_INFO_REPLY        : Result := DoSimpleNodeInfoReply(LccMessage);          // [IN]
            MTI_SIMPLE_TRAIN_INFO_REQUEST     : Result := DoSimpleTrainInfoRequest(LccMessage);       // [OUT]
            MTI_SIMPLE_TRAIN_INFO_REPLY       : Result := DoSimpleTrainInfoReply(LccMessage);         // [IN]
            MTI_EVENTS_IDENTIFY               : Result := DoEventsIdentify(LccMessage);               // [OUT]
            MTI_EVENTS_IDENTIFY_DEST          : Result := DoEventsIdentifyDest(LccMessage);           // [OUT]
            MTI_PRODUCER_IDENDIFY             : Result := DoProducersIdentify(LccMessage);            // [OUT]
            MTI_CONSUMER_IDENTIFY             : Result := DoConsumersIdentify(LccMessage);            // [OUT]
            MTI_PRODUCER_IDENTIFIED_SET       : Result := DoProducerIdentifiedSet(LccMessage);        // [IN]
            MTI_PRODUCER_IDENTIFIED_CLEAR     : Result := DoProducerIdentifiedClear(LccMessage);      // [IN]
            MTI_PRODUCER_IDENTIFIED_UNKNOWN   : Result := DoProducerIdentifiedUnknown(LccMessage);    // [IN]
            MTI_CONSUMER_IDENTIFIED_SET       : Result := DoConsumerIdentifiedUnknown(LccMessage);    // [IN]
            MTI_CONSUMER_IDENTIFIED_CLEAR     : Result := DoConsumerIdentifiedUnknown(LccMessage);    // [IN]
            MTI_CONSUMER_IDENTIFIED_UNKNOWN   : Result := DoConsumerIdentifiedUnknown(LccMessage);    // [IN]
            MTI_TRACTION_PROTOCOL             : Result := DoTractionProtocol(LccMessage);             // [IN]
            MTI_TRACTION_REPLY                : Result := DoTractionProtocolReply(LccMessage);        // [IN]
            MTI_DATAGRAM_REJECTED_REPLY       : Result := DoDatagramRejectedReply(LccMessage);        // [IN]
            MTI_DATAGRAM_OK_REPLY             : Result := DoDatagramOkReply(LccMessage);              // [IN]
            MTI_DATAGRAM                      : Result := DoDatagram(LccMessage);                     // [IN]
          else
            DoUnknownLccMessge(LccMessage);
          end;
        end;
      end;
    end;
  end;
end;

procedure TLccCoreNode.SendAckReply(LccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  WorkerMessage.LoadDatagramAck(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, True, ReplyPending, TimeOutValueN);
  DoRequestSendMessage(WorkerMessage, False);
end;

{ TLccVirtualNode }

function TLccVirtualNode.GetEnabled: Boolean;
begin
  Result := inherited GetEnabled and LoggedIn;
end;

procedure TLccVirtualNode.SetEnabled(AValue: Boolean);
begin
  inherited SetEnabled(AValue);
end;

function TLccVirtualNode.DoCanAMR(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  DatagramReplyQueue.FlushMessagesByAlias(LccMessage.CAN.DestAlias, LccMessage.CAN.SourceAlias, $00000000);
end;

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
      DoRequestSendMessage(WorkerMessage, False);
    end
  end else
  begin
    WorkerMessage.LoadAMD(NodeID, AliasID);
    DoRequestSendMessage(WorkerMessage, False);
  end;
end;

procedure TLccVirtualNode.DoCanDuplicatedAlias(LccMessage: TLccMessage);
begin
  if ((LccMessage.CAN.MTI and $0F000000) >= MTI_CAN_CID6) and ((LccMessage.CAN.MTI and $0F000000) <= MTI_CAN_CID0) then
  begin
    WorkerMessage.LoadRID(AliasID);                   // sorry charlie this is mine
    DoRequestSendMessage(WorkerMessage, False);
  end else
  begin
    Logout;
    Login(False, True);
  end;
end;

procedure TLccVirtualNode.DoCanDuplicatedLoginAlias(LccMessage: TLccMessage);
begin
  LoginTimer.Enabled := False;
  LogInAliasID := CreateAliasID(FNodeID, True);  // Generate a new LoginAlias
  SendAliasLoginRequest;                         // Try to allocate it
  FLoggedIn := False;                            //
  LoginTimer.Enabled := True;                    // Wait for the 500ms timer
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
      DoRequestSendMessage(WorkerMessage, False);
    end
  end;
  DatagramReplyQueue.FlushMessagesByAlias(LccMessage.CAN.DestAlias, LccMessage.CAN.SourceAlias, $00000000);
end;

function TLccVirtualNode.DoCanRID(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoInitializatinComplete(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoOptionalInteractionRejected(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoProtocolSupportReply(LccMessage: TLccMessage): Boolean;
begin
  ProtocolSupport.ProcessMessage(LccMessage);
//  if Assigned(OwnerManager) then
//    OwnerManager.DoProtocolIdentifyReply(LccSourceNode, Self);
  Result := True;
end;

function TLccVirtualNode.DoSimpleNodeInfoRequest(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, SimpleNodeInfo.PackedFormat);
  DoRequestSendMessage(WorkerMessage, False);
end;

function TLccVirtualNode.DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
begin
  Result := True;
  if LccMessage.DataCount = 6 then
  begin
    LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
    if EqualNodeID(TestNodeID, NodeID, False) then
    begin
      WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
      DoRequestSendMessage(WorkerMessage, False);
    end
  end else
  begin
    WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
    DoRequestSendMessage(WorkerMessage, False);
  end;
end;

function TLccVirtualNode.DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
  DoRequestSendMessage(WorkerMessage, False);
end;

function TLccVirtualNode.DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccVirtualNode.DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  ProtocolSupport.ProcessMessage(LccMessage);
 // if Assigned(OwnerManager) then
 //   OwnerManager.DoSimpleNodeIdentReply(LccSourceNode, Self);
end;

function TLccVirtualNode.DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SimpleTrainNodeInfo.ProcessMessage(LccMessage, Traction);
  // if Assigned(OwnerManager) then
  //   OwnerManager.DoSimpleTrainIdentReply(LccSourceNode, Self);
end;

function TLccVirtualNode.DoSimpleTrainInfoRequest(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
  DoRequestSendMessage(WorkerMessage, False);
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

function TLccVirtualNode.DoProducersIdentify(LccMessage: TLccMessage): Boolean;
var
  Event: TLccEvent;
begin
  Result := True;
  Event := EventsProduced.Supports(LccMessage.ExtractDataBytesAsEventID(0)^);
  if Assigned(Event) then
  begin
    WorkerMessage.LoadProducerIdentify(NodeID, AliasID, TLccEventHack( Event).FID);
    DoRequestSendMessage(WorkerMessage, False);
  end;
end;

function TLccVirtualNode.DoProtocolSupportInquiry(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  WorkerMessage.LoadProtocolIdentifyReply(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ProtocolSupport.EncodeFlags);
  DoRequestSendMessage(WorkerMessage, False);
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

function TLccVirtualNode.DoConsumersIdentify(LccMessage: TLccMessage): Boolean;
var
  Event: TLccEvent;
begin
  Result := True;
  Event := EventsConsumed.Supports(LccMessage.ExtractDataBytesAsEventID(0)^);
  if Assigned(Event) then
  begin
    WorkerMessage.LoadConsumerIdentify(NodeID, AliasID, TLccEventHack( Event).FID);
    DoRequestSendMessage(WorkerMessage, False);
  end;
end;

function TLccVirtualNode.DoTractionProtocol(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  Traction.ProcessMessage(LccMessage);
end;

function TLccVirtualNode.DoTractionProtocolReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  //Traction.
end;

function TLccVirtualNode.DoDatagramConfigurationRead(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[1] and $03 of
    MCP_NONE :
       begin
         case LccMessage.DataArrayIndexer[6] of
           MSI_CDI             :
               begin
                 SendAckReply(LccMessage, False, 0);
                 WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                 CDI.LoadReply(LccMessage, WorkerMessage);
                 if WorkerMessage.UserValid then
                   DoRequestSendMessage(WorkerMessage, True);
                 Result := True;
               end;
           MSI_ALL             :
               begin
                 SendAckReply(LccMessage, False, 0);
               end;
           MSI_CONFIG          :
               begin
                 SendAckReply(LccMessage, False, 0);
                 WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                 Configuration.LoadReply(LccMessage, WorkerMessage);
                 if WorkerMessage.UserValid then
                   DoRequestSendMessage(WorkerMessage, True);
                 Result := True;
               end;
           MSI_ACDI_MFG        :
               begin
                 SendAckReply(LccMessage, False, 0);
                 WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                 ACDIMfg.LoadReply(LccMessage, WorkerMessage);
                 if WorkerMessage.UserValid then
                   DoRequestSendMessage(WorkerMessage, True);
                 Result := True;
               end;
           MSI_ACDI_USER       :
               begin
                 SendAckReply(LccMessage, False, 0);
                 WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                 ACDIUser.LoadReply(LccMessage, WorkerMessage);
                 if WorkerMessage.UserValid then
                   DoRequestSendMessage(WorkerMessage, True);
                 Result := True;
               end;
           MSI_FDI             :
                begin
                  SendAckReply(LccMessage, False, 0);
                end;
           MSI_FUNCTION_CONFIG :
                begin
                  SendAckReply(LccMessage, False, 0);
                end;
         end
       end;
    MCP_CONFIGURATION : begin
                         SendAckReply(LccMessage, False, 0);
                         WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                         Configuration.LoadReply(LccMessage, WorkerMessage);
                         if WorkerMessage.UserValid then
                           DoRequestSendMessage(WorkerMessage, True);
                         Result := True;
                       end;
    MCP_ALL           : begin
                          SendAckReply(LccMessage, False, 0);
                        end;
    MCP_CDI           : begin
                         SendAckReply(LccMessage, False, 0);
                         WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                         CDI.LoadReply(LccMessage, WorkerMessage);
                         if WorkerMessage.UserValid then
                           DoRequestSendMessage(WorkerMessage, True);
                         Result := True;
                       end;
    end;
end;

function TLccVirtualNode.DoDatagramOkReply(LccMessage: TLccMessage): Boolean;
var
  QueuedMessage: TLccMessage;
begin
  Result := True;
  QueuedMessage := DatagramReplyQueue.FindMessageByAlias(LccMessage.CAN.DestAlias, LccMessage.CAN.SourceAlias, MTI_DATAGRAM);
  if Assigned(QueuedMessage) then
    DatagramReplyQueue.Remove(QueuedMessage)
end;

function TLccVirtualNode.DoDatagramRejectedReply(LccMessage: TLccMessage): Boolean;
var
  QueuedMessage: TLccMessage;
begin
  Result := True;
  QueuedMessage := DatagramReplyQueue.FindMessageByAlias(LccMessage.CAN.DestAlias, LccMessage.CAN.SourceAlias, MTI_DATAGRAM);
  if Assigned(QueuedMessage) then
  begin
    DoRequestSendMessage(QueuedMessage, False);
    QueuedMessage.AbandonTimeout := 0;;
    QueuedMessage.RetryAttempts := QueuedMessage.RetryAttempts + 1;
  end
end;

function TLccVirtualNode.DoEventsIdentify(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SendConsumedEvents;
  SendProducedEvents;
end;

function TLccVirtualNode.DoEventsIdentifyDest(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  if AliasID = LccMessage.CAN.DestAlias then
  begin
    SendConsumedEvents;
    SendProducedEvents;
  end;
end;

function TLccVirtualNode.DoDatagramConfigruationReadStream(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SendAckReply(LccMessage, False, 0);
end;

function TLccVirtualNode.DoDatagramConfigruationReadReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SendAckReply(LccMessage, False, 0);
end;

function TLccVirtualNode.DoDatagramConfigruationReadStreamReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SendAckReply(LccMessage, False, 0);
end;

function TLccVirtualNode.DoDatagramConfigruationWrite(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[1] and $03 of
    MCP_NONE :
        begin
          case LccMessage.DataArrayIndexer[6] of
            MSI_CDI             :
                begin
                  SendAckReply(LccMessage, False, 0);
                end;  // Not writeable
            MSI_ALL             :
                begin
                  SendAckReply(LccMessage, False, 0);
                end;  // Not writeable
            MSI_CONFIG          :
                begin
                  SendAckReply(LccMessage, False, 0);     // We will be sending a Write Reply
                  Configuration.WriteRequest(LccMessage);
                  Result := True;
                end;
            MSI_ACDI_MFG        :
                begin
                  SendAckReply(LccMessage, False, 0);
                end;  // Not writeable
            MSI_ACDI_USER       :
                begin
                  SendAckReply(LccMessage, False, 0);     // We will be sending a Write Reply
                  ACDIUser.WriteRequest(LccMessage);
                  Result := True;
                end;
            MSI_FDI             :
                begin
                  SendAckReply(LccMessage, False, 0);
                end;  // Not writeable
            MSI_FUNCTION_CONFIG :
                begin
                  SendAckReply(LccMessage, False, 0);
                end;
          end
        end;
    MCP_CONFIGURATION :
        begin
          SendAckReply(LccMessage, False, 0);             // We will be sending a Write Reply
          Configuration.WriteRequest(LccMessage);
          Result := True;
        end;
    MCP_ALL           :
        begin
          SendAckReply(LccMessage, False, 0);
        end; // Not writeable
    MCP_CDI           :
        begin
          SendAckReply(LccMessage, False, 0);
        end; // Not writeable
  end;
end;

function TLccVirtualNode.DoDatagramConfigruationWriteStream(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SendAckReply(LccMessage, False, 0);
end;

function TLccVirtualNode.DoDatagramConfigruationWriteReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  SendAckReply(LccMessage, False, 0);
end;

function TLccVirtualNode.DoDatagramConfigruationOperation(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[1] of
    MCP_OP_GET_CONFIG :
       begin
         SendAckReply(LccMessage, False, 0);
         WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
         ConfigMemOptions.LoadReply(WorkerMessage);
         if WorkerMessage.UserValid then;
           DoRequestSendMessage(WorkerMessage, True);
         Result := True;
       end;
    MCP_OP_GET_ADD_SPACE_INFO :
       begin
         SendAckReply(LccMessage, False, 0);
         WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
         ConfigMemAddressSpaceInfo.LoadReply(LccMessage, WorkerMessage);
         if WorkerMessage.UserValid then
           DoRequestSendMessage(WorkerMessage, True);
         Result := True;
       end;
    MCP_OP_LOCK :
       begin
         SendAckReply(LccMessage, False, 0);
       end;
    MCP_OP_GET_UNIQUEID :
       begin
         SendAckReply(LccMessage, False, 0);
       end;
    MCP_OP_FREEZE :
       begin
         SendAckReply(LccMessage, False, 0);
       end;
    MCP_OP_INDICATE :
       begin
         SendAckReply(LccMessage, False, 0);
       end;
    MCP_OP_RESETS :
       begin
         SendAckReply(LccMessage, False, 0);
       end;
    end // case
end;

procedure TLccVirtualNode.DoUnknownLccCanMessage(LccMessage: TLccMessage);
begin
  // Do nothing
end;

procedure TLccVirtualNode.DoUnknownLccDatagramConfigruationMessage(LccMessage: TLccMessage);
begin
  // Do nothing
end;

procedure TLccVirtualNode.DoUnknownLccDatagramMessage(LccMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagramRejected(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, REJECTED_DATAGRAMS_NOT_ACCEPTED);
  DoRequestSendMessage(WorkerMessage, False);
end;

procedure TLccVirtualNode.DoUnknownLccMessge(LccMessage: TLccMessage);
begin
  if LccMessage.HasDestination then
  begin
    WorkerMessage.LoadOptionalInteractionRejected(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, REJECTED_BUFFER_FULL, LccMessage.MTI);
    DoRequestSendMessage(WorkerMessage, False);
  end;
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

function TLccVirtualNode.IsMessageSourceUsingThisAlias(LccMessage: TLccMessage): Boolean;
begin
   Result := Permitted and (LccMessage.CAN.SourceAlias = AliasID)
end;

function TLccVirtualNode.IsMessageSourceUsingThisLoginAlias(LccMessage: TLccMessage): Boolean;
begin
 // Are we trying to allocate this AliasID?
 Result := (LogInAliasID <> 0) and (LccMessage.CAN.SourceAlias = LogInAliasID)
end;

procedure TLccVirtualNode.CheckForAutoGeneratedEvents;
var
  i: Integer;
  TempEventID: TEventID;
begin
  if EventsProduced.AutoGenerate.Enable then
  begin
    EventsProduced.Clear;
    for i := 0 to EventsProduced.AutoGenerate.Count - 1 do
    begin
      NodeIDToEventID(NodeID, EventsProduced.AutoGenerate.StartIndex + i, TempEventID);
      EventsProduced.Add(TempEventID, EventsProduced.AutoGenerate.DefaultState);
    end;
    EventsProduced.Valid := True;
  end;

  if EventsConsumed.AutoGenerate.Enable then
  begin
    EventsConsumed.Clear;
    for i := 0 to EventsConsumed.AutoGenerate.Count - 1 do
    begin
      NodeIDToEventID(NodeID, EventsConsumed.AutoGenerate.StartIndex + i, TempEventID);
      EventsConsumed.Add(TempEventID, EventsConsumed.AutoGenerate.DefaultState);
    end;
    EventsConsumed.Valid := True;
  end;
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
  DoRequestSendMessage(WorkerMessage, False);
  WorkerMessage.LoadCID(NodeID, LoginAliasID, 1);
  DoRequestSendMessage(WorkerMessage, False);
  WorkerMessage.LoadCID(NodeID, LoginAliasID, 2);
  DoRequestSendMessage(WorkerMessage, False);
  WorkerMessage.LoadCID(NodeID, LoginAliasID, 3);
  DoRequestSendMessage(WorkerMessage, False);
end;

procedure TLccVirtualNode.SendAliasLogin;
begin
  WorkerMessage.LoadRID(AliasID);
  DoRequestSendMessage(WorkerMessage, False);
  WorkerMessage.LoadAMD(NodeID, AliasID);
  DoRequestSendMessage(WorkerMessage, False);
  FPermitted := True;
  WorkerMessage.LoadInitializationComplete(NodeID, AliasID);
  DoRequestSendMessage(WorkerMessage, False);
  FInitialized := True;
end;

procedure TLccVirtualNode.SendAMR;
begin
  WorkerMessage.LoadAMR(NodeID, AliasID);
  DoRequestSendMessage(WorkerMessage, False);
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
    DoRequestSendMessage(WorkerMessage, False);
  end;
end;

procedure TLccVirtualNode.SendProducedEvents;
var
  i: Integer;
begin
  for i := 0 to EventsProduced.EventList.Count - 1 do
  begin
    WorkerMessage.LoadProducerIdentified(NodeID, AliasID, TLccEventHack( EventsProduced.Event[i]).FID , EventsProduced.Event[i].State);
    DoRequestSendMessage(WorkerMessage, False);
  end;
end;

constructor TLccVirtualNode.Create(AnOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AnOwner);
  {$IFDEF FPC}
     LoginTimer := TFPTimer.Create(Self);
  {$ELSE}
     LoginTimer := TTimer.Create(Self);
  {$ENDIF}
  LoginTimer.Enabled := False;
  LoginTimer.Interval := 500;
  LoginTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}OnLoginTimer;
  LogInAliasID := 0;

    // Common Protocols
  ProtocolSupport.Datagram := True;        // We support CDI so we must support datagrams
  ProtocolSupport.MemConfig := True;       // We support CDI so we must support datagrams
  ProtocolSupport.CDI := True;             // We Support CDI
  ProtocolSupport.EventExchange := True;   // We support Events
  ProtocolSupport.SimpleNodeInfo := True;  // We Support SNIP
  ProtocolSupport.ACDI := True;            // We Support ACDI
  ProtocolSupport.Valid := True;

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
  SimpleNodeInfo.Valid := True;

  // Setup a basic CDI
  CDI.AStream.Clear;
  for i := 0 to MAX_CDI_ARRAY - 1 do
  {$IFDEF FPC}
    CDI.AStream.WriteByte(CDI_ARRAY[i]);
  {$ELSE}
    CDI.AStream.Write(CDI_ARRAY[i], 1);
  {$ENDIF}
  CDI.Valid := True;

  // Setup the Configuraion Memory Options:
  ConfigMemOptions.HighSpace := MSI_CDI;
  ConfigMemOptions.LowSpace := MSI_ACDI_USER;
  ConfigMemOptions.SupportACDIMfgRead := True;
  ConfigMemOptions.SupportACDIUserRead := True;
  ConfigMemOptions.SupportACDIUserWrite := True;
  ConfigMemOptions.UnAlignedReads := True;
  ConfigMemOptions.UnAlignedWrites := True;
  ConfigMemOptions.WriteArbitraryBytes := True;
  ConfigMemOptions.WriteLenFourBytes := True;
  ConfigMemOptions.WriteLenOneByte := True;
  ConfigMemOptions.WriteLenSixyFourBytes := True;
  ConfigMemOptions.WriteLenTwoBytes := True;
  ConfigMemOptions.WriteStream := False;
  ConfigMemOptions.WriteUnderMask := False;
  ConfigMemOptions.Valid := True;

  // Setup the Configuration Memory Addres Space Information
  ConfigMemAddressSpaceInfo.Add(MSI_CDI, True, True, True, $00000000, $FFFFFFFF);
  ConfigMemAddressSpaceInfo.Add(MSI_ALL, True, True, True, $00000000, $FFFFFFFF);
  ConfigMemAddressSpaceInfo.Add(MSI_CONFIG, True, False, True, $00000000, $FFFFFFFF);
  ConfigMemAddressSpaceInfo.Add(MSI_ACDI_MFG, True, True, True, $00000000, $FFFFFFFF);      // We don't support ACDI in this object
  ConfigMemAddressSpaceInfo.Add(MSI_ACDI_USER, True, False, True, $00000000, $FFFFFFFF);    // We don't support ACDI in this object
  ConfigMemAddressSpaceInfo.Valid := True;
end;

destructor TLccVirtualNode.Destroy;
begin
  Logout;
  inherited Destroy;
end;

procedure TLccVirtualNode.Login(NewNodeID, RegenerateAliasSeed: Boolean);
begin
 if NewNodeID then
   GenerateNewNodeID;
 CheckForAutoGeneratedEvents;
 Configuration.LoadFromFile;
 LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
 SendAliasLoginRequest;
 LoginTimer.Enabled := True;
end;

procedure TLccVirtualNode.Logout;
begin
  if Permitted then
  begin
    LoginTimer.Enabled := False;
    FLoggedIn := False;
    FPermitted := False;
    WorkerMessage.LoadAMR(NodeID, AliasID);          // You used my Alias you dog......
    DoRequestSendMessage(WorkerMessage, False);
  end;
end;

procedure TLccVirtualNode.LoginWithLccSettings(RegenerateAliasSeed: Boolean; LccSettings: TLccSettings);
var
  TempNodeID: TNodeID;
  TempID, TempID1, TempID2: QWord;
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

    CheckForAutoGeneratedEvents;
    Configuration.LoadFromFile;
    LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
    SendAliasLoginRequest;
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

  CheckForAutoGeneratedEvents;
  Configuration.LoadFromFile;
  LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
  SendAliasLoginRequest;
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

function TLccDatabaseNode.DoCanAMR(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoCanAME(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

procedure TLccDatabaseNode.DoCanDuplicatedAlias(LccMessage: TLccMessage);
begin
  // Do nothing
end;

procedure TLccDatabaseNode.DoCanDuplicatedLoginAlias(LccMessage: TLccMessage);
begin
  // Do nothing
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

function TLccDatabaseNode.DoOptionalInteractionRejected(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoProtocolSupportReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  ProtocolSupport.ProcessMessage(LccMessage);
 // OwnerManager.DoProtocolIdentifyReply(Self, LccDestNode);
end;

function TLccDatabaseNode.DoSimpleNodeInfoRequest(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
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

function TLccDatabaseNode.DoSimpleTrainInfoRequest(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
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

function TLccDatabaseNode.DoProducersIdentify(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoProtocolSupportInquiry(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
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

function TLccDatabaseNode.DoConsumersIdentify(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
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
                  DoRequestSendMessage(WorkerMessage, False);
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

function TLccDatabaseNode.DoDatagramOkReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoDatagramRejectedReply(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoEventsIdentify(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
end;

function TLccDatabaseNode.DoEventsIdentifyDest(LccMessage: TLccMessage): Boolean;
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
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
        end;
    MCP_OP_GET_CONFIG_REPLY :
        begin
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
          ConfigMemOptions.ProcessMessage(LccMessage);
          Result := True;
        end;
    MCP_OP_GET_ADD_SPACE_INFO :
        begin
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
          WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
          ConfigMemAddressSpaceInfo.LoadReply(LccMessage, WorkerMessage);
    //      if WorkerMessage.UserValid then
    //        OwnerManager.DoRequestMessageSend(WorkerMessage);
          Result := True;
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
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
        end;
    MCP_OP_GET_UNIQUEID :
        begin
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
        end;
    MCP_OP_FREEZE :
        begin
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
        end;
    MCP_OP_INDICATE :
        begin
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
        end;
    MCP_OP_RESETS :
        begin
          SendAckReply(LccMessage, False, 0);   // We don't need to send a Reply
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

function TLccDatabaseNode.IsMessageSourceUsingThisAlias(LccMessage: TLccMessage): Boolean;
begin
  Result := False
end;

function TLccDatabaseNode.IsMessageSourceUsingThisLoginAlias(LccMessage: TLccMessage): Boolean;
begin
  Result := False;
end;

procedure TLccDatabaseNode.DoUnknownLccCanMessage(LccMessage: TLccMessage);
begin
  // Do nothing
end;

initialization
  DatagramReplyQueue := TLccMessageQueue.Create;

finalization
  FreeAndNil(DatagramReplyQueue);


end.

