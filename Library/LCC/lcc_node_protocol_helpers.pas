unit lcc_node_protocol_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
  laz2_DOM,
  laz2_XMLRead,
  {$ENDIF}
  lcc_math_float16,
  lcc_messages,
  lcc_defines,
  lcc_utilities;

const
  ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH = $0001;

type
  TLccTraction = class;   // Forward
  TLccSimpleNodeInfo = class;
  TLccConfiguration = class;

  TBaseLccNode = class(TComponent)
  private
    FCreateTime: DWord;
    FErrorCode: Word;
    FValid: Boolean;
    FWorkerMessage: TLccMessage;
    procedure SetValid(AValue: Boolean); virtual;
  protected
    property CreateTime: DWord read FCreateTime write FCreateTime;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  public
    property ErrorCode: Word read FErrorCode write FErrorCode;
    property Valid: Boolean read FValid write SetValid;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; virtual; abstract;
  end;

  { TBaseStreamLccNode }

  TBaseStreamLccNode = class(TBaseLccNode)
  private
    FInProcessAddress: DWord;
    FStream: TMemoryStream;
    FAddressSpace: Byte;
  protected
    procedure SetValid(AValue: Boolean); override;
    procedure DoLoadComplete(LccMessage: TLccMessage); virtual;

    property InProcessAddress: DWord read FInProcessAddress write FInProcessAddress;
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;
  public
    property AStream: TMemoryStream read FStream write FStream;

    constructor Create(AnOwner: TComponent; AnAddressSpace: Byte); reintroduce; virtual;
    destructor Destroy; override;
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage); virtual;
    procedure WriteRequest(LccMessage: TLccMessage); virtual;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TLccACDIMfg }

  TLccACDIMfg = class(TBaseStreamLccNode)
  private
    FSNIP: TLccSimpleNodeInfo;
  protected
    property SNIP: TLccSimpleNodeInfo read FSNIP write FSNIP;
  public
    constructor Create(AnOwner: TComponent; AnAddressSpace: Byte; ASNIP: TLccSimpleNodeInfo); reintroduce; virtual;
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage); override;
  end;

  { TLccACDIUser }

  TLccACDIUser = class(TLccACDIMfg)
  private
    FConfiguration: TLccConfiguration;
  protected
    property Configuration: TLccConfiguration read FConfiguration write FConfiguration;
  public
    constructor Create(AnOwner: TComponent; AnAddressSpace: Byte; ASNIP: TLccSimpleNodeInfo; AConfiguration: TLccConfiguration); reintroduce; virtual;
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage); override;
    procedure WriteRequest(LccMessage: TLccMessage); override;
  end;

  { TLccCDI }

  TLccCDI = class(TBaseStreamLccNode)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  public
    function LoadFromXml(CdiFilePath: String): Boolean;
  end;

  { TLccConfigMemAddressSpaceInfoObject }

  TLccConfigMemAddressSpaceInfoObject = class
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

  { TLccConfigMemAddressSpaceInfo }

  TLccConfigMemAddressSpaceInfo = class(TBaseLccNode)
  private
    FList: TList;
    function GetAddressSpace(Index: Integer): TLccConfigMemAddressSpaceInfoObject;
    function GetCount: Integer;
  protected
    property List: TList read FList write FList;
  public
    property AddressSpace[Index: Integer]: TLccConfigMemAddressSpaceInfoObject read GetAddressSpace; default;
    property Count: Integer read GetCount;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(_Space: Byte; _IsPresent, _IsReadOnly, _ImpliedZeroLowAddress: Boolean; _LowAddress, _HighAddress: DWord);
    procedure Clear;
    function FindByAddressSpace(Space: Byte): TLccConfigMemAddressSpaceInfoObject;
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TLccConfiguration }

  TLccConfiguration = class(TBaseStreamLccNode)
  private
    FAutoSaveOnWrite: Boolean;
    FFilePath: String;
  public
    property AutoSaveOnWrite: Boolean read FAutoSaveOnWrite write FAutoSaveOnWrite;
    property FilePath: String read FFilePath write FFilePath;

    constructor Create(AnOwner: TComponent; AnAddressSpace: Byte); override;
    procedure WriteRequest(LccMessage: TLccMessage); override;
    function ReadAsString(Address: DWord): String;
    procedure LoadFromFile;
  end;

  { TLccConfigurationMemOptions }

  TLccConfigurationMemOptions = class(TBaseLccNode)
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

  { TLccConfigurationMemory }

  TLccConfigurationMemory = class(TBaseLccNode)
  private
    FAddress: DWord;
    FAddressSpace: Byte;
    FDataCount: Integer;
    FDataRaw: TDatagramArray;
    FDataType: TLccConfigDataType;
    FDataTypeBit: Byte;
    FDataTypeEvent: TEventID;
    FDataTypeInteger: Integer;
    FDataTypeString: String;
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
    property DataTypeString: String read FDataTypeString;
    procedure Initialize(AnAddress: DWord; AnAddressSpace: Byte; DataSize: Integer; ADataType: TLccConfigDataType);
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TLccEvent }

  TLccEvent = class
  protected
    FID: TEventID;
    FState: TEventState;
  public
    property ID: TEventID read FID write FID;
    property State: TEventState read FState write FState;
  end;

  { TLccEventAutoGenerate }

  TLccEventAutoGenerate = class
  private
    FEnable: Boolean;
    FCount: Integer;
    FDefaultState: TEventState;
    FStartIndex: Integer;
  public
    property Enable: Boolean read FEnable write FEnable;
    property Count: Integer read FCount write FCount;
    property DefaultState: TEventState read FDefaultState write FDefaultState;
    property StartIndex: Integer read FStartIndex write FStartIndex;
  end;

  { TLccEvents }

  TLccEvents = class(TBaseLccNode)
  private
    FAutoGenerate: TLccEventAutoGenerate;
    {$IFDEF FPC}FEventList: TList;{$ELSE}FEventList: TObjectList<TLccEvent>;{$ENDIF}
    function GetCount: Integer;
    function GetEvent(Index: Integer): TLccEvent;
    function GetEventIDAsStr(Index: Integer): String;
  protected
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;

    procedure Add(Event: TEventID; State: TEventState);
    procedure Clear;
    {$IFDEF FPC}property EventList: TList read FEventList write FEventList;
    {$ELSE}     property EventList: TObjectList<TLccEvent> read FEventList write FEventList;
    {$ENDIF}
    function Supports(Event: TEventID): TLccEvent;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;

    property AutoGenerate: TLccEventAutoGenerate read FAutoGenerate write FAutoGenerate;
    property Count: Integer read GetCount;
    property Event[Index: Integer]: TLccEvent read GetEvent; default;
    property EventIDAsStr[Index: Integer]: String read GetEventIDAsStr;
  end;

  { TLccFDI }

  TLccFDI = class(TBaseStreamLccNode)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  end;

  { TLccFunctionConfiguration }

  TLccFunctionConfiguration = class(TBaseLccNode)
  private
    FFunctionStatesArray: TFunctionStatesArray;
    function GetFunctionStates(iIndex: Integer): Boolean;
  protected
    property FunctionStatesArray: TFunctionStatesArray read FFunctionStatesArray write FFunctionStatesArray;
  public
    property FunctionStates[iIndex: Integer]: Boolean read GetFunctionStates;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TLccProtocolSupport }

  TLccProtocolSupport = class(TBaseLccNode)
  private
    FACDI: Boolean;
    FCDI: Boolean;
    FDatagram: Boolean;
    FDisplay: Boolean;
    FEventExchange: Boolean;
    FFDI: Boolean;
    FIdentification: Boolean;
    FMemConfig: Boolean;
    FRemoteButton: Boolean;
    FReservation: Boolean;
    FSimpleNodeInfo: Boolean;
    FStream: Boolean;
    FTeach_Learn: Boolean;
    FTractionControl: Boolean;
    FTractionProxy: Boolean;
    FSimpleTrainNodeInfo: Boolean;
    FFunctionConfiguration: Boolean;
  protected
    Flags: array of QWord;
    procedure DecodeFlags;
    function EncodeFlags: QWord;
  public
    property Datagram: Boolean read FDatagram write FDatagram;
    property FDI: Boolean read FFDI write FFDI;
    property FunctionConfiguration: Boolean read FFunctionConfiguration write FFunctionConfiguration;
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
    property TractionProxy: Boolean read FTractionProxy write FTractionProxy;
    property SimpleTrainNodeInfo: Boolean read FSimpleTrainNodeInfo write FSimpleTrainNodeInfo;

    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

  { TLccSimpleNodeInfo }

  TLccSimpleNodeInfo = class(TBaseLccNode)
  private
    FConfiguration: TLccConfiguration;
  private
    FHardwareVersion: String;
    FManufacturer: String;
    FModel: String;
    FSoftwareVersion: String;
    FPackedInfo: TSimpleNodeInfoPacked;
    FUserDescription: String;
    FUserName: String;
    FUserVersion: Word;
    FVersion: Word;

    property Configuration: TLccConfiguration read FConfiguration write FConfiguration;

    function GetPackedFormat: TSimpleNodeInfoPacked;
    function GetUserDescription: String;
    function GetUserName: String;
  public
    property Version: Word read FVersion write FVersion;
    property Manufacturer: String read FManufacturer write FManufacturer;
    property Model: String read FModel write FModel;
    property HardwareVersion: String read FHardwareVersion write FHardwareVersion;
    property SoftwareVersion: String read FSoftwareVersion write FSoftwareVersion;
    property UserVersion: Word read FUserVersion write FUserVersion;
    property UserName: String read GetUserName write FUserName;
    property UserDescription: String read GetUserDescription write FUserDescription;

    property PackedFormat: TSimpleNodeInfoPacked read GetPackedFormat;

    constructor Create(AnOwner: TComponent; AConfiguration: TLccConfiguration); reintroduce; virtual;
    {$IFDEF FPC}
    function LoadFromXml(CdiFilePath: String): Boolean;
    {$ENDIF}
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;


  { TLccSimpleTrainNodeInfo }

  TLccSimpleTrainNodeInfo = class(TBaseLccNode)
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

    function ProcessMessage(LccMessage: TLccMessage; Traction: TLccTraction): Boolean; reintroduce; virtual;
  end;

  { TLccTraction }

  TLccTraction = class(TBaseLccNode)
  private
    FLegacySpeedSteps: Byte;
    FLegacyTechnology: Byte;
    FLegacyTrainID: Word;
    FLinkedNode: TBaseLccNode;                 // depends on the Node: Throttle Node = Linked Train Node, Train Node = Linked Throttle Node
    FScratchNode: TBaseLccNode;
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
    property LinkedNode: TBaseLccNode read FLinkedNode write FLinkedNode;
    property LegacyTechnology: Byte read FLegacyTechnology write FLegacyTechnology;
    property LegacyTrainID: Word read FLegacyTrainID write FLegacyTrainID;
    property LegacySpeedSteps: Byte read FLegacySpeedSteps write FLegacySpeedSteps;
    property ScratchNode: TBaseLccNode read FScratchNode write FScratchNode;

    function IsLinked: Boolean;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

    { TLccCoreNode }

  TLccCoreNode = class(TBaseLccNode)
  private
    FACDIMfg: TLccACDIMfg;
    FACDIUser: TLccACDIUser;
    FCDI: TLccCDI;
    FConfiguration: TLccConfiguration;
    FConfigurationMem: TLccConfigurationMemory;
    FConfigMemOptions: TLccConfigurationMemOptions;
    FEventsConsumed: TLccEvents;
    FEventsProduced: TLccEvents;
    FFDI: TLccFDI;
    FFunctionConfiguration: TLccFunctionConfiguration;
    FProtocolSupport: TLccProtocolSupport;
    FSimpleNodeInfo: TLccSimpleNodeInfo;
    FSimpleTrainNodeInfo: TLccSimpleTrainNodeInfo;
    FTraction: TLccTraction;
    FConfigMemAddressSpaceInfo: TLccConfigMemAddressSpaceInfo;

    function GetAliasIDStr: String;
    function GetNodeIDStr: String;
  protected
    FAliasID: Word;
    FInitialized: Boolean;
    FNodeID: TNodeID;
    FPermitted: Boolean;

    function GetInitialized: Boolean; virtual;
    function GetPermitted: Boolean; virtual;

    property Initialized: Boolean read GetInitialized;
    property Permitted: Boolean read GetPermitted;

    function DoCanAME(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoCanAMD(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoCanRID(LccMessage: TLccMessage): Boolean; virtual; abstract;

    function DoInitializatinComplete(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProtocolSupportReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoVerifyNodeIdNumber(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoVerifyNodeIdNumberDest(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoVerifiedNodeIDNumber(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoSimpleNodeInfoReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoSimpleTrainInfoReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProducerIdentifiedSet(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProducerIdentifiedClear(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoProducerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoConsumerIdentifiedSet(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DConsumerIdentifiedClear(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoConsumerIdentifiedUnknown(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoTractionProtocol(LccMessage: TLccMessage): Boolean; virtual; abstract;
    function DoTractionProtocolReply(LccMessage: TLccMessage): Boolean; virtual; abstract;
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
    function IsMessageSourceUsingThisAlias(LccMessage: TLccMessage): Boolean; virtual;

    procedure DoCanDuplicatedAlias(LccMessage: TLccMessage);
    function DoDatagram(LccMessage: TLccMessage): Boolean;
    function DoDatagramConfiguration(LccMessage: TLccMessage): Boolean;
    procedure DoSendMessage(LccMessage: TLccMessage); virtual;

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
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr: String read GetNodeIDStr;

    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    function IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
    procedure Login(NewNodeID, RegenerateAliasSeed: Boolean); virtual; abstract;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;


implementation

{ TBaseLccNode }

procedure TBaseLccNode.SetValid(AValue: Boolean);
begin
  if FValid=AValue then Exit;
  FValid:=AValue;
end;

constructor TBaseLccNode.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  FCreateTime := GetTickCount;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TBaseLccNode.Destroy;
begin
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

{ TBaseStreamLccNode }

procedure TBaseStreamLccNode.SetValid(AValue: Boolean);
begin
  inherited SetValid(AValue);
  if not AValue then
  begin
    AStream.Size := 0;
    InProcessAddress := 0;
  end
end;

procedure TBaseStreamLccNode.WriteRequest(LccMessage: TLccMessage);
begin

end;

constructor TBaseStreamLccNode.Create(AnOwner: TComponent; AnAddressSpace: Byte);
begin
  inherited Create(AnOwner);
  FStream := TMemoryStream.Create;
  FAddressSpace := AnAddressSpace;
end;

destructor TBaseStreamLccNode.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TBaseStreamLccNode.DoLoadComplete(LccMessage: TLccMessage);
begin

end;

{ TLccACDIMfg }

constructor TLccACDIMfg.Create(AnOwner: TComponent; AnAddressSpace: Byte; ASNIP: TLccSimpleNodeInfo);
begin
  inherited Create(AnOwner, AnAddressSpace);
  FSNIP := ASNIP;
end;

procedure TLccACDIMfg.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
var
  i, Offset: Integer;
  ReadCount: Integer;
  Address: DWord;
  FlatArray: array[0..ACDI_MFG_SIZE - 1] of Byte;
begin
  // Assumption is this is a datagram message
  ReadCount := LccMessage.ExtractDataBytesAsInt(7, 7);
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArrayIndexer[0];
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArrayIndexer[1] or $10;   // Make it a reply
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];          // Copy the address
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArrayIndexer[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArrayIndexer[5];
  OutMessage.DataArrayIndexer[6] := LccMessage.DataArrayIndexer[6];

  FlatArray[0] := 0;
  FillChar(FlatArray, ACDI_MFG_SIZE, #0);

  FlatArray[0] := SNIP.Version;
  Offset := ACDI_MFG_OFFSET_MANUFACTURER;
  StringToNullArray(SNIP.Manufacturer, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_MODEL;
  StringToNullArray(SNIP.Model, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_HARDWARE_VERSION;
  StringToNullArray(SNIP.HardwareVersion, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_SOFTWARE_VERSION;
  StringToNullArray(SNIP.SoftwareVersion, FlatArray, Offset);

  OutMessage.DataCount := ReadCount + 7;
  for i := 0 to ReadCount - 1 do
    OutMessage.DataArrayIndexer[i + 7] := FlatArray[Address + DWord( i)];
  OutMessage.UserValid := True;
end;

{ TLccACDIUser }

constructor TLccACDIUser.Create(AnOwner: TComponent; AnAddressSpace: Byte;
  ASNIP: TLccSimpleNodeInfo; AConfiguration: TLccConfiguration);
begin
  inherited Create(AnOwner, AnAddressSpace, ASNIP);
  Configuration := AConfiguration;
end;

procedure TLccACDIUser.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
var
  i, Offset: Integer;
  ReadCount: Integer;
  Address: DWord;
  FlatArray: array[0..ACDI_USER_SIZE - 1] of Byte;
begin
  FlatArray[0] := 0;
  // Assumption is this is a datagram message
  ReadCount := LccMessage.ExtractDataBytesAsInt(7, 7);
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArrayIndexer[0];
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArrayIndexer[1] or $10;   // Make it a reply
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];          // Copy the address
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArrayIndexer[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArrayIndexer[5];
  OutMessage.DataArrayIndexer[6] := LccMessage.DataArrayIndexer[6];

  FillChar(FlatArray, ACDI_USER_SIZE, #0);

  FlatArray[0] := SNIP.UserVersion;
  Offset := ACDI_USER_OFFSET_NAME;
  StringToNullArray(SNIP.UserName, FlatArray, Offset);
  Offset := ACDI_USER_OFFSET_DESCRIPTION;
  StringToNullArray(SNIP.UserDescription, FlatArray, Offset);

  OutMessage.DataCount := ReadCount + 7;
  for i := 0 to ReadCount - 1 do
    OutMessage.DataArrayIndexer[i + 7] := FlatArray[Address + DWord(i)];
  OutMessage.UserValid := True;
end;

procedure TLccACDIUser.WriteRequest(LccMessage: TLccMessage);
var
  Address: DWord;
begin
  // We should never allow the Version to be written too so never write to 0 offset
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  if Address > 0 then
    Configuration.WriteRequest(LccMessage);
end;

{TLccCDI}

procedure TLccCDI.DoLoadComplete(LccMessage: TLccMessage);
//var
 // SourceNode, DestNode: TBaseLccNode;
begin
 { if Assigned(OwnerManager) then
  begin
    SourceNode := OwnerManager.FindSourceNode(LccMessage, True);
    DestNode := OwnerManager.FindDestNode(LccMessage, True);
    if Assigned(SourceNode) and Assigned(DestNode) then
      OwnerManager.DoCDI(SourceNode, DestNode);
  end;  }
end;

function TLccCDI.LoadFromXml(CdiFilePath: String): Boolean;
var
  XmlFile: TStringList;
  i, j: Integer;
begin
  Result := False;
  if FileExists(String( CdiFilePath)) then
  begin
    XmlFile := TStringList.Create;
    try
      XmlFile.LoadFromFile(String( CdiFilePath));
      XmlFile.Text := Trim(XmlFile.Text);
      AStream.Clear;
      for i := 0 to XmlFile.Count - 1 do
      begin
        if Length(XmlFile[i]) > 0 then
        begin
          for j := 1 to Length(XmlFile[i]) do
          begin
            {$IFDEF FPC}
            AStream.WriteByte(Ord(XmlFile[i][j]));
            {$ELSE}
            AByte := Ord(XmlFile[i][j]);
            AStream.Write(AByte, 1);
            {$ENDIF}
          end;
        end
      end;
      Result := True;
    finally
      FreeAndNil(XmlFile);
    end;
  end;
end;

{ TLccConfigMemAddressSpaceInfo }

procedure TLccConfigMemAddressSpaceInfo.Add(_Space: Byte; _IsPresent, _IsReadOnly, _ImpliedZeroLowAddress: Boolean; _LowAddress, _HighAddress: DWord);
var
  Info: TLccConfigMemAddressSpaceInfoObject;
begin
  Info := TLccConfigMemAddressSpaceInfoObject.Create;
  Info.FAddressSpace := _Space;
  Info.FIsPresent := _IsPresent;
  Info.FIsReadOnly := _IsReadOnly;
  Info.FImpliedZeroLowAddress := _ImpliedZeroLowAddress;
  Info.FLowAddress := _LowAddress;
  Info.FHighAddress := _HighAddress;
  List.Add(Info);
end;

procedure TLccConfigMemAddressSpaceInfo.Clear;
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

constructor TLccConfigMemAddressSpaceInfo.Create(AnOwner: TComponent);
begin
  inherited;
  List := TList.Create;
end;

destructor TLccConfigMemAddressSpaceInfo.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TLccConfigMemAddressSpaceInfo.FindByAddressSpace(Space: Byte): TLccConfigMemAddressSpaceInfoObject;
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

function TLccConfigMemAddressSpaceInfo.GetAddressSpace(Index: Integer): TLccConfigMemAddressSpaceInfoObject;
begin
  Result := TLccConfigMemAddressSpaceInfoObject( List[Index])
end;

function TLccConfigMemAddressSpaceInfo.GetCount: Integer;
begin
  Result := List.Count
end;

procedure TLccConfigMemAddressSpaceInfo.LoadReply(LccMessage, OutMessage: TLccMessage);
var
  Info: TLccConfigMemAddressSpaceInfoObject;
begin
   // Decode the LccMessage
  Info := FindByAddressSpace( LccMessage.DataArrayIndexer[2]);
  if Assigned(Info) then
  begin
    if Info.IsPresent then
      OutMessage.DataArrayIndexer[1] := MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY
    else
      OutMessage.DataArrayIndexer[1] := MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY;
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
    OutMessage.DataArrayIndexer[1] := MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY;
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

function TLccConfigMemAddressSpaceInfo.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  Info: TLccConfigMemAddressSpaceInfoObject;
  IsPresent, ImpliedZeroAddress, IsReadOnly: Boolean;
  Space: Byte;
begin
  Result := True;
  IsPresent := LccMessage.DataArrayIndexer[1] = MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY;
  ImpliedZeroAddress := LccMessage.DataArrayIndexer[7] and $02 = 0;
  IsReadOnly := LccMessage.DataArrayIndexer[7] and $01 <> 0;
  Space := LccMessage.DataArrayIndexer[2];

  Info := FindByAddressSpace(Space);
  if not Assigned(Info) then
  begin
    if ImpliedZeroAddress then
      Add(Space,                                       // Space
          IsPresent,                                   // Present?
          IsReadOnly,                                  // Readonly?
          ImpliedZeroAddress,                          // Implied Zero Address
          0,                                           // Low Memory Address
          LccMessage.ExtractDataBytesAsInt(3, 6))      // High Memory Address
    else
      Add(Space,                                       // Space
          IsPresent,                                   // Present?
          IsReadOnly,                                  // Readonly?
          ImpliedZeroAddress,                          // Implied Zero Address
          LccMessage.ExtractDataBytesAsInt(8, 11),     // Low Memory Address
          LccMessage.ExtractDataBytesAsInt(3, 6));     // High Memory Address
 //   OwnerManager.DoConfigMemAddressSpaceInfoReply(OwnerManager.FindSourceNode(LccMessage, True), OwnerManager.FindDestNode(LccMessage, True), Space);
  end;
  Valid := True;                                       // Had at least one....
end;

{ TLccConfiguration }

constructor TLccConfiguration.Create(AnOwner: TComponent; AnAddressSpace: Byte);
begin
  inherited Create(AnOwner, AnAddressSpace);
  AutoSaveOnWrite := True;
end;

procedure TLccConfiguration.LoadFromFile;
begin
  if FileExists(String( FilePath)) then
    AStream.LoadFromFile(String( FilePath))
end;

function TLccConfiguration.ReadAsString(Address: DWord): String;
var
  i: DWord;
  C: Char;
  Done: Boolean;
begin
  Result := '';
  if AStream.Size > Address then
  begin
    AStream.Position := Address;
    i := 0;
    Done := False;
    while (i + Address < DWord( AStream.Size)) and not Done do
    begin
      {$IFDEF FPC} C := Chr(AStream.ReadByte);
      {$ELSE}      AStream.Read(C, 1); {$ENDIF}
      if C <> #0 then
        Result := Result + C
      else
        Done := True;
      Inc(i)
    end;
  end;
end;

procedure TLccConfiguration.WriteRequest(LccMessage: TLccMessage);
var
  i: Integer;
  iStart : Integer;
  WriteCount,Address: DWord;
  {$IFNDEF FPC}AByte: Byte;{$ENDIF}
begin
  // Assumption is this is a datagram message
  if LccMessage.DataArrayIndexer[1] and $03 = 0 then
    iStart := 7
  else
    iStart := 6;
  WriteCount := LccMessage.DataCount - iStart;
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  if Address + WriteCount > DWord( AStream.Size) then
    AStream.Size := Int64( Address) + Int64(WriteCount);
  AStream.Position := Address;
  for i := iStart to LccMessage.DataCount - 1 do
  begin
    {$IFDEF FPC} AStream.WriteByte(LccMessage.DataArrayIndexer[i]);
    {$ELSE}
    AByte := LccMessage.DataArrayIndexer[i];
    AStream.Write(AByte, 1); {$ENDIF}
  end;
  if AutoSaveOnWrite then
  begin
    if FileExists(String( FilePath)) then
      AStream.SaveToFile(String( FilePath))
    {$IFNDEF FPC_CONSOLE_APP}
    else
      ShowMessage('Attempt to write to configuration failed, file path not valid');
    {$ENDIF}
  end;
end;

{ TLccConfigurationMemOptions }

procedure TLccConfigurationMemOptions.LoadReply(LccMessage: TLccMessage);
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

function TLccConfigurationMemOptions.ProcessMessage(LccMessage: TLccMessage): Boolean;
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
          Valid := True;
   //       OwnerManager.DoConfigMemOptionsReply(OwnerManager.FindSourceNode(LccMessage, True), OwnerManager.FindSourceNode(LccMessage, True));
        end;
    end
  end;
end;

{ TLccConfigurationMemory }

constructor TLccConfigurationMemory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TLccConfigurationMemory.Destroy;
begin
  inherited Destroy;
end;

function TLccConfigurationMemory.GetDataRawIndexer(iIndex: Word): Byte;
begin
  Result := FDataRaw[iIndex]
end;

procedure TLccConfigurationMemory.Initialize(AnAddress: DWord;
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

function TLccConfigurationMemory.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  iStart, i, RemainingCount: Integer;
  LocalAddressSpace: Byte;
 // SourceNode: TBaseLccNode;
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
              FDataTypeString := FDataTypeString + Chr(LccMessage.DataArrayIndexer[i+iStart]);

            RemainingCount := DataCount - Length(FDataTypeString);           // Strings are 1 indexed
            if RemainingCount > 64 then
              RemainingCount := 64;
            if RemainingCount > 0 then
            begin
              WorkerMessage.LoadConfigMemRead(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, MSI_CONFIG, InProcessAddress, RemainingCount);
    //          OwnerManager.DoRequestMessageSend(WorkerMessage);
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
   {   SourceNode := OwnerManager.FindSourceNode(LccMessage, True);
      if Assigned(OwnerManager.CdiParser) then    // Callback on the CDI Parser if available
        OwnerManager.CdiParser.DoConfigMemReadReply(SourceNode);
      OwnerManager.DoConfigMemReadReply(SourceNode, OwnerManager.FindDestNode(LccMessage, True));   }
    end;
  end else
  if LccMessage.DataArrayIndexer[1] and MCP_WRITE_REPLY <> 0 then
  begin
    ErrorCode := 0;

    if ErrorCode = 0 then
    begin
 {     SourceNode := OwnerManager.FindSourceNode(LccMessage, True);
      if Assigned(OwnerManager.CdiParser) then    // Callback on the CDI Parser if available
        OwnerManager.CdiParser.DoConfigMemWriteReply(SourceNode);
      OwnerManager.DoConfigMemWriteReply(SourceNode, OwnerManager.FindDestNode(LccMessage, True));  }
    end;
  end;
end;

procedure TLccConfigurationMemory.SetDataRawIndexer(iIndex: Word; const Value: Byte);
begin
  FDataRaw[iIndex] := Value
end;

{ TLccEvents }

constructor TLccEvents.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  {$IFDEF FPC} FEventList := TList.Create;
  {$ELSE}
    FEventList := TObjectList<TLccEvent>.Create;
    EventList.OwnsObjects := False;{$ENDIF}
  FAutoGenerate := TLccEventAutoGenerate.Create;
end;

destructor TLccEvents.Destroy;
begin
  Clear;
  FreeAndNil(FEventList);
  FreeAndNil(FAutoGenerate);
  inherited Destroy;
end;

function TLccEvents.GetEvent(Index: Integer): TLccEvent;
begin
  {$IFDEF FPC} Result := TLccEvent( EventList[Index])
  {$ELSE}      Result := EventList[Index] {$ENDIF}
end;

function TLccEvents.GetEventIDAsStr(Index: Integer): String;
begin
  Result := EventIDToString(Event[Index].ID);
end;

function TLccEvents.GetCount: Integer;
begin
  Result := EventList.Count;
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
    {$IFDEF FPC} TObject( EventList[i]).Free;
    {$ELSE}      EventList[i].Free; {$ENDIF}
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

{ TLccFDI }

procedure TLccFDI.DoLoadComplete(LccMessage: TLccMessage);
//var
 // SourceNode, DestNode: TBaseLccNode;
begin
 { if Assigned(OwnerManager) then
  begin
    SourceNode := OwnerManager.FindSourceNode(LccMessage);
    DestNode := OwnerManager.FindDestNode(LccMessage);
    if Assigned(SourceNode) and Assigned(DestNode) then
      OwnerManager.DoFDI(SourceNode, DestNode);
  end; }
end;

function TLccFunctionConfiguration.GetFunctionStates(iIndex: Integer): Boolean;
begin
  if (iIndex > -1) and (iIndex < 30) then
    Result := FFunctionStatesArray[iIndex] = 1
  else
    Result := False;
end;

function TLccFunctionConfiguration.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  SourceNode, DestNode: TBaseLccNode;
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
  {  if Assigned(OwnerManager) then
    begin
      SourceNode := OwnerManager.FindSourceNode(LccMessage);
      DestNode := OwnerManager.FindDestNode(LccMessage);
      if Assigned(SourceNode) and Assigned(DestNode) then
        OwnerManager.DoFunctionConfiguration(SourceNode, DestNode);
    end;    }
  end;
end;

{TLccProtocolSupport}

procedure TLccProtocolSupport.DecodeFlags;
begin
  if Length(Flags) > 0 then
  begin
    FACDI := Flags[0] and PIP_ABBREVIATED_CDI <> 0;
    FCDI := Flags[0] and PIP_CDI <> 0;
    FDatagram := Flags[0] and PIP_DATAGRAM <> 0;
    FDisplay := Flags[0] and PIP_DISPLAY <> 0;
    FEventExchange := Flags[0] and PIP_EVENT_EXCHANGE <> 0;
    FFDI := Flags[0] and PIP_FDI <> 0;
    FFunctionConfiguration := Flags[0] and PIP_FUNCTION_CONFIGURATION <> 0;
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
    Valid := True;
  end;
end;

function TLccProtocolSupport.EncodeFlags: QWord;
begin
  Result := 0;
  if ACDI then Result := Result or PIP_ABBREVIATED_CDI;
  if CDI then Result := Result or PIP_CDI;
  if Datagram then Result := Result or PIP_DATAGRAM;
  if Display then Result := Result or PIP_DISPLAY;
  if EventExchange then Result := Result or PIP_EVENT_EXCHANGE;
  if FDI then Result := Result or PIP_FDI;
  if FunctionConfiguration then Result := Result or PIP_FUNCTION_CONFIGURATION;
  if Identification then Result := Result or PIP_PIP;
  if MemConfig then Result := Result or PIP_MEMORY_CONFIG;
  if RemoteButton then Result := Result or PIP_REMOTE_BUTTON;
  if Reservation then Result := Result or PIP_RESERVATION;
  if SimpleNodeInfo then Result := Result or PIP_SIMPLE_NODE_INFO;
  if Stream then Result := Result or PIP_STREAM;
  if Teach_Learn then Result := Result or PIP_TEACH_LEARN;
  if SimpleTrainNodeInfo then Result := Result or PIP_SIMPLE_TRAIN_NODE_INFO;
  if TractionControl then Result := Result or PIP_TRACTION;
  if TractionProxy then Result := Result or PIP_TRACTION_PROXY;
end;

function TLccProtocolSupport.ProcessMessage(LccMessage: TLccMessage): Boolean;
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

{ TLccCoreNode }

function TLccCoreNode.GetNodeIDStr: String;
begin
  Result := IntToHex(NodeID[1], 6);
  Result := Result + IntToHex(NodeID[0], 6);
  Result := '0x' + Result
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

function TLccCoreNode.IsMessageSourceUsingThisAlias(LccMessage: TLccMessage): Boolean;
begin
  Result := Permitted and LccMessage.IsCAN and (LccMessage.CAN.SourceAlias = AliasID)
end;

procedure TLccCoreNode.DoCanDuplicatedAlias(LccMessage: TLccMessage);
begin
  if ((LccMessage.CAN.MTI and $0F000000) >= MTI_CAN_CID6) and ((LccMessage.CAN.MTI and $0F000000) <= MTI_CAN_CID0) then
  begin
    WorkerMessage.LoadRID(AliasID);                   // sorry charlie this is mine
    DoSendMessage(WorkerMessage);
  end else
  begin
    WorkerMessage.LoadAMR(NodeID, AliasID);          // You used my Alias you dog......
    DoSendMessage(WorkerMessage);
    FPermitted := False;
    Login(False, True);
  end;
end;

function TLccCoreNode.DoDatagram(LccMessage: TLccMessage): Boolean;
begin
  case LccMessage.DataArrayIndexer[0] of
    DATAGRAM_PROTOCOL_CONFIGURATION : Result := DoDatagramConfiguration(LccMessage);
  else
    DoUnknownLccDatagramMessage(LccMessage);
  end;
end;

function TLccCoreNode.DoDatagramConfiguration(LccMessage: TLccMessage): Boolean;
begin
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

procedure TLccCoreNode.DoSendMessage(LccMessage: TLccMessage);
begin
 // if Assigned(SendMessage) then
 //   SendMessage(LccMessage);
end;

function TLccCoreNode.GetAliasIDStr: String;
begin
  Result := '0x' + IntToHex(FAliasID, 4);
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
var
  LccDestNode: TLccCoreNode;
  EventPtr: PEventID;
begin
  Result := False;
  LccDestNode := nil;

 { if Assigned(OwnerManager) then
  begin
    if LccMessage.HasDestination then
    begin
      LccDestNode := OwnerManager.FindDestNode(LccMessage, True);
      if not Assigned(LccDestNode) then
      begin
        LccDestNode := OwnerManager.CreateNodeByDestMessage(LccMessage);
        if Assigned(OwnerManager) and OwnerManager.AutoInterrogateDiscoveredNodes and not LccMessage.IsCAN then  // Have other nodes send out Verified
        begin
          WorkerMessage.LoadVerifyNodeIDAddressed(NodeID, AliasID, LccDestNode.NodeID, LccDestNode.AliasID);
          OwnerManager.DoRequestMessageSend(WorkerMessage);
        end;
      end;
    end;
  end;  }

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
          MTI_INITIALIZATION_COMPLETE    : Result := DoInitializatinComplete(LccMessage);
          MTI_PROTOCOL_SUPPORT_REPLY     : Result := DoProtocolSupportReply(LccMessage);
          MTI_VERIFY_NODE_ID_NUMBER      : Result := DoVerifyNodeIdNumber(LccMessage);
          MTI_VERIFY_NODE_ID_NUMBER_DEST : Result := DoVerifyNodeIdNumberDest(LccMessage);
          MTI_VERIFIED_NODE_ID_NUMBER    : Result := DoVerifiedNodeIDNumber(LccMessage);
          MTI_SIMPLE_NODE_INFO_REPLY     : Result := DoSimpleNodeInfoReply(LccMessage);
          MTI_SIMPLE_TRAIN_INFO_REPLY    : Result := DoSimpleTrainInfoReply(LccMessage);
          MTI_PRODUCER_IDENTIFIED_SET    : Result := DoProducerIdentifiedSet(LccMessage);
          MTI_PRODUCER_IDENTIFIED_CLEAR  : Result := DoProducerIdentifiedClear(LccMessage);
          MTI_PRODUCER_IDENTIFIED_UNKNOWN: Result := DoProducerIdentifiedUnknown(LccMessage);
          MTI_CONSUMER_IDENTIFIED_SET    : Result := DoConsumerIdentifiedUnknown(LccMessage);
          MTI_CONSUMER_IDENTIFIED_CLEAR  : Result := DoConsumerIdentifiedUnknown(LccMessage);
          MTI_CONSUMER_IDENTIFIED_UNKNOWN: Result := DoConsumerIdentifiedUnknown(LccMessage);
          MTI_TRACTION_PROTOCOL          : Result := DoTractionProtocol(LccMessage);
          MTI_TRACTION_REPLY             : Result := DoTractionProtocolReply(LccMessage);
          MTI_DATAGRAM                   : Result := DoDatagram(LccMessage);
        else
          DoUnknownLccMessge(LccMessage);
        end;
      end;
    end;
  end;
end;

procedure TLccCoreNode.SendAckReply(LccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  WorkerMessage.LoadDatagramAck(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, True, ReplyPending, TimeOutValueN);
  DoSendMessage(LccMessage);
end;

{ TLccSimpleNodeInfo }

function TLccSimpleNodeInfo.GetPackedFormat: TSimpleNodeInfoPacked;
const
  NULL_COUNT = 6;
  VERSION_COUNT = 2;
var
  iArray, i: Integer;
begin
  i :=  Length(Manufacturer) + Length(Model) + Length(HardwareVersion) + Length(SoftwareVersion) + Length(UserName) + Length(UserDescription);
  i := i + NULL_COUNT + VERSION_COUNT;
  SetLength(FPackedInfo, i);
  iArray := 0;

  FPackedInfo[iArray] := Version;      // 4 Items follow
  Inc(iArray);
  StringToNullArray(Manufacturer, FPackedInfo, iArray);
  StringToNullArray(Model, FPackedInfo, iArray);
  StringToNullArray(HardwareVersion, FPackedInfo, iArray);
  StringToNullArray(SoftwareVersion, FPackedInfo, iArray);

  FPackedInfo[iArray] := UserVersion;  // 2 items follow
  Inc(iArray);
  StringToNullArray(UserName, FPackedInfo, iArray);
  StringToNullArray(UserDescription, FPackedInfo, iArray);

  Result := FPackedInfo;
end;

function TLccSimpleNodeInfo.GetUserDescription: String;
begin
  Result := FUserDescription;
  if Assigned(Configuration) then
    Result := Configuration.ReadAsString(64);
end;

function TLccSimpleNodeInfo.GetUserName: String;
begin
  Result := FUserName;
  if Assigned(Configuration) then
    Result := Configuration.ReadAsString(1);
end;

constructor TLccSimpleNodeInfo.Create(AnOwner: TComponent; AConfiguration: TLccConfiguration);
begin
  inherited Create(AnOwner);
  FConfiguration := AConfiguration;
end;

{$IFDEF FPC}
function TLccSimpleNodeInfo.LoadFromXml(CdiFilePath: String): Boolean;
var
  XMLDoc: TXMLDocument;
  CdiNode, IdentificationNode, ChildNode: TDOMNode;
begin
  Result := False;
  if FileExists(CdiFilePath) then
  begin
    try
      ReadXMLFile(XmlDoc, CdiFilePath);
      if Assigned(XmlDoc) then
      begin
        CdiNode := XmlDoc.FindNode('cdi');
        if Assigned(CdiNode) then
        begin
          IdentificationNode := CdiNode.FindNode('identification');
          if Assigned(IdentificationNode) then
          begin
             Version := 1;
             ChildNode := IdentificationNode.FindNode('manufacturer');
             if Assigned(ChildNode) then FManufacturer := ChildNode.FirstChild.NodeValue else Exit;
             ChildNode := IdentificationNode.FindNode('model');
             if Assigned(ChildNode) then FModel := ChildNode.FirstChild.NodeValue else Exit;
             ChildNode := IdentificationNode.FindNode('hardwareVersion');
             if Assigned(ChildNode) then FHardwareVersion := ChildNode.FirstChild.NodeValue else Exit;
             ChildNode := IdentificationNode.FindNode('softwareVersion');
             if Assigned(ChildNode) then FSoftwareVersion := ChildNode.FirstChild.NodeValue else Exit;
             UserVersion := 1;
             Result := True;
          end;
        end;
      end;
    except
      // Quiet fail
    end;
  end;
end;

function TLccSimpleNodeInfo.ProcessMessage(LccMessage: TLccMessage): Boolean;

  {$IFDEF LCC_MOBILE}
  function NextString(AStrPtr: PChar): PChar;
  {$ELSE}
  function NextString(AStrPtr: PAnsiChar): PAnsiChar;
  {$ENDIF}

  begin
    Result := AStrPtr;
    while Result^ <> #0 do
      Inc(Result);
    Inc(Result);
  end;

{$IFDEF LCC_MOBILE}
var
  StrPtr: PChar;
{$ELSE}
var
  StrPtr: PAnsiChar;
{$ENDIF}
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
{$ENDIF}

{ TLccTraction }

procedure TLccTraction.SetFunctions(Index: DWord; AValue: Word);
begin
  GrowArray(Index + 1);
  FunctionArray[Index] := AValue
end;

function TLccTraction.GetFunctions(Index: DWord): Word;
begin
  GrowArray(Index + 1);
  Result := FunctionArray[Index];
end;

procedure TLccTraction.GrowArray(NewSize: DWord);
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

function TLccTraction.IsLinked: Boolean;
begin
  Result := Assigned(LinkedNode)
end;

function TLccTraction.ProcessMessage(LccMessage: TLccMessage): Boolean;
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

{TLccSimpleTrainNodeInfo}

function TLccSimpleTrainNodeInfo.ProcessMessage(LccMessage: TLccMessage;
  Traction: TLccTraction): Boolean;

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

{TBaseStreamLccNode}

procedure TBaseStreamLccNode.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
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
  if iStart = 7 then
    OutMessage.DataArrayIndexer[6] := LccMessage.DataArrayIndexer[6];

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

function TBaseStreamLccNode.ProcessMessage(LccMessage: TLccMessage): Boolean;
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
      WorkerMessage.IsCAN := False;
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
  //    OwnerManager.DoRequestMessageSend(WorkerMessage);
    end;
  end;
end;

end.

