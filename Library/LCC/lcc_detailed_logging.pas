unit lcc_detailed_logging;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, SynEdit, math, lcc_math_float16, strutils, lcc_defines,
  lcc_threadedcirculararray;


  procedure PrintToSynEdit(MessageStr: AnsiString; SynEditLog: TSynEdit; Paused: Boolean; Detailed: Boolean; JMRIFormat: Boolean);
  procedure PrintTCPToSynEdit(MessageStr: WideString; ByteArray: TDynamicByteArray; SynEditLog: TSynEdit; Paused: Boolean; Detailed: Boolean; JMRIFormat: Boolean);
  function MTI_ToString(MTI: DWord): WideString;


implementation

const
  CAN_BYTE_COUNT = 8;
  MAX_EVENT_LEN                            = 8;
  MAX_DATAGRAM_LENGTH = 72;
  MAX_MULTIFRAME_LEN = 12;

type
  TCANByteArray = array[0..CAN_BYTE_COUNT-1] of Byte;
  PCANByteArray = ^TCANByteArray;

type
  TDatagramArray = array[0..MAX_DATAGRAM_LENGTH-1] of Byte;
  PDatagramArray = ^TDatagramArray;

  TStreamArray = array of Byte;

  TEventID = array[0..MAX_EVENT_LEN-1] of Byte;
  PEventID = ^TEventID;

  TMultiFrameArray = array[0..MAX_MULTIFRAME_LEN-1] of Byte;
  PMultiFrameArray = ^TMultiFrameArray;

  TOpenLCBLayer = (ol_CAN, ol_OpenLCB);

  THexArray = TEventID;

type
  TLccMessage = class
     // Core base for objects that hold OpenLCB messages,
   end;

  { TLccMessageHelper }

  TLccMessageHelper = class( TLccMessage)
  private
    FDestinationAliasID: Word;
    FFramingBits: Byte;
    FHasDestinationAddress: Boolean;
    FForwardingBitNotSet: Boolean;
    FSourceAliasID: Word;
    FData: TCANByteArray;
    FDataCount: Integer;
    FLayer: TOpenLCBLayer;
    FMTI: DWord;
    FUnimplementedBitsSet: Boolean;
    procedure SetData(AValue: TCANByteArray);
    procedure SetLayer(AValue: TOpenLCBLayer);
  public
    property Layer: TOpenLCBLayer read FLayer write SetLayer;
    property MTI: DWord read FMTI write FMTI;
    property Data: TCANByteArray read FData write SetData;
    property DataCount: Integer read FDataCount write FDataCount;
    property SourceAliasID: Word read FSourceAliasID write FSourceAliasID;
    property DestinationAliasID: Word read FDestinationAliasID write FDestinationAliasID;
    property ForwardingBitNotSet: Boolean read FForwardingBitNotSet write FForwardingBitNotSet;
    property FramingBits: Byte read FFramingBits write FFramingBits;
    property UnimplementedBitsSet: Boolean read FUnimplementedBitsSet write FUnimplementedBitsSet;
    property HasDestinationAddress: Boolean read FHasDestinationAddress write FHasDestinationAddress;

    constructor Create;
    destructor Destroy; override;
    procedure CopyTo(Target: TLccMessageHelper);
    function Decompose(MessageStr: AnsiString): Boolean;
    function Encode: AnsiString;
    procedure Load(ALayer: TOpenLCBLayer; AMTI: DWord; ASourceAlias: Word; ADestinationAlias: Word; ADataCount: Integer; AData0, AData1, AData2, AData3, AData4, AData5, AData6, AData7: Byte);
    procedure StoreNodeIDToData(NodeID: Int64; IsAddressed: Boolean);
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
    function ExtractDataBytesAsString(StartIndex, Count: Integer): String;
    function ExtractDataBytesAsEventID: PEventID;
    procedure IntToByteArray(Int: QWord; var ByteArray: TCANByteArray);
  end;

  { TMultiFrameBuffer }

  TMultiFrameBuffer = class
  private
    FAliasID: Word;
    FCurrentIndex: Integer;
    FDataArray: TMultiFrameArray;
    FDataArraySize: Integer;
  public
    constructor Create;
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
    function ExtractDataBytesAsHex(StartByteIndex, EndByteIndex: Integer): string;
    property AliasID: Word read FAliasID write FAliasID;
    property DataArray: TMultiFrameArray read FDataArray write FDataArray;
    property DataArraySize: Integer read FDataArraySize;
    property CurrentIndex: Integer read FCurrentIndex write FCurrentIndex;
  end;

  { TMultiFrameBufferList }

  TMultiFrameBufferList = class
  private
    FList: TList;
    function GetMultiFrameBuffer(Index: Integer): TMultiFrameBuffer;
    procedure SetMultiFrameBuffer(Index: Integer; AValue: TMultiFrameBuffer);
  protected
    property List: TList read FList write FList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function FindByAlias(TestAliasID: Word): TMultiFrameBuffer;
    function ProcessFrame(NewFrame: TLccMessageHelper): TMultiFrameBuffer;
    property MultiFrameBuffers[Index: Integer]: TMultiFrameBuffer read GetMultiFrameBuffer write SetMultiFrameBuffer;
  end;

const
  MTI_ADDRESS_PRESENT                = $00008000;                                // Address in the CAN Data present if set

  MTI_CAN                            = $00000000;                                // Frame Type CAN Control Message
  MTI_CID0                           = $07000000;                                // First 12 Bits of 48 bit Node ID
  MTI_CID1                           = $06000000;                                // 2rd 12 Bits of 48 bit Node ID
  MTI_CID2                           = $05000000;                                // 3nd 12 Bits of 48 bit Node ID
  MTI_CID3                           = $04000000;                                // Last 12 Bits of 48 bit Node ID
  MTI_CID4                           = $03000000;                                // non-OpenLCB Protocol
  MTI_CID5                           = $02000000;                                // non-OpenLCB Protocol
  MTI_CID6                           = $01000000;                                // non-OpenLCB Protocol
  MTI_CID_MASK                       = $07000000;

  MTI_RID                            = $00700000;                                // Reserve ID
  MTI_AMD                            = $00701000;                                // Alias Map Definition
  MTI_AME                            = $00702000;                                // Alias Mapping Enquiry
  MTI_AMR                            = $00703000;                                // Alias Map Reset Frame

  MTI_MASK                              = $0FFFF000;
  MTI_FRAME_TYPE_MASK                   = $0F000000;
  MTI_FRAME_TYPE_GENERAL                = $09000000;
  MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME    = $0A000000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME_START   = $0B000000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME         = $0C000000;
  MTI_FRAME_TYPE_DATAGRAM_FRAME_END     = $0D000000;

  MTI_ADDRESSED_MASK                 = $00008000;
  MTI_SIMPLE_PROTOCOL_MASK           = $00010000;
  MTI_EVENT_PRESENT_MASK             = $00002000;

  MTI_INITIALIZATION_COMPLETE        = $09100000;                                // Databytes = Full Node ID
  MTI_VERIFY_NODE_ID_NUMBER_DEST     = $09488000;                                // Databytes = Destination Alias
  MTI_VERIFY_NODE_ID_NUMBER          = $09490000;                                //
  MTI_VERIFIED_NODE_ID_NUMBER        = $09170000;                                // {Optional Full Node ID}
  MTI_OPTIONAL_INTERACTION_REJECTED  = $09068000;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_TERMINATE_DUE_TO_ERROR         = $090A8000;                                // Databytes = Destination Alias, Error, {Optional Info}

  MTI_PROTOCOL_SUPPORT_INQUIRY       = $09828000;                                // Databytes = Destination Alias
  MTI_PROTOCOL_SUPPORT_REPLY         = $09668000;                                // Databytes = Destination Alias, Protocol Flags

  MTI_CONSUMER_IDENTIFY              = $098F4000;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFY_RANGE        = $094A4000;                                // Databytes = EventID with Mask
  MTI_CONSUMER_IDENTIFIED_UNKNOWN    = $094C7000;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_SET        = $094C4000;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_CLEAR      = $094C5000;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_RESERVED   = $094C6000;                                // Databytes = EventID
  MTI_PRODUCER_IDENDIFY              = $09914000;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFY_RANGE        = $09524000;                                // Databytes = EventID with Mask
  MTI_PRODUCER_IDENTIFIED_UNKNOWN    = $09547000;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_SET        = $09544000;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_CLEAR      = $09545000;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_RESERVED   = $09546000;                                // Databytes = EventID
  MTI_EVENTS_IDENTIFY_DEST           = $09968000;                                // Databytes = Destination Alias
  MTI_EVENTS_IDENTIFY                = $09970000;                                //
  MTI_EVENT_LEARN                    = $09594000;                                // Databytes = EventID
  MTI_PC_EVENT_REPORT                = $095B4000;                                // Databytes = EventID  (Infamouse PCER)

  MTI_SIMPLE_NODE_INFO_REQUEST       = $09DE8000;                                // Databytes = Destination Alias
  MTI_SIMPLE_NODE_INFO_REPLY         = $09A08000;                                // Databytes = Destination Alias, ACDI Data

  MTI_SIMPLE_TRAIN_INFO_REQUEST      = $09DA8000;                                // Databytes = Destination Alias
  MTI_SIMPLE_TRAIN_INFO_REPLY        = $099C8000;                                // Databytes = Destination Alias, ACDI Data

  MTI_TRACTION_PROTOCOL              = $095EA000;                                // Databyte = depends
  MTI_TRACTION_PROXY_PROTOCOL        = $091EA000;
  MTI_TRACTION_REPLY                 = $095E8000;                                // Databyte = depends
  MTI_TRACTION_PROXY_REPLY           = $091E8000;

  MTI_STREAM_INIT_REQUEST            = $09CC8000;
  MTI_STREAM_INIT_REPLY              = $09868000;
  MTI_FRAME_TYPE_CAN_STREAM_SEND     = $0F000000;
  MTI_STREAM_PROCEED                 = $09888000;
  MTI_STREAM_COMPLETE                = $098A8000;

  MTI_DATAGRAM_OK_REPLY              = $09A28000;                                // Databytes = Destination Alias
  MTI_DATAGRAM_REJECTED_REPLY        = $09A48000;                                // Databytes = Destination Alias, Error Code

  DATAGRAM_OK_ACK_REPLY_PENDING      = $80;

  STREAM_REPLY_CONTENT_TYPE                               = $01;                // LSB = 1 = first 6 bytes in data are UID of data type in stream
  STREAM_REPLY_UNEXPECTED_ERROR                           = $02;                // Bit 2 = 1 the Stream was Rejected, Out of order, or other "should not happen" error
  STREAM_REPLY_PERMANENT_ERROR                            = $40;                // Bit 6 = 1 = if STREAM_REPLY_ACCEPT = 1 then this is the error type where 1 = permanent
  STREAM_REPLY_ACCEPT                                     = $80;                // MSB = 1 = Accept

  STREAM_REPLY_ERROR_LOGGED                               = $01;                // Error was logged
  STREAM_REPLY_INVALID_REQUEST                            = $20;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_SOURCE_NOT_PERMITTED                       = $40;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_STREAM_NOT_ACCEPTED                        = $80;                // if Error is permanent then these are the possible reasons

  MASK_SOURCE_ALIAS                  = $00000FFF;                                // Masks out just the Source Alias Address

  PIP_PIP                            = $800000000000;
  PIP_DATAGRAM                       = $400000000000;
  PIP_STREAM                         = $200000000000;
  PIP_MEMORY_CONFIG                  = $100000000000;
  PIP_RESERVATION                    = $080000000000;
  PIP_EVENT_EXCHANGE                 = $040000000000;
  PIP_IDENTIFCIATION                 = $020000000000;
  PIP_TEACH_LEARN                    = $010000000000;
  PIP_REMOTE_BUTTON                  = $008000000000;
  PIP_ABBREVIATED_CDI                = $004000000000;
  PIP_DISPLAY                        = $002000000000;
  PIP_SIMPLE_NODE_ID                 = $001000000000;
  PIP_CDI                            = $000800000000;
  PIP_TRACTION                       = $000400000000;
  PIP_FDI                            = $000200000000;
  PIP_TRACTION_PROXY                 = $000100000000;

  PIP_UNASSIGNED                     = $0000FFFFFFF0;
  PIP_RESERVED                       = $00000000000F;

  STR_PIP_PIP                        = 'Protocol Identification Protocol';
  STR_PIP_DATAGRAM                   = 'Datagram Protocol';
  STR_PIP_STREAM                     = 'Stream Protocol';
  STR_PIP_MEMORY_CONFIG              = 'Memory Configuration Protocol';
  STR_PIP_RESERVATION                = 'Reservation Protocol';
  STR_PIP_EVENT_EXCHANGE             = 'Event Exchange Protocol';
  STR_PIP_IDENTIFCIATION             = 'Identification Protocol';
  STR_PIP_TEACH_LEARN                = 'Teach/Learn Protocol';
  STR_PIP_REMOTE_BUTTON              = 'Remote Button Protocol';
  STR_PIP_ABBREVIATED_CDI            = 'Abbreviated CDI Protocol';
  STR_PIP_DISPLAY                    = 'Display Protocol';
  STR_PIP_SIMPLE_NODE_ID             = 'Simple Node ID (SNII/SNIP) Protocol';
  STR_PIP_CDI                        = 'Configuration Description Information (CDI) Protocol';
  STR_PIP_TRACTION                   = 'Traction Protocol';
  STR_PIP_FDI                        = 'Function Description Information (FDI) Protocol';
  STR_PIP_TRACTION_PROTOCOL                        = 'Traction Proxy Protocol';


  OIR_TEMPORARY_ERROR                 = $1000;
  OIR_PERMANENT_ERROR                 = $2000;

const
  MCP_WRITE                           = $00;                                    // MemoryConfigurationProtocol - Write Memory Mask
  MCP_WRITE_STREAM                    = $20;
  MCP_READ                            = $40;                                    // MemoryConfigurationProtocol - Read Memory Mask
  MCP_READ_STREAM                     = $60;
  MCP_OPERATION                       = $80;                                    // MemoryConfigurationProtocol - Operation Mask
  MCP_READ_DATAGRAM_REPLY             = $50;                                    // MemoryConfigurationProtocol - Read Reply Mask [Does not include the Address Space Mask "or" it with the the Address space masks below]
  MCP_WRITE_DATAGRAM_REPLY            = $10;
  MCP_READ_STREAM_REPLY               = $60;

  MCP_READ_OK                         = $50;
  MCP_READ_ERROR                      = $58;
  MCP_WRITE_OK                        = $10;
  MCP_WRITE_ERROR                     = $18;

  MCP_CDI                             = $03;                                    // Address space = CDI ($FF) access Mask
  MCP_ALL                             = $02;                                    // Address space = All ($FE) access Mask
  MCP_CONFIGURATION                   = $01;                                    // Address space = Basic Configuration ($FD) access Mask
  MCP_NONE                            = $00;                                    // Use the optional {Space} byte in the datagram to defin the address space

  MCP_OP_GET_CONFIG                  = $80;                                     // MemoryConfigurationProtocol Operation - Get Configuration
  MCP_OP_GET_CONFIG_REPLY            = $82;                                     // MemoryConfigurationProtocol Operation - Get Configuration Reply
  MCP_OP_GET_ADD_SPACE_INFO          = $84;                                     // MemoryConfigurationProtocol Operation - Get Add Space Info
  MCP_OP_GET_ADD_SPACE_INFO_REPLY    = $86;                                     // MemoryConfigurationProtocol Operation - Get Add Space Info Reply
  MCP_OP_LOCK                        = $88;                                     // MemoryConfigurationProtocol Operation - Lock Node
  MCP_OP_LOCK_REPLY                  = $8A;                                     // MemoryConfigurationProtocol Operation - Lock Node Reply
  MCP_OP_GET_UNIQUEID                = $8C;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key
  MCP_OP_GET_UNIQUEID_REPLY          = $8E;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key Reply

  MCP_OP_GET_ADD_SPACE_INFO_REPLY_PRESENT = $01;

  MCP_OP_FREEZE                      = $A0;                                     // MemoryConfigurationProtocol Operation - Freeze Node
  MCP_OP_INDICATE                    = $A4;                                     // MemoryConfigurationProtocol Operation - Indicate
  MCP_OP_RESETS                      = $A8;                                     // MemoryConfigurationProtocol Operation - Resets


  MSI_CDI                            = $FF;                                     // MemorySpaceIdentifier - Access the Configuration Definition Infomation (CDI)
  MSI_ALL                            = $FE;                                     // MemorySpaceIdentifier - Access All memory (define all in the application)
  MSI_CONFIG                         = $FD;                                     // MemorySpaceIdentifier - Access basic configuration memory that feeds into the CDI
  MSI_ACDI_MFG                       = $FC;                                     // MemorySpaceIdentifier - Access the ACDI Manfacturers Info
  MSI_ACDI_USER                      = $FB;                                     // MemorySpaceIdentifier - Access the ACDI User definable Info
  MSI_FDI                            = $FA;                                     // MemorySpaceIdentifier - Access the Traction Functions definable Info
  MSI_FSI                            = $F9;                                     // MemorySpaceIdentifier = Access the Traction Function State Information

  MCO_WRITE_UNDER_MASK               = $8000;                                   // MemoryConfigurationOptions - Write under mask supported
  MCO_UNALIGNED_READS                = $4000;                                   // MemoryConfigurationOptions - Unaligned memory Reads supported
  MCO_UNALIGNED_WRITES               = $2000;                                   // MemoryConfigurationOptions - Unaligned memory Writes supported
  MCO_ACDI_MFG_READS                 = $0800;                                   // MemoryConfigurationOptions - Address Space 0xFC supported (ACDI Manufacturer Area) for reads
  MCO_ACDI_USER_READS                = $0400;                                   // MemoryConfigurationOptions - Address Space 0xFB supported (ACDI User Defined Area) for reads
  MCO_ACDI_USER_WRITES               = $0200;                                   // MemoryConfigurationOptions - Address Space 0xFB supported (ACDI User Defined Area) for writes
  MCO_RESERVED                       = $1FFF;

  MCWL_ONE_BYTE                      = $80;                                     // MemoryConfigurationWriteLength - 1 Byte Write Supported
  MCWL_TWO_BYTE                      = $40;                                     // MemoryConfigurationWriteLength - 2 Byte Write Supported
  MCWL_FOUR_BYTE                     = $20;                                     // MemoryConfigurationWriteLength - 4 Byte Write Supported
  MCWL_64_BYTE                       = $10;                                     // MemoryConfigurationWriteLength - 64 Byte (exactly) Write Supported
  MCWL_ARBITRARY_BYTE                = $02;                                     // MemoryConfigurationWriteLength - Any Number of Byte Write Supported
  MCWL_STREAM_WRITE_SUPPORTED        = $01;                                     // MemoryConfigurationWriteLength - Stream Write Supported
  MCWL_RESERVED                      = $0C;

  TRACTION_PROXY_ALLOCATE            = $01;
  TRACTION_PROXY_ATTACH              = $02;
  TRACTION_PROXY_DETACH              = $03;
  TRACTION_PROXY_MANAGE              = $80;
  TRACTION_PROXY_MANAGE_RESERVE      = $01;
  TRACTION_PROXY_MANAGE_RELEASE      = $02;

  TRACTION_PROXY_TECH_ID_DCC              = $01;
  TRACTION_PROXY_TECH_ID_DC               = $02;
  TRACTION_PROXY_TECH_ID_MARKLIN_DIG      = $03;
  TRACTION_PROXY_TECH_ID_MARKLIN_DELTA    = $04;
  TRACTION_PROXY_TECH_ID_MARKLIN_DIG_ESU  = $05;
  TRACTION_PROXY_TECH_ID_SELECTRIX        = $06;
  TRACTION_PROXY_TECH_ID_MTH_DCS          = $07;
  TRACTION_PROXY_TECH_ID_LIONEL_TMCC      = $08;

  TRACTION_PROXY_MANAGE_RESERVE_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_PROXY_MANAGE_RESERVE_REPLY_FAIL = $FF;    // Failed

  //

  TRACTION_FLAGS_ALIAS_INCLUDED       = $01;

  TRACTION_SPEED_DIR                  = $00;
  TRACTION_FUNCTION                   = $01;
  TRACTION_E_STOP                     = $02;

  TRACTION_QUERY_SPEED                = $10;
  TRACTION_QUERY_FUNCTION             = $11;

  TRACTION_CONTROLLER_CONFIG          = $20;
  TRACTION_CONTROLLER_CONFIG_ASSIGN   = $01;
  TRACTION_CONTROLLER_CONFIG_RELEASE  = $02;
  TRACTION_CONTROLLER_CONFIG_QUERY    = $03;
  TRACTION_CONTROLLER_CONFIG_NOTIFY   = $04;
  TRACTION_CONTROLLER_CONFIG_REPLY_OK = $00;
  TRACTION_CONTROLLER_CONFIG_REPLY_FAIL = $FF;

  TRACTION_CONSIST                    = $30;
  TRACTION_CONSIST_ATTACH             = $01;
  TRACTION_CONSIST_DETACH             = $02;
  TRACTION_CONSIST_QUERY              = $03;

  TRACTION_MANAGE                     = $40;
  TRACTION_MANAGE_RESERVE             = $01;
  TRACTION_MANAGE_RELEASE             = $02;

  TRACTION_MANAGE_RESERVE_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_MANAGE_RESERVE_REPLY_FAIL = $FF;    // Failed


  MAX_CONFIG_MEM_READWRITE_SIZE = 64;

    DATAGRAM_REJECTED                        = $0000;
  DATAGRAM_REJECTED_PERMANENT_ERROR        = $1000;
  DATAGRAM_REJECTED_INFORMATION_LOGGED     = $1010;
  DATAGRAM_REJECTED_SOURCE_NOT_PERMITTED   = $1020;
  DATAGRAM_REJECTED_DATAGRAMS_NOT_ACCEPTED = $1040;
  DATAGRAM_REJECTED_BUFFER_FULL            = $2000;
  DATAGRAM_REJECTED_OUT_OF_ORDER           = $6000;
  DATAGRAM_REJECTED_NO_RESEND_MASK         = $1000;
  DATAGRAM_REJECTED_RESEND_MASK            = $2000;
  DATAGRAM_REJECTED_TRANSPORT_ERROR_MASK   = $4000;

  DATAGRAM_PROTOCOL_LOGREQUEST             = $01;
  DATAGRAM_PROTOCOL_LOGREPLY               = $02;
  DATAGRAM_PROTOCOL_CONFIGURATION          = $20;
  DATAGRAM_PROTOCOL_REMOTEBUTTON           = $21;
  DATAGRAM_PROTOCOL_DISPLAY                = $28;
  DATAGRAM_PROTOCOL_TRAINCONTROL           = $30;
  DATAGRAM_PROTOCOL_TWOBYTE                = $E0;
  DATAGRAM_PROTOCOL_SIXBYTE                = $F0;


const
  NULL_EVENT_ID : TEventID = (0, 0, 0, 0, 0, 0, 0, 0);
  EVENT_EMERGENCY_STOP       : TEventID = ($01, $00, $00, $00, $00, $00, $FF, $FF);
  EVENT_NEW_LOG_ENTRY        : TEventID = ($01, $00, $00, $00, $00, $00, $FF, $F8);
  EVENT_IDENT_BUTTON_PRESSED : TEventID = ($01, $00, $00, $00, $00, $00, $FE, $00);
  EVENT_DUPLICATE_ID_DETECTED: TEventID = ($01, $10, $00, $00, $00, $00, $02, $01);
  EVENT_IS_TRAIN             : TEventID = ($01, $01, $00, $00, $00, $00, $03, $03);
  EVENT_IS_PROXY             : TEventID = ($01, $01, $00, $00, $00, $00, $03, $04);
  EVENT_DELIVERS_CLOCK       : TEventID = ($01, $01, $00, $00, $00, $00, $05, $01);

var
  LocalHelper: TLccMessageHelper;
  MultiFrames: TMultiFrameBufferList;
  LogStrings: TStringList;

function EqualEvents(Event1, Event2: PEventID): Boolean;
begin
  Result := (Event1^[0] = Event2^[0]) and (Event1^[1] = Event2^[1]) and (Event1^[2] = Event2^[2]) and (Event1^[3] = Event2^[3]) and
            (Event1^[4] = Event2^[4]) and (Event1^[5] = Event2^[5]) and (Event1^[6] = Event2^[6]) and (Event1^[7] = Event2^[7])
end;

function IsDatagramMTI(MTI: DWord; IncludeReplies: Boolean): Boolean;
begin
  Result := (MTI = MTI_FRAME_TYPE_DATAGRAM_FRAME_END) or (MTI = MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME) or (MTI = MTI_FRAME_TYPE_DATAGRAM_FRAME) or (MTI = MTI_FRAME_TYPE_DATAGRAM_FRAME_START);
  if IncludeReplies then
    Result := Result or (MTI = MTI_DATAGRAM_OK_REPLY) or (MTI = MTI_DATAGRAM_REJECTED_REPLY)
end;

function IsStreamMTI(MTI: DWord; IncludeSetupTeardowns: Boolean): Boolean;
begin
  if IncludeSetupTeardowns then
    Result := (MTI = MTI_FRAME_TYPE_CAN_STREAM_SEND) or  (MTI = MTI_STREAM_COMPLETE) or  (MTI = MTI_STREAM_INIT_REPLY) or  (MTI = MTI_STREAM_INIT_REQUEST) or  (MTI = MTI_STREAM_PROCEED)
  else
    Result := (MTI = MTI_FRAME_TYPE_CAN_STREAM_SEND)
end;

function MTI_ToString(MTI: DWord): WideString;

  procedure ConfigurationDatagram;
  begin
    if LocalHelper.DataCount > 0 then
        begin
          if LocalHelper.Data[0] = DATAGRAM_PROTOCOL_CONFIGURATION then
          begin
            case LocalHelper.Data[1] and $F8 of
              MCP_WRITE :
                  begin
                    case LocalHelper.Data[1] and $07 of
                      MCP_CDI            : Result := Result + ' Write Command, Address Space = CDI';
                      MCP_ALL            : Result := Result + ' Write Command, Address Space = All';
                      MCP_CONFIGURATION  : Result := Result + ' Write Command, Address Space = Configuration';
                      MCP_NONE           : begin
                                             case LocalHelper.Data[6] of
                                               MSI_CDI         : Result := Result + ' Write Command, Address Space = CDI';
                                               MSI_ALL         : Result := Result + ' Write Command, Address Space = All';
                                               MSI_CONFIG      : Result := Result + ' Write Command, Address Space = Configuration';
                                               MSI_ACDI_MFG    : Result := Result + ' Write Command, Address Space = ACDI Manufacturer';
                                               MSI_ACDI_USER   : Result := Result + ' Write Command, Address Space = ACDI User';
                                               MSI_FDI         : Result := Result + ' Write Command, Address Space = Function Definition Info';
                                               MSI_FSI         : Result := Result + ' Write Command, Address Space = Function State Info'
                                             else
                                               Result := Result + ' Write Command, Address Space = [Unknown]'      ;
                                             end;
                                           end
                    end; // Case
                    Result := Result + ', Starting Address = ' + IntToHex( LocalHelper.ExtractDataBytesAsInt(2, 5), 8);
                  end;
              MCP_READ  :
                  begin
                    case LocalHelper.Data[1] and $07 of
                      MCP_CDI            : Result := Result + ' Read Command, Address Space = CDI';
                      MCP_ALL            : Result := Result + ' Read Command, Address Space = All';
                      MCP_CONFIGURATION  : Result := Result + ' Read Command, Address Space = Configuration';
                      MCP_NONE           : begin
                                             case LocalHelper.Data[6] of
                                               MSI_CDI         : Result := Result + ' Read Command, Address Space = CDI';
                                               MSI_ALL         : Result := Result + ' Read Command, Address Space = All';
                                               MSI_CONFIG      : Result := Result + ' Read Command, Address Space = Configuration';
                                               MSI_ACDI_MFG    : Result := Result + ' Read Command, Address Space = ACDI Manufacturer';
                                               MSI_ACDI_USER   : Result := Result + ' Read Command, Address Space = ACDI User';
                                               MSI_FDI         : Result := Result + ' Read Command, Address Space = Function Definition Info';
                                               MSI_FSI         : Result := Result + ' Read Command, Address Space = Function State Info'
                                             else
                                               Result := Result + ' Read Command, Address Space = [Unknown]'      ;
                                             end;
                                           end
                    end; // Case
                    Result := Result + ', Starting Address = ' + IntToHex( LocalHelper.ExtractDataBytesAsInt(2, 5), 8);
                  end;
              MCP_OPERATION  :
                  begin
                  end;
              MCP_WRITE_DATAGRAM_REPLY :
                  begin
                    case LocalHelper.Data[1] and $07 of
                      MCP_CDI            : Result := Result + ' Write Reply, Address Space = CDI';
                      MCP_ALL            : Result := Result + ' Write Reply, Address Space = All';
                      MCP_CONFIGURATION  : Result := Result + ' Write Reply, Address Space = Configuration';
                      MCP_NONE           : begin
                                             case LocalHelper.Data[6] of
                                               MSI_CDI         : Result := Result + ' Write Reply, Address Space = CDI';
                                               MSI_ALL         : Result := Result + ' Write Reply, Address Space = All';
                                               MSI_CONFIG      : Result := Result + ' Write Reply, Address Space = Configuration';
                                               MSI_ACDI_MFG    : Result := Result + ' Write Reply, Address Space = ACDI Manufacturer';
                                               MSI_ACDI_USER   : Result := Result + ' Write Reply, Address Space = ACDI User';
                                               MSI_FDI         : Result := Result + ' Write Reply, Address Space = Function Definition Info';
                                               MSI_FSI         : Result := Result + ' Write Reply, Address Space = Function State Info'
                                             else
                                               Result := Result + ' Write Reply, Address Space = [Unknown]'      ;
                                             end;
                                           end
                    end; // Case
                    Result := Result + ', Starting Address = ' + IntToHex( LocalHelper.ExtractDataBytesAsInt(2, 5), 8);
                    if LocalHelper.Data[1] and $F8 = MCP_WRITE_OK then
                      Result := Result + ', Success'
                    else
                    if LocalHelper.Data[1] and $F8 = MCP_WRITE_ERROR then
                      Result := Result + ', Error'
                  end;
              MCP_READ_DATAGRAM_REPLY  :
                  begin
                    case LocalHelper.Data[1] and $07 of
                      MCP_CDI            : Result := Result + ' Read Reply, Address Space = CDI';
                      MCP_ALL            : Result := Result + ' Read Reply, Address Space = All';
                      MCP_CONFIGURATION  : Result := Result + ' Read Reply, Address Space = Configuration';
                      MCP_NONE           : begin
                                             case LocalHelper.Data[6] of
                                               MSI_CDI         : Result := Result + ' Read Reply, Address Space = CDI';
                                               MSI_ALL         : Result := Result + ' Read Reply, Address Space = All';
                                               MSI_CONFIG      : Result := Result + ' Read Reply, Address Space = Configuration';
                                               MSI_ACDI_MFG    : Result := Result + ' Read Reply, Address Space = ACDI Manufacturer';
                                               MSI_ACDI_USER   : Result := Result + ' Read Reply, Address Space = ACDI User';
                                               MSI_FDI         : Result := Result + ' Read Reply, Address Space = Function Definition Info';
                                               MSI_FSI         : Result := Result + ' Read Reply, Address Space = Function State Info'
                                             else
                                               Result := Result + ' Read Reply, Address Space = [Unknown]'      ;
                                             end;
                                           end
                    end; // Case
                    Result := Result + ', Starting Address = ' + IntToHex( LocalHelper.ExtractDataBytesAsInt(2, 5), 8);
                    if LocalHelper.Data[1] and $F8 = MCP_READ_OK then
                      Result := Result + ', Success'
                    else
                    if LocalHelper.Data[1] and $F8 = MCP_READ_ERROR then
                      Result := Result + ', Error'
                  end;
              MCP_READ_STREAM_REPLY  :
                  begin

                  end;
            end;
          end;
        end;

  end;

begin
  case MTI of
    MTI_CID0 : Result := 'Check ID 0';
    MTI_CID1 : Result := 'Check ID 1';
    MTI_CID2 : Result := 'Check ID 2';
    MTI_CID3 : Result := 'Check ID 3';
    MTI_CID4 : Result := 'Check ID 4';
    MTI_CID5 : Result := 'Check ID 5';
    MTI_CID6 : Result := 'Check ID 6';

    MTI_RID : Result := 'Reserve ID [RID]';
    MTI_AMD : Result := 'Alias Map Definition [AMD]';
    MTI_AME : Result := 'Alias Map Enquiry [AME]';
    MTI_AMR : Result := 'Alias Map Reset [AMR]';

    MTI_FRAME_TYPE_DATAGRAM_ONLY_FRAME : begin
                                           Result := 'Datagram Single Frame:';
                                           ConfigurationDatagram;
                                         end;
    MTI_FRAME_TYPE_DATAGRAM_FRAME_START : begin
                                           Result := 'Datagram Start Frame:';
                                           ConfigurationDatagram;
                                         end;
    MTI_FRAME_TYPE_DATAGRAM_FRAME : Result := 'Datagram Frame';
    MTI_FRAME_TYPE_DATAGRAM_FRAME_END : Result := 'Datagram End Frame';

    MTI_INITIALIZATION_COMPLETE : Result := 'Initialization Complete';
    MTI_VERIFY_NODE_ID_NUMBER_DEST : Result := 'Verify Node ID with Destination Address';
    MTI_VERIFY_NODE_ID_NUMBER      : Result := 'Verify Node ID Global';
    MTI_VERIFIED_NODE_ID_NUMBER    : Result := 'Verified Node ID';
    MTI_OPTIONAL_INTERACTION_REJECTED : Result := 'Optional Interaction Rejected';
    MTI_TERMINATE_DUE_TO_ERROR        : Result := 'Terminate Due to Error';

    MTI_PROTOCOL_SUPPORT_INQUIRY  : Result := 'Protocol Support Inquiry';
    MTI_PROTOCOL_SUPPORT_REPLY    : Result := 'Protocol Support Reply';

    MTI_TRACTION_PROXY_PROTOCOL   : Result := 'Protocol Traction Proxy';
    MTI_TRACTION_PROXY_REPLY      : Result := 'Protocol Traction Proxy Reply';

    MTI_CONSUMER_IDENTIFY              : Result := 'Consumer Identify';
    MTI_CONSUMER_IDENTIFY_RANGE        : Result := 'Consumer Identify Range';
    MTI_CONSUMER_IDENTIFIED_UNKNOWN    : Result := 'Consumer Identified Unknown';
    MTI_CONSUMER_IDENTIFIED_SET        : Result := 'Consumer Identified Valid';
    MTI_CONSUMER_IDENTIFIED_CLEAR      : Result := 'Consumer Identified Clear';
    MTI_CONSUMER_IDENTIFIED_RESERVED   : Result := 'Consumer Identified Reserved';
    MTI_PRODUCER_IDENDIFY              : Result := 'Producer Identify';
    MTI_PRODUCER_IDENTIFY_RANGE        : Result := 'Producer Identify Range';
    MTI_PRODUCER_IDENTIFIED_UNKNOWN    : Result := 'Producer Identified Unknown';
    MTI_PRODUCER_IDENTIFIED_SET        : Result := 'Producer Identified Valid';
    MTI_PRODUCER_IDENTIFIED_CLEAR      : Result := 'Producer Identified Clear';
    MTI_PRODUCER_IDENTIFIED_RESERVED   : Result := 'Producer Identified Reserved';
    MTI_EVENTS_IDENTIFY_DEST           : Result := 'Events Identify with Destination Address';
    MTI_EVENTS_IDENTIFY                : Result := 'Events Identify Global';
    MTI_EVENT_LEARN                    : Result := 'Event Learn';
    MTI_PC_EVENT_REPORT                : Result := 'Producer/Consumer Event Report [PCER] ';

    MTI_SIMPLE_NODE_INFO_REQUEST       : Result := 'Simple Node Info Request [SNIP]';
    MTI_SIMPLE_NODE_INFO_REPLY         : Result := 'Simple Node Info Reply [SNIP]';

    MTI_SIMPLE_TRAIN_INFO_REQUEST       : Result := 'Simple Train Node Info Request [STNIP]';
    MTI_SIMPLE_TRAIN_INFO_REPLY         : Result := 'Simple Train Node Info Reply [STNIP]';

    MTI_DATAGRAM_OK_REPLY              : begin
                                           Result := 'Datagram Reply OK';
                                           if LocalHelper.DataCount > 2 then
                                           begin
                                             if LocalHelper.Data[2] and DATAGRAM_OK_ACK_REPLY_PENDING = DATAGRAM_OK_ACK_REPLY_PENDING then
                                             begin
                                               if LocalHelper.Data[2] and $7F = 0 then
                                                 Result := Result + ' - Reply Is Pending - Maximum wait time = Infinity'
                                               else
                                                 Result := Result + ' - Reply Is Pending - Maximum wait time = ' + IntToStr( Round( Power(2, LocalHelper.Data[2] and $7F))) + ' seconds'
                                             end else
                                               Result := Result + ' - Reply Is Not Pending'
                                           end else
                                             Result := Result + ' - Does not include Extended Flags';
                                         end;
    MTI_DATAGRAM_REJECTED_REPLY        : Result := 'Datagram Rejected Reply';

    MTI_TRACTION_PROTOCOL              : Result := 'Traction Protocol';
    MTI_TRACTION_REPLY                 : Result := 'Traction Reply';
    MTI_STREAM_INIT_REQUEST            : Result := 'Stream Init Request';
    MTI_STREAM_INIT_REPLY              : Result := 'Stream Init Reply';
    MTI_FRAME_TYPE_CAN_STREAM_SEND     : Result := 'Stream Send - CAN Frame';
    MTI_STREAM_PROCEED                 : Result := 'Stream Proceed';
    MTI_STREAM_COMPLETE                : Result := 'Stream Complete';
   else
    Result := 'Unknown MTI';
  end;
  if LocalHelper.HasDestinationAddress and not IsDatagramMTI(LocalHelper.MTI, False) then
  begin
    if LocalHelper.FramingBits = $00 then
      Result := Result + ' Only Frame'
    else
    if LocalHelper.FramingBits = $10 then
      Result := Result + ' First Frame'
    else
    if LocalHelper.FramingBits = $20 then
      Result := Result + ' Last Frame'
    else
    if LocalHelper.FramingBits = $30 then
      Result := Result + ' Middle Frame'
    else
  end;
end;

function EventIDToString(EventID: PEventID): WideString;
var
  Address: Word;
begin
  if EqualEvents(EventID, @EVENT_IS_TRAIN) then
    Result := 'EVENT_TRAIN'
  else
  if EqualEvents(EventID, @EVENT_IS_PROXY) then
    Result := 'EVENT_PROXY'
  else
  if EqualEvents(EventID, @EVENT_DUPLICATE_ID_DETECTED) then
    Result := 'EVENT_DUPLICATE_NODE_ID'
  else
  if EqualEvents(EventID, @EVENT_EMERGENCY_STOP) then
    Result := 'EVENT_EMERGENCY_STOP'
  else
  if EqualEvents(EventID, @EVENT_NEW_LOG_ENTRY) then
    Result := 'EVENT_NEW_LOG_ENTRY'
  else
  if EqualEvents(EventID, @EVENT_IDENT_BUTTON_PRESSED) then
    Result := 'EVENT_IDENT_BUTTON_PRESSED'
  else
  if (EventID^[0] = $06) and (EventID^[1] = $01) then
  begin
    Address := ((EventID^[4] shl 8) or EventID^[5]) and $3FFF;  // Strip off the Extended bits if there are there
    if EventID^[4] and $C0 = $C0 then
      Result := 'EVENT_TRAIN_QUERY_DCC_ADDRESS : Extended Address = ' + IntToStr(Address) + ', (0x' + IntToHex(Address, 4) + ')'
    else
      Result := 'EVENT_TRAIN_QUERY_DCC_ADDRESS : Short Address = ' + IntToStr(Address) + ', (0x' + IntToHex(Address, 4) + ')'
  end
  else
    Result := 'Unique Event'
end;

function IsPrintableChar(C: Char): Boolean;
begin
  Result := ((Ord( C) >= 32) and (Ord( C) <= 126))  or ((Ord( C) >= 128) and (Ord( C) <= 255))
end;

function TractionProxyTechnologyToStr(Technology: Byte): WideString;
begin
  case Technology of
    TRACTION_PROXY_TECH_ID_DCC              : Result := 'DCC';
    TRACTION_PROXY_TECH_ID_DC               : Result := 'DC';
    TRACTION_PROXY_TECH_ID_MARKLIN_DIG      : Result := 'Marklin Digital';
    TRACTION_PROXY_TECH_ID_MARKLIN_DELTA    : Result := 'Marklin Delta';
    TRACTION_PROXY_TECH_ID_MARKLIN_DIG_ESU  : Result := 'Marklin Digital (ESU)';
    TRACTION_PROXY_TECH_ID_SELECTRIX        : Result := 'Selectrix';
    TRACTION_PROXY_TECH_ID_MTH_DCS          : Result := 'MTH DCS';
    TRACTION_PROXY_TECH_ID_LIONEL_TMCC      : Result := 'Lionel TMCC';
  end;
end;

function GridConnectToJMRI(GridStr: AnsiString): AnsiString;
var
  NPos: integer;
  Header: PChar;
  i: Integer;
begin
  Result := GridStr;
  NPos := Pos('N', GridStr);
  GridStr[NPos] := #0;
  Header := @GridStr[3];
  Result := '[' + Header + ']';
  Header := @GridStr[NPos] + 1;
  if Header^ <> ';' then
    Result := Result + ' ';
  while Header^ <> ';' do
  begin
    Result := Result + Header^;
    Inc(Header);
    if Header^ = ';' then
      Break;
    Result := Result + Header^ + ' ';
    Inc(Header);
  end;
  Result := Trim(Result);
  for i := 0 to (40 - Length(Result)) do
    Result := Result + ' ';  // Pad them all to the same length
end;

function RawHelperDataToStr(HelperData: TLccMessageHelper; ASCII: Boolean): string;
var
  j, iStart: Integer;
begin
  Result := '';
//  if HelperData.HasDestinationAddress then
 //   iStart := 2
//  else

  iStart := 0;
  Result := Result + ' [';
  for j := iStart to HelperData.DataCount - 1 do                     // Skip the Address
  begin
    if ASCII then
    begin
      if IsPrintableChar( Chr( HelperData.Data[j])) then
        Result := Result + Chr( HelperData.Data[j])
      else
        Result := Result + '.';
    end else
    begin
      Result := Result + IntToHex(HelperData.Data[j], 2);
      if j < HelperData.DataCount then
        Result := Result + '.'
    end;
  end;
  Result := Result + ']';
end;

function MessageToDetailedMessage(MessageString: string; Sending: Boolean): string;
var
  j, S_Len: Integer;
  f: single;
  Half: Word;
  MultiFrame: TMultiFrameBuffer;
begin
  if LocalHelper.Decompose(MessageString) then
  begin
    Result := MessageString;
    S_Len := Length(Result);
    for j := 0 to (28-S_Len) do
      Result := Result + ' ' ;

    if Sending then
      Result := Result + '  Send:   '
    else
      Result := Result + '  Receive: ';

    Result := Result + 'From = 0x' + IntToHex( LocalHelper.SourceAliasID, 4);

    if IsDatagramMTI(LocalHelper.MTI, False) then
      Result := Result + RawHelperDataToStr(LocalHelper, True) + ' MTI: ' + MTI_ToString(LocalHelper.MTI)
    else
      Result := Result + '   MTI: ' + MTI_ToString(LocalHelper.MTI) + ' - ';

    if IsStreamMTI( LocalHelper.MTI, True) then
    begin
      case LocalHelper.MTI of
        MTI_STREAM_INIT_REQUEST            : Result := Result + ' Suggested Bufer Size: ' + IntToStr((Localhelper.Data[2] shl 8) or LocalHelper.Data[3]) + ' Flags: 0x' + IntToHex(LocalHelper.Data[4], 2) + ' Additional Flags: 0x' + IntToHex(LocalHelper.Data[5], 2) + ' Source Stream ID: ' + IntToStr(LocalHelper.Data[6]);
        MTI_STREAM_INIT_REPLY              : Result := Result + ' Negotiated Bufer Size: ' + IntToStr((Localhelper.Data[2] shl 8) or LocalHelper.Data[3]) + ' Flags: 0x' + IntToHex(LocalHelper.Data[4], 2) + ' Additional Flags: 0x' + IntToHex(LocalHelper.Data[5], 2) + ' Source Stream ID: ' + IntToStr(LocalHelper.Data[6]) + ' Destination Stream ID: ' + IntToStr(LocalHelper.Data[7]);
        MTI_FRAME_TYPE_CAN_STREAM_SEND     : begin end;
        MTI_STREAM_PROCEED                 : Result := Result + ' Source Stream ID: ' + IntToStr(Localhelper.Data[2]) + ' Destination Stream ID: ' + IntToStr(LocalHelper.Data[3]) + ' Flags: 0x' + IntToHex(LocalHelper.Data[4], 2) + ' Additional Flags: 0x' + IntToHex(LocalHelper.Data[5], 2);
        MTI_STREAM_COMPLETE                : Result := Result + ' Source Stream ID: ' + IntToStr(Localhelper.Data[2]) + ' Destination Stream ID: ' + IntToStr(LocalHelper.Data[3]) + ' Flags: 0x' + IntToHex(LocalHelper.Data[4], 2) + ' Additional Flags: 0x' + IntToHex(LocalHelper.Data[5], 2);
      end
    end;

    if LocalHelper.MTI = MTI_OPTIONAL_INTERACTION_REJECTED then
    begin
    end;

    // SNII/SNIP
    if LocalHelper.MTI = MTI_SIMPLE_NODE_INFO_REPLY then
      Result := Result + RawHelperDataToStr(LocalHelper, True);

    // STNIP
    if LocalHelper.MTI = MTI_SIMPLE_TRAIN_INFO_REPLY then
      Result := Result + RawHelperDataToStr(LocalHelper, True);

    // Events
    if (LocalHelper.MTI = MTI_PRODUCER_IDENDIFY) or (LocalHelper.MTI = MTI_PRODUCER_IDENTIFIED_SET) or (LocalHelper.MTI = MTI_PRODUCER_IDENTIFIED_CLEAR) or
      (LocalHelper.MTI = MTI_PRODUCER_IDENTIFIED_UNKNOWN) or (LocalHelper.MTI = MTI_CONSUMER_IDENTIFY) or (LocalHelper.MTI = MTI_CONSUMER_IDENTIFIED_SET) or
      (LocalHelper.MTI = MTI_CONSUMER_IDENTIFIED_CLEAR) or (LocalHelper.MTI = MTI_CONSUMER_IDENTIFIED_UNKNOWN) or (LocalHelper.MTI = MTI_PC_EVENT_REPORT)
    then begin
        Result := Result + 'EventID: ' + EventIDToString(@LocalHelper.Data);
    end;

    // Traction Protocol
    if LocalHelper.MTI = MTI_TRACTION_PROTOCOL then
    begin
      MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
      if Assigned(MultiFrame) then
      begin
        case MultiFrame.DataArray[0] of
            TRACTION_SPEED_DIR :
              begin
                Result := Result + ' LCC Speed/Dir Operation; Speed = ';
                f := HalfToFloat( (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2]);
                if f = 0 then
                begin
                  if DWord( f) and $80000000 = $80000000 then
                    Result := Result + '-0.0'
                  else
                    Result := Result + '+0.0'
                end else
                  Result := Result + IntToStr( round(f));
              end;
            TRACTION_FUNCTION : Result := Result + ' LCC Traction Operation - Function Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 3), 6) + '], Value: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(4, 5), 2) + ']';
            TRACTION_E_STOP : Result := Result + ' LCC Traction Emergency Stop';
            TRACTION_QUERY_SPEED : Result := Result + ' Query Speeds';
            TRACTION_QUERY_FUNCTION : Result := Result + ' Query Function - Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 3), 6) + ']';
            TRACTION_CONTROLLER_CONFIG :
              begin;
                case MultiFrame.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN :
                    begin
                      if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                        Result := Result + ' Controller Config Assign - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                      else
                        Result := Result + ' Controller Config Assign - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                    end;
                  TRACTION_CONTROLLER_CONFIG_RELEASE :
                    begin
                      if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                        Result := Result + ' Controller Config Release - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                      else
                        Result := Result + ' Controller Config Release - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                    end;
                  TRACTION_CONTROLLER_CONFIG_QUERY :
                    begin
                      Result := Result + ' Controller Config Query';
                    end;
                  TRACTION_CONTROLLER_CONFIG_NOTIFY :
                    begin
                      if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                        Result := Result + ' Controller Config Notify - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                      else
                        Result := Result + ' Controller Config Notify - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                    end
                end
              end;
            TRACTION_CONSIST :
              begin
                case MultiFrame.DataArray[1] of
                  TRACTION_CONSIST_ATTACH : Result := Result + 'Consist Config Attach';
                  TRACTION_CONSIST_DETACH : Result := Result + 'Consist Config Detach';
                  TRACTION_CONSIST_QUERY : Result := Result + 'Consit Config Query';
                end
              end;
            TRACTION_MANAGE :
              begin
                case MultiFrame.DataArray[1] of
                    TRACTION_MANAGE_RESERVE : Result := Result + 'Traction Management Reserve';
                    TRACTION_MANAGE_RELEASE : Result := Result + 'Traction Management Release'
                end
              end
        else
          Result := Result + 'Unknown Traction Operation';
        end;

        FreeAndNil(MultiFrame);
      end;
    end;

    // Traction Protocol Reply
    if LocalHelper.MTI = MTI_TRACTION_REPLY then
    begin
      MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
      if Assigned(MultiFrame) then
      begin
        case MultiFrame.DataArray[0] of
            TRACTION_QUERY_SPEED :
              begin
                Result := Result + 'Query Speed Reply : Set Speed = ';
                  Half := (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2];
                  if Half = $FFFF then
                  begin
                    Result := Result + 'NaN'
                  end else
                  begin
                    f := HalfToFloat( Half);
                    if f = 0 then
                    begin
                      if DWord( f) and $80000000 = $80000000 then
                        Result := Result + '-0.0'
                      else
                        Result := Result + '+0.0'
                    end else
                      Result := Result + IntToStr( round(f));
                  end;

                  Result := Result + ': Status = ' + MultiFrame.ExtractDataBytesAsHex(3, 3);

                  Result := Result + ': Commanded Speed = ';
                  Half := (MultiFrame.DataArray[4] shl 8) or MultiFrame.DataArray[5];
                  if Half = $FFFF then
                  begin
                    Result := Result + 'NaN'
                  end else
                  begin
                    f := HalfToFloat( Half);
                    if f = 0 then
                    begin
                      if DWord( f) and $80000000 = $80000000 then
                        Result := Result + '-0.0'
                      else
                        Result := Result + '+0.0'
                    end else
                      Result := Result + IntToStr( round(f));
                  end;

                  Result := Result + ': Actual Speed = ';
                  Half := (MultiFrame.DataArray[6] shl 8) or MultiFrame.DataArray[7];
                  if Half = $FFFF then
                  begin
                    Result := Result + 'NaN'
                  end else
                  begin
                    f := HalfToFloat( Half);
                    if f = 0 then
                    begin
                      if DWord( f) and $80000000 = $80000000 then
                        Result := Result + '-0.0'
                      else
                        Result := Result + '+0.0'
                    end else
                      Result := Result + IntToStr( round(f));
                  end
              end;
            TRACTION_QUERY_FUNCTION : Result := Result + 'Query Function Reply - Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ', Value: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5));
            TRACTION_CONTROLLER_CONFIG :
              begin;
                case MultiFrame.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN :
                    begin
                      Result := Result + 'Controller Config Assign Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2)
                    end;
                  TRACTION_CONTROLLER_CONFIG_QUERY :
                    begin
                      if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                        Result := Result + 'Controller Config Query Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Result = ' + MultiFrame.ExtractDataBytesAsHex(3, 3) + ' Active Controller = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(4, 9), 12) + ' Alias = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(10, 11), 4)
                      else
                        Result := Result + 'Controller Config Query Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Result = ' + MultiFrame.ExtractDataBytesAsHex(3, 3) + ' Active Controller = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(4, 9), 12);
                    end;
                  TRACTION_CONTROLLER_CONFIG_NOTIFY :
                    begin
                      Result := Result + 'Controller Config Notify Reply - Result = ' + MultiFrame.ExtractDataBytesAsHex(2, 2)
                    end;
                end
              end;
            TRACTION_CONSIST :
              begin
                case MultiFrame.DataArray[1] of
                  TRACTION_CONSIST_ATTACH : Result := Result + 'Consist Config Attach Reply';
                  TRACTION_CONSIST_DETACH : Result := Result + 'Consist Config Detach Reply';
                  TRACTION_CONSIST_QUERY : Result := Result + 'Consit Config Query Reply';
                end
              end;
            TRACTION_MANAGE :
              begin
                case MultiFrame.DataArray[1] of
                    TRACTION_MANAGE_RESERVE : Result := Result +  'Manage: Reserve' + 'Result = ' + MultiFrame.ExtractDataBytesAsHex(2, 2);
                end
              end
        else
          Result := Result + 'Unknown Traction Reply Operation';
        end;

        FreeAndNil(MultiFrame);
      end;
    end;

    if LocalHelper.MTI = MTI_TRACTION_PROXY_PROTOCOL then
    begin
      MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
      if Assigned(MultiFrame) then
      begin
        case MultiFrame.DataArray[0] of
            TRACTION_PROXY_ALLOCATE : Result := Result + 'Allocate: Suggested Legacy Technology: ' + IntToStr(MultiFrame.DataArray[1]) + ' Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(2, 3) and not $C000) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(2, 3), 4) + '] Speed Steps = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 4));
            TRACTION_PROXY_ATTACH   : Result := Result + 'Attach: Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 2) and not $C000) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 2), 4) + ']' ;
            TRACTION_PROXY_DETACH   : Result := Result + 'Detach: Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 2) and not $C000) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 2), 4) + ']' ;
            TRACTION_PROXY_MANAGE   :
              begin
                case MultiFrame.DataArray[1] of
                    TRACTION_PROXY_MANAGE_RESERVE : Result := Result + 'Manage: Reserve';
                    TRACTION_PROXY_MANAGE_RELEASE : Result := Result + 'Manage: Release'
                else
                  Result := Result + 'Unknown Traction Manage Command'
                end;
              end
        else
          Result := Result + 'Unknown Traction Proxy Command';
        end;

        FreeAndNil(MultiFrame);
      end;
    end;

    if LocalHelper.MTI = MTI_TRACTION_PROXY_REPLY then
    begin
      MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
      if Assigned(MultiFrame) then
      begin
         case MultiFrame.DataArray[0] of
            TRACTION_PROXY_ALLOCATE :
              begin
                if MultiFrame.DataArray[1] and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                  Result := Result + 'Flags = ' + IntToStr(MultiFrame.DataArray[1]) + ', Allocate: Technology = ' + TractionProxyTechnologyToStr(MultiFrame.DataArray[2]) + ', Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(3, 4)  and not $C000) +  ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(3, 4), 4) + '], Train NodeID ' + MultiFrame.ExtractDataBytesAsHex(5, 10) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(11, 12) + ']'
                else
                  Result := Result + 'Flags = ' + IntToStr(MultiFrame.DataArray[1]) + ', Allocate: Technology = ' + TractionProxyTechnologyToStr(MultiFrame.DataArray[2]) + ', Train ID = ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(3, 4)  and not $C000) +  ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(3, 4), 4) + '], Train NodeID ' + MultiFrame.ExtractDataBytesAsHex(5, 10)
              end;
            TRACTION_PROXY_ATTACH   : Result := Result + 'Attach: ReplyCode = ' + IntToHex(MultiFrame.DataArray[1], 2);
            TRACTION_PROXY_MANAGE   :
              begin
                case MultiFrame.DataArray[1] of
                    TRACTION_PROXY_MANAGE_RESERVE : Result := Result + 'Manage: Reserve' + 'Result = ' + MultiFrame.ExtractDataBytesAsHex(2, 2);
                else
                  Result := Result + 'Unknown Traction Manage Command'
                end;
              end
        else
          Result := Result + 'Unknown Traction Proxy Command';
        end;

        FreeAndNil(MultiFrame);
      end;
    end;
  end;
end;

procedure PrintToSynEdit(MessageStr: AnsiString; SynEditLog: TSynEdit; Paused: Boolean; Detailed: Boolean; JMRIFormat: Boolean);
var
  SemiColonPos: Integer;
  Header: PChar;
  i: Integer;
begin
  if not Paused then
  begin
     SynEditLog.BeginUpdate();
     try
       LogStrings.Text := MessageStr;
       for i := 0 to LogStrings.Count - 1 do
       begin
         if JMRIFormat then
         begin
           if Detailed then
           begin
              LogStrings[i] := MessageToDetailedMessage(LogStrings[i], True);
              SemiColonPos := Pos(';',  LogStrings[i]);
              Header := @LogStrings[i][SemiColonPos] + 1;
              SynEditLog.Lines.Add( GridConnectToJMRI(LogStrings[i]) + Header);
           end else
             SynEditLog.Lines.Add( GridConnectToJMRI(LogStrings[i]));
         end else
         begin
           if Detailed then
             SynEditLog.Lines.Add(MessageToDetailedMessage(LogStrings[i], True))
           else
             SynEditLog.Lines.Add(LogStrings[i]);
         end;
       end;
     finally
       SynEditLog.CaretY := SynEditLog.LineHeight * SynEditLog.Lines.Count;
       SynEditLog.EndUpdate;
     end;
  end;
end;

procedure PrintTCPToSynEdit(MessageStr: WideString; ByteArray: TDynamicByteArray; SynEditLog: TSynEdit; Paused: Boolean; Detailed: Boolean; JMRIFormat: Boolean);
const
  LF = #13#10;
var
  i: Integer;
 // MTI: DWord;
begin
  // ARGGGG... this unit assumes CAN MTIs........  Need to update
 // MTI := (ByteArray[17] shl 8) or ByteArray[18];
//  MessageStr := MessageStr +  'MTI: ' + MTI_ToString(MTI) + LF + 'Header: ';
  MessageStr := MessageStr + LF + 'TCP Header: ';
  for i := 0 to MAX_HEADER_ONLY_LEN - 1 do
    MessageStr := MessageStr + ' ' + IntToHex(ByteArray[i], 2);

  MessageStr := MessageStr + LF + 'TCP Message: ';
  for i := MAX_HEADER_ONLY_LEN to Length(ByteArray) - 1 do
    MessageStr := MessageStr + ' ' + IntToHex(ByteArray[i], 2);

  PrintToSynEdit(MessageStr, SynEditLog,  Paused, Detailed, JMRIFormat);
end;

{ TMultiFrameBuffer }

constructor TMultiFrameBuffer.Create;
begin
   FAliasID := 0;
   FDataArraySize := 0;
   FCurrentIndex := 0;
end;

function TMultiFrameBuffer.ExtractDataBytesAsHex(StartByteIndex, EndByteIndex: Integer): string;
begin
  Result := IntToHex(ExtractDataBytesAsInt(StartByteIndex, EndByteIndex), EndByteIndex-StartByteIndex);
end;

function TMultiFrameBuffer.ExtractDataBytesAsInt(StartByteIndex,EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( DataArray[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

procedure TMultiFrameBufferList.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
    List.Count := 0;
  finally
    FreeAndNil(FList);
  end;
end;

constructor TMultiFrameBufferList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMultiFrameBufferList.Destroy;
begin
  Clear;
  inherited Destroy
end;

function TMultiFrameBufferList.FindByAlias(TestAliasID: Word): TMultiFrameBuffer;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to List.Count - 1 do
  begin
    if MultiFrameBuffers[i].AliasID = TestAliasID then
    begin
      Result := MultiFrameBuffers[i];
      Exit;
    end;
  end;
end;

function TMultiFrameBufferList.GetMultiFrameBuffer(Index: Integer): TMultiFrameBuffer;
begin
  Result := TMultiFrameBuffer( List[Index])
end;

function TMultiFrameBufferList.ProcessFrame(NewFrame: TLccMessageHelper): TMultiFrameBuffer;
var
  Buffer: TMultiFrameBuffer;
  i: Integer;
begin
  Result := nil;

  Buffer := FindByAlias(NewFrame.DestinationAliasID);
  if not Assigned(Buffer) then
  begin
    Buffer := TMultiFrameBuffer.Create;
    Buffer.AliasID := NewFrame.DestinationAliasID;
    List.Add(Buffer);
  end;

  for i := 2 to NewFrame.DataCount - 1 do          // Skip the Alias
  begin
    Buffer.DataArray[Buffer.CurrentIndex] := NewFrame.Data[i];
    Inc(Buffer.FDataArraySize);
    Inc(Buffer.FCurrentIndex);
  end;

  if (NewFrame.FramingBits = $20) or (NewFrame.FramingBits = $00) then
  begin
    List.Remove(Buffer);
    Result := Buffer
  end;
end;

procedure TMultiFrameBufferList.SetMultiFrameBuffer(Index: Integer; AValue: TMultiFrameBuffer);
begin
  List[Index] := AValue
end;

{ TOpenLCBMessageHelper }

procedure TLccMessageHelper.SetData(AValue: TCANByteArray);
begin
  FData:=AValue;
end;

procedure TLccMessageHelper.SetLayer(AValue: TOpenLCBLayer);
begin
  if FLayer=AValue then Exit;
  FLayer:=AValue;
end;

constructor TLccMessageHelper.Create;
var
  i: Integer;
begin
  inherited Create;
  FLayer := ol_CAN;
  FMTI := 0;
  for i := 0 to CAN_BYTE_COUNT - 1 do
    FData[i] := 0;
  FDataCount := 0;
  FSourceAliasID := 0;
  FDestinationAliasID := 0;
  FForwardingBitNotSet := False;
  FUnimplementedBitsSet := False;
  FFramingBits := 0;
end;

destructor TLccMessageHelper.Destroy;
begin
  inherited Destroy
end;

procedure TLccMessageHelper.CopyTo(Target: TLccMessageHelper);
begin
  Target.FDestinationAliasID := DestinationAliasID;
  Target.FHasDestinationAddress := HasDestinationAddress;
  Target.FForwardingBitNotSet := ForwardingBitNotSet;
  Target.FSourceAliasID := SourceAliasID;
  Target.FData := Data;
  Target.FDataCount := DataCount;
  Target.FLayer := Layer;
  Target.FMTI := MTI;
  Target.FUnimplementedBitsSet := UnimplementedBitsSet;
end;

function TLccMessageHelper.Decompose(MessageStr: AnsiString): Boolean;
var
  x, n, SemiColon, i: Integer;
  ByteStr: AnsiString;
begin
  Result := False;
  if MessageStr <> '' then
  begin
    MessageStr := UpperCase(MessageStr);

    x := Pos('X', MessageStr);         // Find were the "X" is in the string
    if x > 0 then
    begin
      n := PosEx('N', MessageStr, x);  // Find where the "N" is in the string
      if n > 0 then
      begin
        Result := True;           // At least it has an X and a N
        MessageStr[n] := #0;           // Set the "N" to a null to create a null string of the MTI
        Inc(n);                        // Move just pass where the "N" was
        SemiColon := PosEx(';', MessageStr, n);  // Look for the terminating ";"
        if SemiColon > 0 then
        begin
          MTI := StrToInt('$' + PAnsiChar( @MessageStr[x+1])); // Convert the string MTI into a number
          SourceAliasID := MTI and $00000FFF;                  // Strip off the Source Alias
          if MTI and $08000000 = $08000000 then                // Was this an OpenLCB or CAN message?
            Layer := ol_OpenLCB
          else
            Layer := ol_CAN;

          FForwardingBitNotSet := MTI and $10000000 = $00000000;    // Check if the Forwarding Bit was set
          FUnimplementedBitsSet := MTI and $E0000000 <> $00000000;  // Check to see the state of the unimplemented bits

          MTI := MTI and not $10000000;    // Strip off the reserved bits
          MTI := MTI and $FFFFF000;        // Strip off the Source Alias

          if Layer = ol_CAN then
          begin
            if MTI and MTI_CID_MASK <> 0 then
              MTI := MTI and MTI_CID_MASK;
          end;

          for i := 0 to CAN_BYTE_COUNT - 1 do
            Data[i] := 0;

          // Convert the CAN payload bytes into numbers
          FDataCount := 0;
          i := n;
          while i < SemiColon do
          begin
            ByteStr := MessageStr[i] + MessageStr[i+1];
            Data[FDataCount] := StrToInt('$'+ByteStr);
            Inc(i, 2);
            Inc(FDataCount);
          end;

          // Determine if the message has a destination address and if so store it
          HasDestinationAddress := False;
          FramingBits := 0;
          if Layer = ol_OpenLCB then
          begin
            if MTI and MTI_FRAME_TYPE_MASK > MTI_FRAME_TYPE_GENERAL then        // See if the destination Alias is in the MTI
            begin
              DestinationAliasID := (MTI and $00FFF000) shr 12;
              MTI := MTI and $FF000FFF;
              HasDestinationAddress := True;
            end else
            begin
              if MTI and MTI_ADDRESS_PRESENT = MTI_ADDRESS_PRESENT then
              begin
                DestinationAliasID := Word( (Data[0] shl 8) and $0FFF) or (Data[1]);
                FramingBits := Data[0] and $30;
                HasDestinationAddress := True;
              end
            end
          end
        end
      end
    end
  end;
end;

function TLccMessageHelper.Encode: AnsiString;
var
  i: Integer;
  FullMTI: DWord;
begin
  FullMTI := MTI or SourceAliasID;
  FullMTI := FullMTI or $10000000;
  if Layer = ol_OpenLCB then
    FullMTI := FullMTI or $08000000;

  if MTI and MTI_FRAME_TYPE_MASK > MTI_FRAME_TYPE_GENERAL then
  begin
    // Datagram or Stream
    FullMTI := FullMTI or (DWord( DestinationAliasID) shl 12);
    Result := ':X' + IntToHex(FullMTI, 8) + 'N';
    for i := 0 to DataCount - 1 do
      Result := Result + IntToHex(Data[i], 2);
  end else
  begin
    Result := ':X' + IntToHex(FullMTI, 8) + 'N';
    for i := 0 to DataCount - 1 do
    begin
      if (i < 2) and (DestinationAliasID <> 0) then
      begin
        if i = 0 then
          Result := Result + IntToHex(((DestinationAliasID shr 8) or FramingBits) and $00FF, 2)
        else
          Result := Result + IntToHex(DestinationAliasID and $00FF, 2)
      end else
        Result := Result + IntToHex(Data[i], 2);
    end;
  end;
  Result := Result  + ';'
end;

function TLccMessageHelper.ExtractDataBytesAsEventID: PEventID;
begin
  Result := @Data;
end;

procedure TLccMessageHelper.Load(ALayer: TOpenLCBLayer; AMTI: DWord;
  ASourceAlias: Word; ADestinationAlias: Word; ADataCount: Integer; AData0,
  AData1, AData2, AData3, AData4, AData5, AData6, AData7: Byte);
begin
  Layer := ALayer;
  MTI := AMTI;
  DataCount := ADataCount;
  SourceAliasID := ASourceAlias;
  DestinationAliasID := ADestinationAlias;
  Data[0] := AData0;
  Data[1] := AData1;
  Data[2] := AData2;
  Data[3] := AData3;
  Data[4] := AData4;
  Data[5] := AData5;
  Data[6] := AData6;
  Data[7] := AData7;
end;

procedure TLccMessageHelper.StoreNodeIDToData(NodeID: Int64; IsAddressed: Boolean);
var
  Offset: Integer;
begin
  if IsAddressed then
    Offset := 2
  else
    Offset := 0;
  Data[0+Offset] := (NodeID shr 40) and $000000FF;
  Data[1+Offset] := (NodeID shr 32) and $000000FF;
  Data[2+Offset] := (NodeID shr 24) and $000000FF;
  Data[3+Offset] := (NodeID shr 16) and $000000FF;
  Data[4+Offset] := (NodeID shr 8) and $000000FF;
  Data[5+Offset] := (NodeID) and $000000FF;
  DataCount := 6 + Offset;
end;

function TLccMessageHelper.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( Data[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

function TLccMessageHelper.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := StartIndex to Count - 1 do
    Result := Result + Chr( Data[i]);
end;

procedure TLccMessageHelper.IntToByteArray(Int: QWord; var ByteArray: TCANByteArray);
begin
  ByteArray[0] := Int and $000000FF;
  ByteArray[1] := (Int shr 8) and $000000FF;
  ByteArray[2] := (Int shr 16) and $000000FF;
  ByteArray[3] := (Int shr 24) and $000000FF;
  ByteArray[4] := (Int shr 32) and $000000FF;
  ByteArray[5] := (Int shr 40) and $000000FF;
  ByteArray[6] := (Int shr 48) and $000000FF;
  ByteArray[7] := (Int shr 56) and $000000FF;
end;



initialization
  LocalHelper := TLccMessageHelper.Create;
  MultiFrames := TMultiFrameBufferList.Create;
  LogStrings := TStringList.Create;

finalization
  FreeAndNil(MultiFrames);
  FreeAndNil(LocalHelper);
  FreeAndNil(LogStrings);

end.

