unit lcc_defines;

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
  System.Device.Storage,
  SmartCL.Device.Storage,
  SmartCL.Application,
  SmartCL.Components,
  SmartCL.System;
{$ELSE}
  {$IFDEF FPC_CONSOLE_APP}
    fptimer,
  {$ELSE}
    {$IFDEF ULTIBO}
      fptimer,
    {$ELSE}
      {$IFDEF FPC}
        ExtCtrls,
      {$ELSE}
        FMX.Types,
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  Classes,
  SysUtils;
{$ENDIF}

{$IFDEF DWSCRIPT}
const
  S_OK = $00;
  S_FALSE = $01;
{$ENDIF}


const
  PATH_OSX_RESOURCES = 'Contents/Resources/';
  PATH_OSX_EXECUTABLE = 'Contents/MacOS/';
  PATH_UNIX_APPLICATION = '/usr/share/';    // Typical place to store the application foldler
  PATH_UNIX_SETTINGS = '/home/{user}/.config/{executable_name}';  // GetAppConfigDir  does this for us but this is what it returns
  PATH_LINUX_DEV = '/dev/';
  PATH_OSX_DEV = 'dev/';

const
  LCC_BYTE_COUNT            = 1024;       // This is longest data structure defined in Lcc
  MAX_DATAGRAM_LENGTH       = 72;
  MAX_EVENT_LEN             = 8;
  MAX_NODEID_LEN            = 6;
  MAX_MULTIFRAME_LEN        = 12;
  MAX_SUPPORTEDPROTOCOL_LEN = 6;

{$IFDEF DWSCRIPT}
type
  DWord = Longword;
  TComponent = TObject;
{$ELSE}

  {$IFNDEF FPC}    // Must be Delphi
  type
    DWord = Cardinal;
    QWord = UInt64;
    {$IFDEF LCC_MOBILE}
      AnsiChar = Char;
      AnsiString = string;
      PAnsiString = ^string;
      PAnsiChar = ^Char;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

type
  {$IFDEF DWSCRIPT}
    TLccTimer = TW3Timer;
  {$ELSE}
    {$IFDEF ULTIBO}
    TLccTimer = TFPTimer;
    {$ELSE}
      {$IFDEF FPC_CONSOLE_APP}
       TLccTimer = TFPTimer;
       {$ELSE}
       TLccTimer = TTimer;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

type
  TLccByteArray = array[0..LCC_BYTE_COUNT-1] of Byte;
  TLccSupportedProtocolArray = array[0..MAX_SUPPORTEDPROTOCOL_LEN] of Byte;

  TNodeID = array[0..1] of DWord;
  {$IFNDEF DWSCRIPT}
  PNodeID = ^TNodeID;
  {$ENDIF}

  TFunctionStatesArray = array[0..28] of Word;
  TLccDynamicByteArray = array of Byte;

type
  TDatagramArray = array[0..MAX_DATAGRAM_LENGTH-1] of Byte;

  TEventID = array[0..MAX_EVENT_LEN-1] of Byte;
  {$IFNDEF DWSCRIPT}
  PEventID = ^TEventID;
  {$ENDIF}

  THexArray = TEventID;

  TMultiFrameArray = array[0..MAX_MULTIFRAME_LEN-1] of Byte;

type
  TEventState = (evs_Unknown, evs_Valid, evs_InValid);
  TLccConfigDataType = (cdt_String, cdt_Int, cdt_EventID, cdt_Bit);

  TLccConnectionState = (lcsConnecting, lcsConnected, lcsDisconnecting, lcsDisconnected);

{$IFNDEF DWSCRIPT}
// Solves circular reference as the parser need to know about lcc_nodemanager and vice versa
type
  TLccCdiParserBase = class(TComponent)
  public
    procedure SetNodeManager(ANodeManager: TObject); virtual; abstract;
    procedure DoConfigMemReadReply(ANode: TObject); virtual; abstract;
    procedure DoConfigMemWriteReply(ANode: TObject); virtual; abstract;
    procedure NotifyLccNodeDestroy(LccNode: TObject); virtual; abstract;
  end;
{$ENDIF}

const
  MFG_INFO_VERSION_ID      = 1;
  USER_MFG_INFO_VERSION_ID = 1;

  LEN_MFG_VERSION      = 1;
  LEN_MFG_NAME         = 41;
  LEN_MODEL_NAME       = 41;
  LEN_HARDWARE_VERSION = 21;
  LEN_SOFTWARE_VERSION = 21;

  LEN_USER_VERSION     = 1;
  LEN_USER_NAME        = 63;
  LEN_USER_DESCRIPTION = 64;


  LEN_MANUFACTURER_INFO = LEN_MFG_VERSION + LEN_MFG_NAME + LEN_MODEL_NAME + LEN_HARDWARE_VERSION + LEN_SOFTWARE_VERSION;
  LEN_USER_MANUFACTURER_INFO = LEN_USER_VERSION + LEN_USER_NAME + LEN_USER_DESCRIPTION;

  ADDRESS_VERSION           = 0;     // Same for both fixed and user

  ADDRESS_MFG_NAME          = 1;
  ADDRESS_MODEL_NAME        = 42;
  ADDRESS_HARDWARE_VERSION  = 83;
  ADDRESS_SOFTWARE_VERSION  = 104;

  ADDRESS_USER_NAME        = 1;
  ADDRESS_USER_DESCRIPTION  = 64;

const
  // Full CAN MTI
  MTI_CAN_ADDRESS_PRESENT                = $00008000;                                // Address in the CAN Data present if set

  MTI_CAN_CAN                            = $00000000;                                // Frame Type CAN Control Message
  MTI_CAN_CID0                           = $07000000;                                // First 12 Bits of 48 bit Node ID
  MTI_CAN_CID1                           = $06000000;                                // 2rd 12 Bits of 48 bit Node ID
  MTI_CAN_CID2                           = $05000000;                                // 3nd 12 Bits of 48 bit Node ID
  MTI_CAN_CID3                           = $04000000;                                // Last 12 Bits of 48 bit Node ID
  MTI_CAN_CID4                           = $03000000;                                // non-OpenLCB Protocol
  MTI_CAN_CID5                           = $02000000;                                // non-OpenLCB Protocol
  MTI_CAN_CID6                           = $01000000;                                // non-OpenLCB Protocol

  MTI_CAN_RID                            = $00700000;                                // Reserve ID
  MTI_CAN_AMD                            = $00701000;                                // Alias Map Definition
  MTI_CAN_AME                            = $00702000;                                // Alias Mapping Enquiry
  MTI_CAN_AMR                            = $00703000;                                // Alias Map Reset Frame

  MTI_CAN_MASK                              = $0FFFF000;
  MTI_CAN_FRAME_TYPE_MASK                   = $0F000000;
  MTI_CAN_FRAME_TYPE_GENERAL                = $09000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY    = $0A000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START   = $0B000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME         = $0C000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END     = $0D000000;

  MTI_CAN_ADDRESSED_MASK                 = $00008000;
  MTI_CAN_SIMPLE_PROTOCOL_MASK           = $00010000;
  MTI_CAN_EVENT_PRESENT_MASK             = $00002000;

  MTI_CAN_INITIALIZATION_COMPLETE        = $09100000;                                // Databytes = Full Node ID
  MTI_CAN_VERIFY_NODE_ID_NUMBER_DEST     = $09488000;                                // Databytes = Destination Alias
  MTI_CAN_VERIFY_NODE_ID_NUMBER          = $09490000;                                //
  MTI_CAN_VERIFIED_NODE_ID_NUMBER        = $09170000;                                // {Optional Full Node ID}
  MTI_CAN_OPTIONAL_INTERACTION_REJECTED  = $09068000;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_CAN_TERMINATE_DUE_TO_ERROR         = $090A8000;                                // Databytes = Destination Alias, Error, {Optional Info}

  MTI_CAN_PROTOCOL_SUPPORT_INQUIRY       = $09828000;                                // Databytes = Destination Alias
  MTI_CAN_PROTOCOL_SUPPORT_REPLY         = $09668000;                                // Databytes = Destination Alias, Protocol Flags

  MTI_CAN_CONSUMER_IDENTIFY              = $098F4000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFY_RANGE        = $094A4000;                                // Databytes = EventID with Mask
  MTI_CAN_CONSUMER_IDENTIFIED_UNKNOWN    = $094C7000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_SET        = $094C4000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_CLEAR      = $094C5000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_RESERVED   = $094C6000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENDIFY              = $09914000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFY_RANGE        = $09524000;                                // Databytes = EventID with Mask
  MTI_CAN_PRODUCER_IDENTIFIED_UNKNOWN    = $09547000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_SET        = $09544000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_CLEAR      = $09545000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_RESERVED   = $09546000;                                // Databytes = EventID
  MTI_CAN_EVENTS_IDENTIFY_DEST           = $09968000;                                // Databytes = Destination Alias
  MTI_CAN_EVENTS_IDENTIFY                = $09970000;                                //
  MTI_CAN_EVENT_LEARN                    = $09594000;                                // Databytes = EventID
  MTI_CAN_PC_EVENT_REPORT                = $095B4000;                                // Databytes = EventID  (Infamouse PCER)

  MTI_CAN_SIMPLE_NODE_INFO_REQUEST       = $09DE8000;                                // Databytes = Destination Alias
  MTI_CAN_SIMPLE_NODE_INFO_REPLY         = $09A08000;                                // Databytes = Destination Alias, ACDI Data

  MTI_CAN_SIMPLE_TRAIN_INFO_REQUEST      = $09DA8000;                                // Databytes = Destination Alias
  MTI_CAN_SIMPLE_TRAIN_INFO_REPLY        = $099C8000;                                // Databytes = Destination Alias, ACDI Data

  MTI_CAN_TRACTION_PROTOCOL              = $095EA000;                                // Databyte = depends
  MTI_CAN_TRACTION_PROXY_PROTOCOL        = $091EA000;
  MTI_CAN_TRACTION_REPLY                 = $095E8000;                                // Databyte = depends
  MTI_CAN_TRACTION_PROXY_REPLY           = $091E8000;

  MTI_CAN_STREAM_INIT_REQUEST            = $09CC8000;
  MTI_CAN_STREAM_INIT_REPLY              = $09868000;
  MTI_CAN_FRAME_TYPE_CAN_STREAM_SEND     = $0F000000;
  MTI_CAN_STREAM_PROCEED                 = $09888000;
  MTI_CAN_STREAM_COMPLETE                = $098A8000;

  MTI_CAN_DATAGRAM_OK_REPLY              = $09A28000;                                // Databytes = Destination Alias
  MTI_CAN_DATAGRAM_REJECTED_REPLY        = $09A48000;                                // Databytes = Destination Alias, Error Code

  // Raw MTI
  MTI_ADDRESSED_MASK                 = $0008;
  MTI_SIMPLE_PROTOCOL_MASK           = $0010;
  MTI_EVENT_PRESENT_MASK             = $0002;

  MTI_INITIALIZATION_COMPLETE        = $0100;                                // Databytes = Full Node ID
  MTI_VERIFY_NODE_ID_NUMBER_DEST     = $0488;                                // Databytes = Destination Alias
  MTI_VERIFY_NODE_ID_NUMBER          = $0490;                                //
  MTI_VERIFIED_NODE_ID_NUMBER        = $0170;                                // {Optional Full Node ID}
  MTI_OPTIONAL_INTERACTION_REJECTED  = $0068;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_TERMINATE_DUE_TO_ERROR         = $00A8;                                // Databytes = Destination Alias, Error, {Optional Info}

  MTI_PROTOCOL_SUPPORT_INQUIRY       = $0828;                                // Databytes = Destination Alias
  MTI_PROTOCOL_SUPPORT_REPLY         = $0668;                                // Databytes = Destination Alias, Protocol Flags

  MTI_CONSUMER_IDENTIFY              = $08F4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFY_RANGE        = $04A4;                                // Databytes = EventID with Mask
  MTI_CONSUMER_IDENTIFIED_UNKNOWN    = $04C7;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_SET        = $04C4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_CLEAR      = $04C5;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_RESERVED   = $04C6;                                // Databytes = EventID
  MTI_PRODUCER_IDENDIFY              = $0914;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFY_RANGE        = $0524;                                // Databytes = EventID with Mask
  MTI_PRODUCER_IDENTIFIED_UNKNOWN    = $0547;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_SET        = $0544;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_CLEAR      = $0545;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_RESERVED   = $0546;                                // Databytes = EventID
  MTI_EVENTS_IDENTIFY_DEST           = $0968;                                // Databytes = Destination Alias
  MTI_EVENTS_IDENTIFY                = $0970;                                //
  MTI_EVENT_LEARN                    = $0594;                                // Databytes = EventID
  MTI_PC_EVENT_REPORT                = $05B4;                                // Databytes = EventID  (Infamouse PCER)

  MTI_SIMPLE_NODE_INFO_REQUEST       = $0DE8;                                // Databytes = Destination Alias
  MTI_SIMPLE_NODE_INFO_REPLY         = $0A08;                                // Databytes = Destination Alias, ACDI Data

  MTI_TRACTION_SIMPLE_TRAIN_INFO_REQUEST = $0DA8;                                // Databytes = Destination Alias
  MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY   = $09C8;                                // Databytes = Destination Alias, ACDI Data

  MTI_TRACTION_REQUEST               = $05E8;                                // Databyte = depends
  MTI_TRACTION_REPLY                 = $01E8;                                // Databyte = depends

  MTI_REMOTE_BUTTON_REQUEST           = $0948;
  MTI_REMOTE_BUTTON_REPLY            = $0949;

  MTI_STREAM_INIT_REQUEST            = $0CC8;
  MTI_STREAM_INIT_REPLY              = $0868;
  MTI_STREAM_SEND                    = $1F88;
  MTI_STREAM_PROCEED                 = $0888;
  MTI_STREAM_COMPLETE                = $08A8;

  MTI_DATAGRAM                       = $1C48;
  MTI_DATAGRAM_OK_REPLY              = $0A28;                                // Databytes = Destination Alias
  MTI_DATAGRAM_REJECTED_REPLY        = $0A48;                                // Databytes = Destination Alias, Error Code

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

  // Byte 0
  PIP_SIMPLENODE                     = $80;
  PIP_DATAGRAM                       = $40;
  PIP_STREAM                         = $20;
  PIP_MEMORY_CONFIG                  = $10;
  PIP_RESERVATION                    = $08;
  PIP_EVENT_EXCHANGE                 = $04;
  PIP_IDENTIFCIATION                 = $02;
  PIP_TEACH_LEARN                    = $01;

  // Byte 2
  PIP_REMOTE_BUTTON                  = $80;
  PIP_ABBREVIATED_CDI                = $40;
  PIP_DISPLAY                        = $20;
  PIP_SIMPLE_NODE_INFO               = $10;
  PIP_CDI                            = $08;
  PIP_TRACTION                       = $04;
  PIP_FDI                            = $02;
// PIP_DCC_COMMAND_STATION            = $01; depreciated

  // Byte 3
  PIP_SIMPLE_TRAIN_NODE_INFO         = $80;
  PIP_FUNCTION_CONFIGURATION         = $40;
  PIP_FIRMWARE_UPGRADE               = $20;
  PIP_FIRMWARE_UPGRADE_ACTIVE        = $10;


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
  STR_PIP_FIRMWARE_UPGRADE           = 'Firmware Upgrade Protocol';
  STR_PIP_FIRMWARE_UPGRADE_ACTIVE    = 'Firmware Upgrade Active Protocol';

const
  MCP_WRITE                           = $00;                                    // MemoryConfigurationProtocol - Write Memory Mask
  MCP_WRITE_STREAM                    = $20;
  MCP_READ                            = $40;                                    // MemoryConfigurationProtocol - Read Memory Mask
  MCP_READ_STREAM                     = $60;
  MCP_OPERATION                       = $80;                                    // MemoryConfigurationProtocol - Operation Mask
  MCP_READ_REPLY                      = $50;                                    // MemoryConfigurationProtocol - Read Reply Mask [Does not include the Address Space Mask "or" it with the the Address space masks below]
  MCP_WRITE_REPLY                     = $10;
  MCP_READ_STREAM_REPLY               = $70;

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
  MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY     = $87;
  MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY = $86;                            // MemoryConfigurationProtocol Operation - Get Add Space Info Reply
  MCP_OP_LOCK                        = $88;                                     // MemoryConfigurationProtocol Operation - Lock Node
  MCP_OP_LOCK_REPLY                  = $8A;                                     // MemoryConfigurationProtocol Operation - Lock Node Reply
  MCP_OP_GET_UNIQUEID                = $8C;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key
  MCP_OP_GET_UNIQUEID_REPLY          = $8E;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key Reply

  MCP_OP_FREEZE                      = $A0;                                     // MemoryConfigurationProtocol Operation - Freeze Node
  MCP_OP_INDICATE                    = $A4;                                     // MemoryConfigurationProtocol Operation - Indicate
  MCP_OP_RESETS                      = $A8;                                     // MemoryConfigurationProtocol Operation - Resets


  MSI_CDI                            = $FF;                                     // MemorySpaceIdentifier - Access the Configuration Definition Infomation (CDI)
  MSI_ALL                            = $FE;                                     // MemorySpaceIdentifier - Access All memory (define all in the application)
  MSI_CONFIG                         = $FD;                                     // MemorySpaceIdentifier - Access basic configuration memory that feeds into the CDI
  MSI_ACDI_MFG                       = $FC;                                     // MemorySpaceIdentifier - Access the ACDI Manfacturers Info
  MSI_ACDI_USER                      = $FB;                                     // MemorySpaceIdentifier - Access the ACDI User definable Info
  MSI_TRACTION_FDI                   = $FA;                                     // MemorySpaceIdentifier - Access the Traction Functions definable Info
  MSI_TRACTION_FUNCTION_CONFIG       = $F9;                                     // MemorySpaceIdentifier = Access the Traction Function State Information

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

  TRACTION_FLAGS_ALIAS_INCLUDED =  $01;

  TRACTION_SPEED_DIR                  = $00;
  TRACTION_FUNCTION                   = $01;
  TRACTION_E_STOP                     = $02;

  TRACTION_QUERY_SPEED                = $10;
  TRACTION_QUERY_SPEED_REPLY          = $10;
  TRACTION_QUERY_FUNCTION             = $11;
  TRACTION_QUERY_FUNCTION_REPLY       = $11;


  TRACTION_CONTROLLER_CONFIG                 = $20;
  TRACTION_CONTROLLER_CONFIG_REPLY           = $20;
  TRACTION_CONTROLLER_CONFIG_ASSIGN          = $01;
  TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY    = $01;
  TRACTION_CONTROLLER_CONFIG_RELEASE         = $02;
  TRACTION_CONTROLLER_CONFIG_QUERY           = $03;
  TRACTION_CONTROLLER_CONFIG_QUERY_REPLY     = $03;
  TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY = $04;
  TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY  = $04;

  TRACTION_CONTROLLER_CONFIG_REPLY_OK = $00;
  TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER = $01;  // Bit 0
  TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN = $02; // Bit 1
  TRACTION_CONTROLLER_CONFIG_REPLY_PENDING = $FF;  // This is an internal value used in the statemachine

  TRACTION_LISTENER                   = $30;
  TRACTION_LISTENER_ATTACH            = $01;
  TRACTION_LISTENER_DETACH            = $02;
  TRACTION_LISTENER_QUERY             = $03;

  TRACTION_LISTENER_FLAG_ALIAS_VALID  = $01;
  TRACTION_LISTENER_FLAG_REVERSE_DIR  = $02;
  TRACTION_LISTENER_FLAG_LINK_F0      = $04;
  TRACTION_LISTENER_FLAG_LINK_FN      = $08;
  TRACTION_LISTENER_FLAG_HIDDEN       = $10;

  TRACTION_MANAGE                     = $40;
  TRACTION_MANAGE_RESERVE             = $01;
  TRACTION_MANAGE_RELEASE             = $02;
  TRACTION_RESERVE_REPLY              = $01;

  TRACTION_MANAGE_RESERVE_REPLY_OK   = $00;    // Failed is not 0

type
  TLccDccSpeedStep = (ldssDefault, ldss14, ldss28, ldss128);
  TLccMarklinProtocolVersion = (lmpvDefault, lmvpVer1, lmvpVer2, lmvpVer2ExtFunction);

const

  TRACTION_SEARCH_QUERY = $090099FF00000000;

  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_MASK            = $18; // 0001 1000

  // Bit 3-4 options
  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY             = $00; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY        = $08; // b0000 1000
  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_RESERVED_1      = $10; // b0001 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_RESERVED_2      = $18; // b0001 1000

  // Bit 2 options
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT    = $00; // b0000 0000 valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY only
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG       = $04; // b0000 0100 valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY only
  TRACTION_SEARCH_TRACK_PROTOCOL_NON_MARKLIN            = $00; // b0000 0000 valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY only
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN                = $04; // b0000 0100 valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY only

  TRACTION_SEARCH_PROTCOL_DETAILS_MASK                  = $03; // b0000 0011 bottom 2 bits

  // Bit 0-1
  // valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY and TRACTION_SEARCH_TRACK_PROTOCOL_NON_MARKLIN only
  TRACTION_SEARCH_TRACK_PROTOCOL_ALL                    = $00; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_NATIVE_OPENLCB         = $01; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_MFX_M4                 = $02; // b0000 0010
  TRACTION_SEARCH_TRACK_PROTOCOL_RESERVED_1             = $03; // b0000 0011

  // valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY and TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN only
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_ANY            = $00; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_1      = $01; // b0000 0001
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2      = $02; // b0000 0010
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2_F8   = $03; // b0000 0011

  // valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP     = $00; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP      = $01; // b0000 0001
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP      = $02; // b0000 0010
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP     = $03; // b0000 0011

  // Bit 7
  TRACTION_SEARCH_ALLOCATE_FORCE                        = $80; // b1000 0000
  TRACTION_SEARCH_ALLOCATE_EXISTING_ONLY                = $00; // b0000 0000

  // Bit 6
  TRACTION_SEARCH_TYPE_EXACT_MATCH                      = $40; // b0100 0000
  TRACTION_SEARCH_TYPE_ALL_MATCH                        = $00; // b0000 0000

  // Bit 5
  TRACTION_SEARCH_TARGET_ADDRESS_MATCH                  = $20; // b0010 0000
  TRACTION_SEARCH_TARGET_ANY_MATCH                      = $00; // b0000 0000

  MAX_CONFIG_MEM_READWRITE_SIZE = 64;

  ERROR_TEMPORARY                     = $2000;
  ERROR_PERMANENT                     = $1000;

  // Permanent error modifier (OR with ERROR_PERMANENT}
  ERROR_SOURCE_NOT_PERMITED           = $0020;
  ERROR_NOT_FOUND                     = $0030;
  ERROR_NOT_IMPLEMENTED               = $0040;

  ERROR_SUBCOMMAND                    = $0001;
  ERROR_TYPE                          = $0002;
  ERROR_MTI                           = $0003;

  // Temporary error modifier (OR with ERROR_TEMPORARY}
  ERROR_TIMEOUT                       = $0010;
  ERROR_BUFFER_UNAVAILABLE            = $0020;
  ERROR_NOT_EXPECTED                  = $0040;
  ERROR_TRANSFER_ERROR                = $0080;
  ERROR_INVALID_ARGUMENTS             = $0080;

  ERROR_WAITING_FOR_ENDFRAME          = $0001;
  ERROR_NO_START_FRAME                = $0001;
  ERROR_NO_END_FRAME                  = $0002;

  DATAGRAM_PROTOCOL_LOGREQUEST             = $01;
  DATAGRAM_PROTOCOL_LOGREPLY               = $02;
  DATAGRAM_PROTOCOL_CONFIGURATION          = $20;
  DATAGRAM_PROTOCOL_REMOTEBUTTON           = $21;
  DATAGRAM_PROTOCOL_DISPLAY                = $28;
  DATAGRAM_PROTOCOL_TRAINCONTROL           = $30;
  DATAGRAM_PROTOCOL_TWOBYTE                = $E0;
  DATAGRAM_PROTOCOL_SIXBYTE                = $F0;

  ACDI_MFG_SIZE_VERSION                    = 1;
  ACDI_MFG_SIZE_MANUFACTURER               = 41;
  ACDI_MFG_SIZE_MODEL                      = 41;
  ACDI_MFG_SIZE_HARDWARE_VERSION           = 21;
  ACDI_MFG_SIZE_SOFTWARE_VERSION           = 21;
  ACDI_MFG_SIZE                            = ACDI_MFG_SIZE_VERSION + ACDI_MFG_SIZE_MANUFACTURER + ACDI_MFG_SIZE_MODEL + ACDI_MFG_SIZE_HARDWARE_VERSION + ACDI_MFG_SIZE_SOFTWARE_VERSION;

  ACDI_USER_SIZE_VERSION                   = 1;
  ACDI_USER_SIZE_NAME                      = 63;
  ACDI_USER_SIZE_DESCRIPTION               = 64;
  ACDI_USER_SIZE                           = ACDI_USER_SIZE_VERSION + ACDI_USER_SIZE_NAME + ACDI_USER_SIZE_DESCRIPTION;

  ACDI_MFG_OFFSET_VERSION                   = 0;
  ACDI_MFG_OFFSET_MANUFACTURER              = ACDI_MFG_SIZE_VERSION;
  ACDI_MFG_OFFSET_MODEL                     = ACDI_MFG_OFFSET_MANUFACTURER + ACDI_MFG_SIZE_MANUFACTURER;
  ACDI_MFG_OFFSET_HARDWARE_VERSION          = ACDI_MFG_OFFSET_MODEL +  ACDI_MFG_SIZE_MODEL;
  ACDI_MFG_OFFSET_SOFTWARE_VERSION          = ACDI_MFG_OFFSET_HARDWARE_VERSION + ACDI_MFG_SIZE_SOFTWARE_VERSION;

  ACDI_USER_OFFSET_VERSION                  = 0;
  ACDI_USER_OFFSET_NAME                     =  ACDI_USER_SIZE_VERSION;
  ACDI_USER_OFFSET_DESCRIPTION              =  ACDI_USER_OFFSET_NAME + ACDI_USER_SIZE_NAME;

var
  NULL_NODE_ID: TNodeID;

  NULL_EVENT_ID              : TEventID;
  EVENT_EMERGENCY_STOP       : TEventID;
  EVENT_NEW_LOG_ENTRY        : TEventID;
  EVENT_IDENT_BUTTON_PRESSED : TEventID;
  EVENT_DUPLICATE_ID_DETECTED: TEventID;
  EVENT_IS_TRAIN             : TEventID;
 // EVENT_IS_PROXY             : TEventID;        depreciated
  EVENT_DELIVERS_CLOCK       : TEventID;

  // TCP Header:
  //  |   Flags 16 Bits   |       Size 24 Bits          |             Originating/Gateway Node 48 Bits              |             Message Capture Time 48 Bits                  |
  //  | Byte 00 | Byte 01 | Byte 02 | Byte 03 | Byte 04 | Byte 05 | Byte 06 | Byte 07 | Byte 08 | Byte 09 | Byte 10 | Byte 11 | Byte 12 | Byte 13 | Byte 14 | Byte 15 | Byte 16 |
  //         2 bytes      +          3 bytes            +                       6 bytes                             +                      6 bytes

  // TCP Message:
  //  |  MTI 16 bits      |               Source Node ID 48 bits                      |                Dest Node ID 48 bits {optional}            | Data Payload
  //  | Byte 17 | Byte 18 | Byte 19 | Byte 20 | Byte 21 | Byte 22 | Byte 23 | Byte 24 | Byte 25 | Byte 26 | Byte 27 | Byte 28 | Byte 29 | Byte 30 | Byte 31 | Byte 32 | Byte ... |
  //         2 bytes      +                 6 bytes                             +                      6 bytes

  // OR

  // TCP Message:
  //  |    MTI 16 bits    |               Source Node ID 48 bits                      |                        Data Payload
  //  | Byte 17 | Byte 18 | Byte 19 | Byte 20 | Byte 21 | Byte 22 | Byte 23 | Byte 24 | Byte 25 | Byte 26 | Byte ...|
  //         2 bytes      +                 6 bytes

  const
    OPSTACK_TCP_FLAG_LCC_MESSAGE = $8000;        //  Link Message = 0
    OPSTACK_TCP_FLAG_CHAINING     = $4000;
    // $2000, $1000 reserved
    OPSTACK_TCP_FLAG_MULTI_PART   = $0C00;        // $0000 = Single part, $0020 = First part, $0030 = center part, $0040 = last part
    // Rest are reserved

  const
    MAX_LCC_TCP_MESSAGE_DATA = 253;
    MAX_LCC_TCP_MESSAGE_PREAMBLE = 14;      // 2 - MTI, 6 - Source ID, 6 - Dest ID
    MIN_LCC_TCP_MESSAGE_PREAMBLE = 8;       // 2 - MTI, 6 - Source ID
    MAX_HEADER_ONLY_LEN = 17;
    MAX_HEADER_CONTRIBUTION_TO_SIZE_FIELD_LEN = 12;

    MAX_LCC_TCP_FRAME_LEN = MAX_LCC_TCP_MESSAGE_DATA + MAX_LCC_TCP_MESSAGE_PREAMBLE;    // Max frame sizse for a TCP message with Header and all included
    MAX_TCP_MESSAGE_ONLY_LEN = MAX_LCC_TCP_FRAME_LEN - MAX_HEADER_ONLY_LEN;               // Max frame size for just the Lcc Message itself

var
  Max_Allowed_Buffers: Integer = 1;

implementation

initialization

  // Necessary because of bug in Smart Mobile Studio Compiler.... 12/18/2019
  NULL_NODE_ID[0]               := 0;
  NULL_NODE_ID[1]               := 0;

  NULL_EVENT_ID[0]               := 0;
  NULL_EVENT_ID[1]               := 0;
  NULL_EVENT_ID[2]               := 0;
  NULL_EVENT_ID[3]               := 0;
  NULL_EVENT_ID[4]               := 0;
  NULL_EVENT_ID[5]               := 0;
  NULL_EVENT_ID[6]               := 0;

  EVENT_EMERGENCY_STOP[0]        := $01;
  EVENT_EMERGENCY_STOP[1]        := $00;
  EVENT_EMERGENCY_STOP[2]        := $00;
  EVENT_EMERGENCY_STOP[3]        := $00;
  EVENT_EMERGENCY_STOP[4]        := $00;
  EVENT_EMERGENCY_STOP[5]        := $FF;
  EVENT_EMERGENCY_STOP[6]        := $FF;

  EVENT_NEW_LOG_ENTRY[0]         := $01;
  EVENT_NEW_LOG_ENTRY[1]         := $00;
  EVENT_NEW_LOG_ENTRY[2]         := $00;
  EVENT_NEW_LOG_ENTRY[3]         := $00;
  EVENT_NEW_LOG_ENTRY[4]         := $00;
  EVENT_NEW_LOG_ENTRY[5]         := $FF;
  EVENT_NEW_LOG_ENTRY[6]         := $F8;

  EVENT_IDENT_BUTTON_PRESSED[0]  := $01;
  EVENT_IDENT_BUTTON_PRESSED[1]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[2]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[3]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[4]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[5]  := $FE;
  EVENT_IDENT_BUTTON_PRESSED[6]  := $00;

  EVENT_DUPLICATE_ID_DETECTED[0] := $01;
  EVENT_DUPLICATE_ID_DETECTED[1] := $10;
  EVENT_DUPLICATE_ID_DETECTED[2] := $00;
  EVENT_DUPLICATE_ID_DETECTED[3] := $00;
  EVENT_DUPLICATE_ID_DETECTED[4] := $00;
  EVENT_DUPLICATE_ID_DETECTED[5] := $02;
  EVENT_DUPLICATE_ID_DETECTED[6] := $01;

  EVENT_IS_TRAIN[0]              := $01;
  EVENT_IS_TRAIN[1]              := $01;
  EVENT_IS_TRAIN[2]              := $00;
  EVENT_IS_TRAIN[3]              := $00;
  EVENT_IS_TRAIN[4]              := $00;
  EVENT_IS_TRAIN[5]              := $03;
  EVENT_IS_TRAIN[6]              := $03;

  // EVENT_IS_PROXY Depreciated

  EVENT_DELIVERS_CLOCK[0]        := $01;
  EVENT_DELIVERS_CLOCK[1]        := $01;
  EVENT_DELIVERS_CLOCK[2]        := $00;
  EVENT_DELIVERS_CLOCK[3]        := $00;
  EVENT_DELIVERS_CLOCK[4]        := $00;
  EVENT_DELIVERS_CLOCK[5]        := $05;
  EVENT_DELIVERS_CLOCK[6]        := $01;


end.

