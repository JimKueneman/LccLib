unit lcc_node_messages;

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
  System.Device.Storage,
  SmartCL.Device.Storage,
  SmartCL.Application,
  SmartCL.Components,
  SmartCL.System,
  System.Memory,
  System.Memory.Buffer,
  System.Memory.Views,
  {$ELSE}
  Classes,
  SysUtils,
  {$ENDIF}
  lcc_math_float16,
  lcc_defines,
  lcc_utilities;

type
  TLccMessage = class; // forward
  TOnMessageEvent = procedure(Sender: TObject; LccMessage: TLccMessage) of object;
  TOnMessageEventFunc = function(Sender: TObject; LccMessage: TLccMessage): Boolean of object;
  TOnMessageEventArray = array of TOnMessageEventFunc;
  TSearchEncodeStringError = (sese_ok, sese_TooLong, sese_InvalidCharacters);

  { TLccCANMessage }

  TLccCANMessage = class
  private
    FDestAlias: Word;
    FFramingBits: Byte;
    FiTag: Integer;
    FMTI: DWord;
    FSourceAlias: Word;
  public
    property iTag: Integer read FiTag write FiTag;        // General purpose counter/integer depending on the message
    property MTI: DWord read FMTI write FMTI;             // WARNING:  This MTI is shifted Left by 2 Bytes where the Source Address Was!!!!!!
    property DestAlias: Word read FDestAlias write FDestAlias;
    property FramingBits: Byte read FFramingBits write FFramingBits; // Bottom 2 bits, upper nibble of the Destination alias
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
  end;


{ TLccMessage }

TLccMessage = class
private
  FAbandonTimeout: Integer;                 // If the message is being held for some reason (CAN multi frame assembly, waiting for AME to aquire the Alias, etc) this is used to see how long it has been alive and when to decide it has been abandon and should be freed
  FIsCAN: Boolean;                          // The message is a CAN link message which typically needs special handling
  FCAN: TLccCANMessage;
  FDataArray: TLccByteArray;                // The payload of the message (if there is a payload).  This is the full payload that has been already been assembled if we are on a CAN link
  FDataCount: Integer;                      // How many bytes in the DataArray are valid
  FDestID: TNodeID;                         // NodeID of the Destination of a message (typically a node in our NodeManager)
  FSourceID: TNodeID;                       // NodeID of the Source of a message (AliasServer will ensure this is populated)
  FMTI: Word;                               // The Actual MTI of the message IF it is not a CAN frame message
  FRetryAttempts: Integer;                  // If a message returned "Temporary" (like no buffers) this holds how many time it has been retried and defines a give up time to stop resending
  function GetHasDestination: Boolean;
  function GetHasDestNodeID: Boolean;
  function GetHasSourceNodeID: Boolean;
  function GetDataArrayIndexer(iIndex: DWord): Byte;

  procedure SetDataArrayIndexer(iIndex: DWord; const Value: Byte);
protected

public
  property AbandonTimeout: Integer read FAbandonTimeout write FAbandonTimeout;
  property CAN: TLccCANMessage read FCAN write FCAN;
  property DestID: TNodeID read FDestID write FDestID;
  property DataArray: TLccByteArray read FDataArray write FDataArray;
  property DataArrayIndexer[iIndex: DWord]: Byte read GetDataArrayIndexer write SetDataArrayIndexer;
  property DataCount: Integer read FDataCount write FDataCount;
  property HasDestination: Boolean read GetHasDestination;
  property HasDestNodeID: Boolean read GetHasDestNodeID;
  property HasSourceNodeID: Boolean read GetHasSourceNodeID;
  property IsCAN: Boolean read FIsCAN write FIsCAN;
  property MTI: Word read FMTI write FMTI;
  property RetryAttempts: Integer read FRetryAttempts write FRetryAttempts;
  property SourceID: TNodeID read FSourceID write FSourceID;

  constructor Create;
  destructor Destroy; override;

  procedure AppendDataArray(LccMessage: TLccMessage);
  function AppendDataArrayAsString(LccMessage: TLccMessage; LastNull: Byte): Boolean;
  function Clone: TLccMessage;
  procedure InsertNodeID(StartIndex: Integer; var ANodeID: TNodeID);
  procedure InsertEventID(StartIndex: Integer; var AnEventID: TEventID);
  procedure InsertDWordAsDataBytes(DoubleWord: DWord; StartByteIndex: Integer);
  procedure InsertWordAsDataBytes(AWord: DWord; StartByteIndex: Integer);
  function ExtractDataBytesAsEventID(StartIndex: Integer): TEventID;
  function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): DWORD;
  function ExtractDataBytesAsNodeID(StartIndex: Integer; var ANodeID: TNodeID): TNodeID;
  function ExtractDataBytesAsString(StartIndex, Count: Integer): String;
  function ExtractDataBytesAsWord(StartIndex: Integer): Word;
  function ExtractDataBytesAsHex(StartByteIndex, EndByteIndex: Integer): string;
  function DestinationMatchs(TestAliasID: Word; TestNodeID: TNodeID): Boolean;

  function LoadByGridConnectStr(GridConnectStr: String): Boolean;
  function LoadByLccTcp(var ByteArray: TLccDynamicByteArray): Boolean;
  function ConvertToGridConnectStr(Delimiter: String; Details: Boolean): String;
  function ConvertToLccTcp(var ByteArray: TLccDynamicByteArray): Boolean;
  procedure CopyToTarget(TargetMessage: TLccMessage);
  class function ConvertToLccTcpString(var ByteArray: TLccDynamicByteArray): String;
  procedure ZeroFields;

  // CAN
  procedure LoadCID(ASourceID: TNodeID; ASourceAlias: Word; ACID: Byte);
  procedure LoadRID(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadAMD(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadAMR(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadAME(ASourceID: TNodeID; ASourceAlias: Word; TargetNodeID: TNodeID);
  // Basic
  procedure LoadInitializationComplete(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadVerifyNodeIDAddressed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadVerifyNodeID(ASourceID: TNodeID; ASourceAlias: Word; OptionalTargetNodeID: TNodeID);
  procedure LoadVerifiedNodeID(ASourceID: TNodeID; ASourceAlias: Word);
  // Protocol Support (PIP)
  procedure LoadProtocolIdentifyInquiry(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadProtocolIdentifyReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Flags: TLccSupportedProtocolArray);
  // Event Exchange
  procedure LoadConsumerIdentify(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID);
  procedure LoadConsumerIdentified(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID; EventState: TEventState);
  procedure LoadProducerIdentify(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID);
  procedure LoadProducerIdentified(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID; EventState: TEventState);
  procedure LoadIdentifyEventsAddressed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadIdentifyEvents(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadPCER(ASourceID: TNodeID; ASourceAlias: Word; AnEvent: TEventID);
  // Traction Control
  procedure LoadTractionSetSpeed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ASpeed: single);
  procedure LoadTractionSetFunction(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AnAddress: DWord; AValue: Word);
  procedure LoadTractionEStop(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadTractionQuerySpeed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadTractionQuerySpeedReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; SetSpeed: THalfFloat; Status: Byte; CommandedSpeed, ActualSpeed: THalfFloat);
  procedure LoadTractionQueryFunction(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Address: Word);
  procedure LoadTractionQueryFunctionReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Address: Word; Value: Word);
  procedure LoadTractionControllerAssign(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AControllerNodeID: TNodeID; AControllerAlias: Word);
  procedure LoadTractionControllerAssignReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AResult: Byte);
  procedure LoadTractionControllerRelease(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ANodeID: TNodeID; AnAlias: Word);
  procedure LoadTractionControllerQuery(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadTractionControllerQueryReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AControllerID: TNodeID; AControllerAlias: Word);
  procedure LoadTractionControllerChangingNotify(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AControllerNodeID: TNodeID; AControllerAlias: Word);
  procedure LoadTractionControllerChangedReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Allow: Boolean);
  procedure LoadTractionListenerAttach(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AListenerNodeID: TNodeID);
  procedure LoadTractionListenerAttachReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AListenerNodeID: TNodeID; ReplyCode: Word);
  procedure LoadTractionListenerDetach(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AListenerNodeID: TNodeID);
  procedure LoadTractionListenerDetachReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AListenerNodeID: TNodeID; ReplyCode: Word);
  procedure LoadTractionListenerQuery(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AListenerNodeID: TNodeID);
  procedure LoadTractionListenerQueryReply_NoInfo(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadTractionListenerQueryReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ListenerCount, ListenerNodeIndex: Byte; AListenerNodeID: TNodeID; Flags: Byte);
  procedure LoadTractionManage(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Reserve: Boolean);
  procedure LoadTractionManageReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Accepted: Boolean);

  function TractionExtractSetSpeed: THalfFloat;
  function TractionExtractCommandedSpeed: THalfFloat;
  function TractionExtractActualSpeed: THalfFloat;
  function TractionExtractSpeedStatus: Byte;
  function TractionExtractFunctionAddress: LongWord;
  function TractionExtractFunctionValue: Word;

  // Traction Search
  class function TractionSearchEncodeSearchString(SearchString: string; TrackProtocolFlags: Byte; var SearchData: DWORD): TSearchEncodeStringError;
  function TractionSearchDecodeSearchString: string;
  procedure LoadTractionSearch(ASourceID: TNodeID; ASourceAlias: Word; SearchData: DWORD);
  function TractionSearchExtractSearchData: DWORD;
  function TractionSearchIsEvent: Boolean;
  function TractionSearchIsForceAllocate: Boolean;
  function TractionSearchIsExactMatchOnly: Boolean;
  function TractionSearchIsAddressMatchOnly: Boolean;
  function TractionSearchIsProtocolAny: Boolean;
  function TractionSearchIsProtocolNativeOpenLcb: Boolean;
  function TractionSearchIsProtocolMFX_M4: Boolean;
  function TractionSearchIsProtocolMarklin(var ProtocolVersion: TLccMarklinProtocolVersion): Boolean;
  function TractionSearchIsProtocolDCC(var ForceLongAddress: Boolean; var SpeedStep: TLccDccSpeedStep): Boolean;
class  function TractionSearchEncodeNativeOpenLcb(ForceAllocate, ExactMatchOnly, MatchAddressOnly: Boolean): Byte;
class  function TractionSearchEncodeMFX_M4(ForceAllocate, ExactMatchOnly, MatchAddressOnly: Boolean): Byte;
class  function TractionSearchEncodeMarklin(ProtocolVersion: TLccMarklinProtocolVersion; ForceAllocate, ExactMatchOnly, MachAddressOnly: Boolean): Byte;
class  function TractionSearchEncodeNMRA(ForceLongAddress: Boolean; SpeedStep: TLccDccSpeedStep; ForceAllocate, ExactMatchOnly, MatchAddressOnly: Boolean): Byte;

  // Remote Button

  // Traction Identification (STNIP)
  procedure LoadSimpleTrainNodeIdentInfoRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure ExtractSimpleTrainNodeIdentInfoReply(var Version: Byte; var RoadName: string; var TrainClass: string; var RoadNumber: string; var TrainName: string; var Manufacturer: string; var Owner: string);
  // Node Ident (SNIP)
  procedure LoadSimpleNodeIdentInfoReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; SimplePackedArray: TLccDynamicByteArray);
  procedure LoadSimpleNodeIdentInfoRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  // FDI
  procedure LoadFDIRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadFunctionConfigurationRead(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; FunctionAddress: DWord; Count: Integer);
  procedure LoadFunctionConfigurationWrite(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; FunctionAddress: DWord; Count: Integer; Functions: TFunctionStatesArray);
  // CDI
  procedure LoadCDIRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  // Datagram
  procedure LoadDatagram(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadDatagramAck(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Ok: Boolean; ReplyPending: Boolean; TimeOutValueN: Byte);
  procedure LoadDatagramRejected(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Reason: Word);
  // ConfigurationMemory
  function ExtractAddressSpace: Byte;
  procedure LoadConfigMemAddressSpaceInfo(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte);
  procedure LoadConfigMemOptions(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadConfigMemRead(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte; ConfigMemAddress: DWord; ConfigMemSize: Byte);
  procedure LoadConfigMemWriteInteger(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte; ConfigMemAddress: DWord; IntegerSize: Byte; DataInteger: Integer);
  procedure LoadConfigMemWriteString(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte; ConfigMemAddress: DWord; AString: String);
  procedure LoadConfigMemWriteArray(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte; ConfigMemAddress: DWord; ArraySize: Integer; AnArray: array of Byte);
  // MTIs
  procedure LoadOptionalInteractionRejected(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Reason: Word; AnMTI: Word);

  procedure SwapDestAndSourceIDs;
end;


{$IFDEF FPC}
function MessageToDetailedMessage(AMessage: TLccMessage): String;
{$ENDIF}

implementation

var
  CaptureTime: Longword;


{$IFDEF FPC}
function IsPrintableChar(C: Char): Boolean;
begin
  Result := ((Ord( C) >= 32) and (Ord( C) <= 126)) { or ((Ord( C) >= 128) and (Ord( C) <= 255)) }
end;

function MTI_ToString(MTI: DWord): String;
begin
  case MTI of
    MTI_CAN_CID0 : Result := 'Check ID 0';
    MTI_CAN_CID1 : Result := 'Check ID 1';
    MTI_CAN_CID2 : Result := 'Check ID 2';
    MTI_CAN_CID3 : Result := 'Check ID 3';
    MTI_CAN_CID4 : Result := 'Check ID 4';
    MTI_CAN_CID5 : Result := 'Check ID 5';
    MTI_CAN_CID6 : Result := 'Check ID 6';

    MTI_CAN_RID : Result := 'Reserve ID [RID]';
    MTI_CAN_AMD : Result := 'Alias Map Definition [AMD]';
    MTI_CAN_AME : Result := 'Alias Map Enquiry [AME]';
    MTI_CAN_AMR : Result := 'Alias Map Reset [AMR]';

    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY : begin
                                           Result := 'Datagram Single Frame:';

                                         end;
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START : begin
                                           Result := 'Datagram Start Frame:';

                                         end;
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME : Result := 'Datagram Frame';
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END : Result := 'Datagram End Frame';

    MTI_INITIALIZATION_COMPLETE : Result := 'Initialization Complete';
    MTI_VERIFY_NODE_ID_NUMBER_DEST : Result := 'Verify Node ID with Destination Address';
    MTI_VERIFY_NODE_ID_NUMBER      : Result := 'Verify Node ID Global';
    MTI_VERIFIED_NODE_ID_NUMBER    : Result := 'Verified Node ID';
    MTI_OPTIONAL_INTERACTION_REJECTED : Result := 'Optional Interaction Rejected';
    MTI_TERMINATE_DUE_TO_ERROR        : Result := 'Terminate Due to Error';

    MTI_PROTOCOL_SUPPORT_INQUIRY  : Result := 'Protocol Support Inquiry';
    MTI_PROTOCOL_SUPPORT_REPLY    : Result := 'Protocol Support Reply';

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

    MTI_TRACTION_SIMPLE_TRAIN_INFO_REQUEST       : Result := 'Simple Train Node Info Request [STNIP]';
    MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY         : Result := 'Simple Train Node Info Reply [STNIP]';

    MTI_DATAGRAM                        : Result := 'Datagram';
    MTI_DATAGRAM_OK_REPLY               : Result := 'Datagram Reply OK';
    MTI_DATAGRAM_REJECTED_REPLY         : Result := 'Datagram Rejected Reply';

    MTI_TRACTION_REQUEST               : Result := 'Traction Protocol';
    MTI_TRACTION_REPLY                 : Result := 'Traction Reply';
    MTI_STREAM_INIT_REQUEST            : Result := 'Stream Init Request';
    MTI_STREAM_INIT_REPLY              : Result := 'Stream Init Reply';
    MTI_STREAM_SEND                    : Result := 'Stream Send - CAN Frame';
    MTI_STREAM_PROCEED                 : Result := 'Stream Proceed';
    MTI_STREAM_COMPLETE                : Result := 'Stream Complete';
   else
    Result := 'Unknown MTI';
  end;
end;

function RawHelperDataToStr(AMessage: TLccMessage; ASCII: Boolean): string;
var
  j, iStart: Integer;
begin
  Result := '';
//  if AMessage.HasDestinationAddress then
 //   iStart := 2
//  else

  iStart := 0;
  Result := Result + ' [';
  for j := iStart to AMessage.DataCount - 1 do                     // Skip the Address
  begin
    if ASCII then
    begin
      if IsPrintableChar( Chr( AMessage.DataArray[j])) then
        Result := Result + Chr( AMessage.DataArray[j])
      else
        Result := Result + '.';
    end else
    begin
      Result := Result + IntToHex(AMessage.DataArray[j], 2);
      if j < AMessage.DataCount then
        Result := Result + '.'
    end;
  end;
  Result := Result + ']';
end;

function EqualEvents(Event1, Event2: PEventID): Boolean;
begin
  Result := (Event1^[0] = Event2^[0]) and (Event1^[1] = Event2^[1]) and (Event1^[2] = Event2^[2]) and (Event1^[3] = Event2^[3]) and
            (Event1^[4] = Event2^[4]) and (Event1^[5] = Event2^[5]) and (Event1^[6] = Event2^[6]) and (Event1^[7] = Event2^[7])
end;

function EventIDToString(EventID: PEventID): String;
var
  Address: Word;
begin
  if EqualEvents(EventID, @EVENT_IS_TRAIN) then
    Result := 'EVENT_TRAIN'
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
  if (EventID^[0] = $09) and (EventID^[1] = $00) and (EventID^[2] = $99) and (EventID^[3] = $FF) then
  begin
    Result := 'Traction Search Event';
  end else
    Result := 'Unique Event'
end;


function MessageToDetailedMessage(AMessage: TLccMessage): String;
var
  j, S_Len: Integer;
  f: single;
  Half: Word;
begin
  Result := AMessage.ConvertToGridConnectStr('', False);
  S_Len := Length(Result);
  for j := 0 to (28-S_Len) do
    Result := Result + ' ' ;

  if AMessage.IsCAN then
  begin
    if AMessage.CAN.MTI = MTI_CAN_CID0 then Result := Result + 'CAN Check ID 0' else
    if AMessage.CAN.MTI = MTI_CAN_CID1 then Result := Result + 'CAN Check ID 1' else
    if AMessage.CAN.MTI = MTI_CAN_CID2 then Result := Result + 'CAN Check ID 2' else
    if AMessage.CAN.MTI = MTI_CAN_CID3 then Result := Result + 'CAN Check ID 3' else
    if AMessage.CAN.MTI = MTI_CAN_CID4 then Result := Result + 'CAN Check ID 4' else
    if AMessage.CAN.MTI = MTI_CAN_CID5 then Result := Result + 'CAN Check ID 5' else
    if AMessage.CAN.MTI = MTI_CAN_CID6 then Result := Result + 'CAN Check ID 6' else
    if AMessage.CAN.MTI = MTI_CAN_RID then Result := Result + 'CAN Reserve ID' else
    if AMessage.CAN.MTI = MTI_CAN_AMD then Result := Result + 'CAN Alias Map Definition' else
    if AMessage.CAN.MTI = MTI_CAN_AME then Result := Result + 'CAN Alias Mapping Enquiry' else
    if AMessage.CAN.MTI = MTI_CAN_AMR then Result := Result + 'CAN Alias Map Reset';
    Exit
  end;

  if AMessage.HasDestination then
    Result := Result + '0x' + IntToHex( AMessage.CAN.SourceAlias, 4) + ' -> ' + '0x' + IntToHex( AMessage.CAN.DestAlias, 4)
  else
    Result := Result + '0x' + IntToHex( AMessage.CAN.SourceAlias, 4);

  if AMessage.MTI = MTI_DATAGRAM then
    Result := Result + RawHelperDataToStr(AMessage, True) + ' MTI: ' + MTI_ToString(AMessage.MTI)
  else
    Result := Result + '   MTI: ' + MTI_ToString(AMessage.MTI) + ' - ';

  if AMessage.MTI = MTI_STREAM_SEND then
  begin
    case AMessage.MTI of
      MTI_STREAM_INIT_REQUEST            : Result := Result + ' Suggested Bufer Size: ' + IntToStr((AMessage.DataArray[2] shl 8) or AMessage.DataArray[3]) + ' Flags: 0x' + IntToHex(AMessage.DataArray[4], 2) + ' Additional Flags: 0x' + IntToHex(AMessage.DataArray[5], 2) + ' Source Stream ID: ' + IntToStr(AMessage.DataArray[6]);
      MTI_STREAM_INIT_REPLY              : Result := Result + ' Negotiated Bufer Size: ' + IntToStr((AMessage.DataArray[2] shl 8) or AMessage.DataArray[3]) + ' Flags: 0x' + IntToHex(AMessage.DataArray[4], 2) + ' Additional Flags: 0x' + IntToHex(AMessage.DataArray[5], 2) + ' Source Stream ID: ' + IntToStr(AMessage.DataArray[6]) + ' Destination Stream ID: ' + IntToStr(AMessage.DataArray[7]);
      MTI_STREAM_SEND                    : begin end;
      MTI_STREAM_PROCEED                 : Result := Result + ' Source Stream ID: ' + IntToStr(AMessage.DataArray[2]) + ' Destination Stream ID: ' + IntToStr(AMessage.DataArray[3]) + ' Flags: 0x' + IntToHex(AMessage.DataArray[4], 2) + ' Additional Flags: 0x' + IntToHex(AMessage.DataArray[5], 2);
      MTI_STREAM_COMPLETE                : Result := Result + ' Source Stream ID: ' + IntToStr(AMessage.DataArray[2]) + ' Destination Stream ID: ' + IntToStr(AMessage.DataArray[3]) + ' Flags: 0x' + IntToHex(AMessage.DataArray[4], 2) + ' Additional Flags: 0x' + IntToHex(AMessage.DataArray[5], 2);
    end
  end;

  if AMessage.MTI = MTI_OPTIONAL_INTERACTION_REJECTED then
  begin
  end;

  // SNII/SNIP
  if AMessage.MTI = MTI_SIMPLE_NODE_INFO_REPLY then
    Result := Result + RawHelperDataToStr(AMessage, True);

  // STNIP
  if AMessage.MTI = MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY then
    Result := Result + RawHelperDataToStr(AMessage, True);

  // Events
  if (AMessage.MTI = MTI_PRODUCER_IDENDIFY) or (AMessage.MTI = MTI_PRODUCER_IDENTIFIED_SET) or (AMessage.MTI = MTI_PRODUCER_IDENTIFIED_CLEAR) or
    (AMessage.MTI = MTI_PRODUCER_IDENTIFIED_UNKNOWN) or (AMessage.MTI = MTI_CONSUMER_IDENTIFY) or (AMessage.MTI = MTI_CONSUMER_IDENTIFIED_SET) or
    (AMessage.MTI = MTI_CONSUMER_IDENTIFIED_CLEAR) or (AMessage.MTI = MTI_CONSUMER_IDENTIFIED_UNKNOWN) or (AMessage.MTI = MTI_PC_EVENT_REPORT)
  then begin
      Result := Result + 'EventID: ' + EventIDToString(@AMessage.DataArray);
  end;

  // Traction Protocol
  if AMessage.MTI = MTI_TRACTION_REQUEST then
  begin
    case AMessage.DataArray[0] of
        TRACTION_SPEED_DIR :
          begin
            Result := Result + ' LCC Speed/Dir Operation; Speed = ';
            f := HalfToFloat( (AMessage.DataArray[1] shl 8) or AMessage.DataArray[2]);
            if f = 0 then
            begin
              if DWord( f) and $80000000 = $80000000 then
                Result := Result + '-0.0'
              else
                Result := Result + '+0.0'
            end else
              Result := Result + IntToStr( round(f));
          end;
        TRACTION_FUNCTION : Result := Result + ' LCC Traction Operation - Function Address: ' + IntToStr( AMessage.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( AMessage.ExtractDataBytesAsInt(1, 3), 6) + '], Value: ' + IntToStr( AMessage.ExtractDataBytesAsInt(4, 5)) + ' [0x' + IntToHex( AMessage.ExtractDataBytesAsInt(4, 5), 2) + ']';
        TRACTION_E_STOP : Result := Result + ' LCC Traction Emergency Stop';
        TRACTION_QUERY_SPEED : Result := Result + ' Query Speeds';
        TRACTION_QUERY_FUNCTION : Result := Result + ' Query Function - Address: ' + IntToStr( AMessage.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( AMessage.ExtractDataBytesAsInt(1, 3), 6) + ']';
        TRACTION_CONTROLLER_CONFIG :
          begin;
            case AMessage.DataArray[1] of
              TRACTION_CONTROLLER_CONFIG_ASSIGN :
                begin
                  if AMessage.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                    Result := Result + ' Controller Config Assign - Flags: ' + AMessage.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + AMessage.ExtractDataBytesAsHex(3, 6) + AMessage.ExtractDataBytesAsHex(7, 8) + ' [Alias: ' + AMessage.ExtractDataBytesAsHex(9, 10) + ']'
                  else
                    Result := Result + ' Controller Config Assign - Flags: ' + AMessage.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + AMessage.ExtractDataBytesAsHex(3, 6) + AMessage.ExtractDataBytesAsHex(7, 8) + ' Alias not included'
                end;
              TRACTION_CONTROLLER_CONFIG_RELEASE :
                begin
                  if AMessage.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                    Result := Result + ' Controller Config Release - Flags: ' + AMessage.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + AMessage.ExtractDataBytesAsHex(3, 6) + AMessage.ExtractDataBytesAsHex(7, 8) + ' [Alias: ' + AMessage.ExtractDataBytesAsHex(9, 10) + ']'
                  else
                    Result := Result + ' Controller Config Release - Flags: ' + AMessage.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + AMessage.ExtractDataBytesAsHex(3, 6) + AMessage.ExtractDataBytesAsHex(7, 8) + ' Alias not included'
                end;
              TRACTION_CONTROLLER_CONFIG_QUERY :
                begin
                  Result := Result + ' Controller Config Query';
                end;
              TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY :
                begin
                  if AMessage.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                    Result := Result + ' Controller Config Notify - Flags: ' + AMessage.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + AMessage.ExtractDataBytesAsHex(3, 6) + AMessage.ExtractDataBytesAsHex(7, 8) + ' [Alias: ' + AMessage.ExtractDataBytesAsHex(9, 10) + ']'
                  else
                    Result := Result + ' Controller Config Notify - Flags: ' + AMessage.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + AMessage.ExtractDataBytesAsHex(3, 6) + AMessage.ExtractDataBytesAsHex(7, 8) + ' Alias not included'
                end
            end
          end;
        TRACTION_LISTENER :
          begin
            case AMessage.DataArray[1] of
              TRACTION_LISTENER_ATTACH : Result := Result + 'Consist Listener Attach';
              TRACTION_LISTENER_DETACH : Result := Result + 'Consist Listener Detach';
              TRACTION_LISTENER_QUERY : Result := Result + 'Consit Listener Query';
            end
          end;
        TRACTION_MANAGE :
          begin
            case AMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE : Result := Result + 'Traction Management Reserve';
                TRACTION_MANAGE_RELEASE : Result := Result + 'Traction Management Release'
            end
          end
    else
      Result := Result + 'Unknown Traction Operation';
    end;
  end;

  // Traction Protocol Reply
  if AMessage.MTI = MTI_TRACTION_REPLY then
  begin
    case AMessage.DataArray[0] of
        TRACTION_QUERY_SPEED :
          begin
            Result := Result + 'Query Speed Reply : Set Speed = ';
              Half := (AMessage.DataArray[1] shl 8) or AMessage.DataArray[2];
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

              Result := Result + ': Status = ' + AMessage.ExtractDataBytesAsHex(3, 3);

              Result := Result + ': Commanded Speed = ';
              Half := (AMessage.DataArray[4] shl 8) or AMessage.DataArray[5];
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
              Half := (AMessage.DataArray[6] shl 8) or AMessage.DataArray[7];
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
        TRACTION_QUERY_FUNCTION : Result := Result + 'Query Function Reply - Address: ' + IntToStr( AMessage.ExtractDataBytesAsInt(1, 3)) + ', Value: ' + IntToStr( AMessage.ExtractDataBytesAsInt(4, 5));
        TRACTION_CONTROLLER_CONFIG :
          begin;
            case AMessage.DataArray[1] of
              TRACTION_CONTROLLER_CONFIG_ASSIGN :
                begin
                  Result := Result + 'Controller Config Assign Reply - Flags = ' + AMessage.ExtractDataBytesAsHex(2, 2)
                end;
              TRACTION_CONTROLLER_CONFIG_QUERY :
                begin
                  if AMessage.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                    Result := Result + 'Controller Config Query Reply - Flags = ' + AMessage.ExtractDataBytesAsHex(2, 2) + ' Result = ' + AMessage.ExtractDataBytesAsHex(3, 3) + ' Active Controller = 0x' + IntToHex(AMessage.ExtractDataBytesAsInt(4, 9), 12) + ' Alias = 0x' + IntToHex(AMessage.ExtractDataBytesAsInt(10, 11), 4)
                  else
                    Result := Result + 'Controller Config Query Reply - Flags = ' + AMessage.ExtractDataBytesAsHex(2, 2) + ' Result = ' + AMessage.ExtractDataBytesAsHex(3, 3) + ' Active Controller = 0x' + IntToHex(AMessage.ExtractDataBytesAsInt(4, 9), 12);
                end;
              TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY :
                begin
                  Result := Result + 'Controller Config Notify Reply - Result = ' + AMessage.ExtractDataBytesAsHex(2, 2)
                end;
            end
          end;
        TRACTION_LISTENER :
          begin
            case AMessage.DataArray[1] of
              TRACTION_LISTENER_ATTACH : Result := Result + 'Consist Listener Attach Reply';
              TRACTION_LISTENER_DETACH : Result := Result + 'Consist Listener Detach Reply';
              TRACTION_LISTENER_QUERY : Result := Result + 'Consit Listener Query Reply';
            end
          end;
        TRACTION_MANAGE :
          begin
            case AMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE : Result := Result +  'Manage: Reserve' + 'Result = ' + AMessage.ExtractDataBytesAsHex(2, 2);
            end
          end
    else
      Result := Result + 'Unknown Traction Reply Operation';
    end;
  end;
end;

{$ENDIF}

{ TLccMessage }

procedure TLccMessage.AppendDataArray(LccMessage: TLccMessage);
var
  i: Integer;
begin
  for i := 0 to LccMessage.DataCount - 1 do
  begin
    FDataArray[DataCount] := LccMessage.DataArray[i];
    Inc(FDataCount);
  end;
end;

function TLccMessage.AppendDataArrayAsString(LccMessage: TLccMessage;
  LastNull: Byte): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to LccMessage.DataCount - 1 do
  begin
    FDataArray[DataCount] := LccMessage.DataArray[i];
    if DataArray[DataCount] = Ord(#0) then
    begin
      Inc(CAN.FiTag);
      if CAN.iTag = LastNull then
        Result := True;
    end;
    Inc(FDataCount);
  end;
end;

function TLccMessage.Clone: TLccMessage;
begin
  Result := TLccMessage.Create;
  Result.FIsCAN := FIsCAN;
  Result.CAN.FDestAlias := CAN.FDestAlias;
  Result.CAN.FFramingBits := CAN.FFramingBits;
  Result.CAN.FiTag := CAN.FiTag;
  Result.CAN.FMTI := CAN.FMTI;
  Result.CAN.FSourceAlias := CAN.FSourceAlias;
  Result.FDataArray := FDataArray;
  Result.FDataCount := FDataCount;
  Result.FDestID := FDestID;
  Result.FSourceID := FSourceID;
  Result.FMTI := FMTI;
  Result.RetryAttempts := 0;
end;

procedure TLccMessage.InsertNodeID(StartIndex: Integer; var ANodeID: TNodeID);
begin
  FDataArray[StartIndex]     := _Higher( ANodeID[1]); // But these all need the 48 Bit Full ID in the Byte Fields
  FDataArray[StartIndex + 1] := _Hi(     ANodeID[1]);
  FDataArray[StartIndex + 2] := _Lo(     ANodeID[1]);
  FDataArray[StartIndex + 3] := _Higher( ANodeID[0]);
  FDataArray[StartIndex + 4] := _Hi(     ANodeID[0]);
  FDataArray[StartIndex + 5] := _Lo(     ANodeID[0]);
end;

procedure TLccMessage.InsertEventID(StartIndex: Integer; var AnEventID: TEventID);
begin
  FDataArray[StartIndex]     := AnEventID[0]; // But these all need the 48 Bit Full ID in the Byte Fields
  FDataArray[StartIndex + 1] := AnEventID[1];
  FDataArray[StartIndex + 2] := AnEventID[2];
  FDataArray[StartIndex + 3] := AnEventID[3];
  FDataArray[StartIndex + 4] := AnEventID[4];
  FDataArray[StartIndex + 5] := AnEventID[5];
  FDataArray[StartIndex + 6] := AnEventID[6];
  FDataArray[StartIndex + 7] := AnEventID[7];
end;

procedure TLccMessage.InsertDWordAsDataBytes(DoubleWord: DWord; StartByteIndex: Integer);
begin
  FDataArray[StartByteIndex]   := _Highest(DoubleWord);
  FDataArray[StartByteIndex+1] := _Higher(DoubleWord);
  FDataArray[StartByteIndex+2] := _Hi(DoubleWord);
  FDataArray[StartByteIndex+3] := _Lo(DoubleWord);
end;

procedure TLccMessage.InsertWordAsDataBytes(AWord: DWord; StartByteIndex: Integer);
begin
  FDataArray[StartByteIndex]   := _Hi(AWord);
  FDataArray[StartByteIndex+1] := _Lo(AWord);
end;

function TLccMessage.GetDataArrayIndexer(iIndex: DWord): Byte;
begin
  Result := FDataArray[iIndex]
end;

function TLccMessage.GetHasDestination: Boolean;
begin
  Result := (CAN.DestAlias <> 0) or ((DestID[0] <> 0) and (DestID[1] <> 0))
end;

function TLccMessage.GetHasDestNodeID: Boolean;
begin
  Result := (FDestID[0] <> 0) and (FDestID[1] <> 0)
end;

function TLccMessage.GetHasSourceNodeID: Boolean;
begin
  Result := (FSourceID[0] <> 0) and (FSourceID[1] <> 0)
end;

constructor TLccMessage.Create;
begin
  inherited Create;
  CAN := TLccCANMessage.Create;
end;

destructor TLccMessage.Destroy;
begin
  FCAN.Free;
  inherited Destroy;
end;

function TLccMessage.ExtractAddressSpace: Byte;
begin
  // Only valid if the message is a configuration memory message!!!!!!!
  Result := 0;
  case DataArrayIndexer[1] and $03 of
    0 : Result := DataArrayIndexer[6];
    1 : Result := MSI_CONFIG;
    2 : Result := MSI_ALL;
    3 : Result := MSI_CDI;
  end;
end;

function TLccMessage.ExtractDataBytesAsEventID(StartIndex: Integer): TEventID;
begin
  Result[0] := DataArray[StartIndex];
  Result[1] := DataArray[StartIndex+1];
  Result[2] := DataArray[StartIndex+2];
  Result[3] := DataArray[StartIndex+3];
  Result[4] := DataArray[StartIndex+4];
  Result[5] := DataArray[StartIndex+5];
  Result[6] := DataArray[StartIndex+6];
  Result[7] := DataArray[StartIndex+7];
end;

function TLccMessage.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): DWORD; // QWord;
var
  i, Offset, Shift: Integer;
  ByteAsDWORD, ShiftedByte: DWORD;
begin
  // Dec 2019, changed from QWORD to DWORD to work with Smart Mobile Studio and JScript
  // Can't find an instance where more than 4 bytes are requested in the library but this will test to be sure at runtime
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  Assert(Offset < 4, 'ExtractDataBytesAsInt requested larger than a DWORD result');
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsDWORD := DWORD( DataArray[i]);
    ShiftedByte := ByteAsDWORD shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;


function TLccMessage.ExtractDataBytesAsNodeID(StartIndex: Integer; var ANodeID: TNodeID): TNodeID;
begin
  ANodeID[1] := (DataArray[StartIndex] shl 16) or (DataArray[StartIndex+1] shl 8) or DataArray[StartIndex+2];
  ANodeID[0] := (DataArray[StartIndex+3] shl 16) or (DataArray[StartIndex+4] shl 8) or DataArray[StartIndex+5];
  Result := ANodeID;
end;

function TLccMessage.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := StartIndex to Count - 1 do
  begin
    if DataArray[i] <> Ord(#0) then
      Result := Result + Chr( DataArray[i])
    else
      Break
  end;
end;

function TLccMessage.ExtractDataBytesAsWord(StartIndex: Integer): Word;
begin
  Result := DataArray[StartIndex];
  Result := Result shl 8;
  Result := DataArray[StartIndex+1] or Result;
end;

function TLccMessage.ExtractDataBytesAsHex(StartByteIndex, EndByteIndex: Integer): string;
begin
  Result := IntToHex(ExtractDataBytesAsInt(StartByteIndex, EndByteIndex), EndByteIndex-StartByteIndex);
end;

procedure TLccMessage.ExtractSimpleTrainNodeIdentInfoReply(var Version: Byte;
  var RoadName: string; var TrainClass: string; var RoadNumber: string;
  var TrainName: string; var Manufacturer: string; var Owner: string);
begin
  if DataCount > 0 then
    Version := DataArray[0];
  // TODO
  RoadName := 'RoadName';
  TrainClass := 'TrainClass';
  RoadNumber := 'RoadNumber';
  TrainName := 'TrainName';
  Manufacturer := 'Manufacturer';
  Owner := 'Owner';
end;

function TLccMessage.LoadByGridConnectStr(GridConnectStr: String): Boolean;
var
  DestHi, DestLo: Byte;
  ZeroIndex: Boolean;   // FireMonkey uses 0 offset for strings instead of 1

  HeaderStr, DataStr, ByteStr: string;
  i, i_X, i_N, i_SemiColon, i_Data, len_Data, i_Data_Count: Integer;
  TempNodeID: TNodeID;
  CANFrameType: DWord;
begin
  Result := False;

  if GridConnectStr <> '' then
  begin
    ZeroFields;
    {$IFDEF FPC}
    ZeroIndex := False;
    {$ELSE}
    ZeroIndex := Low(GridConnectStr) = 0;   // Decide if we are LCC_MOBILE or not automatically
    {$ENDIF}
    GridConnectStr := UpperCase(GridConnectStr);
    i_X := Pos('X', GridConnectStr);                                              // Find were the "X" is in the string
    i_N := Pos('N', GridConnectStr);                                              // Find were the "N" is in the string
    i_SemiColon := Pos(';', GridConnectStr);
    i_Data := 1;  // First index in the DataStr (if it exists)
    // Find where the ";" is in the string
    if ZeroIndex then
    begin
      Dec(i_X);
      Dec(i_N);
      Dec(i_SemiColon);
      Dec(i_Data);
    end;

    HeaderStr := '';
    DataStr := '';

    for i:= i_X + 1 to i_N - 1 do
      HeaderStr := HeaderStr + GridConnectStr[i];
    for i := i_N + 1 to i_SemiColon - 1 do
      DataStr := DataStr + GridConnectStr[i];
    Len_Data := Length(DataStr);

    {$IFDEF DWSCRIPT}
    CAN.MTI := TDatatype.HexStrToInt('0x' + HeaderStr);
    {$ELSE}
    CAN.MTI := StrToInt( FormatStrToInt(HeaderStr));              // Convert the string MTI into a number  ;
    {$ENDIF}
    CAN.SourceAlias := Word( CAN.MTI and $00000FFF);                      // Grab the Source Alias before it is stripped off
    CAN.MTI := CAN.MTI and not $10000000;                                 // Strip off the reserved bits
    CAN.MTI := CAN.MTI and $FFFFF000;                                     // Strip off the Source Alias
    // Was this an OpenLCB or CAN specific message? This covers special multiFrame LccMessage and CAN layer specific messages
    IsCAN := (CAN.MTI and $07000000 <> $01000000);

    // Extract the General OpenLCB message if possible
    if IsCAN then                                                       // IsCAN means CAN Frames OR OpenLCB message that are only on CAN (Datagrams frames and Stream Send)
    begin
      CANFrameType := CAN.MTI and MTI_CAN_FRAME_TYPE_MASK;

      if CANFrameType = MTI_CAN_CAN then     // the get the AME, AMD, AMR, RID, Error messages $007xx000
        CAN.MTI := CAN.MTI and $0FFFF000
      else
      if CANFrameType <= MTI_CAN_CID0 then
        CAN.MTI := CANFrameType
      else
      if (CANFrameType >= MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY) and (CANFrameType <= MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END) then
      begin
        CAN.DestAlias := (CAN.MTI and $00FFF000) shr 12;
        CAN.MTI := CAN.MTI and $0F000000; // $FF000FFF;
        MTI := MTI_DATAGRAM
      end;
    end else
    begin
      if CAN.MTI and MTI_CAN_ADDRESS_PRESENT = MTI_CAN_ADDRESS_PRESENT then
      begin

        Assert(Length(DataStr) >= 4, 'Malformed message.  Address Present bit set but not enough bytes in the payload');

        ByteStr := DataStr[i_Data] + DataStr[i_Data+1];
        {$IFDEF DWSCRIPT}
        DestHi := TDatatype.HexStrToInt('0x' + ByteStr);
        ByteStr := DataStr[i_Data+2] + DataStr[i_Data+3];
        DestLo := TDatatype.HexStrToInt('0x' + ByteStr);
        {$ELSE}
        DestHi := StrToInt( FormatStrToInt(ByteStr));
        ByteStr := DataStr[i_Data+2] + DataStr[i_Data+3];
        DestLo := StrToInt( FormatStrToInt(ByteStr));
        {$ENDIF}
        Inc(i_Data, 4);            // First 4 in the Data where the MTI, move to the real payload (if it exisits)
        Dec(Len_Data, 4);
        CAN.FramingBits := DestHi and $30;
        CAN.DestAlias := Word(( DestHi shl 8) and $0FFF) or DestLo;
      end else
      begin
        CAN.DestAlias := 0;
        CAN.FramingBits := 0;
      end;
      MTI := Word( (CAN.MTI shr 12) and $0000FFF);
    end;

    FDataCount := 0;
    i_Data_Count :=  len_Data + i_Data;  // Adjust for mobile
    while i_Data < i_Data_Count do
    begin
      ByteStr := DataStr[i_Data] + DataStr[i_Data+1];
      {$IFDEF DWSCRIPT}
      FDataArray[FDataCount] := TDatatype.HexStrToInt('0x' + ByteStr);
      {$ELSE}
      FDataArray[FDataCount] := StrToInt( FormatStrToInt(ByteStr));
      {$ENDIF}
      Inc(i_Data, 2);
      Inc(FDataCount);
    end;
    if IsCAN then
    begin
      case CAN.MTI of
        MTI_CAN_AMD :
          begin
            TempNodeID := NULL_NODE_ID;
            ExtractDataBytesAsNodeID(0, TempNodeID);   // SMS workaround
            FSourceID := TempNodeID;
          end;
      end;
    end;
    Result := True
  end;
end;

function TLccMessage.LoadByLccTcp(var ByteArray: TLccDynamicByteArray): Boolean;
var
  Flags: Word;
  Size, Offset: DWord;
  i, Count: Int64;
begin
  Result := False;
  ZeroFields;

  Flags := (ByteArray[0] shl 8) or ByteArray[1];
  if Flags and OPSTACK_TCP_FLAG_LCC_MESSAGE = OPSTACK_TCP_FLAG_LCC_MESSAGE then
  begin
    Size := (ByteArray[2] shl 16) or (ByteArray[3] shl 8) or (ByteArray[4]);
    // Skip the Originating Node
    // Skip the Time
    MTI := (ByteArray[17] shl 8) or ByteArray[18];
    FSourceID[1] := (ByteArray[19] shl 16) or (ByteArray[20] shl 8) or (ByteArray[21]);
    FSourceID[0] := (ByteArray[22] shl 16) or (ByteArray[23] shl 8) or (ByteArray[24]);

    if MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
    begin
      FDestID[1] := (ByteArray[25] shl 16) or (ByteArray[26] shl 8) or (ByteArray[27]);
      FDestID[0] := (ByteArray[28] shl 16) or (ByteArray[29] shl 8) or (ByteArray[30]);
      Offset := 26;  //  31 - 5
    end else
      Offset := 20;  // 25 - 5
    Count := Size - Offset;
    Inc(Offset, 5);
    i := 0;
    FDataCount := 0;
    while i < Count do
    begin
      FDataArray[i] := ByteArray[Offset + i];
      Inc(FDataCount);
      Inc(i);
    end;
    Result := True;
  end;
end;

function TLccMessage.ConvertToGridConnectStr(Delimiter: String; Details: Boolean): String;
var
  i, iFrameCount, Offset: Integer;
  LocalMTI: DWord;
  FrameCount: Integer;
begin
  Result := '';

  // This can be longer than 6 to 8 bytes
  if (MTI = MTI_DATAGRAM) then
  begin
    // There are 2 ways a Datagram can enter here:
    // 1) A raw Message with a MTI_DATAGRAM MTI that has not been parsed into CAN frames yet
    // 2) A parsed CAN frame that effectively is a piece of a MTI_DATAGRAM but does not need to be reparsed
    if IsCAN then
    begin
      LocalMTI := CAN.MTI or CAN.SourceAlias  or $10000000;
      LocalMTI := LocalMTI or (DWord( CAN.DestAlias) shl 12);
      Result := ':X' + IntToHex(LocalMTI, 8) + 'N';
      for i := 0 to DataCount - 1 do
        Result := Result + IntToHex(DataArray[i], 2);
      Result := Result + ';' + Delimiter
    end else
    begin
      if DataCount < 9 then
      begin
        Result := Result + ':X' + IntToHex(DWord( $1A000000 or (CAN.DestAlias shl 12) or CAN.SourceAlias), 8) + 'N';
        for i := 0 to DataCount - 1 do
          Result := Result + IntToHex(DataArray[i], 2);
        Result := Result + ';'
      end else
      begin
        Offset := 0;
        while Offset < DataCount do
        begin
          if Offset = 0 then
            Result := Result + ':X' + IntToHex(DWord( $1B000000 or (CAN.DestAlias shl 12) or CAN.SourceAlias), 8) + 'N'
          else
          if Offset + 8 >= DataCount then
            Result := Result + ':X' + IntToHex(DWord( $1D000000 or (CAN.DestAlias shl 12) or CAN.SourceAlias), 8) + 'N'
          else
            Result := Result + ':X' + IntToHex(DWord( $1C000000 or (CAN.DestAlias shl 12) or CAN.SourceAlias), 8) + 'N';

          for i := 0 to 7 do
          begin
            Result := Result + IntToHex(DataArray[Offset], 2);
            Inc(Offset);
            if Offset >= DataCount then
              Break;
          end;
          Result := Result + ';' + Delimiter;
        end;
      end;
    end;
  end else
  if MTI = MTI_STREAM_SEND then
  begin
    if IsCAN then
    begin

    end else
    begin

    end;
  end else
  begin
    if IsCAN then
    begin
      LocalMTI := CAN.MTI or CAN.SourceAlias or $10000000;
      case CAN.MTI of
        MTI_CAN_CID0 : LocalMTI := LocalMTI or (SourceID[1] and $00FFF000);
        MTI_CAN_CID1 : LocalMTI := LocalMTI or ((SourceID[1] shl 12) and $00FFF000);
        MTI_CAN_CID2 : LocalMTI := LocalMTI or (SourceID[0] and $00FFF000);
        MTI_CAN_CID3 : LocalMTI := LocalMTI or ((SourceID[0] shl 12) and $00FFF000);
      end;
    end else
      LocalMTI := DWord(( MTI shl 12) or CAN.SourceAlias or MTI_CAN_FRAME_TYPE_GENERAL or $10000000);

    if LocalMTI and MTI_CAN_FRAME_TYPE_MASK > MTI_CAN_FRAME_TYPE_GENERAL then
    begin
      // Datagram or Stream
      LocalMTI := LocalMTI or (DWord( CAN.DestAlias) shl 12);
      Result := ':X' + IntToHex(LocalMTI, 8) + 'N';
      for i := 0 to DataCount - 1 do
        Result := Result + IntToHex(DataArray[i], 2);
    end else
    begin
      if HasDestination then
      begin
        if DataCount > 6 then                                                     // Is it a multiframe message?
        begin
          Result := '';
          Offset := 0;
          FrameCount := DataCount div 6;
          if DataCount mod 6 <> 0 then
            Inc(FrameCount);

          for iFrameCount := 0 to FrameCount - 1 do
          begin
            Result := Result + ':X' + IntToHex(LocalMTI, 8) + 'N';

            if MTI = MTI_SIMPLE_NODE_INFO_REPLY then
            begin
              Result := Result + IntToHex(((CAN.DestAlias shr 8)) and $00FF, 2);
            end else
            begin
              if iFrameCount = 0 then
                Result := Result + IntToHex(((CAN.DestAlias shr 8) or $10) and $00FF, 2)
              else
              if iFrameCount = FrameCount - 1 then
                Result := Result + IntToHex(((CAN.DestAlias shr 8) or $20) and $00FF, 2)
              else
                Result := Result + IntToHex(((CAN.DestAlias shr 8) or $30) and $00FF, 2);
            end;

            Result := Result + IntToHex(CAN.DestAlias and $00FF, 2);

            i := Offset;
            while i < DataCount do
            begin
              Result := Result + IntToHex(DataArray[i], 2);
              Inc(i);
              if (i - Offset) = 6 then
              begin
                Inc(Offset, 6);
                Break;
              end;
            end;

            Result := Result  + ';' + Delimiter       // MultiFrame Message
          end;
        end else
        begin
          Result := ':X' + IntToHex(LocalMTI, 8) + 'N';
          Result := Result + IntToHex((CAN.DestAlias shr 8) or CAN.FramingBits and $00FF, 2);
          Result := Result + IntToHex(CAN.DestAlias and $00FF, 2);
          for i := 0 to DataCount - 1 do
            Result := Result + IntToHex(DataArray[i], 2);
          Result := Result  + ';'                    // Single Frame message
        end;
      end else
      begin
        // Non destination messages can't have multiple frames
        Result := ':X' + IntToHex(LocalMTI, 8) + 'N';
        for i := 0 to DataCount - 1 do
          Result := Result + IntToHex(DataArray[i], 2);
        Result := Result  + ';'
      end;
    end;
  end;
  Result := UpperCase(Result);
end;

function TLccMessage.ConvertToLccTcp(var ByteArray: TLccDynamicByteArray): Boolean;
var
  Flags: Word;
  Size: DWord;
  i, Offset: Integer;
begin
  if IsCAN then
    Result := False
  else begin
    {$IFDEF DWSCRIPT}
    var BinaryData: TBinaryData;
    if HasDestination then
      BinaryData := TBinaryData.Create(DataCount + MAX_HEADER_ONLY_LEN + MAX_LCC_TCP_MESSAGE_PREAMBLE)
    else
      BinaryData := TBinaryData.Create(DataCount + MAX_HEADER_ONLY_LEN + MIN_LCC_TCP_MESSAGE_PREAMBLE);
    ByteArray := BinaryData.ToBytes;
    {$ELSE}
    if HasDestination then
      SetLength(ByteArray, DataCount + MAX_HEADER_ONLY_LEN + MAX_LCC_TCP_MESSAGE_PREAMBLE)
    else
      SetLength(ByteArray, DataCount + MAX_HEADER_ONLY_LEN + MIN_LCC_TCP_MESSAGE_PREAMBLE);
    {$ENDIF}
    Size := Length(ByteArray) - 5;

    Flags := OPSTACK_TCP_FLAG_LCC_MESSAGE;
    ByteArray[0] := _Hi(Flags);
    ByteArray[1] := _Lo(Flags);

    // Size
    ByteArray[2] := _Higher(Size);
    ByteArray[3] := _Hi(Size);
    ByteArray[4] := _Lo(Size);

    // Originating Node
    ByteArray[5] := _Higher(SourceID[1]);
    ByteArray[6] := _Hi(SourceID[1]);
    ByteArray[7] := _Lo(SourceID[1]);
    ByteArray[8] := _Higher(SourceID[0]);
    ByteArray[9] := _Hi(SourceID[0]);
    ByteArray[10] := _Lo(SourceID[0]);

    // Let the socket fill in the monotonicly increasing Capture Time
    {$IFDEF DWSCRIPT}
    ByteArray[11] := 0;
    ByteArray[12] := 0;
    {$ELSE}
    ByteArray[11] := _Highest2(CaptureTime);
    ByteArray[12] := _Highest1(CaptureTime);
    {$ENDIF}
    ByteArray[13] := _Highest(CaptureTime);
    ByteArray[14] := _Higher(CaptureTime);  ;
    ByteArray[15] := _Hi(CaptureTime);
    ByteArray[16] := _Lo(CaptureTime);

    // MTI
    ByteArray[17] := _Hi(MTI);
    ByteArray[18] := _Lo(MTI);

    // Source Node
    ByteArray[19] := _Higher(SourceID[1]);
    ByteArray[20] := _Hi(SourceID[1]);
    ByteArray[21] := _Lo(SourceID[1]);
    ByteArray[22] := _Higher(SourceID[0]);
    ByteArray[23] := _Hi(SourceID[0]);
    ByteArray[24] := _Lo(SourceID[0]);

    if HasDestination then
    begin
      ByteArray[25] := _Higher(DestID[1]);
      ByteArray[26] := _Hi(DestID[1]);
      ByteArray[27] := _Lo(DestID[1]);
      ByteArray[28] := _Higher(DestID[0]);
      ByteArray[29] := _Hi(DestID[0]);
      ByteArray[30] := _Lo(DestID[0]);
      Offset := 31;
    end else
      Offset := 25;

    for i := 0 to DataCount - 1 do
      ByteArray[Offset + i] := DataArray[i];

    Result := True;
  end
end;

class function TLccMessage.ConvertToLccTcpString(var ByteArray: TLccDynamicByteArray): String;
const
  LF = #13#10;
var
  i: Integer;
begin
  Result := '';
  if Length(ByteArray) > 0 then
  begin
    Result := '';
    Result := Result + LF + 'TCP Header: ';
    for i := 0 to MAX_HEADER_ONLY_LEN - 1 do
      Result := Result + ' ' + IntToHex(ByteArray[i], 2);

    Result := Result + LF + 'TCP Message: ';
    for i := MAX_HEADER_ONLY_LEN to Length(ByteArray) - 1 do
      Result := Result + ' ' + IntToHex(ByteArray[i], 2);
  end;
end;

procedure TLccMessage.CopyToTarget(TargetMessage: TLccMessage);
begin
  TargetMessage.FIsCAN := FIsCAN;
  TargetMessage.FMTI := FMTI;
  TargetMessage.FDataArray := FDataArray;
  TargetMessage.FDataCount := FDataCount;
  TargetMessage.FDestID := FDestID;
  TargetMessage.FSourceID := FSourceID;
  TargetMessage.CAN.FDestAlias := CAN.FDestAlias;
  TargetMessage.CAN.FSourceAlias := CAN.FSourceAlias;
  TargetMessage.CAN.FFramingBits := CAN.FFramingBits;
  TargetMessage.CAN.FiTag := 0;
  TargetMessage.CAN.FMTI := CAN.FMTI;
end;

function TLccMessage.DestinationMatchs(TestAliasID: Word; TestNodeID: TNodeID): Boolean;
begin
  if CAN.DestAlias > 0 then
    Result := TestAliasID = CAN.DestAlias
  else
    Result := (TestNodeID[0] = DestID[0]) and (TestNodeID[1] = DestID[1]);
end;

procedure TLccMessage.ZeroFields;
begin
  FIsCAN := False;
  FDataCount := 0;
  FDestID := NULL_NODE_ID;
  FSourceID := NULL_NODE_ID;
  CAN.FSourceAlias := 0;
  CAN.FDestAlias := 0;
  CAN.FFramingBits := 0;
  CAN.FiTag := 0;
  FMTI := 0;
  CAN.FMTI := 0;
end;

procedure TLccMessage.LoadCID(ASourceID: TNodeID; ASourceAlias: Word; ACID: Byte);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  SourceID := ASourceID;
  // The NodeID bits will be added when converting to a gridconnect string
  case ACID of
    0 : CAN.MTI := MTI_CAN_CID0;
    1 : CAN.MTI := MTI_CAN_CID1;
    2 : CAN.MTI := MTI_CAN_CID2;
    3 : CAN.MTI := MTI_CAN_CID3;
  end;
end;

procedure TLccMessage.LoadRID(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  CAN.MTI := MTI_CAN_RID; // or ASourceAlias;
end;

procedure TLccMessage.LoadAMD(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  CAN.MTI := MTI_CAN_AMD;// or ASourceAlias;
  SourceID := ASourceID;
  InsertNodeID(0, ASourceID);
  DataCount := 6;
end;

procedure TLccMessage.LoadAME(ASourceID: TNodeID; ASourceAlias: Word;
  TargetNodeID: TNodeID);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  CAN.MTI := MTI_CAN_AME;// or ASourceAlias;
  SourceID := ASourceID;
  if NullNodeID(TargetNodeID) then
  begin
    DataCount := 0;
  end else
  begin
    InsertNodeID(0, TargetNodeID);
    DataCount := 6;
  end;
end;

procedure TLccMessage.LoadAMR(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  CAN.MTI := MTI_CAN_AMR;// or ASourceAlias;
  SourceID := ASourceID;
  InsertNodeID(0, ASourceID);
  DataCount := 6;
end;

procedure TLccMessage.LoadInitializationComplete(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  InsertNodeID(0, ASourceID);
  DataCount := 6;
  MTI := MTI_INITIALIZATION_COMPLETE;
end;

procedure TLccMessage.LoadOptionalInteractionRejected(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Reason: Word;
  AnMTI: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 4;
  MTI := MTI_OPTIONAL_INTERACTION_REJECTED;
  FDataArray[0] := _Hi(Reason);
  FDataArray[1] := _Lo(Reason);
  FDataArray[2] := _Hi(AnMTI);
  FDataArray[3] := _Lo(AnMTI);
end;


procedure TLccMessage.LoadPCER(ASourceID: TNodeID; ASourceAlias: Word; AnEvent: TEventID);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  InsertEventID(0, AnEvent);
  DataCount := 8;
  MTI := MTI_PC_EVENT_REPORT;
end;

procedure TLccMessage.LoadProducerIdentified(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID; EventState: TEventState);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  InsertEventID(0, Event);
  DataCount := 8;
  case EventState of
    evs_Valid : MTI := MTI_PRODUCER_IDENTIFIED_SET;
    evs_InValid : MTI := MTI_PRODUCER_IDENTIFIED_CLEAR;
    evs_Unknown : MTI := MTI_PRODUCER_IDENTIFIED_UNKNOWN;
  end;
end;

procedure TLccMessage.LoadVerifyNodeIDAddressed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  MTI := MTI_VERIFY_NODE_ID_NUMBER_DEST;
end;

procedure TLccMessage.SetDataArrayIndexer(iIndex: DWord; const Value: Byte);
begin
  FDataArray[iIndex] := Value
end;

procedure TLccMessage.SwapDestAndSourceIDs;
var
  TempNodeID: TNodeID;
  TempAlias: Word;
begin
  TempNodeID := SourceID;
  TempAlias := CAN.SourceAlias;
  SourceID := DestID;
  DestID := TempNodeID;
  CAN.SourceAlias := CAN.DestAlias;
  CAN.DestAlias := TempAlias;
end;

function TLccMessage.TractionExtractActualSpeed: THalfFloat;
begin
  Result := 0;
  Result := DataArray[6];
  Result := Result shl 8;
  Result := Result or DataArray[7];
end;

function TLccMessage.TractionExtractCommandedSpeed: THalfFloat;
begin
  Result := 0;
  Result := DataArray[4];
  Result := Result shl 8;
  Result := Result or DataArray[5];
end;

function TLccMessage.TractionExtractFunctionAddress: LongWord;
begin
  Result := 0;
  Result := DataArray[1];
  Result := Result shl 8;
  Result := Result or DataArray[2];
  Result := Result shl 8;
  Result := Result or DataArray[3];
end;

function TLccMessage.TractionExtractFunctionValue: Word;
begin
  Result := 0;
  Result := DataArray[4];
  Result := Result shl 8;
  Result := Result or DataArray[5];
end;

function TLccMessage.TractionExtractSetSpeed: THalfFloat;
var
  Speed: Word;
begin
  Speed := DataArray[1];
  Speed := Speed shl 8;
  Speed := Speed or DataArray[2];
  Result := THalfFloat( Speed);
end;

function TLccMessage.TractionExtractSpeedStatus: Byte;
begin
  Result := DataArray[3];
end;

function TLccMessage.TractionSearchDecodeSearchString: string;
var
  Nibble, Snippit: Byte;
  i: Integer;
begin
  Result := '';
  for i := 4 to 6 do
  begin
    Nibble := DataArray[i];
    Snippit := Nibble shr 4;
    case Snippit of
       $0F : begin end;
       $00 : Result := Result + '0';
       $01 : Result := Result + '1';
       $02 : Result := Result + '2';
       $03 : Result := Result + '3';
       $04 : Result := Result + '4';
       $05 : Result := Result + '5';
       $06 : Result := Result + '6';
       $07 : Result := Result + '7';
       $08 : Result := Result + '8';
       $09 : Result := Result + '9';
    end;
    Snippit := Nibble and $0F;
    case Snippit of
       $0F : begin end;
       $00 : Result := Result + '0';
       $01 : Result := Result + '1';
       $02 : Result := Result + '2';
       $03 : Result := Result + '3';
       $04 : Result := Result + '4';
       $05 : Result := Result + '5';
       $06 : Result := Result + '6';
       $07 : Result := Result + '7';
       $08 : Result := Result + '8';
       $09 : Result := Result + '9';
    end;
  end;
end;

class function TLccMessage.TractionSearchEncodeSearchString(
  SearchString: string; TrackProtocolFlags: Byte; var SearchData: DWORD
  ): TSearchEncodeStringError;
//
// TrackProtocolFlags are a combonation of TRACTION_SEARCH_TRACK_PROTOCOL_xxx flags
//
var
  i, iFiller: Integer;
begin
  Result := sese_ok;
  if Length(SearchString) > 6 then
  begin
    Result := sese_TooLong;
    Exit;
  end;

  {$IFDEF LCC_MOBILE}
    SearchData := 0;
  for i := 0 to Length(SearchString)-1 do
  begin
    case SearchString[i] of
      '0' : begin SearchData := SearchData or $00 end;
      '1' : begin SearchData := SearchData or $01 end;
      '2' : begin SearchData := SearchData or $02 end;
      '3' : begin SearchData := SearchData or $03 end;
      '4' : begin SearchData := SearchData or $04 end;
      '5' : begin SearchData := SearchData or $05 end;
      '6' : begin SearchData := SearchData or $06 end;
      '7' : begin SearchData := SearchData or $07 end;
      '8' : begin SearchData := SearchData or $08 end;
      '9' : begin SearchData := SearchData or $09 end;
      'F' : begin SearchData := SearchData or $0F end;
      else
        SearchData := 0;
        Result := sese_InvalidCharacters;
        Exit;
    end;
    SearchData := SearchData shl 4;
  end;
  {$ELSE}
  SearchData := 0;
  for i := 1 to Length(SearchString) do
  begin
    case SearchString[i] of
      '0' : begin SearchData := SearchData or $00 end;
      '1' : begin SearchData := SearchData or $01 end;
      '2' : begin SearchData := SearchData or $02 end;
      '3' : begin SearchData := SearchData or $03 end;
      '4' : begin SearchData := SearchData or $04 end;
      '5' : begin SearchData := SearchData or $05 end;
      '6' : begin SearchData := SearchData or $06 end;
      '7' : begin SearchData := SearchData or $07 end;
      '8' : begin SearchData := SearchData or $08 end;
      '9' : begin SearchData := SearchData or $09 end;
      'F' : begin SearchData := SearchData or $0F end;
      else
        SearchData := 0;
        Result := sese_InvalidCharacters;
        Exit;
    end;
    SearchData := SearchData shl 4;
  end;
  {$ENDIF}

  iFiller := (6 - Length(SearchString));

  for i := 0 to iFiller - 1 do
  begin
    SearchData := SearchData or $0F;
    SearchData := SearchData shl 4;
  end;

  SearchData := SearchData shl 4;

   // Above the shl 4 was done on the last byte so it is shifted over ready for the Track Protocol Flags
  SearchData := SearchData or DWORD( TrackProtocolFlags);
end;

function TLccMessage.TractionSearchExtractSearchData: DWORD;
begin
  Result := ExtractDataBytesAsInt(4, 7);
end;

function TLccMessage.TractionSearchIsAddressMatchOnly: Boolean;
begin
  Result := DataArray[7] and TRACTION_SEARCH_TARGET_ADDRESS_MATCH = TRACTION_SEARCH_TARGET_ADDRESS_MATCH
end;

function TLccMessage.TractionSearchIsEvent: Boolean;
begin
  Result := (DataArray[0] = $09) and (DataArray[1] = $00) and (DataArray[2] = $99) and (DataArray[3] = $FF);
end;

function TLccMessage.TractionSearchIsExactMatchOnly: Boolean;
begin
  Result := DataArray[7] and TRACTION_SEARCH_TYPE_EXACT_MATCH = TRACTION_SEARCH_TYPE_EXACT_MATCH;
end;

function TLccMessage.TractionSearchIsForceAllocate: Boolean;
begin
  Result := DataArray[7] and TRACTION_SEARCH_ALLOCATE_FORCE = TRACTION_SEARCH_ALLOCATE_FORCE;
end;

function TLccMessage.TractionSearchIsProtocolAny: Boolean;
begin
  Result := (DataArray[7] and $001F) = 0;
end;

function TLccMessage.TractionSearchIsProtocolDCC(var ForceLongAddress: Boolean; var SpeedStep: TLccDccSpeedStep): Boolean;
begin
  Result := (DataArray[7] and TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_MASK) = TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;
  if Result then
  begin
    case DataArray[7] and TRACTION_SEARCH_PROTCOL_DETAILS_MASK of
      TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_ANY          : SpeedStep := ldssDefault;
      TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_1    : SpeedStep := ldss14;
      TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2    : SpeedStep := ldss28;
      TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2_F8 : SpeedStep := ldss128;
    end;
    ForceLongAddress := (DataArray[7] and TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG) = TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG ;
  end;
end;

function TLccMessage.TractionSearchIsProtocolMarklin(var ProtocolVersion: TLccMarklinProtocolVersion): Boolean;
begin
  Result := (DataArray[7] and (TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_MASK or $04)) = TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN;
  if Result then
  begin
    case DataArray[7] and TRACTION_SEARCH_PROTCOL_DETAILS_MASK of
      TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_ANY          : ProtocolVersion := lmpvDefault;
      TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_1    : ProtocolVersion := lmvpVer1;
      TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2    : ProtocolVersion := lmvpVer2;
      TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2_F8 : ProtocolVersion := lmvpVer2ExtFunction;
    end;
  end;
end;

function TLccMessage.TractionSearchIsProtocolMFX_M4: Boolean;
begin
  Result := (DataArray[7] and TRACTION_SEARCH_TRACK_PROTOCOL_MFX_M4) = TRACTION_SEARCH_TRACK_PROTOCOL_MFX_M4;
end;

function TLccMessage.TractionSearchIsProtocolNativeOpenLcb: Boolean;
begin
  Result := (DataArray[7] and TRACTION_SEARCH_TRACK_PROTOCOL_NATIVE_OPENLCB) = TRACTION_SEARCH_TRACK_PROTOCOL_NATIVE_OPENLCB;
end;

class function TLccMessage.TractionSearchEncodeMarklin(
  ProtocolVersion: TLccMarklinProtocolVersion; ForceAllocate, ExactMatchOnly,
  MachAddressOnly: Boolean): Byte;
begin
  Result := $00;
  if ForceAllocate then Result := Result or TRACTION_SEARCH_ALLOCATE_FORCE;
  if ExactMatchOnly then Result := Result or TRACTION_SEARCH_TYPE_EXACT_MATCH;
  if MachAddressOnly then Result := Result or TRACTION_SEARCH_TARGET_ADDRESS_MATCH;
  Result := Result or $03;   // Odd ball bit for Marklin
  case ProtocolVersion of
    lmpvDefault         : begin end;  // done
    lmvpVer1            : Result := Result or TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_1;
    lmvpVer2            : Result := Result or TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2;
    lmvpVer2ExtFunction : Result := Result or TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2_F8;
  end;
end;

class function TLccMessage.TractionSearchEncodeMFX_M4(ForceAllocate,
  ExactMatchOnly, MatchAddressOnly: Boolean): Byte;
begin
  Result := $00;
  if ForceAllocate then Result := Result or TRACTION_SEARCH_ALLOCATE_FORCE;
  if ExactMatchOnly then Result := Result or TRACTION_SEARCH_TYPE_EXACT_MATCH;
  if MatchAddressOnly then Result := Result or TRACTION_SEARCH_TARGET_ADDRESS_MATCH;
end;

class function TLccMessage.TractionSearchEncodeNativeOpenLcb(
  ForceAllocate, ExactMatchOnly, MatchAddressOnly: Boolean): Byte;
begin
  Result := $00;
  if ForceAllocate then Result := Result or TRACTION_SEARCH_ALLOCATE_FORCE;
  if ExactMatchOnly then Result := Result or TRACTION_SEARCH_TYPE_EXACT_MATCH;
  if MatchAddressOnly then Result := Result or TRACTION_SEARCH_TARGET_ADDRESS_MATCH;
end;

class function TLccMessage.TractionSearchEncodeNMRA(ForceLongAddress: Boolean;
  SpeedStep: TLccDccSpeedStep; ForceAllocate, ExactMatchOnly,
  MatchAddressOnly: Boolean): Byte;
begin
  Result := $00;
  if ForceAllocate then Result := Result or TRACTION_SEARCH_ALLOCATE_FORCE;
  if ExactMatchOnly then Result := Result or TRACTION_SEARCH_TYPE_EXACT_MATCH;
  if MatchAddressOnly then Result := Result or TRACTION_SEARCH_TARGET_ADDRESS_MATCH;
  Result := Result or TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;
  if ForceLongAddress then Result := Result or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG;
  case SpeedStep of
    ldssDefault : begin end;
    ldss14      : Result := Result or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
    ldss28      : Result := Result or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
    ldss128     : Result := Result or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
  end;
end;

procedure TLccMessage.LoadVerifyNodeID(ASourceID: TNodeID; ASourceAlias: Word;
  OptionalTargetNodeID: TNodeID);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  if not NullNodeID(OptionalTargetNodeID) then
  begin
    DataCount := 6;
    InsertNodeID(0, OptionalTargetNodeID);
  end;
  MTI := MTI_VERIFY_NODE_ID_NUMBER;
end;

procedure TLccMessage.LoadProtocolIdentifyInquiry(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  MTI := MTI_PROTOCOL_SUPPORT_INQUIRY;
end;

procedure TLccMessage.LoadProtocolIdentifyReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Flags: TLccSupportedProtocolArray);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  FDataArray[5] := Flags[0];
  FDataArray[4] := Flags[1];
  FDataArray[3] := Flags[2];
  FDataArray[2] := Flags[3];
  FDataArray[1] := Flags[4];
  FDataArray[0] := Flags[5];
  DataCount := 6;
  MTI := MTI_PROTOCOL_SUPPORT_REPLY;
end;

procedure TLccMessage.LoadConsumerIdentified(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID; EventState: TEventState);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  InsertEventID(0, Event);
  DataCount := 8;
  case EventState of
    evs_Valid : MTI := MTI_CONSUMER_IDENTIFIED_SET;
    evs_InValid : MTI := MTI_CONSUMER_IDENTIFIED_CLEAR;
    evs_Unknown : MTI := MTI_CONSUMER_IDENTIFIED_UNKNOWN;
  end;
end;

procedure TLccMessage.LoadConsumerIdentify(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  InsertEventID(0, Event);
  DataCount := 8;
  MTI := MTI_CONSUMER_IDENTIFY;
end;

procedure TLccMessage.LoadDatagram(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadProducerIdentify(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  InsertEventID(0, Event);
  DataCount := 8;
  MTI := MTI_PRODUCER_IDENDIFY;
end;

procedure TLccMessage.LoadIdentifyEventsAddressed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  MTI := MTI_EVENTS_IDENTIFY_DEST;
end;

procedure TLccMessage.LoadIdentifyEvents(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  DestID := NULL_NODE_ID;
  MTI := MTI_EVENTS_IDENTIFY;
end;

procedure TLccMessage.LoadTractionSetSpeed(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ASpeed: single);
var
  HalfFloatSpeed: THalfFloat;
begin
  HalfFloatSpeed := FloatToHalf(ASpeed);
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 3;
  FDataArray[0] := TRACTION_SPEED_DIR;
  FDataArray[1] := Hi( HalfFloatSpeed);
  FDataArray[2] := Lo( HalfFloatSpeed);
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadVerifiedNodeID(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  InsertNodeID(0, ASourceID);
  DataCount := 6;
  MTI := MTI_VERIFIED_NODE_ID_NUMBER;
end;

procedure TLccMessage.LoadTractionSetFunction(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AnAddress: DWord;
  AValue: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 6;
  FDataArray[0] := TRACTION_FUNCTION;
  FDataArray[1] := Byte((AnAddress shr 16) and $0000FF);
  FDataArray[2] := Byte((AnAddress shr 8) and $0000FF);
  FDataArray[3] := Byte(AnAddress and $0000FF);
  FDataArray[4] := Hi(AValue);
  FDataArray[5] := Lo(AValue);
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionEStop(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 1;
  FDataArray[0] := TRACTION_E_STOP;
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionQuerySpeed(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 1;
  FDataArray[0] := TRACTION_QUERY_SPEED;
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionQuerySpeedReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; SetSpeed: THalfFloat;
  Status: Byte; CommandedSpeed, ActualSpeed: THalfFloat);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 8;
  FDataArray[0] := TRACTION_QUERY_SPEED_REPLY;
  FDataArray[1] := Hi(SetSpeed);
  FDataArray[2] := Lo(SetSpeed);
  FDataArray[3] := Status;
  FDataArray[4] := Hi(CommandedSpeed);
  FDataArray[5] := Lo(CommandedSpeed);
  FDataArray[6] := Hi(ActualSpeed);
  FDataArray[7] := Lo(ActualSpeed);
  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadTractionSearch(ASourceID: TNodeID;
  ASourceAlias: Word; SearchData: DWORD);
var
  i: Integer;
begin
  for i := 0 to LCC_BYTE_COUNT - 1 do
    FDataArray[i] := 0;
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  FDataArray[0] := $09;
  FDataArray[1] := $00;
  FDataArray[2] := $99;
  FDataArray[3] := $FF;
  InsertDWordAsDataBytes(SearchData, 4);
  DataCount := 8;
  MTI := MTI_PRODUCER_IDENDIFY;
end;

procedure TLccMessage.LoadTractionQueryFunction(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Address: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 4;
  FDataArray[0] := TRACTION_QUERY_FUNCTION;
  FDataArray[1] := Byte((Address shr 16) and $0000FF);
  FDataArray[2] := Byte((Address shr 8) and $0000FF);
  FDataArray[3] := Byte(Address and $0000FF);
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionQueryFunctionReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Address: Word;
  Value: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 6;
  FDataArray[0] := TRACTION_QUERY_FUNCTION_REPLY;
  FDataArray[1] := Byte((Address shr 16) and $0000FF);
  FDataArray[2] := Byte((Address shr 8) and $0000FF);
  FDataArray[3] := Byte(Address and $00FF);
  FDataArray[4] := hi(Value);
  FDataArray[5] := lo(Value);
  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadTractionControllerAssign(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AControllerNodeID: TNodeID;
  AControllerAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  if AControllerAlias <> 0 then
  begin
    DataCount := 11;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, AControllerNodeID);
    FDataArray[9] := Hi( AControllerAlias);
    FDataArray[10] := Lo( AControllerAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN;
    FDataArray[2] := 0;
    InsertNodeID(3, AControllerNodeID);
  end;
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionControllerAssignReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AResult: Byte);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;

  DataCount := 3;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG_REPLY;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY;
  FDataArray[2] := AResult;

  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadTractionControllerRelease(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ANodeID: TNodeID;
  AnAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  if AnAlias <> 0 then
  begin
    DataCount := 11;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_RELEASE;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, ANodeID);
    FDataArray[9] := Hi( AnAlias);
    FDataArray[10] := Lo( AnAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_RELEASE;
    FDataArray[2] := 0;
    InsertNodeID(3, ANodeID);
  end;
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionControllerQuery(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 2;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_QUERY;
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionControllerQueryReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word;
  AControllerID: TNodeID; AControllerAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;

  if AControllerAlias <> 0 then
  begin
    DataCount := 11;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_QUERY_REPLY;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, AControllerID);
    FDataArray[9] := Hi( AControllerAlias);
    FDataArray[10] := Lo( AControllerAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_QUERY_REPLY;
    FDataArray[2] := 0;
    InsertNodeID(3, AControllerID);
  end;

  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadTractionControllerChangingNotify(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AControllerNodeID: TNodeID;
  AControllerAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  if AControllerAlias <> 0 then
  begin
    DataCount := 11;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, AControllerNodeID);
    FDataArray[9] := Hi( AControllerAlias);
    FDataArray[10] := Lo( AControllerAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY;
    FDataArray[2] := 0;
    InsertNodeID(3, AControllerNodeID);
  end;
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionControllerChangedReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Allow: Boolean);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  MTI := MTI_TRACTION_REPLY;
  DataCount := 3;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY;
  if Allow then
    FDataArray[2] := 0
  else
    FDataArray[2] := TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER
end;

procedure TLccMessage.LoadTractionListenerAttach(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word;
  AListenerNodeID: TNodeID);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 9;
  FDataArray[0] := TRACTION_LISTENER;
  FDataArray[1] := TRACTION_LISTENER_ATTACH;
  FDataArray[2] := 0;
  InsertNodeID(3, AListenerNodeID);
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionListenerAttachReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word;
  AListenerNodeID: TNodeID; ReplyCode: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 10;
  FDataArray[0] := TRACTION_LISTENER;
  FDataArray[1] := TRACTION_LISTENER_ATTACH;
  FDataArray[2] := 0;
  InsertNodeID(3, AListenerNodeID);
  FDataArray[8] := Hi(ReplyCode);
  FDataArray[9] := Lo(ReplyCode);
  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadTractionListenerDetach(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AListenerNodeID: TNodeID);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 9;
  FDataArray[0] := TRACTION_LISTENER;
  FDataArray[1] := TRACTION_LISTENER_DETACH;
  FDataArray[2] := 0;
  InsertNodeID(3, AListenerNodeID);
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionListenerDetachReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word;
  AListenerNodeID: TNodeID; ReplyCode: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 10;
  FDataArray[0] := TRACTION_LISTENER;
  FDataArray[1] := TRACTION_LISTENER_DETACH;
  FDataArray[2] := 0;
  InsertNodeID(3, AListenerNodeID);
  FDataArray[8] := Hi(ReplyCode);
  FDataArray[9] := Lo(ReplyCode);
  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadTractionListenerQuery(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AListenerNodeID: TNodeID);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 9;
  FDataArray[0] := TRACTION_LISTENER;
  FDataArray[1] := TRACTION_LISTENER_QUERY;
  FDataArray[2] := 0;
  InsertNodeID(3, AListenerNodeID);
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionListenerQueryReply_NoInfo(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 0;
  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadTractionListenerQueryReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ListenerCount,
  ListenerNodeIndex: Byte; AListenerNodeID: TNodeID; Flags: Byte);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  if NullNodeID(AListenerNodeID) then
  begin
    DataCount := 3;
    FDataArray[0] := TRACTION_LISTENER;
    FDataArray[1] := TRACTION_LISTENER_QUERY;
    FDataArray[2] := ListenerCount;
  end else
  begin
    DataCount := 10;
    FDataArray[0] := TRACTION_LISTENER;
    FDataArray[1] := TRACTION_LISTENER_QUERY;
    FDataArray[2] := ListenerCount;
    FDataArray[3] := ListenerNodeIndex;
    FDataArray[4] := Flags;
    InsertNodeID(5, AListenerNodeID);
  end;
  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadTractionManage(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Reserve: Boolean);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 2;
  FDataArray[0] := TRACTION_MANAGE;
  if Reserve then
    FDataArray[1] := TRACTION_MANAGE_RESERVE
  else
    FDataArray[1] := TRACTION_MANAGE_RELEASE;
  MTI := MTI_TRACTION_REQUEST;
end;

procedure TLccMessage.LoadTractionManageReply(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Accepted: Boolean);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 3;
  FDataArray[0] := TRACTION_MANAGE;
  FDataArray[1] := TRACTION_RESERVE_REPLY;
  if Accepted then
    FDataArray[2] := S_OK
  else
    FDataArray[2] := S_FALSE;
  MTI := MTI_TRACTION_REPLY;
end;

procedure TLccMessage.LoadSimpleTrainNodeIdentInfoRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  MTI := MTI_TRACTION_SIMPLE_TRAIN_INFO_REQUEST;
end;

procedure TLccMessage.LoadSimpleNodeIdentInfoReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; SimplePackedArray: TLccDynamicByteArray);
var
  i: Integer;
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  for i := 0 to Length(SimplePackedArray) - 1 do
    FDataArray[i] := SimplePackedArray[i];
  DataCount := Length(SimplePackedArray);
  MTI := MTI_SIMPLE_NODE_INFO_REPLY;
end;

procedure TLccMessage.LoadSimpleNodeIdentInfoRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 0;
  MTI := MTI_SIMPLE_NODE_INFO_REQUEST;
end;

procedure TLccMessage.LoadFDIRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 8;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_READ;
  FDataArray[2] := 0;
  FDataArray[3] := 0;
  FDataArray[4] := 0;
  FDataArray[5] := 0;
  FDataArray[6] := MSI_TRACTION_FDI;
  FDataArray[7] := 64;                     // Read until the end.....
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadFunctionConfigurationRead(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; FunctionAddress: DWord; Count: Integer);
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 8;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_READ;
  FDataArray[2] := _Highest(FunctionAddress);
  FDataArray[3] := _Higher(FunctionAddress);  // F0..F28
  FDataArray[4] := _Hi(FunctionAddress);
  FDataArray[5] := _Lo(FunctionAddress);
  FDataArray[6] := MSI_TRACTION_FUNCTION_CONFIG;
  FDataArray[7] := Count*2;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadFunctionConfigurationWrite(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word;
  FunctionAddress: DWord; Count: Integer; Functions: TFunctionStatesArray);
var
  i: Integer;
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_WRITE;
  FDataArray[2] := _Highest(FunctionAddress);
  FDataArray[3] := _Higher(FunctionAddress);  // F0..F28
  FDataArray[4] := _Hi(FunctionAddress);
  FDataArray[5] := _Lo(FunctionAddress);
  FDataArray[6] := MSI_TRACTION_FUNCTION_CONFIG;
  DataCount := 7;
  for i := 0 to Count - 1 do
  begin
    FDataArray[DataCount] := Hi(Functions[i]);
    Inc(FDataCount);
    FDataArray[DataCount] := Lo(Functions[i]);
    Inc(FDataCount);
  end;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadCDIRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 8;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_READ;
  FDataArray[2] := 0;
  FDataArray[3] := 0;
  FDataArray[4] := 0;
  FDataArray[5] := 0;
  FDataArray[6] := MSI_CDI;
  FDataArray[7] := 64;                     // Read until the end.....
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadDatagramAck(ASourceID: TNodeID; ASourceAlias: Word;
  ADestID: TNodeID; ADestAlias: Word; Ok: Boolean; ReplyPending: Boolean;
  TimeOutValueN: Byte);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 1;
  if ReplyPending then
    DataArrayIndexer[0] := $80 or (TimeoutValueN and $0F)
  else
    DataArrayIndexer[0] := 0;
  if Ok then
    MTI := MTI_DATAGRAM_OK_REPLY
  else
    MTI := MTI_DATAGRAM_REJECTED_REPLY;
end;

procedure TLccMessage.LoadDatagramRejected(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Reason: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 2;
  MTI := MTI_DATAGRAM_REJECTED_REPLY;
  FDataArray[0] := _Hi(Reason);
  FDataArray[1] := _Lo(Reason);
end;

procedure TLccMessage.LoadConfigMemAddressSpaceInfo(ASourceID: TNodeID; ASourceAlias: Word;
  ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_OP_GET_ADD_SPACE_INFO;
  FDataArray[2] := AddressSpace;
  FDataCount := 3;
  FMTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadConfigMemOptions(ASourceID: TNodeID; ASourceAlias: Word;
  ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_OP_GET_CONFIG;
  FDataCount := 2;
  FMTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadConfigMemRead(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte;
  ConfigMemAddress: DWord; ConfigMemSize: Byte);
begin
  if ConfigMemSize > 64 then
    ConfigMemSize := 64;

  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  if AddressSpace < MSI_CONFIG then
  begin
    FDataArray[1] := MCP_READ;
    FDataArray[2] := _Highest(ConfigMemAddress);
    FDataArray[3] := _Higher(ConfigMemAddress);
    FDataArray[4] := _Hi(ConfigMemAddress);
    FDataArray[5] := _Lo(ConfigMemAddress);
    FDataArray[6] := AddressSpace;
    FDataArray[7] := ConfigMemSize;
    DataCount := 8;
  end else
  begin
    case AddressSpace of
      MSI_CDI : FDataArray[1] := MCP_READ or MCP_CDI;
      MSI_ALL : FDataArray[1] := MCP_READ or MCP_ALL;
      MSI_CONFIG : FDataArray[1] := MCP_READ or MCP_CONFIGURATION;
    end;
    FDataArray[2] := _Highest(ConfigMemAddress);
    FDataArray[3] := _Higher(ConfigMemAddress);
    FDataArray[4] := _Hi(ConfigMemAddress);
    FDataArray[5] := _Lo(ConfigMemAddress);
    FDataArray[6] := ConfigMemSize;
    DataCount := 7;
  end;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadConfigMemWriteArray(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte;
  ConfigMemAddress: DWord; ArraySize: Integer; AnArray: array of Byte);
var
  i, iDatagram, DatagramCount, DatagramLength, iArrayPos: Integer;
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....

  if ArraySize mod 64 = 0 then
    DatagramCount := ArraySize div 64
  else
    DatagramCount := (ArraySize div 64) + 1;

  for iDatagram := 0 to DatagramCount - 1 do
  begin
    DatagramLength := ArraySize;
    if DatagramLength > 64 then
      DatagramLength := 64;

    ZeroFields;
    SourceID := ASourceID;
    DestID := ADestID;
    CAN.SourceAlias := ASourceAlias;
    CAN.DestAlias := ADestAlias;
    FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
    if AddressSpace < MSI_CONFIG then
    begin
      FDataArray[1] := MCP_WRITE;
      FDataArray[2] := _Highest(ConfigMemAddress);
      FDataArray[3] := _Higher(ConfigMemAddress);
      FDataArray[4] := _Hi(ConfigMemAddress);
      FDataArray[5] := _Lo(ConfigMemAddress);
      FDataArray[6] := AddressSpace;
      DataCount := 7;
    end else
    begin
      case AddressSpace of
        MSI_CDI : FDataArray[1] := MCP_WRITE or MCP_CDI;
        MSI_ALL : FDataArray[1] := MCP_WRITE or MCP_ALL;
        MSI_CONFIG : FDataArray[1] := MCP_WRITE or MCP_CONFIGURATION;
      end;
      FDataArray[2] := _Highest(ConfigMemAddress);
      FDataArray[3] := _Higher(ConfigMemAddress);
      FDataArray[4] := _Hi(ConfigMemAddress);
      FDataArray[5] := _Lo(ConfigMemAddress);
      DataCount := 6;
    end;

    iArrayPos := 0;
    for i := 0 to DatagramLength - 1 do
    begin
      FDataArray[DataCount] := AnArray[iArrayPos];
      Inc(FDataCount);
      Inc(iArrayPos);
      Inc(ConfigMemAddress);
    end;

    MTI := MTI_DATAGRAM;
  end;
end;

procedure TLccMessage.LoadConfigMemWriteInteger(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte;
  ConfigMemAddress: DWord; IntegerSize: Byte; DataInteger: Integer);
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  if AddressSpace < MSI_CONFIG then
  begin
    FDataArray[1] := MCP_WRITE;
    FDataArray[2] := _Highest(ConfigMemAddress);
    FDataArray[3] := _Higher(ConfigMemAddress);
    FDataArray[4] := _Hi(ConfigMemAddress);
    FDataArray[5] := _Lo(ConfigMemAddress);
    FDataArray[6] := AddressSpace;
    DataCount := 7;
  end else
  begin
    case AddressSpace of
      MSI_CDI : FDataArray[1] := MCP_WRITE or MCP_CDI;
      MSI_ALL : FDataArray[1] := MCP_WRITE or MCP_ALL;
      MSI_CONFIG : FDataArray[1] := MCP_WRITE or MCP_CONFIGURATION;
    end;
    FDataArray[2] := _Highest(ConfigMemAddress);
    FDataArray[3] := _Higher(ConfigMemAddress);
    FDataArray[4] := _Hi(ConfigMemAddress);
    FDataArray[5] := _Lo(ConfigMemAddress);
    DataCount := 6;
  end;

  case IntegerSize of
    1 : begin
          FDataArray[DataCount] := _Lo(DataInteger);
          Inc(FDataCount, 1);
        end;
    2 : begin
          InsertWordAsDataBytes(Word( DataInteger), DataCount);
          Inc(FDataCount, 2);
        end;
    4 : begin
          InsertDWordAsDataBytes(DWord( DataInteger), DataCount);
          Inc(FDataCount, 4);
        end;
  end;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadConfigMemWriteString(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte;
  ConfigMemAddress: DWord; AString: String);
var
  i, iDatagram, DatagramCount, iStringPos, DatagramLength, StrLength: Integer;
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....

  iStringPos := 1;         // 1 indexed

  StrLength := Length(AString) + 1;  // Include the Null

  if StrLength mod 64 = 0 then
    DatagramCount := StrLength div 64
  else
    DatagramCount := (StrLength div 64) + 1;

  for iDatagram := 0 to DatagramCount - 1 do
  begin
    DatagramLength := StrLength - (iStringPos - 1);
    if DatagramLength > 64 then
      DatagramLength := 64;

    ZeroFields;
    SourceID := ASourceID;
    DestID := ADestID;
    CAN.SourceAlias := ASourceAlias;
    CAN.DestAlias := ADestAlias;
    FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
    if AddressSpace < MSI_CONFIG then
    begin
      FDataArray[1] := MCP_WRITE;
      FDataArray[2] := _Highest(ConfigMemAddress);
      FDataArray[3] := _Higher(ConfigMemAddress);
      FDataArray[4] := _Hi(ConfigMemAddress);
      FDataArray[5] := _Lo(ConfigMemAddress);
      FDataArray[6] := AddressSpace;
      DataCount := 7;
    end else
    begin
      case AddressSpace of
        MSI_CDI : FDataArray[1] := MCP_WRITE or MCP_CDI;
        MSI_ALL : FDataArray[1] := MCP_WRITE or MCP_ALL;
        MSI_CONFIG : FDataArray[1] := MCP_WRITE or MCP_CONFIGURATION;
      end;
      FDataArray[2] := _Highest(ConfigMemAddress);
      FDataArray[3] := _Higher(ConfigMemAddress);
      FDataArray[4] := _Hi(ConfigMemAddress);
      FDataArray[5] := _Lo(ConfigMemAddress);
      DataCount := 6;
    end;

    if DatagramLength = 1 then
    begin
      FDataArray[DataCount] := Ord(#0);
      Inc(FDataCount);
    end else
    begin
      for i := 0 to DatagramLength - 1 do
      begin
        FDataArray[DataCount] := Ord( AString[iStringPos]);
        Inc(FDataCount);
        Inc(iStringPos);
        Inc(ConfigMemAddress);
      end;
    end;
    MTI := MTI_DATAGRAM;
  end;
end;

initialization
  CaptureTime := 0;

finalization

end.
