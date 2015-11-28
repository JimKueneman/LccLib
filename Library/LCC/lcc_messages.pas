unit lcc_messages;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP}
    ExtCtrls,
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF FPC}
  Types,
  {$ENDIF}
  lcc_defines,
  lcc_math_float16,
  lcc_utilities,
  lcc_threadedcirculararray,
  strutils,
  lcc_compiler_types;

const
  CAN_BYTE_COUNT = 8;
  LCC_BYTE_COUNT = 1024;       // This is longest data structure defined in Lcc

type
  TCANByteArray = array[0..CAN_BYTE_COUNT-1] of Byte;
  TLccByteArray = array[0..LCC_BYTE_COUNT-1] of Byte;

  TSimpleNodeInfoPacked = array of Byte;

TLccMessage = class;  // forward

TOnMessageEvent = procedure(Sender: TObject; LccMessage: TLccMessage) of object;

TConnectionState = (ccsClientConnecting, ccsClientConnected, ccsClientDisconnecting, ccsClientDisconnected,
                    ccsListenerConnecting, ccsListenerConnected, ccsListenerDisconnecting, ccsListenerDisconnected,
                    ccsListenerClientConnecting, ccsListenerClientConnected, ccsListenerClientDisconnecting, ccsListenerClientDisconnected,
                    ccsPortConnecting, ccsPortConnected, ccsPortDisconnecting, ccsPortDisconnected);

TFunctionStatesArray = array[0..28] of Word;

TLccCdiParserBase = class(TComponent)
public
  procedure SetNodeManager(ANodeManager: TObject); virtual; abstract;
  procedure DoConfigMemReadReply(ANode: TObject); virtual; abstract;
  procedure DoConfigMemWriteReply(ANode: TObject); virtual; abstract;
  procedure NotifyLccNodeDestroy(LccNode: TObject); virtual; abstract;
end;

// TLccMessage encapsulates the information needed for full MTI messages.  The
// data bytes are limited only by the longest OpenLCB data structure (TCP frame
// current set to 1024 bytes).  It allows easy peaking and poking into the array
// to pull or push OpenLCB data strutures from/to the byte array as well as
// hold the raw data for the MTI and nodes involved in the message.  This
// class does not handle CAN only messages

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
  property MTI: DWord read FMTI write FMTI;
  property DestAlias: Word read FDestAlias write FDestAlias;
  property FramingBits: Byte read FFramingBits write FFramingBits;            // Bottom 2 bits, upper nibble
  property SourceAlias: Word read FSourceAlias write FSourceAlias;
end;



{ TLccMessage }

TLccMessage = class
private
  FAbandonTimeout: Integer;
  FIsCAN: Boolean;                                                          // True if only The CAN_Message MTI is valid
  FCAN: TLccCANMessage;
  FDataArray: TLccByteArray;
  FDataCount: Integer;
  FDestID: TNodeID;
  FMTI: Word;
  FRetryAttempts: Integer;
  FUserValid: Boolean;
  function GetHasDestination: Boolean;
  function GetHasDestNodeID: Boolean;
  function GetHasSourceNodeID: Boolean;
  function GetIsDatagram: Boolean;
  function GetIsStream: Boolean;
  function GetDataArrayIndexer(iIndex: DWord): Byte;

  procedure SetDataArrayIndexer(iIndex: DWord; const Value: Byte);protected
  FSourceID: TNodeID;
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
  property IsDatagram: Boolean read GetIsDatagram;
  property IsStream: Boolean read GetIsStream;
  property MTI: Word read FMTI write FMTI;
  property RetryAttempts: Integer read FRetryAttempts write FRetryAttempts;
  property SourceID: TNodeID read FSourceID write FSourceID;
  property UserValid: Boolean read FUserValid write FUserValid;

  constructor Create;
  destructor Destroy; override;

  procedure AppendDataArray(LccMessage: TLccMessage);
  function AppendDataArrayAsString(LccMessage: TLccMessage; LastNull: Byte): Boolean;
  function Clone: TLccMessage;
  procedure InsertNodeID(StartIndex: Integer; var ANodeID: TNodeID);
  procedure InsertEventID(StartIndex: Integer; var AnEventID: TEventID);
  procedure InsertDWordAsDataBytes(DoubleWord: DWord; StartByteIndex: Integer);
  procedure InsertWordAsDataBytes(AWord: DWord; StartByteIndex: Integer);
  function ExtractDataBytesAsEventID(StartIndex: Integer): PEventID;
  function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
  function ExtractDataBytesAsNodeID(StartIndex: Integer; var ANodeID: TNodeID): PNodeID;
  function ExtractDataBytesAsString(StartIndex, Count: Integer): String;

  function LoadByGridConnectStr(GridConnectStr: String): Boolean;
  function LoadByLccTcp(var ByteArray: TDynamicByteArray): Boolean;
  function ConvertToGridConnectStr(Delimiter: String): String;
  function ConvertToLccTcp(var ByteArray: TDynamicByteArray): Boolean;
  procedure Copy(TargetMessage: TLccMessage);
  class function ConvertToLccTcpString(var ByteArray: TDynamicByteArray): String;
  procedure ZeroFields;

  // CAN
  procedure LoadCID(ASourceID: TNodeID; ASourceAlias: Word; ACID: Byte);
  procedure LoadRID(ASourceAlias: Word);
  procedure LoadAMD(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadAMR(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadAME(ASourceID: TNodeID; ASourceAlias: Word);
  // Basic
  procedure LoadInitializationComplete(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadVerifyNodeIDAddressed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadVerifyNodeID(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadVerifiedNodeID(ASourceID: TNodeID; ASourceAlias: Word);
  // Protocol Support (PIP)
  procedure LoadProtocolIdentifyInquiry(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadProtocolIdentifyReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Flags: QWord);
  // Event Exchange
  procedure LoadConsumerIdentify(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID);
  procedure LoadConsumerIdentified(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID; EventState: TEventState);
  procedure LoadProducerIdentify(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID);
  procedure LoadProducerIdentified(ASourceID: TNodeID; ASourceAlias: Word; var Event: TEventID; EventState: TEventState);
  procedure LoadIdentifyEventsAddressed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadIdentifyEvents(ASourceID: TNodeID; ASourceAlias: Word);
  procedure LoadPCER(ASourceID: TNodeID; ASourceAlias: Word; AnEvent: PEventID);
  // Traction Control
  procedure LoadTractionSetSpeed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ASpeed: THalfFloat);
  procedure LoadTractionSetFunction(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AnAddress: DWord; AValue: Word);
  procedure LoadTractionEStop(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadTractionQuerySpeed(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadTractionQueryFunction(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadTractionControllerAssign(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ANodeID: TNodeID; AnAlias: Word);
  procedure LoadTractionControllerRelease(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ANodeID: TNodeID; AnAlias: Word);
  procedure LoadTractionControllerQuery(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  procedure LoadTractionControllerChangeNotify(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ANodeID: TNodeID; AnAlias: Word);
  procedure LoadTractionControllerChangeNotifyReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Allow: Boolean);
  procedure LoadTractionConsistAttach(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ANodeID: TNodeID; AnAlias: Word);
  procedure LoadTractionConsistDetach(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ANodeID: TNodeID; AnAlias: Word);
  procedure LoadTractionConsistQuery(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ANodeID: TNodeID; AnAlias: Word);
  procedure LoadTractionManage(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Reserve: Boolean);
  // Remote Button

  // Traction Identification (STNIP)
  procedure LoadSimpleTrainNodeIdentInfoRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
  // Node Ident (SNIP)
  procedure LoadSimpleNodeIdentInfoReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; SimplePackedArray: TSimpleNodeInfoPacked);
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

implementation

var
  CaptureTime: QWord;

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

function TLccMessage.GetIsDatagram: Boolean;
begin
  Result := MTI = MTI_DATAGRAM;
end;

function TLccMessage.GetIsStream: Boolean;
begin
  Result := MTI = MTI_STREAM_SEND;
end;

constructor TLccMessage.Create;
begin
  inherited Create;
  CAN := TLccCANMessage.Create;
end;

destructor TLccMessage.Destroy;
begin
  FreeAndNil(FCAN);
  inherited Destroy;
end;

function TLccMessage.ExtractDataBytesAsEventID(StartIndex: Integer): PEventID;
begin
  Result := @DataArray[StartIndex];
end;

function TLccMessage.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
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

function TLccMessage.ExtractDataBytesAsNodeID(StartIndex: Integer; var ANodeID: TNodeID): PNodeID;
begin
  Result := @ANodeID;
  ANodeID[1] := (DataArray[StartIndex] shl 16) or (DataArray[StartIndex+1] shl 8) or DataArray[StartIndex+2];
  ANodeID[0] := (DataArray[StartIndex+3] shl 16) or (DataArray[StartIndex+4] shl 8) or DataArray[StartIndex+5];
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

function TLccMessage.LoadByGridConnectStr(GridConnectStr: String): Boolean;
var
  x, n, SemiColon, i, j: Integer;
  ByteStr: String;
  DestHi, DestLo: Byte;
  ZeroIndex: Boolean;
begin
  Result := False;
  if GridConnectStr <> '' then
  begin
    ZeroFields;
    {$IFDEF FPC}
    ZeroIndex := False;
    {$ELSE}
    ZeroIndex := Low(GridConnectStr) = 0;
    {$ENDIF}
    GridConnectStr := UpperCase(GridConnectStr);
    x := Pos('X', GridConnectStr);                                              // Find were the "X" is in the string
    if ZeroIndex then Dec(x);
    if x > 0 then
    begin
      n := PosEx('N', GridConnectStr, x);                                       // Find where the "N" is in the string
      if ZeroIndex then Dec(n);
      if n > 0 then
      begin
        GridConnectStr[n] := #0;                                                // Set the "N" to a null to create a null string of the MTI
        Inc(n);                                                                 // Move just pass where the "N" was
        SemiColon := PosEx(';', GridConnectStr, n);                             // Look for the terminating ";"
        if ZeroIndex then Dec(SemiColon);
        if SemiColon > 0 then
        begin
          ByteStr := '';
          j := x+1;
          while GridConnectStr[j] <> #0 do
          begin
            ByteStr := ByteStr + GridConnectStr[j];
            Inc(j);
          end;
          CAN.MTI := StrToInt('$' + ByteStr);              // Convert the string MTI into a number  ;
          CAN.SourceAlias := Word( CAN.MTI and $00000FFF);                      // Grab the Source Alias before it is stripped off
          CAN.MTI := CAN.MTI and not $10000000;                                 // Strip off the reserved bits
          CAN.MTI := CAN.MTI and $FFFFF000;                                     // Strip off the Source Alias
          // Was this an OpenLCB or CAN specific message? This covers special multiFrame LccMessage and CAN layer specific messages
          IsCAN := (CAN.MTI and $07000000 <> $01000000);

          // Extract the General OpenLCB message if possible
          if IsCAN then                                                       // IsCAN means CAN Frames OR OpenLCB message that are only on CAN (Datagrams frames and Stream Send)
          begin
            if CAN.MTI and MTI_CAN_FRAME_TYPE_MASK < MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY then
            begin
              CAN.MTI := CAN.MTI and MTI_CAN_CID_MASK;
              MTI := 0;
            end
            else begin
              CAN.DestAlias := (CAN.MTI and $00FFF000) shr 12;
              CAN.MTI := CAN.MTI and $0F000000; // $FF000FFF;
              MTI := MTI_DATAGRAM
            end;
          end else
          begin
            if CAN.MTI and MTI_CAN_ADDRESS_PRESENT = MTI_CAN_ADDRESS_PRESENT then
            begin
              ByteStr := GridConnectStr[n] + GridConnectStr[n+1];
              DestHi := StrToInt('$' + ByteStr);
              ByteStr := GridConnectStr[n+2] + GridConnectStr[n+3];
              DestLo := StrToInt('$' + ByteStr);
              CAN.FramingBits := DestHi and $30;
              CAN.DestAlias := Word(( DestHi shl 8) and $0FFF) or DestLo;
              Inc(n, 4);
            end else
            begin
              CAN.DestAlias := 0;
              CAN.FramingBits := 0;
            end;
            MTI := Word( (CAN.MTI shr 12) and $0000FFF);
          end;

  //      FForwardingBitNotSet := GridConnect_MTI and $10000000 = $00000000;    // Check if the Forwarding Bit was set
  //      FUnimplementedBitsSet := GridConnect_MTI and $E0000000 <> $00000000;  // Check to see the state of the unimplemented bits


          FDataCount := 0;                                                       // Convert the CAN payload bytes into number
          i := n;
          while i < SemiColon do
          begin
            ByteStr := GridConnectStr[i] + GridConnectStr[i+1];
            FDataArray[FDataCount] := StrToInt('$' + ByteStr);
            Inc(i, 2);
            Inc(FDataCount);
          end;
        end;
        Result := True
      end
    end
  end;
end;

function TLccMessage.LoadByLccTcp(var ByteArray: TDynamicByteArray): Boolean;
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

function TLccMessage.ConvertToGridConnectStr(Delimiter: String): String;
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
      LocalMTI := CAN.MTI or CAN.SourceAlias or $10000000
    else
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
end;

function TLccMessage.ConvertToLccTcp(var ByteArray: TDynamicByteArray): Boolean;
var
  Flags: Word;
  Size: DWord;
  i, Offset: Integer;
begin
  if IsCAN then
    Result := False
  else begin
    if HasDestination then
      SetLength(ByteArray, DataCount + MAX_HEADER_ONLY_LEN + MAX_LCC_TCP_MESSAGE_PREAMBLE)
    else
      SetLength(ByteArray, DataCount + MAX_HEADER_ONLY_LEN + MIN_LCC_TCP_MESSAGE_PREAMBLE);
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
    ByteArray[11] := _Highest2(CaptureTime);
    ByteArray[12] := _Highest1(CaptureTime);
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

class function TLccMessage.ConvertToLccTcpString(var ByteArray: TDynamicByteArray): String;
const
  LF = #13#10;
var
  i: Integer;
begin
  Result := '';
  Result := Result + LF + 'TCP Header: ';
  for i := 0 to MAX_HEADER_ONLY_LEN - 1 do
    Result := Result + ' ' + IntToHex(ByteArray[i], 2);

  Result := Result + LF + 'TCP Message: ';
  for i := MAX_HEADER_ONLY_LEN to Length(ByteArray) - 1 do
    Result := Result + ' ' + IntToHex(ByteArray[i], 2);
end;

procedure TLccMessage.Copy(TargetMessage: TLccMessage);
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

procedure TLccMessage.ZeroFields;
begin
  FIsCAN := False;
  FDataCount := 0;
  CAN.FDestAlias := 0;
  FDestID := NULL_NODE_ID;
  CAN.FSourceAlias := 0;
  FSourceID := NULL_NODE_ID;
  CAN.FFramingBits := 0;
  CAN.FiTag := 0;
  FMTI := 0;
  CAN.FMTI := 0;
  FUserValid := False;
end;

procedure TLccMessage.LoadCID(ASourceID: TNodeID; ASourceAlias: Word; ACID: Byte);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  SourceID := ASourceID;
  case ACID of
    0 : CAN.MTI := MTI_CAN_CID0 or DWord(ASourceAlias) or (ASourceID[1] and $00FFF000);
    1 : CAN.MTI := MTI_CAN_CID1 or DWord(ASourceAlias) or ((ASourceID[1] shl 12) and $00FFF000);
    2 : CAN.MTI := MTI_CAN_CID2 or DWord(ASourceAlias) or (ASourceID[0] and $00FFF000);
    3 : CAN.MTI := MTI_CAN_CID3 or DWord(ASourceAlias) or ((ASourceID[0] shl 12) and $00FFF000);
  end;
end;

procedure TLccMessage.LoadRID(ASourceAlias: Word);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  CAN.MTI := MTI_CAN_RID or ASourceAlias;
end;

procedure TLccMessage.LoadAMD(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  CAN.MTI := MTI_CAN_AMD or ASourceAlias;
  SourceID := ASourceID;
  InsertNodeID(0, ASourceID);
  DataCount := 6;
end;

procedure TLccMessage.LoadAME(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  CAN.MTI := MTI_CAN_AME or ASourceAlias;
  SourceID := ASourceID;
  InsertNodeID(0, ASourceID);
  DataCount := 6;
end;

procedure TLccMessage.LoadAMR(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  CAN.SourceAlias := ASourceAlias;
  IsCAN := True;
  CAN.MTI := MTI_CAN_AMR or ASourceAlias;
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

procedure TLccMessage.LoadPCER(ASourceID: TNodeID; ASourceAlias: Word; AnEvent: PEventID);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
  InsertEventID(0, AnEvent^);
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

procedure TLccMessage.LoadVerifyNodeID(ASourceID: TNodeID; ASourceAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  CAN.SourceAlias := ASourceAlias;
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

procedure TLccMessage.LoadProtocolIdentifyReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; Flags: QWord);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  FDataArray[5] := _Lo(Flags);
  FDataArray[4] := _Hi(Flags);
  FDataArray[3] := _Higher(Flags);
  FDataArray[2] := _Highest(Flags);
  FDataArray[1] := _Highest1(Flags);
  FDataArray[0] := _Highest2(Flags);

{  DataArray[0] := _Lo(Flags);
  DataArray[1] := _Hi(Flags);
  DataArray[2] := _Higher(Flags);
  DataArray[3] := _Highest(Flags);
  DataArray[4] := _Highest1(Flags);
  DataArray[5] := _Highest2(Flags); }
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
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; ASpeed: THalfFloat);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 3;
  FDataArray[0] := TRACTION_SPEED_DIR;
  FDataArray[1] := Hi( ASpeed);
  FDataArray[2] := Lo( ASpeed);
  MTI := MTI_TRACTION_PROTOCOL;
end;


procedure TLccMessage.LoadVerifiedNodeID(ASourceID: TNodeID; ASourceAlias: Word
  );
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
  FDataArray[1] := _Higher( AnAddress);
  FDataArray[2] := _Hi( AnAddress);
  FDataArray[3] := _Lo( AnAddress);
  FDataArray[4] := _Hi( AValue);
  FDataArray[5] := _Lo (AValue);
  MTI := MTI_TRACTION_PROTOCOL;
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
  MTI := MTI_TRACTION_PROTOCOL;
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
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionQueryFunction(ASourceID: TNodeID;
  ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  DataCount := 1;
  FDataArray[0] := TRACTION_QUERY_FUNCTION;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionControllerAssign(ASourceID: TNodeID;
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
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, ANodeID);
    FDataArray[9] := Hi( AnAlias);
    FDataArray[10] := Lo( AnAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN;
    FDataArray[2] := 0;
    InsertNodeID(3, ANodeID);
  end;
  MTI := MTI_TRACTION_PROTOCOL;
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
  MTI := MTI_TRACTION_PROTOCOL;
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
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionControllerChangeNotify(ASourceID: TNodeID;
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
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_NOTIFY;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, ANodeID);
    FDataArray[9] := Hi( AnAlias);
    FDataArray[10] := Lo( AnAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
    FDataArray[1] := TRACTION_CONTROLLER_CONFIG_NOTIFY;
    FDataArray[2] := 0;
    InsertNodeID(3, ANodeID);
  end;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionControllerChangeNotifyReply(
  ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word;
  Allow: Boolean);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  MTI := MTI_TRACTION_REPLY;
  DataCount := 3;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_NOTIFY;
  if Allow then
    FDataArray[2] := 0
  else
    FDataArray[2] := $FF
end;

procedure TLccMessage.LoadTractionConsistAttach(ASourceID: TNodeID;
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
    FDataArray[0] := TRACTION_CONSIST;
    FDataArray[1] := TRACTION_CONSIST_ATTACH;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, ANodeID);
    FDataArray[9] := Hi( AnAlias);
    FDataArray[10] := Lo( AnAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONSIST;
    FDataArray[1] := TRACTION_CONSIST_ATTACH;
    FDataArray[2] := 0;
    InsertNodeID(3, ANodeID);
  end;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionConsistDetach(ASourceID: TNodeID;
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
    FDataArray[0] := TRACTION_CONSIST;
    FDataArray[1] := TRACTION_CONSIST_DETACH;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, ANodeID);
    FDataArray[9] := Hi( AnAlias);
    FDataArray[10] := Lo( AnAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONSIST;
    FDataArray[1] := TRACTION_CONSIST_DETACH;
    FDataArray[2] := 0;
    InsertNodeID(3, ANodeID);
  end;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionConsistQuery(ASourceID: TNodeID;
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
    FDataArray[0] := TRACTION_CONSIST;
    FDataArray[1] := TRACTION_CONSIST_QUERY;
    FDataArray[2] := TRACTION_FLAGS_ALIAS_INCLUDED;
    InsertNodeID(3, ANodeID);
    FDataArray[9] := Hi( AnAlias);
    FDataArray[10] := Lo( AnAlias);
  end else
  begin
    DataCount := 9;
    FDataArray[0] := TRACTION_CONSIST;
    FDataArray[1] := TRACTION_CONSIST_QUERY;
    FDataArray[2] := 0;
    InsertNodeID(3, ANodeID);
  end;
  MTI := MTI_TRACTION_PROTOCOL;
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
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadSimpleTrainNodeIdentInfoRequest(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
begin
  ZeroFields;
  SourceID := ASourceID;
  DestID := ADestID;
  CAN.SourceAlias := ASourceAlias;
  CAN.DestAlias := ADestAlias;
  MTI := MTI_SIMPLE_TRAIN_INFO_REQUEST;
end;

procedure TLccMessage.LoadSimpleNodeIdentInfoReply(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; SimplePackedArray: TSimpleNodeInfoPacked);
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
  FDataArray[6] := MSI_FDI;
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
  FDataArray[6] := MSI_FUNCTION_CONFIG;
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
  FDataArray[6] := MSI_FUNCTION_CONFIG;
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

procedure TLccMessage.LoadConfigMemAddressSpaceInfo(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word; AddressSpace: Byte);
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

procedure TLccMessage.LoadConfigMemOptions(ASourceID: TNodeID; ASourceAlias: Word; ADestID: TNodeID; ADestAlias: Word);
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

