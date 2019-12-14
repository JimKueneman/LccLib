unit lcc_protocol_memory_options;

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
{$ENDIF}
  lcc_protocol_base,
  lcc_defines,
  lcc_node_messages,
  lcc_utilities;

type

  { TProtocolMemoryOptions }

TProtocolMemoryOptions = class(TNodeProtocolBase)
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

implementation

{ TProtocolMemoryOptions }

procedure TProtocolMemoryOptions.LoadReply(LccMessage: TLccMessage);
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

function TProtocolMemoryOptions.ProcessMessage(LccMessage: TLccMessage): Boolean;
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
     //     OwnerManager.DoConfigMemOptionsReply(OwnerManager.FindMirroredNodeBySourceID(LccMessage, True), OwnerManager.FindMirroredNodeBySourceID(LccMessage, True));
        end;
    end
  end;
end;

end.

