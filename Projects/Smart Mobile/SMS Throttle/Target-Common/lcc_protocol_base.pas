unit lcc_protocol_base;

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
  System.Memory.Buffer,
  System.Memory,
{$ELSE}
  Classes,
  SysUtils,
{$ENDIF}
  lcc_defines,
  lcc_utilities,
  lcc_node_messages;


type

{ TNodeProtocolBase }

TNodeProtocolBase = class(TObject)
private
//JDK  FCreateTime: DWord;    Not sure what I used this for
  FErrorCode: Word;
  FSendMessageFunc: TLccSendMessageFunc;
  FWorkerMessage: TLccMessage;
protected
  FValid: Boolean;
  procedure SetValid(AValue: Boolean); virtual;  // Just so it can be overridden for special behavior
//JDK  property CreateTime: DWord read FCreateTime write FCreateTime;
  property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
public
  property SendMessageFunc: TLccSendMessageFunc read FSendMessageFunc;
  property ErrorCode: Word read FErrorCode write FErrorCode;
  property Valid: Boolean read FValid write SetValid;

  constructor Create(ASendMessageFunc: TLccSendMessageFunc); virtual;
  destructor Destroy; override;

  function ReadAsString(Address: DWord; AStream: TStream): String;
  procedure DatagramReadRequest(LccMessage: TLccMessage; OutMessage: TLccMessage; AStream: TStream); virtual;
  procedure DatagramWriteRequest(LccMessage: TLccMessage; AStream: TStream); virtual;
end;

implementation

{ TNodeProtocolBase }

procedure TNodeProtocolBase.SetValid(AValue: Boolean);
begin
  if FValid=AValue then Exit;
  FValid:=AValue;
end;

procedure TNodeProtocolBase.DatagramWriteRequest(LccMessage: TLccMessage; AStream: TStream);
var
 i: Integer;
 iStart : Integer;
 WriteCount,Address: DWord;
begin
  // Assumption is this is a datagram message

  // First see if the memory space to work on is in byte 6 or part of the first byte
  // to determine where the first byte of real data is
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
    StreamWriteByte(AStream, LccMessage.DataArrayIndexer[i]);
end;

constructor TNodeProtocolBase.Create(ASendMessageFunc: TLccSendMessageFunc);
begin
  inherited Create;
//JDK  FCreateTime := GetTickCount;
  FSendMessageFunc := ASendMessageFunc;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TNodeProtocolBase.Destroy;
begin
  FWorkerMessage.Free;
  inherited Destroy;
end;

function TNodeProtocolBase.ReadAsString(Address: DWord; AStream: TStream): String;
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
     C := Chr( StreamReadByte(AStream));
     if C <> #0 then
       Result := Result + C
     else
       Done := True;
     Inc(i)
   end;
 end;
end;

procedure TNodeProtocolBase.DatagramReadRequest(LccMessage: TLccMessage; OutMessage: TLccMessage; AStream: TStream);
//
// Assumes the Source and Destination have already been set up
//
var
  i: Integer;
  FirstDataByte, BytesToRead: Integer;
  AddressStart: DWord;
begin
  // Assumption is this is a datagram message

  // Is the addressStart space in the header or is it the first byte in the data that
  // we need to skip over?
  if LccMessage.DataArrayIndexer[1] and $03 = 0 then
    FirstDataByte := 7     // Skip over the addressStart space byte in the data
  else
    FirstDataByte := 6;    // The addressStart space is encoded in the header so use all the data bytes
  BytesToRead := LccMessage.DataArrayIndexer[FirstDataByte];                  // number of bytes to read
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArrayIndexer[0];          // Just copy the original message
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArrayIndexer[1] or $10;   // except set the reply flag
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];          // Copy the addressStart
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArrayIndexer[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArrayIndexer[5];
  if FirstDataByte = 7 then
    OutMessage.DataArrayIndexer[6] := LccMessage.DataArrayIndexer[6];

  AddressStart := LccMessage.ExtractDataBytesAsInt(2, 5);     // Pull out the AddressStart

  if (AStream.Size = 0) or (AddressStart > AStream.Size) then   // Something is wrong.. Should I send an error?
    OutMessage.DataCount := FirstDataByte - 1
  else begin
    AStream.Position := AddressStart;
    i := 0;
    while (AStream.Position < AStream.Size) and (i < BytesToRead) do
    begin
      OutMessage.DataArrayIndexer[FirstDataByte + i] := StreamReadByte(AStream);
      Inc(i);
    end;
    OutMessage.DataCount := FirstDataByte + i;
  end;
end;

end.

