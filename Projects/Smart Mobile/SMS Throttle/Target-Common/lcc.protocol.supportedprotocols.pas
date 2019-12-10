unit lcc.protocol.supportedprotocols;

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
  lcc.protocol.base,
  lcc.defines,
  lcc.node.messages;

type

{ TProtocolSupportedProtocols }

TProtocolSupportedProtocols = class(TNodeProtocolBase)
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
  FSimpleTrainNodeInfo: Boolean;
  FFunctionConfiguration: Boolean;
protected

public
  {$IFDEF DWSCRIPT}
  ????
  {$ELSE}
  Flags: array of QWord;
  procedure DecodeFlags;
  function EncodeFlags: QWord;
  {$ENDIF}

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
  property SimpleTrainNodeInfo: Boolean read FSimpleTrainNodeInfo write FSimpleTrainNodeInfo;

  function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
end;

implementation

{ TProtocolSupportedProtocols }

procedure TProtocolSupportedProtocols.DecodeFlags;
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
    Valid := True;
  end;
end;

function TProtocolSupportedProtocols.EncodeFlags: QWord;
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
end;

function TProtocolSupportedProtocols.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  i, FlagBlocks, Offset: Integer;
begin
  Result := True;
  FlagBlocks := LccMessage.DataCount div 6;

  {$IFDEF DWSCRIPT}
  var BinaryData: TBinaryData;
  BinaryData := TBinaryData.Create(TMarshal.AllocMem(FlagBlocks).Segment);
  Flags := BinaryData.ToBytes;
  {$ELSE}
  SetLength(Flags, FlagBlocks);
  {$ENDIF}

  Offset := 0;
  for i := 0 to FlagBlocks - 1 do
  begin
    Flags[i] := LccMessage.ExtractDataBytesAsInt(Offset, 5);     // Protocol uses 6 byte chunks due to needing to use 2 in the CAN for the destination
    Offset := Offset + 6;
  end;
  DecodeFlags;
end;

end.

