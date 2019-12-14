unit lcc_protocol_supportedprotocols;

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
  System.Memory,
  System.Memory.Allocation,
  System.Memory.Buffer,
{$ELSE}
  Classes,
  SysUtils,
{$ENDIF}
  lcc_protocol_base,
  lcc_defines,
  lcc_node_messages;

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
  FFirmwareUpgradeActive: Boolean;
  FFirmwareUpgrade: Boolean;

protected

public
  Flags: TLccSupportedProtocolArray;

  procedure DecodeFlags;
  function EncodeFlags: TLccSupportedProtocolArray;

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
  property FirmwareUpgrade: Boolean read FFirmwareUpgrade write FFirmwareUpgrade;
  property FirmwareUpgradeActive: Boolean read FFirmwareUpgradeActive write FFirmwareUpgradeActive;

  function ProcessMessage(SourceLccMessage: TLccMessage): Boolean; override;
end;

implementation

{ TProtocolSupportedProtocols }

procedure TProtocolSupportedProtocols.DecodeFlags;
begin
  if Length(Flags) > 0 then
  begin
    // SimpleNode Flags[5]
    FDatagram := Flags[5] and PIP_DATAGRAM <> 0;
    FStream := Flags[5] and PIP_STREAM <> 0;
    FMemConfig := Flags[5] and PIP_MEMORY_CONFIG <> 0;
    FReservation := Flags[5] and PIP_RESERVATION <> 0;
    FEventExchange := Flags[5] and PIP_EVENT_EXCHANGE <> 0;
    FIdentification := Flags[5] and PIP_IDENTIFCIATION <> 0;
    FTeach_Learn := Flags[5] and PIP_TEACH_LEARN <> 0;

    FRemoteButton := Flags[4] and PIP_REMOTE_BUTTON <> 0;
    FACDI := Flags[4] and PIP_ABBREVIATED_CDI <> 0;
    FDisplay := Flags[4] and PIP_DISPLAY <> 0;
    FSimpleNodeInfo := Flags[4] and PIP_SIMPLE_NODE_INFO <> 0;
    FCDI := Flags[4] and PIP_CDI <> 0;
    FTractionControl := Flags[4] and PIP_TRACTION <> 0;
    FFDI := Flags[4] and PIP_FDI <> 0;
    // Dcc_Command_Station Flags[4]

    FSimpleTrainNodeInfo := Flags[3] and PIP_SIMPLE_TRAIN_NODE_INFO <> 0;
    FFunctionConfiguration := Flags[3] and PIP_FUNCTION_CONFIGURATION <> 0;
    FFirmwareUpgrade := Flags[3] and PIP_FIRMWARE_UPGRADE <> 0;
    FirmwareUpgradeActive := Flags[3] and PIP_FIRMWARE_UPGRADE_ACTIVE <> 0;

    Valid := True;
  end;
end;

function TProtocolSupportedProtocols.EncodeFlags: TLccSupportedProtocolArray;
var
  i: Integer;
begin
  for i := 0 to MAX_SUPPORTEDPROTOCOL_LEN - 1 do
    Result[i] := 0;

// if SimpleNode then Result[5] := ....
  if Datagram then Result[5] := Result[5] or PIP_DATAGRAM;
  if Stream then Result[5] := Result[5] or PIP_STREAM;
  if MemConfig then Result[5] := Result[5] or PIP_MEMORY_CONFIG;
  if Reservation then Result[5] := Result[5] or PIP_RESERVATION;
  if EventExchange then Result[5] := Result[5] or PIP_EVENT_EXCHANGE;
  if Identification then Result[5] := Result[5] or PIP_IDENTIFCIATION;
  if Teach_Learn then Result[5] := Result[5] or PIP_TEACH_LEARN;

  if RemoteButton then Result[4] := Result[4] or PIP_REMOTE_BUTTON;
  if ACDI then Result[4] := Result[4] or PIP_ABBREVIATED_CDI;
  if Display then Result[4] := Result[4] or PIP_DISPLAY;
  if SimpleNodeInfo then Result[4] := Result[4] or PIP_SIMPLE_NODE_INFO;
  if CDI then Result[4] := Result[4] or PIP_CDI;
  if TractionControl then Result[4] := Result[4] or PIP_TRACTION;
  if FDI then Result[4] := Result[4] or PIP_FDI;
// if DccCommand Station then Result[4] := .... depreciated

  if SimpleTrainNodeInfo then Result[3] := Result[3] or PIP_SIMPLE_TRAIN_NODE_INFO;
  if FunctionConfiguration then Result[3] := Result[3] or PIP_FUNCTION_CONFIGURATION;
  if FirmwareUpgrade then Result[3] := Result[3] or PIP_FIRMWARE_UPGRADE;
  if FirmwareUpgradeActive then Result[3] := Result[3] or PIP_FIRMWARE_UPGRADE_ACTIVE


end;

function TProtocolSupportedProtocols.ProcessMessage(SourceLccMessage: TLccMessage): Boolean;
begin
  Result := True;
  Flags[0] := SourceLccMessage.DataArrayIndexer[5];
  Flags[1] := SourceLccMessage.DataArrayIndexer[4];
  Flags[2] := SourceLccMessage.DataArrayIndexer[3];
  Flags[3] := SourceLccMessage.DataArrayIndexer[2];
  Flags[4] := SourceLccMessage.DataArrayIndexer[1];
  Flags[5] := SourceLccMessage.DataArrayIndexer[0];

  DecodeFlags;
end;

end.

