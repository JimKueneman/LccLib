unit lcc_protocol_traction;

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
  lcc_utilities,
  lcc_math_float16;

type

  { TProtocolTraction }

  TProtocolTraction = class(TNodeProtocolBase)
  public
    procedure SetSpeedDir(ALccMessage: TLccMessage);
    procedure SetFunction(ALccMessage: TLccMessage);

  end;

implementation

{ TProtocolTraction }

procedure TProtocolTraction.SetSpeedDir(ALccMessage: TLccMessage);
var
  Speed: Single;
begin
  Speed := HalfToFloat( ALccMessage.ExtractDataBytesAsInt(1, 2));
end;

procedure TProtocolTraction.SetFunction(ALccMessage: TLccMessage);
var
  FunctionAddress: DWORD;
  FunctionValue: WORD;
begin
  FunctionAddress := ALccMessage.ExtractDataBytesAsInt(1, 3);
  FunctionValue := ALccMessage.ExtractDataBytesAsInt(4, 5);
end;


end.

