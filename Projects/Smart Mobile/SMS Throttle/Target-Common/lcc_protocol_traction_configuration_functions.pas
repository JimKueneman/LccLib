unit lcc_protocol_traction_configuration_functions;

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
  lcc_defines,
  lcc_messages,
  lcc_utilities;

type

  { TFunctionConfiguration }

TFunctionConfiguration = class(TStreamBasedProtocol)
private
  FFunctionStatesArray: TFunctionStatesArray;
  function GetFunctionStates(iIndex: Integer): Boolean;
protected
  property FunctionStatesArray: TFunctionStatesArray read FFunctionStatesArray write FFunctionStatesArray;
public
  property FunctionStates[iIndex: Integer]: Boolean read GetFunctionStates;
  function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
end;


implementation

{ TFunctionConfiguration }

function TFunctionConfiguration.GetFunctionStates(iIndex: Integer): Boolean;
begin
  if (iIndex > -1) and (iIndex < 30) then
    Result := FFunctionStatesArray[iIndex] = 1
  else
    Result := False;
end;

function TFunctionConfiguration.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  SourceNode, DestNode: TLccNode;
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
  end;
end;

end.

