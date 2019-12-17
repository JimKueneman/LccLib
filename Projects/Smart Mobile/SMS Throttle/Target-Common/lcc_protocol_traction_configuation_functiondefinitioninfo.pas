unit lcc_protocol_traction_configuation_functiondefinitioninfo;

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
  lcc_defines,
  lcc_messages,
  lcc_utilities;

type

{ TFDI }

TFDI = class(TStreamBasedProtocol)
protected
  procedure DoLoadComplete(LccMessage: TLccMessage); override;
end;

implementation


{ TFDI }

procedure TFDI.DoLoadComplete(LccMessage: TLccMessage);
var
  SourceNode, DestNode: TLccNode;
begin

end;

end.

