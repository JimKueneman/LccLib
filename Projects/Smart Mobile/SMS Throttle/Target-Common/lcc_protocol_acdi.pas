unit lcc_protocol_acdi;

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
  lcc_node_messages;

type

{ TACDIMfg }

TACDIMfg = class(TNodeProtocolBase)
public
end;

{ TACDIUser }

TACDIUser = class(TNodeProtocolBase)
public
end;


implementation

end.

