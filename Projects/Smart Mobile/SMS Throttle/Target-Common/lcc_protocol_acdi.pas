unit lcc_protocol_acdi;

//
// In order to use ACDI 4 things must occur:
// 1) the <ADCI> tag must be in the CDI file
// 2) the Memory Options protocol must have flags set: SupportACDIMfgRead, SupportACDIUserRead, SupportACDIUserWrite
// 3) the Memory Options protocol must show that which ACDI memory spaces are valid spaces
// 4) Protocol Identification Protocol (PIP) must return supports ACDI
//

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

