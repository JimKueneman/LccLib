unit LccNode;

interface

uses 
  System.Types,
  System.Types.Convert,
  System.Types.Graphics,
  System.Objects,
  System.Time,
  System.Streams,
  System.Reader,
  System.Writer,
  System.IOUtils,
  System.Device.Storage,
  SmartCL.FileUtils,
  SmartCL.Device.Storage,
  SmartCL.System,
  SmartCL.Time,
  SmartCL.Components,
  SmartCL.Application,
  lcc_node_manager,
  lcc_node,
  lcc_node_messages,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_defines,
  lcc_math_float16;

type
  TWebLccNode = class
  public
    property NodeManager: TLccNodeManager;

    constructor Create;
    destructor Destroy; override;
  end;

var
  WebLccNode: TWebLccNode;

implementation



{ TWebLccNode }

constructor TWebLccNode.Create;
begin
  NodeManager := TLccCanNodeManager.Create(nil);

end;

destructor TWebLccNode.Destroy;
begin
  NodeManager.Free;
end;

initialization
  WebLccNode := TWebLccNode.Create;

finalization
  WebLccNode.Free;


end.
