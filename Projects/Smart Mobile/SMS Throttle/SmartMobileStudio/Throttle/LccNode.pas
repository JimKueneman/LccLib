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
  System.Lists,
  lcc_node_manager,
  lcc_node,
  lcc_node_controller,
  lcc_node_messages,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_defines,
  lcc_math_float16;

type
  TControllerManager = class
  private
    FControllerCreated: Boolean;
  public
    NodeManager: TLccCanNodeManager;
    ControllerNode: TLccTrainController; // First Node created by the NodeManager, it is assigned when the Ethenetlink is established
    MessageList: TStringList;

    property ControllerCreated: Boolean read FControllerCreated;

    constructor Create;
    destructor Destroy; override;
    procedure CreateController;
    procedure DestroyController;
  end;

function GetControllerManager: TControllerManager;

implementation

var
  InternalControllerManager: TControllerManager = nil;


function GetControllerManager: TControllerManager;
begin
  if not Assigned(InternalControllerManager) then
    InternalControllerManager := TControllerManager.Create;
  Result := InternalControllerManager
end;

{ TControllerManager }

constructor TControllerManager.Create;
begin
  NodeManager := TLccCanNodeManager.Create(nil);
end;

destructor TControllerManager.Destroy;
begin
  NodeManager.Free;
end;

procedure TControllerManager.CreateController;
begin
  if not Assigned(ControllerNode) then
  begin
    ControllerNode := NodeManager.AddNodeByClass('', TLccTrainController, True) as TLccTrainController;
    FControllerCreated := Assigned(ControllerNode);
    lcc_defines.Max_Allowed_Buffers := 1; // HACK ALLERT: Allow OpenLCB Python Scripts to run
  end
end;

procedure TControllerManager.DestroyController;
begin
  if Assigned(ControllerNode) then
  begin
    NodeManager.Clear;
    FControllerCreated := False;
    ControllerNode := nil;
  end
end;

end.
