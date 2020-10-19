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

var
  ControllerManager: TControllerManager;

implementation



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
  ControllerNode := NodeManager.AddNodeByClass('', TLccTrainController, True) as TLccTrainController;
  FControllerCreated := Assigned(ControllerNode);
  lcc_defines.Max_Allowed_Buffers := 1; // HACK ALLERT: Allow OpenLCB Python Scripts to run
end;

procedure TControllerManager.DestroyController;
begin
  NodeManager.Clear;
  FControllerCreated := False;
  ControllerNode := nil;
end;

initialization
  ControllerManager := TControllerManager.Create;

finalization
  ControllerManager.Free;


end.
