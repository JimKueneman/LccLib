unit TabMainForm;

interface

uses 
  System.Types,
  System.Types.Convert,
  System.Objects,
  System.Time,
  System.IOUtils,
  System.Device.Storage,
  SmartCL.System,
  SmartCL.Time,
  SmartCL.Graphics,
  SmartCL.Components,
  SmartCL.FileUtils,
  SmartCL.Device.Storage,
  SmartCL.Forms,
  SmartCL.Fonts,
  SmartCL.Theme,
  SmartCL.Borders,
  SmartCL.Application,
  SmartCL.Controls,
  SmartCL.Layout,
  Storage,
  SmartCL.Controls.Button,
  SmartCL.Controls.EditBox,
  SmartCL.Controls.Label,
  SmartCL.Controls.Panel,
  SmartCL.Slider,
  lcc_defines,
  lcc_node_messages,
  lcc_math_float16,
  lcc_node,
  lcc_node_train,
  lcc_utilities,
  lcc_node_controller,
  LccNode, SmartCL.Controls.CheckBox,
  SmartCL.Controls.RadioGroup, SmartCL.Controls.ScrollBar;

type
  TTabMainForm = class(TW3Form)
    procedure W3ButtonQueryFunctionClick(Sender: TObject);
    procedure W3ButtonReleaseTrainClick(Sender: TObject);
    procedure W3ButtonAssignTrainClick(Sender: TObject);
    procedure W3ButtonFunctionClick(Sender: TObject);
    procedure W3SliderSpeedChange(Sender: TObject);
    procedure W3ButtonReverseClick(Sender: TObject);
    procedure W3ButtonForwardClick(Sender: TObject);
  private
    {$I 'TabMainForm:intf'}
  protected
    ControllerManager: TControllerManager;
    FStartupInitializeComplete: Boolean;
    FCallbacksAssigned: Boolean;

    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;

    // The Controller is the Controller Node created in the NodeManager
    procedure OnControllerSearchResult(Sender: TLccTractionAssignTrainAction; Results: TLccSearchResultsArray; SearchResultCount: Integer; var SelectedResultIndex: Integer);
    procedure OnControllerTrainAssigned(Sender: TLccNode; Reason: TControllerTrainAssignResult);
    procedure OnControllerTrainReleased(Sender: TLccNode);
    procedure OnControllerQuerySpeedReply(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte);
    procedure OnControllerQueryFunctionReply(Sender: TLccNode; Address: DWORD; Value: Word);
    procedure OnControllerRequestTakeover(Sender: TLccNode; var Allow: Boolean);

    procedure EnableThrottleControls;
    procedure DisableThrottleControls;
    procedure AssignCallbacks;

  public
    property StartupInitializeComplete: Boolean read FStartupInitializeComplete;
    property CallbacksAssigned: Boolean read FCallbacksAssigned;
  end;

implementation

{ TTabMainForm }

procedure TTabMainForm.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  ControllerManager := GetControllerManager;
end;

procedure TTabMainForm.OnControllerRequestTakeover(Sender: TLccNode; var Allow: Boolean);
begin
  Allow := True;
  if W3CheckBoxQueryRelease.Checked then
    Allow := Prompt('Allow another throttle to take over the train');
  if Allow and Assigned(ControllerManager.ControllerNode) then
    ControllerManager.ControllerNode.ReleaseTrain;
end;

procedure TTabMainForm.OnControllerSearchResult(Sender: TLccTractionAssignTrainAction; Results: TLccSearchResultsArray; SearchResultCount: Integer; var SelectedResultIndex: Integer);
begin
 // ShowMessage('Search Result');
end;

procedure TTabMainForm.OnControllerQueryFunctionReply(Sender: TLccNode; Address: DWord; Value: Word);
begin
  ShowMessage('Query Function Reply');
end;

procedure TTabMainForm.OnControllerQuerySpeedReply(Sender: TLccNode; SetSpeed: THalfFloat; CommandSpeed: THalfFloat; ActualSpeed: THalfFloat; Status: Byte);
begin
  ShowMessage('Query Speed Reply');
end;

procedure TTabMainForm.OnControllerTrainAssigned(Sender: TLccNode; Reason: TControllerTrainAssignResult);
begin
  case Reason of
    tarAssigned:
      begin
        EnableThrottleControls;
      end;
    tarFailTrainRefused:
      begin
        ShowMessage('Failed: Train Refused to Release');
        DisableThrottleControls;
      end;
    tarFailControllerRefused:
      begin
        ShowMessage('Failed Controller Refused to Release');
        DisableThrottleControls
      end;
  end;
end;

procedure TTabMainForm.OnControllerTrainReleased(Sender: TLccNode);
begin
//  ShowMessage('Released');
  DisableThrottleControls;
end;

procedure TTabMainForm.InitializeObject;
begin
  inherited;
  {$I 'TabMainForm:impl'}
  FStartupInitializeComplete := False;
  FCallbacksAssigned := False;
end;

procedure TTabMainForm.Resize;
begin
  inherited;
  W3Panel1.Width := Width;
  W3Panel1.Height := Height;

  if not StartupInitializeComplete then
  begin
   // Javascript limitation, can't do it in InitializeForm or InitialzeObject :(
    W3RadioGroupSpeedStep.ItemIndex := 0;

    DisableThrottleControls;
    FStartupInitializeComplete := True;
  end;
end;

procedure TTabMainForm.W3ButtonReverseClick(Sender: TObject);
begin
  if ControllerManager.ControllerCreated then
    ControllerManager.ControllerNode.Direction := tdReverse
end;

procedure TTabMainForm.W3SliderSpeedChange(Sender: TObject);
begin
  W3LabelSpeed.Caption := 'Speed: ' + FloatToStr(W3SliderSpeed.Value, 0);
  if ControllerManager.ControllerCreated then
    ControllerManager.ControllerNode.Speed := W3SliderSpeed.Value
end;

procedure TTabMainForm.W3ButtonFunctionClick(Sender: TObject);
begin
  if ControllerManager.ControllerCreated then
    ControllerManager.ControllerNode.Functions[(Sender as TW3Button).TagValue] := not ControllerManager.ControllerNode.Functions[(Sender as TW3Button).TagValue]
end;

procedure TTabMainForm.W3ButtonReleaseTrainClick(Sender: TObject);
begin
  if ControllerManager.ControllerCreated then
  begin
    ControllerManager.ControllerNode.ReleaseTrain;
  end;
end;

procedure TTabMainForm.W3ButtonQueryFunctionClick(Sender: TObject);
begin
  if ControllerManager.ControllerCreated then
  begin
    ControllerManager.ControllerNode.QueryFunction(0);
  end;
end;

procedure TTabMainForm.W3ButtonAssignTrainClick(Sender: TObject);
begin
  if not CallbacksAssigned then
  begin
    AssignCallbacks;
    FCallbacksAssigned := True;
  end;

  if ControllerManager.ControllerCreated then
    ControllerManager.ControllerNode.AssignTrainByDccAddress(StrToInt(W3EditBoxDccAddress.Text), W3CheckBoxLongAddress.Checked, TLccDccSpeedStep( W3RadioGroupSpeedStep.ItemIndex + 1))
end;

procedure TTabMainForm.W3ButtonForwardClick(Sender: TObject);
begin
  if ControllerManager.ControllerCreated then
    ControllerManager.ControllerNode.Direction := tdForward
end;

procedure TTabMainForm.EnableThrottleControls;
begin
  W3ButtonF0.Enabled := True;
  W3ButtonF1.Enabled := True;
  W3ButtonF2.Enabled := True;
  W3ButtonF3.Enabled := True;
  W3ButtonForward.Enabled := True;
  W3ButtonReverse.Enabled := True;
  W3SliderSpeed.Enabled := True;
end;


procedure TTabMainForm.DisableThrottleControls;
begin
  W3ButtonF0.Enabled := False;
  W3ButtonF1.Enabled := False;
  W3ButtonF2.Enabled := False;
  W3ButtonF3.Enabled := False;
  W3ButtonForward.Enabled := False;
  W3ButtonReverse.Enabled := False;
  W3SliderSpeed.Enabled := False;
end;

procedure TTabMainForm.AssignCallbacks;
begin
   // Javascript limitation, can't do it in InitializeForm or InitialzeObject :(
  ControllerManager.ControllerNode.OnTrainAssigned := @OnControllerTrainAssigned;
  ControllerManager.ControllerNode.OnTrainReleased := @OnControllerTrainReleased;
  ControllerManager.ControllerNode.OnControllerRequestTakeover := @OnControllerRequestTakeover;
  ControllerManager.ControllerNode.OnQueryFunctionReply := @OnControllerQueryFunctionReply;
  ControllerManager.ControllerNode.OnSearchResult := @OnControllerSearchResult;
end;

initialization
  Forms.RegisterForm({$I %FILE%}, TTabMainForm);
end.
