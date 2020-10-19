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
  SmartCL.Controls.RadioGroup;

type
  TTabMainForm = class(TW3Form)
    procedure W3ButtonAssignTrainClick(Sender: TObject);
    procedure W3ButtonFunctionClick(Sender: TObject);
    procedure W3SliderSpeedChange(Sender: TObject);
    procedure W3ButtonReverseClick(Sender: TObject);
    procedure W3ButtonForwardClick(Sender: TObject);
  private
    {$I 'TabMainForm:intf'}
 //   FOptionsLayout: TLayout;
 //   FLayout: TLayout;
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;

    // The Controller is the Controller Node created in the NodeManager
    procedure ControllerTrainAssigned(Sender: TLccNode; Reason: TControllerTrainAssignResult);
    procedure ControllerTrainReleased(Sender: TLccNode);
  end;

implementation

{ TTabMainForm }

procedure TTabMainForm.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  W3RadioGroupSpeedStep.ItemIndex := 0;
end;

procedure TTabMainForm.InitializeObject;
begin
  inherited;
  {$I 'TabMainForm:impl'}

{  FOptionsLayout := Layout.Client(Layout.Margins(3).Spacing(3), [
    Layout.Top([
      Layout.Left(Layout.Spacing(3), [W3LabelIPAddress]),
      Layout.Client(Layout.Margins(3), [W3EditBoxIpAddress])
    ]),
    Layout.Top([
      Layout.Left(Layout.Spacing(3), [W3LabelIpPort]),
      Layout.Client(Layout.Margins(3), [W3EditBoxIpPort])
    ]),
    Layout.Top(W3CheckBoxTcp)

  ]);

  FLayout := Layout.Client(Layout.Margins(3).Spacing(3), [
    Layout.Top(W3LabelHeader),
    Layout.Client(W3PanelSettings),
    Layout.Bottom(W3ButtonConnection),
    Layout.Bottom(W3TabControlNav)
  ]);

  }

end;


 
procedure TTabMainForm.Resize;
begin
  inherited;
//  FLayout.Resize(Self);
//  FOptionsLayout.Resize(W3PanelSettings);
  W3Panel1.Width := Width;
  W3Panel1.Height := Height;


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

procedure TTabMainForm.W3ButtonAssignTrainClick(Sender: TObject);
begin
  if ControllerManager.ControllerCreated then
  begin
    ControllerManager.ControllerNode.AssignTrainByDccAddress(StrToInt(W3EditBoxDccAddress.Text), W3CheckBoxLongAddress.Checked, TLccDccSpeedStep( W3RadioGroupSpeedStep.ItemIndex));
  end
end;

procedure TTabMainForm.W3ButtonForwardClick(Sender: TObject);
begin
  if ControllerManager.ControllerCreated then
    ControllerManager.ControllerNode.Direction := tdForward
end;

procedure TTabMainForm.ControllerTrainReleased(Sender: TLccNode);
begin

end;

procedure TTabMainForm.ControllerTrainAssigned(Sender: TLccNode; Reason: TControllerTrainAssignResult);
begin

end;

initialization
  Forms.RegisterForm({$I %FILE%}, TTabMainForm);
end.
