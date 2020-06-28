unit OpenLcbThrottleUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.ScrollBox, FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation,
  lcc_utilities,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_defines,
  lcc_ethernet_server,
  lcc_ethernet_client,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_protocol_traction,
  lcc_protocol_traction_configuration_functions,
  lcc_protocol_traction_configuation_functiondefinitioninfo,
  lcc_protocol_traction_simpletrainnodeinfo,
  lcc_math_float16, FMX.Edit, FMX.Objects, FMX.ListBox, FMX.Layouts,
  FMX.Memo, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Colors, FMX.ListView, FMX.MultiView, FMX.Ani,
  FMX.Effects, FMX.Filter.Effects, System.Math.Vectors, FMX.Controls3D,
  FMX.Layers3D;

type
  TOpenLcbThrottleForm = class(TForm)
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControlMain: TTabControl;
    TabItemTrains: TTabItem;
    TabItemOpenLCB: TTabItem;
    TabItemSettings: TTabItem;
    GestureManager1: TGestureManager;
    ListBox1: TListBox;
    ListBoxSettingsItemServer: TListBoxItem;
    Text1: TText;
    EditServerIP: TEdit;
    ListBoxItemSettingServerPort: TListBoxItem;
    TextSettingsServerPort: TText;
    EditSettingsServerPort: TEdit;
    ListBoxGroupHeaderSettingsNetwork: TListBoxGroupHeader;
    ListBoxGroupHeaderSettingsOLCB: TListBoxGroupHeader;
    ListBoxItemSettingsOLCBMonitorDepth: TListBoxItem;
    Text2: TText;
    EditSettingsBufferDepth: TEdit;
    ListBoxItemDataFormat: TListBoxItem;
    TextSettingsOCLBDataFormat: TText;
    SwitchSettingsDataFormatGridConnect: TSwitch;
    SpeedButtonOpenLCBClear: TSpeedButton;
    SwitchOpenLCBLog: TSwitch;
    Text3: TText;
    MemoOpenLCB: TMemo;
    MultiViewTrains: TMultiView;
    ListView1: TListView;
    ColorKeyAlphaEffect1: TColorKeyAlphaEffect;
    GridLayout1: TGridLayout;
    SpeedButtonMore: TSpeedButton;
    LayoutMainTrain: TLayout;
    GridLayoutFunctions: TGridLayout;
    CornerButtonF0: TCornerButton;
    CornerButtonF1: TCornerButton;
    CornerButtonF2: TCornerButton;
    CornerButtonF3: TCornerButton;
    CornerButtonF4: TCornerButton;
    CornerButtonF5: TCornerButton;
    CornerButtonF6: TCornerButton;
    CornerButtonF7: TCornerButton;
    CornerButtonF8: TCornerButton;
    CornerButtonF9: TCornerButton;
    CornerButton10: TCornerButton;
    CornerButton11: TCornerButton;
    LayoutThrottle: TLayout;
    TextSpeed: TText;
    Text4: TText;
    ScrollBarThrottle: TScrollBar;
    CornerButtonForward: TCornerButton;
    TextDirection: TText;
    CornerButtonReverse: TCornerButton;
    LayoutDirection: TLayout;
    Rectangle1: TRectangle;
    LayoutTrainsMultiViewToolbar: TLayout;
    SpeedButton1: TSpeedButton;
    Rectangle2: TRectangle;
    SpeedButtonTrainSearch: TSpeedButton;
    LayoutTrainsMultiViewSearch: TLayout;
    EditTrainSearch: TEdit;
    FloatAnimationTrainSearch: TFloatAnimation;
    SpeedButtonConnect: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure ScrollBarThrottleChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CornerButtonForwardClick(Sender: TObject);
    procedure CornerButtonReverseClick(Sender: TObject);
    procedure SpeedButtonTrainSearchClick(Sender: TObject);
    procedure MultiViewTrainsStartShowing(Sender: TObject);
    procedure GridLayoutFunctionsResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OpenLcbThrottleForm: TOpenLcbThrottleForm;

implementation

{$R *.fmx}

procedure TOpenLcbThrottleForm.CornerButtonForwardClick(Sender: TObject);
begin
  TextDirection.Text := 'Forward'
end;

procedure TOpenLcbThrottleForm.CornerButtonReverseClick(Sender: TObject);
begin
  TextDirection.Text := 'Reverse';
end;

procedure TOpenLcbThrottleForm.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControlMain.ActiveTab := TabItemTrains;
end;

procedure TOpenLcbThrottleForm.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if TabControlMain.ActiveTab <> TabControlMain.Tabs[TabControlMain.TabCount-1] then
        TabControlMain.ActiveTab := TabControlMain.Tabs[TabControlMain.TabIndex+1];
      Handled := True;
    end;

    sgiRight:
    begin
      if TabControlMain.ActiveTab <> TabControlMain.Tabs[0] then
        TabControlMain.ActiveTab := TabControlMain.Tabs[TabControlMain.TabIndex-1];
      Handled := True;
    end;
  end;
{$ENDIF}
end;

procedure TOpenLcbThrottleForm.FormShow(Sender: TObject);
begin
  MultiViewTrains.HideMaster;
end;

procedure TOpenLcbThrottleForm.GridLayoutFunctionsResize(Sender: TObject);
{
  procedure CalculateLayout(Columns, GridClientW, GridClientH: single);
  var
    Rows: Integer;
  begin
    GridLayoutFunctions.ItemWidth := GridClientW/Columns;
    GridLayoutFunctions.ItemHeight := GridLayoutFunctions.ItemWidth;
  end;

var
  Columns, Rows: Integer;
  GridClientW, GridClientH: single;     }
begin
{  Columns := 1;
  GridClientW := GridLayoutFunctions.Width - (GridLayoutFunctions.Padding.Left+GridLayoutFunctions.Padding.Right);
  GridClientH := GridLayoutFunctions.Height - (GridLayoutFunctions.Padding.Top+GridLayoutFunctions.Padding.Bottom);

  repeat
    CalculateLayout(Columns, GridClientW, GridClientH);
    Rows := GridLayoutFunctions.ChildrenCount div Columns + 1;
    Inc(Columns);
  until GridLayoutFunctions.ItemHeight * Rows <= GridClientH;
     }
end;

procedure TOpenLcbThrottleForm.MultiViewTrainsStartShowing(Sender: TObject);
begin
  MultiViewTrains.Width := Width * 0.75
end;

procedure TOpenLcbThrottleForm.ScrollBarThrottleChange(Sender: TObject);
begin
  TextSpeed.Text := IntToStr( Round(ScrollBarThrottle.Value));
end;

procedure TOpenLcbThrottleForm.SpeedButtonTrainSearchClick(Sender: TObject);
begin
  FloatAnimationTrainSearch.Inverse := not FloatAnimationTrainSearch.Inverse;
  FloatAnimationTrainSearch.Start
end;

end.
