unit unitMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, ComCtrls, Spin, Menus, unitFrameItemEditor,
  virtuallistview, unitFrameBlockEvent;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionActionsSelectAll: TAction;
    ActionActionsDeselectAll: TAction;
    ActionActionsExpandAll: TAction;
    ActionActionsCollapseAll: TAction;
    ActionList1: TActionList;
    ActionNewGenericResponse: TAction;
    ActionNewGenericAction: TAction;
    ActionNewThrowTurnout: TAction;
    ActionNewThreeColorSignal: TAction;
    ActionNewTwoColorSignal: TAction;
    ActionNewTurnoutPositionChanged: TAction;
    ActionNewPushbuttonPressed: TAction;
    ActionNewBlock: TAction;
    ActionListMain: TActionList;
    ButtonNewAction: TButton;
    ButtonNewResponse: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    ColorDialog1: TColorDialog;
    ComboBoxLayoutActions: TComboBox;
    ComboBoxLayoutResponses: TComboBox;
    ImageListState: TImageList;
    ImageList: TImageList;
    ImageListExpand: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelSelectedCount: TLabel;
    LabelSelectedAddress: TLabel;
    MenuItemSelectAll: TMenuItem;
    MenuItemUnSelectAll: TMenuItem;
    MenuItemCollapseAll: TMenuItem;
    MenuItemExpandAll: TMenuItem;
    N1: TMenuItem;
    Panel3: TPanel;
    PanelFrameDock: TPanel;
    PanelAction: TPanel;
    PanelResponse: TPanel;
    PanelActionHeader: TPanel;
    PanelResponseHeader: TPanel;
    PopupMenuActions: TPopupMenu;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    VirtualListviewActions: TVirtualListview;
    VirtualListviewResponses: TVirtualListview;
    procedure ActionActionsCollapseAllExecute(Sender: TObject);
    procedure ActionActionsDeselectAllExecute(Sender: TObject);
    procedure ActionActionsExpandAllExecute(Sender: TObject);
    procedure ActionActionsSelectAllExecute(Sender: TObject);
    procedure ActionNewBlockExecute(Sender: TObject);
    procedure ActionNewGenericActionExecute(Sender: TObject);
    procedure ActionNewGenericResponseExecute(Sender: TObject);
    procedure ButtonNewActionClick(Sender: TObject);
    procedure ButtonNewResponseClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure ColorButton1Click(Sender: TObject);
    procedure ColorButton2Click(Sender: TObject);
    procedure ColorButton3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
    procedure VirtualListviewActionsFocusedChanged(Sender: TObject; FocusedItem, OldFocusedItem: TVirtualListviewItem);
    procedure VirtualListviewActionsSelectedChanged(Sender: TObject);
    procedure VirtualListviewResponsesFocusedChanged(Sender: TObject; FocusedItem, OldFocusedItem: TVirtualListviewItem);
  private
    FActiveFrame: TFrame;
    FDisplayedItem: TVirtualListviewItem;
    FFrameEditItem: TFrameItemEditor;
    FFrameNewBlock: TFrameBlockName;
    function GetFrameEditItem: TFrameItemEditor;
    function GetFrameNewBlock: TFrameBlockName;
    procedure SetActiveFrame(AValue: TFrame);
    procedure SetDisplayedItem(AValue: TVirtualListviewItem);

  public
    property ActiveFrame: TFrame read FActiveFrame write SetActiveFrame;
    property DisplayedItem: TVirtualListviewItem read FDisplayedItem write SetDisplayedItem;
    property FrameEditItem: TFrameItemEditor read GetFrameEditItem write FFrameEditItem;
    property FrameNewBlock: TFrameBlockName read GetFrameNewBlock write FFrameNewBlock;

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ActionActionsCollapseAllExecute(Sender: TObject);
begin
  VirtualListviewActions.CollapseAll(True);
end;

procedure TForm1.ActionActionsDeselectAllExecute(Sender: TObject);
begin
  VirtualListviewActions.UnSelectAll(True);
end;

procedure TForm1.ActionActionsExpandAllExecute(Sender: TObject);
begin
  VirtualListviewActions.ExpandAll(True);
end;

procedure TForm1.ActionActionsSelectAllExecute(Sender: TObject);
begin
  VirtualListviewActions.SelectAll(True);
end;

procedure TForm1.ActionNewBlockExecute(Sender: TObject);
begin
  ActiveFrame := FrameNewBlock;
end;

procedure TForm1.ActionNewGenericActionExecute(Sender: TObject);
var
  Item: TVirtualListviewItem;
begin
  VirtualListviewActions.BeginUpdate;
  Item := VirtualListviewActions.Items.Add('New Layout Action', -1);
  if Assigned(Item) then
  begin
    Item.Captions.Add('Description');
    Item.Captions.Add('Responses Triggered: 0');
  end;
  VirtualListviewActions.EndUpdate;

end;

procedure TForm1.ActionNewGenericResponseExecute(Sender: TObject);
var
  Item: TVirtualListviewItem;
begin
  VirtualListviewResponses.BeginUpdate;
  Item := VirtualListviewResponses.Items.Add('New Layout Response', -1);
  if Assigned(Item) then
  begin
    Item.Captions.Add('Description');
    Item.Captions.Add('Trigged by Actions: 0');
  end;
  VirtualListviewResponses.EndUpdate;
end;

procedure TForm1.ButtonNewActionClick(Sender: TObject);
begin
  if ComboBoxLayoutActions.ItemIndex> -1 then
    (ComboBoxLayoutActions.Items.Objects[ComboBoxLayoutActions.ItemIndex] as TAction).Execute;
end;

procedure TForm1.ButtonNewResponseClick(Sender: TObject);
begin
  if ComboBoxLayoutResponses.ItemIndex> -1 then
    (ComboBoxLayoutResponses.Items.Objects[ComboBoxLayoutResponses.ItemIndex] as TAction).Execute;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  VirtualListviewActions.ExpandableItems := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  if CheckBox2.Checked then VirtualListviewActions.ExpandImages := ImageListExpand else
    VirtualListviewActions.ExpandImages := nil;
  VirtualListviewActions.BeginUpdate;
  VirtualListviewActions.EndUpdate;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  if CheckBox3.Checked then VirtualListviewActions.StateImages := ImageList  else
    VirtualListviewActions.StateImages := nil;
  VirtualListviewActions.BeginUpdate;
  VirtualListviewActions.EndUpdate;
end;

procedure TForm1.CheckBox4Change(Sender: TObject);
begin
  if CheckBox4.Checked then VirtualListviewActions.Images := ImageListState  else
    VirtualListviewActions.Images := nil;
  VirtualListviewActions.BeginUpdate;
  VirtualListviewActions.EndUpdate;
end;

procedure TForm1.CheckBox5Change(Sender: TObject);
begin
  VirtualListviewActions.BkGndGradientEnable := CheckBox5.Checked;
end;

procedure TForm1.CheckBox6Change(Sender: TObject);
begin
  VirtualListviewActions.ShowFocus := CheckBox6.Checked;
end;

procedure TForm1.ColorButton1Click(Sender: TObject);
begin
  ColorDialog1.Color := ColorButton1.ButtonColor;
  if ColorDialog1.Execute then
  begin
    ColorButton1.ButtonColor := ColorDialog1.Color;
    VirtualListviewActions.BkGndGradient1 := ColorDialog1.Color;
  end;
end;

procedure TForm1.ColorButton2Click(Sender: TObject);
begin
  ColorDialog1.Color := ColorButton2.ButtonColor;
  if ColorDialog1.Execute then
  begin
    ColorButton2.ButtonColor := ColorDialog1.Color;
    VirtualListviewActions.BkGndGradient2 := ColorDialog1.Color;
  end;
end;

procedure TForm1.ColorButton3Click(Sender: TObject);
begin
  ColorDialog1.Color := ColorButton3.ButtonColor;
  if ColorDialog1.Execute then
  begin
    ColorButton3.ButtonColor := ColorDialog1.Color;
    VirtualListviewActions.BkGndColor := ColorDialog1.Color;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: Integer;
  AnAction: TAction;
begin
  for i := 0 to ActionListMain.ActionCount - 1 do
  begin
    AnAction := ActionListMain.Actions[i] as TAction;
    if AnAction.Category = 'Layout Action' then
      ComboBoxLayoutActions.AddItem(AnAction.Caption, AnAction);
    if AnAction.Category = 'Layout Response' then
      ComboBoxLayoutResponses.AddItem(AnAction.Caption, AnAction);
  end;
  if ComboBoxLayoutResponses.Items.Count > 0 then
    ComboBoxLayoutResponses.ItemIndex := 0;
  if ComboBoxLayoutActions.Items.Count > 0 then
    ComboBoxLayoutActions.ItemIndex := 0;

  ColorButton1.ButtonColor := VirtualListviewActions.BkGndGradient1;
  ColorButton2.ButtonColor := VirtualListviewActions.BkGndGradient2;
  CheckBox5.Checked := VirtualListviewActions.BkGndGradientEnable;
  ColorButton3.ButtonColor := VirtualListviewActions.BkGndColor;
end;

function TForm1.GetFrameEditItem: TFrameItemEditor;
begin
  if not Assigned(FFrameEditItem) then
  begin
    FFrameEditItem := TFrameItemEditor.Create(Self);
    FFrameEditItem.Visible := False;
    FFrameEditItem.Align := alClient;
    FFrameEditItem.Parent := PanelFrameDock;
    FFrameEditItem.VirtualListviewActions := VirtualListviewActions;
    FFrameEditItem.VirtualListviewResponses := VirtualListviewResponses;
  end;
  Result := FFrameEditItem;
end;

function TForm1.GetFrameNewBlock: TFrameBlockName;
begin
  if not Assigned(FFrameNewBlock) then
  begin
    FFrameNewBlock := TFrameBlockName.Create(Self);
    FFrameNewBlock.Visible := False;
    FFrameNewBlock.Align := alClient;
    FFrameNewBlock.Parent := PanelFrameDock;
    FFrameNewBlock.VirtualListviewActions := VirtualListviewActions;
    FFrameNewBlock.VirtualListviewResponses := VirtualListviewResponses;
  end;
  Result := FFrameNewBlock;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  VirtualListviewActions.TextLayout := TVirtualListviewTextLayout(RadioGroup2.ItemIndex);
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);
begin
  VirtualListviewActions.ExpandImagePosition := TExpandImagePostition(RadioGroup3.ItemIndex);
end;

procedure TForm1.SetActiveFrame(AValue: TFrame);
begin
  if FActiveFrame = AValue then Exit;

  if Assigned(ActiveFrame) then
    ActiveFrame.Visible := False;

  FActiveFrame := AValue;
  FActiveFrame.Visible := True;
end;

procedure TForm1.SetDisplayedItem(AValue: TVirtualListviewItem);
begin
  if FDisplayedItem = AValue then Exit;
  FDisplayedItem := AValue;

  ActiveFrame := FrameEditItem;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  VirtualListviewActions.CaptionIndent := SpinEdit1.Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  VirtualListviewActions.DetailsIndent := SpinEdit2.Value;
end;

procedure TForm1.SpinEdit3Change(Sender: TObject);
begin
  VirtualListviewActions.CaptionLineCount := SpinEdit3.Value;
end;

procedure TForm1.SpinEdit4Change(Sender: TObject);
begin
  VirtualListviewActions.DefaultItemHeight := SpinEdit4.Value;
end;

procedure TForm1.VirtualListviewActionsFocusedChanged(Sender: TObject;
  FocusedItem, OldFocusedItem: TVirtualListviewItem);
begin
  DisplayedItem := FocusedItem;
end;

procedure TForm1.VirtualListviewActionsSelectedChanged(Sender: TObject);
begin
  LabelSelectedCount.Caption := 'Selected Count = ' + IntToStr(VirtualListviewActions.SelectedCount);
  LabelSelectedAddress.Caption := 'Selected Address = $' + IntToHex(Cardinal (VirtualListviewActions.SelectedItem), 8);
end;

procedure TForm1.VirtualListviewResponsesFocusedChanged(Sender: TObject;
  FocusedItem, OldFocusedItem: TVirtualListviewItem);
begin
  DisplayedItem := FocusedItem;
end;

end.

