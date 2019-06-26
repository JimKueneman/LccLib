unit unitMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, ComCtrls, unitFrameItemEditor, virtuallistview,
  unitFrameBlockEvent;

type

  { TForm1 }

  TForm1 = class(TForm)
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
    ComboBoxLayoutActions: TComboBox;
    ComboBoxLayoutResponses: TComboBox;
    ImageListState: TImageList;
    ImageList: TImageList;
    ImageListExpand: TImageList;
    PanelFrameDock: TPanel;
    PanelAction: TPanel;
    PanelResponse: TPanel;
    PanelActionHeader: TPanel;
    PanelResponseHeader: TPanel;
    VirtualListviewActions: TVirtualListview;
    VirtualListviewResponses: TVirtualListview;
    procedure ActionNewBlockExecute(Sender: TObject);
    procedure ActionNewGenericActionExecute(Sender: TObject);
    procedure ActionNewGenericResponseExecute(Sender: TObject);
    procedure ButtonNewActionClick(Sender: TObject);
    procedure ButtonNewResponseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VirtualListviewActionsFocusedChanged(Sender: TObject; FocusedItem, OldFocusedItem: TVirtualListviewItem);
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

procedure TForm1.VirtualListviewActionsFocusedChanged(Sender: TObject;
  FocusedItem, OldFocusedItem: TVirtualListviewItem);
begin
  DisplayedItem := FocusedItem;
end;

procedure TForm1.VirtualListviewResponsesFocusedChanged(Sender: TObject;
  FocusedItem, OldFocusedItem: TVirtualListviewItem);
begin
  DisplayedItem := FocusedItem;
end;

end.

