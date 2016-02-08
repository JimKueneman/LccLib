unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, ActnList, lcc_sdn_utilities, types, unitsegmentwizardform,
  unitobjectwizardform;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionDeleteOutputAction: TAction;
    ActionAddNewOutputAction: TAction;
    ActionDeleteInputAction: TAction;
    ActionAddNewInputAction: TAction;
    ActionDeleteObject: TAction;
    ActionAddNewObject: TAction;
    ActionDeleteSegment: TAction;
    ActionAddNewSegment: TAction;
    ActionList: TActionList;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LccSdnController: TLccSdnController;
    ListViewSegments: TListView;
    ListViewObjects: TListView;
    ListViewInputActions: TListView;
    ListViewOuputActions: TListView;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Splitter1: TSplitter;
    TabSheetActionEditor: TTabSheet;
    TabSheetActionXML: TTabSheet;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton2: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure ActionAddNewInputActionExecute(Sender: TObject);
    procedure ActionAddNewObjectExecute(Sender: TObject);
    procedure ActionAddNewSegmentExecute(Sender: TObject);
    procedure ActionDeleteInputActionExecute(Sender: TObject);
    procedure ActionDeleteObjectExecute(Sender: TObject);
    procedure ActionDeleteOutputActionExecute(Sender: TObject);
    procedure ActionDeleteSegmentExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewObjectsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ListViewSegmentsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    { private declarations }
    procedure ListviewDeleteSelected(Listview: TListview);
    function AddListviewItem(Listview: TListview; AName, ADescription, AClass: string; AnLccClass: TObject): TListItem;
    procedure ClearListviews;
    procedure RebuildController;
    procedure LockListviews;
    procedure UnlockListviews;
    procedure UpdateUI;
  public
    { public declarations }
    function SelectedSegement: TLccSegment;
    function SelectedObject: TLccObject;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ActionAddNewInputActionExecute(Sender: TObject);
begin
  Beep;
end;

procedure TForm1.ActionAddNewObjectExecute(Sender: TObject);
var
  ListItem: TListItem;
  LccObject: TLccObject;
  LccSegment: TLccSegment;
begin
  if FormObjectWizard.ShowModal = mrOk then
  begin
    LccSegment := SelectedSegement;
    if Assigned(LccSegment) then
    begin
      LccObject := TLccObject.Create;
      LccObject.Name := FormObjectWizard.EditName.Text;
      LccObject.Description := FormObjectWizard.EditDescription.Text;
      LccObject.LccClass := FormObjectWizard.ComboBoxClass.Text;
      ListItem := AddListviewItem(ListViewObjects, LccObject.Name, LccObject.Description, LccObject.LccClass, LccObject);
      LccSegment.LccObjects.Add(LccObject);
      ListItem.Selected := True;
      UpdateUI
    end;
  end;
end;

procedure TForm1.ActionAddNewSegmentExecute(Sender: TObject);
var
  LccSegment: TLccSegment;
  ListItem: TListItem;
begin
  if FormSegmentWizard.ShowModal = mrOk then
  begin
    LccSegment := TLccSegment.Create;
    LccSegment.Name := FormSegmentWizard.EditName.Text;
    LccSegment.Description := FormSegmentWizard.EditDescription.Text;
    LccSegment.LccClass := FormSegmentWizard.ComboBoxClass.Text;
    LccSdnController.LccSegments.Add(LccSegment);
    ListItem := AddListviewItem(ListViewSegments, LccSegment.Name, LccSegment.Description, LccSegment.LccClass, LccSegment);
    ListItem.Selected := True;
    UpdateUI
  end;
end;

procedure TForm1.ActionDeleteInputActionExecute(Sender: TObject);
begin
  ListviewDeleteSelected(ListViewInputActions);
  UpdateUI
end;

procedure TForm1.ActionDeleteObjectExecute(Sender: TObject);
begin
  ListviewDeleteSelected(ListViewObjects);
  UpdateUI
end;

procedure TForm1.ActionDeleteOutputActionExecute(Sender: TObject);
begin
  ListviewDeleteSelected(ListViewOuputActions);
  UpdateUI;
end;

procedure TForm1.ActionDeleteSegmentExecute(Sender: TObject);
begin
  ListviewDeleteSelected(ListViewSegments);
  UpdateUI;
end;

function TForm1.AddListviewItem(Listview: TListview; AName, ADescription, AClass: string; AnLccClass: TObject): TListItem;
begin
  Result := Listview.Items.Add;
  Result.Caption := AName;
  Result.SubItems.Add(ADescription);
  Result.SubItems.Add(AClass);
  Result.Data := AnLccClass;
end;

procedure TForm1.ClearListviews;
begin
  ListViewSegments.Clear;
  ListViewInputActions.Clear;
  ListViewObjects.Clear;
  ListViewOuputActions.Clear;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  UpdateUI
end;

procedure TForm1.ListviewDeleteSelected(Listview: TListview);
var
  i: Integer;
  Lcc: TObject;
  LccSegment: TLccSegment;
  LccObject: TLccObject;
begin
  Listview.BeginUpdate;
  try
    for i := Listview.Items.Count - 1 downto 0 do
    begin
      if Listview.Items[i].Selected then
      begin
        if TObject( Listview.Items[i].Data) is TLccSegment then
        begin
          LccSegment := TLccSegment( Listview.Items[i].Data);
          Listview.Items[i].Data := nil;
          LccSdnController.LccSegments.Remove(LccSegment);
          Listview.Items.Delete(i);
        end else
        if TObject( Listview.Items[i].Data) is TLccObject then
        begin
          LccObject := TLccObject( Listview.Items[i].Data);
          Listview.Items[i].Data := nil;
          LccSegment := SelectedSegement;
          LccSegment.LccObjects.Remove(LccObject);
          Listview.Items.Delete(i);
        end;
      end;
    end;
  finally
    Listview.EndUpdate;
  end;
end;

procedure TForm1.ListViewObjectsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  LccObject: TLccObject;
  i, j: Integer;
begin
  LockListviews;
  try
    if ListViewSegments.SelCount = 0 then
    begin
      ListViewOuputActions.Clear;
      ListViewInputActions.Clear;
    end else
    begin
      if Selected and Assigned(ListViewObjects.Selected) then
      begin
        ListViewOuputActions.Clear;
        ListViewInputActions.Clear;
        LccObject := TLccObject( ListViewObjects.Selected.Data);
        for i := 0 to LccObject.InputActionGroups.Count - 1 do
          for j := 0 to LccObject.InputActionGroup[i].Actions.Count - 1 do
            AddListviewItem(ListViewInputActions, LccObject.InputActionGroup[i].Action[j].Name, LccObject.InputActionGroup[i].Action[j].Description, LccObject.InputActionGroup[i].Action[j].LccClass, LccObject.InputActionGroup[i].Action[j]);
        for i := 0 to LccObject.OutputActionGroups.Count - 1 do
          for j := 0 to LccObject.OutputActionGroup[i].Actions.Count - 1 do
            AddListviewItem(ListViewOuputActions, LccObject.OutputActionGroup[i].Action[j].Name, LccObject.OutputActionGroup[i].Action[j].Description, LccObject.OutputActionGroup[i].Action[j].LccClass, LccObject.OutputActionGroup[i].Action[j]);
      end;
    end;
  finally
    UnlockListviews;
    UpdateUI;
  end;
end;

procedure TForm1.ListViewSegmentsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  LccSegment: TLccSegment;
  i: Integer;
begin
  LockListviews;
  try
    if ListViewSegments.SelCount = 0 then
    begin
      ListViewObjects.Clear;
      ListViewOuputActions.Clear;
      ListViewInputActions.Clear;
    end else
    begin
      if Selected and Assigned(ListviewSegments.Selected.Data) then
      begin
        ListViewObjects.Clear;
        ListViewOuputActions.Clear;
        ListViewInputActions.Clear;
        LccSegment := TLccSegment( ListviewSegments.Selected.Data);
        for i := 0 to LccSegment.LccObjects.Count - 1 do
          AddListviewItem(ListViewObjects, LccSegment.LccObject[i].Name, LccSegment.LccObject[i].Description, LccSegment.LccObject[i].LccClass, LccSegment.LccObject[i]);
      end;
    end;
  finally
    UnlockListviews;
    UpdateUI;
  end;
end;

procedure TForm1.LockListviews;
begin
  ListViewSegments.BeginUpdate;
  ListViewInputActions.BeginUpdate;
  ListViewObjects.BeginUpdate;
  ListViewOuputActions.BeginUpdate;
end;

procedure TForm1.RebuildController;
var
  i: Integer;
  ListItem: TListItem;
begin
  LockListviews;
  try
    ClearListviews;
    for i := 0 to LccSdnController.LccSegments.Count - 1 do
      AddListviewItem(ListViewSegments, LccSdnController.LccSegment[i].Name, LccSdnController.LccSegment[i].Description, LccSdnController.LccSegment[i].LccClass, LccSdnController.LccSegment[i]);
  finally
    UnlockListviews
  end;
end;

function TForm1.SelectedObject: TLccObject;
begin
  Result := nil;
  if ListViewObjects.SelCount > 0 then
    Result := TLccObject( ListViewObjects.Selected.Data);
end;

function TForm1.SelectedSegement: TLccSegment;
begin
  Result := nil;
  if ListViewSegments.SelCount > 0 then
    Result := TLccSegment( ListViewSegments.Selected.Data);
end;

procedure TForm1.UnlockListviews;
begin
  ListViewSegments.EndUpdate;
  ListViewInputActions.EndUpdate;
  ListViewObjects.EndUpdate;
  ListViewOuputActions.EndUpdate;
end;

procedure TForm1.UpdateUI;
begin
  ActionAddNewInputAction.Enabled := ListViewObjects.SelCount = 1;
  ActionDeleteInputAction.Enabled := (ListViewObjects.SelCount = 1) and (ListViewInputActions.SelCount > 0);
  ActionAddNewOutputAction.Enabled := ListViewObjects.SelCount = 1;
  ActionDeleteOutputAction.Enabled := (ListViewObjects.SelCount = 1) and (ListViewOuputActions.SelCount > 0);

  ActionAddNewObject.Enabled := ListViewSegments.SelCount = 1;
  ActionDeleteObject.Enabled := (ListViewSegments.SelCount = 1) and (ListViewObjects.SelCount > 0);

  ActionAddNewSegment.Enabled := True;
  ActionDeleteSegment.Enabled := (ListViewSegments.SelCount > 0);
end;

end.

