unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, ActnList, Grids, Menus, lcc_sdn_utilities, types,
  unitdistrictwizardform, unitobjectwizardform;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionAddGeneralIO: TAction;
    ActionAddObjectSignal: TAction;
    ActionAddObjectBlock: TAction;
    ActionAddObjectCrossingGate: TAction;
    ActionAddObjectTurnout: TAction;
    ActionFileSave: TAction;
    ActionFileOpen: TAction;
    ActionDeleteOutputAction: TAction;
    ActionAddNewOutputAction: TAction;
    ActionDeleteInputAction: TAction;
    ActionAddNewInputAction: TAction;
    ActionDeleteObject: TAction;
    ActionAddNewObject: TAction;
    ActionDeleteDistrict: TAction;
    ActionAddNewDistrict: TAction;
    ActionList: TActionList;
    Button1: TButton;
    Button2: TButton;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelActionInputDescription: TLabel;
    LabelActionOutputDescription: TLabel;
    LabelObjectDescription: TLabel;
    LccSdnController: TLccSdnController;
    ListViewSegments: TListView;
    ListViewObjects: TListView;
    ListViewInputActions: TListView;
    ListViewOuputActions: TListView;
    MenuItemAddObjectCrossingGate: TMenuItem;
    MenuItemAddObjectBlock: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemAddObjectTurnout: TMenuItem;
    MenuItemAddObjectGeneralIO: TMenuItem;
    MenuItemAddObjectSignal: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PanelDistricts: TPanel;
    PanelObjects: TPanel;
    PanelRight: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    PopupMenuAddOutputAction: TPopupMenu;
    PopupMenuAddInputAction: TPopupMenu;
    PopupMenuAddObject: TPopupMenu;
    PopupMenuAddDistrict: TPopupMenu;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TabSheetActionEditor: TTabSheet;
    TabSheetActionXML: TTabSheet;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolButtonDistrictAdd: TToolButton;
    ToolButtonInputActionDelete: TToolButton;
    ToolButtonOutputActionAdd: TToolButton;
    ToolButtonOutputActionDelete: TToolButton;
    ToolButtonDistrictDelete: TToolButton;
    ToolButtonObjectAdd: TToolButton;
    ToolButtonObjectDelete: TToolButton;
    ToolButtonInputActionAdd: TToolButton;
    procedure ActionAddNewInputActionExecute(Sender: TObject);
    procedure ActionAddNewObjectExecute(Sender: TObject);
    procedure ActionAddNewDistrictExecute(Sender: TObject);
    procedure ActionAddObjectBlockExecute(Sender: TObject);
    procedure ActionAddObjectSignalExecute(Sender: TObject);
    procedure ActionAddObjectTurnoutExecute(Sender: TObject);
    procedure ActionAddObjectCrossingGateExecute(Sender: TObject);
    procedure ActionDeleteInputActionExecute(Sender: TObject);
    procedure ActionDeleteObjectExecute(Sender: TObject);
    procedure ActionDeleteOutputActionExecute(Sender: TObject);
    procedure ActionDeleteDistrictExecute(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
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
    function SelectedSegement: TLccDistrict;
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
  LccSegment: TLccDistrict;
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

procedure TForm1.ActionAddObjectBlockExecute(Sender: TObject);
begin
  beep;
end;

procedure TForm1.ActionAddObjectSignalExecute(Sender: TObject);
begin
  beep;
end;

procedure TForm1.ActionAddObjectTurnoutExecute(Sender: TObject);
begin
  beep;
end;

procedure TForm1.ActionAddObjectCrossingGateExecute(Sender: TObject);
begin
  beep;
end;

procedure TForm1.ActionAddNewDistrictExecute(Sender: TObject);
var
  LccSegment: TLccDistrict;
  ListItem: TListItem;
begin
  if FormDistrictWizard.ShowModal = mrOk then
  begin
    LccSegment := TLccDistrict.Create;
    LccSegment.Name := FormDistrictWizard.EditName.Text;
    LccSegment.Description := FormDistrictWizard.EditDescription.Text;
    LccSegment.LccClass := FormDistrictWizard.ComboBoxClass.Text;
    LccSdnController.LccDistricts.Add(LccSegment);
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

procedure TForm1.ActionDeleteDistrictExecute(Sender: TObject);
begin
  ListviewDeleteSelected(ListViewSegments);
  UpdateUI;
end;

procedure TForm1.ActionFileOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    LccSdnController.XMLParse(OpenDialog.FileName);
    RebuildController;
  end;
end;

procedure TForm1.ActionFileSaveExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    LccSdnController.XMLExport(SaveDialog.FileName);
  end;
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
  LccSegment: TLccDistrict;
  LccObject: TLccObject;
begin
  Listview.BeginUpdate;
  try
    for i := Listview.Items.Count - 1 downto 0 do
    begin
      if Listview.Items[i].Selected then
      begin
        if TObject( Listview.Items[i].Data) is TLccDistrict then
        begin
          LccSegment := TLccDistrict( Listview.Items[i].Data);
          Listview.Items[i].Data := nil;
          LccSdnController.LccDistricts.Remove(LccSegment);
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
  LccSegment: TLccDistrict;
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
        LccSegment := TLccDistrict( ListviewSegments.Selected.Data);
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
    for i := 0 to LccSdnController.LccDistricts.Count - 1 do
      AddListviewItem(ListViewSegments, LccSdnController.LccDistrict[i].Name, LccSdnController.LccDistrict[i].Description, LccSdnController.LccDistrict[i].LccClass, LccSdnController.LccDistrict[i]);
    if ListViewSegments.Items.Count > 0 then
      ListViewSegments.Items[0].Selected := True;
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

function TForm1.SelectedSegement: TLccDistrict;
begin
  Result := nil;
  if ListViewSegments.SelCount > 0 then
    Result := TLccDistrict( ListViewSegments.Selected.Data);
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

  ActionAddNewDistrict.Enabled := True;
  ActionDeleteDistrict.Enabled := (ListViewSegments.SelCount > 0);
end;

end.

