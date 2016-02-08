unit signalheadoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList, StdCtrls, Spin;

type

  { TFormSignalHeadOptions }

  TFormSignalHeadOptions = class(TForm)
    ActionMoveDown: TAction;
    ActionMoveUp: TAction;
    ActionRemoveAspect: TAction;
    ActionAddAspect: TAction;
    ActionListSignalHeadOptions: TActionList;
    ImageListSignalHeadOptions: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelExternalsSwitches: TLabel;
    ListViewAspects: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEditBlocks: TSpinEdit;
    SpinEditTurnouts: TSpinEdit;
    SpinEditCTC: TSpinEdit;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ActionAddAspectExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionRemoveAspectExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewAspectsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    { private declarations }
    procedure UpdateUI;
  public
    { public declarations }
  end;

var
  FormSignalHeadOptions: TFormSignalHeadOptions;

implementation

{$R *.lfm}

{ TFormSignalHeadOptions }

procedure TFormSignalHeadOptions.ActionAddAspectExecute(Sender: TObject);
begin
  ListViewAspects.AddItem('New Aspect', nil);
  ListViewAspects.Items[ListViewAspects.Items.Count - 1].EditCaption;
end;

procedure TFormSignalHeadOptions.ActionMoveDownExecute(Sender: TObject);
begin
  if (ListViewAspects.SelCount > 0) and (ListViewAspects.Selected.Index < ListViewAspects.Items.Count - 1) then
  begin
    ListViewAspects.Items.Move(ListViewAspects.Selected.Index, ListViewAspects.Selected.Index + 1);
    UpdateUi
  end;
end;

procedure TFormSignalHeadOptions.ActionMoveUpExecute(Sender: TObject);
begin
  if (ListViewAspects.SelCount > 0) and (ListViewAspects.Selected.Index > 0) then
  begin
    ListViewAspects.Items.Move(ListViewAspects.Selected.Index, ListViewAspects.Selected.Index - 1);
    UpdateUI
  end;
end;

procedure TFormSignalHeadOptions.ActionRemoveAspectExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := ListViewAspects.Items.Count - 1 downto 0 do
  begin
    if ListViewAspects.Items[i].Selected then
      ListViewAspects.Items.Delete(i);
  end;
end;

procedure TFormSignalHeadOptions.FormShow(Sender: TObject);
begin
  ListViewAspects.AddItem('Red', nil);
  ListViewAspects.AddItem('Yellow', nil);
  ListViewAspects.AddItem('Green', nil);
  UpdateUI;
end;

procedure TFormSignalHeadOptions.ListViewAspectsSelectItem(Sender: TObject;Item: TListItem; Selected: Boolean);
begin
  UpdateUI
end;

procedure TFormSignalHeadOptions.UpdateUI;
begin
  ActionMoveUp.Enabled := Assigned( ListViewAspects.Selected) and (ListViewAspects.Selected.Index > 0);
  ActionMoveDown.Enabled := Assigned( ListViewAspects.Selected) and (ListViewAspects.Selected.Index < ListViewAspects.Items.Count - 1);
end;

end.

