unit signalheadoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList;

type

  { TFormSignalHeadOptions }

  TFormSignalHeadOptions = class(TForm)
    ActionRemoveAspect: TAction;
    ActionAddAspect: TAction;
    ActionListSignalHeadOptions: TActionList;
    ImageListSignalHeadOptions: TImageList;
    ListViewAspects: TListView;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure ActionAddAspectExecute(Sender: TObject);
    procedure ActionRemoveAspectExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
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
end;

end.

