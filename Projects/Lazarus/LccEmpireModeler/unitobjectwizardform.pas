unit unitobjectwizardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFormObjectWizard }

  TFormObjectWizard = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBoxClass: TComboBox;
    EditName: TEdit;
    EditDescription: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormObjectWizard: TFormObjectWizard;

implementation

{$R *.lfm}

{ TFormObjectWizard }

procedure TFormObjectWizard.FormShow(Sender: TObject);
begin
  EditName.Text := '[Object]';
  EditDescription.Text := '';
  ComboBoxClass.ItemIndex := 0;
  EditName.SetFocus;
end;

end.

