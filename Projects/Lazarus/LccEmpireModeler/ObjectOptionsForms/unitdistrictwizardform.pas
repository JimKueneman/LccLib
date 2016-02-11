unit unitdistrictwizardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFormDistrictWizard }

  TFormDistrictWizard = class(TForm)
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
  FormSegmentWizard: TFormDistrictWizard;

implementation

{$R *.lfm}

{ TFormDistrictWizard }

procedure TFormDistrictWizard.FormShow(Sender: TObject);
begin
  EditName.Text := '[District]';
  EditDescription.Text := '';
  ComboBoxClass.ItemIndex := 0;
  EditName.SetFocus;
end;

end.

