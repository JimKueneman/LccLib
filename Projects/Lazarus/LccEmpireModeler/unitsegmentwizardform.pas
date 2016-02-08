unit unitsegmentwizardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFormSegmentWizard }

  TFormSegmentWizard = class(TForm)
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
  FormSegmentWizard: TFormSegmentWizard;

implementation

{$R *.lfm}

{ TFormSegmentWizard }

procedure TFormSegmentWizard.FormShow(Sender: TObject);
begin
  EditName.Text := '[Segment]';
  EditDescription.Text := '';
  ComboBoxClass.ItemIndex := 0;
  EditName.SetFocus;
end;

end.

