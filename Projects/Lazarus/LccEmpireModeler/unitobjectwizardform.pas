unit unitobjectwizardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, turnoutoptions, signalheadoptions;

type

  { TFormObjectWizard }

  TFormObjectWizard = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBoxClass: TComboBox;
    EditDescription: TEdit;
    EditName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelObjectOptions: TPanel;
    procedure ComboBoxClassChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOptionPlugin: TForm;
    procedure SetOptionPlugin(AValue: TForm);
  private
    { private declarations }
    property OptionPlugin: TForm read FOptionPlugin write SetOptionPlugin;
  public
    { public declarations }
  end;

var
  FormObjectWizard: TFormObjectWizard;

implementation

{$R *.lfm}

{ TFormObjectWizard }

procedure TFormObjectWizard.ComboBoxClassChange(Sender: TObject);
begin
  if ComboBoxClass.Caption = 'Turnout' then
  begin
     OptionPlugin := TFormTurnoutOptions.Create(Self);
     OptionPlugin.Align := alClient;
     OptionPlugin.Parent := PanelObjectOptions;
     OptionPlugin.Show;
  end else
  if ComboBoxClass.Caption = 'Signal Head' then
  begin
     OptionPlugin := TFormSignalHeadOptions.Create(Self);
     OptionPlugin.Align := alClient;
     OptionPlugin.Parent := PanelObjectOptions;
     OptionPlugin.Show;
  end else
  begin
    if Assigned(FOptionPlugin) then
    begin
      OptionPlugin.Close;
      FreeAndNil(FOptionPlugin);
    end;
  end;
end;

procedure TFormObjectWizard.FormShow(Sender: TObject);
begin
  EditName.Text := '[Object]';
  EditDescription.Text := '';
  ComboBoxClass.ItemIndex := 0;
  EditName.SetFocus;
end;

procedure TFormObjectWizard.SetOptionPlugin(AValue: TForm);
begin
  if FOptionPlugin = AValue then Exit;
  if Assigned(FOptionPlugin) then
  begin
    OptionPlugin.Close;
    FreeAndNil(FOptionPlugin);
  end;
  FOptionPlugin := AValue;
end;

end.

