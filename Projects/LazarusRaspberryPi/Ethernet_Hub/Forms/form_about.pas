unit form_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonOk: TButton;
    ImageLcc: TImage;
    LabelBuild: TLabel;
    LabelBuildDate: TLabel;
    LabelCPU: TLabel;
    LabelIcon: TLabel;
    LabelMyName: TLabel;
    LabelNodeExplorer: TLabel;
    LabelTargetCPU: TLabel;
    LabelTargetOperatingSystem: TLabel;
    LabelTargetOS: TLabel;
    LabelURLFreePascal: TLabel;
    LabelURLIcons: TLabel;
    LabelURLLazarus: TLabel;
    LabelWrittenIn: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.FormShow(Sender: TObject);
begin
  LabelBuildDate.Caption := {$I %DATE%} + ': ' + {$I %TIME%};
  LabelTargetOS.Caption := {$I %FPCTARGETOS%};
  LabelTargetCPU.Caption := {$I %FPCTARGETCPU%};
end;

end.

