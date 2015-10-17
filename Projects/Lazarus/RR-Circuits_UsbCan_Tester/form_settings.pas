unit form_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, ExtCtrls, StdCtrls, frame_lcc_settings, lcc_app_common_settings;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    CheckBoxAutoResolveLocalAddress: TCheckBox;
    FrameLccSettings: TFrameLccSettings;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
begin
  FrameLccSettings.SyncWithLccSettings;
end;

end.

