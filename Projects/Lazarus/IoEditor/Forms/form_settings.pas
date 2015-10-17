unit form_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  frame_lcc_settings;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    FrameLccSettings1: TFrameLccSettings;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

end.

