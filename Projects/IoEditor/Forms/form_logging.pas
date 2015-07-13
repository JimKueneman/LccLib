unit form_logging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  frame_lcc_logging;

type

  { TFormLogging }

  TFormLogging = class(TForm)
    FrameLccLogging: TFrameLccLogging;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormLogging: TFormLogging;

implementation

{$R *.lfm}

end.

