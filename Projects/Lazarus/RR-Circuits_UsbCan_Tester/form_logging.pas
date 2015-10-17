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
    procedure FormHide(Sender: TObject);
  private
    FOnHideNotifyEvent: TNotifyEvent;
    { private declarations }
  public
    { public declarations }
    property OnHideNotifyEvent: TNotifyEvent read FOnHideNotifyEvent write FOnHideNotifyEvent;
  end;

var
  FormLogging: TFormLogging;

implementation

{$R *.lfm}

{ TFormLogging }

procedure TFormLogging.FormHide(Sender: TObject);
begin
  if Assigned(OnHideNotifyEvent) then
    OnHideNotifyEvent(Self)
end;

end.

