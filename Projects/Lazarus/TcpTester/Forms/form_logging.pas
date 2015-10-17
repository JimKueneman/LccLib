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
    procedure FormShow(Sender: TObject);
  private
    FOnHideNotify: TNotifyEvent;
    { private declarations }
  public
    { public declarations }
    property OnHideNotify: TNotifyEvent read FOnHideNotify write FOnHideNotify;
  end;

var
  FormLogging: TFormLogging;

implementation

{$R *.lfm}

{ TFormLogging }

procedure TFormLogging.FormShow(Sender: TObject);
begin
  FrameLccLogging.Visible := True;
end;

procedure TFormLogging.FormHide(Sender: TObject);
begin
  FrameLccLogging.Visible := False;
  if Assigned(OnHideNotify) then
    OnHideNotify(Self);
end;

end.

