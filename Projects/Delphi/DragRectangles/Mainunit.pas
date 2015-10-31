unit Mainunit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TMainForm = class(TForm)
    Layout1: TLayout;
    Rectangle1: TRectangle;
    procedure Rectangle1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Layout1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Layout1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

const
  MAX_RECTANGLES = 10;
  RECT_WIDTH = 80;
  RECT_HEIGHT = 80;

var
  Grab: boolean = false;
  Offset: tpointf;
  MovingRectangle: TRectangle;
  RectArray: array [0 .. MAX_RECTANGLES] of TRectangle;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: integer;
  TitleRect: TRectangle;
  TitleLabel: TLabel;
begin
  Layout1.HitTest := false; // want to pick rectangles
  for I := Low(RectArray) to high(RectArray) do
  begin
    RectArray[I] := TRectangle.Create(self);
    RectArray[I].Parent := Layout1;
    RectArray[I].OnMouseDown := Rectangle1MouseDown;
    RectArray[I].OnMouseUp := Layout1MouseUp;
    RectArray[I].Width := RECT_WIDTH;
    RectArray[I].Height := RECT_HEIGHT;
    RectArray[I].fill.Color := random($FFFFFF) or $FF000000;
    RectArray[I].Position.X := random(trunc(Layout1.Width - RECT_WIDTH));
    RectArray[I].Position.Y := random(trunc(Layout1.Height - RECT_HEIGHT));

    TitleRect := TRectangle.Create(self);
    TitleRect.fill.Color := Talphacolorrec.White;
    TitleRect.Position.X := 0;
    TitleRect.Position.Y := 0;
    TitleRect.Width := RECT_WIDTH;
    TitleRect.Height := 16;
    TitleRect.HitTest := false;

    TitleLabel := TLabel.Create(self);
    TitleLabel.StyledSettings:=[];
    TitleLabel.Font.Size:=12;
    TitleLabel.Text := 'Caption ' + I.ToString;
    TitleRect.AddObject(TitleLabel);

    RectArray[I].AddObject(TitleRect);
  end;
end;

procedure TMainForm.Layout1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  TestPt: tpointf;
begin
  if Grab and (ssleft in Shift) then
  begin
    // keep from dragging off Layout
    if X > (Layout1.Width + Offset.X - RECT_WIDTH) then
      X := Layout1.Width + Offset.X - RECT_WIDTH;
    if Y > (Layout1.Height + Offset.Y - RECT_HEIGHT) then
      Y := Layout1.Height + Offset.Y - RECT_HEIGHT;
    if X < Offset.X then
      X := Offset.X;
    if Y < Offset.Y then
      Y := Offset.Y;

    TestPt.X := X - Offset.X;
    TestPt.Y := Y - Offset.Y;


    MovingRectangle.Position.X := TestPt.X;
    MovingRectangle.Position.Y := TestPt.Y;
  end;
end;

procedure TMainForm.Layout1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Grab := false;
  // MouseUp automatically turns off mouse capture of Layout1
end;

procedure TMainForm.Rectangle1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  MovingRectangle := Sender as TRectangle;
  Offset.X := X;
  Offset.Y := Y;
  // sets mouse capture to Layout1
  Layout1.Root.Captured := Layout1;
  MovingRectangle.BringToFront; // optional
  MovingRectangle.Repaint;
  Grab := true;
end;

end.
