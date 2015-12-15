unit Mustangpeak.FMX.AniScroller;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, System.Generics.Collections,
  System.Generics.Defaults, System.UITypes, System.UIConsts, System.Types, FMX.Graphics,
  FMX.Ani, FMX.InertialMovement, System.Math;

type
  TAniTargets = array of TAniCalculations.TTarget;

  TOnMouseClick = procedure(Button: TMouseButton; Shift: TShiftState; WorldX, WorldY: Single) of object;

type
  TAniScroller = class
  private
    FAniCalcVert: TAniCalculations;
    FAniTargets: TAniTargets;
    FPreviousScrollPos: TPointF;
    FScrolling: Boolean;
    FOwnerControl: TControl;
    FWorldRect: TRectF;
    FLineScroll: real;
    FIsMouseLeftButtonDown: Boolean;
    FIsMouseRightButtonDown: Boolean;
    FIsMouseMiddleButtonDown: Boolean;
    FScrollMouseDownPoint: TPointF;
    FScrollDeltaMoveStart: single;
    FOnMouseClick: TOnMouseClick;
    FAniCalcHorz: TAniCalculations;
    FSwiping: Boolean;
    FPreviousSwipePos: TPointF;
    FSwipeDeltaMoveStart: single;
    FSwipeMouseDownPoint: TPointF;
    function GetScrollOffsetMaxX: single;
    function GetScrollOffsetMaxY: single;
    function GetScrollOffsetX: single;
    function GetScrollOffsetY: single;
    procedure SetScrollOffsetX(const Value: single);
    procedure SetScrollOffsetY(const Value: single);
  protected
    property AniCalcHorz: TAniCalculations read FAniCalcHorz write FAniCalcHorz;
    property AniCalcVert: TAniCalculations read FAniCalcVert write FAniCalcVert;
    property AniTargets: TAniTargets read FAniTargets write FAniTargets;
    property ClientRect: TRectF read FWorldRect write FWorldRect;
    property PreviousScrollPos: TPointF read FPreviousScrollPos write FPreviousScrollPos;
    property PreviousSwipePos: TPointF read FPreviousSwipePos write FPreviousSwipePos;
    property ScrollMouseDownPoint: TPointF read FScrollMouseDownPoint write FScrollMouseDownPoint;
    property SwipeMouseDownPoint: TPointF read FSwipeMouseDownPoint write FSwipeMouseDownPoint;
    property ScrollDeltaMoveStart: single read FScrollDeltaMoveStart write FScrollDeltaMoveStart;
    property SwipeDeltaMoveStart: single read FSwipeDeltaMoveStart write FSwipeDeltaMoveStart;

    procedure AniCalcStartHorz(Sender: TObject);
    procedure AniCalcChangeHorz(Sender: TObject);
    procedure AniCalcStopHorz(Sender: TObject);
    procedure AniCalcStartVert(Sender: TObject);
    procedure AniCalcChangeVert(Sender: TObject);
    procedure AniCalcStopVert(Sender: TObject);
    procedure DoMouseClick(Button: TMouseButton; Shift: TShiftState; WorldX, WorldY: Single); virtual;
  public
    property IsMouseLeftButtonDown: Boolean read FIsMouseLeftButtonDown;
    property IsMouseRightButtonDown: Boolean read FIsMouseRightButtonDown;
    property IsMouseMiddleButtonDown: Boolean read FIsMouseMiddleButtonDown;
    property OwnerControl: TControl read FOwnerControl;
    property Scrolling: Boolean read FScrolling write FScrolling;
    property ScrollOffsetX: single read GetScrollOffsetX write SetScrollOffsetX;
    property ScrollOffsetMaxX: single read GetScrollOffsetMaxX;
    property ScrollOffsetY: single read GetScrollOffsetY write SetScrollOffsetY;
    property ScrollOffsetMaxY: single read GetScrollOffsetMaxY;
    property Swiping: Boolean read FSwiping;
    property LineScroll: real read FLineScroll write FLineScroll;
    property OnMouseClick: TOnMouseClick read FOnMouseClick write FOnMouseClick;

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure GetWindowRect(var ARect: TRectF);
    procedure GetViewportRect(var ARect: TRectF);
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure MouseLeave;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SetWorldRect(ARect: TRectF);
  end;

implementation

type
  TControlHack = class(TControl)

  end;

{ TAniScroller }

procedure TAniScroller.AniCalcChangeHorz(Sender: TObject);
begin
  if (PreviousSwipePos.X <> AniCalcHorz.ViewportPositionF.X) or (PreviousSwipePos.Y <> AniCalcHorz.ViewportPositionF.Y) then
  begin
    OwnerControl.InvalidateRect(OwnerControl.LocalRect);
    PreviousSwipePos := AniCalcHorz.ViewportPositionF;
  end;
end;

procedure TAniScroller.AniCalcChangeVert(Sender: TObject);
begin
  if (PreviousScrollPos.X <> AniCalcVert.ViewportPositionF.X) or (PreviousScrollPos.Y <> AniCalcVert.ViewportPositionF.Y) then
  begin
    OwnerControl.InvalidateRect(OwnerControl.LocalRect);
    PreviousScrollPos := AniCalcVert.ViewportPositionF;
  end;
end;

procedure TAniScroller.AniCalcStartHorz(Sender: TObject);
begin
  FSwiping := True;
end;

procedure TAniScroller.AniCalcStartVert(Sender: TObject);
begin
  if OwnerControl.Scene <> nil then
    OwnerControl.Scene.ChangeScrollingState(OwnerControl, True);
  FScrolling := True;
end;

procedure TAniScroller.AniCalcStopHorz(Sender: TObject);
begin
  FSwiping := False;
end;

procedure TAniScroller.AniCalcStopVert(Sender: TObject);
begin
  FScrolling := False;
  if OwnerControl.Scene <> nil then
    OwnerControl.Scene.ChangeScrollingState(nil, False);
end;

constructor TAniScroller.Create(AOwner: TComponent);
begin
  FOwnerControl := AOwner as TControl;
  SetLength(FAniTargets, 2);
  FAniCalcVert := TAniCalculations.Create(OwnerControl);
  AniCalcVert.Animation := True;
  AniCalcVert.Averaging := True;
  AniCalcVert.OnChanged := AniCalcChangeVert;
  AniCalcVert.Interval := 8;
  AniCalcVert.OnStart := AniCalcStartVert;
  AniCalcVert.OnStop := AniCalcStopVert;
  AniCalcVert.BoundsAnimation := True;     //FPullToRefresh.Enabled;
  AniCalcVert.TouchTracking := [ttVertical];

  FAniCalcHorz := TAniCalculations.Create(OwnerControl);
  AniCalcHorz.Animation := True;
  AniCalcHorz.Averaging := True;
  AniCalcHorz.Interval := 1;   // Scroll slower for a swipe
  AniCalcHorz.OnChanged := AniCalcChangeHorz;
  AniCalcHorz.OnStart := AniCalcStartHorz;
  AniCalcHorz.OnStop := AniCalcStopHorz;
  AniCalcHorz.BoundsAnimation := False;     //FPullToRefresh.Enabled;
  AniCalcHorz.TouchTracking := [ttHorizontal];

  FLineScroll := 44;
  FScrollDeltaMoveStart := 5.0; // move 5 before a mouse action is called a scroll
  FSwipeDeltaMoveStart := 5.0;
end;

destructor TAniScroller.Destroy;
begin
  FreeAndNil(FAniCalcVert);
  FreeAndNil(FAniCalcHorz);
  inherited;
end;

procedure TAniScroller.DoMouseClick(Button: TMouseButton; Shift: TShiftState; WorldX, WorldY: Single);
begin
  if Assigned(OnMouseClick) then
    OnMouseClick(Button, Shift, WorldX, WorldY);
end;

function TAniScroller.GetScrollOffsetMaxX: single;
begin
  Result := Max(0 - OwnerControl.Width, 0);
end;

function TAniScroller.GetScrollOffsetMaxY: single;
begin
  Result := Max( ClientRect.Height - OwnerControl.Height, 0);
end;

function TAniScroller.GetScrollOffsetX: single;
begin
  Result := Round( AniCalcVert.ViewportPositionF.X)
end;

function TAniScroller.GetScrollOffsetY: single;
begin
  Result := Round(AniCalcVert.ViewportPositionF.Y);
end;

procedure TAniScroller.GetViewportRect(var ARect: TRectF);
begin
  GetWindowRect(ARect);
  ARect.Offset(ScrollOffsetX, ScrollOffsetY);
end;

procedure TAniScroller.GetWindowRect(var ARect: TRectF);
begin
  ARect.Create(0, 0, OwnerControl.Width, OwnerControl.Height);
end;

procedure TAniScroller.KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkDown  : ScrollOffsetY := ScrollOffsetY + LineScroll;
    vkUp    : ScrollOffsetY := ScrollOffsetY - LineScroll;
    vkRight : ScrollOffsetX := ScrollOffsetX - LineScroll;
    vkLeft  : ScrollOffsetX := ScrollOffsetX - LineScroll;
    vkNext  : ScrollOffsetY := ScrollOffsetY + OwnerControl.Height;
    vkPrior : ScrollOffsetY := ScrollOffsetY - OwnerControl.Height;
    vkHome  : begin
                ScrollOffsetY := 0;
                ScrollOffsetX := 0;
              end;
    vkEnd :   begin
                ScrollOffsetY := ScrollOffsetMaxY;
                ScrollOffsetX := ScrollOffsetMaxY;
              end;
  end;
end;

procedure TAniScroller.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  TControlHack( FOwnerControl).Capture;
  case Button of
    TMouseButton.mbLeft:
      begin
        if Scrolling then
          AniCalcVert.MouseDown(X, Y)
        else
        if Swiping then
          AniCalcHorz.MouseDown(X, Y)
        else begin
          FIsMouseLeftButtonDown := True;
          FScrollMouseDownPoint.Create(X, Y);        // get ready to detect a scroll
          FSwipeMouseDownPoint.Create(X, Y);         // get ready to detect a swipe
        end;
      end;
    TMouseButton.mbRight: FIsMouseRightButtonDown := True;
    TMouseButton.mbMiddle: FIsMouseMiddleButtonDown := True;
  end;
end;

procedure TAniScroller.MouseLeave;
begin
  AniCalcVert.MouseLeave;
  AniCalcHorz.MouseLeave;
end;

procedure TAniScroller.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if Scrolling then
  begin
    AniCalcVert.MouseMove(X, Y)
  end else
  if Swiping then
    AniCalcHorz.MouseMove(X, Y)
  else begin
    if IsMouseLeftButtonDown then
    begin
      // Only a y move will start a scroll, a x move will start a swipe
      if (Y < ScrollMouseDownPoint.Y - ScrollDeltaMoveStart) or (Y > ScrollMouseDownPoint.Y - ScrollDeltaMoveStart) then
        AniCalcVert.MouseDown(X, Y)  // Start from the current position
      else
      // Only a x move will start a swipe, a t move will start a scroll
      if (X < ScrollMouseDownPoint.X - SwipeDeltaMoveStart) or (X > ScrollMouseDownPoint.X - SwipeDeltaMoveStart) then
        AniCalcHorz.MouseDown(X, Y);  // Start from the current position
    end;
  end;
end;

procedure TAniScroller.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Single);
begin
  if Scrolling then
    AniCalcVert.MouseUp(X, Y)
  else
  if Swiping then
    AniCalcHorz.MouseUp(X, Y)
  else
    DoMouseClick(Button, Shift, X + ScrollOffsetX, Y + ScrollOffsetY);

  case Button of
    TMouseButton.mbLeft: FIsMouseLeftButtonDown := False;
    TMouseButton.mbRight: FIsMouseRightButtonDown := False;
    TMouseButton.mbMiddle: FIsMouseMiddleButtonDown := False;
  end;
end;

procedure TAniScroller.SetWorldRect(ARect: TRectF);
begin
  FWorldRect := ARect;
  FAniTargets[0].TargetType := TAniCalculations.TTargetType.Min;
  FAniTargets[0].Point := TPointD.Create(0, 0);
  FAniTargets[1].TargetType := TAniCalculations.TTargetType.Max;
  FAniTargets[1].Point := TPointD.Create(ScrollOffsetMaxX, ScrollOffsetMaxY);
  AniCalcVert.SetTargets(FAniTargets);
end;

procedure TAniScroller.SetScrollOffsetX(const Value: single);
var
  Temp: single;
begin
  Temp := Max(Value, -OwnerControl.Width);
  Temp := Min(Temp, ScrollOffsetMaxX + OwnerControl.Width);
  if AniCalcVert.ViewportPositionF.X <> Temp then
  begin
    AniCalcVert.ViewportPositionF := TPointF.Create(Temp, ScrollOffsetY);
    OwnerControl.InvalidateRect(OwnerControl.LocalRect);
  end;
end;

procedure TAniScroller.SetScrollOffsetY(const Value: single);
var
  Temp: single;
begin
  Temp := Max(Value, -OwnerControl.Height);
  Temp := Min(Temp, ScrollOffsetMaxY + OwnerControl.Height);
  if AniCalcVert.ViewportPositionF.Y <> Temp then
  begin
    AniCalcVert.ViewportPositionF := TPointF.Create(ScrollOffsetX, Temp);
    OwnerControl.InvalidateRect(OwnerControl.LocalRect);
  end;
end;

end.
