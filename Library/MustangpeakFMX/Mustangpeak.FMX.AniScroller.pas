unit Mustangpeak.FMX.AniScroller;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, System.Generics.Collections,
  System.Generics.Defaults, System.UITypes, System.UIConsts, System.Types, FMX.Graphics,
  FMX.Ani, FMX.InertialMovement, System.Math, System.Rtti;

type
  TAniTargets = array of TAniCalculations.TTarget;
  TSwipeDirection = (Left, Right);

  TOnMouseClick = procedure(Button: TMouseButton; Shift: TShiftState; WorldX, WorldY: Single) of object;
  TOnSwipeStart = procedure(WorldX, WorldY: Single; SwipeDirection: TSwipeDirection) of object;
  TOnSwipeAction = procedure(WorldX, WorldY, Velocity: Single) of object;

type
  TFloatKinematicAnimation = class(TCustomPropertyAnimation)
  private
    FStartFloat: Single;
    FStopMaxValue: Single;
    FStartFromCurrent: Boolean;
    FInitialVelocity: Single;
    FAcceleration: Single;
    FInternalAcceleration: Single;
    procedure SetInitialVelocity(const Value: Single);
  protected
    property InternalAcceleration: Single read FInternalAcceleration write FInternalAcceleration;
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Acceleration: Single read FAcceleration write FAcceleration;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Loop default False;
    property InitialVelocity: Single read FInitialVelocity write SetInitialVelocity;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: Single read FStartFloat write FStartFloat stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopMaxValue: Single read FStopMaxValue write FStopMaxValue stored True nodefault;
    property Trigger;
    property TriggerInverse;
  end;


  TAniScroller = class
  private
    FAniCalcVert: TAniCalculations;
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
    FSwiping: Boolean;
    FSwipeCurrentPos: TPointF;
    FSwipeDeltaMoveStart: single;
    FSwipeMouseDownPoint: TPointF;
    FOnSwipeStart: TOnSwipeStart;
    FOnSwipeEnd: TOnSwipeAction;
    FOnSwipeMove: TOnSwipeAction;
    FSwipeLastTickCount: Cardinal;
    FSwipeVelocity: Single;
    function GetScrollOffsetMaxX: single;
    function GetScrollOffsetMaxY: single;
    function GetScrollOffsetX: single;
    function GetScrollOffsetY: single;
    procedure SetScrollOffsetX(const Value: single);
    procedure SetScrollOffsetY(const Value: single);
    function GetSwipeOffset: single;
  protected
    property AniCalcVert: TAniCalculations read FAniCalcVert write FAniCalcVert;
    property ClientRect: TRectF read FWorldRect write FWorldRect;
    property PreviousScrollPos: TPointF read FPreviousScrollPos write FPreviousScrollPos;
    property SwipeVelocity: Single read FSwipeVelocity write FSwipeVelocity;
    property SwipeCurrentPos: TPointF read FSwipeCurrentPos write FSwipeCurrentPos;
    property SwipeLastTickCount: Cardinal read FSwipeLastTickCount write FSwipeLastTickCount;
    property ScrollMouseDownPoint: TPointF read FScrollMouseDownPoint write FScrollMouseDownPoint;
    property SwipeMouseDownPoint: TPointF read FSwipeMouseDownPoint write FSwipeMouseDownPoint;
    property ScrollDeltaMoveStart: single read FScrollDeltaMoveStart write FScrollDeltaMoveStart;
    property SwipeDeltaMoveStart: single read FSwipeDeltaMoveStart write FSwipeDeltaMoveStart;

    procedure AniCalcStartVert(Sender: TObject);
    procedure AniCalcChangeVert(Sender: TObject);
    procedure AniCalcStopVert(Sender: TObject);
    procedure DoMouseClick(Button: TMouseButton; Shift: TShiftState; WorldX, WorldY: Single); virtual;
    procedure DoSwipeEnd(WorldX, WorldY: Single); virtual;
    procedure DoSwipeMove(WorldX, WorldY: Single); virtual;
    procedure DoSwipeStart(WorldX, WorldY: Single; SwipeDirection: TSwipeDirection); virtual;

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
    property SwipeOffset: single read GetSwipeOffset;
    property LineScroll: real read FLineScroll write FLineScroll;
    property OnMouseClick: TOnMouseClick read FOnMouseClick write FOnMouseClick;
    property OnSwipeEnd: TOnSwipeAction read FOnSwipeEnd write FOnSwipeEnd;
    property OnSwipeMove: TOnSwipeAction read FOnSwipeMove write FOnSwipeMove;
    property OnSwipeStart: TOnSwipeStart read FOnSwipeStart write FOnSwipeStart;

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

procedure TAniScroller.AniCalcChangeVert(Sender: TObject);
begin
  if (PreviousScrollPos.X <> AniCalcVert.ViewportPositionF.X) or (PreviousScrollPos.Y <> AniCalcVert.ViewportPositionF.Y) then
  begin
    OwnerControl.InvalidateRect(OwnerControl.LocalRect);
    PreviousScrollPos := AniCalcVert.ViewportPositionF;
  end;
end;

procedure TAniScroller.AniCalcStartVert(Sender: TObject);
begin
  if OwnerControl.Scene <> nil then
    OwnerControl.Scene.ChangeScrollingState(OwnerControl, True);
  FScrolling := True;
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
  FAniCalcVert := TAniCalculations.Create(OwnerControl);
  AniCalcVert.Animation := True;
  AniCalcVert.Averaging := True;
  AniCalcVert.OnChanged := AniCalcChangeVert;
  AniCalcVert.Interval := 8;
  AniCalcVert.OnStart := AniCalcStartVert;
  AniCalcVert.OnStop := AniCalcStopVert;
  AniCalcVert.BoundsAnimation := True;     //FPullToRefresh.Enabled;
  AniCalcVert.TouchTracking := [ttVertical];

  FLineScroll := 44;
  FScrollDeltaMoveStart := 5.0; // move 5 before a mouse action is called a scroll
  FSwipeDeltaMoveStart := 5.0;
end;

destructor TAniScroller.Destroy;
begin
  AniCalcVert.DisposeOf;
  FAniCalcVert := nil;
  inherited;
end;

procedure TAniScroller.DoMouseClick(Button: TMouseButton; Shift: TShiftState; WorldX, WorldY: Single);
begin
  if Assigned(OnMouseClick) then
    OnMouseClick(Button, Shift, WorldX, WorldY);
end;

procedure TAniScroller.DoSwipeEnd(WorldX, WorldY: Single);
begin
  if Assigned(OnSwipeEnd) then
    OnSwipeEnd(WorldX, WorldY, SwipeVelocity);
end;

procedure TAniScroller.DoSwipeMove(WorldX, WorldY: Single);
begin
  if Assigned(OnSwipeMove) then
    OnSwipeMove(WorldX, WorldY, SwipeVelocity);
end;

procedure TAniScroller.DoSwipeStart(WorldX, WorldY: Single; SwipeDirection: TSwipeDirection);
begin
  if Assigned(OnSwipeStart) then
    OnSwipeStart(WorldX, WorldY, SwipeDirection);
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

function TAniScroller.GetSwipeOffset: single;
begin
  Result := SwipeCurrentPos.X - SwipeMouseDownPoint.X
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
          SwipeCurrentPos := TPointF.Create(X, Y)
        else begin
          FIsMouseLeftButtonDown := True;
          FScrollMouseDownPoint := TPointF.Create(X, Y);        // get ready to detect a scroll
          FSwipeMouseDownPoint := TPointF.Create(X, Y);         // get ready to detect a swipe
        end;
      end;
    TMouseButton.mbRight:
      begin
        FIsMouseRightButtonDown := True;
      end;
    TMouseButton.mbMiddle:
      begin
        FIsMouseMiddleButtonDown := True;
      end;
  end;
end;

procedure TAniScroller.MouseLeave;
begin
  AniCalcVert.MouseLeave;
end;

procedure TAniScroller.MouseMove(Shift: TShiftState; X, Y: Single);
var
  CurrentTime: Cardinal;
begin
  if Scrolling then
    AniCalcVert.MouseMove(X, Y)
  else
  if Swiping then
  begin
    CurrentTime := TThread.GetTickCount * 1000;
    SwipeVelocity := (Y - SwipeCurrentPos.Y) / (CurrentTime - SwipeLastTickCount);
    SwipeLastTickCount := CurrentTime;
    SwipeCurrentPos := TPointF.Create(X, Y);
    DoSwipeMove(X + ScrollOffsetX, Y + ScrollOffsetY);
  end else
  begin
    if IsMouseLeftButtonDown then
    begin
      // Only a y move will start a scroll, a x move will start a swipe
      if (Y < ScrollMouseDownPoint.Y - ScrollDeltaMoveStart) or (Y > ScrollMouseDownPoint.Y + ScrollDeltaMoveStart) then
        AniCalcVert.MouseDown(X, Y)  // Start from the current position
      else
      // Only a x move will start a swipe, a t move will start a scroll
      if (X < ScrollMouseDownPoint.X - SwipeDeltaMoveStart) then
      begin
        FSwiping := True;
        SwipeLastTickCount := TThread.GetTickCount;
        SwipeVelocity := 0;
        DoSwipeStart(X + ScrollOffsetX, Y + ScrollOffsetY, TSwipeDirection.Right);
      end else
      if (X > ScrollMouseDownPoint.X + SwipeDeltaMoveStart) then
      begin
        FSwiping := True;
        SwipeLastTickCount := TThread.GetTickCount;
        SwipeVelocity := 0;
        DoSwipeStart(X + ScrollOffsetX, Y + ScrollOffsetY, TSwipeDirection.Left);
      end;
    end;
  end;
end;

procedure TAniScroller.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Single);
begin
  if Scrolling then
    AniCalcVert.MouseUp(X, Y)
  else
  if Swiping then
  begin
    DoSwipeEnd(X + ScrollOffsetX, Y + ScrollOffsetY);
    FSwiping := False;
  end else
    DoMouseClick(Button, Shift, X + ScrollOffsetX, Y + ScrollOffsetY);

  case Button of
    TMouseButton.mbLeft: FIsMouseLeftButtonDown := False;
    TMouseButton.mbRight: FIsMouseRightButtonDown := False;
    TMouseButton.mbMiddle: FIsMouseMiddleButtonDown := False;
  end;
end;

procedure TAniScroller.SetWorldRect(ARect: TRectF);
var
  AniTargets: TAniTargets;
begin
  FWorldRect := ARect;
  SetLength(AniTargets, 2);
  AniTargets[0].TargetType := TAniCalculations.TTargetType.Min;
  AniTargets[0].Point := TPointD.Create(0, 0);
  AniTargets[1].TargetType := TAniCalculations.TTargetType.Max;
  AniTargets[1].Point := TPointD.Create(ScrollOffsetMaxX, ScrollOffsetMaxY);
  AniCalcVert.SetTargets(AniTargets);
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

{ TFloatKinematicAnimation }

constructor TFloatKinematicAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartFloat := 0;
  FStopMaxValue := 0;
  FAcceleration := 9.8;
  FInitialVelocity := 0;
end;

procedure TFloatKinematicAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
  StopValue: Single;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        StartValue := P.GetValue(FInstance).AsExtended;
    end;
  end;
  InternalAcceleration := 2*(StopMaxValue-(InitialVelocity*Duration)/(Duration*Duration));
end;

procedure TFloatKinematicAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
  s: Single;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      s := StartValue + (InitialVelocity*CurrentTime+(0.5*InternalAcceleration*CurrentTime*CurrentTime));
      if s > StopMaxValue then
        s := StopMaxValue;
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        P.SetValue(FInstance, s);
    end;
  end;
end;

procedure TFloatKinematicAnimation.SetInitialVelocity(const Value: Single);
begin
  FInitialVelocity := Value;
  if FInitialVelocity = 0 then
    FInitialVelocity := 100;
end;

end.
