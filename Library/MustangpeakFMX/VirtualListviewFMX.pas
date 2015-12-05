unit VirtualListviewFMX;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, System.Generics.Collections,
  System.Generics.Defaults, System.UITypes, System.UIConsts, System.Types, FMX.Graphics,
  FMX.Ani, FMX.InertialMovement, System.Math;

type
  TVirtualListItems = class;
  TCustomVirtualListviewFMX = class;

  TVirtualListItem = class(TPersistent)
  private
    FListviewItems: TVirtualListItems;
    FColor: TAlphaColor;
    FBoundsRect: TRectF;
  protected
    property ListviewItems: TVirtualListItems read FListviewItems;
    property BoundsRect: TRectF read FBoundsRect write FBoundsRect;

    procedure Paint(ACanvas: TCanvas; DrawingRect: TRectF);
  public
    property Color: TAlphaColor read FColor write FColor;

    constructor Create;
    destructor Destroy; override;
  end;

  TVirtualListItems = class(TPersistent)
  private
    FItems: TObjectList<TVirtualListItem>;
    FListview: TCustomVirtualListviewFMX;
    FCellColor: TAlphaColor;
    function GetCount: Integer;
    function GetItem(Index: Integer): TVirtualListItem;
    procedure SetItem(Index: Integer; const Value: TVirtualListItem);
    procedure SetCellColor(const Value: TAlphaColor);
  protected
    property CellColor: TAlphaColor read FCellColor write SetCellColor;
    property Items: TObjectList<TVirtualListItem> read FItems write FItems;
    property Listview: TCustomVirtualListviewFMX read FListview write FListview;

    procedure Paint(ACanvas: TCanvas; DrawingRect: TRectF);
    procedure RecalculateCellBoundsRects;
  public
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TVirtualListItem read GetItem write SetItem; default;

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Clear;
    function Add: TVirtualListItem;
  end;

  TAniTargets = array of TAniCalculations.TTarget;

  TCustomVirtualListviewFMX = class(TControl)
  private
    FItems: TVirtualListItems;
    FCellHeight: Integer;
    FCellColor: TAlphaColor;
    FAniCalc: TAniCalculations;
    FScrolling: Boolean;
    FAniTargets: TAniTargets;
    FPreviousScrollPos: TPointF;
    FClipRect: TClipRects;
    procedure SetCellHeight(const Value: Integer);
    procedure SetCellColor(const Value: TAlphaColor);
    function GetCellCount: Integer;
    procedure SetCellCount(const Value: Integer);
    function GetScrollOffsetX: single;
    function GetScrollOffsetY: single;
    procedure SetScrollOffsetX(const Value: single);
    procedure SetScrollOffsetY(const Value: single);
    function GetScrollOffsetMaxX: single;
    function GetScrollOffsetMaxY: single;
  protected
    procedure AniCalcStart(Sender: TObject);
    procedure AniCalcChange(Sender: TObject);
    procedure AniCalcStop(Sender: TObject);
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Paint; override;
    procedure RecalculateScrollOffsets;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;


    property AniCalc: TAniCalculations read FAniCalc write FAniCalc;
    property AniTargets: TAniTargets read FAniTargets write FAniTargets;
    property CellColor: TAlphaColor read FCellColor write SetCellColor default claWhite;
    property CellCount: Integer read GetCellCount write SetCellCount default 0;
    property CellHeight: Integer read FCellHeight write SetCellHeight default 44;
    property ClipRect: TClipRects read FClipRect write FClipRect;
    property Items: TVirtualListItems read FItems write FItems;
    property PreviousScrollPos: TPointF read FPreviousScrollPos write FPreviousScrollPos;
    property Scrolling: Boolean read FScrolling write FScrolling;
    property ScrollOffsetX: single read GetScrollOffsetX write SetScrollOffsetX;
    property ScrollOffsetMaxX: single read GetScrollOffsetMaxX;
    property ScrollOffsetY: single read GetScrollOffsetY write SetScrollOffsetY;
    property ScrollOffsetMaxY: single read GetScrollOffsetMaxY;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TVirtualListviewFMX = class(TCustomVirtualListviewFMX)
  public
    property Items;
  published
  //  property Appearence: TksListViewAppearence read FAppearence write FAppearence;
  //  property ItemHeight: integer read FItemHeight write SetKsItemHeight default 44;
  //  property HeaderHeight: integer read FHeaderHeight write SetKsHeaderHeight default 44;
  //  property ItemImageSize: integer read FItemImageSize write SetItemImageSize default 32;
 //   property OnEditModeChange;
  //  property OnEditModeChanging;
 //   property EditMode;
  //  property Transparent default False;
 //   property AllowSelection;
 //   property AlternatingColors;
//    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    {$IFDEF XE8_OR_NEWER}
    property Images;
    {$ENDIF}
 //   property ScrollViewPos;
 //   property SideSpace;
 //   property OnItemClick: TksListViewRowClickEvent read FOnItemClick write FOnItemClick;
 //   property OnItemClickRight: TksListViewRowClickEvent read FOnItemRightClick write FOnItemRightClick;
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property CellColor;
    property CellCount;
    property CellHeight;
 //   property CheckMarks: TksListViewCheckMarks read FCheckMarks write SetCheckMarks default ksCmNone;
 //   property CheckMarkStyle: TksListViewCheckStyle read FCheckMarkStyle write SetCheckMarkStyle default ksCmsDefault;
    property ClipChildren default True;
    property ClipParent default False;
 //   property Cursor default crDefault;
 //   property DeleteButton: TksDeleteButton read FDeleteButton write FDeleteButton;
    property DisableFocusEffect default True;
//    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
 //   property FullWidthSeparator: Boolean read FFullWidthSeparator write FFullWidthSeparator default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ScrollOffsetY;
    property ScrollOffsetX;

 //   property SelectOnRightClick: Boolean read FSelectOnRightClick write FSelectOnRightClick default False;
    property Size;
//    property RowIndicators: TksListViewRowIndicators read FRowIndicators write FRowIndicators;

    //property ShowIndicatorColors: Boolean read FShowIndicatorColors write SetShowIndicatorColors default False;
    property TabOrder;
    property TabStop;
    property Visible default True;
    property Width;
 //   property AfterRowCache: TksRowCacheEvent read FAfterRowCache write FAfterRowCache;
 //   property BeforeRowCache: TksRowCacheEvent read FBeforeRowCache write FBeforeRowCache;

 //   property OnSelectOption: TksOptionSelectionEvent read FOnOptionSelection write FOnOptionSelection;
 //   property OnEmbeddedEditChange: TksEmbeddedEditChange read FOnEmbeddedEditChange write FOnEmbeddedEditChange;
 //   property OnEmbeddedListBoxChange: TksEmbeddedListBoxChange read FOnEmbeddedListBoxChange write FOnEmbeddedListBoxChange;

 //   property OnSelectDate: TksListViewSelectDateEvent read FOnSelectDate write FOnSelectDate;
 //   property OnSelectPickerItem: TksListViewSelectPickerItem read FOnSelectPickerItem write FOnSelectPickerItem;
 //   property OnSearchFilterChanged: TksSearchFilterChange read FOnSearchFilterChanged write FOnSearchFilterChanged;
    { events }
    property OnApplyStyleLookup;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Keyboard events }
    property OnKeyDown;
    property OnKeyUp;
    { Mouse events }
    property OnCanFocus;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

 //   property PageCaching: TksPageCaching read FPageCaching write FPageCaching;
    property OnPainting;
    property OnPaint;
    property OnResize;

 //   property HelpContext;
 //   property HelpKeyword;
  //  property HelpType;

 //   property StyleLookup;
    property TouchTargetExpansion;

    property OnDblClick;

    { ListView selection events }
 //   property OnChange;
 //   property OnChangeRepainted;
    {$IFDEF XE8_OR_NEWER}
    property OnItemsChange;
    property OnScrollViewChange;
    property OnFilter;
    property PullRefreshWait;
    {$ENDIF}

 //   property OnDeletingItem;
 //   property OnDeleteItem: TKsDeleteItemEvent read FOnDeleteItem write FOnDeleteItem;
 //   property OnDeleteChangeVisible;
 //   property OnSearchChange;
 //   property OnPullRefresh;

 //   property AutoTapScroll;
 //   property AutoTapTreshold;
    //property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
//    property ShowSelection: Boolean read FShowSelection write SetShowSelection;
 //   property DisableMouseWheel;

//    property SearchVisible;
 //   property SearchAlwaysOnTop;
 //   property SelectionCrossfade;
 //   property PullToRefresh;
//    property KeepSelection: Boolean read FKeepSelection write FKeepSelection default False;
 //   property OnLongClick: TksListViewRowClickEvent read FOnLongClick write FOnLongClick;
 //   property OnSwitchClick: TksListViewClickSwitchEvent read FOnSwitchClicked write FOnSwitchClicked;
 //   property OnButtonClicked: TksListViewClickButtonEvent read FOnButtonClicked write FOnButtonClicked;
 //   property OnSegmentButtonClicked: TksListViewClickSegmentButtonEvent read FOnSegmentButtonClicked write FOnSegmentButtonClicked;
 //   property OnScrollFinish: TksListViewFinishScrollingEvent read FOnFinishScrolling write FOnFinishScrolling;
 //   property OnScrollLastItem: TNotifyEvent read FOnScrollLastItem write FOnScrollLastItem;
 //   property OnItemSwipe: TksItemSwipeEvent read FOnItemSwipe write FOnItemSwipe;
  //  property OnItemActionButtonClick: TksItemActionButtonClickEvent read FOnItemActionButtonClick write FOnItemActionButtonClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MustangpeakFMX', [TVirtualListviewFMX]);
end;

{ TCustomVirtualListviewFMX }
procedure TCustomVirtualListviewFMX.AniCalcChange(Sender: TObject);
begin
  if (PreviousScrollPos.X <> AniCalc.ViewportPositionF.X) or (PreviousScrollPos.Y <> AniCalc.ViewportPositionF.Y) then
  begin
    InvalidateRect(LocalRect);
  end;

 // if FScrolling then
  begin
  //  ScrollOffsetX := AniCalc.ViewportPosition.X;
  //  ScrollOffsetY := AniCalc.ViewportPosition.Y;
  end;
end;

procedure TCustomVirtualListviewFMX.AniCalcStart(Sender: TObject);
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(Self, True);

  FScrolling := True;
end;

procedure TCustomVirtualListviewFMX.AniCalcStop(Sender: TObject);
begin
  FScrolling := False;
 // FSwipeDirection := ksSwipeUnknown;

  if Scene <> nil then
    Scene.ChangeScrollingState(nil, False);
end;

constructor TCustomVirtualListviewFMX.Create(AOwner: TComponent);
begin
  inherited;
  FCellHeight := 44;
  Items := TVirtualListItems.Create(Self);
  Items.FListview := Self;
  FAniCalc := TAniCalculations.Create(Self);
  AniCalc.Animation := True;
  AniCalc.Averaging := True;
  AniCalc.OnChanged := AniCalcChange;
  AniCalc.Interval := 8;
  AniCalc.OnStart := AniCalcStart;
  AniCalc.OnStop := AniCalcStop;
  AniCalc.BoundsAnimation := True;     //FPullToRefresh.Enabled;
  AniCalc.TouchTracking := [ttVertical];
  SetLength(FAniTargets, 2);
  SetLength(FClipRect, 1);
end;

destructor TCustomVirtualListviewFMX.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TCustomVirtualListviewFMX.DoMouseLeave;
begin
  inherited;
  AniCalc.MouseLeave;
end;

function TCustomVirtualListviewFMX.GetCellCount: Integer;
begin
  Result := Items.Count
end;

function TCustomVirtualListviewFMX.GetScrollOffsetMaxX: single;
begin
  Result := Max(0 - Width, 0);
end;

function TCustomVirtualListviewFMX.GetScrollOffsetMaxY: single;
begin
  Result := Max((Items.Count * CellHeight) - Height, 0);
end;

function TCustomVirtualListviewFMX.GetScrollOffsetX: single;
begin
  Result := AniCalc.ViewportPositionF.X
end;

function TCustomVirtualListviewFMX.GetScrollOffsetY: single;
begin
  Result := AniCalc.ViewportPositionF.Y
end;

procedure TCustomVirtualListviewFMX.KeyDown(var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  inherited;
  if Key = vkDown then
  begin
    ScrollOffsetY := ScrollOffsetY + CellHeight;
  end else
  if Key = vkUp then
  begin
    ScrollOffsetY := ScrollOffsetY - CellHeight;
  end;
end;

procedure TCustomVirtualListviewFMX.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  Capture;
 // AniCalc.BoundsAnimation := FPullToRefresh.Enabled;
  AniCalc.MouseDown(x, y);
  Scrolling := True;
end;

procedure TCustomVirtualListviewFMX.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Scrolling then
    AniCalc.MouseMove(X, Y);
end;

procedure TCustomVirtualListviewFMX.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Scrolling then
    AniCalc.MouseUp(X, Y);
  Scrolling := False;
  AniCalc.BoundsAnimation := True;
end;

procedure TCustomVirtualListviewFMX.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if (not Handled) then
  begin
    if (ssHorizontal in Shift) then
    begin

    end else
    begin
   //   Offset := Height / 5;
   //   Offset := Offset * -1 * (WheelDelta / 120);
   //   ANewPos := Max(ScrollViewPos + Offset, 0);
   //   ANewPos := Min(ANewPos, (FMaxScrollPos));
   //   SetScrollViewPos(ANewPos);
    //  FAniCalc.ViewportPosition := TPointD.Create(0, FScrollPos);
   //   if Scrolling then
   //     AniCalc.MouseWheel();

      Handled := True;
    end
  end;
end;

procedure TCustomVirtualListviewFMX.Paint;
var
  VisibleRect: TRectF;
begin
  inherited;
  ClipRect[0] := LocalRect;
  ClipRect[0].Bottom := ClipRect[0].Bottom - 50;
  if Scene.Canvas.BeginScene(@FClipRect) then
  begin
    try
      VisibleRect := LocalRect;
      VisibleRect.Offset(ScrollOffsetX, ScrollOffsetY);
      Items.Paint(Scene.Canvas, VisibleRect);
    finally
      Scene.Canvas.EndScene;
    end;
  end;
end;

procedure TCustomVirtualListviewFMX.RecalculateScrollOffsets;
begin
  FAniTargets[0].TargetType := TAniCalculations.TTargetType.Min;
  FAniTargets[0].Point := TPointD.Create(0, 0);
  FAniTargets[1].TargetType := TAniCalculations.TTargetType.Max;
  FAniTargets[1].Point := TPointD.Create(ScrollOffsetMaxX, ScrollOffsetMaxY);
  AniCalc.SetTargets(FAniTargets);
end;

procedure TCustomVirtualListviewFMX.Resize;
begin
  inherited;
  Items.RecalculateCellBoundsRects;
  RecalculateScrollOffsets
end;

procedure TCustomVirtualListviewFMX.SetCellColor(const Value: TAlphaColor);
begin
  if FCellColor <> Value then
  begin
    FCellColor := Value;
    Items.CellColor := Value;
  end;
end;

procedure TCustomVirtualListviewFMX.SetCellCount(const Value: Integer);
var
  i: Integer;
begin
  Items.Clear;
  for i := Items.Count to Value - 1 do
    Items.Add;
end;

procedure TCustomVirtualListviewFMX.SetCellHeight(const Value: Integer);
begin
  if FCellHeight <> Value then
  begin
    FCellHeight := Value;
    Items.RecalculateCellBoundsRects;
  end;
end;

procedure TCustomVirtualListviewFMX.SetScrollOffsetX(const Value: single);
var
  Temp: single;
begin
  Temp := Max(Value, -Width);
  Temp := Min(Temp, ScrollOffsetMaxX + Width);
  if AniCalc.ViewportPositionF.X <> Temp then
  begin
    AniCalc.ViewportPositionF := TPointF.Create(Temp, ScrollOffsetY);
    InvalidateRect(LocalRect);
  end;
end;

procedure TCustomVirtualListviewFMX.SetScrollOffsetY(const Value: single);
var
  Temp: single;
begin
  Temp := Max(Value, -Height);
  Temp := Min(Temp, ScrollOffsetMaxY + Height);
  if AniCalc.ViewportPositionF.Y <> Temp then
  begin
    AniCalc.ViewportPositionF := TPointF.Create(ScrollOffsetX, Temp);
    InvalidateRect(LocalRect);
  end;
end;

{ TListviewItems }

function TVirtualListItems.Add: TVirtualListItem;
begin
  Result := TVirtualListItem.Create;
  Result.FListviewItems := Self;
  Result.FBoundsRect := RectF(0, Count*Listview.CellHeight, Listview.Width, (Count+1)*Listview.CellHeight);
  Result.Color := Listview.CellColor;
  Items.Add(Result);
  Listview.RecalculateScrollOffsets;
end;

procedure TVirtualListItems.Clear;
begin
  Items.Clear;
end;

constructor TVirtualListItems.Create(AOwner: TComponent);
begin
  Items := TObjectList<TVirtualListItem>.Create;
end;

destructor TVirtualListItems.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TVirtualListItems.GetCount: Integer;
begin
  Result := Items.Count
end;

function TVirtualListItems.GetItem(Index: Integer): TVirtualListItem;
begin
  Result := Items[Index]
end;

procedure TVirtualListItems.Paint(ACanvas: TCanvas; DrawingRect: TRectF);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Item[i].Paint(ACanvas, DrawingRect);
end;

procedure TVirtualListItems.SetCellColor(const Value: TAlphaColor);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Item[i].Color := Value;
end;

procedure TVirtualListItems.SetItem(Index: Integer; const Value: TVirtualListItem);
begin
  Items[Index] := Value;
end;

procedure TVirtualListItems.RecalculateCellBoundsRects;
var
  i: Integer;
  TempRect: TRectF;
begin
  for i := 0 to Items.Count - 1 do
  begin
    TempRect.Top := i * Listview.CellHeight;
    TempRect.Left := 0;
    TempRect.Bottom := TempRect.Top + Listview.CellHeight;
    TempRect.Right := Listview.Width;
    Items[i].FBoundsRect := TempRect;
  end;
end;

{ TListviewCell }

constructor TVirtualListItem.Create;
begin
  inherited;
  Color := claWhite;
end;

destructor TVirtualListItem.Destroy;
begin
  inherited;
end;

procedure TVirtualListItem.Paint(ACanvas: TCanvas; DrawingRect: TRectF);
var
  R: TRectF;
begin
  ACanvas.Fill.Color := Color;
  if DrawingRect.IntersectsWith(BoundsRect) then
  begin
    R := BoundsRect;
    OffsetRect(R, -DrawingRect.Left, -DrawingRect.Top);
    ACanvas.FillRect(R, 0.0, 0.0, [TCorner.TopLeft, TCorner.BottomRight], 1.0);
  end;
end;

end.
