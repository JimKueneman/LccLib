unit VirtualListviewFMX;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, System.Generics.Collections,
  System.Generics.Defaults, System.UITypes, System.UIConsts, System.Types, FMX.Graphics,
  FMX.Ani, FMX.InertialMovement, System.Math, AniScrollerFMX, FMX.TextLayout,
  FMX.Forms, FMX.Platform, FMX.Edit;

type
  TVirtualListItems = class;
  TCustomVirtualListviewFMX = class;
  TVirtualListItem = class;

  TOnCustomDrawItem = procedure(Sender: TObject; Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean) of object;

  TVirtualEdit = class(TEdit)
  protected
    procedure DoPaint; override;
    procedure Paint; override;

  end;

  TVirtualListItem = class(TPersistent)
  private
    FListviewItems: TVirtualListItems;
    FColor: TAlphaColor;
    FBoundsRect: TRectF;
    FText: string;
    FTextLayout: TTextLayout;
    FFrame: TFrame;
    FControlEdit: TVirtualEdit;
    procedure SetBoundsRect(const Value: TRectF);
  protected
    property ControlEdit: TVirtualEdit read FControlEdit write FControlEdit;
    property BoundsRect: TRectF read FBoundsRect write SetBoundsRect;
    property ListviewItems: TVirtualListItems read FListviewItems;
    property TextLayout: TTextLayout read FTextLayout write FTextLayout;

    procedure Paint(ACanvas: TCanvas; ViewportRect: TRectF);
  public
    property Color: TAlphaColor read FColor write FColor;
    property Text: string read FText write FText;
    property Frame: TFrame read FFrame write FFrame;

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

    procedure Paint(ACanvas: TCanvas; ViewportRect: TRectF);
    procedure NormalizedItemRect(var ARect: TRectF);

  public
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TVirtualListItem read GetItem write SetItem; default;

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Clear;
    function Add: TVirtualListItem;
    function AddAsFrame(VirtualListviewCellFrame: TFrame): TVirtualListItem;
  end;

  TCustomVirtualListviewFMX = class(TControl)
  private
    FItems: TVirtualListItems;
    FCellHeight: Integer;
    FCellColor: TAlphaColor;
    FScroller: TAniScroller;
    FOnCustomDrawItem: TOnCustomDrawItem;
    FFont: TFont;
    FBorderColor: TAlphaColor;
    FBorderWidth: real;
    procedure SetCellHeight(const Value: Integer);
    procedure SetCellColor(const Value: TAlphaColor);
    function GetCellCount: Integer;
    procedure SetCellCount(const Value: Integer);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetBorderWidth(const Value: real);
  protected
    procedure DoCustomDrawItem(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean); virtual;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Paint; override;
    procedure RecalculateCellViewportRects(RecalcWorldRect: Boolean);
    procedure RecalculateWorldRect;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;

    property BorderWidth: real read FBorderWidth write SetBorderWidth;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default claDarkgray;
    property CellColor: TAlphaColor read FCellColor write SetCellColor default claWhite;
    property CellCount: Integer read GetCellCount write SetCellCount default 0;
    property CellHeight: Integer read FCellHeight write SetCellHeight default 44;
    property Font: TFont read FFont write FFont;
    property Items: TVirtualListItems read FItems write FItems;
    property OnCustomDrawItem: TOnCustomDrawItem read FOnCustomDrawItem write FOnCustomDrawItem;
    property Scroller: TAniScroller read FScroller write FScroller;
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
    property BorderColor;
    property BorderWidth;
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
    property Font;
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
    property Scroller;

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
    property OnCustomDrawItem;
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

var
  ScreenScale: real = 0;

procedure Register;
begin
  RegisterComponents('MustangpeakFMX', [TVirtualListviewFMX]);
end;

function GetScreenScale: single;
var
  Service: IFMXScreenService;
begin
  if ScreenScale > 0 then
  begin
    Result := ScreenScale;
    Exit;
  end;
  Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService(IFMXScreenService));
  Result := Service.GetScreenScale;
  {$IFDEF IOS}
  if Result < 2 then
   Result := 2;
  {$ENDIF}
  ScreenScale := Result;
end;

function LogicalPointsBasedOnPixels(LogicalSize: real; MinimumPixels: Integer): real;
var
  Pixels: real;
begin
  Pixels := Round(LogicalSize*ScreenScale);

  if (Pixels < MinimumPixels) then
    Pixels := MinimumPixels;

  Result := Pixels / ScreenScale;
end;

{ TCustomVirtualListviewFMX }

constructor TCustomVirtualListviewFMX.Create(AOwner: TComponent);
begin
  inherited;
  ClipChildren  := True;
  FCellHeight := 44;
  FBorderColor := claDarkgray;
  FBorderWidth := 0;
  FFont := TFont.Create;
  FScroller := TAniScroller.Create(Self);
  Items := TVirtualListItems.Create(Self);
  Items.FListview := Self;
  CanFocus := True;
end;

destructor TCustomVirtualListviewFMX.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FFont);
  inherited;
end;

procedure TCustomVirtualListviewFMX.DoCustomDrawItem(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean);
begin
  if Assigned(OnCustomDrawItem) then
    OnCustomDrawItem(Self, Item, WindowRect, ItemCanvas, TextLayout, Handled);
end;

procedure TCustomVirtualListviewFMX.DoMouseLeave;
begin
  inherited;
  Scroller.MouseLeave;
end;

function TCustomVirtualListviewFMX.GetCellCount: Integer;
begin
  Result := Items.Count
end;

procedure TCustomVirtualListviewFMX.KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  Scroller.KeyDown(Key, KeyChar, Shift);
end;

procedure TCustomVirtualListviewFMX.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  Capture;
  Scroller.MouseDown(Button, Shift, x, y);
end;

procedure TCustomVirtualListviewFMX.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  Scroller.MouseMove(Shift, X, Y);
end;

procedure TCustomVirtualListviewFMX.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  Scroller.MouseUp(Button, Shift, X, Y);
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
  ViewportRect: TRectF;
  SavedState: TCanvasSaveState;
  R: TRectF;
begin
  inherited;
  if Scene.Canvas.BeginScene then
  begin
    try
      SavedState := Scene.Canvas.SaveState;
      Scene.Canvas.Stroke.Color := BorderColor;
      Scene.Canvas.Stroke.Kind := TBrushKind.Solid;
      Scene.Canvas.Stroke.Thickness := LogicalPointsBasedOnPixels(BorderWidth, 0);
      Scene.Canvas.IntersectClipRect(LocalRect);
      Scene.Canvas.DrawRect(LocalRect, 0, 0, [TCorner.TopLeft, TCorner.BottomRight], 1.0 );
      Scene.Canvas.RestoreState(SavedState);

      R := LocalRect;
      R.Inflate(-BorderWidth/2, -BorderWidth/2);

      SavedState := Scene.Canvas.SaveState;
      Scene.Canvas.IntersectClipRect(R);
      Scroller.GetViewportRect(ViewportRect);
      Items.Paint(Scene.Canvas, ViewportRect);
      Scene.Canvas.RestoreState(SavedState);
    finally
      Scene.Canvas.EndScene;
    end;
  end;
end;

procedure TCustomVirtualListviewFMX.RecalculateCellViewportRects(RecalcWorldRect: Boolean);
var
  i: Integer;
  TempRect: TRectF;
begin
  for i := 0 to Items.Count - 1 do
  begin
    Items.NormalizedItemRect(TempRect);
    TempRect.Offset(0, i * CellHeight);
    Items[i].BoundsRect := TempRect;
  end;
  Scroller.LineScroll := CellHeight;
  if RecalcWorldRect then
    RecalculateWorldRect;
end;

procedure TCustomVirtualListviewFMX.RecalculateWorldRect;
begin
  Scroller.SetWorldRect(TRectF.Create(0, 0, Width, (Items.Count * CellHeight) + BorderWidth));
end;

procedure TCustomVirtualListviewFMX.Resize;
begin
  inherited;
  RecalculateCellViewportRects(True);
end;

procedure TCustomVirtualListviewFMX.SetBorderColor(const Value: TAlphaColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    InvalidateRect(LocalRect);
  end;
end;

procedure TCustomVirtualListviewFMX.SetBorderWidth(const Value: real);
begin
  if Value <> BorderWidth then
  begin
    FBorderWidth := Value;
    RecalculateCellViewportRects(True);
    InvalidateRect(LocalRect);
  end;
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
    RecalculateCellViewportRects(True);
  end;
end;

{ TListviewItems }

function TVirtualListItems.Add: TVirtualListItem;
var
  TempRect: TRectF;
begin
  Result := TVirtualListItem.Create;
  Result.FListviewItems := Self;
  Result.ControlEdit := TVirtualEdit.Create(Listview); // Must to do clip?
  Result.ControlEdit.Visible := False;
  Result.ControlEdit.Parent := Listview;
  NormalizedItemRect(TempRect);
  TempRect.Offset(0, Count*Listview.CellHeight);
  Result.BoundsRect := TempRect;
  Result.Color := Listview.CellColor;
  Result.TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  Items.Add(Result);
  Listview.RecalculateWorldRect;
end;

function TVirtualListItems.AddAsFrame(VirtualListviewCellFrame: TFrame): TVirtualListItem;

  procedure ClipChildWindows(AControl: TControl);
  var
    i: Integer;
  begin
    for i := 0 to AControl.ControlsCount - 1 do
    begin
      if AControl.Controls[i].ControlsCount > 0 then
        ClipChildWindows(AControl.Controls[i]);
      AControl.ClipChildren := True;
    end;

  end;

var
  TempRect: TRectF;
begin
  ClipChildWindows(VirtualListviewCellFrame);
  Result := TVirtualListItem.Create;
  Result.FListviewItems := Self;
  Result.ControlEdit := TVirtualEdit.Create(Listview); // Must to do clip?
  Result.ControlEdit.Visible := False;
  Result.ControlEdit.Parent := Listview;
  NormalizedItemRect(TempRect);
  TempRect.Offset(0, Count*Listview.CellHeight);
  Result.BoundsRect := TempRect;
  Result.Frame := VirtualListviewCellFrame;
  Result.Frame.Visible := False;
  Result.Frame.Parent := Listview;
  Result.TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  Items.Add(Result);
  Listview.RecalculateWorldRect;
end;

procedure TVirtualListItems.Clear;
var
  i: Integer;
begin
  for i := Items.Count - 1 downto 0 do
    Items.Delete(i);
end;

constructor TVirtualListItems.Create(AOwner: TComponent);
begin
  Items := TObjectList<TVirtualListItem>.Create;
  Items.OwnsObjects := True;
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

procedure TVirtualListItems.NormalizedItemRect(var ARect: TRectF);
begin
  ARect.Left := Listview.BorderWidth/2;
  ARect.Top := Listview.BorderWidth/2;
  ARect.Right := Listview.Width - Listview.BorderWidth/2;
  ARect.Bottom := ARect.Top + Listview.CellHeight;
end;

procedure TVirtualListItems.Paint(ACanvas: TCanvas; ViewportRect: TRectF);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Item[i].Paint(ACanvas, ViewportRect);
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

{ TListviewCell }

constructor TVirtualListItem.Create;
begin
  inherited;
  Color := claWhite;
end;

destructor TVirtualListItem.Destroy;
begin
  FreeAndNil(FFrame);
  FreeAndNil(FControlEdit);
  inherited;
end;

procedure TVirtualListItem.Paint(ACanvas: TCanvas; ViewportRect: TRectF);
var
  WindowRect, TextRect: TRectF;
  LayoutSize: TPointF;
  Handled: Boolean;
begin
  if Assigned(Frame) then
  begin
    if ViewportRect.IntersectsWith(BoundsRect) then
    begin
      WindowRect := BoundsRect;
      OffsetRect(WindowRect, -ViewportRect.Left, -ViewportRect.Top);
      Frame.Position.X := WindowRect.Left;
      Frame.Position.Y := WindowRect.Top;
      Frame.Visible := True;
    end else
      Frame.Visible := False;
  end else
  begin
    if ViewportRect.IntersectsWith(BoundsRect) then
    begin
      WindowRect := BoundsRect;
      OffsetRect(WindowRect, -ViewportRect.Left, -ViewportRect.Top);
      Handled := False;
      ListviewItems.Listview.DoCustomDrawItem(Self, WindowRect, ACanvas, TextLayout, Handled);
      if not Handled then
      begin
        TextLayout.BeginUpdate;
        try
          TextLayout.Text := 'This is a whole lot of text, what will happen';
          TextLayout.HorizontalAlign := TTextAlign.Center;
          TextLayout.VerticalAlign := TTextAlign.Center;
          TextLayout.WordWrap := True;
          TextLayout.Color := claBlack;
          TextLayout.Font.Assign(ListviewItems.Listview.Font);
          TextLayout.TopLeft := WindowRect.TopLeft;
          TextLayout.MaxSize := WindowRect.Size;
        finally
          TextLayout.EndUpdate
        end;
        ACanvas.Fill.Color := Color;
        ACanvas.FillRect(WindowRect, 0.0, 0.0, [TCorner.TopLeft, TCorner.BottomRight], 1.0);
        TextLayout.RenderLayout(ACanvas);

        if Assigned(ControlEdit) then
        begin
          ControlEdit.Position.X := 50;
          ControlEdit.Position.Y := WindowRect.Top + 10;
          ControlEdit.Visible := True;
          ControlEdit.InvalidateRect(ControlEdit.LocalRect);
        end;
      end;
    end else
    begin
      ControlEdit.Visible := False;
    end;
  end;
end;

procedure TVirtualListItem.SetBoundsRect(const Value: TRectF);
begin
  FBoundsRect := Value;
  if Assigned(Frame) then
  begin
    Frame.Width := BoundsRect.Width;
    Frame.Height := BoundsRect.Height;
  end
end;

{ TVirtualEdit }

procedure TVirtualEdit.DoPaint;
begin
  inherited;
end;

procedure TVirtualEdit.Paint;
begin
  inherited;

end;

initialization
  GetScreenScale;

finalization

end.
