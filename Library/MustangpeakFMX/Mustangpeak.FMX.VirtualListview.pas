unit Mustangpeak.FMX.VirtualListview;

interface

{$DEFINE XE8_OR_NEWER}

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, System.Generics.Collections,
  System.Generics.Defaults, System.UITypes, System.UIConsts, System.Types, FMX.Graphics,
  FMX.Ani, FMX.InertialMovement, System.Math, Mustangpeak.FMX.AniScroller, FMX.TextLayout,
  FMX.Forms, FMX.Platform, FMX.Edit, FMX.ImgList;

type
  TCustomVirtualListviewFMX = class;
  TVirtualListItems = class;
  TVirtualListItem = class;
  TCustomVirtualImageLayout = class;
  TVirtualImageLayout = class;

  TVirtualLayoutKind = (Text, Image, Empty, Control);
  TVirtualLayoutWidth = (Fixed, Variable); // Only one Primative in an Item can be Variable
  TVirtualLayout = record
    ID: Integer;  // User defined ID to identify which Kind is being requested in other callbacks
    Kind: TVirtualLayoutKind;
    Width: real;
    WidthType: TVirtualLayoutWidth;

    constructor Create(AnID: Integer; AKind: TVirtualLayoutKind; AWidth: real; AWidthType: TVirtualLayoutWidth);
  end;
  TVirtualItemLayoutArray = array of TVirtualLayout;

  TOnItemCustomDraw = procedure(Sender: TObject; Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean) of object;
  TOnItemDrawBackground = procedure(Sender: TObject; Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; var Handled: Boolean) of object;
  TOnGetItemCheckImage = procedure(Sender: TObject; Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout) of object;
  TOnGetItemImage = procedure(Sender: TObject; Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout) of object;
  TOnGetItemLayout = procedure(Sender: TObject; Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray) of object;
  TOnGetItemSize = procedure(Sender: TObject; Item: TVirtualListItem; var Width, Height: real) of object;
  TOnGetItemText = procedure(Sender: TObject; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer) of object;
  TOnGetItemDetailText = procedure(Sender: TObject; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout) of object;


  TVirtualTextLayout = class(TPersistent)
  private
    FOpacity: Single;
    FHorizontalAlign: TTextAlign;
    FColor: TAlphaColor;
    FPadding: TBounds;
    FFont: TFont;
    FTrimming: TTextTrimming;
    FVerticalAlign: TTextAlign;
    FWordWrap: Boolean;
    FListview: TCustomVirtualListviewFMX;
    FText: string;
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetHorizontalAlign(const Value: TTextAlign);
    procedure SetOpacity(const Value: Single);
    procedure SetPadding(const Value: TBounds);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetVerticalAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetText(const Value: string);
  public
    constructor Create(AListview: TCustomVirtualListviewFMX);
    destructor Destroy; override;
    procedure AssignToTextLayout(Target: TTextLayout);

    property Listview: TCustomVirtualListviewFMX read FListview;
  published
    property Text: string read FText write SetText;
    property Padding: TBounds read FPadding write SetPadding;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property HorizontalAlign: TTextAlign read FHorizontalAlign write SetHorizontalAlign default TTextAlign.Leading;
    property VerticalAlign: TTextAlign read FVerticalAlign write SetVerticalAlign default TTextAlign.Leading;
    property Color: TAlphaColor read FColor write SetColor default TAlphaColorRec.Black;
    property Font: TFont read FFont write SetFont;
    property Opacity: Single read FOpacity write SetOpacity;
    property Trimming: TTextTrimming read FTrimming write SetTrimming default TTextTrimming.None;
  end;

  TVirtualDetails = class(TPersistent)
  private
    FTextLayout: TVirtualTextLayout;
    FListview: TCustomVirtualListviewFMX;
    FLines: Integer;
    procedure SetLines(const Value: Integer);
  public
    constructor Create(AListview: TCustomVirtualListviewFMX);
    destructor Destroy; override;
    property Listview: TCustomVirtualListviewFMX read FListview;
  published
    property Lines: Integer read FLines write SetLines default 1;
    property TextLayout: TVirtualTextLayout read FTextLayout write FTextLayout;
  end;

  TCustomVirtualImageLayout = class(TPersistent)
  private
    FWidth: real;
    FOpacity: real;
    FPadding: TBounds;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FListview: TCustomVirtualListviewFMX;
  protected
    property Images: TCustomImageList read FImages write FImages;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
  public
    constructor Create(AListview: TCustomVirtualListviewFMX);
    destructor Destroy; override;

    procedure AssignToImageLayoutLayout(Target: TCustomVirtualImageLayout);

    property Listview: TCustomVirtualListviewFMX read FListview;
    property Opacity: real read FOpacity write FOpacity;
    property Padding: TBounds read FPadding write FPadding;
    property Width: real read FWidth write FWidth;
  end;

  TVirtualImageLayout = class(TCustomVirtualImageLayout)
  public
    property Images;
    property ImageIndex;
  end;

  TPublishedVirtualImageLayout = class(TCustomVirtualImageLayout)
  published
    property Opacity;
    property Padding;
    property Width;
  end;

  TVirtualListItem = class(TPersistent)
  private
    FListviewItems: TVirtualListItems;
    FBoundsRect: TRectF;
    FVisible: Boolean;
    FIndex: Integer;
    FFocused: Boolean;
    FSelected: Boolean;
    FChecked: Boolean;

    procedure SetBoundsRect(const Value: TRectF);
    procedure SetVisible(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
    procedure SetSelected(const Value: Boolean);
  protected
    property BoundsRect: TRectF read FBoundsRect write SetBoundsRect;
    property ListviewItems: TVirtualListItems read FListviewItems;

    procedure Paint(ACanvas: TCanvas; ViewportRect: TRectF; LayoutArray: TVirtualItemLayoutArray);

  public
    property Checked: Boolean read FChecked write SetChecked;
    property Focused: Boolean read FFocused write SetFocused;
    property Selected: Boolean read FSelected write SetSelected;
    property Index: Integer read FIndex;
    property Visible: Boolean read FVisible write SetVisible;

    constructor Create(AnItemList: TVirtualListItems);
    destructor Destroy; override;
  end;

  TVirtualListGroup = class(TVirtualListItem)
  private
    FExpanded: Boolean;
    procedure SetExpanded(const Value: Boolean);
  public
    property Expanded: Boolean read FExpanded write SetExpanded;
  end;

  TVirtualListItems = class(TPersistent)
  private
    FItems: TObjectList<TVirtualListItem>;
    FListview: TCustomVirtualListviewFMX;
    FCellColor: TAlphaColor;
    FUpdateCount: Integer;
    FTextLayout: TTextLayout;
    function GetCount: Integer;
    function GetItem(Index: Integer): TVirtualListItem;
    procedure SetItem(Index: Integer; const Value: TVirtualListItem);
    procedure SetCellColor(const Value: TAlphaColor);
  protected
    property CellColor: TAlphaColor read FCellColor write SetCellColor;
    property Items: TObjectList<TVirtualListItem> read FItems write FItems;
    property Listview: TCustomVirtualListviewFMX read FListview write FListview;
    property TextLayout: TTextLayout read FTextLayout write FTextLayout;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;

    procedure Paint(ACanvas: TCanvas; ViewportRect: TRectF);

  public
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TVirtualListItem read GetItem write SetItem; default;

    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    function Add: TVirtualListItem;
  end;

  TCustomVirtualListviewFMX = class(TControl)
  private
    FOnCustomDrawItem: TOnItemCustomDraw;
    FScroller: TAniScroller;
    FOnGetItemText: TOnGetItemText;
    FItems: TVirtualListItems;
    FCellColor: TAlphaColor;
    FFont: TFont;
    FOnGetItemDetailText: TOnGetItemDetailText;
    FCellHeight: Integer;
    FOnGetItemImage: TOnGetItemImage;
    FOnGetItemSize: TOnGetItemSize;
    FTextTitleLayout: TVirtualTextLayout;
    FTextDetailLayout: TVirtualDetails;
    FImageLayout: TPublishedVirtualImageLayout;
    FOnGetItemLayout: TOnGetItemLayout;
    FOnItemDrawBackground: TOnItemDrawBackground;

    procedure SetCellHeight(const Value: Integer);
    procedure SetCellColor(const Value: TAlphaColor);
    function GetCellCount: Integer;
    procedure SetCellCount(const Value: Integer);
  protected

    procedure DoGetItemImage(Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout); virtual;
    procedure DoGetItemLayout(Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray); virtual;
    procedure DoGetItemSize(Item: TVirtualListItem; var Width, Height: real); virtual;
    procedure DoGetItemText(Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer); virtual;
    procedure DoGetItemDetailText(Item: TVirtualListItem; DetailLineIndex: Integer; TextLayout: TTextLayout); virtual;
    procedure DoItemCustomDraw(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean); virtual;
    procedure DoItemDrawBackground(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; var Handled: Boolean); virtual;
    procedure DoRedraw; virtual;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Paint; override;
    procedure RecalculateCellViewportRects(RespectUpdateCount: Boolean; RecalcWorldRect: Boolean);
    procedure RecalculateWorldRect;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure LoadItemLayout(Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray); virtual;

    property CellColor: TAlphaColor read FCellColor write SetCellColor default claWhite;
    property CellCount: Integer read GetCellCount write SetCellCount default 0;
    property CellHeight: Integer read FCellHeight write SetCellHeight default 44;
    property Font: TFont read FFont write FFont;
    property ImageLayout: TPublishedVirtualImageLayout read FImageLayout write FImageLayout;
    property Items: TVirtualListItems read FItems write FItems;
    property OnItemCustomDraw: TOnItemCustomDraw read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnItemDrawBackground: TOnItemDrawBackground read FOnItemDrawBackground write FOnItemDrawBackground;
    property OnGetItemImage: TOnGetItemImage read FOnGetItemImage write FOnGetItemImage;
    property OnGetItemLayout: TOnGetItemLayout read FOnGetItemLayout write FOnGetItemLayout;
    property OnGetItemSize: TOnGetItemSize read FOnGetItemSize write FOnGetItemSize;
    property OnGetItemText: TOnGetItemText read FOnGetItemText write FOnGetItemText;
    property OnGetItemDetailText: TOnGetItemDetailText read FOnGetItemDetailText write FOnGetItemDetailText;
    property Scroller: TAniScroller read FScroller write FScroller;
    property TextTitleLayout: TVirtualTextLayout read FTextTitleLayout write FTextTitleLayout;
    property TextDetailLayout: TVirtualDetails read FTextDetailLayout write FTextDetailLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TVirtualListviewFMX = class(TCustomVirtualListviewFMX)
  public
    property Items;
  published
    property TextDetailLayout;
    property TextTitleLayout;
    property ImageLayout;

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
 //   property ClipChildren default True;
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
    property OnItemCustomDraw;
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

    property OnGetItemSize;
    property OnDblClick;
    property OnGetItemImage;
    property OnGetItemLayout;
    property OnGetItemText;
    property OnGetItemDetailText;
    property OnItemDrawBackground;

    { ListView selection events }
 //   property OnChange;
 //   property OnChangeRepainted;
    {$IFDEF XE8_OR_NEWER}
  //  property OnItemsChange;
  //  property OnScrollViewChange;
  //  property OnFilter;
  //  property PullRefreshWait;
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

function GetScreenScale: single;
function LogicalPointsBasedOnPixels(LogicalSize: real; MinimumPixels: Integer): real;

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
  FFont := TFont.Create;
  FScroller := TAniScroller.Create(Self);
  FItems := TVirtualListItems.Create(Self);
  FTextDetailLayout := TVirtualDetails.Create(Self);
  FTextTitleLayout := TVirtualTextLayout.Create(Self);
  FImageLayout := TPublishedVirtualImageLayout.Create(Self);
  CanFocus := True;
end;

destructor TCustomVirtualListviewFMX.Destroy;
begin
  FreeAndNil(FTextDetailLayout);
  FreeAndNil(FTextTitleLayout);
  FreeAndNil(FImageLayout);
  inherited;
end;

procedure TCustomVirtualListviewFMX.DoItemCustomDraw(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean);
begin
  if Assigned(OnItemCustomDraw) then
    OnItemCustomDraw(Self, Item, WindowRect, ItemCanvas, TextLayout, Handled);
end;

procedure TCustomVirtualListviewFMX.DoItemDrawBackground(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; var Handled: Boolean);
begin
  if Assigned(OnItemDrawBackground) then
    OnItemDrawBackground(Self, Item, WindowRect, ItemCanvas, Handled);
end;

procedure TCustomVirtualListviewFMX.DoGetItemDetailText(Item: TVirtualListItem; DetailLineIndex: Integer; TextLayout: TTextLayout);
begin
  if Assigned(OnGetItemDetailText) then
    OnGetItemDetailText(Self, Item, DetailLineIndex, TextLayout);
end;

procedure TCustomVirtualListviewFMX.DoGetItemImage(Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout);
begin
  if Assigned(OnGetItemImage) then
    OnGetItemImage(Self, Item, ID, ImageLayout);
end;

procedure TCustomVirtualListviewFMX.DoGetItemLayout(Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray);
begin
  if Assigned(OnGetItemLayout) then
    OnGetItemLayout(Self, Item, Layout);
end;

procedure TCustomVirtualListviewFMX.DoGetItemSize(Item: TVirtualListItem; var Width, Height: real);
begin
  if Assigned(OnGetItemSize) then
    OnGetItemSize(Self, Item, Width, Height);
end;

procedure TCustomVirtualListviewFMX.DoGetItemText(Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer);
begin
  if Assigned(OnGetItemText) then
    OnGetItemText(Self, Item, ID, TextLayout, DetailLines);
end;

procedure TCustomVirtualListviewFMX.DoMouseLeave;
begin
  inherited;
  Scroller.MouseLeave;
end;

procedure TCustomVirtualListviewFMX.DoRedraw;
begin
  InvalidateRect(LocalRect);
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

procedure TCustomVirtualListviewFMX.Loaded;
begin
  inherited;
  RecalculateCellViewportRects(False, True);
end;

procedure TCustomVirtualListviewFMX.LoadItemLayout(Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray);
begin
  SetLength(Layout, 0);
  DoGetItemLayout(Item, Layout);
  if Length(Layout) = 0 then
  begin
    SetLength(Layout, 2);
    Layout[0] := TVirtualLayout.Create(0, TVirtualLayoutKind.Image, ImageLayout.Width, TVirtualLayoutWidth.Fixed);
    Layout[1] := TVirtualLayout.Create(0, TVirtualLayoutKind.Text, 0, TVirtualLayoutWidth.Variable);
  end
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
      R := LocalRect;
      Scene.Canvas.IntersectClipRect(R);
      Scroller.GetViewportRect(ViewportRect);
      Items.Paint(Scene.Canvas, ViewportRect);
      Scene.Canvas.RestoreState(SavedState);
    finally
      Scene.Canvas.EndScene;
    end;
  end;
end;

procedure TCustomVirtualListviewFMX.RecalculateCellViewportRects(RespectUpdateCount: Boolean; RecalcWorldRect: Boolean);
var
  i: Integer;
  AWidth, AHeight: real;
  Run: Boolean;
begin
  if ([csDestroying, csLoading] * ComponentState = []) then
  begin
    Run := True;
    if RespectUpdateCount then
      Run := Items.UpdateCount = 0;

    if Run then
    begin
      for i := 0 to Items.Count - 1 do
      begin
        Items[i].FIndex := i;
        AWidth := Width;
        if Items[i].Visible then
        begin
          AHeight := CellHeight;
          DoGetItemSize(Items[i], AWidth, AHeight);
        end else
          AHeight := 0;
        Items[i].BoundsRect.Create(0, 0, AWidth, AHeight);
        if i > 0 then
          Items[i].BoundsRect.Offset(0, Items[i-1].BoundsRect.Bottom);
      end;
      Scroller.LineScroll := CellHeight;
      if RecalcWorldRect then
        RecalculateWorldRect;
    end;
  end;
end;

procedure TCustomVirtualListviewFMX.RecalculateWorldRect;
begin
  if Items.Count > 0 then
    Scroller.SetWorldRect(TRectF.Create(0, 0, Width, Items[Items.Count-1].BoundsRect.Bottom))
  else
    Scroller.SetWorldRect(TRectF.Create(0, 0, Width, 0));
end;

procedure TCustomVirtualListviewFMX.Resize;
begin
  inherited;
  RecalculateCellViewportRects(True, True);
end;

procedure TCustomVirtualListviewFMX.SetCellColor(const Value: TAlphaColor);
begin
  if FCellColor <> Value then
  begin
    FCellColor := Value;
    Items.CellColor := Value;
    DoRedraw;
  end;
end;

procedure TCustomVirtualListviewFMX.SetCellCount(const Value: Integer);
var
  i: Integer;
begin
  Items.Clear;
  Items.BeginUpdate;
  try
    for i := Items.Count to Value - 1 do
      Items.Add;
  finally
    Items.EndUpdate;
  end;
end;

procedure TCustomVirtualListviewFMX.SetCellHeight(const Value: Integer);
begin
  if FCellHeight <> Value then
  begin
    FCellHeight := Value;
    RecalculateCellViewportRects(True, True);
  end;
  DoRedraw;
end;

{ TListviewItems }

function TVirtualListItems.Add: TVirtualListItem;
begin
  Result := TVirtualListItem.Create(Self);
  Result.FVisible := True;
  Result.FFocused := False;
  Result.FChecked := False;
  Result.FSelected := False;
  Items.Add(Result);
  Listview.RecalculateCellViewportRects(True, True);
 end;

procedure TVirtualListItems.BeginUpdate;
begin
  Inc(FUpdateCount);
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
  FListview := AOwner as TCustomVirtualListviewFMX;
  Items.OwnsObjects := True;
  TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
end;

destructor TVirtualListItems.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FTextLayout);
  inherited;
end;

procedure TVirtualListItems.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    Listview.RecalculateCellViewportRects(True, True);
  end;
end;

function TVirtualListItems.GetCount: Integer;
begin
  Result := Items.Count
end;

function TVirtualListItems.GetItem(Index: Integer): TVirtualListItem;
begin
  Result := Items[Index]
end;

procedure TVirtualListItems.Paint(ACanvas: TCanvas; ViewportRect: TRectF);
var
  i: Integer;
  Layout: TVirtualItemLayoutArray;
begin
  for i := 0 to Count - 1 do
  begin
    Listview.LoadItemLayout(Item[i], Layout);
    Item[i].Paint(ACanvas, ViewportRect, Layout);
  end;
end;

procedure TVirtualListItems.SetCellColor(const Value: TAlphaColor);
begin
  Listview.DoRedraw
end;

procedure TVirtualListItems.SetItem(Index: Integer; const Value: TVirtualListItem);
begin
  Items[Index] := Value;
end;

{ TListviewCell }

constructor TVirtualListItem.Create(AnItemList: TVirtualListItems);
begin
  inherited Create;
  FListviewItems := AnItemList;
end;

destructor TVirtualListItem.Destroy;
begin
  inherited;
end;

procedure TVirtualListItem.Paint(ACanvas: TCanvas; ViewportRect: TRectF; LayoutArray: TVirtualItemLayoutArray);

  procedure FixupHorizontalAttributes;
  {$IFDEF MACOS}
  var
    LocalPoint: TPointF;
    LocalSize: TSizeF;
  {$ENDIF}
  begin
    {$IFDEF MACOS}
    // FMX is unimplemented for horizontal positioning in XE8 at least
    if ListviewItems.TextLayout.HorizontalAlign <> TTextAlign.Trailing then
    begin
      LocalPoint := ListviewItems.TextLayout.TopLeft;
      LocalPoint.Offset(ListviewItems.TextLayout.Padding.Left, 0);
      ListviewItems.TextLayout.TopLeft := LocalPoint;
    end else
    begin
      LocalSize := ListviewItems.TextLayout.MaxSize;
      LocalSize.Width := LocalSize.Width - ListviewItems.TextLayout.Padding.Right;
      ListviewItems.TextLayout.MaxSize := LocalSize;
    end;
    {$ENDIF}
  end;

  procedure PaintImage(LocalCanvas: TCanvas; ID: Integer; ImageRect: TRectF);
  var
    ImageLayout: TVirtualImageLayout;
  begin
    // Paint the image
    ImageLayout := TVirtualImageLayout.Create(ListviewItems.Listview);
    try
      // Assign the default sizes
      ListviewItems.Listview.ImageLayout.AssignToImageLayoutLayout(ImageLayout);
      ListviewItems.Listview.DoGetItemImage(Self, ID, ImageLayout);
      if Assigned(ImageLayout.Images) and (ImageLayout.ImageIndex > -1) then
      begin
        ImageRect.Left := ImageRect.Left + ImageLayout.Padding.Left;
        ImageRect.Right := ImageRect.Right - ImageLayout.Padding.Right;
        ImageRect.Top := ImageRect.Top + ImageLayout.Padding.Top;
        ImageRect.Bottom := ImageRect.Bottom - ImageLayout.Padding.Bottom;
        ImageLayout.Images.Draw(LocalCanvas, ImageRect, ImageLayout.ImageIndex, ImageLayout.Opacity)
      end;
    finally
      ImageLayout.DisposeOf;
    end;
  end;

  procedure PaintText(LocalCanvas: TCanvas; ID: Integer; TextLayoutRect: TRectF);
  var
    DetailLines: Integer;
    LocalTextLayout: TTextLayout;
  begin
    LocalTextLayout := ListviewItems.TextLayout;

    // Paint the title
    DetailLines := ListviewItems.Listview.TextDetailLayout.Lines;
    LocalTextLayout.BeginUpdate;
    ListviewItems.Listview.TextTitleLayout.AssignToTextLayout(LocalTextLayout);
    LocalTextLayout.Text := ListviewItems.Listview.TextTitleLayout.Text;
    ListviewItems.Listview.DoGetItemText(Self, ID, LocalTextLayout, DetailLines);
    // If wanting details doing anything but top aligning makes no sense
    if DetailLines > 0 then
      LocalTextLayout.VerticalAlign := TTextAlign.Leading;
    LocalTextLayout.TopLeft := TextLayoutRect.TopLeft;
    LocalTextLayout.MaxSize := TextLayoutRect.Size;
    FixupHorizontalAttributes;
    LocalTextLayout.EndUpdate;
    if LocalTextLayout.Text <> '' then
      LocalTextLayout.RenderLayout(LocalCanvas);
    TextLayoutRect.Top := TextLayoutRect.Top + LocalTextLayout.Height;

    // Paint the details
    for ID := 0 to DetailLines - 1 do
    begin
      if TextLayoutRect.Height > 0 then
      begin
        LocalTextLayout.BeginUpdate;
        ListviewItems.Listview.TextDetailLayout.TextLayout.AssignToTextLayout(LocalTextLayout);
        LocalTextLayout.Text := ListviewItems.Listview.TextDetailLayout.TextLayout.Text;
        ListviewItems.Listview.DoGetItemDetailText(Self, ID, LocalTextLayout);
        LocalTextLayout.VerticalAlign := TTextAlign.Leading;
        LocalTextLayout.TopLeft := TextLayoutRect.TopLeft;
        LocalTextLayout.MaxSize := TextLayoutRect.Size;
        FixupHorizontalAttributes;
        LocalTextLayout.EndUpdate;
        if LocalTextLayout.Text <> '' then
          LocalTextLayout.RenderLayout(LocalCanvas);
        TextLayoutRect.Top := TextLayoutRect.Top + LocalTextLayout.Height
      end;
    end;
  end;

var
  WindowRect, ElementRect: TRectF;
  FixedWidthTotal, RightMarker: real;
  Handled: Boolean;
  i, iVariableLayout: Integer;
begin
  if ViewportRect.IntersectsWith(BoundsRect) then
  begin
    // Get a WindowRect that is referenced to 0, 0 of the device context
    WindowRect := BoundsRect;
    OffsetRect(WindowRect, -ViewportRect.Left, -ViewportRect.Top);

    // Give the user the ability to handle the entire drawing
    Handled := False;
    ListviewItems.Listview.DoItemCustomDraw(Self, WindowRect, ACanvas, ListviewItems.TextLayout, Handled);
    if not Handled then
    begin
      Handled := False;
      ListviewItems.Listview.DoItemDrawBackground(Self, WindowRect, ACanvas, Handled);
      if not Handled then
      begin
        // Fill the background if the application did not
        ACanvas.Fill.Color := ListviewItems.Listview.CellColor;
        ACanvas.FillRect(WindowRect, 0.0, 0.0, [TCorner.TopLeft, TCorner.BottomRight], 1.0);
      end;

      // Find the variable width layout element if it exists
      // and the total width of all fixed width layout elements
      iVariableLayout := -1;
      FixedWidthTotal := 0;
      for i := 0 to Length(LayoutArray) - 1 do
      begin
        if LayoutArray[i].WidthType = Fixed then
          FixedWidthTotal := FixedWidthTotal + LayoutArray[i].Width
        else
          iVariableLayout := i;
      end;

      // Now know everything to calculate the variable width element
      LayoutArray[iVariableLayout].Width := WindowRect.Width - FixedWidthTotal;

      RightMarker := 0;
      // Now have the widths for all elements
      for i := 0 to Length(LayoutArray) - 1 do
      begin
        // Calculate the Elements rectangle
        ElementRect := WindowRect;
        ElementRect.Left := RightMarker;
        ElementRect.Right := ElementRect.Left + LayoutArray[i].Width;

        case LayoutArray[i].Kind of
          TVirtualLayoutKind.Text: PaintText(ACanvas, LayoutArray[i].ID, ElementRect);
          TVirtualLayoutKind.Image: PaintImage(ACanvas, LayoutArray[i].ID, ElementRect);
          TVirtualLayoutKind.Empty: begin end; // Don't do anything
          TVirtualLayoutKind.Control: begin end;
        end;

        // Slide over the marker for the next element
        RightMarker := RightMarker + ElementRect.Width;
      end;
    end;
  end
end;

procedure TVirtualListItem.SetBoundsRect(const Value: TRectF);
begin
  FBoundsRect := Value;
end;

procedure TVirtualListItem.SetChecked(const Value: Boolean);
begin
  if Value <> FChecked then
  begin
   FChecked := Value;
   ListviewItems.Listview.DoRedraw;
  end;
end;

procedure TVirtualListItem.SetFocused(const Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    ListviewItems.Listview.DoRedraw;
  end;
end;

procedure TVirtualListItem.SetSelected(const Value: Boolean);
begin
  if Value <> FSelected then
  begin
    FSelected := Value;
    ListviewItems.Listview.DoRedraw;
  end;
end;

procedure TVirtualListItem.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    ListviewItems.Listview.RecalculateCellViewportRects(True, True);
  end;
end;

{ TVirtualTextLayout }

procedure TVirtualTextLayout.AssignToTextLayout(Target: TTextLayout);
begin
  Target.Text := '';
  Target.Font.Assign(Font);
  Target.Color := Color;
  Target.Padding.Assign(Padding);
  Target.HorizontalAlign := HorizontalAlign;
  Target.VerticalAlign := VerticalAlign;
  Target.WordWrap := WordWrap;
  Target.Trimming := Trimming;
  Target.Opacity := Opacity;
end;

constructor TVirtualTextLayout.Create(AListview: TCustomVirtualListviewFMX);
begin
  FListview := AListview;
  FFont := TFont.Create;
  FHorizontalAlign := TTextAlign.Leading;
  FVerticalAlign := TTextAlign.Leading;
  Color := claBlack;
  FPadding := TBounds.Create(TRectF.Create(0, 0, 0, 0));
  FOpacity := 1;
end;

destructor TVirtualTextLayout.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FPadding);
  inherited;
end;

procedure TVirtualTextLayout.SetColor(const Value: TAlphaColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Listview.DoRedraw;
  end;
end;

procedure TVirtualTextLayout.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Listview.DoRedraw;
end;

procedure TVirtualTextLayout.SetHorizontalAlign(const Value: TTextAlign);
begin
  if Value <> FHorizontalAlign then
  begin
    FHorizontalAlign := Value;
    Listview.DoRedraw;
  end;
end;

procedure TVirtualTextLayout.SetOpacity(const Value: Single);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    Listview.DoRedraw;
  end;
end;

procedure TVirtualTextLayout.SetPadding(const Value: TBounds);
begin
  if Value <> FPadding then
  begin
    FPadding := Value;
    Listview.DoRedraw;
  end;
end;

procedure TVirtualTextLayout.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    Listview.DoRedraw;
  end;
end;

procedure TVirtualTextLayout.SetTrimming(const Value: TTextTrimming);
begin
  if Value <> FTrimming then
  begin
    FTrimming := Value;
    Listview.DoRedraw;
  end;
end;

procedure TVirtualTextLayout.SetVerticalAlign(const Value: TTextAlign);
begin
  if Value <> FVerticalAlign then
  begin
    FVerticalAlign := Value;
    Listview.DoRedraw;
  end;
end;

procedure TVirtualTextLayout.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    Listview.DoRedraw;
  end;
end;

{ TVirtualDetails }

constructor TVirtualDetails.Create(AListview: TCustomVirtualListviewFMX);
begin
  FListview := AListview;
  FLines := 1;
  FTextLayout := TVirtualTextLayout.Create(AListview);
end;

destructor TVirtualDetails.Destroy;
begin
  FreeAndNil(FTextLayout);
  inherited;
end;

procedure TVirtualDetails.SetLines(const Value: Integer);
begin
  if Value <> FLines then
  begin
    FLines := Value;
    Listview.DoRedraw;
  end;
end;

{ TVirtualListGroup }

procedure TVirtualListGroup.SetExpanded(const Value: Boolean);
begin
  if Value <> FExpanded then
  begin
    FExpanded := Value;
    ListviewItems.Listview.RecalculateCellViewportRects(True, True);
  end;
end;

{ TVirtualImageProperties }

procedure TCustomVirtualImageLayout.AssignToImageLayoutLayout(Target: TCustomVirtualImageLayout);
begin
  Target.Width := Width;
  Target.Opacity := Opacity;
  Target.Padding.Assign(Padding);
  Target.Images := Images;
  Target.ImageIndex := ImageIndex;
end;

constructor TCustomVirtualImageLayout.Create(AListview: TCustomVirtualListviewFMX);
begin
  FListview := AListview;
  FPadding := TBounds.Create(TRectF.Create(0, 0, 0, 0));
  ImageIndex := -1;
end;

destructor TCustomVirtualImageLayout.Destroy;
begin
  FreeAndNil(FPadding);
  inherited;
end;

{ TVirtualLayoutPrimatives }

constructor TVirtualLayout.Create(AnID: Integer;
  AKind: TVirtualLayoutKind; AWidth: real;
  AWidthType: TVirtualLayoutWidth);
begin
  ID := AnID;
  Kind := AKind;
  Width := AWidth;
  WidthType := AWidthType;
end;

initialization
  GetScreenScale;

finalization

end.
