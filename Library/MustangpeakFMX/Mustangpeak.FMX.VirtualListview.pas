unit Mustangpeak.FMX.VirtualListview;

interface

{$DEFINE XE8_OR_NEWER}

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, System.Generics.Collections,
  System.Generics.Defaults, System.UITypes, System.UIConsts, System.Types, FMX.Graphics,
  FMX.Ani, FMX.InertialMovement, System.Math, Mustangpeak.FMX.AniScroller, FMX.TextLayout,
  FMX.Forms, FMX.Platform, FMX.Edit, FMX.ImgList, Mustangpeak.FMX.Animations;

type
  TCustomVirtualListview = class;
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

  TVirtualLayoutSwipe = record
    ID: Integer;  // User defined ID to identify which Kind is being requested in other callbacks
    Kind: TVirtualLayoutKind;
    Width: real;
    Color: TAlphaColor;
    Opacity: real;
    constructor Create(AnID: Integer; AKind: TVirtualLayoutKind; AWidth: real; AColor: TAlphaColor; AnOpacity: real);
  end;
  TVirtualItemLayoutSwipeArray = array of TVirtualLayoutSwipe;

  TOnItemDelete = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem) of object;
  TOnItemCustomDraw = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean) of object;
  TOnItemDrawBackground = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; var Handled: Boolean) of object;
  TOnItemLayoutElementClick = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; Button: TMouseButton; Shift: TShiftState; ID: Integer) of object;
  TOnGetItemCheckImage = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout) of object;
  TOnGetItemImage = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout) of object;
  TOnGetItemLayout = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray) of object;
  TOnGetItemLayoutSwipe = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; var Layout: TVirtualItemLayoutSwipeArray) of object;
  TOnGetItemSize = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; var Width, Height: real) of object;
  TOnGetItemText = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer) of object;
  TOnGetItemTextSwipe = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout) of object;
  TOnGetItemTextDetail = procedure(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout) of object;


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
    FListview: TCustomVirtualListview;
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
    constructor Create(AListview: TCustomVirtualListview);
    destructor Destroy; override;
    procedure AssignToTextLayout(Target: TTextLayout);

    property Listview: TCustomVirtualListview read FListview;
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
    FListview: TCustomVirtualListview;
    FLines: Integer;
    procedure SetLines(const Value: Integer);
  public
    constructor Create(AListview: TCustomVirtualListview);
    destructor Destroy; override;
    property Listview: TCustomVirtualListview read FListview;
  published
    property Lines: Integer read FLines write SetLines default 1;
    property TextLayout: TVirtualTextLayout read FTextLayout write FTextLayout;
  end;

  TCustomVirtualImageLayout = class(TPersistent)
  private
    FWidth: Single;
    FOpacity: real;
    FPadding: TBounds;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FListview: TCustomVirtualListview;
  protected
    property Images: TCustomImageList read FImages write FImages;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
  public
    constructor Create(AListview: TCustomVirtualListview);
    destructor Destroy; override;

    procedure AssignToImageLayoutLayout(Target: TCustomVirtualImageLayout);

    property Listview: TCustomVirtualListview read FListview;
    property Opacity: real read FOpacity write FOpacity;
    property Padding: TBounds read FPadding write FPadding;
    property Width: Single read FWidth write FWidth;
  end;

  TVirtualImageLayout = class(TCustomVirtualImageLayout)
  public
    property Images;
    property ImageIndex;
  end;

  TVirtualDefaultLayoutImage = class(TCustomVirtualImageLayout)
  published
    property Opacity;
    property Padding;
    property Width;
  end;

  TVirtualDefaultLayoutSwipe = class(TPersistent)
  private
    FWidth: Single;
    FListview: TCustomVirtualListview;
    FCount: Integer;
    FOpacity: real;
    FColor: TAlphaColor;
  public
    constructor Create(AListview: TCustomVirtualListview);

    property Listview: TCustomVirtualListview read FListview;
  published
    property Color: TAlphaColor read FColor write FColor;
    property Opacity: real read FOpacity write FOpacity;
    property Count: Integer read FCount write FCount default 1;
    property Width: Single read FWidth write FWidth;
  end;

  TVirtualListItem = class(TFMXObject)
  private
    FListviewItems: TVirtualListItems;
    FBoundsRect: TRectF;
    FVisible: Boolean;
    FIndex: Integer;
    FFocused: Boolean;
    FSelected: Boolean;
    FChecked: Boolean;
    FSwipeState: TSwipeStateSet;
    FSwipeTargetOffset: Single;
    FSwipeAni: TFloatAnimation;
    FSwipeCurrentOffset: Single;
    FSwipeTotalLayoutWidth: Single;
    FSwiped: Boolean;

    procedure SetBoundsRect(const Value: TRectF);
    procedure SetVisible(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
    procedure SetSelected(const Value: Boolean);
  protected
    property BoundsRect: TRectF read FBoundsRect write SetBoundsRect;
    property ListviewItems: TVirtualListItems read FListviewItems;
    property SwipeTargetOffset: Single read FSwipeTargetOffset write FSwipeTargetOffset;
    property SwipeTotalLayoutWidth: Single read FSwipeTotalLayoutWidth write FSwipeTotalLayoutWidth;
    property SwipeAni: TFloatAnimation read FSwipeAni write FSwipeAni;

    procedure CalculateVariableWidthElement(var LayoutArray: TVirtualItemLayoutArray);
    procedure Paint(ACanvas: TCanvas; ViewportRect: TRectF; LayoutArray: TVirtualItemLayoutArray);
    procedure SwipeAniProcess(Sender: TObject);
    procedure SwipeAniFinish(Sender: TObject);
    procedure SwipeEnd(Listview: TCustomVirtualListview; WorldX, WorldY: Single);
    procedure SwipeStart(Listview: TCustomVirtualListview; WorldX, WorldY: Single; SwipeState: TSwipeStateSet);
    procedure SwipeMove(Listview: TCustomVirtualListview; WorldX, WorldY: Single);
  public
    property Checked: Boolean read FChecked write SetChecked;
    property Focused: Boolean read FFocused write SetFocused;
    property Selected: Boolean read FSelected write SetSelected;
    property Swiped: Boolean read FSwiped;
    property SwipeState: TSwipeStateSet read FSwipeState;
    property iIndex: Integer read FIndex;
    property Visible: Boolean read FVisible write SetVisible;

    constructor Create(AnOwner: TComponent; AnItemList: TVirtualListItems); reintroduce; virtual;
    destructor Destroy; override;

    procedure Invalidate;
    function PointToLayoutID(Point: TPointF; LayoutArray: TVirtualItemLayoutArray): Integer;
    function PointToSwipeLayoutID(Point: TPointF; LayoutArray: TVirtualItemLayoutSwipeArray): Integer;
  published
    property SwipeCurrentOffset: Single read FSwipeCurrentOffset write FSwipeCurrentOffset;
  end;

  TVirtualListItems = class(TFMXObject)
  private
    FItems: TObjectList<TVirtualListItem>;
    FListview: TCustomVirtualListview;
    FCellColor: TAlphaColor;
    FUpdateCount: Integer;
    FTextLayout: TTextLayout;
    FActiveSwipe: TVirtualListItem;
    function GetCount: Integer;
    function GetItem(Index: Integer): TVirtualListItem;
    procedure SetItem(Index: Integer; const Value: TVirtualListItem);
    procedure SetCellColor(const Value: TAlphaColor);
    procedure SetActiveSwipe(const Value: TVirtualListItem);
  protected
    property CellColor: TAlphaColor read FCellColor write SetCellColor;
    property Items: TObjectList<TVirtualListItem> read FItems write FItems;
    property Listview: TCustomVirtualListview read FListview write FListview;
    property TextLayout: TTextLayout read FTextLayout write FTextLayout;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;

    procedure Paint(ACanvas: TCanvas; ViewportRect: TRectF);

  public
    property ActiveSwipe: TVirtualListItem read FActiveSwipe write SetActiveSwipe;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TVirtualListItem read GetItem write SetItem; default;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    function Add: TVirtualListItem;
    function FindItemByPt(APoint: TPointF): TVirtualListItem;
  end;

  TCustomVirtualListview = class(TControl)
  private
    FOnCustomDrawItem: TOnItemCustomDraw;
    FScroller: TAniScroller;
    FOnGetItemText: TOnGetItemText;
    FItems: TVirtualListItems;
    FCellColor: TAlphaColor;
    FFont: TFont;
    FOnGetItemTextDetail: TOnGetItemTextDetail;
    FCellHeight: Integer;
    FOnGetItemImage: TOnGetItemImage;
    FOnGetItemSize: TOnGetItemSize;
    FTextLayoutTitle: TVirtualTextLayout;
    FTextLayoutDetail: TVirtualDetails;
    FDefaultLayoutImage: TVirtualDefaultLayoutImage;
    FOnGetItemLayout: TOnGetItemLayout;
    FOnItemDrawBackground: TOnItemDrawBackground;
    FOnItemLayoutElementClick: TOnItemLayoutElementClick;
    FOnGetItemLayoutSwipe: TOnGetItemLayoutSwipe;
    FOnItemLayoutElementSwipeClick: TOnItemLayoutElementClick;
    FOnItemDelete: TOnItemDelete;
    FTextLayoutSwipe: TVirtualTextLayout;
    FOnGetItemTextSwipe: TOnGetItemTextSwipe;
    FDefaultLayoutSwipe: TVirtualDefaultLayoutSwipe;
    FTextLayoutDetailSwipe: TVirtualDetails;
    FOnGetItemTextDetailSwipe: TOnGetItemTextDetail;

    procedure SetCellHeight(const Value: Integer);
    procedure SetCellColor(const Value: TAlphaColor);
    function GetCellCount: Integer;
    procedure SetCellCount(const Value: Integer);
  protected
    procedure DoGetItemImage(Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout); virtual;
    procedure DoGetItemLayout(Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray); virtual;
    procedure DoGetItemLayoutSwipe(Item: TVirtualListItem; var ElementLayout: TVirtualItemLayoutSwipeArray); virtual;
    procedure DoGetItemSize(Item: TVirtualListItem; var Width, Height: real); virtual;
    procedure DoGetItemText(Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer); virtual;
    procedure DoGetItemTextDetail(Item: TVirtualListItem; DetailLineIndex: Integer; TextLayout: TTextLayout); virtual;
    procedure DoGetItemTextDetailSwipe(Item: TVirtualListItem; DetailLineIndex: Integer; TextLayout: TTextLayout); virtual;
    procedure DoGetItemTextSwipe(Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout); virtual;
    procedure DoItemCustomDraw(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean); virtual;
    procedure DoItemDelete(Item: TVirtualListItem); virtual;
    procedure DoItemDrawBackground(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; var Handled: Boolean); virtual;
    procedure DoItemLayoutElementClick(Item: TVirtualListItem; Button: TMouseButton; Shift: TShiftState; ID: Integer); virtual;
    procedure DoItemLayoutElementSwipeClick(Item: TVirtualListItem; Button: TMouseButton; Shift: TShiftState; ID: Integer); virtual;
    procedure DoMouseLeave; override;
    procedure EventMouseClick(Button: TMouseButton; Shift: TShiftState; WorldX, WorldY: Single);
    procedure EventSwipeStart(WorldX, WorldY: Single; SwipeState: TSwipeStateSet);
    procedure EventSwipeEnd(WorldX, WorldY: Single);
    procedure EventSwipeMove(WorldX, WorldY: Single);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

    procedure Paint; override;
    procedure RecalculateCellViewportRects(RespectUpdateCount: Boolean; RecalcWorldRect: Boolean);
    procedure RecalculateWorldRect;
    procedure Resize; override;
    procedure Invalidate;
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure LoadItemLayout(Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray); virtual;
    procedure LoadItemLayoutSwipe(Item: TVirtualListItem; var Layout: TVirtualItemLayoutSwipeArray); virtual;

    property CellColor: TAlphaColor read FCellColor write SetCellColor default claWhite;
    property CellCount: Integer read GetCellCount write SetCellCount default 0;
    property CellHeight: Integer read FCellHeight write SetCellHeight default 44;
    property Font: TFont read FFont write FFont;
    property Items: TVirtualListItems read FItems write FItems;
    property DefaultLayoutImage: TVirtualDefaultLayoutImage read FDefaultLayoutImage write FDefaultLayoutImage;
    property DefaultLayoutSwipe: TVirtualDefaultLayoutSwipe read FDefaultLayoutSwipe write FDefaultLayoutSwipe;
    property OnItemCustomDraw: TOnItemCustomDraw read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnItemDelete: TOnItemDelete read FOnItemDelete write FOnItemDelete;
    property OnItemDrawBackground: TOnItemDrawBackground read FOnItemDrawBackground write FOnItemDrawBackground;
    property OnItemLayoutElementClick: TOnItemLayoutElementClick read FOnItemLayoutElementClick write FOnItemLayoutElementClick;
    property OnItemLayoutElementSwipeClick: TOnItemLayoutElementClick read FOnItemLayoutElementSwipeClick write FOnItemLayoutElementSwipeClick;
    property OnGetItemImage: TOnGetItemImage read FOnGetItemImage write FOnGetItemImage;
    property OnGetItemLayout: TOnGetItemLayout read FOnGetItemLayout write FOnGetItemLayout;
    property OnGetItemLayoutSwipe: TOnGetItemLayoutSwipe read FOnGetItemLayoutSwipe write FOnGetItemLayoutSwipe;
    property OnGetItemSize: TOnGetItemSize read FOnGetItemSize write FOnGetItemSize;
    property OnGetItemTextSwipe: TOnGetItemTextSwipe read FOnGetItemTextSwipe write FOnGetItemTextSwipe;
    property OnGetItemText: TOnGetItemText read FOnGetItemText write FOnGetItemText;
    property OnGetItemTextDetail: TOnGetItemTextDetail read FOnGetItemTextDetail write FOnGetItemTextDetail;
    property OnGetItemTextDetailSwipe: TOnGetItemTextDetail read FOnGetItemTextDetailSwipe write FOnGetItemTextDetailSwipe;
    property Scroller: TAniScroller read FScroller write FScroller;
    property TextLayoutSwipe: TVirtualTextLayout read FTextLayoutSwipe write FTextLayoutSwipe;
    property TextLayoutTitle: TVirtualTextLayout read FTextLayoutTitle write FTextLayoutTitle;
    property TextLayoutDetail: TVirtualDetails read FTextLayoutDetail write FTextLayoutDetail;
    property TextLayoutDetailSwipe: TVirtualDetails read FTextLayoutDetailSwipe write FTextLayoutDetailSwipe;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TVirtualListviewFMX = class(TCustomVirtualListview)
  public
    property Items;
  published
    property TextLayoutSwipe;
    property TextLayoutDetail;
    property TextLayoutDetailSwipe;
    property TextLayoutTitle;
    property DefaultLayoutImage;
    property DefaultLayoutSwipe;

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

    property OnGetItemSize;
    property OnGetItemImage;
    property OnGetItemLayout;
    property OnGetItemLayoutSwipe;
    property OnGetItemText;
    property OnGetItemTextDetail;
    property OnGetItemTextDetailSwipe;
    property OnGetItemTextSwipe;
    property OnItemCustomDraw;
    property OnItemDelete;
    property OnItemDrawBackground;
    property OnItemLayoutElementClick;
    property OnItemLayoutElementSwipeClick;

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

constructor TCustomVirtualListview.Create(AOwner: TComponent);
begin
  inherited;
  ClipChildren  := True;
  FCellHeight := 44;
  FFont := TFont.Create;
  FScroller := TAniScroller.Create(Self);
  Scroller.OnMouseClick := EventMouseClick;
  Scroller.OnSwipeEnd := EventSwipeEnd;
  Scroller.OnSwipeMove := EventSwipeMove;
  Scroller.OnSwipeStart := EventSwipeStart;
  FItems := TVirtualListItems.Create(Self);
  FTextLayoutDetail := TVirtualDetails.Create(Self);
  FTextLayoutTitle := TVirtualTextLayout.Create(Self);
  FTextLayoutSwipe := TVirtualTextLayout.Create(Self);
  FDefaultLayoutImage := TVirtualDefaultLayoutImage.Create(Self);
  FDefaultLayoutSwipe := TVirtualDefaultLayoutSwipe.Create(Self);
  FTextLayoutDetailSwipe := TVirtualDetails.Create(Self);
  CanFocus := True;
end;

destructor TCustomVirtualListview.Destroy;
begin
  FTextLayoutDetail.DisposeOf;
  FTextLayoutTitle.DisposeOf;
  FTextLayoutSwipe.DisposeOf;
  FDefaultLayoutImage.DisposeOf;
  FDefaultLayoutSwipe.DisposeOf;
  FTextLayoutDetailSwipe.DisposeOf;
  inherited;
end;

procedure TCustomVirtualListview.DoItemCustomDraw(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; TextLayout: TTextLayout; var Handled: Boolean);
begin
  if Assigned(OnItemCustomDraw) then
    OnItemCustomDraw(Self, Item, WindowRect, ItemCanvas, TextLayout, Handled);
end;

procedure TCustomVirtualListview.DoItemDelete(Item: TVirtualListItem);
begin
  if Item = Items.ActiveSwipe then
    Items.ActiveSwipe := nil;
  if Assigned(OnItemDelete) then
    OnItemDelete(Self, Item);
end;

procedure TCustomVirtualListview.DoItemDrawBackground(Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; var Handled: Boolean);
begin
  if Assigned(OnItemDrawBackground) then
    OnItemDrawBackground(Self, Item, WindowRect, ItemCanvas, Handled);
end;

procedure TCustomVirtualListview.DoItemLayoutElementClick(Item: TVirtualListItem; Button: TMouseButton; Shift: TShiftState; ID: Integer);
begin
  if Assigned(OnItemLayoutElementClick) then
    OnItemLayoutElementClick(Self, Item, Button, Shift, ID);
end;

procedure TCustomVirtualListview.DoItemLayoutElementSwipeClick(Item: TVirtualListItem; Button: TMouseButton; Shift: TShiftState; ID: Integer);
begin
  if Assigned(OnItemLayoutElementSwipeClick) then
    OnItemLayoutElementSwipeClick(Self, Item, Button, Shift, ID)
end;

procedure TCustomVirtualListview.DoGetItemTextDetail(Item: TVirtualListItem; DetailLineIndex: Integer; TextLayout: TTextLayout);
begin
  if Assigned(OnGetItemTextDetail) then
    OnGetItemTextDetail(Self, Item, DetailLineIndex, TextLayout);
end;

procedure TCustomVirtualListview.DoGetItemTextDetailSwipe(Item: TVirtualListItem; DetailLineIndex: Integer; TextLayout: TTextLayout);
begin
  if Assigned(OnGetItemTextDetailSwipe) then
    OnGetItemTextDetailSwipe(Self, Item, DetailLineIndex, TextLayout);
end;

procedure TCustomVirtualListview.DoGetItemImage(Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout);
begin
  if Assigned(OnGetItemImage) then
    OnGetItemImage(Self, Item, ID, ImageLayout);
end;

procedure TCustomVirtualListview.DoGetItemLayout(Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray);
begin
  if Assigned(OnGetItemLayout) then
    OnGetItemLayout(Self, Item, Layout);
end;

procedure TCustomVirtualListview.DoGetItemSize(Item: TVirtualListItem; var Width, Height: real);
begin
  if Assigned(OnGetItemSize) then
    OnGetItemSize(Self, Item, Width, Height);
end;

procedure TCustomVirtualListview.DoGetItemLayoutSwipe(Item: TVirtualListItem; var ElementLayout: TVirtualItemLayoutSwipeArray);
begin
  if Assigned(OnGetItemLayoutSwipe) then
    OnGetItemLayoutSwipe(Self, Item, ElementLayout);
end;

procedure TCustomVirtualListview.DoGetItemTextSwipe(Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout);
begin
  if Assigned(OnGetItemTextSwipe) then
    OnGetItemTextSwipe(Self, Item, ID, TextLayout);
end;

procedure TCustomVirtualListview.DoGetItemText(Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer);
begin
  if Assigned(OnGetItemText) then
    OnGetItemText(Self, Item, ID, TextLayout, DetailLines);
end;

procedure TCustomVirtualListview.DoMouseLeave;
begin
  inherited;
  Scroller.MouseLeave;
end;

function TCustomVirtualListview.GetCellCount: Integer;
begin
  Result := Items.Count
end;

procedure TCustomVirtualListview.Invalidate;
begin
  if [csDestroying, csLoading] * ComponentState = [] then
    InvalidateRect(LocalRect);
end;

procedure TCustomVirtualListview.KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  Scroller.KeyDown(Key, KeyChar, Shift);
end;

procedure TCustomVirtualListview.Loaded;
begin
  inherited;
  RecalculateCellViewportRects(False, True);
end;

procedure TCustomVirtualListview.LoadItemLayout(Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray);
begin
  SetLength(Layout, 0);
  DoGetItemLayout(Item, Layout);
  if Length(Layout) = 0 then
  begin
    SetLength(Layout, 2);
    Layout[0] := TVirtualLayout.Create(0, TVirtualLayoutKind.Image, DefaultLayoutImage.Width, TVirtualLayoutWidth.Fixed);
    Layout[1] := TVirtualLayout.Create(0, TVirtualLayoutKind.Text, 0, TVirtualLayoutWidth.Variable);
  end
end;

procedure TCustomVirtualListview.LoadItemLayoutSwipe(Item: TVirtualListItem; var Layout: TVirtualItemLayoutSwipeArray);
var
  i: Integer;
begin
  SetLength(Layout, 0);
  DoGetItemLayoutSwipe(Item, Layout);
  if Length(Layout) = 0 then
  begin
    SetLength(Layout, DefaultLayoutSwipe.Count);
    for i := 0 to DefaultLayoutSwipe.Count - 1 do
      Layout[i] := TVirtualLayoutSwipe.Create(i, TVirtualLayoutKind.Text, DefaultLayoutSwipe.Width, DefaultLayoutSwipe.Color, DefaultLayoutSwipe.Opacity);
  end
end;

procedure TCustomVirtualListview.EventMouseClick(Button: TMouseButton; Shift: TShiftState; WorldX, WorldY: Single);
var
  Item: TVirtualListItem;
  Layout: TVirtualItemLayoutArray;
  SwipeLayout: TVirtualItemLayoutSwipeArray;
  Point: TPointF;
  ID: Integer;
begin
  Point := TPointF.Create(WorldX, WorldY);
  Item := Items.FindItemByPt(Point);
  if Assigned(Item) then
  begin
    if Item.SwipeState <> [] then
    begin
      LoadItemLayoutSwipe(Item, SwipeLayout);
      ID := Item.PointToSwipeLayoutID(Point, SwipeLayout);
      if ID > -1 then
        DoItemLayoutElementSwipeClick(Item, Button, Shift, ID);
    end else
    begin
      if Assigned(Items.ActiveSwipe) then
        Items.ActiveSwipe := nil
      else begin
        LoadItemLayout(Item, Layout);
        ID := Item.PointToLayoutID(Point, Layout);
        if ID > -1 then
          DoItemLayoutElementClick(Item, Button, Shift, ID);
      end;
    end;
  end;
end;

procedure TCustomVirtualListview.EventSwipeEnd(WorldX, WorldY: Single);
begin
  if Assigned(Items.ActiveSwipe) then
  begin
    if not Items.ActiveSwipe.Swiped then
    begin
      Items.ActiveSwipe.SwipeEnd(Self, WorldX, WorldY);
      if Items.ActiveSwipe.SwipeTargetOffset = 0 then
        Items.ActiveSwipe := nil;
    end;
  end;
end;

procedure TCustomVirtualListview.EventSwipeMove(WorldX, WorldY: Single);
begin
  if Assigned(Items.ActiveSwipe) then
  begin
    if not Items.ActiveSwipe.Swiped then
      Items.ActiveSwipe.SwipeMove(Self, WorldX, WorldY);
  end;
end;

procedure TCustomVirtualListview.EventSwipeStart(WorldX, WorldY: Single; SwipeState: TSwipeStateSet);
var
  Point: TPointF;
  TempSwipeItem: TVirtualListItem;
begin
  Point := TPointF.Create(WorldX, WorldY);
  TempSwipeItem := Items.FindItemByPt(Point);
  if Items.ActiveSwipe <> TempSwipeItem then
  begin
    Items.ActiveSwipe := TempSwipeItem;
    if Assigned(Items.ActiveSwipe) then
      Items.ActiveSwipe.SwipeStart(Self, WorldX, WorldY, SwipeState);
  end;
end;

procedure TCustomVirtualListview.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  Capture;
  Scroller.MouseDown(Button, Shift, x, y);
end;

procedure TCustomVirtualListview.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  Scroller.MouseMove(Shift, X, Y);
end;

procedure TCustomVirtualListview.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  Scroller.MouseUp(Button, Shift, X, Y);
end;

procedure TCustomVirtualListview.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
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

procedure TCustomVirtualListview.Paint;
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

procedure TCustomVirtualListview.RecalculateCellViewportRects(RespectUpdateCount: Boolean; RecalcWorldRect: Boolean);
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

procedure TCustomVirtualListview.RecalculateWorldRect;
begin
  if Items.Count > 0 then
    Scroller.SetWorldRect(TRectF.Create(0, 0, Width, Items[Items.Count-1].BoundsRect.Bottom))
  else
    Scroller.SetWorldRect(TRectF.Create(0, 0, Width, 0));
end;

procedure TCustomVirtualListview.Resize;
begin
  inherited;
  RecalculateCellViewportRects(True, True);
end;

procedure TCustomVirtualListview.SetCellColor(const Value: TAlphaColor);
begin
  if FCellColor <> Value then
  begin
    FCellColor := Value;
    Items.CellColor := Value;
    Invalidate;
  end;
end;

procedure TCustomVirtualListview.SetCellCount(const Value: Integer);
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

procedure TCustomVirtualListview.SetCellHeight(const Value: Integer);
begin
  if FCellHeight <> Value then
  begin
    FCellHeight := Value;
    RecalculateCellViewportRects(True, True);
  end;
  Invalidate;
end;

{ TListviewItems }

function TVirtualListItems.Add: TVirtualListItem;
begin
  Result := TVirtualListItem.Create(Self, Self);
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
  FListview := AOwner as TCustomVirtualListview;
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

function TVirtualListItems.FindItemByPt(APoint: TPointF): TVirtualListItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Item[i].BoundsRect.Contains(APoint) then
    begin
      Result := Item[i];
      Break;
    end;
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

procedure TVirtualListItems.SetActiveSwipe(const Value: TVirtualListItem);
begin
  if Value <> FActiveSwipe then
  begin
    if Assigned(FActiveSwipe) then
    begin
      ActiveSwipe.SwipeTargetOffset := 0;
      ActiveSwipe.SwipeEnd(Listview, 0, 0);
      Listview.Invalidate;
    end;
    FActiveSwipe := Value;
  end;
end;

procedure TVirtualListItems.SetCellColor(const Value: TAlphaColor);
begin
  Listview.Invalidate;
end;

procedure TVirtualListItems.SetItem(Index: Integer; const Value: TVirtualListItem);
begin
  Items[Index] := Value;
end;

{ TListviewCell }

procedure TVirtualListItem.CalculateVariableWidthElement(var LayoutArray: TVirtualItemLayoutArray);
var
  iVariableLayout, i: Integer;
  FixedWidthTotal: real;
begin
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
  LayoutArray[iVariableLayout].Width := BoundsRect.Width - FixedWidthTotal;
end;

constructor TVirtualListItem.Create(AnOwner: TComponent; AnItemList: TVirtualListItems);
begin
  inherited Create(AnOwner);
  FListviewItems := AnItemList;
  FSwipeAni := TFloatAnimation.Create(Self);
  SwipeAni.Parent := Self;
  SwipeAni.SetRoot(ListviewItems.Listview.Parent as IRoot);
  SwipeAni.Enabled := False;
  SwipeAni.OnProcess := SwipeAniProcess;
  SwipeAni.OnFinish := SwipeAniFinish;
end;

destructor TVirtualListItem.Destroy;
begin
  ListviewItems.Listview.DoItemDelete(Self);
  inherited;
end;

procedure TVirtualListItem.Invalidate;
var
  OffsetRect: TRectF;
begin
  OffsetRect := BoundsRect;
  OffsetRect.Offset(-ListviewItems.Listview.Scroller.ScrollOffsetX, -ListviewItems.Listview.Scroller.ScrollOffsetY);
  ListviewItems.Listview.InvalidateRect(OffsetRect);
  ListviewItems.Listview.RepaintRect(OffsetRect);
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
      ListviewItems.Listview.DefaultLayoutImage.AssignToImageLayoutLayout(ImageLayout);
      ListviewItems.Listview.DoGetItemImage(Self, ID, ImageLayout);
      if Assigned(ImageLayout.Images) and (ImageLayout.ImageIndex > -1) then
      begin
        ImageRect.Left := ImageRect.Left + ImageLayout.Padding.Left;
        ImageRect.Right := ImageRect.Right - ImageLayout.Padding.Right;
        ImageRect.Top := ImageRect.Top + ImageLayout.Padding.Top;
        ImageRect.Bottom := ImageRect.Bottom - ImageLayout.Padding.Bottom;
        if LocalCanvas.Scale <> 1 then
        begin
          ImageRect.Width := ImageRect.Width/LocalCanvas.Scale;
          ImageRect.Height := ImageRect.Height/LocalCanvas.Scale;
        end;
        ImageLayout.Images.Draw(LocalCanvas, ImageRect, ImageLayout.ImageIndex, ImageLayout.Opacity)
      end;
    finally
      ImageLayout.DisposeOf;
    end;
  end;

  procedure PaintText(LocalCanvas: TCanvas; ID: Integer; TextLayoutRect: TRectF; SwipeText: Boolean);
  var
    DetailLines: Integer;
    LocalTextLayout: TTextLayout;
  begin
    LocalTextLayout := ListviewItems.TextLayout;

    // Paint the title
    LocalTextLayout.BeginUpdate;
    if SwipeText then
    begin
      DetailLines := ListviewItems.Listview.TextLayoutDetailSwipe.Lines;
      ListviewItems.Listview.TextLayoutSwipe.AssignToTextLayout(LocalTextLayout);
      LocalTextLayout.Text := ListviewItems.Listview.TextLayoutSwipe.Text;
      ListviewItems.Listview.DoGetItemTextSwipe(Self, ID, LocalTextLayout);
    end else
    begin
      DetailLines := ListviewItems.Listview.TextLayoutDetail.Lines;
      ListviewItems.Listview.TextLayoutTitle.AssignToTextLayout(LocalTextLayout);
      LocalTextLayout.Text := ListviewItems.Listview.TextLayoutTitle.Text;
      ListviewItems.Listview.DoGetItemText(Self, ID, LocalTextLayout, DetailLines);
    end;
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
        if SwipeText then
        begin
          ListviewItems.Listview.TextLayoutDetailSwipe.TextLayout.AssignToTextLayout(LocalTextLayout);
          LocalTextLayout.Text := ListviewItems.Listview.TextLayoutDetailSwipe.TextLayout.Text;
          ListviewItems.Listview.DoGetItemTextDetailSwipe(Self, ID, LocalTextLayout);
        end else
        begin
          ListviewItems.Listview.TextLayoutDetail.TextLayout.AssignToTextLayout(LocalTextLayout);
          LocalTextLayout.Text := ListviewItems.Listview.TextLayoutDetail.TextLayout.Text;
          ListviewItems.Listview.DoGetItemTextDetail(Self, ID, LocalTextLayout);
        end;

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
  RightMarker, SwipeScaling: real;
  Handled: Boolean;
  i: Integer;
  LayoutSwipeArray: TVirtualItemLayoutSwipeArray;
begin
  if ViewportRect.IntersectsWith(BoundsRect) then
  begin
    // Get a WindowRect that is referenced to 0, 0 of the device context
    WindowRect := BoundsRect;
    WindowRect.Offset(-ViewportRect.Left, -ViewportRect.Top);

    // Offset the item if it is swiping
    if ([TSwipeState.Right, TSwipeState.Left] * SwipeState <> []) or SwipeAni.Enabled then
    begin
      if SwipeTotalLayoutWidth = 0 then
        SwipeScaling := 1
      else
        SwipeScaling := Abs(SwipeCurrentOffset/SwipeTotalLayoutWidth);
      LayoutSwipeArray := nil;
      ListviewItems.Listview.DoGetItemLayoutSwipe(Self, LayoutSwipeArray);
      if TSwipeState.Left in SwipeState then
        RightMarker := 0
      else
        RightMarker := WindowRect.Width + SwipeCurrentOffset;   // Offset is negative here

      // Now have the widths for all elements
      for i := 0 to Length(LayoutSwipeArray) - 1 do
      begin
        ElementRect := WindowRect;
        // Calculate the Elements rectangle
        ElementRect.Left := ElementRect.Left + RightMarker;
        ElementRect.Right := ElementRect.Left + (LayoutSwipeArray[i].Width * SwipeScaling);

        ACanvas.Fill.Color := LayoutSwipeArray[i].Color;
        ACanvas.FillRect(ElementRect, 0.0, 0.0, [TCorner.TopLeft, TCorner.BottomRight], LayoutSwipeArray[i].Opacity);
        case LayoutSwipeArray[i].Kind of
          TVirtualLayoutKind.Text: PaintText(ACanvas, LayoutSwipeArray[i].ID, ElementRect, True);
          TVirtualLayoutKind.Image: PaintImage(ACanvas, LayoutSwipeArray[i].ID, ElementRect);
          TVirtualLayoutKind.Empty: begin end; // Don't do anything
          TVirtualLayoutKind.Control: begin end;
        end;

        // Slide over the marker for the next element
        RightMarker := RightMarker + ElementRect.Width;
      end;
      WindowRect.Offset(SwipeCurrentOffset, 0);
    end;

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

      CalculateVariableWidthElement(LayoutArray);

      RightMarker := 0;
      // Now have the widths for all elements
      for i := 0 to Length(LayoutArray) - 1 do
      begin
        // Calculate the Elements rectangle
        ElementRect := WindowRect;
        ElementRect.Left := ElementRect.Left + RightMarker;
        ElementRect.Right := ElementRect.Left + LayoutArray[i].Width;

        case LayoutArray[i].Kind of
          TVirtualLayoutKind.Text: PaintText(ACanvas, LayoutArray[i].ID, ElementRect, False);
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

function TVirtualListItem.PointToLayoutID(Point: TPointF; LayoutArray: TVirtualItemLayoutArray): Integer;
var
  i: Integer;
  RunningRight: single;
begin
  Result := -1;
  RunningRight := 0;
  CalculateVariableWidthElement(LayoutArray);
  for i := 0 to Length(LayoutArray) - 1 do
  begin
    RunningRight := RunningRight + LayoutArray[i].Width;
    if Point.X < RunningRight then
    begin
      Result := LayoutArray[i].ID;
      Break
    end;
  end;
end;

function TVirtualListItem.PointToSwipeLayoutID(Point: TPointF; LayoutArray: TVirtualItemLayoutSwipeArray): Integer;
var
  i: Integer;
  RunningEdge: single;
begin
  Result := -1;
  if TSwipeState.Left in SwipeState then
  begin
    RunningEdge := 0;
    for i := 0 to Length(LayoutArray) - 1 do
    begin
      RunningEdge := RunningEdge + LayoutArray[i].Width;
      if Point.X < RunningEdge then
      begin
        Result := LayoutArray[i].ID;
        Break
      end;
    end;
  end else
  begin
    RunningEdge := BoundsRect.Right;
    for i := Length(LayoutArray) - 1 downto 0 do
    begin
      RunningEdge := RunningEdge - LayoutArray[i].Width;
      if Point.X > RunningEdge then
      begin
        Result := LayoutArray[i].ID;
        Break
      end;
    end;
  end;
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
   ListviewItems.Listview.Invalidate;;
  end;
end;

procedure TVirtualListItem.SetFocused(const Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    ListviewItems.Listview.Invalidate;
  end;
end;

procedure TVirtualListItem.SetSelected(const Value: Boolean);
begin
  if Value <> FSelected then
  begin
    FSelected := Value;
    ListviewItems.Listview.Invalidate;
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

procedure TVirtualListItem.SwipeAniFinish(Sender: TObject);
begin
  SwipeAni.Enabled := False;
  if Abs(SwipeTargetOffset) > 0 then
  begin
    FSwiped := True;
    Exclude(FSwipeState, TSwipeState.Swiping);
  end else
  begin
    FSwiped := False;
    Exclude(FSwipeState, TSwipeState.Left);
    Exclude(FSwipeState, TSwipeState.Right);
    Exclude(FSwipeState, TSwipeState.Swiping);
  end;
end;

procedure TVirtualListItem.SwipeAniProcess(Sender: TObject);
begin
  Invalidate
end;

procedure TVirtualListItem.SwipeEnd(Listview: TCustomVirtualListview; WorldX, WorldY: Single);
begin
  if SwipeCurrentOffset <> SwipeTargetOffset then
  begin
    SwipeAni.PropertyName := 'SwipeCurrentOffset';
    SwipeAni.Parent := Self;
    SwipeAni.StopValue := SwipeTargetOffset;
    SwipeAni.StartValue := SwipeCurrentOffset;
    SwipeAni.Duration := 0.500;
    SwipeAni.Interpolation := TInterpolationType.Quintic;
    SwipeAni.AnimationType := TAnimationType.Out;
    SwipeAni.Enabled := True;
  end;
end;

procedure TVirtualListItem.SwipeMove(Listview: TCustomVirtualListview; WorldX, WorldY: Single);
begin
  SwipeCurrentOffset := Listview.Scroller.SwipeOffset;
  Invalidate;
end;

procedure TVirtualListItem.SwipeStart(Listview: TCustomVirtualListview; WorldX, WorldY: Single; SwipeState: TSwipeStateSet);
var
  ElementLayout: TVirtualItemLayoutSwipeArray;
  i: Integer;
begin
  SwipeCurrentOffset := 0;
  FSwipeState := SwipeState;

  SwipeTotalLayoutWidth := 0;
  SetLength(ElementLayout, 0);
  Listview.DoGetItemLayoutSwipe(Self, ElementLayout);
  for i := 0 to Length(ElementLayout) - 1 do
    SwipeTotalLayoutWidth := SwipeTotalLayoutWidth + ElementLayout[i].Width;

  if TSwipeState.Left in SwipeState then
    SwipeTargetOffset := SwipeTotalLayoutWidth
  else
    SwipeTargetOffset := -SwipeTotalLayoutWidth;
  Include(FSwipeState, TSwipeState.Swiping);
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

constructor TVirtualTextLayout.Create(AListview: TCustomVirtualListview);
begin
  inherited Create;
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
    Listview.Invalidate;
  end;
end;

procedure TVirtualTextLayout.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Listview.Invalidate;
end;

procedure TVirtualTextLayout.SetHorizontalAlign(const Value: TTextAlign);
begin
  if Value <> FHorizontalAlign then
  begin
    FHorizontalAlign := Value;
    Listview.Invalidate;
  end;
end;

procedure TVirtualTextLayout.SetOpacity(const Value: Single);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    Listview.Invalidate;
  end;
end;

procedure TVirtualTextLayout.SetPadding(const Value: TBounds);
begin
  if Value <> FPadding then
  begin
    FPadding := Value;
    Listview.Invalidate;
  end;
end;

procedure TVirtualTextLayout.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    Listview.Invalidate;
  end;
end;

procedure TVirtualTextLayout.SetTrimming(const Value: TTextTrimming);
begin
  if Value <> FTrimming then
  begin
    FTrimming := Value;
    Listview.Invalidate;
  end;
end;

procedure TVirtualTextLayout.SetVerticalAlign(const Value: TTextAlign);
begin
  if Value <> FVerticalAlign then
  begin
    FVerticalAlign := Value;
    Listview.Invalidate;
  end;
end;

procedure TVirtualTextLayout.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    Listview.Invalidate;
  end;
end;

{ TVirtualDetails }

constructor TVirtualDetails.Create(AListview: TCustomVirtualListview);
begin
  inherited Create;
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
    Listview.Invalidate;
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

constructor TCustomVirtualImageLayout.Create(AListview: TCustomVirtualListview);
begin
  inherited Create;
  FListview := AListview;
  FPadding := TBounds.Create(TRectF.Create(0, 0, 0, 0));
  ImageIndex := -1;
  Opacity := 1.0;
  Width := 48.0;
end;

destructor TCustomVirtualImageLayout.Destroy;
begin
  FreeAndNil(FPadding);
  inherited;
end;

{ TVirtualLayoutPrimatives }

constructor TVirtualLayout.Create(AnID: Integer; AKind: TVirtualLayoutKind; AWidth: real; AWidthType: TVirtualLayoutWidth);
begin
  ID := AnID;
  Kind := AKind;
  Width := AWidth;
  WidthType := AWidthType;
end;

{ TVirtualSwipeElementLayout }

constructor TVirtualLayoutSwipe.Create(AnID: Integer; AKind: TVirtualLayoutKind; AWidth: real; AColor: TAlphaColor; AnOpacity: real);
begin
  ID := AnID;
  Kind := AKind;
  Width := AWidth;
  Color := AColor;
  Opacity := AnOpacity;
end;

{ TVirtualDefaultLayoutSwipe }

constructor TVirtualDefaultLayoutSwipe.Create(AListview: TCustomVirtualListview);
begin
  inherited Create;
  FListview := AListview;
  FCount := 1;
  FWidth := 100;
end;

initialization
  GetScreenScale;

finalization

end.
