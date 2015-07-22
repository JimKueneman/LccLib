unit lcc_nodeselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  {$IFDEF FPC}
  LCLType,
  LMessages,
  {$ENDIF}
  ComCtrls, ExtCtrls, Menus, StdCtrls, Types, contnrs, lcc_defines;

type
  TLccNodeSelectorBase = class;
  TLccGuiNode = class;

  TOnLccSelectorFocusedChanged = procedure(Sender: TObject; FocusedNode, OldFocusedNode: TLccGuiNode) of object;

  { TLccGuiNode }

  TLccGuiNode = class(TPersistent)
  private
    FAliasID: Word;
    FCaptions: TStringList;
    FColor: TColor;
    FFocused: Boolean;
    FHeight: Integer;
    FImageIndex: Integer;
    FIndex: Integer;
    FLeft: Integer;
    FNodeID: TNodeID;
    FOwnerSelector: TLccNodeSelectorBase;
    FSelected: Boolean;
    FTop: Integer;
    FVisible: Boolean;
    FVisibleIndex: Integer;
    FWidth: Integer;
    function GetNodeIDStr: string;
    procedure SetCaptions(AValue: TStringList);
    procedure SetColor(AValue: TColor);
    procedure SetFocused(AValue: Boolean);
    procedure SetHeight(AValue: Integer);
    procedure SetImageIndex(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetSelected(AValue: Boolean);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
  protected
    procedure Paint(Canvas: TCanvas);
  public
    property AliasID: Word read FAliasID;
    property Captions: TStringList read FCaptions write SetCaptions;
    property Color: TColor read FColor write SetColor;
    property Focused: Boolean read FFocused write SetFocused;
    property Height: Integer read FHeight write SetHeight;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Index: Integer read FIndex;
    property Left: Integer read FLeft write SetLeft;
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr: string read GetNodeIDStr;
    property OwnerSelector: TLccNodeSelectorBase read FOwnerSelector;
    property Selected: Boolean read FSelected write SetSelected;
    property Top: Integer read FTop write SetTop;
    property Visible: Boolean read FVisible write SetVisible;
    property VisibleIndex: Integer read FVisibleIndex;
    property Width: Integer read FWidth write SetWidth;

    constructor Create;
    destructor Destroy; override;
    function BoundsRect: TRect;
    procedure Invalidate(Update: Boolean);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
    function PtInNode(APt: TPoint): Boolean;
  end;

  { TLccGuiNodeList }

  TLccGuiNodeList = class(TComponent)
  private
    FNodeList: TObjectList;
    FOwnerSelector: TLccNodeSelectorBase;
    function GetCount: Integer;
    function GetNodes(Index: Integer): TLccGuiNode;
    procedure SetNodes(Index: Integer; AValue: TLccGuiNode);
  protected
    property NodeList: TObjectList read FNodeList write FNodeList;
  public
    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: TLccGuiNode read GetNodes write SetNodes; default;
    property OwnerSelector: TLccNodeSelectorBase read FOwnerSelector;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(ANodeID: TNodeID; AnAliasID: Word): TLccGuiNode;
    procedure Delete(Index: Integer);
    function Find(ANodeID: TNodeID): TLccGuiNode;
  end;

  { TLccGuiVisibleNodeList }

  TLccGuiVisibleNodeList = class(TComponent)
  private
    FNodeList: TObjectList;
    function GetCount: Integer;
    function GetNodes(Index: Integer): TLccGuiNode;
    procedure SetNodes(Index: Integer; AValue: TLccGuiNode);
  protected
    property NodeList: TObjectList read FNodeList write FNodeList;
  public
    property Count: Integer read GetCount;
    property Nodes[Index: Integer]: TLccGuiNode read GetNodes write SetNodes; default;

    constructor Create;
    destructor Destroy; override;
    procedure Add(Node: TLccGuiNode);
    procedure Clear;
    procedure Delete(Index: Integer);
  end;

  { TLccNodeSelectorBase }

  TLccNodeSelectorBase = class(TScrollBox)
  private
    FAlignment: TAlignment;
    FFocusedNode: TLccGuiNode;
    FCalculatedHorzScrollRange: Integer;
    FImages: TImageList;
    FTextLayout: TTextLayout;
    FCaptionIndent: Integer;
    FCaptionLineCount: Integer;
    FDefaultNodeHeight: Integer;
    FDetailsFont: TFont;
    FDetailsIndent: Integer;
    FLccNodes: TLccGuiNodeList;
    FLccVisibleNodes: TLccGuiVisibleNodeList;
    FUpdateLock: Integer;
    OnFocusChanged: TOnLccSelectorFocusedChanged;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCalculatedHorzScrollRange(AValue: Integer);
    procedure SetFocusedNode(AValue: TLccGuiNode);
    procedure SetTextLayout(AValue: TTextLayout);
    procedure SetCaptionIndent(AValue: Integer);
    procedure SetCaptionLineCount(AValue: Integer);
    procedure SetDefaultNodeHeight(AValue: Integer);
    procedure SetDetailsFont(AValue: TFont);
    procedure SetDetailsIndent(AValue: Integer);
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property CalculatedHorzScrollRange: Integer read FCalculatedHorzScrollRange write SetCalculatedHorzScrollRange;
    property CaptionIndent: Integer read FCaptionIndent write SetCaptionIndent;
    property CaptionLineCount: Integer read FCaptionLineCount write SetCaptionLineCount;
    property DefaultNodeHeight: Integer read FDefaultNodeHeight write SetDefaultNodeHeight;
    property DetailsIndent: Integer read FDetailsIndent write SetDetailsIndent;
    property DetailsFont: TFont read FDetailsFont write SetDetailsFont;
    property FocusedNode: TLccGuiNode read FFocusedNode write SetFocusedNode;
    property Images: TImageList read FImages write FImages;
    property LccNodes: TLccGuiNodeList read FLccNodes write FLccNodes;
    property LccVisibleNodes: TLccGuiVisibleNodeList read FLccVisibleNodes write FLccVisibleNodes;
    property OnFocusedChanged: TOnLccSelectorFocusedChanged read OnFocusChanged write OnFocusChanged;
    property TextLayout: TTextLayout read FTextLayout write SetTextLayout;
    property UpdateLock: Integer read FUpdateLock write FUpdateLock;

    function AdjustedClientWidth: Integer;
    procedure CalculateAutoRanges; override;
    procedure DoOnFocusChanged(LccNode, OldLccNode: TLccGuiNode); virtual;
    procedure DoOnResize; override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure Paint; override;
    procedure RebuildNodes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    function ClientRectToCurrentViewPt(ARect: TRect): TRect;
    function ClientPtToCurrentViewPt(APt: TPoint): TPoint;
    function ClientPtToVisibleNode(ClientPt: TPoint; CurrentViewOnly: Boolean): TLccGuiNode;
    procedure CurrentViewRect(var ARect: TRect);
    procedure EndUpdate;
    function First: TLccGuiNode;
    function FirstInView: TLccGuiNode;
    function FirstVisible: TLccGuiNode;
    function IsInCurrentView(Node: TLccGuiNode): Boolean;
    function Last: TLccGuiNode;
    function LastVisible: TLccGuiNode;
    function Next(Node: TLccGuiNode): TLccGuiNode;
    function NextVisible(Node: TLccGuiNode): TLccGuiNode;
    function Previous(Node: TLccGuiNode): TLccGuiNode;
    function PreviousVisible(Node: TLccGuiNode): TLccGuiNode;
    procedure ScrollIntoView(Node: TLccGuiNode);
  end;

  TLccNodeSelector = class(TLccNodeSelectorBase)
  public
    property LccNodes;
    property LccVisibleNodes;
  published
    property Alignment;
    property CaptionIndent;
    property CaptionLineCount;
    property DefaultNodeHeight;
    property DetailsFont;
    property DetailsIndent;
    property Images;
    property OnFocusedChanged;
    property TextLayout;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF FPC}
 // {$I TLccNodeSelector.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccNodeSelector]);
end;

procedure EmptyRect(var ARect: TRect);
begin
  ARect := Rect(0, 0, 0, 0);
end;

{ TLccGuiVisibleNodeList }

procedure TLccGuiVisibleNodeList.Add(Node: TLccGuiNode);
begin
  NodeList.Add(Node);
end;

procedure TLccGuiVisibleNodeList.Clear;
begin
  if Assigned(NodeList) then
    NodeList.Clear;
end;

constructor TLccGuiVisibleNodeList.Create;
begin
  inherited;
  FNodeList := TObjectList.Create;
  NodeList.OwnsObjects := False;
end;

procedure TLccGuiVisibleNodeList.Delete(Index: Integer);
begin
  NodeList.Delete(Index);
end;

destructor TLccGuiVisibleNodeList.Destroy;
begin
  FreeAndNil(FNodeList);
  inherited Destroy;
end;

function TLccGuiVisibleNodeList.GetCount: Integer;
begin
  Result := NodeList.Count;
end;

function TLccGuiVisibleNodeList.GetNodes(Index: Integer): TLccGuiNode;
begin
  Result := NodeList[Index] as TLccGuiNode
end;

procedure TLccGuiVisibleNodeList.SetNodes(Index: Integer; AValue: TLccGuiNode);
begin
  NodeList[Index] := AValue
end;

{ TLccGuiNode }

function TLccGuiNode.BoundsRect: TRect;
begin
  Result.Top := Top;
  Result.Bottom := Top + FHeight;
  Result.Left := Left;
  Result.Right := Left + FWidth;
end;

constructor TLccGuiNode.Create;
begin
  inherited Create;
  FCaptions := TStringList.Create;
  FImageIndex := -1;
  FVisible := True;
  FColor := clWindow;
  FTop := 0;
  FLeft := 0;
  FWidth := 20;
  FHeight := 20;
end;

destructor TLccGuiNode.Destroy;
begin
  FreeAndNil(FCaptions);
  inherited Destroy;
end;

procedure TLccGuiNode.Invalidate(Update: Boolean);
begin
  if Assigned(OwnerSelector) then
  begin
    OwnerSelector.Invalidate;
    if Update then
      OwnerSelector.Update;
  end;
end;

procedure TLccGuiNode.Paint(Canvas: TCanvas);
var
  TextBox: TRect;
  i, Offset: Integer;
  TextExtent: TSize;
  TextRects: array of TRect;
  ImageWidth: Integer;
begin
  ImageWidth := 0;
  if Assigned(OwnerSelector.Images) and (ImageIndex > -1) then
    ImageWidth := OwnerSelector.Images.Width + 4;

  SetLength(TextRects, Captions.Count);
  for i := 0 to Length(TextRects) - 1 do
    EmptyRect(TextRects[i]);

  if (Captions.Count > 0) and (OwnerSelector.CaptionLineCount > 0) then
  begin
    Canvas.Font.Assign(OwnerSelector.Font);
    TextExtent := Canvas.TextExtent(Captions[0]);
    TextRects[0].Right := TextExtent.cx + ImageWidth;
    TextRects[0].Bottom := TextExtent.cy;
    OffsetRect(TextRects[0], OwnerSelector.CaptionIndent + ImageWidth, 0);
    if TextRects[0].Right > Width - OwnerSelector.CaptionIndent then
      TextRects[0].Right := Width - OwnerSelector.CaptionIndent;

    Canvas.Font.Assign(OwnerSelector.DetailsFont);
    i := 1;
    while (i < OwnerSelector.CaptionLineCount) and (i < Captions.Count) do
    begin
      TextExtent := Canvas.TextExtent(Captions[i]);
      TextRects[i].Right := TextExtent.cx + ImageWidth;
      TextRects[i].Bottom := TextExtent.cy;
      OffsetRect(TextRects[i], OwnerSelector.DetailsIndent + ImageWidth, 0);
      if TextRects[i].Right > Width - OwnerSelector.DetailsIndent then
        TextRects[i].Right := Width - OwnerSelector.DetailsIndent;
      OffsetRect(TextRects[i], 0, TextRects[i-1].Bottom);
      Inc(i)
    end;
  end;

  EmptyRect(TextBox);
  for i := 0 to Length(TextRects) - 1 do
    UnionRect(TextBox, TextBox, TextRects[i]);

  case OwnerSelector.TextLayout of
    tlTop :
      begin
        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], 0, 0);
      end;
    tlCenter :
      begin
        if TextBox.Bottom < Height then
          Offset := (Height - TextBox.Bottom) div 2
        else
          Offset := 0;

        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], 0, Offset);
      end;
    tlBottom :
      begin
        if TextBox.Bottom < Height then
        begin
          Offset := Height - TextBox.Bottom
        end else
          Offset := 0;

        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], 0, Offset);
      end;
  end;

  case OwnerSelector.Alignment of
    taLeftJustify :
      begin
        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], 0, 0);
      end;
    taCenter :
      begin
        if TextBox.Right < Width then
          Offset := (Width - TextBox.Right) div 2
        else
          Offset := 0;

        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], Offset, 0);
      end;
    taRightJustify :
      begin
        if TextBox.Right < Width then
        begin
          Offset := Width - TextBox.Right
        end else
          Offset := 0;

        for i := 0 to Length(TextRects) - 1 do
          OffsetRect(TextRects[i], Offset, 0);
      end;
  end;

  Canvas.ClipRect := BoundsRect;
  Canvas.Clipping := True;


  Canvas.Brush.Color := Color;
  Canvas.FillRect(BoundsRect);
  if Focused then
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clDkGray;
    Canvas.Brush.Color := clHighlight;
    Canvas.RoundRect(BoundsRect, 8, 8);
  end;
  if Assigned(OwnerSelector.Images) and (ImageIndex > -1) then
  case OwnerSelector.TextLayout of
    tlTop    : OwnerSelector.Images.Draw(Canvas, Left + 4, Top + 4, ImageIndex);
    tlCenter : OwnerSelector.Images.Draw(Canvas, Left + 4, Top + ((Height - OwnerSelector.Images.Height) div 2), ImageIndex);
    tlBottom : OwnerSelector.Images.Draw(Canvas, Left + 4, Top + (Height - OwnerSelector.Images.Height - 4), ImageIndex);
  end;


  if (Captions.Count > 0) and (OwnerSelector.CaptionLineCount > 0) then
  begin
    Canvas.Font.Assign(OwnerSelector.Font);
    OffsetRect(TextRects[0], Left, Top);
    Canvas.TextRect(TextRects[0], TextRects[0].Left, TextRects[0].Top, Captions[0]);
  //  if TextRects[0].Right > OwnerSelector.CalculatedHorzScrollRange then
  //    OwnerSelector.CalculatedHorzScrollRange := TextRects[0].Right;
    Canvas.Font.Assign(OwnerSelector.DetailsFont);
    i := 1;
    while (i < OwnerSelector.CaptionLineCount) and (i < Captions.Count) do
    begin
      OffsetRect(TextRects[i], Left, Top);
      Canvas.TextRect(TextRects[i], TextRects[i].Left, TextRects[i].Top, Captions[i]);
    //  if TextRects[i].Right > OwnerSelector.CalculatedHorzScrollRange then
    //    OwnerSelector.CalculatedHorzScrollRange := TextRects[i].Right;
      Inc(i);
    end;
  end;
end;

function TLccGuiNode.PtInNode(APt: TPoint): Boolean;
begin
  Result := PtInRect(BoundsRect, APt);
end;

procedure TLccGuiNode.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FTop := ATop;
  FLeft := ALeft;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TLccGuiNode.SetCaptions(AValue: TStringList);
begin
  FCaptions.Assign(AValue);
  Invalidate(True);
end;

function TLccGuiNode.GetNodeIDStr: string;
begin
  Result := IntToHex(NodeID[1], 6);
  Result := Result + IntToHex(NodeID[0], 6);
  Result := '0x' + Result
end;

procedure TLccGuiNode.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  Invalidate(True);
end;

procedure TLccGuiNode.SetFocused(AValue: Boolean);
begin
  if FFocused = AValue then Exit;
  FFocused := AValue;
  if Assigned(OwnerSelector) then
  begin
    if AValue then
    begin
      if Assigned(OwnerSelector.FocusedNode) and (OwnerSelector.FocusedNode <> Self) then
        OwnerSelector.FocusedNode.Focused := False;
      OwnerSelector.FocusedNode := Self;
      if not OwnerSelector.IsInCurrentView(Self) then
        OwnerSelector.ScrollIntoView(Self);
    end;
  end;
  Invalidate(True);
end;

procedure TLccGuiNode.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
end;

procedure TLccGuiNode.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  Invalidate(True);
end;

procedure TLccGuiNode.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
end;

procedure TLccGuiNode.SetSelected(AValue: Boolean);
begin
  if FSelected = AValue then Exit;
  FSelected := AValue;
end;

procedure TLccGuiNode.SetTop(AValue: Integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
end;

procedure TLccGuiNode.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  OwnerSelector.RebuildNodes;
end;

procedure TLccGuiNode.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
end;

{ TLccNodeSelectorBase }

function TLccNodeSelectorBase.AdjustedClientWidth: Integer;
begin
  Result := ClientWidth - 2
end;

procedure TLccNodeSelectorBase.BeginUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TLccNodeSelectorBase.CalculateAutoRanges;
begin
  VertScrollBar.Range := DefaultNodeHeight * LccVisibleNodes.Count;
  HorzScrollBar.Range := CalculatedHorzScrollRange;
  AutoScroll := True;
end;

procedure TLccNodeSelectorBase.DoOnFocusChanged(LccNode, OldLccNode: TLccGuiNode);
begin
  if Assigned(OnFocusChanged) then
    OnFocusChanged(Self, LccNode, OldLccNode);
end;

function TLccNodeSelectorBase.ClientPtToCurrentViewPt(APt: TPoint): TPoint;
begin
  Result.X := APt.X + HorzScrollBar.Position;
  Result.Y := APt.Y + VertScrollBar.Position;
end;

function TLccNodeSelectorBase.ClientPtToVisibleNode(ClientPt: TPoint; CurrentViewOnly: Boolean): TLccGuiNode;
var
  ViewPt: TPoint;
  Node: TLccGuiNode;
begin
  Result := nil;
  ViewPt := ClientPtToCurrentViewPt(ClientPt);
  if CurrentViewOnly then
    Node := FirstInView
  else
    Node := FirstVisible;
  while Assigned(Node) and not Assigned(Result) do
  begin
    if Node.PtInNode(ViewPt) then
    begin
      Result := Node;
      Break
    end else
    begin
      Node := NextVisible(Node);
      if CurrentViewOnly then
      begin
        if not IsInCurrentView(Node) then
        Node := nil
      end
    end;
  end;
end;

function TLccNodeSelectorBase.ClientRectToCurrentViewPt(ARect: TRect): TRect;
begin
  Result.TopLeft := ClientPtToCurrentViewPt(ARect.TopLeft);
  Result.BottomRight := ClientPtToCurrentViewPt(ARect.BottomRight);
end;

constructor TLccNodeSelectorBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLccNodes := TLccGuiNodeList.Create;
  LccVisibleNodes := TLccGuiVisibleNodeList.Create;
  FLccNodes.FOwnerSelector := Self;
  FDefaultNodeHeight := 44;
  FDetailsFont := TFont.Create;
  DetailsFont.Assign(Font);
  DetailsFont.Color := clGray;
  FDetailsIndent := 8;
  FCaptionIndent := 4;
  FCaptionLineCount := 1;
end;

destructor TLccNodeSelectorBase.Destroy;
begin
  FreeAndNil(FLccVisibleNodes);
  FreeAndNil(FLccNodes);
  FreeAndNil(FDetailsFont);
  inherited Destroy;
end;

procedure TLccNodeSelectorBase.DoOnResize;
begin
  inherited DoOnResize;
  RebuildNodes;
end;

procedure TLccNodeSelectorBase.DoOnShowHint(HintInfo: PHintInfo);
var
  LccNode: TLccGuiNode;
begin
  inherited DoOnShowHint(HintInfo);
  LccNode := ClientPtToVisibleNode(HintInfo^.CursorPos, True);
  if Assigned(LccNode) then
  begin
    HintInfo^.HintStr := 'NodeID: ' + LccNode.NodeIDStr;
    if LccNode.AliasID <> 0 then
      HintInfo^.HintStr := HintInfo^.HintStr + #13+#10 + 'AliasID: 0x' + IntToHex(LccNode.AliasID, 4);
  end;
end;

procedure TLccNodeSelectorBase.EndUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock < 1 then
  begin
    FUpdateLock := 0;
    RebuildNodes;
    Invalidate;
    Update
  end;
end;

function TLccNodeSelectorBase.First: TLccGuiNode;
begin
  Result := nil;
  if LccNodes.Count > 0 then
    Result := LccNodes[0];
end;

function TLccNodeSelectorBase.FirstInView: TLccGuiNode;
var
  Index: Integer;
begin
  Result := nil;
  if LccVisibleNodes.Count > 0 then
  begin
    Index := (VertScrollBar.Position div DefaultNodeHeight);
    if Index >= LccVisibleNodes.Count then
      Index := LccVisibleNodes.Count - 1;
    Result := LccVisibleNodes[Index];
  end;
end;

function TLccNodeSelectorBase.FirstVisible: TLccGuiNode;
begin
  Result := nil;
  if LccVisibleNodes.Count > 0 then
    Result := LccVisibleNodes[0];
end;

function TLccNodeSelectorBase.IsInCurrentView(Node: TLccGuiNode): Boolean;
var
  ResultRect, AViewRect: TRect;
begin
  Result := False;
  if Assigned(Node) then
  begin
    CurrentViewRect(AViewRect);
    IntersectRect(ResultRect, AViewRect, Node.BoundsRect);
    Result := not IsRectEmpty(ResultRect)
  end;
end;

procedure TLccNodeSelectorBase.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_DOWN :
      begin
        FocusedNode := NextVisible(FocusedNode);
        if not Assigned(FocusedNode) then
          FocusedNode := FirstVisible;
      end;
    VK_UP :
      begin
        FocusedNode := PreviousVisible(FocusedNode);
        if not Assigned(FocusedNode) then
          FocusedNode := FirstVisible;
      end;
    VK_HOME :
      begin
        FocusedNode := FirstVisible;
      end;
    VK_END :
      begin
        FocusedNode := LastVisible;
      end;
  end;
end;

function TLccNodeSelectorBase.Last: TLccGuiNode;
begin
  Result := nil;
  if LccNodes.Count > 0 then
    Result := LccNodes[LccNodes.Count - 1];
end;

function TLccNodeSelectorBase.LastVisible: TLccGuiNode;
begin
  Result := nil;
  if LccVisibleNodes.Count > 0 then
    Result := LccVisibleNodes[LccVisibleNodes.Count - 1];
end;

procedure TLccNodeSelectorBase.ScrollIntoView(Node: TLccGuiNode);
begin
  VertScrollBar.Position := Node.Top;
end;

procedure TLccNodeSelectorBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TLccGuiNode;
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
  MouseCapture := True;
  Node := ClientPtToVisibleNode(Point(X, Y), True);
  FocusedNode := Node;
end;

procedure TLccNodeSelectorBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  MouseCapture := False;
end;

function TLccNodeSelectorBase.Next(Node: TLccGuiNode): TLccGuiNode;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(Node) then
  begin
    i := Node.Index;
    Inc(i);
    if i < LccNodes.NodeList.Count then
      Result := LccNodes.Nodes[i]
  end;
end;

function TLccNodeSelectorBase.NextVisible(Node: TLccGuiNode): TLccGuiNode;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(Node) then
  begin
    i := Node.VisibleIndex;
    Inc(i);
    if i < LccVisibleNodes.NodeList.Count then
      Result := LccVisibleNodes.Nodes[i]
  end;
end;

procedure TLccNodeSelectorBase.Paint;
var
  Node: TLccGuiNode;
  IntersetRect: TRect;
begin
  inherited Paint;
  // TODO:  Find first and last in Viewport
  Node := FirstInView;
  while Assigned(Node) do
  begin
    Node.Paint(Canvas);
    Node := NextVisible(Node);
    if not IsInCurrentView(Node) then
      Node := nil
  end;
end;

function TLccNodeSelectorBase.Previous(Node: TLccGuiNode): TLccGuiNode;
var
  i: Integer;
begin
  i := LccNodes.NodeList.IndexOf(Node);
  if (i > -1) then
  begin
    Dec(i);
    if i < 0 then
      i := LccNodes.Count - 1;                   // Wrap
    Result := LccNodes.Nodes[i]
  end;
end;

function TLccNodeSelectorBase.PreviousVisible(Node: TLccGuiNode): TLccGuiNode;
var
  i: Integer;
begin
  i := LccVisibleNodes.NodeList.IndexOf(Node);
  if (i > -1) then
  begin
    Dec(i);
    if i < 0 then
      i := LccVisibleNodes.Count - 1;                   // Wrap
    Result := LccVisibleNodes.Nodes[i]
  end;
end;

procedure TLccNodeSelectorBase.RebuildNodes;
var
  i, NextTop, LocalVisibleIndex: Integer;
begin
  if UpdateLock > 0 then Exit;
  if Assigned(LccVisibleNodes) and Assigned(LccNodes) then
  begin
    CalculatedHorzScrollRange := 0;
    LccVisibleNodes.Clear;
    NextTop := 0;
    LocalVisibleIndex := 0;
    for i := 0 to LccNodes.Count - 1 do
    begin
      LccNodes[i].FIndex := i;
      if LccNodes[i].Visible then
      begin
        LccNodes[i].FVisibleIndex := LocalVisibleIndex;
        Inc(LocalVisibleIndex);
        LccVisibleNodes.Add(LccNodes[i]);
        LccNodes[i].SetBounds(0, NextTop, AdjustedClientWidth, DefaultNodeHeight);
        NextTop := LccNodes[i].Top + LccNodes[i].Height;
      end else
      begin
        LccNodes[i].Height := 0;
      end;
    end;

    UpdateScrollbars;
    Invalidate;
    Update;
  end;
end;

procedure TLccNodeSelectorBase.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  Invalidate;
  Update;
end;

procedure TLccNodeSelectorBase.SetCalculatedHorzScrollRange(AValue: Integer);
begin
  if AValue < 10 then
    AValue := 10;
  if FCalculatedHorzScrollRange=AValue then Exit;
  FCalculatedHorzScrollRange:=AValue;
  HorzScrollBar.Range := AValue;
end;


procedure TLccNodeSelectorBase.SetTextLayout(AValue: TTextLayout);
begin
  if FTextLayout = AValue then Exit;
  FTextLayout := AValue;
  Invalidate;
  Update;
end;

procedure TLccNodeSelectorBase.CurrentViewRect(var ARect: TRect);
begin
  ARect.Top := VertScrollBar.Position;
  ARect.Bottom := ARect.Top + VertScrollBar.Page;
  ARect.Left := HorzScrollBar.Position;
  ARect.Right := ARect.Left + HorzScrollBar.Page;
end;

procedure TLccNodeSelectorBase.WMShowWindow(var Message: TLMShowWindow);
begin
  inherited WMShowWindow(Message);
end;

procedure TLccNodeSelectorBase.WMKeyDown(var Message: TLMKeyDown);
begin
  inherited;
end;

procedure TLccNodeSelectorBase.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
end;

procedure TLccNodeSelectorBase.SetCaptionIndent(AValue: Integer);
begin
  if FCaptionIndent = AValue then Exit;
  FCaptionIndent := AValue;
  Invalidate;
  Update;
end;

procedure TLccNodeSelectorBase.SetCaptionLineCount(AValue: Integer);
begin
  if FCaptionLineCount = AValue then Exit;
  FCaptionLineCount := AValue;
  Invalidate;
  Update;
end;

procedure TLccNodeSelectorBase.SetDefaultNodeHeight(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FDefaultNodeHeight = AValue then Exit;
  FDefaultNodeHeight := AValue;
  RebuildNodes;
end;

procedure TLccNodeSelectorBase.SetDetailsFont(AValue: TFont);
begin
  FDetailsFont.Assign(AValue);
end;

procedure TLccNodeSelectorBase.SetDetailsIndent(AValue: Integer);
begin
  if FDetailsIndent = AValue then Exit;
  FDetailsIndent := AValue;
  Invalidate;
  Update
end;

procedure TLccNodeSelectorBase.SetFocusedNode(AValue: TLccGuiNode);
var
  OldFocused: TLccGuiNode;
begin
  if FFocusedNode = AValue then Exit;
  OldFocused := FocusedNode;
  FFocusedNode := AValue;
  DoOnFocusChanged(AValue, OldFocused);
  if Assigned(AValue) then
    AValue.Focused := True;
  if Assigned(OldFocused) then
    OldFocused.Focused := False;
end;


{ TLccGuiNodeList }

function TLccGuiNodeList.Add(ANodeID: TNodeID; AnAliasID: Word): TLccGuiNode;
begin
  Result := TLccGuiNode.Create;
  Result.FNodeID := ANodeID;
  Result.FAliasID := AnAliasID;
  Result.FOwnerSelector := OwnerSelector;
  NodeList.Add(Result);
  if Count > 0 then
    Result.SetBounds(0, Nodes[Count-1].Top + Nodes[Count-1].Height, OwnerSelector.AdjustedClientWidth, OwnerSelector.DefaultNodeHeight)
  else
    Result.SetBounds(0, 0, OwnerSelector.AdjustedClientWidth, OwnerSelector.DefaultNodeHeight);
end;

procedure TLccGuiNodeList.Clear;
begin
  OwnerSelector.BeginUpdate;
  try
    if Assigned(OwnerSelector.LccVisibleNodes) then
      OwnerSelector.LccVisibleNodes.Clear;
    if Assigned(NodeList) then
      NodeList.Clear;
  finally
    OwnerSelector.EndUpdate;
  end;
end;

constructor TLccGuiNodeList.Create;
begin
  inherited;
  FNodeList := TObjectList.Create;
  NodeList.OwnsObjects := True;
end;

procedure TLccGuiNodeList.Delete(Index: Integer);
begin
  NodeList.Delete(Index);
end;

function TLccGuiNodeList.Find(ANodeID: TNodeID): TLccGuiNode;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while (i < NodeList.Count) and not Assigned(Result) do
  begin
    if (Nodes[i].NodeID[0] = ANodeID[0]) and (Nodes[i].NodeID[1] = ANodeID[1]) then
      Result := Nodes[i];
    Inc(i);
  end;
end;

destructor TLccGuiNodeList.Destroy;
begin
  Clear;
  FreeAndNil(FNodeList);
  inherited Destroy;
end;

function TLccGuiNodeList.GetCount: Integer;
begin
  Result := NodeList.Count;
end;

function TLccGuiNodeList.GetNodes(Index: Integer): TLccGuiNode;
begin
  Result := NodeList[Index] as TLccGuiNode
end;

procedure TLccGuiNodeList.SetNodes(Index: Integer; AValue: TLccGuiNode);
begin
  NodeList[Index] := AValue
end;

end.

