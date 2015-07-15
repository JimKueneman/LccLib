unit lcc_nodeselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, ExtCtrls, Menus, StdCtrls, Types;

type
  TLccNodeSelectorBase = class;

  { TLccGuiNode }

  TLccGuiNode = class(TPaintBox)
  private
    FCaptions: TStringList;
    FImageIndex: Integer;
    FOwnerSelector: TLccNodeSelectorBase;
    procedure SetCaptions(AValue: TStringList);
    procedure SetImageIndex(AValue: Integer);
  protected
    procedure Paint; override;
  public
    property Captions: TStringList read FCaptions write SetCaptions;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property OwnerSelector: TLccNodeSelectorBase read FOwnerSelector;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TLccGuiNodeList }

  TLccGuiNodeList = class(TComponent)
  private
    FNodeList: TList;
    FOwnerSelector: TLccNodeSelectorBase;
    function GetCount: Integer;
    function GetLccGuiNodes(Index: Integer): TLccGuiNode;
    procedure SetLccGuiNodes(Index: Integer; AValue: TLccGuiNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  protected
    property NodeList: TList read FNodeList write FNodeList;
  public
    property Count: Integer read GetCount;
    property LccGuiNodes[Index: Integer]: TLccGuiNode read GetLccGuiNodes write SetLccGuiNodes; default;
    property OwnerSelector: TLccNodeSelectorBase read FOwnerSelector;
    function Add: TLccGuiNode;
    procedure Delete(Index: Integer);
  end;

  { TLccNodeSelectorBase }

  TLccNodeSelectorBase = class(TScrollBox)
  private
    FAlignment: TAlignment;
    FImages: TImageList;
    FTextLayout: TTextLayout;
    FCaptionIndent: Integer;
    FCaptionLineCount: Integer;
    FDefaultNodeHeight: Integer;
    FDetailsFont: TFont;
    FDetailsIndent: Integer;
    FLccNodes: TLccGuiNodeList;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetTextLayout(AValue: TTextLayout);
    procedure SetCaptionIndent(AValue: Integer);
    procedure SetCaptionLineCount(AValue: Integer);
    procedure SetDefaultNodeHeight(AValue: Integer);
    procedure SetDetailsFont(AValue: TFont);
    procedure SetDetailsIndent(AValue: Integer);
  protected
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property CaptionIndent: Integer read FCaptionIndent write SetCaptionIndent;
    property CaptionLineCount: Integer read FCaptionLineCount write SetCaptionLineCount;
    property DefaultNodeHeight: Integer read FDefaultNodeHeight write SetDefaultNodeHeight;
    property DetailsIndent: Integer read FDetailsIndent write SetDetailsIndent;
    property DetailsFont: TFont read FDetailsFont write SetDetailsFont;
    property Images: TImageList read FImages write FImages;
    property LccNodes: TLccGuiNodeList read FLccNodes write FLccNodes;
    property TextLayout: TTextLayout read FTextLayout write SetTextLayout;
    procedure DoOnResize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TLccNodeSelector = class(TLccNodeSelectorBase)
  public
    property LccNodes: TLccGuiNodeList read FLccNodes write FLccNodes;
  published
    property Alignment;
    property CaptionIndent;
    property CaptionLineCount;
    property DefaultNodeHeight;
    property DetailsFont;
    property DetailsIndent;
    property Images;
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

{ TLccGuiNode }

constructor TLccGuiNode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptions := TStringList.Create;
  FImageIndex := -1;
end;

destructor TLccGuiNode.Destroy;
begin
  FreeAndNil(FCaptions);
  inherited Destroy;
end;

procedure TLccGuiNode.Paint;

  procedure EmptyRect(var ARect: TRect);
  begin
    ARect := Rect(0, 0, 0, 0);
  end;

var
  TextBox: TRect;
  i, Offset: Integer;
  TextExtent: TSize;
  TextRects: array of TRect;
  ImageWidth: Integer;
begin
  inherited Paint;

  ImageWidth := 0;
  if Assigned(OwnerSelector.Images) and (ImageIndex > -1) then
    ImageWidth := OwnerSelector.Images.Width + 4;

  if OwnerSelector.CaptionLineCount > 0 then
  begin
    SetLength(TextRects, Captions.Count);
    for i := 0 to Length(TextRects) - 1 do
      EmptyRect(TextRects[i]);

    Canvas.Font.Assign(OwnerSelector.Font);
    TextExtent := Canvas.TextExtent(Captions[0]);
    TextRects[0].Right := TextExtent.cx + ImageWidth;
    TextRects[0].Bottom := TextExtent.cy;
    OffsetRect(TextRects[0], OwnerSelector.CaptionIndent + ImageWidth, 0);
    if TextRects[0].Right > ClientWidth - OwnerSelector.CaptionIndent then
      TextRects[0].Right := ClientWidth - OwnerSelector.CaptionIndent;

    Canvas.Font.Assign(OwnerSelector.DetailsFont);
    i := 1;
    while (i < OwnerSelector.CaptionLineCount) and (i < Captions.Count) do
    begin
      TextExtent := Canvas.TextExtent(Captions[i]);
      TextRects[i].Right := TextExtent.cx + ImageWidth;
      TextRects[i].Bottom := TextExtent.cy;
      OffsetRect(TextRects[i], OwnerSelector.DetailsIndent + ImageWidth, 0);
      if TextRects[i].Right > ClientWidth - OwnerSelector.DetailsIndent then
        TextRects[i].Right := ClientWidth - OwnerSelector.DetailsIndent;
      OffsetRect(TextRects[i], 0, TextRects[i-1].Bottom);
      Inc(i)
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
          if TextBox.Bottom < ClientHeight then
            Offset := (ClientHeight - TextBox.Bottom) div 2
          else
            Offset := 0;

          for i := 0 to Length(TextRects) - 1 do
            OffsetRect(TextRects[i], 0, Offset);
        end;
      tlBottom :
        begin
          if TextBox.Bottom < ClientHeight then
          begin
            Offset := ClientHeight - TextBox.Bottom
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
          if TextBox.Right < ClientWidth then
            Offset := (ClientWidth - TextBox.Right) div 2
          else
            Offset := 0;

          for i := 0 to Length(TextRects) - 1 do
            OffsetRect(TextRects[i], Offset, 0);
        end;
      taRightJustify :
        begin
          if TextBox.Right < ClientWidth then
          begin
            Offset := ClientWidth - TextBox.Right
          end else
            Offset := 0;

          for i := 0 to Length(TextRects) - 1 do
            OffsetRect(TextRects[i], Offset, 0);
        end;
    end;

    Canvas.Brush.Color := Color;
    Canvas.FillRect(0, 0, ClientWidth, ClientHeight);
    if Assigned(OwnerSelector.Images) and (ImageIndex > -1) then
    case OwnerSelector.TextLayout of
      tlTop    : OwnerSelector.Images.Draw(Canvas, 4, 4, ImageIndex);
      tlCenter : OwnerSelector.Images.Draw(Canvas, 4, ((ClientHeight - OwnerSelector.Images.Height) div 2), ImageIndex);
      tlBottom : OwnerSelector.Images.Draw(Canvas, 4, (ClientHeight - OwnerSelector.Images.Height - 4), ImageIndex);
    end;
    Canvas.Font.Assign(OwnerSelector.Font);
    Canvas.TextRect(TextRects[0], TextRects[0].Left, TextRects[0].Top, Captions[0]);
    Canvas.Font.Assign(OwnerSelector.DetailsFont);
    i := 1;
    while (i < OwnerSelector.CaptionLineCount) and (i < Captions.Count) do
    begin
      Canvas.TextRect(TextRects[i], TextRects[i].Left, TextRects[i].Top, Captions[i]);
      Inc(i)
    end;
  end;
end;

procedure TLccGuiNode.SetCaptions(AValue: TStringList);
begin
  FCaptions.Assign(AValue);
  InvalidateControl(True, False);
  Update;
end;

procedure TLccGuiNode.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  InvalidateControl(True, False);
  Update;
end;

{ TLccNodeSelectorBase }

constructor TLccNodeSelectorBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLccNodes := TLccGuiNodeList.Create;
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
  FreeAndNil(FLccNodes);
  FreeAndNil(FDetailsFont);
  inherited Destroy;
end;

procedure TLccNodeSelectorBase.DoOnResize;
var
  i: Integer;
begin
  inherited DoOnResize;
  for i := 0 to LccNodes.Count - 1 do
  begin
    LccNodes[i].Width := ClientWidth;
  end;
end;

procedure TLccNodeSelectorBase.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  Invalidate;
  Update;
end;


procedure TLccNodeSelectorBase.SetTextLayout(AValue: TTextLayout);
begin
  if FTextLayout = AValue then Exit;
  FTextLayout := AValue;
  Invalidate;
  Update;
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
var
  i: Integer;
begin
  if AValue < 1 then
    AValue := 1;
  if FDefaultNodeHeight = AValue then Exit;
  FDefaultNodeHeight := AValue;
  for i := 0 to LccNodes.Count - 1 do
  begin
    LccNodes[i].Height := DefaultNodeHeight;
    if i > 0 then
      LccNodes[i].Top := LccNodes[i-1].Top + LccNodes[i-1].Height;
  end;
  Invalidate;
  Update
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


{ TLccGuiNodeList }

function TLccGuiNodeList.Add: TLccGuiNode;
begin
  Result := TLccGuiNode.Create(OwnerSelector);
  Result.Top := 0;
  Result.Height := OwnerSelector.DefaultNodeHeight;
  Result.Width := OwnerSelector.ClientWidth;
  Result.Parent := OwnerSelector;
  Result.FOwnerSelector := OwnerSelector;
  if Count > 0 then
    Result.Top := LccGuiNodes[Count-1].Top + LccGuiNodes[Count-1].Height;
  NodeList.Add(Result);
end;

procedure TLccGuiNodeList.Clear;
var
  i: Integer;
  Node: TObject;
begin
  for i := FNodeList.Count - 1 downto 0 do
  begin
    {$IFDEF FPC}
    Node := TObject( FNodeList[i]);
    FreeAndNil(Node);
    {$ELSE}
    Delphi way here
    {$ENDIF}
    Delete(i);
  end;
end;

constructor TLccGuiNodeList.Create;
begin
  inherited;
  FNodeList := TList.Create;
end;

procedure TLccGuiNodeList.Delete(Index: Integer);
begin

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

function TLccGuiNodeList.GetLccGuiNodes(Index: Integer): TLccGuiNode;
begin
  Result := TLccGuiNode( NodeList[Index])
end;

procedure TLccGuiNodeList.SetLccGuiNodes(Index: Integer; AValue: TLccGuiNode);
begin
  NodeList[Index] := AValue
end;

end.

