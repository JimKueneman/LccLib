unit VirtualListviewFMXOverview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Mustangpeak.FMX.VirtualListview, FMX.Controls.Presentation, System.UIConsts,
  Mustangpeak.FMX.AniScroller, System.ImageList, FMX.ImgList, FMX.Edit, FMX.TextLayout,
  FMX.Ani;

const
  ID_CHECK_IMAGE     = 0;
  ID_MAIN_IMAGE      = 1;
  ID_MAIN_TEXT       = 2;
  ID_ACCESSORY_IMAGE = 3;

  ID_SWIPE_DELETE    = 100;
  ID_SWIPE_ARCHIVE   = 101;
  ID_SWIPE_SNOW      = 102;
  ID_SWIPE_FREEZE    = 103;
  ID_SWIPE_MELT      = 104;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    SpeedButton1: TSpeedButton;
    VirtualListviewFMX1: TVirtualListviewFMX;
    ImageList1: TImageList;
    ColorAnimation1: TColorAnimation;
    FloatKeyAnimation1: TFloatKeyAnimation;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure VirtualListviewFMX1GetItemText(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer);
    procedure VirtualListviewFMX1GetItemImage(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout);
    procedure VirtualListviewFMX1ItemDrawBackground(Sender: TCustomVirtualListview; Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; var Handled: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
    procedure VirtualListviewFMX1ItemLayoutElementClick(Sender: TCustomVirtualListview; Item: TVirtualListItem; Button: TMouseButton; Shift: TShiftState; ID: Integer);
    procedure VirtualListviewFMX1GetItemLayoutSwipe(Sender: TCustomVirtualListview; Item: TVirtualListItem; var Layout: TVirtualItemLayoutSwipeArray);
    procedure VirtualListviewFMX1GetItemLayout(Sender: TCustomVirtualListview; Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray);
    procedure VirtualListviewFMX1GetItemTextDetail(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure VirtualListviewFMX1GetItemTextSwipe(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout);
    procedure VirtualListviewFMX1GetItemTextDetailSwipe(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout);
    procedure VirtualListviewFMX1ItemLayoutElementSwipeClick(Sender: TCustomVirtualListview; Item: TVirtualListItem; Button: TMouseButton; Shift: TShiftState; ID: Integer);
  private
    FItemGradient: TGradient;
    { Private declarations }
  public
    { Public declarations }
    property ItemGradient: TGradient read FItemGradient write FItemGradient;
  end;

var
  HeaderFooterForm: THeaderFooterForm;

implementation

{$R *.fmx}

procedure THeaderFooterForm.FormCreate(Sender: TObject);
var
  i: Integer;
  Item: TVirtualListItem;
begin
  ItemGradient := TGradient.Create;
  ItemGradient.Color := claBlueviolet;
  ItemGradient.Color1 := claBlue;
  ItemGradient.Style := TGradientStyle.Linear;

Exit;

  VirtualListviewFMX1.Items.Clear;
  for i := 0 to 3 do
  begin
    Item := VirtualListviewFMX1.Items.Add;
 {  if i mod 2 = 0 then
      Item.Color := claCornsilk
    else
      Item.Color := claSpringgreen       }
  end;
end;


procedure THeaderFooterForm.FormDestroy(Sender: TObject);
begin
  ItemGradient.DisposeOf;
end;

procedure THeaderFooterForm.SpeedButton1Click(Sender: TObject);
begin
  VirtualListviewFMX1.InvalidateRect(VirtualListviewFMX1.LocalRect);
end;

procedure THeaderFooterForm.SpeedButton2Click(Sender: TObject);
begin
  VirtualListviewFMX1.Repaint;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemImage(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout);
begin
  case ID  of
    ID_CHECK_IMAGE :
        begin
          ImageLayout.Images := ImageList1;
          ImageLayout.Opacity := 1.0;
          if Item.Checked then
            ImageLayout.ImageIndex := 2
          else
            ImageLayout.ImageIndex := 1;
        end;
    ID_MAIN_IMAGE : begin
          ImageLayout.Images := ImageList1;
          ImageLayout.ImageIndex := 0;
          ImageLayout.Opacity := 0.25
        end;
    ID_ACCESSORY_IMAGE : begin
          ImageLayout.Images := ImageList1;
          ImageLayout.ImageIndex := 3;
          ImageLayout.Opacity := 1.0;
          ImageLayout.Padding.Right := 8;
        end;
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemLayout(Sender: TCustomVirtualListview; Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray);
begin
  if SpeedButton1.IsPressed then
  begin
    SetLength(Layout, 4);
    Layout[0] := TVirtualLayout.Create(ID_CHECK_IMAGE, TVirtualLayoutKind.Image, 24 + VirtualListviewFMX1.DefaultLayoutImage.Padding.Left + VirtualListviewFMX1.DefaultLayoutImage.Padding.Right, TVirtualLayoutWidth.Fixed);
    Layout[1] := TVirtualLayout.Create(ID_MAIN_IMAGE, TVirtualLayoutKind.Image, 48 + VirtualListviewFMX1.DefaultLayoutImage.Padding.Left + VirtualListviewFMX1.DefaultLayoutImage.Padding.Right, TVirtualLayoutWidth.Fixed);
    Layout[2] := TVirtualLayout.Create(ID_MAIN_TEXT, TVirtualLayoutKind.Text, 0, TVirtualLayoutWidth.Variable);
    Layout[3] := TVirtualLayout.Create(ID_ACCESSORY_IMAGE, TVirtualLayoutKind.Image, 24 + 8 + VirtualListviewFMX1.DefaultLayoutImage.Padding.Right, TVirtualLayoutWidth.Fixed);
  end else
  begin
    SetLength(Layout, 3);
    Layout[0] := TVirtualLayout.Create(ID_MAIN_IMAGE, TVirtualLayoutKind.Image, 48 + VirtualListviewFMX1.DefaultLayoutImage.Padding.Left + VirtualListviewFMX1.DefaultLayoutImage.Padding.Right, TVirtualLayoutWidth.Fixed);
    Layout[1] := TVirtualLayout.Create(ID_MAIN_TEXT, TVirtualLayoutKind.Text, 0, TVirtualLayoutWidth.Variable);
    Layout[2] := TVirtualLayout.Create(ID_ACCESSORY_IMAGE, TVirtualLayoutKind.Image, 24 + 8 + VirtualListviewFMX1.DefaultLayoutImage.Padding.Right, TVirtualLayoutWidth.Fixed);
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemLayoutSwipe(Sender: TCustomVirtualListview; Item: TVirtualListItem; var Layout: TVirtualItemLayoutSwipeArray);
begin
  if TSwipeState.Left in Item.SwipeState then
  begin
    SetLength(Layout, 2);
    Layout[1] := TVirtualLayoutSwipe.Create(ID_SWIPE_DELETE, TVirtualLayoutKind.Text, 80, claRed, 1.0);
    Layout[0] := TVirtualLayoutSwipe.Create(ID_SWIPE_ARCHIVE, TVirtualLayoutKind.Text, 80, claGreen, 1.0);
  end else
  if TSwipeState.Right in Item.SwipeState then
  begin
    SetLength(Layout, 3);
    Layout[2] := TVirtualLayoutSwipe.Create(ID_SWIPE_SNOW, TVirtualLayoutKind.Text, 80, claBlue, 1.0);
    Layout[1] := TVirtualLayoutSwipe.Create(ID_SWIPE_FREEZE, TVirtualLayoutKind.Text, 100, claAqua, 1.0);
    Layout[0] := TVirtualLayoutSwipe.Create(ID_SWIPE_MELT, TVirtualLayoutKind.Text, 80, claAquamarine, 1.0);
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemText(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer);
begin
  case ID of
    ID_MAIN_TEXT : TextLayout.Text := 'Item number: ' + IntToStr(Item.iIndex);
  else
    TextLayout.Text := 'Unknown ID';
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemTextDetail(Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout);
begin
  case ID of
    0 : begin
          TextLayout.Text := 'Detail 1';
        end;
    1 : begin
          TextLayout.Text := 'Detail 2';
        end;
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemTextDetailSwipe(
  Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer;
  TextLayout: TTextLayout);
begin
  case ID of
    0 : begin
          TextLayout.Text := 'Swipe Detail 1';
        end;
    1 : begin
          TextLayout.Text := 'Swipe Detail 2';
        end;
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemTextSwipe(
  Sender: TCustomVirtualListview; Item: TVirtualListItem; ID: Integer;
  TextLayout: TTextLayout);
begin
  case ID of
    ID_SWIPE_DELETE : TextLayout.Text := 'Delete';
    ID_SWIPE_ARCHIVE : TextLayout.Text := 'Archive';
    ID_SWIPE_SNOW : TextLayout.Text := 'Snow';
    ID_SWIPE_FREEZE : TextLayout.Text := 'Freeze';
    ID_SWIPE_MELT : TextLayout.Text := 'Melt';
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1ItemDrawBackground(Sender: TCustomVirtualListview; Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas; var Handled: Boolean);
begin
  if SpeedButton2.IsPressed then
  begin
    Handled := True;
    WindowRect.Inflate(-2, -2);
    ItemCanvas.Stroke.Thickness := 1.0;
    ItemCanvas.Stroke.Color := claBlue;
    ItemCanvas.Fill.Kind := TBrushKind.Gradient;
    ItemCanvas.Fill.Gradient := ItemGradient;
    ItemCanvas.FillRect(WindowRect, 10.0, 10.0, [TCorner.TopLeft,TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight], 0.5, TCornerType.Round);
    ItemCanvas.DrawRect(WindowRect, 10.0, 10.0, [TCorner.TopLeft,TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight], 1.0, TCornerType.Round);
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1ItemLayoutElementClick(Sender: TCustomVirtualListview; Item: TVirtualListItem; Button: TMouseButton; Shift: TShiftState; ID: Integer);
begin
  case ID of
    ID_CHECK_IMAGE     : ShowMessage('Checkbox Image clicked');
    ID_MAIN_IMAGE      : ShowMessage('Main Image clicked');
    ID_MAIN_TEXT       : ShowMessage('Text area clicked');
    ID_ACCESSORY_IMAGE : ShowMessage('Accessory Image clicked');
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1ItemLayoutElementSwipeClick(
  Sender: TCustomVirtualListview; Item: TVirtualListItem; Button: TMouseButton;
  Shift: TShiftState; ID: Integer);
begin
  case ID of
    ID_SWIPE_DELETE     : ShowMessage('Swipe Delete Clicked');
    ID_SWIPE_ARCHIVE    : ShowMessage('Swipe Archive Clicked');
    ID_SWIPE_SNOW       : ShowMessage('Swipe Snow Clicked');
    ID_SWIPE_FREEZE     : ShowMessage('Swipe Freeze Clicked');
    ID_SWIPE_MELT       : ShowMessage('Swipe Melt Clicked');
  end;
end;

end.
