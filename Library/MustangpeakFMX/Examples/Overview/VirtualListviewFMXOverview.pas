unit VirtualListviewFMXOverview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VirtualListviewFMX, FMX.Controls.Presentation, System.UIConsts, AniScrollerFMX,
  System.ImageList, FMX.ImgList, FMX.Edit, FMX.TextLayout;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    SpeedButton1: TSpeedButton;
    VirtualListviewFMX1: TVirtualListviewFMX;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure VirtualListviewFMX1GetItemText(Sender: TObject;
      Item: TVirtualListItem; TextLayout: TTextLayout);
    procedure VirtualListviewFMX1GetItemDetailText(Sender: TObject;
      Item: TVirtualListItem; DetailLineIndex: Integer;
      TextLayout: TTextLayout);
    procedure VirtualListviewFMX1ItemCustomDraw(Sender: TObject;
      Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas;
      TextLayout: TTextLayout; var Handled: Boolean);
    procedure VirtualListviewFMX1GetItemSize(Sender: TObject;
      Item: TVirtualListItem; var Width, Height: Real);
    procedure VirtualListviewFMX1GetItemImage(Sender: TObject;
      Item: TVirtualListItem; var ImageList: TCustomImageList;
      var ImageIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
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


procedure THeaderFooterForm.VirtualListviewFMX1GetItemDetailText(
  Sender: TObject; Item: TVirtualListItem; DetailLineIndex: Integer;
  TextLayout: TTextLayout);
begin
  TextLayout.Text := '';
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemImage(Sender: TObject;
  Item: TVirtualListItem; var ImageList: TCustomImageList;
  var ImageIndex: Integer);
begin
  ImageList := ImageList1;
  ImageIndex := 0;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemSize(Sender: TObject;
  Item: TVirtualListItem; var Width, Height: Real);
begin
  if Item.Index mod 2 = 0 then
    Height := 88;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemText(Sender: TObject;
  Item: TVirtualListItem; TextLayout: TTextLayout);
begin
  TextLayout.Text := 'This is item: ' + IntToStr(Item.Index);
end;

procedure THeaderFooterForm.VirtualListviewFMX1ItemCustomDraw(Sender: TObject;
  Item: TVirtualListItem; WindowRect: TRectF; ItemCanvas: TCanvas;
  TextLayout: TTextLayout; var Handled: Boolean);
begin
  Handled := False;
end;

end.
