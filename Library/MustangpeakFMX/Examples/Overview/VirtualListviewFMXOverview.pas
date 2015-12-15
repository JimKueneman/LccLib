unit VirtualListviewFMXOverview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Mustangpeak.FMX.VirtualListview, FMX.Controls.Presentation, System.UIConsts,
  Mustangpeak.FMX.AniScroller, System.ImageList, FMX.ImgList, FMX.Edit, FMX.TextLayout;

const
  ID_MAIN_IMAGE      = 0;
  ID_MAIN_TEXT       = 1;
  ID_ACCESSORY_IMAGE = 2;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    SpeedButton1: TSpeedButton;
    VirtualListviewFMX1: TVirtualListviewFMX;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure VirtualListviewFMX1GetItemSize(Sender: TObject; Item: TVirtualListItem; var Width, Height: Real);
    procedure VirtualListviewFMX1GetItemText(Sender: TObject; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout; var DetailLines: Integer);
    procedure VirtualListviewFMX1GetItemDetailText(Sender: TObject; Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout);
    procedure VirtualListviewFMX1GetItemImage(Sender: TObject;
      Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout);
    procedure VirtualListviewFMX1GetItemLayout(Sender: TObject;
      Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray);
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
  Sender: TObject; Item: TVirtualListItem; ID: Integer;
  TextLayout: TTextLayout);
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

procedure THeaderFooterForm.VirtualListviewFMX1GetItemImage(Sender: TObject;
  Item: TVirtualListItem; ID: Integer; ImageLayout: TVirtualImageLayout);
begin
  case ID  of
    ID_MAIN_IMAGE : begin
          ImageLayout.Images := ImageList1;
          ImageLayout.ImageIndex := 0;
          ImageLayout.Opacity := 1.0
        end;
    ID_ACCESSORY_IMAGE : begin
          ImageLayout.Images := ImageList1;
          ImageLayout.ImageIndex := 0;
          ImageLayout.Opacity := 1.0
        end;
  end;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemLayout(Sender: TObject; Item: TVirtualListItem; var Layout: TVirtualItemLayoutArray);
begin
  SetLength(Layout, 3);
  Layout[0] := TVirtualLayout.Create(ID_MAIN_IMAGE, TVirtualLayoutKind.Image, 24, TVirtualLayoutWidth.Fixed);
  Layout[1] := TVirtualLayout.Create(ID_MAIN_TEXT, TVirtualLayoutKind.Text, 0, TVirtualLayoutWidth.Variable);
  Layout[2] := TVirtualLayout.Create(ID_ACCESSORY_IMAGE, TVirtualLayoutKind.Image, 16, TVirtualLayoutWidth.Fixed);
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemSize(Sender: TObject;
  Item: TVirtualListItem; var Width, Height: Real);
begin
 // if Item.Index mod 2 = 0 then
 //   Height := 88;
end;

procedure THeaderFooterForm.VirtualListviewFMX1GetItemText(Sender: TObject;
  Item: TVirtualListItem; ID: Integer; TextLayout: TTextLayout;
  var DetailLines: Integer);
begin
  case ID of
    ID_MAIN_TEXT : TextLayout.Text := 'Item number: ' + IntToStr(Item.Index);
  else
    TextLayout.Text := 'Unknown ID';
  end;
end;

end.
