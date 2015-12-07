unit VirtualListviewFMXOverview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  VirtualListviewFMX, FMX.Controls.Presentation, System.UIConsts, AniScrollerFMX,
  System.ImageList, FMX.ImgList, VirtualListviewCellFrame, FMX.Edit;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    SpeedButton1: TSpeedButton;
    VirtualListviewFMX1: TVirtualListviewFMX;
    procedure FormCreate(Sender: TObject);
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
  Frame: TFrameListviewItem;
begin
  VirtualListviewFMX1.Items.Clear;
  for i := 0 to 19 do
  begin
   Frame := TFrameListviewItem.Create(nil);
    Frame.LabelTitle.Text := 'Item: ' + IntToStr(i);
    Frame.LabelDetail.Text := 'SubName';
    Item := VirtualListviewFMX1.Items.AddAsFrame(Frame);
 //   Item := VirtualListviewFMX1.Items.Add;
   if i mod 2 = 0 then
      Item.Color := claCornsilk
    else
      Item.Color := claSpringgreen
  end;
end;

end.
