unit VirtualListviewCellFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Layouts;

type
  TFrameListviewItem = class(TFrame)
    LayoutMainImage: TLayout;
    LayoutAccessoryImage: TLayout;
    Layout1: TLayout;
    LabelTitle: TLabel;
    LabelDetail: TLabel;
    ImageMain: TImage;
    ImageAccessory: TImage;
    Layout2: TLayout;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
