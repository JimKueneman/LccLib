program VirtaulListviewFMXOverview;

uses
  System.StartUpCopy,
  FMX.Forms,
  VirtualListviewFMXOverview in 'VirtualListviewFMXOverview.pas' {HeaderFooterForm},
  VirtualListviewCellFrame in 'VirtualListviewCellFrame.pas' {FrameListviewItem: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THeaderFooterForm, HeaderFooterForm);
  Application.Run;
end.
