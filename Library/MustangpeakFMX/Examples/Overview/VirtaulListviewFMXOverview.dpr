program VirtaulListviewFMXOverview;

uses
  System.StartUpCopy,
  FMX.Forms,
  VirtualListviewFMXOverview in 'VirtualListviewFMXOverview.pas' {HeaderFooterForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THeaderFooterForm, HeaderFooterForm);
  Application.Run;
end.
