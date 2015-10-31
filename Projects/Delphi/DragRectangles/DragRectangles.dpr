program DragRectangles;

uses
  System.StartUpCopy,
  FMX.Forms,
  Mainunit in 'Mainunit.pas' {MainForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
