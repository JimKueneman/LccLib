program LccThrottleApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormLccThrottleApp in 'FormLccThrottleApp.pas' {LccThrottleAppForm},
  lcc_cdi_parser_2 in '..\..\Target-PC\lcc_cdi_parser_2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLccThrottleAppForm, LccThrottleAppForm);
  Application.Run;
end.
