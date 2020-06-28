program OpenLCBThrottle;

uses
  System.StartUpCopy,
  FMX.Forms,
  TabbedTemplate in 'TabbedTemplate.pas' {OpenLcbThrottleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TOpenLcbThrottleForm, OpenLcbThrottleForm);
  Application.Run;
end.
