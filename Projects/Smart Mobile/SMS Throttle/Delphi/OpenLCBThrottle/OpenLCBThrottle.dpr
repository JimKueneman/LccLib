program OpenLCBThrottle;

uses
  System.StartUpCopy,
  FMX.Forms,
  OpenLcbThrottleUnit in 'OpenLcbThrottleUnit.pas' {OpenLcbThrottleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TOpenLcbThrottleForm, OpenLcbThrottleForm);
  Application.Run;
end.
