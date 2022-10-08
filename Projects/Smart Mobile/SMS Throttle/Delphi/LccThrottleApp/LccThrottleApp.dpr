program LccThrottleApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormLccThrottleApp in 'FormLccThrottleApp.pas' {LccThrottleAppForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLccThrottleAppForm, LccThrottleAppForm);
  Application.Run;
end.
