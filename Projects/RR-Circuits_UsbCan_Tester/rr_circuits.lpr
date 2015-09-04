program rr_circuits;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, form_settings, form_template, form_logging, lcc_raspberrypi_spiport;

{$R *.res}

begin
  Application.Title :='rr_cirkits';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormTemplate, FormTemplate);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormLogging, FormLogging);
  Application.Run;
end.

