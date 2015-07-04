program project_template;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, form_settings, form_template, form_logging;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormTemplate, FormTemplate);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormLogging, FormLogging);
  Application.Run;
end.

