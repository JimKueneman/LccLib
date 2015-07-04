program project_template;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, form_settings, form_template, form_trace;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormTemplate, FormTemplate);
  Application.CreateForm(TFormMsgTrace, FormMsgTrace);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.Run;
end.

