program TcpTester;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, form_main, form_settings, form_logging, form_properties, form_about;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormLogging, FormLogging);
  Application.CreateForm(TFormNodeProperties, FormNodeProperties);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.

