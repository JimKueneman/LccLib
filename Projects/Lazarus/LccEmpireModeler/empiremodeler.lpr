program empiremodeler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitmain, unitobjectwizardform, unitsegmentwizardform, turnoutoptions,
  signalheadoptions
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormObjectWizard, FormObjectWizard);
  Application.CreateForm(TFormSegmentWizard, FormSegmentWizard);
  Application.CreateForm(TFormSignalHeadOptions, FormSignalHeadOptions);
  Application.Run;
end.

