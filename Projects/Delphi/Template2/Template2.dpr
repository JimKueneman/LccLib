program Template2;

uses
  System.StartUpCopy,
  FMX.Forms,
  unit_main in 'unit_main.pas' {TabbedwithNavigationForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabbedwithNavigationForm, TabbedwithNavigationForm);
  Application.Run;
end.
