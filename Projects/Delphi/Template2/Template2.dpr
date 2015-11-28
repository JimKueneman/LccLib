program Template2;

uses
  System.StartUpCopy,
  FMX.Forms,
  unit_main in 'unit_main.pas' {TabbedwithNavigationForm},
  settings_frame in 'settings_frame.pas' {FrameSettings: TFrame},
  main_frame in 'main_frame.pas' {FrameMain: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabbedwithNavigationForm, TabbedwithNavigationForm);
  Application.Run;
end.
