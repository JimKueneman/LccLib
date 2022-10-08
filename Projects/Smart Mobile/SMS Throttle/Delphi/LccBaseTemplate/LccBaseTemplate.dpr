program LccBaseTemplate;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormLccBaseTemplate in 'FormLccBaseTemplate.pas' {LccBaseTemplate};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLccBaseTemplate, LccBaseTemplate);
  Application.Run;
end.
