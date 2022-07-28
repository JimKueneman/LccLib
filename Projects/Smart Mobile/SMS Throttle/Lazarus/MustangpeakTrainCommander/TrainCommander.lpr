program TrainCommander;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, TrainCommanderUnit, TrainDatabaseUnit, lcc_ethernet_common,
  lcc_ethernet_websocket, lcc_ethernet_http;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormTrainCommander, FormTrainCommander);
  Application.Run;
end.

