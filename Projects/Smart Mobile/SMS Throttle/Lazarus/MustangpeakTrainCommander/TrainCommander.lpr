program TrainCommander;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, TrainCommanderUnit, TrainDatabaseUnit,
  lcc_node_train, lcc_node_controller, lcc_node_commandstation
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormTrainCommander, FormTrainCommander);
  Application.Run;
end.

