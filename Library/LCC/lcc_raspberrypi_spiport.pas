unit lcc_raspberrypi_spiport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


function GetRaspberryPiSpiPortNames: string;


procedure Register;

implementation

procedure Register;
begin
end;

function GetRaspberryPiSpiPortNames: string;
begin
  result := ''
end;

end.

