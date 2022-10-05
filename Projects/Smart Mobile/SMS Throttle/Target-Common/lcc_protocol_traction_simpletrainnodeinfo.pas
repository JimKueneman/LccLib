unit lcc_protocol_traction_simpletrainnodeinfo;

interface

uses
{$IFDEF DWSCRIPT}
  System.Types,
  System.Types.Convert,
  System.Time,
  System.Streams,
  System.Reader,
  System.Writer,
  System.Lists,
  System.Device.Storage,
  SmartCL.Device.Storage,
  SmartCL.Application,
  SmartCL.Components,
  SmartCL.System,
{$ELSE}
  Classes,
  SysUtils,
{$ENDIF}
  lcc_protocol_base,
  lcc_defines,
  lcc_node_messages;

type

    { TLccTrainSNIPObject }

  TLccTrainSNIPObject = class
  private
    FManufacturer: string;
    FOwner: string;
    FRoadName: string;
    FRoadNumber: string;
    FTrainClass: string;
    FTrainName: string;
    FValid: Boolean;
    FVersion: Byte;
  public
    property Manufacturer: string read FManufacturer write FManufacturer;
    property Owner: string read FOwner write FOwner;
    property RoadName: string read FRoadName write FRoadName;
    property TrainName: string read FTrainName write FTrainName;
    property TrainClass: string read FTrainClass write FTrainClass;
    property RoadNumber: string read FRoadNumber write FRoadNumber;
    property Version: Byte read FVersion write FVersion;
    property Valid: Boolean read FValid write FValid;
  end;

  { TTractionProtocolSimpleTrainNodeInfo }

  TTractionProtocolSimpleTrainNodeInfo = class(TNodeProtocolBase)
  private
    FManufacturer: string;
    FOwner: string;
    FRoadname: string;
    FRoadNumber: string;
    FTrainClass: string;
    FTrainName: string;
    FVersion: Word;
  public
    property Version: Word read FVersion;
    property Roadname: string read FRoadname;
    property TrainClass: string read FTrainClass;
    property RoadNumber: string read FRoadNumber;
    property TrainName: string read FTrainName;
    property Manufacturer: string read FManufacturer;
    property Owner: string read FOwner;

    function ProcessMessage(LccMessage: TLccMessage): Boolean;
  end;

implementation

{ TTractionProtocolSimpleTrainNodeInfo }

{
function TTractionProtocolSimpleTrainNodeInfo.ProcessMessage(LccMessage: TLccMessage;
  Traction: TTraction): Boolean;

    function NextString(AStrPtr: PChar): PChar;
    begin
      Result := AStrPtr;
      while Result^ <> #0 do
        Inc(Result);
      Inc(Result);
    end;

var
  StrPtr: PChar;
begin
  Result := True;
  StrPtr := @LccMessage.DataArray[0];

  FVersion := Ord( StrPtr^);
  Inc(StrPtr);
  FRoadname := StrPtr;
  StrPtr := NextString(StrPtr);
  FTrainClass := StrPtr;
  StrPtr := NextString(StrPtr);
  FRoadNumber := StrPtr;
  StrPtr := NextString(StrPtr);
  FTrainName := StrPtr;
  StrPtr := NextString(StrPtr);
  FManufacturer := StrPtr;
  StrPtr := NextString(StrPtr);
  FOwner := StrPtr;
  StrPtr := NextString(StrPtr);
  Traction.LegacyTechnology := Ord(StrPtr^);
  Inc(StrPtr);
  Traction.LegacyTrainID := Ord( StrPtr^) shl 8;
  Inc(StrPtr);
  Traction.LegacyTrainID := Ord(StrPtr^) or Traction.LegacyTrainID;
  Inc(StrPtr);
  if Ord( StrPtr^) > 0 then
    Traction.LegacyTrainID := Traction.LegacyTrainID or $C000;
  Inc(StrPtr);
  case Ord(StrPtr^) of
    0 : Traction.LegacySpeedSteps := 14;
    1 : Traction.LegacySpeedSteps := 28;
    2 : Traction.LegacySpeedSteps := 128
  else
    Traction.LegacySpeedSteps := 28;
  end;
  Valid := True;
end;
     }
{ TTractionProtocolSimpleTrainNodeInfo }

{function TTractionProtocolSimpleTrainNodeInfo.ProcessMessage(
  LccMessage: TLccMessage; Traction: TTraction): Boolean;
begin

end;

 TTractionProtocolSimpleTrainNodeInfo }

function TTractionProtocolSimpleTrainNodeInfo.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := False
end;

end.


