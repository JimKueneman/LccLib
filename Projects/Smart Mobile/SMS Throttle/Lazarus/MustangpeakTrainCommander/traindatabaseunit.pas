unit TrainDatabaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, lcc_defines, lcc_node;

type

  { TLccTrain }

  TLccTrain = class(TPersistent)
  private
    FDccAddress: Word;
    FLccNode: TLccNode;
    FLongAddress: Boolean;
    FManufacturer: string;
    FModel: string;
    FOwner: string;
    FRoadName: string;
    FRoadNumber: string;
    FSpeedLimit: Single;
    FSpeedSteps: TLccDccSpeedStep;
    function GetDccAddressAsStr: string;
  public
    property RoadName: string read FRoadName write FRoadName;
    property RoadNumber: string read FRoadNumber write FRoadNumber;
    property Manufacturer: string read FManufacturer write FManufacturer;
    property Owner: string read FOwner write FOwner;
    property Model: string read FModel write FModel;
    property DccAddress: Word read FDccAddress write FDccAddress;
    property LongAddress: Boolean read FLongAddress write FLongAddress;
    property SpeedLimit: Single read FSpeedLimit write FSpeedLimit;
    property SpeedSteps: TLccDccSpeedStep read FSpeedSteps write FSpeedSteps;

    property DccAddressAsStr: string read GetDccAddressAsStr;

    property LccNode: TLccNode read FLccNode write FLccNode;
  end;

  { TLccTrainDatabase }

  TLccTrainDatabase = class(TPersistent)
  private
    FTrainList: TObjectList;
    function GetTrain(Index: Integer): TLccTrain;
    procedure SetTrain(Index: Integer; AValue: TLccTrain);
  protected
 //   property TrainList: TObjectList read FTrainList write FTrainList;
  public
    property TrainList: TObjectList read FTrainList write FTrainList;

    constructor Create;
    destructor Destroy; override;

    function AddTrain(ARoadName, ARoadNumber: string; ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep; ALccNode: TLccNode): TLccTrain;
    procedure DeleteByDccAddress(ADccAddress: Word; ALongAddress: Boolean);
    function FindByDccAddress(ADccAddress: Integer; ALongAddress: Boolean; var ListIndex: Integer): TLccTrain;

    property Train[Index: Integer]: TLccTrain read GetTrain write SetTrain;
  end;

implementation

{ TLccTrain }

function TLccTrain.GetDccAddressAsStr: string;
begin
  Result := IntToStr(DccAddress);
end;

{ TLccTrainDatabase }

constructor TLccTrainDatabase.Create;
begin
  inherited;
  FTrainList := TObjectList.Create;
  TrainList.OwnsObjects := True;
end;

function TLccTrainDatabase.AddTrain(ARoadName, ARoadNumber: string;
  ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep;
  ALccNode: TLccNode): TLccTrain;
begin
  Result := TLccTrain.Create;
  Result.DccAddress := ADccAddress;
  Result.RoadName := ARoadName;
  Result.RoadNumber := ARoadNumber;
  Result.FLongAddress := ALongAddress;
  Result.FSpeedSteps := ASpeedStep;
  Result.LccNode := ALccNode;
  TrainList.Add(Result);
end;

procedure TLccTrainDatabase.DeleteByDccAddress(ADccAddress: Word; ALongAddress: Boolean);
var
  ATrain: TLccTrain;
  ListIndex: Integer;
begin
  ListIndex := -1;
  ATrain := FindByDccAddress(ADccAddress, ALongAddress, ListIndex);
  if Assigned(ATrain) then
    TrainList.Delete(ListIndex);
end;

destructor TLccTrainDatabase.Destroy;
begin
  FreeAndNil(FTrainList);
  inherited Destroy;
end;

function TLccTrainDatabase.FindByDccAddress(ADccAddress: Integer;
  ALongAddress: Boolean; var ListIndex: Integer): TLccTrain;
var
  i: Integer;
begin
  ListIndex := -1;
  Result := nil;
  for i := 0 to TrainList.Count - 1 do
   begin
    if (ADccAddress =  TLccTrain( TrainList[i]).DccAddress) and (ALongAddress =  TLccTrain( TrainList[i]).LongAddress) then
    begin
      Result := TLccTrain( TrainList[i]);
      ListIndex := i;
      Break;
    end;
   end;
end;

function TLccTrainDatabase.GetTrain(Index: Integer): TLccTrain;
begin
  Result := TLccTrain( TrainList[Index]);
end;

procedure TLccTrainDatabase.SetTrain(Index: Integer; AValue: TLccTrain);
begin
  TrainList[Index] := AValue;
end;

end.

