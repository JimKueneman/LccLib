unit lcc_train_server;

interface

{$IFNDEF DWSCRIPT}
{$I lcc_compilers.inc}
{$ENDIF}

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
  {$IFNDEF ULTIBO}
    {$IFDEF FPC}
      {$IFNDEF FPC_CONSOLE_APP}
        ExtCtrls,
      {$ENDIF}
    {$ELSE}
      System.Types,
      FMX.Types,
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF DWSCRIPT}
    {$IFDEF FPC}
    contnrs,
    {$ELSE}
      System.Generics.Collections,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
  lcc_math_float16,
  lcc_protocol_traction_simpletrainnodeinfo,
  lcc_protocol_simplenodeinfo;

type

  TLccTrainControllerResult = (tcr_Ok, tcr_ControllerRefused, tcr_TrainRefused);

  { TLccTrainControllerObject }

  TLccTrainControllerObject = class
  private
    FRequestingNodeID: TNodeID;
    FRequestResult: TLccTrainControllerResult;
  public
    property RequestingNodeID: TNodeID read FRequestingNodeID write FRequestingNodeID;
    property RequestResult: TLccTrainControllerResult read FRequestResult write FRequestResult;
  end;

  { TLccTrainObject }

  TLccTrainObject = class
  private
    FController: TLccTrainControllerObject;
    FNodeAlias: Word;
    FNodeID: TNodeID;
    FSNIP: TLccSNIPObject;
    FSpeedActual: THalfFloat;
    FSpeedCommanded: THalfFloat;
    FSpeedSet: THalfFloat;
    FFunctions: TLccFunctions;
    FSpeedStatusEmergencyStop: Boolean;
    FTrainSNIP: TLccTrainSNIPObject;
    function GetFunctions(Index: Integer): Word;
    procedure SetFunctions(Index: Integer; AValue: Word);
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;
    property SpeedSet: THalfFloat read FSpeedSet;
    property SpeedCommanded: THalfFloat read FSpeedCommanded;
    property SpeedActual: THalfFloat read FSpeedActual;
    property SpeedStatusEmergencyStop: Boolean read FSpeedStatusEmergencyStop;
    property SNIP: TLccSNIPObject read FSNIP;
    property TrainSNIP: TLccTrainSNIPObject read FTrainSNIP;
    property Controller: TLccTrainControllerObject read FController write FController;

    constructor Create;
    destructor Destroy; override;
  end;

  { TLccTrainServer }

  TLccTrainServer = class
  private
    {$IFDEF DELPHI}
    FTrainList: TObjectList<TLccTrainObject>;
    {$ELSE}
    FTrainList: TObjectList;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function AddTrainObject(NewNodeID: TNodeID; NewAlias: Word): TLccTrainObject;
    function RemoveTrainObjectByAlias(TestAlias: Word): TLccTrainObject;
    function RemoveTrainObjectByNodeID(TestNodeID: TNodeID): TLccTrainObject;
    function FindTrainObjectByAlias(TestAlias: Word): TLccTrainObject;
    function FindTrainObjectByNodeID(TestNodeID: TNodeID): TLccTrainObject;
    function UpdateSNIP(AMessage: TLccMessage): TLccTrainObject;
    function UpdateTrainSNIP(AMessage: TLccMessage): TLccTrainObject;
    function UpdateListenerCount(AMessage: TLccMessage): TLccTrainObject;
    function UpdateSpeed(AMessage: TLccMessage): TLccTrainObject;
    function UpdateFunction(AMessage: TLccMessage): TLccTrainObject;
    function UpdateControllerAssign(AMessage: TLccMessage): TLccTrainObject;
    function UpdateControllerQuery(AMessage: TLccMessage): TLccTrainObject;

    procedure Clear;

    {$IFDEF DELPHI}
    property TrainList: TObjectList<TLccTrainObject> read FTrainList write FTrainList;
    {$ELSE}
    property TrainList: TObjectList read FTrainList write FTrainList;
    {$ENDIF}
  end;

implementation

{ TLccTrainObject }

function TLccTrainObject.GetFunctions(Index: Integer): Word;
begin
  if (Index > -1) and (Index < MAX_FUNCTIONS) then
    Result := FFunctions[Index]
  else
    Result := 0
end;

procedure TLccTrainObject.SetFunctions(Index: Integer; AValue: Word);
begin
   if (Index > -1) and (Index < MAX_FUNCTIONS) then
     FFunctions[Index] := AValue
end;

constructor TLccTrainObject.Create;
begin
  FSNIP := TLccSNIPObject.Create;
  FTrainSNIP := TLccTrainSNIPObject.Create;
  FController := TLccTrainControllerObject.Create;
end;

destructor TLccTrainObject.Destroy;
begin
  FreeAndNil(FSNIP);
  FreeAndNil(FTrainSNIP);
  FreeAndNil(FController);
  inherited Destroy;
end;

{ TLccTrainServer }

constructor TLccTrainServer.Create;
begin
  {$IFDEF DELPHI}
  FTrainList := TObjectList<TLccTrainObject>.Create(False);
  {$ELSE}
  FTrainList := TObjectList.Create(False);
  {$ENDIF}
end;

destructor TLccTrainServer.Destroy;
begin
  FreeAndNil(FTrainList);
  inherited Destroy;
end;

function TLccTrainServer.AddTrainObject(NewNodeID: TNodeID; NewAlias: Word): TLccTrainObject;
begin
  Result := TLccTrainObject.Create;
  TrainList.Add(Result);
  Result.NodeAlias := NewAlias;
  Result.NodeID := NewNodeID;
end;

function TLccTrainServer.RemoveTrainObjectByAlias(TestAlias: Word): TLccTrainObject;
var
  TrainObject: TLccTrainObject;
begin
  Result := nil;
  TrainObject := FindTrainObjectByAlias(TestAlias);
  if Assigned(TrainObject) then
  begin
    TrainList.Remove(TrainObject);
    Result := TrainObject;
  end;
end;

function TLccTrainServer.RemoveTrainObjectByNodeID(TestNodeID: TNodeID): TLccTrainObject;
var
  TrainObject: TLccTrainObject;
begin
  Result := nil;
  TrainObject := FindTrainObjectByNodeID(TestNodeID);
  if Assigned(TrainObject) then
  begin
    TrainList.Remove(TrainObject);
    Result := TrainObject;
  end;
end;

function TLccTrainServer.FindTrainObjectByAlias(TestAlias: Word): TLccTrainObject;
var
  i: Integer;
  TrainObject: TLccTrainObject;
begin
  Result := nil;
  for i := 0 to TrainList.Count - 1 do
  begin
    TrainObject := TrainList.Items[i] as TLccTrainObject;
    if TestAlias = TrainObject.NodeAlias then
    begin
      Result := TrainObject;
      Break
    end;
  end;
end;

function TLccTrainServer.FindTrainObjectByNodeID(TestNodeID: TNodeID): TLccTrainObject;
var
  i: Integer;
  TrainObject: TLccTrainObject;
begin
  Result := nil;
  for i := 0 to TrainList.Count - 1 do
  begin
    TrainObject := TrainList.Items[i] as TLccTrainObject;
    if EqualNodeID(TestNodeID, TrainObject.NodeID, False) then
    begin
      Result := TrainObject;
      Break
    end;
  end;
end;

function TLccTrainServer.UpdateSNIP(AMessage: TLccMessage): TLccTrainObject;
var
  Version,
  UserVersion: byte;
  Manufacturer,
  Model,
  HardwareVersion,
  SoftwareVersion,
  UserName,
  UserDescription: string;
begin
  Result := FindTrainObjectByNodeID(AMessage.SourceID);
  if Assigned(Result) then
  begin
    Version := 0;
    UserVersion := 0;
    Manufacturer := '';
    Model := '';
    HardwareVersion := '';
    SoftwareVersion := '';
    UserName := '';
    UserDescription := '';
    AMessage.ExtractSimpleNodeIdentInfo(Version, Manufacturer, Model, HardwareVersion, SoftwareVersion, UserVersion, UserName, UserDescription);
    Result.SNIP.Version := Version;
    Result.SNIP.Manufacturer := Manufacturer;
    Result.SNIP.Model := Model;
    Result.SNIP.HardwareVersion := HardwareVersion;
    Result.SNIP.SoftwareVersion := SoftwareVersion;
    Result.SNIP.UserVersion := UserVersion;
    Result.SNIP.UserName := UserName;
    Result.SNIP.UserDescription := UserDescription;
    Result.SNIP.Valid := True;
  end;
end;

function TLccTrainServer.UpdateTrainSNIP(AMessage: TLccMessage): TLccTrainObject;
var
  TrainVersion: Byte;
  TrainRoadName,
  TrainClass,
  TrainRoadNumber,
  TrainName,
  TrainManufacturer,
  TrainOwner: string;
begin
  Result := FindTrainObjectByNodeID(AMessage.SourceID);
  if Assigned(Result) then
  begin
    TrainVersion := 0;
    TrainRoadName := '';
    TrainClass := '';
    TrainRoadNumber := '';
    TrainName := '';
    TrainManufacturer := '';
    TrainOwner := '';
    AMessage.ExtractSimpleTrainNodeIdentInfo(TrainVersion, TrainRoadName, TrainClass, TrainRoadNumber, TrainName, TrainManufacturer, TrainOwner);
    Result.TrainSNIP.Manufacturer := TrainManufacturer;
    Result.TrainSNIP.Owner := TrainOwner;
    Result.TrainSNIP.Roadname := TrainRoadName;
    Result.TrainSNIP.RoadNumber := TrainRoadNumber;
    Result.TrainSNIP.TrainClass := TrainClass;
    Result.TrainSNIP.TrainName := TrainName;
    Result.TrainSNIP.Version := TrainVersion;
    Result.TrainSNIP.Valid := True;
  end;
end;

function TLccTrainServer.UpdateListenerCount(AMessage: TLccMessage): TLccTrainObject;
begin
  Result := FindTrainObjectByNodeID(AMessage.SourceID);
  if Assigned(Result) then
  begin

  end;
end;

function TLccTrainServer.UpdateSpeed(AMessage: TLccMessage): TLccTrainObject;
begin
  Result := FindTrainObjectByNodeID(AMessage.SourceID);
  if Assigned(Result) then
  begin
    Result.FSpeedSet := AMessage.TractionExtractSetSpeed;
    Result.FSpeedCommanded := AMessage.TractionExtractCommandedSpeed;
    Result.FSpeedActual :=  AMessage.TractionExtractActualSpeed;
    Result.FSpeedStatusEmergencyStop := AMessage.TractionExtractSpeedStatus and TRACTION_SPEED_STATUS_E_STOP = TRACTION_SPEED_STATUS_E_STOP;
  end;
end;

function TLccTrainServer.UpdateFunction(AMessage: TLccMessage): TLccTrainObject;
begin
  Result := FindTrainObjectByNodeID(AMessage.SourceID);
  if Assigned(Result) then
    Result.Functions[AMessage.TractionExtractFunctionAddress] := AMessage.TractionExtractFunctionValue;
end;

function TLccTrainServer.UpdateControllerAssign(AMessage: TLccMessage): TLccTrainObject;
begin
  Result := FindTrainObjectByNodeID(AMessage.SourceID);
  if Assigned(Result) then
  begin
    case AMessage.TractionExtractControllerAssignResult of
      TRACTION_CONTROLLER_CONFIG_REPLY_OK :
        begin
          Result.Controller.RequestResult := tcr_Ok;
        end;
      TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
        begin
          Result.Controller.RequestResult := tcr_ControllerRefused;
        end;
      TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
        begin
          Result.Controller.RequestResult := tcr_TrainRefused;
        end;
    end;
  end;
end;

function TLccTrainServer.UpdateControllerQuery(AMessage: TLccMessage): TLccTrainObject;
begin
  Result := FindTrainObjectByNodeID(AMessage.SourceID);
  if Assigned(Result) then
    AMessage.ExtractDataBytesAsNodeID(3, Result.Controller.FRequestingNodeID);
end;

procedure TLccTrainServer.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to TrainList.Count - 1 do
      TrainList[i].Free;
  finally
    TrainList.Clear;
  end;

end;

end.

