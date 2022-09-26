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
  lcc_alias_server;

type

  { TLccTrainObject }

  TLccTrainObject = class
  private
    FNodeAlias: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;

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

    {$IFDEF DELPHI}
    property TrainList: TObjectList<TLccTrainServerObject> read FTrainList write FTrainList;
    {$ELSE}
    property TrainList: TObjectList read FTrainList write FTrainList;
    {$ENDIF}
  end;

implementation

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
  i: Integer;
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
  i: Integer;
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

end.

