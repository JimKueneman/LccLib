unit lcc_alias_server;

{$IFNDEF DWSCRIPT}
{$I lcc_compilers.inc}
{$ENDIF}

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
    {$IFNDEF ULTIBO}
      {$IFDEF FPC}
        ExtCtrls,
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
  lcc_utilities;

type

  { TLccAliasMap }

  TLccAliasMap = class
  private
    FAliasID: Word;
    FNodeID: TNodeID;
  public
    property AliasID: Word read FAliasID;
    property NodeID: TNodeID read FNodeID;

    constructor Create(ANodeID: TNodeID; AnAliasID: Word);
  end;

  { TLccAliasServer }

  TLccAliasServer = class
  private
    {$IFDEF DELPHI}
    FAliasSortedMap: TObjectList<TLccAliasMap>;
    FNodeIDSortedMap: TObjectList<TLccAliasMap>;
   {$ELSE}
    FAliasSortedMap: TObjectList;
    FNodeIDSortedMap: TObjectList;
   {$ENDIF}
    FOnAddMapping: TNotifyEvent;
    FOnDeleteMapping: TNotifyEvent;
    FIsDirty: Boolean;
    function GetCount: Integer;
  protected
    property IsDirty: Boolean read FIsDirty;

    {$IFDEF DELPHI}
    property AliasSortedMap: TObjectList<TLccAliasMap> read FAliasSortedMap write FAliasSortedMap;
    property NodeIDSortedMap: TObjectList<TLccAliasMap> read FNodeIDSortedMap write FNodeIDSortedMap;
    {$ELSE}
    property AliasSortedMap: TObjectList read FAliasSortedMap write FAliasSortedMap;
    property NodeIDSortedMap: TObjectList read FNodeIDSortedMap write FNodeIDSortedMap;
    {$ENDIF}

    procedure DoAddMapping; virtual;
    procedure DoDeleteMapping; virtual;
    function RemoveMappingByNodeID(ANodeID: TNodeID): Boolean;
    function RemoveMappingByAliasID(AnAliasID: Word): Boolean;
    procedure SortMaps;
  public
    property Count: Integer read GetCount;

    property OnDeleteMapping: TNotifyEvent read FOnDeleteMapping write FOnDeleteMapping;
    property OnAddMapping: TNotifyEvent read FOnAddMapping write FOnAddMapping;

    constructor Create;
    destructor Destroy; override;

    function AddMapping(ANodeID: TNodeID; AnAliasID: Word): TLccAliasMap;
    procedure Clear;
    function FindInNodeIDSortedMap(ANodeID: TNodeID; var MapIndex: Integer): TLccAliasMap;
    function FindInAliasSortedMap(AnAliasID: Word; var MapIndex: Integer): TLccAliasMap;
    procedure RemoveMapping(AnAliasID: Word);
    function ValidateAlias(AnAlias: Word): Boolean;


  end;


implementation


{ TLccAliasMap }

constructor TLccAliasMap.Create(ANodeID: TNodeID; AnAliasID: Word);
begin
  FAliasID := AnAliasID;
  FNodeID[0] := ANodeID[0];
  FNodeID[1] := ANodeID[1];
end;

function SortFuncNodeID(Item1, Item2: {$IFDEF DWSCRIPT}TObject{$ELSE}Pointer{$ENDIF}): Integer;
var
  Mapping1, Mapping2: TLccAliasMap;
begin
  Mapping1 := TLccAliasMap(Item1);
  Mapping2 := TLccAliasMap(Item2);
  if Mapping1.NodeID[1] > Mapping2.NodeID[1] then
    Result := 1
  else
  if Mapping1.NodeID[1] < Mapping2.NodeID[1] then
    Result := -1
  else
  if Mapping1.NodeID[0] > Mapping2.NodeID[0] then
    Result := 1
  else
  if Mapping1.NodeID[0] < Mapping2.NodeID[0] then
    Result := -1
  else
    Result := 0; /// THIS IS A BAD ERROR.... Duplicate NodeIDs
end;

function SortFuncAlias(Item1, Item2: {$IFDEF DWSCRIPT}TObject{$ELSE}Pointer{$ENDIF}): Integer;
var
  Mapping1, Mapping2: TLccAliasMap;
begin
  Mapping1 := TLccAliasMap(Item1);
  Mapping2 := TLccAliasMap(Item2);
  if Mapping1.AliasID > Mapping2.AliasID then
    Result := 1
  else
  if Mapping1.AliasID < Mapping2.AliasID then
    Result := -1
  else
    Result := 0; /// THIS IS A BAD ERROR.... Duplicate Alias
end;

 {
procedure BinarySearchAlias(SearchNum : integer);
var
  Found, Failed: Boolean;
  Last, First, MidPoint: integer;
begin
  Found := False;
  Failed:= False;
  Last := UPPER_BOUND;
  First := 1;
  repeat
    Midpoint := (First + Last) DIV 2;
    if Nums[MidPoint] = SearchNum then
      begin
        Found := True;
        writeln('Found at position ' ,MidPoint);
      end
    else if (First > Last) then
      Failed := true;
    else if Nums[MidPoint] < SearchNum then
      First := MidPoint + 1;
    else
      Last := MidPoint - 1;
  until Found or Failed;
  if Failed then
    writeln('Not found.');
end;   }

{ TLccAliasServer }

function TLccAliasServer.AddMapping(ANodeID: TNodeID; AnAliasID: Word): TLccAliasMap;
var
  MapIndex: Integer;
begin
  MapIndex := -1;

  // See if that Alias is Mapped
  Result := FindInAliasSortedMap(AnAliasID, MapIndex);

  if Assigned(Result) then
  begin
    // Check if our mapping is correct
    if not EqualNodeID(ANodeID, Result.NodeID, False) then
    begin // Not correct so dump it and then create a new one below
      RemoveMappingByAliasID(AnAliasID);
      Result := nil;
    end;
  end;

  if not Assigned(Result) then
  begin
    Result := TLccAliasMap.Create(ANodeID, AnAliasID);
    AliasSortedMap.Add(Result);  // Owns the object in non-SMS
    NodeIDSortedMap.Add(Result);
    FIsDirty := True;
    DoAddMapping;
  end;
end;

function TLccAliasServer.RemoveMappingByNodeID(ANodeID: TNodeID): Boolean;
var
  MapIndex: Integer;
begin
  Result := False;
  MapIndex := -1;
  FindInNodeIDSortedMap(ANodeID, MapIndex);
  if MapIndex > -1 then
  begin
    {$IFDEF DWSCRIPT}
    NodeIDSortedMap.Remove(MapIndex);
    AliasSortedMap.Remove(MapIndex);
    {$ELSE}
    NodeIDSortedMap.Delete(MapIndex);
    AliasSortedMap.Delete(MapIndex); // Owns the object in non-SMS
    {$ENDIF}
    DoDeleteMapping;
    Result := True;
  end;
end;

function TLccAliasServer.RemoveMappingByAliasID(AnAliasID: Word): Boolean;
var
  MapIndexInAliasSortedMap, MapIndexInNodeIDSortedMap: Integer;
  AliasMap: TLccAliasMap;
begin
  Result := False;
  MapIndexInAliasSortedMap := -1;
  MapIndexInNodeIDSortedMap := -1;
  AliasMap := FindInAliasSortedMap(AnAliasID, MapIndexInAliasSortedMap);
  if Assigned(AliasMap) then
  begin
    FindInNodeIDSortedMap(AliasMap.NodeID, MapIndexInNodeIDSortedMap);
    if  MapIndexInNodeIDSortedMap > -1 then
    begin
      {$IFDEF DWSCRIPT}
      NodeIDSortedMap.Remove(MapIndexInNodeIDSortedMap);
      {$ELSE}
      NodeIDSortedMap.Delete(MapIndexInNodeIDSortedMap);
      {$ENDIF}
    end;

    if  MapIndexInAliasSortedMap > -1 then
    begin
      {$IFDEF DWSCRIPT}
      AliasSortedMap.Remove(MapIndexInAliasSortedMap); // Owns the object in non-SMS
      {$ELSE}
      AliasSortedMap.Delete(MapIndexInAliasSortedMap);  // Owns the object in non-SMS
      {$ENDIF}
    end;

    DoDeleteMapping;
    Result := True;
  end;
end;

function TLccAliasServer.FindInNodeIDSortedMap(ANodeID: TNodeID; var MapIndex: Integer): TLccAliasMap;
var
  i: Integer;
begin
  // TODO Will make this a Binary Search eventually....
  Result := nil;
  if not EqualNodeID(NULL_NODE_ID, ANodeID, True) then
  begin
    SortMaps;

    MapIndex := -1;
    for i := 0 to NodeIDSortedMap.Count - 1 do
    begin
      if EqualNodeID(ANodeID, TLccAliasMap( NodeIDSortedMap[i]).NodeID, False) then
      begin
        Result := TLccAliasMap( NodeIDSortedMap[i]);
        MapIndex := i;
        Break;
      end;
    end;
  end;
end;

function TLccAliasServer.FindInAliasSortedMap(AnAliasID: Word; var MapIndex: Integer): TLccAliasMap;
var
  i: Integer;
begin
  // TODO Will make this a Binary Search eventually....
  Result := nil;
  if AnAliasID > 0 then
  begin
    SortMaps;

    MapIndex := -1;
    for i := 0 to AliasSortedMap.Count - 1 do
    begin
      if AnAliasID = TLccAliasMap( AliasSortedMap[i]).AliasID then
      begin
        Result := TLccAliasMap( AliasSortedMap[i]);
        MapIndex := i;
        Break;
      end;
    end;
  end;
end;

constructor TLccAliasServer.Create;
begin
  {$IFDEF DELPHI}
    FAliasSortedMap := TObjectList<TLccAliasMap>.Create(False);
    FNodeIDSortedMap := TObjectList<TLccAliasMap>.Create(True);
  {$ELSE}
    FAliasSortedMap := TObjectList.Create;
    FNodeIDSortedMap := TObjectList.Create;
    {$IFNDEF DWSCRIPT}
    NodeIDSortedMap.OwnsObjects := False;
    AliasSortedMap.OwnsObjects := True;
    {$ENDIF}
  {$ENDIF}
end;

destructor TLccAliasServer.Destroy;
begin
  FreeAndNil(FNodeIDSortedMap);  // Free first the AliasSortedMap owns the objects in non-SMS
  FreeAndNil(FAliasSortedMap);
  inherited Destroy;
end;

procedure TLccAliasServer.Clear;
begin
  NodeIDSortedMap.Clear;
  AliasSortedMap.Clear; // Owns the object in non-SMS
  FIsDirty := False;
  DoDeleteMapping;
end;

procedure TLccAliasServer.DoAddMapping;
begin
  if Assigned(OnAddMapping) then
    OnAddMapping(Self);
end;

procedure TLccAliasServer.DoDeleteMapping;
begin
  if Assigned(OnDeleteMapping) then
    OnDeleteMapping(Self);
end;

function TLccAliasServer.GetCount: Integer;
begin
  Result := AliasSortedMap.Count;
end;

procedure TLccAliasServer.RemoveMapping(AnAliasID: Word);
begin
  RemoveMappingByAliasID(AnAliasID);
end;

function TLccAliasServer.ValidateAlias(AnAlias: Word): Boolean;
var
  MapIndex: Integer;
begin
  Result := True;
  if AnAlias <> 0 then
  begin
    MapIndex := -1;
    Result := Assigned(FindInAliasSortedMap(AnAlias, MapIndex));
  end;
end;


procedure TLccAliasServer.SortMaps;
begin
  if IsDirty then
  begin
    {$IFDEF DELPHI}
    AliasSortedMap.Comparer .....
    AliasSortedMap.Sort(;
    NodeIDSortedMap.Sort;
    {$ELSE}
    AliasSortedMap.Sort({$IFNDEF DELPHI}@{$ENDIF}SortFuncAlias);
    NodeIDSortedMap.Sort({$IFNDEF DELPHI}@{$ENDIF}SortFuncNodeID);
    {$ENDIF}
    FIsDirty := False;
  end;
end;

end.

