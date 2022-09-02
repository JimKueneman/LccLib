unit lcc_alias_mappings;

interface

{$IFNDEF DWSCRIPT}
{$I lcc_compilers.inc}
{$ENDIF}

{.$DEFINE DISABLE_STATE_TIMEOUTS_FOR_DEBUG}

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
  lcc_math_float16,
  lcc_utilities;

type
  TLccAliasMapping = class(TObject)
  public
    AnAlias: Word;
    AnID: TNodeID;
  end;

  { TLccAliasMappingList }

  TLccAliasMappingList = class(TObject)

  private
    {$IFDEF DELPHI}
    FMappingList: TObjectList<TLccAliasMapping>;
    {$ELSE}
    FMappingList: TObjectList;
    {$ENDIF}

  protected
    {$IFDEF DELPHI}
    property MappingList: TObjectList<TLccAliasMapping> read FMappingList write FMappingList;
    {$ELSE}
    property MappingList: TObjectList read FMappingList write FMappingList;
    {$ENDIF}

  public
    constructor Create;
    destructor Destroy; override;

    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    function AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
    procedure RemoveMapping(AnAlias: Word);
  end;

implementation

{ TLccAliasMappingList }

constructor TLccAliasMappingList.Create;
begin
  inherited Create;
  {$IFDEF DELPHI}
  FMappingList := TObjectList<TLccAliasMapping>.Create(True);
  {$ELSE}
  FMappingList := TObjectList.Create(True);
  {$ENDIF}
end;

destructor TLccAliasMappingList.Destroy;
begin
  FreeAndNil(FMappingList);
  inherited Destroy;
end;

function TLccAliasMappingList.FindMapping(AnAliasID: Word): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  for i := 0 to MappingList.Count - 1 do
  begin
    TestMapping := MappingList.Items[i] as TLccAliasMapping;
    if TestMapping.AnAlias = AnAliasID then
    begin
      Result := TestMapping;
      Break;
    end;
  end;
end;

function TLccAliasMappingList.FindMapping(ANodeID: TNodeID): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  for i := 0 to MappingList.Count - 1 do
  begin
    TestMapping := MappingList.Items[i] as TLccAliasMapping;
    if (TestMapping.AnID[0] = ANodeID[0]) and (TestMapping.AnID[1] = ANodeID[1]) then
    begin
      Result := TestMapping;
      Break;
    end;
  end;
end;

function TLccAliasMappingList.AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
begin
  Result := FindMapping(AnAlias);
  if not Assigned(Result) then
  begin
    Result := TLccAliasMapping.Create;
    Result.AnID := AnID;
    Result.AnAlias := AnAlias;
    MappingList.Add(Result);
  end;
end;

procedure TLccAliasMappingList.RemoveMapping(AnAlias: Word);
var
  TestMapping: TLccAliasMapping;
begin
  TestMapping := FindMapping(AnAlias);
  if Assigned(TestMapping) then
    MappingList.Remove(TestMapping);
end;

end.

