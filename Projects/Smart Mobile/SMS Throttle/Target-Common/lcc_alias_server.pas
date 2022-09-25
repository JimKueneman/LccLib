unit lcc_alias_server;

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
  lcc_node_messages;



//Is seems like this should all be able to be handled in the SendMessage engine.... if the Alias is needed can''t that engine get the Alias if needed?.....

type
  TLccAliasMapping = class(TObject)
  private
    FNodeAlias: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
  end;

  { TLccAliasServer }

  TLccAliasServer = class(TObject)

  private
    {$IFDEF DELPHI}
    FMappingList: TObjectList<TLccAliasMapping>;      // List of TLccAliasMapping Objects
    FInProcessMessageList: TObjectList<TLccMessage>;  // List of TLccMessages that are in waiting for a valid AliasMapping before processing
    {$ELSE}
    FMappingList: TObjectList;
    FInProcessMessageList: TObjectList;
    {$ENDIF}
    FWorkerMessage: TLccMessage;

  protected
    {$IFDEF DELPHI}
    property MappingList: TObjectList<TLccAliasMapping> read FMappingList write FMappingList;
    property InProcessMessageList: TObjectList<TLccMessage> read FInProcessMessageList write FInProcessMessageList;
    {$ELSE}
    property MappingList: TObjectList read FMappingList write FMappingList;
    property InProcessMessageList: TObjectList read FInProcessMessageList write FInProcessMessageList;
    {$ENDIF}

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

  public
    constructor Create;
    destructor Destroy; override;

    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    procedure FlushInProcessMessages;
    function AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
    function RemoveMapping(AnAlias: Word; FreeMapping: Boolean): TLccAliasMapping;
  end;

implementation

{ TLccAliasServer }

constructor TLccAliasServer.Create;
begin
  inherited Create;
  {$IFDEF DELPHI}
  FMappingList := TObjectList<TLccAliasMapping>.Create(True);
  FInProcessMessageList := TObjectList<TLccMessage>.Create(True);
  {$ELSE}
  FMappingList := TObjectList.Create(True);
  FInProcessMessageList := TObjectList.Create(True);
  {$ENDIF}
  WorkerMessage := TLccMessage.Create;
end;

destructor TLccAliasServer.Destroy;
begin
  FreeAndNil(FMappingList);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FInProcessMessageList);
  inherited Destroy;
end;

function TLccAliasServer.FindMapping(AnAliasID: Word): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  for i := 0 to MappingList.Count - 1 do
  begin
    TestMapping := MappingList.Items[i] as TLccAliasMapping;
    if TestMapping.NodeAlias = AnAliasID then
    begin
      Result := TestMapping;
      Break;
    end;
  end;
end;

function TLccAliasServer.FindMapping(ANodeID: TNodeID): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  for i := 0 to MappingList.Count - 1 do
  begin
    TestMapping := MappingList.Items[i] as TLccAliasMapping;
    if (TestMapping.NodeID[0] = ANodeID[0]) and (TestMapping.NodeID[1] = ANodeID[1]) then
    begin
      Result := TestMapping;
      Break;
    end;
  end;
end;

procedure TLccAliasServer.FlushInProcessMessages;
begin
  InProcessMessageList.Clear;
end;

function TLccAliasServer.AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
begin
  Result := FindMapping(AnAlias);
  if not Assigned(Result) then
  begin
    Result := TLccAliasMapping.Create;
    Result.NodeID := AnID;
    Result.NodeAlias := AnAlias;
    MappingList.Add(Result);
  end;
end;

function TLccAliasServer.RemoveMapping(AnAlias: Word; FreeMapping: Boolean): TLccAliasMapping;
var
  TestMapping: TLccAliasMapping;
  WasOwnsObject: Boolean;
begin
  Result := nil;
  TestMapping := FindMapping(AnAlias);
  if Assigned(TestMapping) then
  begin
    WasOwnsObject := MappingList.OwnsObjects;
    if not FreeMapping then
      MappingList.OwnsObjects := False;
    MappingList.Remove(TestMapping);
    MappingList.OwnsObjects := WasOwnsObject;
    Result := TestMapping;
  end;
end;

end.

