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
    FDelayedMessageList: TObjectList<TLccMessage>;  // List of TLccMessages that are in waiting for a valid AliasMapping before processing
    {$ELSE}
    FMappingList: TObjectList;
    FDelayedMessageList: TObjectList;
    {$ENDIF}
    FWorkerMessage: TLccMessage;

  protected
    {$IFDEF DELPHI}
    property MappingList: TObjectList<TLccAliasMapping> read FMappingList write FMappingList;
    property DelayedMessageList: TObjectList<TLccMessage> read FDelayedMessageList write FDelayedMessageList;
    {$ELSE}
    property MappingList: TObjectList read FMappingList write FMappingList;
    property DelayedMessageList: TObjectList read FDelayedMessageList write FDelayedMessageList;
    {$ENDIF}

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

  public
    constructor Create;
    destructor Destroy; override;

    function HasDelayedMessageByAlias(AnAliasID: Word): Boolean;
    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    procedure FlushDelayedMessages;
    procedure FlushDelayedMessagesByAlias(AnAliasID: Word);
    function AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
    function AddDelayedMessage(ADelayedMessage: TLccMessage): TLccMessage;
    function PopDelayedMessageByAlias(AnAliasID: Word): TLccMessage;
    function RemoveMappingByAlias(AnAlias: Word; FreeMapping: Boolean): TLccAliasMapping;
  end;

implementation

{ TLccAliasServer }

constructor TLccAliasServer.Create;
begin
  inherited Create;
  {$IFDEF DELPHI}
  FMappingList := TObjectList<TLccAliasMapping>.Create(True);
  FDelayedMessageList := TObjectList<TLccMessage>.Create(False);
  {$ELSE}
  FMappingList := TObjectList.Create(True);
  FDelayedMessageList := TObjectList.Create(False);
  {$ENDIF}
  WorkerMessage := TLccMessage.Create;
end;

destructor TLccAliasServer.Destroy;
begin
  FreeAndNil(FMappingList);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FDelayedMessageList);
  inherited Destroy;
end;

function TLccAliasServer.HasDelayedMessageByAlias(AnAliasID: Word): Boolean;
var
  DelayedMessage: TLccMessage;
  i: Integer;
begin
  Result := False;
  // Count backwards so we may not have to move as much memory as they are extracted
  for i := 0 to DelayedMessageList.Count - 1 do
  begin
    DelayedMessage := DelayedMessageList[i] as TLccMessage;
    if DelayedMessage.CAN.SourceAlias = AnAliasID then
    begin
      Result := True;
      Break
    end;
  end;
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

procedure TLccAliasServer.FlushDelayedMessages;
begin
  DelayedMessageList.Clear;
end;

procedure TLccAliasServer.FlushDelayedMessagesByAlias(AnAliasID: Word);
var
  DelayedMessage: TLccMessage;
  i: Integer;
begin
  // Count backwards so we may not have to move as much memory as they are extracted
  // Flush them all associated with this Alias
  for i := DelayedMessageList.Count - 1 downto 0 do
  begin
    DelayedMessage := DelayedMessageList[i] as TLccMessage;
    if DelayedMessage.CAN.SourceAlias = AnAliasID then
    begin
      DelayedMessageList.Delete(i);
      DelayedMessage.Free;
    end;
  end;
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

function TLccAliasServer.AddDelayedMessage(ADelayedMessage: TLccMessage): TLccMessage;
begin
  Result := ADelayedMessage.Clone;
  DelayedMessageList.Add(Result);
end;

function TLccAliasServer.PopDelayedMessageByAlias(AnAliasID: Word): TLccMessage;
var
  i: Integer;
  TestMessage: TLccMessage;
begin
  // First in is First out
  Result := nil;
  for i := 0 to DelayedMessageList.Count - 1 do
  begin
    TestMessage := DelayedMessageList[i] as TLccMessage;
    if TestMessage.CAN.SourceAlias = AnAliasID then
    begin
      Result := TestMessage;
      DelayedMessageList.Delete(i);
      Break;
    end;
  end;
end;

function TLccAliasServer.RemoveMappingByAlias(AnAlias: Word;
  FreeMapping: Boolean): TLccAliasMapping;
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

