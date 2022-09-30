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
  lcc_utilities,
  lcc_node_messages;

type
  TLccAliasMapping = class(TObject)
  private
    FNodeAlias: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
  end;


  TLccAliasMappingRec = record
    Alias: Word;
    ID: TNodeID;
  end;

  TLccAliasMappingRecArray = array of TLccAliasMappingRec;

  { TLccDelayedMessage }

  TLccDelayedMessage = class(TObject)
  private
    FLccMessage: TLccMessage;
  public
    ExtraMappings: TLccAliasMappingRecArray;
    property LccMessage: TLccMessage read FLccMessage write FLccMessage;

    constructor Create(AMessage: TLccMessage);
    destructor Destroy; override;
    procedure AddExtraMapping(NodeID: TNodeID; NodeAlias: Word);

  //  Maybe I just queue up all messages in here and run them all through?  Does that simplify the code in the Node at all?
  end;

  { TLccAliasServer }

  TLccAliasServer = class(TObject)

  private
    {$IFDEF DELPHI}
    FMappingList: TObjectList<TLccAliasMapping>;      // List of TLccAliasMapping Objects
    FDelayedMessageList: TObjectList<TLccDelayedMessage>;  // List of TLccMessages that are in waiting for a valid AliasMapping before processing
    {$ELSE}
    FMappingList: TObjectList;
    FDelayedMessageList: TObjectList;
    {$ENDIF}
    FWorkerMessage: TLccMessage;

  protected
    {$IFDEF DELPHI}
    property MappingList: TObjectList<TLccAliasMapping> read FMappingList write FMappingList;
    property DelayedMessageList: TObjectList<TLccDelayedMessage> read FDelayedMessageList write FDelayedMessageList;
    {$ELSE}
    property MappingList: TObjectList read FMappingList write FMappingList;
    property DelayedMessageList: TObjectList read FDelayedMessageList write FDelayedMessageList;
    {$ENDIF}

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

  public
    constructor Create;
    destructor Destroy; override;

    function DelayedMessageMappingComplete(ADelayedMessage: TLccDelayedMessage): Boolean;
    function AddDelayedMessage(ADelayedMessage: TLccMessage): TLccDelayedMessage;
    function UpdateDelayedMessageMapping(NewMapping: TLccAliasMapping): TLccDelayedMessage;
    procedure FlushDelayedMessages;
    procedure FlushDelayedMessagesByAlias(AnAliasID: Word);

    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    function AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
    function RemoveMappingByAlias(AnAlias: Word; FreeMapping: Boolean): TLccAliasMapping;
  end;

implementation

{ TLccDelayedMessage }

constructor TLccDelayedMessage.Create(AMessage: TLccMessage);
begin
  inherited Create;
  FLccMessage := AMessage.Clone;
end;

destructor TLccDelayedMessage.Destroy;
begin
  if Assigned(LccMessage) then
    LccMessage.Free;
  inherited Destroy;
end;

procedure TLccDelayedMessage.AddExtraMapping(NodeID: TNodeID; NodeAlias: Word);
begin
  if (NodeAlias <> 0) or (NodeID[0] <> 0) or (NodeID[1] <> 0) then
  begin
    SetLength(ExtraMappings, Length(ExtraMappings) + 1);
    ExtraMappings[Length(ExtraMappings) - 1].Alias := NodeAlias;
    ExtraMappings[Length(ExtraMappings) - 1].ID := NodeID;
  end;
end;

{ TLccAliasServer }

constructor TLccAliasServer.Create;
begin
  inherited Create;
  {$IFDEF DELPHI}
  FMappingList := TObjectList<TLccAliasMapping>.Create(True);
  FDelayedMessageList := TObjectList<TLccDelayedMessage>.Create(False);
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

function TLccAliasServer.DelayedMessageMappingComplete(ADelayedMessage: TLccDelayedMessage): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Length(ADelayedMessage.ExtraMappings) - 1 do
  begin
    if (ADelayedMessage.ExtraMappings[i].Alias = 0) and
       ((ADelayedMessage.ExtraMappings[i].ID[0] = 0) or (ADelayedMessage.ExtraMappings[i].ID[1] = 0)) then
      Result := False;
  end;
  if Result then
    Result := (ADelayedMessage.LccMessage.CAN.SourceAlias <> 0) and ((ADelayedMessage.LccMessage.SourceID[0] <> 0) or (ADelayedMessage.LccMessage.SourceID[1] <> 0));
  // If there is a destination ID need to test it as well.
  if Result and ADelayedMessage.LccMessage.HasDestination then
    Result := (ADelayedMessage.LccMessage.CAN.DestAlias <> 0) and ((ADelayedMessage.LccMessage.DestID[0] <> 0) or (ADelayedMessage.LccMessage.DestID[1] <> 0));
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
  DelayedMessage: TLccDelayedMessage;
  i, j: Integer;
  DoDelete: Boolean;
begin
  // Count backwards so we may not have to move as much memory as they are extracted
  // Flush them all associated with this Alias
  for i := DelayedMessageList.Count - 1 downto 0 do
  begin
    DoDelete := False;
    DelayedMessage := DelayedMessageList[i] as TLccDelayedMessage;

    // Check the source alias
    if DelayedMessage.LccMessage.CAN.SourceAlias = AnAliasID then
      DoDelete := True;

    // Check the dest alias (if needed)
    if DelayedMessage.LccMessage.HasDestination then
    begin
      if DelayedMessage.LccMessage.CAN.DestAlias = AnAliasID then
        DoDelete := True;
    end;

    // Check extra mappings
     for j := 0 to Length(DelayedMessage.ExtraMappings) - 1 do
    begin
      if DelayedMessage.ExtraMappings[j].Alias = AnAliasID then
        DoDelete := True;
    end;

    if DoDelete then
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

function TLccAliasServer.AddDelayedMessage(ADelayedMessage: TLccMessage): TLccDelayedMessage;
begin
  Result := TLccDelayedMessage.Create(ADelayedMessage);
  DelayedMessageList.Add(Result);
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

function TLccAliasServer.UpdateDelayedMessageMapping(NewMapping: TLccAliasMapping): TLccDelayedMessage;
var
  i, j: Integer;
  DelayedMessage: TLccDelayedMessage;
begin
  Result := nil;
  for i := 0 to DelayedMessageList.Count - 1 do
  begin
    DelayedMessage := DelayedMessageList[i] as TLccDelayedMessage;

    // Update the Source
    if (DelayedMessage.LccMessage.CAN.SourceAlias = NewMapping.NodeAlias) or
       EqualNodeID(DelayedMessage.LccMessage.SourceID, NewMapping.NodeID, False) then
    begin
      DelayedMessage.LccMessage.CAN.SourceAlias := NewMapping.NodeAlias;
      DelayedMessage.LccMessage.SourceID := NewMapping.NodeID;
    end;

    // Update the Dest (if required)
    if DelayedMessage.LccMessage.HasDestination then
    begin
      if (DelayedMessage.LccMessage.CAN.DestAlias = NewMapping.NodeAlias) or
         EqualNodeID(DelayedMessage.LccMessage.DestID, NewMapping.NodeID, False) then
      begin
        DelayedMessage.LccMessage.CAN.DestAlias := NewMapping.NodeAlias;
        DelayedMessage.LccMessage.DestID := NewMapping.NodeID;
      end
    end;

    // Update Extra Mapping if necessary
    for j := 0 to Length(DelayedMessage.ExtraMappings) - 1 do
    begin
      if (DelayedMessage.ExtraMappings[j].Alias = NewMapping.NodeAlias) or
          EqualNodeID(DelayedMessage.ExtraMappings[j].ID, NewMapping.NodeID, False) then
      begin
        DelayedMessage.ExtraMappings[j].ID := NewMapping.NodeID;
        DelayedMessage.ExtraMappings[j].Alias := NewMapping.NodeAlias;
      end;
    end;
  end;

  if DelayedMessageList.Count > 0 then
  begin
    if DelayedMessageMappingComplete(DelayedMessageList[0] as TLccDelayedMessage) then
    begin
      Result := DelayedMessageList[0] as TLccDelayedMessage;
      DelayedMessageList.Delete(0);
    end;
  end;
end;

end.

