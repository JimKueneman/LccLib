unit lcc_protocol_events;


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
  contnrs,
{$ENDIF}
  lcc_protocol_base,
  lcc_defines,
  lcc_node_messages,
  lcc_utilities;

type

{ TLccEvent }

TLccEvent = class(TObject)
private
  FID: TEventID;
  FIDStr: string;
  FState: TEventState;
  procedure SetID(AValue: TEventID);
  procedure SetIDStr(AValue: string);
public
  property ID: TEventID read FID write SetID;
  property IDStr: string read FIDStr write SetIDStr;
  property State: TEventState read FState write FState;
end;

{ TLccEventAutoGenerate }

TLccEventAutoGenerate = class(TObject)
private
  FCount: Integer;
  FDefaultState: TEventState;
  FStartIndex: Integer;
  procedure SetCount(AValue: Integer);
  procedure SetDefaultState(AValue: TEventState);
  procedure SetStartIndex(AValue: Integer);
public
  property Count: Integer read FCount write SetCount;
  property DefaultState: TEventState read FDefaultState write SetDefaultState;
  property StartIndex: Integer read FStartIndex write SetStartIndex;
end;

{ TProtocolEvents }

TProtocolEvents = class(TNodeProtocolBase)
private
  FAutoGenerate: TLccEventAutoGenerate;
  FEventList: TObjectList;
  function GetCount: Integer;
  function GetEvent(Index: Integer): TLccEvent;
  function GetEventIDAsStr(Index: Integer): String;
protected
  property EventList: TObjectList read FEventList write FEventList;
public
  constructor Create(ASendMessageFunc: TLccSendMessageFunc); override;
  destructor Destroy; override;

  procedure Add(Event: TEventID; State: TEventState);
  procedure Clear;
  function Supports(Event: TEventID): TLccEvent;
  function ProcessMessage(LccMessage: TLccMessage): Boolean; override;

  property AutoGenerate: TLccEventAutoGenerate read FAutoGenerate write FAutoGenerate;
  property Count: Integer read GetCount;
  property Event[Index: Integer]: TLccEvent read GetEvent; default;
  property EventIDAsStr[Index: Integer]: String read GetEventIDAsStr;
end;

implementation

{ TLccEvent }

procedure TLccEvent.SetID(AValue: TEventID);
begin
  FID[0] := AValue[0];
  FID[1] := AValue[1];
  FID[2] := AValue[2];
  FID[3] := AValue[3];
  FID[4] := AValue[4];
  FID[5] := AValue[5];
  FID[6] := AValue[6];
  FID[7] := AValue[7];
  FIDStr := EventIDToString(FID, False);
end;

procedure TLccEvent.SetIDStr(AValue: string);
begin
  if FIDStr = AValue then Exit;
  FIDStr := AValue;
  FID := StrToEventID(AValue)
end;

{ TLccEventAutoGenerate }

procedure TLccEventAutoGenerate.SetCount(AValue: Integer);
begin
  if FCount = AValue then Exit;
  FCount := AValue;
end;

procedure TLccEventAutoGenerate.SetDefaultState(AValue: TEventState);
begin
  if FDefaultState = AValue then Exit;
  FDefaultState := AValue;
end;

procedure TLccEventAutoGenerate.SetStartIndex(AValue: Integer);
begin
  if FStartIndex = AValue then Exit;
  FStartIndex := AValue;
end;

{ TProtocolEvents }

constructor TProtocolEvents.Create(ASendMessageFunc: TLccSendMessageFunc);
begin
  inherited Create(ASendMessageFunc);
  FEventList := TObjectList.Create;
  FAutoGenerate := TLccEventAutoGenerate.Create;
  {$IFNDEF DWSCRIPT}EventList.OwnsObjects := False;{$ENDIF}
end;

destructor TProtocolEvents.Destroy;
begin
  Clear;
  FEventList.Free;
  FAutoGenerate.Free;
  inherited Destroy;
end;

function TProtocolEvents.GetEvent(Index: Integer): TLccEvent;
begin
  Result := TLccEvent( EventList[Index])
end;

function TProtocolEvents.GetEventIDAsStr(Index: Integer): String;
begin
  Result := EventIDToString(Event[Index].ID, True);
end;

function TProtocolEvents.GetCount: Integer;
begin
  Result := EventList.Count;
end;

function TProtocolEvents.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := False;
end;

procedure TProtocolEvents.Add(Event: TEventID; State: TEventState);
var
  LccEvent: TLccEvent;
begin
  LccEvent := Supports(Event);
  if Assigned(LccEvent) then
    LccEvent.State := State
  else begin
    LccEvent := TLccEvent.Create;
    LccEvent.ID := Event;
    LccEvent.State := State;
    EventList.Add(LccEvent);
  end;
end;

procedure TProtocolEvents.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to EventList.Count - 1 do
    {$IFDEF FPC}
      TObject( EventList[i]).Free;
    {$ELSE}
      EventList[i].Free;
    {$ENDIF}
  finally
    EventList.Clear
  end;
end;

function TProtocolEvents.Supports(Event: TEventID): TLccEvent;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while not Assigned(Result) and (i < EventList.Count) do
  begin
    if EqualEventID(Event, TLccEvent( EventList[i]).ID) then
      Result := TLccEvent( EventList[i]);
    Inc(i);
  end;

end;

end.

