unit lcc_protocol_datagram;


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
  {$IFDEF DELPHI}
  System.Generics.Collections,
  {$ELSE}
  contnrs,
  {$ENDIF}
  {$IFNDEF ULTIBO}
    {$IFDEF FPC}
      ExtCtrls,
    {$ELSE}
      FMX.Types,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities;


// A node may have to resend a datagram if the destination has full buffers or other
// problems.  This queue stores the datagrams a node has sent waiting to see if it is
// successful, if not the node finds and resends it from this list else it deletes it

type
  { TDatagramQueue }

TDatagramQueue = class
private
  {$IFDEF DELPHI}
  FQueue: TObjectList<TLccMessage>;
  {$ELSE}
  FQueue: TObjectList;
  {$ENDIF}
  FSendMessageFunc: TOnMessageEvent;
protected
  {$IFDEF DELPHI}
  property Queue: TObjectList<TLccMessage> read FQueue write FQueue;
  {$ELSE}
  property Queue: TObjectList read FQueue write FQueue;
  {$ENDIF}

  function FindBySourceNode(LccMessage: TLccMessage): Integer;
public
  property SendMessageFunc: TOnMessageEvent read FSendMessageFunc;

  constructor Create(ASendMessageFunc: TOnMessageEvent);
  destructor Destroy; override;
  function Add(LccMessage: TLccMessage): Boolean;
  procedure Clear;
  procedure Resend(LccMessage: TLccMessage);
  procedure Remove(LccMessage: TLccMessage);
  procedure TickTimeout;
end;


implementation

{ TDatagramQueue }

procedure TDatagramQueue.Remove(LccMessage: TLccMessage);
var
  iLocalMessage: Integer;
begin
  iLocalMessage := FindBySourceNode(LccMessage);
  if iLocalMessage > -1 then
    {$IFDEF DWSCRIPT}
    Queue.Remove(Queue.IndexOf(LccMessage));
    {$ELSE}
    Queue. Delete(iLocalMessage);
    {$ENDIF}
end;

function TDatagramQueue.Add(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  Queue.Add(LccMessage);
  LccMessage.RetryAttempts := 0;
  LccMessage.AbandonTimeout := 0;
end;

constructor TDatagramQueue.Create(ASendMessageFunc: TOnMessageEvent);
begin
  inherited Create;
  FSendMessageFunc := ASendMessageFunc;
  {$IFDEF DELPHI}
  Queue := TObjectList<TLccMessage>.Create;
  {$ELSE}
  Queue := TObjectList.Create;
  {$ENDIF}
  {$IFNDEF DWSCRIPT}
  Queue.OwnsObjects := True;
  {$ENDIF}
end;

destructor TDatagramQueue.Destroy;
begin
  {$IFDEF FPC}
  FreeAndNil(FQueue);
  {$ELSE}
    {$IFDEF DWSCRIPT}
    FQueue.Free;
    {$ELSE}
    Queue.DisposeOf;
    {$ENDIF}
  {$ENDIF}
  inherited Destroy;
end;

function TDatagramQueue.FindBySourceNode(LccMessage: TLccMessage): Integer;
var
  i: Integer;
  QueueAlias: Word;
  QueueNodeID: TNodeID;
begin
  Result := -1;
  i := 0;
  while i < Queue.Count do
  begin
    QueueAlias := (Queue[i] as TLccMessage).CAN.DestAlias;
    QueueNodeID := (Queue[i] as TLccMessage).DestID;
    if (QueueAlias <> 0) and (LccMessage.CAN.SourceAlias <> 0) then
    begin
      if QueueAlias = LccMessage.CAN.SourceAlias then
      begin
        Result := i;
        Break
      end;
    end else
    if not NullNodeID(QueueNodeID) and not NullNodeID(LccMessage.SourceID) then
    begin
      if EqualNodeID(QueueNodeID, LccMessage.SourceID, False) then
      begin
        Result := i;
        Break
      end;
    end;
    Inc(i)
  end;
end;

procedure TDatagramQueue.Clear;
begin
  Queue.Clear;
end;

procedure TDatagramQueue.Resend(LccMessage: TLccMessage);
var
  iLocalMessage: Integer;
  LocalMessage: TLccMessage;
begin
  iLocalMessage := FindBySourceNode(LccMessage);
  if iLocalMessage > -1 then
  begin
    LocalMessage := Queue[iLocalMessage] as TLccMessage;
    if LocalMessage.RetryAttempts < 5 then
    begin
      LocalMessage := Queue[iLocalMessage] as TLccMessage;
      SendMessageFunc(Self, LocalMessage);
      LocalMessage.RetryAttempts := LocalMessage.RetryAttempts + 1;
    end else
      {$IFDEF DWSCRIPT}
      Queue.Remove(Queue.IndexOf(LocalMessage));
      {$ELSE}
      Queue.Delete(iLocalMessage);
      {$ENDIF}
  end;
end;

procedure TDatagramQueue.TickTimeout;
var
  LocalMessage: TLccMessage;
  i: Integer;
begin
  for i := Queue.Count - 1 downto 0 do
  begin
    LocalMessage := Queue[i] as TLccMessage;
    if LocalMessage.AbandonTimeout < 6 then   // 800ms * 6
      LocalMessage.AbandonTimeout := LocalMessage.AbandonTimeout + 1
    else
      {$IFDEF DWSCRIPT}
      Queue.Remove(Queue.IndexOf(LocalMessage));
      {$ELSE}
      Queue.Delete(i);
      {$ENDIF}
  end;
end;

end.

