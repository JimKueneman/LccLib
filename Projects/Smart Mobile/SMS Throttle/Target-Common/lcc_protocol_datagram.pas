unit lcc_protocol_datagram;

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
  ExtCtrls,
{$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities;

type
  { TDatagramQueue }

TDatagramQueue = class
private
  FQueue: TObjectList;
  FSendMessageFunc: TLccSendMessageFunc;
protected
  property Queue: TObjectList read FQueue write FQueue;

  function FindBySourceNode(LccMessage: TLccMessage): Integer;
public
  property SendMessageFunc: TLccSendMessageFunc read FSendMessageFunc;

  constructor Create(ASendMessageFunc: TLccSendMessageFunc);
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

constructor TDatagramQueue.Create(ASendMessageFunc: TLccSendMessageFunc);
begin
  inherited Create;
  FSendMessageFunc := ASendMessageFunc;
  Queue := TObjectList.Create;
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
  //    Assert(SendMessageFunc = nil, 'SendMessge function not assigned');
      SendMessageFunc(LocalMessage);
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
    if LocalMessage.AbandonTimeout < 6 then   // 800ms + 6
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

