unit lcc_node_messages_can_assembler_disassembler;

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
  {$IFDEF FPC}
    contnrs,
  {$ELSE}
    System.Generics.Collections,
  {$ENDIF}
{$ENDIF}
{$IFDEF LCC_WINDOWS}
  {$IFNDEF FPC}
  System.Types,
  {$ENDIF}
{$ENDIF}
  lcc_node_messages,
  lcc_defines;

// TLccMessageQueue holds TLccMessages that are being received piece-meal over
// an interface such as CAN where it can't sent entire message arrays and decodes
// TCP message frames into easy and common TLccMessage structures that the application
// can use_  Pass all messages from the wire protocols through this class to receive
// a TLccMessage to use independantly of wire protocol

type

  TIncomingMessageGridConnectReply = (imgcr_False, imgcr_True, imgcr_ErrorToSend, imgcr_UnknownError);

{ TLccGridConnectMessageAssembler }

TLccGridConnectMessageAssembler = class
private
  {$IFDEF DELPHI}
  FInProcessMessageList: TObjectList<TLccMessage>;
  {$ELSE}
  FInProcessMessageList: TObjectList;
  {$ENDIF}
  FWorkerMessage: TLccMessage;
  function GetCount: Integer;
protected
  property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
public
  property Count: Integer read GetCount;
  {$IFDEF DELPHI}
  property Messages: TObjectList<TLccMessage> read FInProcessMessageList write FInProcessMessageList;
  {$ELSE}
  property Messages: TObjectList read FInProcessMessageList write FInProcessMessageList;
  {$ENDIF}

  constructor Create;
  destructor Destroy; override;

  procedure Add(AMessage: TLccMessage);
  procedure Clear;
  procedure Remove(AMessage: TLccMessage; DoFree: Boolean);
  function FindByAliasAndMTI(AMessage: TLccMessage): TLccMessage;
  procedure FlushMessagesByAlias(Alias: Word);
  function IncomingMessageGridConnect(GridConnectStr: String; LccMessage: TLccMessage): TIncomingMessageGridConnectReply; overload;
  function IncomingMessageGridConnect(LccMessage: TLccMessage): TIncomingMessageGridConnectReply; overload;
end;

{ TLccGridConnectMessageDisAssembler }

TLccGridConnectMessageDisAssembler = class
public
  function OutgoingMsgToGridConnect(Msg: TLccMessage): String;
  procedure OutgoingMsgToMsgList(Msg: TLccMessage; MsgList: TStringList);
end;

var
  AllocatedDatagrams: Integer;


implementation

{ TLccGridConnectMessageDisAssembler }

function TLccGridConnectMessageDisAssembler.OutgoingMsgToGridConnect(Msg: TLccMessage): String;
begin
  // Unsure if there is anything special to do here yet
  Result := Msg.ConvertToGridConnectStr(#10, False);
end;

procedure TLccGridConnectMessageDisAssembler.OutgoingMsgToMsgList(Msg: TLccMessage; MsgList: TStringList);
begin
  if Assigned(MsgList) then
    MsgList.Text := Msg.ConvertToGridConnectStr(#10, False);
end;

{ TLccGridConnectMessageAssembler }

function TLccGridConnectMessageAssembler.GetCount: Integer;
begin
  Result := FInProcessMessageList.Count;
end;

constructor TLccGridConnectMessageAssembler.Create;
begin
  inherited Create;
  {$IFDEF DELPHI}
   FInProcessMessageList := TObjectList<TLccMessage>.Create;
  {$ELSE}
  FInProcessMessageList := TObjectList.Create;
  {$ENDIF}

  {$IFNDEF DWSCRIPT}
  Messages.OwnsObjects := False;
  {$ENDIF}
  WorkerMessage := TLccMessage.Create;
end;

procedure TLccGridConnectMessageAssembler.Remove(AMessage: TLccMessage; DoFree: Boolean);
begin
 {$IFDEF DWSCRIPT}
  Messages.Remove(Messages.IndexOf(AMessage));
  {$ELSE}
  Messages.Remove(AMessage);
  {$ENDIF}
  if DoFree then
    AMessage.Free;
end;

destructor TLccGridConnectMessageAssembler.Destroy;
begin
  Clear;
  FInProcessMessageList.Free;
  FWorkerMessage.Free;
  inherited Destroy;
end;

procedure TLccGridConnectMessageAssembler.Add(AMessage: TLccMessage);
begin
  Messages.Add(AMessage);
end;

procedure TLccGridConnectMessageAssembler.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Messages.Count - 1 do
      Messages[i].Free
  finally
    Messages.Clear;
  end;
end;


function TLccGridConnectMessageAssembler.FindByAliasAndMTI(AMessage: TLccMessage): TLccMessage;
var
  i: Integer;
  LccMessage: TLccMessage;
begin
  Result := nil;
  for i := 0 to Messages.Count - 1 do
  begin
    LccMessage := TLccMessage(Messages[i]);
    if (AMessage.CAN.SourceAlias = LccMessage.CAN.SourceAlias) and (AMessage.CAN.DestAlias = LccMessage.CAN.DestAlias) and (AMessage.MTI = LccMessage.MTI) then
    begin
      Result := LccMessage;
      Break
    end;
  end;
end;


procedure TLccGridConnectMessageAssembler.FlushMessagesByAlias(Alias: Word);
var
  i: Integer;
  AMessage: TLccMessage;
begin
  for i := Messages.Count - 1 downto 0  do
  begin
    AMessage := TLccMessage(Messages[i]);
    if (AMessage.CAN.SourceAlias = Alias) or (AMessage.CAN.DestAlias = Alias) then
    begin
      if AMessage.MTI = MTI_DATAGRAM then
        Dec(AllocatedDatagrams);
       {$IFDEF DWSCRIPT}
        Messages.Remove(i);
        {$ELSE}
        Messages.Delete(i);
        {$ENDIF}
      AMessage.Free
    end;
  end;
end;

function TLccGridConnectMessageAssembler.IncomingMessageGridConnect(GridConnectStr: String; LccMessage: TLccMessage): TIncomingMessageGridConnectReply;
begin
  if LccMessage.LoadByGridConnectStr(GridConnectStr) then
    Result := IncomingMessageGridConnect(LccMessage)
  else
    Result := imgcr_UnknownError
end;

function TLccGridConnectMessageAssembler.IncomingMessageGridConnect(LccMessage: TLccMessage): TIncomingMessageGridConnectReply;
var
  InProcessMessage: TLccMessage;
  i: Integer;
begin                                                                           // The result of LccMessage is undefined if false is returned!
  Result := imgcr_False;
  if Assigned(LccMessage) then
  begin
    if LccMessage.IsCAN then
    begin  // CAN Only frames
      case LccMessage.CAN.MTI of
        MTI_CAN_AMR :
          begin
            // If the Alias is being reset flush all messages associated with it
            FlushMessagesByAlias(LccMessage.CAN.SourceAlias);
            Result := imgcr_True  // Pass it on
          end;
        MTI_CAN_AMD :
          begin
            FlushMessagesByAlias(LccMessage.CAN.SourceAlias);
            Result := imgcr_True  // Pass it on
          end;
        MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY :
          begin
            InProcessMessage := FindByAliasAndMTI(LccMessage);
            if Assigned(InProcessMessage) then
              Remove(InProcessMessage, True)                                         // Something is wrong, out of order.  Throw it away
            else begin
              if AllocatedDatagrams < Max_Allowed_Buffers then
              begin
                LccMessage.IsCAN := False;
                LccMessage.MTI := MTI_DATAGRAM;
                Result := imgcr_True
              end else
              begin
                LccMessage.LoadDatagramRejected(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ERROR_TEMPORARY or ERROR_BUFFER_UNAVAILABLE);
                Result := imgcr_ErrorToSend
              end;
            end;
          end;
        MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START :
          begin
            InProcessMessage := FindByAliasAndMTI(LccMessage);
            if Assigned(InProcessMessage) then
            begin
              LccMessage.LoadDatagramRejected(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ERROR_TEMPORARY or ERROR_NOT_EXPECTED or ERROR_NO_END_FRAME);
              Result := imgcr_ErrorToSend;
              Remove(InProcessMessage, True)                                         // Something is wrong, out of order.  Throw it away
            end
            else begin
              if AllocatedDatagrams < Max_Allowed_Buffers then
              begin
                InProcessMessage := TLccMessage.Create;
                InProcessMessage.MTI := MTI_DATAGRAM;
                LccMessage.CopyToTarget(InProcessMessage);
                Add(InProcessMessage);
                Inc(AllocatedDatagrams)
              end
              // We wait for the final frame before we send any error messages
            end;
          end;
        MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME :
          begin
            InProcessMessage := FindByAliasAndMTI(LccMessage);
            if Assigned(InProcessMessage) then
              InProcessMessage.AppendDataArray(LccMessage)
            // We wait for the final frame before we send any error messages
          end;
        MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END :
          begin
            InProcessMessage := FindByAliasAndMTI(LccMessage);
            if Assigned(InProcessMessage) then
            begin
              InProcessMessage.AppendDataArray(LccMessage);
              InProcessMessage.CopyToTarget(LccMessage);
              Remove(InProcessMessage, True);
              LccMessage.IsCAN := False;
              LccMessage.MTI := MTI_DATAGRAM;
              LccMessage.CAN.MTI := 0;
              Dec(AllocatedDatagrams);
              Result := imgcr_True
            end else
            begin  // Out of order but let the node handle that if needed, note this could be also if we ran out of buffers....
              LccMessage.LoadDatagramRejected(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ERROR_TEMPORARY or ERROR_NOT_EXPECTED or ERROR_NO_START_FRAME);
              Result := imgcr_ErrorToSend
            end;
          end;
        MTI_CAN_FRAME_TYPE_CAN_STREAM_SEND :
          begin

          end
      else
        Result := imgcr_True
      end
    end else
    begin
      if LccMessage.CAN.FramingBits <> $00 then                                // Is it a Multi Frame Message?
      begin
        case LccMessage.CAN.FramingBits of                                     // Train SNIP falls under this now
          $10 : begin   // First Frame
                  InProcessMessage := FindByAliasAndMTI(LccMessage);
                  if Assigned(InProcessMessage) then
                  begin
                    LccMessage.LoadOptionalInteractionRejected(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ERROR_TEMPORARY or ERROR_NOT_EXPECTED or ERROR_NO_END_FRAME, LccMessage.MTI);
                    Result := imgcr_ErrorToSend;
                    Remove(InProcessMessage, True)                              // Something is wrong, out of order.  Throw it away
                  end else
                  begin
                    InProcessMessage := TLccMessage.Create;
                    LccMessage.CopyToTarget(InProcessMessage);
                    Add(InProcessMessage);
                  end;
                end;
          $20 : begin   // Last Frame
                  InProcessMessage := FindByAliasAndMTI(LccMessage);
                  if Assigned(InProcessMessage) then
                  begin
                    InProcessMessage.AppendDataArray(LccMessage);
                    InProcessMessage.CopyToTarget(LccMessage);
                    Remove(InProcessMessage, True);
                    Result := imgcr_True
                  end else
                  begin
                    // Out of order but let the node handle that if needed (Owned Nodes Only)
                    // Don't swap the IDs, need to find the right target node first
                    LccMessage.LoadOptionalInteractionRejected(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ERROR_TEMPORARY or ERROR_NOT_EXPECTED or ERROR_NO_START_FRAME, LccMessage.MTI);
                    Result := imgcr_ErrorToSend
                  end;
                end;
          $30 : begin   // Middle Frame
                  InProcessMessage := FindByAliasAndMTI(LccMessage);
                  if Assigned(InProcessMessage) then
                    InProcessMessage.AppendDataArray(LccMessage)
                end;
        end;
      end else                                                                  // Is it a Multi Frame String Message?
      if (LccMessage.MTI = MTI_SIMPLE_NODE_INFO_REPLY) then
      begin
        InProcessMessage := FindByAliasAndMTI(LccMessage);
        if not Assigned(InProcessMessage) then
        begin
          InProcessMessage := TLccMessage.Create;
          LccMessage.CopyToTarget(InProcessMessage);
          for i := 0 to InProcessMessage.DataCount - 1 do
          begin
            if InProcessMessage.DataArray[i] = Ord(#0) then
              InProcessMessage.CAN.iTag := InProcessMessage.CAN.iTag + 1
          end;
          Add(InProcessMessage);
        end else
        begin
          if InProcessMessage.AppendDataArrayAsString(LccMessage, 6) then
          begin
            InProcessMessage.CopyToTarget(LccMessage);
            Remove(InProcessMessage, True);
            Result := imgcr_True;
          end
        end
      end else
        Result := imgcr_True;                                        // Single frame just create a message
    end
  end
end;

initialization
  AllocatedDatagrams := 0;

end.

