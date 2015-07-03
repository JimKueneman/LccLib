unit lcc_message_scheduler;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, lcc_messages,
  {$IFDEF FPC}
  {$ELSE}
    System.Generics.Collections,
    Types,
  {$ENDIF}
  lcc_defines,
  lcc_utilities,
  lcc_can_message_assembler_disassembler,
  lcc_threadedcirculararray;

type

  TLccSendMessageFunc = procedure(LccMessage: TLccMessage) of object;

  { TSchedulerBase }

  TOnMessageRemoveWaitingForReplyEvent = procedure(Sender: TObject; LccMessage, LccMessageReply: TLccMessage) of object;

  TSchedulerBase = class(TComponent)
  private
    FGridConnectStrings: TStringList;
    {$IFDEF FPC}
    FMessagesOutgoingList: TList;
    FMessagesPermanentErrorList: TList;
    FMessagesWaitingForReplyLIst: TList;
    {$ELSE}
    FMessagesOutgoingList: TObjectList<TLccMessage>;
    FMessagesPermanentErrorList: TObjectList<TLccMessage>;
    FMessagesWaitingForReplyLIst: TObjectList<TLccMessage>;
    {$ENDIF}
    FMsgAssembler: TLccMessageAssembler;
    FMsgDisAssembler: TLccMessageDisAssembler;
    FOwnerThread: TLccEthernetBaseThread;
    FSendMessageFunc: TLccSendMessageFunc;                                      // Points back to the owner Threads SendMessage function
    FOnRemoveOutgoingMessage: TOnMessageEvent;
    FOnAddOutgoingMessage: TOnMessageEvent;
    FOnAddWaitingForReplyMessage: TOnMessageEvent;
    FOnRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent;
    FPipelineSize: Integer;
    FWorkerMessageIncoming: TLccMessage;
    FWorkerMessageOutgoing: TLccMessage;
  protected
    property GridConnectStrings: TStringList read FGridConnectStrings write FGridConnectStrings;
    property MsgAssembler: TLccMessageAssembler read FMsgAssembler write FMsgAssembler;
    property MsgDisAssembler: TLccMessageDisAssembler read FMsgDisAssembler write FMsgDisAssembler;
    {$IFDEF FPC}
    property MessagesOutgoingList: TList read FMessagesOutgoingList write FMessagesOutgoingList;
    property MessagesPermanentErrorList: TList read FMessagesPermanentErrorList write FMessagesPermanentErrorList;
    property MessagesWaitingForReplyList: TList read FMessagesWaitingForReplyLIst write FMessagesWaitingForReplyLIst;
    {$ELSE}
    property MessagesOutgoingList: TObjectList<TLccMessage> read FMessagesOutgoingList write FMessagesOutgoingList;
    property MessagesPermanentErrorList: TObjectList<TLccMessage> read FMessagesPermanentErrorList write FMessagesPermanentErrorList;
    property MessagesWaitingForReplyList: TObjectList<TLccMessage> read FMessagesWaitingForReplyLIst write FMessagesWaitingForReplyLIst;
    {$ENDIF}
    property WorkerMessageIncoming: TLccMessage read FWorkerMessageIncoming write FWorkerMessageIncoming;
    property WorkerMessageOutgoing: TLccMessage read FWorkerMessageOutgoing write FWorkerMessageOutgoing;

    procedure CheckForNextMessageToSend;
    procedure DoMessageOutgoingAdd(LccMessage: TLccMessage);   // Push message on the stack
    procedure DoMessageOutgoingRemove(LccMessage: TLccMessage; DoFree: Boolean);    // Pop message from stack
    procedure DoMessageWaitingForReplyAdd(LccMessage: TLccMessage);  // Push message on the Waiting List
    procedure DoMessageWaitingForReplyRemove(LccMessageInWaiting, LccMessageReply: TLccMessage; DoFree: Boolean); // Message on Waiting List received a reply
    procedure DoMessagePermanentErrorAdd(LccMessage: TLccMessage);
    procedure DoMessagePermanentErrorRemove(LccMessage: TLccMessage; DoFree: Boolean);
    function ExistingSimilarMessageIsWaiting(LccMessage: TLccMessage): Boolean;
    function IsMessageWithPotentialReply(LccMessage: TLccMessage): Boolean;
    function MatchingNodeIDsAndMTIRequestReplyPair(var LccMessage: TLccMessage; ReplyLccMessage: TLccMessage; AMessageQueue: {$IFDEF FPC}TList{$ELSE}TObjectList<TLccMessage>{$ENDIF}): Boolean;
    function MatchingNodeIDsAndExternalMTIRequestReplyPair(var MessageWaitingForReply: TLccMessage; MessageReplyToCheck: TLccMessage; AMessageQueue:  {$IFDEF FPC}TList{$ELSE}TObjectList<TLccMessage>{$ENDIF}; TestMTI: Word): Boolean;
  public
    property SendMessageFunc: TLccSendMessageFunc read FSendMessageFunc write FSendMessageFunc;
    property OnAddOutgoingMessage: TOnMessageEvent read FOnAddOutgoingMessage write FOnAddOutgoingMessage;
    property OnRemoveOutgoingMessage: TOnMessageEvent read FOnRemoveOutgoingMessage write FOnRemoveOutgoingMessage;
    property OnAddWaitingForReplyMessage: TOnMessageEvent read FOnAddWaitingForReplyMessage write FOnAddWaitingForReplyMessage;
    property OnRemoveWaitingForReplyMessage: TOnMessageRemoveWaitingForReplyEvent read FOnRemoveWaitingForReplyMessage write FOnRemoveWaitingForReplyMessage;
    property OwnerThread: TLccEthernetBaseThread read FOwnerThread write FOwnerThread;
    property PipelineSize: Integer read FPipelineSize write FPipelineSize;

    constructor Create(AOwner: TComponent; SendFunc: TLccSendMessageFunc); reintroduce; virtual;
    destructor Destroy; override;
    procedure ClearQueue; virtual;
    procedure ClearPermenentErrorQueue; virtual;
    procedure ClearSentQueue; virtual;
    function IncomingMsg(LccMessage: TLccMessage): Boolean; virtual;
    function IncomingMsgGridConnectStr(GridConnectStr: string; var LccMessage: TLccMessage): Boolean; virtual;
    function IncomingMsgEthernet(var LccTcpMessage: TDynamicByteArray; var LccMessage: TLccMessage): Boolean; virtual;
    procedure OutgoingMsg(LccMessage: TLccMessage); virtual;
    procedure SendMessage(LccMessage: TLccMessage);
  end;
  TSchedulerBaseClass = class of TSchedulerBase;

  TOnSchedulerClassEvent = procedure(Sender: TObject; var SchedulerClass : TSchedulerBaseClass) of object;

 { TSchedulerPassThrough }

  TSchedulerPassThrough = class(TSchedulerBase)
  end;
  TSchedulerPassThroughClass = class of TSchedulerPassThrough;

  { TSchedulerSimplePipeline }

  TSchedulerSimplePipeline = class(TSchedulerBase)
  public
    function IncomingMsg(LccMessage: TLccMessage): Boolean; override;
    procedure OutgoingMsg(LccMessage: TLccMessage); override;
  end;


implementation

{ TSchedulerSimplePipeline }

function TSchedulerSimplePipeline.IncomingMsg(LccMessage: TLccMessage): Boolean;
var
  WaitingMessage: TLccMessage;
  LocalErrorCode, RejectedMTI: Word;

  procedure HandleErrorCode;
  begin
    if LocalErrorCode and COMMON_TEMPORARY_ERROR = COMMON_TEMPORARY_ERROR then
    begin
      if LccMessage.RetryAttempts > 5 then
      begin  // We give up, throw it away and try the next
        DoMessageWaitingForReplyRemove(WaitingMessage, LccMessage, True);
        CheckForNextMessageToSend;
      end else
      begin  // Move it to the back of the list to try later
        DoMessageWaitingForReplyRemove(WaitingMessage, LccMessage, False);
        DoMessageOutgoingAdd(WaitingMessage);
        CheckForNextMessageToSend;
      end
    end else
    begin   // Permanent error
      DoMessageWaitingForReplyRemove(WaitingMessage, LccMessage, False);
      DoMessagePermanentErrorAdd(LccMessage);
      CheckForNextMessageToSend;
    end
  end;

begin
  Result := True;
  WaitingMessage := nil;
  if LccMessage.MTI = MTI_OPTIONAL_INTERACTION_REJECTED then
  begin
    LocalErrorCode := LccMessage.ExtractDataBytesAsInt(0, 1);
    RejectedMTI := LccMessage.ExtractDataBytesAsInt(2, 3);
    if MatchingNodeIDsAndExternalMTIRequestReplyPair(WaitingMessage, LccMessage, MessagesWaitingForReplyList, RejectedMTI) then
    begin
      HandleErrorCode;
      Result := False
    end;
  end else
  if LccMessage.MTI = MTI_DATAGRAM_REJECTED_REPLY then
  begin
    LocalErrorCode := LccMessage.ExtractDataBytesAsInt(0, 1);
    if MatchingNodeIDsAndExternalMTIRequestReplyPair(WaitingMessage, LccMessage, MessagesWaitingForReplyList, MTI_DATAGRAM) then
    begin
      HandleErrorCode;
      Result := False
    end;
  end else
  if LccMessage.MTI = MTI_DATAGRAM_OK_REPLY then
  begin  // If the ConfigMem Write has a reply then need to wait for it, else throw the message away and get the next one
    if LccMessage.DataCount > 0 then
     if MatchingNodeIDsAndExternalMTIRequestReplyPair(WaitingMessage, LccMessage, MessagesWaitingForReplyList, MTI_DATAGRAM) then
       if (WaitingMessage.DataArray[0] = $20) and (WaitingMessage.DataArray[1] and $F0 = MCP_WRITE) then
         if LccMessage.DataArray[0] and DATAGRAM_OK_ACK_REPLY_PENDING <> DATAGRAM_OK_ACK_REPLY_PENDING then
         begin
           DoMessageWaitingForReplyRemove(WaitingMessage, LccMessage, True);
           CheckForNextMessageToSend;
         end;
    Result := False;
  end else
  begin

  //  if LccMessage.Mti = MTI_DATAGRAM then
  //    beep;

    if MatchingNodeIDsAndMTIRequestReplyPair(WaitingMessage, LccMessage, MessagesWaitingForReplyList) then
    begin
      DoMessageWaitingForReplyRemove(WaitingMessage, LccMessage, True);
      CheckForNextMessageToSend;
    end
  end;
end;

procedure TSchedulerSimplePipeline.OutgoingMsg(LccMessage: TLccMessage);
begin
  if IsMessageWithPotentialReply(LccMessage) then
  begin
    DoMessageOutgoingAdd(LccMessage.Clone);
    CheckForNextMessageToSend;
  end else
    SendMessage(LccMessage);  // Send it
end;


{ TSchedulerBase }

procedure TSchedulerBase.DoMessageOutgoingAdd(LccMessage: TLccMessage);
begin

   //     if LccMessage.MTI = MTI_DATAGRAM then
   // beep;

  MessagesOutgoingList.Add(LccMessage);
  if Assigned(OnAddOutgoingMessage) then
    OnAddOutgoingMessage(Self, LccMessage);
end;

procedure TSchedulerBase.DoMessageOutgoingRemove(LccMessage: TLccMessage;
  DoFree: Boolean);
begin
  MessagesOutgoingList.Remove(LccMessage);
  if Assigned(OnRemoveOutgoingMessage) then
    OnRemoveOutgoingMessage(Self, LccMessage);
  if DoFree then
    LccMessage.Free
end;

procedure TSchedulerBase.DoMessagePermanentErrorAdd(LccMessage: TLccMessage);
begin
  MessagesPermanentErrorList.Add(LccMessage);
end;

procedure TSchedulerBase.DoMessagePermanentErrorRemove(
  LccMessage: TLccMessage; DoFree: Boolean);
begin
  MessagesPermanentErrorList.Remove(LccMessage);
  if DoFree then
    LccMessage.Free
end;

procedure TSchedulerBase.DoMessageWaitingForReplyAdd(LccMessage: TLccMessage);
begin
  MessagesWaitingForReplyList.Add(LccMessage);
  if Assigned(OnAddWaitingForReplyMessage) then
    OnAddWaitingForReplyMessage(Self, LccMessage);
end;

procedure TSchedulerBase.DoMessageWaitingForReplyRemove(LccMessageInWaiting,
  LccMessageReply: TLccMessage; DoFree: Boolean);
begin
  MessagesWaitingForReplyList.Remove(LccMessageInWaiting);
  if Assigned(OnRemoveWaitingForReplyMessage) then
    OnRemoveWaitingForReplyMessage(Self, LccMessageInWaiting, LccMessageReply);
  if DoFree then
    LccMessageInWaiting.Free
end;

function TSchedulerBase.ExistingSimilarMessageIsWaiting(LccMessage: TLccMessage): Boolean;
var
  LocalMsg: TLccMessage;
  i: Integer;
begin
  Result := False;
  i := 0;
  while i < MessagesWaitingForReplyList.Count do
  begin
    {$IFDEF FPC}
    LocalMsg := TLccMessage( MessagesWaitingForReplyList[i]);
    {$ELSE}
    LocalMsg := MessagesWaitingForReplyList[i];
    {$ENDIF}
    begin
      if (EqualNodeID(LccMessage.SourceID, LocalMsg.DestID, False) or (LccMessage.CAN.SourceAlias = LocalMsg.CAN.DestAlias)) and (LocalMsg.MTI = LccMessage.MTI) then
      begin
        Result := True;
        Break
      end;
    end;
    Inc(i);
  end;
end;

function TSchedulerBase.IsMessageWithPotentialReply(LccMessage: TLccMessage): Boolean;
begin
  if (LccMessage.MTI = MTI_TRACTION_PROXY_PROTOCOL) then
    Result := not ((LccMessage.DataArray[0] = $80) and (LccMessage.DataArray[1] = $02))
  else
  if (LccMessage.MTI = MTI_TRACTION_PROTOCOL) then
    Result := not ((LccMessage.DataArray[0] = $40) and (LccMessage.DataArray[1] = $02)  or    // Manage/Release
                   (LccMessage.DataArray[0] = $00) or                             // Set Speed
                   (LccMessage.DataArray[0] = $01) or                             // Set Function
                   (LccMessage.DataArray[0] = $02) or                             // E Stop
                   ((LccMessage.DataArray[0] = $20) and (LccMessage.DataArray[1] = $02)) // Controller Release
                   )
  else
    Result := (LccMessage.MTI = MTI_PROTOCOL_SUPPORT_INQUIRY) or
              (LccMessage.MTI = MTI_REMOTE_BUTTON_REQUEST) or
              (LccMessage.MTI = MTI_SIMPLE_TRAIN_INFO_REQUEST) or     // Last frame only
        //      (LccMessage.MTI = MTI_SIMPLE_NODE_INFO_REQUEST) or         // The null counting sucks to find the last frame
              (LccMessage.MTI = MTI_VERIFY_NODE_ID_NUMBER_DEST) or
              (LccMessage.MTI = MTI_DATAGRAM) or
              (LccMessage.MTI = MTI_STREAM_INIT_REQUEST) or
              (LccMessage.MTI = MTI_STREAM_SEND);
end;

function TSchedulerBase.MatchingNodeIDsAndMTIRequestReplyPair(
  var LccMessage: TLccMessage; ReplyLccMessage: TLccMessage;
  AMessageQueue: {$IFDEF FPC}TList{$ELSE}TObjectList<TLccMessage>{$ENDIF}): Boolean;
var
  LocalMsg: TLccMessage;
  i: Integer;
begin
  Result := False;
  LccMessage := nil;
  i := 0;
  while i < AMessageQueue.Count do
  begin
    {$IFDEF FPC}
    LocalMsg := TLccMessage( AMessageQueue[i]);
    {$ELSE}
    LocalMsg := AMessageQueue[i];
    {$ENDIF}
    begin
      if EqualNodeID(ReplyLccMessage.SourceID, LocalMsg.DestID, False) or (ReplyLccMessage.CAN.SourceAlias = LocalMsg.CAN.DestAlias) then
      begin
        case LocalMsg.MTI of
          MTI_TRACTION_PROTOCOL              : Result := ReplyLccMessage.MTI = MTI_TRACTION_REPLY;
          MTI_TRACTION_PROXY_PROTOCOL        : Result := ReplyLccMessage.MTI = MTI_TRACTION_PROXY_REPLY;
          MTI_PROTOCOL_SUPPORT_INQUIRY       : Result := ReplyLccMessage.MTI = MTI_PROTOCOL_SUPPORT_REPLY;
          MTI_REMOTE_BUTTON_REQUEST          : Result := ReplyLccMessage.MTI = MTI_REMOTE_BUTTON_REPLY;
          MTI_SIMPLE_TRAIN_INFO_REQUEST      : Result := (ReplyLccMessage.MTI = MTI_SIMPLE_TRAIN_INFO_REPLY);
          MTI_SIMPLE_NODE_INFO_REQUEST       : Result := ReplyLccMessage.MTI = MTI_SIMPLE_NODE_INFO_REPLY;
          MTI_VERIFY_NODE_ID_NUMBER_DEST     : Result := ReplyLccMessage.MTI = MTI_VERIFIED_NODE_ID_NUMBER;
          MTI_DATAGRAM                       : Result := ReplyLccMessage.MTI = MTI_DATAGRAM;
          MTI_DATAGRAM_REJECTED_REPLY        : Result := ReplyLccMessage.MTI = MTI_DATAGRAM;
          MTI_STREAM_INIT_REQUEST            : Result := ReplyLccMessage.MTI = MTI_STREAM_INIT_REPLY;
          MTI_STREAM_PROCEED                 : Result := ReplyLccMessage.MTI = MTI_STREAM_SEND;
        end;
        if Result then
        begin
          LccMessage := LocalMsg;
          Break
        end;
      end;
    end;
    Inc(i);
  end;
end;

function TSchedulerBase.MatchingNodeIDsAndExternalMTIRequestReplyPair(
  var MessageWaitingForReply: TLccMessage; MessageReplyToCheck: TLccMessage;
  AMessageQueue: {$IFDEF FPC}TList{$ELSE}TObjectList<TLccMessage>{$ENDIF}; TestMTI: Word): Boolean;
//
// This only works with previously assembled messages such as SNIP, STNIP, Datagrams, Streams, etc
// Interim frames from these messages will cause early detection of the message reply and get the
// component out of sync
// Returns the message stored in the SentMessageQueue that the passed reply is matched with, else returns nil if no match
//
var
  LocalMsg: TLccMessage;
  i: Integer;
begin
  Result := False;
  MessageWaitingForReply := nil;
  i := 0;
  while i < AMessageQueue.Count do
  begin
    {$IFDEF FPC}
    LocalMsg := TLccMessage( AMessageQueue[i]);
    {$ELSE}
    LocalMsg := AMessageQueue[i];
    {$ENDIF}
    begin
      if NullNodeID(MessageReplyToCheck.SourceID) or NullNodeID(LocalMsg.DestID) then
      begin
        // NodeIDs are invalid so use the Aliases
        if (MessageReplyToCheck.CAN.SourceAlias <> 0) and (LocalMsg.CAN.DestAlias <> 0) then
        begin
          if  MessageReplyToCheck.CAN.SourceAlias = LocalMsg.CAN.DestAlias then
          begin
            Result := True;
            MessageWaitingForReply := LocalMsg;
            Break
          end;
        end
      end else
      if not NullNodeID(MessageReplyToCheck.SourceID) and not NullNodeID(LocalMsg.DestID) then
      begin
        if (EqualNodeID(MessageReplyToCheck.SourceID, LocalMsg.DestID, False) or (MessageReplyToCheck.CAN.SourceAlias = LocalMsg.CAN.DestAlias)) and (LocalMsg.MTI = TestMTI) then
        begin
          Result := True;
          MessageWaitingForReply := LocalMsg;
          Break
        end;
      end
    end;
    Inc(i);
  end;
end;

constructor TSchedulerBase.Create(AOwner: TComponent; SendFunc: TLccSendMessageFunc);
begin
  inherited Create(AOwner);
  FGridConnectStrings := TStringList.Create;
  FMsgAssembler := TLccMessageAssembler.Create;
  FMsgDisAssembler := TLccMessageDisAssembler.Create;
  {$IFDEF FPC}
    FMessagesOutgoingList := TList.Create;
    FMessagesWaitingForReplyLIst := TList.Create;
    FMessagesPermanentErrorList := TList.Create;
  {$ELSE}
    FMessagesOutgoingList := TObjectList<TLccMessage>.Create;
    FMessagesWaitingForReplyLIst := TObjectList<TLccMessage>.Create;
    FMessagesPermanentErrorList := TObjectList<TLccMessage>.Create;
    MessagesOutgoingList.OwnsObjects := False;
    MessagesWaitingForReplyLIst.OwnsObjects := False;
    MessagesPermanentErrorList.OwnsObjects := False;
  {$ENDIF}
  FWorkerMessageIncoming := TLccMessage.Create;
  FWorkerMessageOutgoing := TLccMessage.Create;
  FSendMessageFunc := SendFunc;
  PipelineSize := 1;
end;

destructor TSchedulerBase.Destroy;
begin
  ClearQueue;
  ClearPermenentErrorQueue;
  ClearSentQueue;
  FreeAndNil(FMessagesOutgoingList);
  FreeAndNil(FMessagesWaitingForReplyLIst);
  FreeAndNil(FMessagesPermanentErrorList);
  FreeAndNil(FMsgAssembler);
  FreeandNil(FMsgDisAssembler);
  FreeAndNil( FGridConnectStrings);
  FreeAndNil( FWorkerMessageIncoming);
  FreeAndNil( FWorkerMessageOutgoing);
  inherited Destroy;
end;

procedure TSchedulerBase.ClearQueue;
var
  i: Integer;
begin
  try
    for i := 0 to MessagesOutgoingList.Count - 1 do
      TObject( MessagesOutgoingList[i]).Free;
  finally
    MessagesOutgoingList.Clear;
  end;
end;

procedure TSchedulerBase.CheckForNextMessageToSend;
var
  Done: Boolean;
  iMessage: Integer;
  OutgoingMessage: TLccMessage;
begin
  if (MessagesOutgoingList.Count > 0) and (MessagesWaitingForReplyList.Count < PipelineSize) then
  begin
    Done := False;
    iMessage := 0;
    while not Done and (iMessage < MessagesOutgoingList.Count) do
    begin
      {$IFDEF FPC}
      OutgoingMessage := TLccMessage( MessagesOutgoingList[iMessage]);
      {$ELSE}
      OutgoingMessage := MessagesOutgoingList[iMessage];
      {$ENDIF}
      if not ExistingSimilarMessageIsWaiting(OutgoingMessage) then
      begin
        DoMessageOutgoingRemove(OutgoingMessage, False);
        DoMessageWaitingForReplyAdd(OutgoingMessage);
        SendMessage(OutgoingMessage);
        Done := True;
      end;
      Inc(iMessage);
    end;
  end;
end;

procedure TSchedulerBase.ClearPermenentErrorQueue;
var
  i: Integer;
begin
  try
    for i := 0 to MessagesPermanentErrorList.Count - 1 do
      TObject( MessagesPermanentErrorList[i]).Free;
  finally
    MessagesPermanentErrorList.Clear;
  end;
end;

procedure TSchedulerBase.ClearSentQueue;
var
  i: Integer;
begin
  try
    for i := 0 to MessagesWaitingForReplyList.Count - 1 do
      TObject( MessagesWaitingForReplyList[i]).Free;
  finally
    MessagesWaitingForReplyList.Clear;
  end;
end;

function TSchedulerBase.IncomingMsg(LccMessage: TLccMessage): Boolean;
begin
  Result := True
end;

function TSchedulerBase.IncomingMsgEthernet(var LccTcpMessage: TDynamicByteArray; var LccMessage: TLccMessage): Boolean;
begin
  Result := False;
  LccMessage := WorkerMessageIncoming;
  if LccMessage.LoadByLccTcp(LccTcpMessage) then
  begin
    if IncomingMsg(LccMessage) then
      Result := True
    else
      LccMessage := nil
  end else
    LccMessage := nil;
end;

function TSchedulerBase.IncomingMsgGridConnectStr(GridConnectStr: string; var LccMessage: TLccMessage): Boolean;
begin
  Result := False;
  LccMessage := WorkerMessageIncoming;
  case  MsgAssembler.IncomingMessageGridConnect(GridConnectStr, LccMessage) of
   imgcr_True :
     begin
       if IncomingMsg(LccMessage) then
         Result := True
       else
         LccMessage := nil;
     end;
   imgcr_False :
     begin
       LccMessage := nil;
     end;
   imgcr_Error :
     begin
       Result := True;
     end;
  end;
  {
  if MsgAssembler.IncomingMessageGridConnect(GridConnectStr, LccMessage) then    // LocalMessage may change if a multiframe message is found
  begin
    if IncomingMsg(LccMessage) then
      Result := True
    else
      LccMessage := nil
  end else
    LccMessage := nil}
end;

procedure TSchedulerBase.OutgoingMsg(LccMessage: TLccMessage);
begin
  SendMessage(LccMessage)
end;

procedure TSchedulerBase.SendMessage(LccMessage: TLccMessage);
var
  iString: integer;
begin
  if OwnerThread.GridConnect then
  begin
    MsgDisAssembler.OutgoingMsgToMsgList(LccMessage, GridConnectStrings);
    for iString := 0 to GridConnectStrings.Count - 1 do
    begin
      if WorkerMessageOutgoing.LoadByGridConnectStr(GridConnectStrings[iString]) then
       SendMessageFunc(WorkerMessageOutgoing)
    end;
  end else
  begin
    // No need for a Disassembler, or anything.....
    SendMessageFunc(LccMessage)
  end
end;


end.

