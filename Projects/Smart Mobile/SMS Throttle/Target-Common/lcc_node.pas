unit lcc_node;

{$mode objfpc}{$H+}

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
  ExtCtrls,
{$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
//  lcc_protocol_traction,
//  lcc_protocol_traction_simpletrainnodeinfo,
//  lcc_protocol_traction_configuruation_functiondefinitioninfo,
//  lcc_protocol_traction_configuration_functions,
  lcc_protocol_configuration_configuration,
  lcc_protocol_configuration_configurationdefinitioninfo,
  lcc_protocol_configuration_options,
  lcc_protocol_configuration_information,
  lcc_protocol_simplenodeinfo,
  lcc_protocol_acdi,
  lcc_protocol_events,
  lcc_protocol_supportedprotocols,
  lcc_protocol_datagram,
  lcc_protocol_base;

const
  ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH = $0001;

type

(*

{ TConfigurationMemory }

TConfigurationMemory = class(TNodeProtocolBase)
private
  FAddress: DWord;
  FAddressSpace: Byte;
  FDataCount: Integer;
  FDataRaw: TDatagramArray;
  FDataType: TLccConfigDataType;
  FDataTypeBit: Byte;
  FDataTypeEvent: TEventID;
  FDataTypeInteger: Integer;
  FDataTypeString: String;
  FInProcessAddress: DWord;
  function GetDataRawIndexer(iIndex: Word): Byte;

  procedure SetDataRawIndexer(iIndex: Word; const Value: Byte);
protected
  property InProcessAddress: DWord read FInProcessAddress write FInProcessAddress;
public
  property Address: DWord read FAddress write FAddress;
  property AddressSpace: Byte read FAddressSpace write FAddressSpace;
  property DataCount: Integer read FDataCount write FDataCount;
  property DataRaw: TDatagramArray read FDataRaw write FDataRaw;
  property DataRawIndexer[iIndex: Word]: Byte read GetDataRawIndexer write SetDataRawIndexer;
  property DataType: TLccConfigDataType read FDataType write FDataType;
  property DataTypeInteger: Integer read FDataTypeInteger;
  property DataTypeEvent: TEventID read FDataTypeEvent;
  property DataTypeBit: Byte read FDataTypeBit;
  property DataTypeString: String read FDataTypeString;
  procedure Initialize(AnAddress: DWord; AnAddressSpace: Byte; DataSize: Integer; ADataType: TLccConfigDataType);
  function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
end;

*)

{ TLccNode }

TLccNode = class(TNodeProtocolBase)
private
  FInitialized: Boolean;
  FTProtocolConfigurationDefinitionInfo: TProtocolConfigurationDefinitionInfo;
  FProtocolConfigurationOptions: TProtocolConfigurationOptions;
  FProtocolEventConsumed: TProtocolEvents;
  FProtocolEventsProduced: TProtocolEvents;
  FProtocolSupportedProtocols: TProtocolSupportedProtocols;
  FProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo;
  FProtocolConfigurationInfo: TProtocolConfiguratinInfo;
  FACDIMfg: TACDIMfg;
  FACDIUser: TACDIUser;
  FNodeID: TNodeID;
  FConfiguration: TConfiguration;
  FDatagramQueue: TDatagramQueue;
  {$IFNDEF FPC_CONSOLE_APP}
  F_800msTimer: TTimer;
  {$ELSE}
  F_800msTimer: TFPTimer;
  {$ENDIF}

  function GetNodeIDStr: String;
protected
  property DatagramQueue: TDatagramQueue read FDatagramQueue write FDatagramQueue;
  {$IFNDEF FPC_CONSOLE_APP}
  property _800msTimer: TTimer read F_800msTimer write F_800msTimer;
  {$ELSE}
  property _800msTimer: TFPTimer read F_800msTimer write F_800msTimer;
  {$ENDIF}

  procedure AutoGenerateEvents;
  procedure SendAckReply(LccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
  procedure On_800msTimer(Sender: TObject);  virtual;
public
  property NodeID: TNodeID read FNodeID;
  property NodeIDStr: String read GetNodeIDStr;
  property Initialized: Boolean read FInitialized;

  property ACDIMfg: TACDIMfg read FACDIMfg write FACDIMfg;
  property ACDIUser: TACDIUser read FACDIUser write FACDIUser;
  property Configuration: TConfiguration read FConfiguration write FConfiguration;
  property ProtocolConfigurationDefinitionInfo: TProtocolConfigurationDefinitionInfo read FTProtocolConfigurationDefinitionInfo write FTProtocolConfigurationDefinitionInfo;
  property ProtocolConfigurationOptions: TProtocolConfigurationOptions read FProtocolConfigurationOptions write FProtocolConfigurationOptions;
  property ProtocolConfigurationInfo: TProtocolConfiguratinInfo read FProtocolConfigurationInfo write FProtocolConfigurationInfo;
  property ProtocolEventConsumed: TProtocolEvents read FProtocolEventConsumed write FProtocolEventConsumed;
  property ProtocolEventsProduced: TProtocolEvents read FProtocolEventsProduced write FProtocolEventsProduced;
  property ProtocolSupportedProtocols: TProtocolSupportedProtocols read FProtocolSupportedProtocols write FProtocolSupportedProtocols;
  property ProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo read FProtocolSimpleNodeInfo write FProtocolSimpleNodeInfo;

  constructor Create(ASendMessageFunc: TLccSendMessageFunc); override;
  destructor Destroy; override;
  function IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean; virtual;
  procedure Login(ANodeID: TNodeID); virtual;
  procedure Logout; virtual;
  function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  procedure SendEvents;
  procedure SendConsumedEvents;
  procedure SendConsumerIdentify(var Event: TEventID);
  procedure SendProducedEvents;
  procedure SendProducerIdentify(var Event: TEventID);
  procedure SendInitializeComplete;
end;

{ TLccCanNode }

TLccCanNode = class(TLccNode)
private
  FAliasID: Word;
  FDuplicateAliasDetected: Boolean;
  FSeedNodeID: TNodeID;
  FPermitted: Boolean;

  function GetAliasIDStr: String;
protected
  property DuplicateAliasDetected: Boolean read FDuplicateAliasDetected write FDuplicateAliasDetected;
  property SeedNodeID: TNodeID read FSeedNodeID write FSeedNodeID;

//  function CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
  procedure CreateNodeID(var Seed: TNodeID);
  function GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
  procedure GenerateNewSeed(var Seed: TNodeID);
  procedure On_800msTimer(Sender: TObject); override;
  procedure Relogin;
  procedure SendAMD;
  procedure SendAMR;
public
   constructor Create(ASendMessageFunc: TLccSendMessageFunc); override;
   destructor Destroy; override;

   property AliasID: Word read FAliasID;
   property AliasIDStr: String read GetAliasIDStr;
   property Permitted: Boolean read FPermitted;

   function IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean; override;
   procedure Login(ANodeID: TNodeID); override;
   procedure Logout; override;
   function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
end;


(*
{ TLccOwnedNode }

TLccOwnedNode = class(TLccNode)
private
  FInitialized: Boolean;

protected

public
  property Initialized: Boolean read FInitialized;


  constructor Create(ASendMessageFunc: TLccSendMessageFunc); override;
  destructor Destroy; override;
  procedure Login(NewNodeID, RegenerateAliasSeed: Boolean);
//JDK   procedure LoginWithLccSettings(RegenerateAliasSeed: Boolean);
  procedure LoginWithNodeID(ANodeID: TNodeId; RegenerateAliasSeed: Boolean);
  procedure LogOut;

  function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
end;

{ TLccDefaultRootNode }

TLccDefaultRootNode = class(TLccOwnedNode)
public
  constructor Create(ASendMessageFunc: TLccSendMessageFunc); override;
end;
TLccDefaultRootNodeClass = class of TLccDefaultRootNode;
 *)

implementation

{ TLccCanNode }

constructor TLccCanNode.Create(ASendMessageFunc: TLccSendMessageFunc);
begin
  inherited Create(ASendMessageFunc);
end;

destructor TLccCanNode.Destroy;
begin
  if Permitted then
  begin
    WorkerMessage.LoadAMR(NodeID, AliasID);
    SendMessageFunc(WorkerMessage);
  end;
  inherited Destroy;
end;

function TLccCanNode.GetAliasIDStr: String;
begin
   Result := '0x' + IntToHex(FAliasID, 4);
end;

function TLccCanNode.IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
begin
   Result := False;
   if TestType = ntt_Dest then
   begin
     if LccMessage.HasDestNodeID and not NullNodeID(NodeID) then
       Result := ((NodeID[0] = LccMessage.DestID[0]) and (NodeID[1] = LccMessage.DestID[1])) or (AliasID = LccMessage.CAN.DestAlias)
     else
     if (AliasID <> 0) and (LccMessage.CAN.DestAlias <> 0) then
       Result := AliasID = LccMessage.CAN.DestAlias
   end else
   if TestType = ntt_Source then
   begin
     if LccMessage.HasSourceNodeID and not NullNodeID(NodeID) then
       Result := ((NodeID[0] = LccMessage.SourceID[0]) and (NodeID[1] = LccMessage.SourceID[1])) or (AliasID = LccMessage.CAN.SourceAlias)
     else
     if (AliasID <> 0) and (LccMessage.CAN.SourceAlias <> 0) then
       Result := AliasID = LccMessage.CAN.SourceAlias
   end;
end;

procedure TLccCanNode.Login(ANodeID: TNodeID);
begin
  //
  if NullNodeID(ANodeID) then
    CreateNodeID(ANodeID);
  SeedNodeID := ANodeID;
  FAliasID := GenerateID_Alias_From_Seed(FSeedNodeID);
  FNodeID := ANodeID;

  Assert(SendMessageFunc = nil, 'SentMessageFunc is nil');
  WorkerMessage.LoadCID(NodeID, AliasID, 0);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 1);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 2);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 3);
  SendMessageFunc(WorkerMessage);

  _800msTimer.Enabled := True;  //  Next state is in the event handler to see if anyone objects tor our Alias
end;

procedure TLccCanNode.Logout;
begin
  SendAMR;
  FPermitted := False;
  inherited Logout;
end;

procedure TLccCanNode.On_800msTimer(Sender: TObject);
begin
  Assert(SendMessageFunc = nil, 'SentMessageFunc is nil');

  if not Permitted then
  begin
     // Did any node object to this Alias through ProcessMessage?
    if DuplicateAliasDetected then
    begin
      GenerateNewSeed(FSeedNodeID);
      FAliasID := GenerateID_Alias_From_Seed(FSeedNodeID);
      WorkerMessage.LoadCID(NodeID, AliasID, 0);
      SendMessageFunc(WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 1);
      SendMessageFunc(WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 2);
      SendMessageFunc(WorkerMessage);
      WorkerMessage.LoadCID(NodeID, AliasID, 3);
      SendMessageFunc(WorkerMessage);
    end else
    begin
      FPermitted := True;
      WorkerMessage.LoadRID(AliasID);
      SendMessageFunc(WorkerMessage);
      WorkerMessage.LoadAMD(NodeID, AliasID);
      SendMessageFunc(WorkerMessage);
    end
  end;
  if Permitted then
    inherited On_800msTimer(Sender);
end;

function TLccCanNode.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
begin
  Result := False;

   Assert(SendMessageFunc = nil, 'SentMessageFunc is nil');

  if (AliasID <> 0) and (LccMessage.CAN.SourceAlias = AliasID) then
  begin
    if ((LccMessage.CAN.MTI and $0F000000) >= MTI_CAN_CID6) and ((LccMessage.CAN.MTI and $0F000000) <= MTI_CAN_CID0) then
    begin
      WorkerMessage.LoadRID(AliasID);                   // sorry charlie this is mine
      SendMessageFunc(WorkerMessage);
      Result := True;
    end else
    if Permitted then
    begin
      Logout;
      Relogin;
    end
  end;

  if not Permitted then
  begin
    if LccMessage.CAN.SourceAlias = AliasID then
      DuplicateAliasDetected := True;
  end else
  begin
    TestNodeID[0] := 0;
    TestNodeID[1] := 0;
    if LccMessage.IsCAN then
    begin
      case LccMessage.CAN.MTI of
        MTI_CAN_AME :          // Alias Map Enquiry
          begin
            if LccMessage.DataCount = 6 then
            begin
              LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
              if EqualNodeID(TestNodeID, NodeID, False) then
              begin
                WorkerMessage.LoadAMD(NodeID, AliasID);
                SendMessageFunc(WorkerMessage);
              end
            end else
            begin
              WorkerMessage.LoadAMD(NodeID, AliasID);
              SendMessageFunc(WorkerMessage);
            end;
            Result := True;
          end;
      end
    end;
    if not Result then
      Result := inherited ProcessMessage(LccMessage);
  end;
end;

procedure TLccCanNode.Relogin;
begin
  // Typically due to an alias conflict to create a new one
  GenerateNewSeed(FSeedNodeID);
  FAliasID := GenerateID_Alias_From_Seed(FSeedNodeID);

  Assert(SendMessageFunc = nil, 'SentMessageFunc is nil');
  WorkerMessage.LoadCID(NodeID, AliasID, 0);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 1);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 2);
  SendMessageFunc(WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 3);
  SendMessageFunc(WorkerMessage);

  _800msTimer.Enabled := True;  //  Next state is in the event handler to see if anyone objects tor our Alias
end;

function TLccCanNode.GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
begin
  Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
end;

procedure TLccCanNode.CreateNodeID(var Seed: TNodeID);
begin
  Randomize;
  Seed[1] := StrToInt('0x020112');
  {$IFDEF DWSCRIPT}
  Result[0] := RandomInt($FFFFFF);
  {$ELSE}
  Seed[0] := Random($FFFFFF);
  {$ENDIF}
end;

procedure TLccCanNode.GenerateNewSeed(var Seed: TNodeID);
var
  temp1,              // Upper 24 Bits of temp 48 bit number
  temp2: DWORD;       // Lower 24 Bits of temp 48 Bit number
begin
  temp1 := ((Seed[1] shl 9) or ((Seed[0] shr 15) and $000001FF)) and $00FFFFFF;   // x(i+1)(2^9 + 1)*x(i) + C  = 2^9 * x(i) + x(i) + C
  temp2 := (Seed[0] shl 9) and $00FFFFFF;                                                                  // Calculate 2^9 * x

  Seed[0] := Seed[0] + temp2 + $7A4BA9;   // Now y = 2^9 * x so all we have left is x(i+1) = y + x + c
  Seed[1] := Seed[1] + temp1 + $1B0CA3;

  Seed[1] := (Seed[1] and $00FFFFFF) or (Seed[0] and $FF000000) shr 24;   // Handle the carries of the lower 24 bits into the upper
  Seed[0] := Seed[0] and $00FFFFFF;
end;

procedure TLccCanNode.SendAMD;
begin
  if Permitted then
  begin
    WorkerMessage.LoadAMD(NodeID, AliasID);
    Assert(SendMessageFunc = nil, 'SentMessageFunc is nil');
    SendMessageFunc(WorkerMessage);
  end;
end;

procedure TLccCanNode.SendAMR;
begin
  if Permitted then
  begin
    FPermitted := False;
    WorkerMessage.LoadAMR(NodeID, AliasID);
    Assert(SendMessageFunc = nil, 'SentMessageFunc is nil');
    SendMessageFunc(WorkerMessage);
  end;
end;



{TLccNode }

function TLccNode.GetNodeIDStr: String;
begin
 Result := IntToHex(NodeID[1], 6);
 Result := Result + IntToHex(NodeID[0], 6);
 Result := '0x' + Result
end;

constructor TLccNode.Create(ASendMessageFunc: TLccSendMessageFunc);
begin
  inherited Create(ASendMessageFunc);
  FProtocolSupportedProtocols := TProtocolSupportedProtocols.Create(ASendMessageFunc);
  FProtocolSimpleNodeInfo := TProtocolSimpleNodeInfo.Create(ASendMessageFunc);
  FTProtocolConfigurationDefinitionInfo := TProtocolConfigurationDefinitionInfo.Create(ASendMessageFunc, MSI_CDI, True);
 //JDK FConfigurationMem := TConfigurationMemory.Create(ASendMessageFunc);
 FProtocolEventConsumed := TProtocolEvents.Create(ASendMessageFunc);
  FProtocolEventsProduced := TProtocolEvents.Create(ASendMessageFunc);
  FProtocolConfigurationOptions := TProtocolConfigurationOptions.Create(ASendMessageFunc);
  FProtocolConfigurationInfo := TProtocolConfiguratinInfo.Create(ASendMessageFunc);
  FACDIMfg := TACDIMfg.Create(nil, MSI_ACDI_MFG, True);
  FACDIUser := TACDIUser.Create(SendMessageFunc, MSI_ACDI_USER, True);
  FConfiguration := TConfiguration.Create(SendMessageFunc, MSI_CONFIG, False);
  FDatagramQueue := TDatagramQueue.Create(SendMessageFunc);
  {$IFNDEF FPC_CONSOLE_APP}
  _800msTimer := TTimer.Create(nil);
  {$ELSE}
  _800msTimer := TFPTimer.Create(nil);
  {$ENDIF}
  _800msTimer.Enabled := False;
  _800msTimer.Interval := 800;
  _800msTimer.OnTimer := @On_800msTimer;
end;

procedure TLccNode.AutoGenerateEvents;
var
  i: Integer;
  TempEventID: TEventID;
begin
  TempEventID := NULL_EVENT_ID;
  if ProtocolEventConsumed.AutoGenerate.Enable then
  begin
    ProtocolEventConsumed.Clear;
    for i := 0 to ProtocolEventConsumed.AutoGenerate.Count - 1 do
    begin
      NodeIDToEventID(NodeID, ProtocolEventConsumed.AutoGenerate.StartIndex + i, TempEventID);
      ProtocolEventConsumed.Add(TempEventID, ProtocolEventConsumed.AutoGenerate.DefaultState);
    end;
    ProtocolEventConsumed.Valid := True;
  end;

  if ProtocolEventsProduced.AutoGenerate.Enable then
  begin
    ProtocolEventsProduced.Clear;
    for i := 0 to ProtocolEventsProduced.AutoGenerate.Count - 1 do
    begin
      NodeIDToEventID(NodeID, ProtocolEventsProduced.AutoGenerate.StartIndex + i, TempEventID);
      ProtocolEventsProduced.Add(TempEventID, ProtocolEventsProduced.AutoGenerate.DefaultState);
    end;
    ProtocolEventsProduced.Valid := True;
  end;
end;

destructor TLccNode.Destroy;
begin
  _800msTimer.Enabled := False;
  _800msTimer.Free;
  FProtocolSupportedProtocols.Free;
  FProtocolSimpleNodeInfo.Free;
  FTProtocolConfigurationDefinitionInfo.Free;
 //JDK FConfigurationMem.Free;
  FProtocolEventConsumed.Free;
  FProtocolEventsProduced.Free;
  FProtocolConfigurationOptions.Free;
  FProtocolConfigurationInfo.Free;
  FACDIMfg.Free;
  FACDIUser.Free;
  FConfiguration.Free;
  FDatagramQueue.Free;
  inherited;
end;

function TLccNode.IsNode(LccMessage: TLccMessage; TestType: TIsNodeTestType): Boolean;
begin
 Result := False;

 {
 if TestType = ntt_Dest then
 begin
   if LccMessage.HasDestNodeID and not NullNodeID(NodeID) then
     Result := ((NodeID[0] = LccMessage.DestID[0]) and (NodeID[1] = LccMessage.DestID[1])) or (AliasID = LccMessage.CAN.DestAlias)
   else
   if (AliasID <> 0) and (LccMessage.CAN.DestAlias <> 0) then
     Result := AliasID = LccMessage.CAN.DestAlias
 end else
 if TestType = ntt_Source then
 begin
   if LccMessage.HasSourceNodeID and not NullNodeID(NodeID) then
     Result := ((NodeID[0] = LccMessage.SourceID[0]) and (NodeID[1] = LccMessage.SourceID[1])) or (AliasID = LccMessage.CAN.SourceAlias)
   else
   if (AliasID <> 0) and (LccMessage.CAN.SourceAlias <> 0) then
     Result := AliasID = LccMessage.CAN.SourceAlias
 end;     }
end;

procedure TLccNode.Login(ANodeID: TNodeID);
begin

end;

procedure TLccNode.Logout;
begin
  _800msTimer.Enabled := False;
  DatagramQueue.Clear;
end;

procedure TLccNode.On_800msTimer(Sender: TObject);
begin
  DatagramQueue.TickTimeout;
end;

function TLccNode.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  TestNodeID: TNodeID;
begin
  Result := False;
  if not Initialized then
  begin
    SendInitializeComplete;
    SendEvents;
    FInitialized := True;
  end;



                       (*
  if LoggedIn then
  begin
    TestNodeID[0] := 0;
    TestNodeID[1] := 0;

    if LogInAliasID <> 0 then
    begin
      if LccMessage.CAN.SourceAlias = LogInAliasID then
      begin
       {$IFDEF DWSCRIPT}
        var Temp: TNodeID;
        Temp := FNodeID;
        LoginAliasID := CreateAliasID(Temp, True);
        FNodeID := Temp;
        {$ELSE}
        LoginAliasID := CreateAliasID(FSeedNodeID, True);
        {$ENDIF}
        SendAliasLoginRequest;
        FLoggedIn := False;
        LoginTimer.Enabled := True;
        Exit;
      end;
    end;

    if Permitted then
    begin
      if LccMessage.CAN.SourceAlias = AliasID then
      begin
        if ((LccMessage.CAN.MTI and $0F000000) >= MTI_CAN_CID6) and ((LccMessage.CAN.MTI and $0F000000) <= MTI_CAN_CID0) then
        begin
          WorkerMessage.LoadRID(AliasID);                   // sorry charlie this is mine
 //JDK         OwnerManager.DoRequestMessageSend(WorkerMessage);
          Result := True;
          Exit;
        end else
        begin
          WorkerMessage.LoadAMR(NodeID, AliasID);          // You used my Alias you dog......
 //JDK         OwnerManager.DoRequestMessageSend(WorkerMessage);
          FPermitted := False;
          Login(False, True);
          Result := True;
          Exit;
        end;
      end;
    end;

    if LccMessage.HasDestination then
    begin
      if (LccMessage.CAN.DestAlias > 0) and (AliasID > 0) then
      begin
        if LccMessage.CAN.DestAlias <> AliasID then
          Exit;
      end else
      begin
        if not EqualNodeID(LccMessage.DestID, NodeID, False) then
          Exit;
      end;
  //    LccSourceNode := OwnerManager.FindMirroredNodeBySourceID(LccMessage, True);
    end;

    if Permitted and Initialized then
    begin
      if LccMessage.IsCAN then
      begin
        case LccMessage.CAN.MTI of
          MTI_CAN_AME  :
              begin
                if LccMessage.DataCount = 6 then
                begin
                  LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
                  if EqualNodeID(TestNodeID, NodeID, False) then
                  begin
                    WorkerMessage.LoadAMD(NodeID, AliasID);
 //JDK                   OwnerManager.DoRequestMessageSend(WorkerMessage);
                  end
                end else
                begin
                  WorkerMessage.LoadAMD(NodeID, AliasID);
//JDK                  OwnerManager.DoRequestMessageSend(WorkerMessage);
                end;
                Result := True;
              end;
          MTI_CAN_AMD  :
              begin
                if LccMessage.DataCount = 6 then
                begin
                  LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
                  if EqualNodeID(TestNodeID, NodeID, False) then                  // some Dog has my Node ID!
                  begin
                    {$IFDEF DWSCRIPT}
                    WorkerMessage.LoadPCER(NodeID, AliasID, EVENT_DUPLICATE_ID_DETECTED);
                    {$ELSE}
                    WorkerMessage.LoadPCER(NodeID, AliasID, @EVENT_DUPLICATE_ID_DETECTED);
                    {$ENDIF}
  //JDK                  OwnerManager.DoRequestMessageSend(WorkerMessage);
                  end
                end;
                Result := True;
              end;
          MTI_CAN_RID  : begin end;
        end;
      end else
      begin
        case LccMessage.MTI of
          MTI_OPTIONAL_INTERACTION_REJECTED :
              begin
              end;
          MTI_VERIFY_NODE_ID_NUMBER      :
              begin
                if LccMessage.DataCount = 6 then
                begin
                  LccMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
                  if EqualNodeID(TestNodeID, NodeID, False) then
                  begin
                    WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
  //JDK                  OwnerManager.DoRequestMessageSend(WorkerMessage);
                  end
                end else
                begin
                  WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
  //JDK                 OwnerManager.DoRequestMessageSend(WorkerMessage);
                end;
                Result := True;
              end;
          MTI_VERIFY_NODE_ID_NUMBER_DEST :
              begin
                WorkerMessage.LoadVerifiedNodeID(NodeID, AliasID);
  //JDK               OwnerManager.DoRequestMessageSend(WorkerMessage);
                Result := True;
              end;
          MTI_VERIFIED_NODE_ID_NUMBER :
              begin

              end;
          MTI_SIMPLE_NODE_INFO_REQUEST :
              begin
                WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ProtocolSimpleNodeInfo.PackedFormat);
  //JDK               OwnerManager.DoRequestMessageSend(WorkerMessage);
                Result := True;
              end;
          MTI_SIMPLE_NODE_INFO_REPLY :
              begin               // Called if I send a SNIP;
                ProtocolSupportedProtocols.ProcessMessage(LccMessage);
  //JDK               if Assigned(OwnerManager) then
 //JDK                 OwnerManager.DoSimpleNodeIdentReply(LccSourceNode, Self);
                Result := True;
              end;
          MTI_PROTOCOL_SUPPORT_INQUIRY :
              begin
                WorkerMessage.LoadProtocolIdentifyReply(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, ProtocolSupportedProtocols.EncodeFlags);
 //JDK                OwnerManager.DoRequestMessageSend(WorkerMessage);
                Result := True;
              end;
          MTI_PROTOCOL_SUPPORT_REPLY :
              begin   // Called if I send a Protocol Support
                 ProtocolSupportedProtocols.ProcessMessage(LccMessage);
  //JDK               if Assigned(OwnerManager) then
 //JDK                 OwnerManager.DoProtocolIdentifyReply(LccSourceNode, Self);
                Result := True;
              end;
          MTI_EVENTS_IDENTIFY :
              begin
                SendConsumedEvents;
                SendProducedEvents;
                Result := True;
              end;
          MTI_EVENTS_IDENTIFY_DEST :
              begin
                if AliasID = LccMessage.CAN.DestAlias then
                begin
                  SendConsumedEvents;
                  SendProducedEvents;
                end;
                Result := True;
              end;
          MTI_PRODUCER_IDENDIFY :
              begin
                {$IFDEF DWSCRIPT}
                var Temp: TEventID;
                Temp := LccMessage.ExtractDataBytesAsEventID(0);
                SendProducerIdentify(Temp);
                {$ELSE}
                SendProducerIdentify(LccMessage.ExtractDataBytesAsEventID(0)^);
                {$ENDIF}
                Result := True;
              end;
          MTI_CONSUMER_IDENTIFY :
              begin
                {$IFDEF DWSCRIPT}
                var Temp: TEventID;
                Temp := LccMessage.ExtractDataBytesAsEventID(0);
                SendConsumerIdentify(Temp);
                {$ELSE}
                SendConsumerIdentify(LccMessage.ExtractDataBytesAsEventID(0)^);
                {$ENDIF}
                Result := True;
              end;
           MTI_CONSUMER_IDENTIFIED_CLEAR :
              begin
              end;
           MTI_CONSUMER_IDENTIFIED_SET :
              begin
              end;
           MTI_CONSUMER_IDENTIFIED_UNKNOWN :
              begin
              end;
           MTI_PRODUCER_IDENTIFIED_CLEAR :
              begin
              end;
           MTI_PRODUCER_IDENTIFIED_SET :
              begin
              end;
           MTI_PRODUCER_IDENTIFIED_UNKNOWN :
              begin
              end;
           MTI_DATAGRAM_REJECTED_REPLY :
             begin
               DatagramQueue.Resend(LccMessage);
             end;
           MTI_DATAGRAM_OK_REPLY :
             begin
               DatagramQueue.Remove(LccMessage);
             end;
           MTI_DATAGRAM :
             begin
               case LccMessage.DataArrayIndexer[0] of
                 DATAGRAM_PROTOCOL_CONFIGURATION :
                   begin
                     case LccMessage.DataArrayIndexer[1] and $F0 of
                       MCP_WRITE :
                         begin
                           case LccMessage.DataArrayIndexer[1] and $03 of
                             MCP_NONE :
                                 begin
                                   case LccMessage.DataArrayIndexer[6] of
                                     MSI_CDI             :
                                         begin
                                         end;  // Not writeable
                                     MSI_ALL             :
                                         begin
                                         end;  // Not writeable
                                     MSI_CONFIG          :
                                         begin
                                           SendAckReply(LccMessage, False, 0);     // We will be sending a Write Reply
                                           Configuration.WriteRequest(LccMessage);
                                           Result := True;
                                         end;
                                     MSI_ACDI_MFG        :
                                         begin
                                         end;  // Not writeable
                                     MSI_ACDI_USER       :
                                         begin
  //                                         SendAckReply(LccMessage, False, 0);     // We will be sending a Write Reply
  //                                         ACDIUser.WriteRequest(LccMessage);
                                           Result := True;
                                         end;
                                     MSI_FDI             :
                                         begin
                                         end;  // Not writeable
                                     MSI_FUNCTION_CONFIG :
                                         begin
                                         end;
                                   end
                                 end;
                             MCP_CONFIGURATION :
                                 begin
                                   SendAckReply(LccMessage, False, 0);             // We will be sending a Write Reply
                                   Configuration.WriteRequest(LccMessage);
                                   Result := True;
                                 end;
                             MCP_ALL           :
                                 begin
                                 end; // Not writeable
                             MCP_CDI           :
                                 begin
                                 end; // Not writeable
                           end;
                         end;
                       MCP_WRITE_STREAM :
                           begin
                           end;
                       MCP_READ :
                           begin
                             case LccMessage.DataArrayIndexer[1] and $03 of
                               MCP_NONE :
                                   begin
                                     case LccMessage.DataArrayIndexer[6] of
                                       MSI_CDI             :
                                           begin
                                             SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                             WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                                             ProtocolConfigurationDefinitionInfo.LoadReply(LccMessage, WorkerMessage);
                                             if WorkerMessage.UserValid then
                                             begin
   //JDK                                             OwnerManager.DoRequestMessageSend(WorkerMessage);
                                               DatagramQueue.Add(WorkerMessage.Clone);      // Waiting for an ACK
                                             end;
                                             Result := True;
                                           end;
                                       MSI_ALL             :
                                           begin
                                             SendAckReply(LccMessage, False, 0);   // We won't be sending a Read Reply
                                           end;
                                       MSI_CONFIG          :
                                           begin
                                             SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                             WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                                             Configuration.LoadReply(LccMessage, WorkerMessage);
                                             if WorkerMessage.UserValid then
                                             begin
   //JDK                                             OwnerManager.DoRequestMessageSend(WorkerMessage);
                                               DatagramQueue.Add(WorkerMessage.Clone);      // Waiting for an ACK
                                             end;
                                             Result := True;
                                           end;
                                       MSI_ACDI_MFG        :
                                           begin
                                             {
                                             SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                             WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                                             ACDIMfg.LoadReply(LccMessage, WorkerMessage);
                                             if WorkerMessage.UserValid then
                                             begin
                                               OwnerManager.DoRequestMessageSend(WorkerMessage);
                                               DatagramQueue.Add(WorkerMessage.Clone);      // Waiting for an ACK
                                             end;
                                             Result := True;
                                             }
                                             Result := False;
                                           end;
                                       MSI_ACDI_USER       :
                                           begin
                                             {
                                             SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                             WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                                             ACDIUser.LoadReply(LccMessage, WorkerMessage);
                                             if WorkerMessage.UserValid then
                                             begin
                                               OwnerManager.DoRequestMessageSend(WorkerMessage);
                                               DatagramQueue.Add(WorkerMessage.Clone);      // Waiting for an ACK
                                             end;
                                             Result := True;
                                             }
                                             Result := False;
                                           end;
                                       MSI_FDI             :
                                            begin
                                            end;
                                       MSI_FUNCTION_CONFIG :
                                            begin
                                            end;
                                     end
                                   end;
                               MCP_CONFIGURATION : begin
                                                     SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                                     WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                                                     Configuration.LoadReply(LccMessage, WorkerMessage);
                                                     if WorkerMessage.UserValid then
                                                     begin
   //JDK                                                     OwnerManager.DoRequestMessageSend(WorkerMessage);
                                                       DatagramQueue.Add(WorkerMessage.Clone);      // Waiting for an ACK
                                                     end;
                                                     Result := True;
                                                   end;
                               MCP_ALL           : begin  end;
                               MCP_CDI           : begin
                                                     SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                                     WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                                                     ProtocolConfigurationDefinitionInfo.LoadReply(LccMessage, WorkerMessage);
                                                     if WorkerMessage.UserValid then
                                                     begin
    //JDK                                                    OwnerManager.DoRequestMessageSend(WorkerMessage);
                                                       DatagramQueue.Add(WorkerMessage.Clone);      // Waiting for an ACK
                                                     end;
                                                     Result := True;
                                                   end;
                             end;
                           end;
                       MCP_READ_STREAM :
                           begin
                           end;
                       MCP_OPERATION :
                           begin
                             case LccMessage.DataArrayIndexer[1] of
                               MCP_OP_GET_CONFIG :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                     WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                                     ProtocolConfigurationOptions.LoadReply(WorkerMessage);
                                     if WorkerMessage.UserValid then;
                                     begin
   //JDK                                     OwnerManager.DoRequestMessageSend(WorkerMessage);
                                       DatagramQueue.Add(WorkerMessage.Clone);      // Waiting for an ACK
                                     end;
                                     Result := True;
                                   end;
                               MCP_OP_GET_ADD_SPACE_INFO :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                     WorkerMessage.LoadDatagram(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias);
                                     ProtocolConfigurationInfo.LoadReply(LccMessage, WorkerMessage);
                                     if WorkerMessage.UserValid then
                                     begin
 //JDK                                       OwnerManager.DoRequestMessageSend(WorkerMessage);
                                       DatagramQueue.Add(WorkerMessage.Clone);      // Waiting for an ACK
                                     end;
                                     Result := True;
                                   end;
                               MCP_OP_LOCK :
                                   begin
                                   end;
                               MCP_OP_GET_UNIQUEID :
                                   begin
                                   end;
                               MCP_OP_FREEZE :
                                   begin
                                   end;
                               MCP_OP_INDICATE :
                                   begin
                                   end;
                               MCP_OP_RESETS :
                                   begin
                                   end;
                             end // case
                           end
                     end; // case
                   end
               else begin
                   // Undknown Datagram Type
                   WorkerMessage.LoadDatagramRejected(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, REJECTED_DATAGRAMS_NOT_ACCEPTED);
       //JDK             OwnerManager.DoRequestMessageSend(WorkerMessage);
                   Result := True;
                 end;
               end;  // case
             end;
        else begin
            if LccMessage.HasDestination then
            begin
              WorkerMessage.LoadOptionalInteractionRejected(NodeID, AliasID, LccMessage.SourceID, LccMessage.CAN.SourceAlias, REJECTED_BUFFER_FULL, LccMessage.MTI);
      //JDK         OwnerManager.DoRequestMessageSend(WorkerMessage);
              Result := True;
            end;
          end;
        end; // case
      end;
    end;
  end;            *)
end;

procedure TLccNode.SendAckReply(LccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  // Only Ack if we accept the datagram
  WorkerMessage.LoadDatagramAck(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, True, ReplyPending, TimeOutValueN);
  Assert(SendMessageFunc = nil, 'SendMessageFunc is nil');
  SendMessageFunc(WorkerMessage);
end;

procedure TLccNode.SendConsumedEvents;
var
  i: Integer;
  Temp: TEventID;
begin
  for i := 0 to ProtocolEventConsumed.Count - 1 do
  begin
    Temp := ProtocolEventConsumed.Event[i].ID;
//JDK    WorkerMessage.LoadConsumerIdentified(NodeID, AliasID, Temp, ProtocolEventConsumed.Event[i].State);
    Assert(SendMessageFunc = nil, 'SentMessageFunc is nil');
    SendMessageFunc(WorkerMessage);
  end;
end;

procedure TLccNode.SendConsumerIdentify(var Event: TEventID);
var
  EventObj: TLccEvent;
  Temp: TEventID;
begin
  EventObj := ProtocolEventConsumed.Supports(Event);
  if Assigned(EventObj) then
  begin
    Temp := EventObj.ID;
 //JDK   WorkerMessage.LoadConsumerIdentified(NodeID, AliasID, Temp, EventObj.State);
    SendMessageFunc(WorkerMessage);
  end;
end;

procedure TLccNode.SendEvents;
begin
  SendConsumedEvents;
  SendProducedEvents;
end;

procedure TLccNode.SendInitializeComplete;
begin
  WorkerMessage.LoadInitializationComplete(NodeID, );
end;

procedure TLccNode.SendProducedEvents;
var
  i: Integer;
  Temp: TEventID;
begin
  for i := 0 to ProtocolEventsProduced.Count - 1 do
  begin
    Temp := ProtocolEventsProduced.Event[i].ID;
 //JDK   WorkerMessage.LoadProducerIdentified(NodeID, AliasID, Temp, ProtocolEventsProduced.Event[i].State);
    SendMessageFunc(WorkerMessage);
  end;
end;

procedure TLccNode.SendProducerIdentify(var Event: TEventID);
var
  EventObj: TLccEvent;
  Temp: TEventID;
begin
   EventObj := ProtocolEventsProduced.Supports(Event);
   if Assigned(EventObj) then
   begin
     Temp := EventObj.ID;
 //JDK    WorkerMessage.LoadProducerIdentified(NodeID, AliasID, Temp, EventObj.State);
     SendMessageFunc(WorkerMessage);
   end;
end;

(*
{ TLccOwnedNode }

constructor TLccOwnedNode.Create(ASendMessageFunc: TLccSendMessageFunc);
begin
  inherited Create(ASendMessageFunc);
//  LogInAliasID := 0;
end;


destructor TLccOwnedNode.Destroy;
begin
  inherited Destroy;
end;

procedure TLccOwnedNode.Login(NewNodeID, RegenerateAliasSeed: Boolean);
begin
  if NewNodeID then
    GenerateNewNodeID;

  AutoGenerateEvents;
  Configuration.LoadFromFile;
//JDK  if Assigned(OwnerManager) then
//JDK    OwnerManager.DoNodeIDChanged(Self);
  {$IFDEF DWSCRIPT}
  var Temp: TNodeID;
  Temp := FSeedNodeID;
  LoginAliasID := CreateAliasID(Temp, RegenerateAliasSeed);
  FSeedNodeID := Temp;
  {$ELSE}
  LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
  {$ENDIF}
  SendAliasLoginRequest;
  DuplicateAliasDetected := False;
  FLoggedIn := False;
//  LoginTimer.Enabled := True;
end;

{
procedure TLccOwnedNode.LoginWithLccSettings(RegenerateAliasSeed: Boolean);
var
  TempNodeID: TNodeID;
  TempID, TempID1, TempID2: QWord;
begin
  TempNodeID[0] := 0;
  TempNodeID[1] := 0;
  if Assigned(OwnerManager.LccSettings)then
  begin
    if OwnerManager.LccSettings.General.NodeIDAsVal = 0 then
    begin
      GenerateNewNodeID;
      OwnerManager.LccSettings.General.NodeIDAsTNodeID(TempNodeID);
      if not EqualNodeID(TempNodeID, NodeID, True) then
      begin
         TempID1 := QWord(NodeID[0]);
         TempID2 := QWord(NodeID[1]);
         TempID2 := TempID2 shl 24;
         TempID := TempID1 or TempID2;
         OwnerManager.LccSettings.General.NodeID := '0x'+IntToHex(TempID, 12);
         OwnerManager.LccSettings.SaveToFile;
      end;
    end else
    begin
      OwnerManager.LccSettings.General.NodeIDAsTNodeID(TempNodeID);
      FNodeID[0] := TempNodeID[0];
      FNodeID[1] := TempNodeID[1];
    end;
    AutoGenerateEvents;
    Configuration.LoadFromFile;
    if Assigned(OwnerManager) then
      OwnerManager.DoNodeIDChanged(Self);
    LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
    SendAliasLoginRequest;
    DuplicateAliasDetected := False;
    FLoggedIn := False;
    LoginTimer.Enabled := True;
  end else
    Login(True, True)
end;
}

procedure TLccOwnedNode.LoginWithNodeID(ANodeID: TNodeId; RegenerateAliasSeed: Boolean);
begin
  FNodeID[0] := ANodeID[0];
  FNodeID[1] := ANodeID[1];
  FSeedNodeID[0] := ANodeID[0];
  FSeedNodeID[1] := ANodeID[1];

  AutoGenerateEvents;
  Configuration.LoadFromFile;
//JDK  if Assigned(OwnerManager) then
//JDK    OwnerManager.DoNodeIDChanged(Self);
  {$IFDEF DWSCRIPT}
  var Temp: TNodeID;
  Temp := FSeedNodeID;
  LoginAliasID := CreateAliasID(Temp, RegenerateAliasSeed);
  FSeedNodeID := Temp;
  {$ELSE}
  LoginAliasID := CreateAliasID(FSeedNodeID, RegenerateAliasSeed);
  {$ENDIF}
  SendAliasLoginRequest;
  DuplicateAliasDetected := False;
  FLoggedIn := False;
 // LoginTimer.Enabled := True;
end;

procedure TLccOwnedNode.LogOut;
begin
//JDK  ClearOwned;
//  LoginTimer.Enabled := False;
  SendAMR;
  DatagramQueue.Clear;
  FLoggedIn := False;
  FInitialized := False;
  FPermitted := False;
end;

{ TLccDefaultRootNode }

constructor TLccDefaultRootNode.Create(ASendMessageFunc: TLccSendMessageFunc);
var
  i: Integer;
begin
  inherited Create(ASendMessageFunc);
  // Common Protocols
  ProtocolSupportedProtocols.Datagram := True;        // We support ProtocolConfigurationDefinitionInfo and Configruation Memory so we must support datagrams
  ProtocolSupportedProtocols.MemConfig := True;       // Memory Configuration
  ProtocolSupportedProtocols.CDI := True;             // We Support ProtocolConfigurationDefinitionInfo
  ProtocolSupportedProtocols.EventExchange := True;   // We support Events
  ProtocolSupportedProtocols.SimpleNodeInfo := True;  // We Support SNIP
  ProtocolSupportedProtocols.Valid := True;

  // Setup a basic ProtocolConfigurationDefinitionInfo
  {$IFDEF DWSCRIPT}
  CDI.AStream.Position := 0;
  CDI.AStream.Size := 0;
  {$ELSE}
  ProtocolConfigurationDefinitionInfo.AStream.Clear;
  {$ENDIF}
  {$IFDEF LCC_MOBILE}     // Delphi only
    for i := 0 to Length(CDI_XML) - 1 do
      CDI.AStream.Write(Ord(CDI_XML[i]), 1);
  {$ELSE}
    for i := 1 to Length(CDI_XML) do
    {$IFDEF FPC}
      ProtocolConfigurationDefinitionInfo.AStream.WriteByte(Ord(CDI_XML[i]));
    {$ELSE}
      CDI.AStream.Write( Ord(CDI_XML[i]), 1);
    {$ENDIF}
  {$ENDIF}
  ProtocolConfigurationDefinitionInfo.Valid := True;

  ProtocolConfigurationDefinitionInfo.LoadSNIP(ProtocolSimpleNodeInfo);

  // Setup the Configuraion Memory Options:
  ProtocolConfigurationOptions.HighSpace := MSI_CDI;
  ProtocolConfigurationOptions.LowSpace := MSI_ACDI_USER;
  ProtocolConfigurationOptions.SupportACDIMfgRead := True;
  ProtocolConfigurationOptions.SupportACDIUserRead := True;
  ProtocolConfigurationOptions.SupportACDIUserWrite := True;
  ProtocolConfigurationOptions.UnAlignedReads := True;
  ProtocolConfigurationOptions.UnAlignedWrites := True;
  ProtocolConfigurationOptions.WriteArbitraryBytes := True;
  ProtocolConfigurationOptions.WriteLenFourBytes := True;
  ProtocolConfigurationOptions.WriteLenOneByte := True;
  ProtocolConfigurationOptions.WriteLenSixyFourBytes := True;
  ProtocolConfigurationOptions.WriteLenTwoBytes := True;
  ProtocolConfigurationOptions.WriteStream := False;
  ProtocolConfigurationOptions.WriteUnderMask := False;
  ProtocolConfigurationOptions.Valid := True;

  // Setup the Configuration Memory Addres Space Information
  ProtocolConfigurationInfo.Add(MSI_CDI, True, True, True, $00000000, $FFFFFFFF);
  ProtocolConfigurationInfo.Add(MSI_ALL, True, True, True, $00000000, $FFFFFFFF);
  ProtocolConfigurationInfo.Add(MSI_CONFIG, True, False, True, $00000000, $FFFFFFFF);
  ProtocolConfigurationInfo.Add(MSI_ACDI_MFG, True, True, True, $00000000, $FFFFFFFF);      // We don't support ACDI in this object
  ProtocolConfigurationInfo.Add(MSI_ACDI_USER, True, False, True, $00000000, $FFFFFFFF);    // We don't support ACDI in this object
  ProtocolConfigurationInfo.Valid := True;
end;

*)


(*
{ TConfigurationMemory }

function TConfigurationMemory.GetDataRawIndexer(iIndex: Word): Byte;
begin
  Result := FDataRaw[iIndex]
end;

procedure TConfigurationMemory.Initialize(AnAddress: DWord;
  AnAddressSpace: Byte; DataSize: Integer; ADataType: TLccConfigDataType);
begin
  ErrorCode := 0;
  Address := AnAddress;
  DataCount := DataSize;
  AddressSpace := AnAddressSpace;
  InProcessAddress := AnAddress;
  DataType := ADataType;
  Valid := False;
  FDataTypeInteger := 0;
  FDataTypeEvent[0] := 0;
  FDataTypeEvent[1] := 0;
  FDataTypeEvent[2] := 0;
  FDataTypeEvent[3] := 0;
  FDataTypeEvent[4] := 0;
  FDataTypeEvent[5] := 0;
  FDataTypeEvent[6] := 0;
  FDataTypeEvent[7] := 0;
  FDataTypeBit := 0;
  FDataTypeString := '';
end;

function TConfigurationMemory.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  iStart, i, RemainingCount: Integer;

  LocalAddressSpace: Byte;
  SourceNode: TLccNode;
begin
  Result := True;
  LocalAddressSpace := 0;
  RemainingCount := 0;
  if LccMessage.DataArrayIndexer[1] and MCP_READ_REPLY = MCP_READ_REPLY then
  begin
    // First Block of Data
    if InProcessAddress = Address then
    begin
      if LccMessage.DataArrayIndexer[1] and $03 <> 0 then
      begin
         case LccMessage.DataArrayIndexer[1] and $03 of
           MCP_CDI           : LocalAddressSpace := MSI_CDI;
           MCP_ALL           : LocalAddressSpace := MSI_ALL;
           MCP_CONFIGURATION : LocalAddressSpace := MSI_CONFIG;
         end;
         iStart := 6;
      end else
      begin
         LocalAddressSpace := LccMessage.DataArrayIndexer[6];
         iStart := 7
      end;
      if LocalAddressSpace <> AddressSpace then
        ErrorCode := ErrorCode or ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH;
    end else
    begin
      // Subsequent Blocks of Data
      if LccMessage.DataArrayIndexer[1] and $03 <> 0 then
        iStart := 6
      else
        iStart := 7
    end;

    DataCount := 0;
    if ErrorCode = 0 then
    begin
      DataCount := LccMessage.DataCount - iStart;
      for i := 0 to DataCount - 1 do
        DataRawIndexer[i] := LccMessage.DataArrayIndexer[i + iStart];
      case DataType of
        cdt_String :
          begin
            InProcessAddress := InProcessAddress + DWord((LccMessage.DataCount - iStart));
            for i := 0 to LccMessage.DataCount - iStart - 1 do
              FDataTypeString := FDataTypeString + Chr(LccMessage.DataArrayIndexer[i+iStart]);

            RemainingCount := DataCount - Length(FDataTypeString);           // Strings are 1 indexed
            if RemainingCount > 64 then
              RemainingCount := 64;
            if RemainingCount > 0 then
            begin
              WorkerMessage.LoadConfigMemRead(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.SourceAlias, MSI_CONFIG, InProcessAddress, RemainingCount);
   //JDK           OwnerManager.DoRequestMessageSend(WorkerMessage);
            end
          end;
        cdt_Int :
          begin
            FDataTypeInteger := LccMessage.ExtractDataBytesAsInt(iStart, LccMessage.DataCount-1);
            RemainingCount := 0;
          end;
        cdt_EventID :
          begin
            FDataTypeEvent := LccMessage.ExtractDataBytesAsEventID(iStart){$IFNDEF DWSCRIPT}^{$ENDIF};
            RemainingCount := 0;
          end;
        cdt_Bit :
          begin
            // ToDo
          end;
       end
    end;

    if (ErrorCode = 0) or (RemainingCount <= 0) then
    begin
      Valid := ErrorCode = 0;
 {     SourceNode := OwnerManager.FindMirroredNodeBySourceID(LccMessage, True);
      {$IFNDEF DWSCRIPT}
      if Assigned(OwnerManager.CdiParser) then    // Callback on the CDI Parser if available to sync its UI
        OwnerManager.CdiParser.DoConfigMemReadReply(SourceNode);
      {$ENDIF}
      OwnerManager.DoConfigMemReadReply(SourceNode, OwnerManager.FindMirroredNodeByDestID(LccMessage, True));   }
    end;
  end else
  if LccMessage.DataArrayIndexer[1] and MCP_WRITE_REPLY <> 0 then
  begin
    ErrorCode := 0;

    if ErrorCode = 0 then
    begin
{      SourceNode := OwnerManager.FindMirroredNodeBySourceID(LccMessage, True);
      {$IFNDEF DWSCRIPT}
      if Assigned(OwnerManager.CdiParser) then    // Callback on the CDI Parser if available to sync its UI
        OwnerManager.CdiParser.DoConfigMemWriteReply(SourceNode);
      {$ENDIF}
      OwnerManager.DoConfigMemWriteReply(SourceNode, OwnerManager.FindMirroredNodeByDestID(LccMessage, True));  }
    end;
  end;
end;

procedure TConfigurationMemory.SetDataRawIndexer(iIndex: Word; const Value: Byte);
begin
  FDataRaw[iIndex] := Value
end;
*)

(*
function TLccCanNode.CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
begin
  if Regenerate then
    PsudoRandomNumberGeneratorOnSeed(Seed);
  Result := GenerateID_Alias_From_Seed(Seed);
  if Result = 0 then
  begin
    PsudoRandomNumberGeneratorOnSeed(Seed);
    Result := GenerateID_Alias_From_Seed(Seed);
  end;
end;
 *)





initialization

finalization

end.

