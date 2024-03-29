unit lcc_node_train;

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
  lcc_alias_server,
  lcc_utilities,
  lcc_defines,
  lcc_node_messages,
  lcc_math_float16,
  lcc_node;

const
  MAX_NMRA_DCC_DATA = 5;     // Number of Bytes in a valid NMRA DCC Message
  NMRA_LONGADDRESS_MASK_BYTE         = $C0;
  NMRA_LONGADDRESS_MASK_WORD         = $C000;
  MAX_DCC_FUNCTIONS                  = 29;

  // Mappings of the train speed step to the encoded Byte
  // 128 step is a direct mapping with the high bit the direction
  const
  _28_STEP_TABLE: array[0..28] of Byte = (
    {$IFNDEF FPC}
    $00,    // Stop
    $02,    // Step 1
    $12,    // Step 2
    $03,    // Step 3
    $13,    // Step 4
    $04,    // Step 5
    $14,    // Step 6
    $05,    // Step 7
    $15,    // Step 8
    $06,    // Step 9
    $16,    // Step 10
    $07,    // Step 11
    $17,    // Step 12
    $08,    // Step 13
    $18,    // Step 14
    $09,    // Step 15
    $19,    // Step 16
    $0A,    // Step 17
    $1A,    // Step 18
    $0B,    // Step 19
    $1B,    // Step 20
    $0C,    // Step 21
    $1C,    // Step 22
    $0D,    // Step 23
    $1D,    // Step 24
    $0E,    // Step 25
    $1E,    // Step 26
    $0F,    // Step 27
    $0F     // Step 28
    {$ELSE}
    %00000000,    // Stop
    %00000010,    // Step 1
    %00010010,    // Step 2
    %00000011,    // Step 3
    %00010011,    // Step 4
    %00000100,    // Step 5
    %00010100,    // Step 6
    %00000101,    // Step 7
    %00010101,    // Step 8
    %00000110,    // Step 9
    %00010110,    // Step 10
    %00000111,    // Step 11
    %00010111,    // Step 12
    %00001000,    // Step 13
    %00011000,    // Step 14
    %00001001,    // Step 15
    %00011001,    // Step 16
    %00001010,    // Step 17
    %00011010,    // Step 18
    %00001011,    // Step 19
    %00011011,    // Step 20
    %00001100,    // Step 21
    %00011100,    // Step 22
    %00001101,    // Step 23
    %00011101,    // Step 24
    %00001110,    // Step 25
    %00011110,    // Step 26
    %00001111,    // Step 27
    %00011111     // Step 28
    {$ENDIF}
  );

  _14_STEP_TABLE: array[0..14] of Byte = (
    {$IFNDEF FPC}
    $00,    // Stop
    $02,    // Step 1
    $03,    // Step 3
    $04,    // Step 5
    $05,    // Step 7
    $06,    // Step 9
    $07,    // Step 11
    $08,    // Step 13
    $09,    // Step 15
    $0A,    // Step 17
    $0B,    // Step 19
    $0C,    // Step 21
    $0D,    // Step 23
    $0E,    // Step 25
    $0F     // Step 27
    {$ELSE}
    %00000000,    // Stop
    %00000010,    // Step 1
    %00000011,    // Step 3
    %00000100,    // Step 5
    %00000101,    // Step 7
    %00000110,    // Step 9
    %00000111,    // Step 11
    %00001000,    // Step 13
    %00001001,    // Step 15
    %00001010,    // Step 17
    %00001011,    // Step 19
    %00001100,    // Step 21
    %00001101,    // Step 23
    %00001110,    // Step 25
    %00001111     // Step 27
    {$ENDIF}
  );

type
    // ***************************************************************************
  // Implements the raw byte array that hold the NMRA DCC Message bytes
  // ***************************************************************************
  TDCCPacketBytesArray = array[0..MAX_NMRA_DCC_DATA-1] of Byte;
 // PDCCPacketBytesArray = ^TDCCPacketBytesArray;


  TDCCPacket = record
    PacketBytes: TDCCPacketBytesArray;       // NMRA defines at most 5 data LoadTransmitter per message packet
    Flags: Byte;                               // See the QUEUE_MESSAGE_XXXXX Flags
                                               // Bit 0 1 2         = Number of Valid data bytes in the message.
                                               // Bit 3             = Address is a Multi-Function decoder with 7 bit address (short address)
                                               // Bit 4             = Address is a Basic Accessory Decoder with 9 bit address and Extended Accessory Decoder with 11 bit address
                                               // Bit 5             = Address is a Multi-Function decoder with a 14 bit address (extended address)
                                               // Bit 6             = Address is in the NMRA Reserved range
                                               // Bit 7             = Address is special ($00 = Reset; $FF = Idle; $FE = ??? but defined in S-9.2);
  end;


const
  CDI_XML_TRAIN_NODE: string = (
  '<?xml version="1.0" encoding="utf-8"?>'+
  '<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
  '<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
         '<identification>'+
       	  '<manufacturer>Mustangpeak</manufacturer>'+
       	  '<model>VTN1000</model>'+
       	  '<hardwareVersion>1.0.0.0</hardwareVersion>'+
       	  '<softwareVersion>1.0.0.0</softwareVersion>'+
         '</identification>'+
         '<segment origin="1" space="253">'+
       	  '<name>User</name>'+
       	  '<description>User defined information</description>'+
       	  '<group>'+
       		  '<name>User Data</name>'+
       		  '<description>Add your own unique node info here</description>'+
       		  '<string size="63">'+
       			  '<name>User Name</name>'+
       		  '</string>'+
       		  '<string size="64">'+
       			  '<name>User Description</name>'+
       		  '</string>'+
       	  '</group>'+
         '</segment>'+
  '</cdi>');


type

   { TLccActionTractionControllerAssignReply }

   TLccActionTractionControllerAssignReply = class(TLccAction)
   private
     FAssignResult: Byte;
     FRequestingControllerAliasID: Word;
     FRequestingControllerNodeID: TNodeID;
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
     function _1TestForAnotherControllerAssigned(Sender: TObject; SourceMessage: TLccMessage): Boolean;
     function _2WaitForChangeNotify(Sender: TObject; SourceMessage: TLccMessage): Boolean;
     function _3SendAssignReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

     procedure LoadStateArray; override;

     property AssignResult: Byte read FAssignResult write FAssignResult;
     property RequestingControllerNodeID: TNodeID read FRequestingControllerNodeID write FRequestingControllerNodeID;
     property RequestingControllerAliasID: Word read FRequestingControllerAliasID write FRequestingControllerAliasID;
   end;

   { TLccActionTractionControllerRelease }

   TLccActionTractionControllerRelease = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
   end;

   { TLccActionTractionQueryController }

   TLccActionTractionQueryController = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccActionTractionQueryFunctionReply }

   TLccActionTractionQueryFunctionReply = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccActionTractionQuerySpeedReply }

   TLccActionTractionQuerySpeedReply = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccActionTractionSetSpeed }

   TLccActionTractionSetSpeed = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
   end;

   { TLccActionTractionSetFunction }

   TLccActionTractionSetFunction = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
   end;

   { TLccActionTractionEmergencyStop }

   TLccActionTractionEmergencyStop = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccActionTractionListenerAttachAndReply }

   TLccActionTractionListenerAttachAndReply = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;

     procedure LoadStateArray; override;
   end;

   { TLccActionTractionListenerDetachAndReply }

   TLccActionTractionListenerDetachAndReply = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccActionTractionQueryListenerAndReply }

   TLccActionTractionQueryListenerAndReply = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;


   { TLccActionTractionManageReserve }

   TLccActionTractionManageReserve = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;

     procedure LoadStateArray; override;
   end;


   { TLccActionTractionManageRelease }

   TLccActionTractionManageRelease = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccActionMemoryConfigurationRead }

   TLccActionMemoryConfigurationRead = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
     function _1WaitForReadReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

     procedure LoadStateArray; override;
   end;

type

  { TListenerNode }

  TListenerNode = class
  private
    FHidden: Boolean;
    FLinkF0: Boolean;
    FLinkFn: Boolean;
    FNodeID: TNodeID;
    FReverseDirection: Boolean;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property ReverseDirection: Boolean read FReverseDirection write FReverseDirection;
    property LinkF0: Boolean read FLinkF0 write FLinkF0;
    property LinkFn: Boolean read FLinkFn write FLinkFn;
    property Hidden: Boolean read FHidden write FHidden;

    procedure DecodeFlags(Flags: Byte);
    function EncodeFlags: Byte;
  end;

  { TListenerList }

  TListenerList = class
  private
    function GetCount: Integer;
    function GetListener(Index: Integer): TListenerNode;
  protected
    {$IFDEF DELPHI}
    FListenerList: TObjectList<TListenerNode>;
   {$ELSE}
    FListenerList: TObjectList;
   {$ENDIF}

    function FindNodeIndex(NodeID: TNodeID): Integer;
  public
    property Count: Integer read GetCount;
    property Listener[Index: Integer]: TListenerNode read GetListener;

    constructor Create;
    destructor Destroy; override;
    function Add(NodeID: TNodeID; Flags: Byte; AliasID: Word = 0): TListenerNode;
    function Delete(NodeID: TNodeID): Boolean;
    function FindNode(NodeID: TNodeID): TListenerNode;
    function FindByIndex(Index: Byte): TListenerNode;
  end;

type
  { TLccTrainDccNode }

  TLccTrainDccNode = class(TLccNode)
  private
    // DCC Only fields, should make a common class and derive a DCC specific one from it
    FDccAddress: Word;
    FDccLongAddress: Boolean;
    FDccSpeedStep: TLccDccSpeedStep;
    FOnSendMessageComPort: TMessageComPort;

    // Lcc Traction fields
    FFunctions: TLccFunctions;
    FReservationAliasID: Word;
    FReservationNodeID: TNodeID;
    FSpeed: THalfFloat;

    FSearchEvent: TEventID;  // When creating a train node this is the temporary storage for the train node as the node is being created and allocating is NodeID/AliasID
    FListeners: TListenerList;
    FAttachedController: TAttachedController;
    FRequestingControllerAliasID: Word;    // When a controller in trying to attach it is stored here in the actions when we have to query other controllers to ask them if it is ok
    FRequestingControllerNodeID: TNodeID;  // When a controller in trying to attach it is stored here in the actions when we have to query other controllers to ask them if it is ok

    function GetDirection: TLccTrainDirection;
    function GetFunctions(Index: Integer): Word;

    procedure SetDccAddress(AValue: Word);
    procedure SetDccLongAddress(AValue: Boolean);
    procedure SetDccSpeedStep(AValue: TLccDccSpeedStep);

    procedure SetDirection(AValue: TLccTrainDirection);
    procedure SetFunctions(Index: Integer; AValue: Word);
    procedure SetSpeed(AValue: THalfFloat);
  protected
    property AttachedController: TAttachedController read FAttachedController write FAttachedController;

    property RequestingControllerNodeID: TNodeID read FRequestingControllerNodeID write FRequestingControllerNodeID;
    property RequestingControllerAliasID: Word read FRequestingControllerAliasID write FRequestingControllerAliasID;

    property ReservationNodeID: TNodeID read FReservationNodeID write FReservationNodeID;
    property ReservationAliasID: Word read FReservationAliasID write FReservationAliasID;

    procedure ClearAttachedController;
    procedure ClearReservationNode;

    function GetCdiFile: string; override;
    procedure BeforeLogin; override;
    function EncodeFunctionValuesDccStyle: DWORD;
    function EncodeToDccGridConnect(DccPacket: TDCCPacket): String;
    procedure DoSendMessageComPort(GridConnectString: string);

    function DccFunctionHandler(DccAddress: Word; LongAddress: Boolean; FunctionAddress: DWORD; AllDccFunctionBitsEncoded: DWORD): TDCCPacket;
    function DccSpeedDirHandler(DccAddress: Word; LongAddress: Boolean; SpeedDir: THalfFloat; DccSpeedStep: TLccDccSpeedStep): TDCCPacket;
    procedure DccLoadPacket(var NewMessage: TDCCPacket; Data1, Data2, Data3, Data4, Data5, ValidDataByes: Byte);
  public

    property DccAddress: Word read FDccAddress write SetDccAddress;
    property DccLongAddress: Boolean read FDccLongAddress write SetDccLongAddress;
    property DccSpeedStep: TLccDccSpeedStep read FDccSpeedStep write SetDccSpeedStep;

    property Listeners: TListenerList read FListeners write FListeners;
    property Speed: THalfFloat read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read GetDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;
    property OnSendMessageComPort: TMessageComPort read FOnSendMessageComPort write FOnSendMessageComPort;
    property SearchEvent: TEventID read FSearchEvent write FSearchevent;

    constructor Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: String; GridConnectLink: Boolean); override;
    destructor Destroy; override;

    function ControllerEquals(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
    function IsReservedBy(SourceMessage: TLccMessage): Boolean;
    function IsReserved: Boolean;
    function ReservationEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
    function ControllerAssigned: Boolean;
    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccTrainCanNodeClass = class of TLccTrainDccNode;

  function SpeedStepToString(SpeedStep: TLccDccSpeedStep; Verbose: Boolean): string;
  function SpeedStepToIndex(SpeedStep: TLccDccSpeedStep): Integer;
  function IndexToSpeedStep(Index: Integer): TLccDccSpeedStep;
  function AddressBooleanToText(IsLong: Boolean; Verbose: Boolean): string;

implementation

uses
  lcc_node_manager;

function SpeedStepToString(SpeedStep: TLccDccSpeedStep; Verbose: Boolean): string;
begin
  if Verbose then
  begin
    case SpeedStep of
      ldssDefault : Result := 'Default';
      ldss14      : Result := '14 Step';
      ldss28      : Result := '28 Step';
      ldss128     : Result := '128 Step';
     else
       Result := 'Unknown'
     end;
  end else
  begin
    case SpeedStep of
      ldssDefault : Result := 'S=Def';
      ldss14      : Result := 'S=14';
      ldss28      : Result := 'S=28';
      ldss128     : Result := 'S=128';
     else
       Result := 'Unknown'
     end;
  end;
end;

function SpeedStepToIndex(SpeedStep: TLccDccSpeedStep): Integer;
begin
  case SpeedStep of
    ldssDefault : Result := 0;
    ldss14      : Result := 1;
    ldss28      : Result := 2;
    ldss128     : Result := 3;
   else
     Result := -1;
   end;
end;

function IndexToSpeedStep(Index: Integer): TLccDccSpeedStep;
begin
  case Index of
     0 : Result := ldssDefault;
     1 : Result := ldss14;
     2 : Result := ldss28;
     3 : Result := ldss128;
   else
     Result := ldssDefault;
   end;
end;

function AddressBooleanToText(IsLong: Boolean; Verbose: Boolean): string;
begin
  if Verbose then
  begin
    if IsLong then
      Result := 'Long Address'
    else
      Result := 'Short Address'
  end else
  begin
     if IsLong then
      Result := 'L'
    else
      Result := 'S'
  end
end;

{ TLccActionMemoryConfigurationRead }

function TLccActionMemoryConfigurationRead._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
  DccServiceModeGridConnect, AddressStr: string;
  Address: DWord;
  ReadCount: Word;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  if Assigned(SourceMessage) then
  begin
    OwnerTrain := Owner as TLccTrainDccNode;
    Address := SourceMessage.ExtractDataBytesAsInt(2, 5);
    ReadCount := 0;
    if SourceMessage.DataArray[1] = MCP_READ then
    begin
      if SourceMessage.DataArray[6] = MSI_CONFIG then
        ReadCount := SourceMessage.ExtractDataBytesAsWord(7)
      else if SourceMessage.DataArray[1] = MCP_READ_CONFIGURATION then
        ReadCount := SourceMessage.ExtractDataBytesAsWord(6)
    end;

    if ReadCount > 0 then
    begin
      AddressStr := IntToStr(Address);
      DccServiceModeGridConnect := ':S00000000N' + AddressStr + '0000;';
      OwnerTrain.DoSendMessageComPort(DccServiceModeGridConnect);
      AdvanceToNextState;
    end else
    begin
      SourceMessage.LoadConfigMemReadReplyError(SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, Address, MSI_CONFIG, $FF, 'Read Timed Out');
      AdvanceToLastState;
    end;
  end;
end;

function TLccActionMemoryConfigurationRead._1WaitForReadReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := True;
  if TimeoutCounts > TimeoutCountThreshold then
    AdvanceToNextState();
end;

procedure TLccActionMemoryConfigurationRead.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1WaitForReadReply;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup
end;

{ TLccActionTractionManageRelease }

function TLccActionTractionManageRelease._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  if Assigned(SourceMessage) then
  begin
    OwnerTrain := Owner as TLccTrainDccNode;
    if OwnerTrain.ReservationEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
      OwnerTrain.ClearReservationNode;
  end;

  AdvanceToNextState;
end;

{ TLccActionTractionManageReserve }

function TLccActionTractionManageReserve._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  OwnerTrain := Owner as TLccTrainDccNode;

  if OwnerTrain.ReservationEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) or OwnerTrain.ReservationEquals(NULL_NODE_ID, 0) then
  begin
    OwnerTrain.ReservationNodeID := SourceMessage.SourceID;
    OwnerTrain.ReservationAliasID := SourceMessage.CAN.SourceAlias;
    WorkerMessage.LoadTractionManageReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, True);
    SendMessage(Self, WorkerMessage);
  end else
  begin
    WorkerMessage.LoadTractionManageReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, False);
    SendMessage(Self, WorkerMessage);
  end;
  AdvanceToNextState;
end;

procedure TLccActionTractionManageReserve.LoadStateArray;
begin
  SetStateArrayLength(4);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup
end;

{ TListenerNode }

procedure TListenerNode.DecodeFlags(Flags: Byte);
begin
  FReverseDirection := Flags and TRACTION_LISTENER_FLAG_REVERSE_DIR = TRACTION_LISTENER_FLAG_REVERSE_DIR;
  FLinkF0 := Flags and TRACTION_LISTENER_FLAG_LINK_F0 = TRACTION_LISTENER_FLAG_LINK_F0;
  FLinkFn := Flags and TRACTION_LISTENER_FLAG_LINK_FN = TRACTION_LISTENER_FLAG_LINK_FN;
  FHidden := Flags and TRACTION_LISTENER_FLAG_HIDDEN = TRACTION_LISTENER_FLAG_HIDDEN;
end;

function TListenerNode.EncodeFlags: Byte;
begin
  Result := 0;
  if ReverseDirection then Result := Result or TRACTION_LISTENER_FLAG_REVERSE_DIR;
  if ReverseDirection then Result := Result or TRACTION_LISTENER_FLAG_LINK_F0;
  if ReverseDirection then Result := Result or TRACTION_LISTENER_FLAG_LINK_FN;
  if ReverseDirection then Result := Result or TRACTION_LISTENER_FLAG_HIDDEN;
end;

{ TListenerList }

constructor TListenerList.Create;
begin
  inherited Create;
  {$IFDEF DELPHI}
  FListenerList := TObjectList<TListenerNode>.Create(False);
  {$ELSE}
    FListenerList := TObjectList.Create;
    {$IFNDEF DWSCRIPT}
    FListenerList.OwnsObjects := True;
    {$ENDIF}
  {$ENDIF}
end;

function TListenerList.Add(NodeID: TNodeID; Flags: Byte; AliasID: Word): TListenerNode;
begin
 // AliasID := AliasID;
  Result := TListenerNode.Create;
  Result.NodeID := NodeID;
  Result.DecodeFlags(Flags);
  FListenerList.Add(Result);
end;

function TListenerList.Delete(NodeID: TNodeID): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := FindNodeIndex(NodeID);
  if Index >= 0 then
  begin
    {$IFDEF DWSCRIPT}
    FListenerList.Remove(Index);
    {$ELSE}
    FListenerList.Delete(Index);
    {$ENDIF}
    Result := True;
  end;
end;

destructor TListenerList.Destroy;
begin
  {$IFNDEF DWSCRIPT}
  FreeAndNil(FListenerList);
  {$ELSE}
  FListenerList.Free;
  {$ENDIF}
  inherited Destroy;
end;

function TListenerList.FindNode(NodeID: TNodeID): TListenerNode;
var
  i: Integer;
  ListenerNode: TListenerNode;
begin
  Result := nil;
  i := 0;
  while (Result = nil) and (i < FListenerList.Count) do
  begin
    ListenerNode := (FListenerList[i] as TListenerNode);
    if (ListenerNode.NodeID[0] = NodeID[0]) and (ListenerNode.NodeID[1] = NodeID[1]) then
      Result := ListenerNode;
    Inc(i);
  end;
end;

function TListenerList.FindByIndex(Index: Byte): TListenerNode;
begin
  if Index < Count then
    Result := FListenerList[Index] as TListenerNode
  else
    Result := nil
end;

function TListenerList.FindNodeIndex(NodeID: TNodeID): Integer;
var
  i: Integer;
  ListenerNode: TListenerNode;
begin
  Result := -1;
  i := 0;
  while (Result < 0) and (i < FListenerList.Count) do
  begin
    ListenerNode := (FListenerList[i] as TListenerNode);
    if (ListenerNode.NodeID[0] = NodeID[0]) and (ListenerNode.NodeID[1] = NodeID[1]) then
      Result := i;
    Inc(i);
  end;

end;

function TListenerList.GetCount: Integer;
begin
  Result := FListenerList.Count;
end;

function TListenerList.GetListener(Index: Integer): TListenerNode;
begin
  Result := FListenerList[Index] as TListenerNode
end;


{ TLccActionTractionQueryListenerAndReply }

function TLccActionTractionQueryListenerAndReply._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
  Listener: TListenerNode;
  RequestedIndex: Integer;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  OwnerTrain := Owner as TLccTrainDccNode;
  if (SourceMessage.DataCount = 2) then // no query data, just wants the total number of listeners retured
    WorkerMessage.LoadTractionListenerQueryReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, OwnerTrain.Listeners.Count, 0, NULL_NODE_ID, 0)
  else begin
    RequestedIndex := SourceMessage.DataArray[2];
    if RequestedIndex < OwnerTrain.Listeners.Count then
    begin
      Listener := OwnerTrain.Listeners.FindByIndex(RequestedIndex);
      WorkerMessage.LoadTractionListenerQueryReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, OwnerTrain.Listeners.Count, RequestedIndex, Listener.NodeID, Listener.EncodeFlags);
      if Assigned(SourceMessage) then
        (OwnerTrain.NodeManager as INodeManagerCallbacks).DoTractionListenerQuery(OwnerTrain, RequestedIndex);
    end else
    begin
      WorkerMessage.LoadTractionListenerQueryReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, OwnerTrain.Listeners.Count, 0, NULL_NODE_ID, 0);   // Outside of range, bad index
      if Assigned(SourceMessage) then
        (OwnerTrain.NodeManager as INodeManagerCallbacks).DoTractionListenerQuery(OwnerTrain, -1);
    end;
  end;
  SendMessage(Self, WorkerMessage);
  AdvanceToNextState;
end;

{ TLccActionTractionListenerDetachAndReply }

function TLccActionTractionListenerDetachAndReply._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
  ReplyCode: Word;
  ListenerNodeID: TNodeID;
  ListenerFlags: Byte;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  OwnerTrain := Owner as TLccTrainDccNode;
  ListenerNodeID := NULL_NODE_ID;
  SourceMessage.ExtractDataBytesAsNodeID(3, ListenerNodeID);
  ListenerFlags := SourceMessage.DataArrayIndexer[2];

  if OwnerTrain.Listeners.Delete(ListenerNodeID) then
    ReplyCode := S_OK
  else
    ReplyCode := ERROR_PERMANENT_INVALID_ARGUMENTS;

  (OwnerTrain.NodeManager as INodeManagerCallbacks).DoTractionListenerDetach(OwnerTrain, ListenerNodeID, ListenerFlags);

  WorkerMessage.LoadTractionListenerDetachReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, ListenerNodeID, ReplyCode);
  SendMessage(Self, WorkerMessage);
  AdvanceToNextState;
end;

{ TLccActionTractionListenerAttachAndReply }

function TLccActionTractionListenerAttachAndReply._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
  NewListenerNode: TListenerNode;
  ReplyCode: Word;
  ListenerAttachFlags: Byte;
  ListenerNodeID: TNodeID;
  ListenerNodeAlias: Word;
  AliasMapping: TLccAliasMapping;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);


  ReplyCode := S_OK;
  OwnerTrain := Owner as TLccTrainDccNode;

  // Things to concider:
  //   1) Can't add a Node as its own Listener
  //   2) Listener could already be in the list, don't add duplicates but may just want to update flags
  //   3) Only the connected Controller can Attach a Listener : Balazs does not agree
  //   4) The Train must be Reserved by the connected controller
  //   5) Not enough memory to allocate another Listener

  ListenerNodeID := NULL_NODE_ID;
  SourceMessage.ExtractDataBytesAsNodeID(3, ListenerNodeID);  // Make SMS happy

  ListenerNodeAlias := 0;
  if Owner.GridConnect then
  begin
    AliasMapping := Owner.AliasServer.FindMapping(ListenerNodeID);
    ListenerNodeAlias := AliasMapping.NodeAlias;
  end;

  ListenerAttachFlags := SourceMessage.DataArray[2];

  if EqualNodeID(SourceNodeID, ListenerNodeID, False) then  // Trying to create a Listener that is the nodes itself.... will cause infinte loops
    ReplyCode := ERROR_PERMANENT_INVALID_ARGUMENTS;
  // Balasz does not agree this is a failure
  // if not EqualNodeID(NodeID, OwnerTrain.AttachedController.NodeID, False) then
  //   ReplyCode := ERROR_PERMANENT or ERROR_SOURCE_NOT_PERMITED;

  if ReplyCode = S_OK then
  begin
    NewListenerNode := OwnerTrain.Listeners.FindNode(ListenerNodeID);
    if Assigned(NewListenerNode) then
    begin  // Simple update of flags
      NewListenerNode.DecodeFlags(ListenerAttachFlags);
      WorkerMessage.LoadTractionListenerAttachReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, ListenerNodeID, ReplyCode);
      SendMessage(Self, WorkerMessage);
    end else
    begin  // Add The listener to the list
      NewListenerNode := OwnerTrain.Listeners.Add(ListenerNodeID, ListenerNodeAlias, ListenerAttachFlags);
      if Assigned(NewListenerNode) then
        ReplyCode := S_OK
      else
        ReplyCode := ERROR_TEMPORARY_BUFFER_UNAVAILABLE;
      WorkerMessage.LoadTractionListenerAttachReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, ListenerNodeID, ReplyCode);
      SendMessage(Self, WorkerMessage);
    end;

    (OwnerTrain.NodeManager as INodeManagerCallbacks).DoTractionListenerAttach(OwnerTrain, ListenerNodeID, ListenerAttachFlags);
  end else
  begin // Send the Error Message
    WorkerMessage.LoadTractionListenerAttachReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, ListenerNodeID, ReplyCode);
    SendMessage(Self, WorkerMessage);
  end;
  AdvanceToNextState;
end;

procedure TLccActionTractionListenerAttachAndReply.LoadStateArray;
begin
  SetStateArrayLength(2);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup
end;

{ TLccActionTractionQueryController }

function TLccActionTractionQueryController._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  AttachedController: TAttachedController;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  AttachedController := (Owner as TLccTrainDccNode).AttachedController;
  if (AttachedController.NodeID[0] = 0) and (AttachedController.NodeID[1] = 0) and (AttachedController.AliasID = 0) then
    WorkerMessage.LoadTractionControllerQueryReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, NULL_NODE_ID)
  else
    WorkerMessage.LoadTractionControllerQueryReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, AttachedController.NodeID);
  SendMessage(Self, WorkerMessage);
  AdvanceToNextState;
end;

{ TLccActionTractionControllerRelease }

function TLccActionTractionControllerRelease._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  OwnerTrain := Owner as TLccTrainDccNode;
  if OwnerTrain.ControllerEquals(DestNodeID, DestAliasID) then
    OwnerTrain.ClearAttachedController;
  AdvanceToNextState;
end;

{ TLccActionTractionEmergencyStop }

function TLccActionTractionEmergencyStop._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  OwnerTrain := Owner as TLccTrainDccNode;
  OwnerTrain.Speed := 0;
  AdvanceToNextState;
end;

{ TLccActionTractionSetFunction }

function TLccActionTractionSetFunction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
  FunctionAddress: DWORD;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  OwnerTrain := Owner as TLccTrainDccNode;
  if OwnerTrain.ControllerAssigned then
    if OwnerTrain.ControllerEquals(DestNodeID, DestAliasID) then
    begin
      FunctionAddress := SourceMessage.TractionExtractFunctionAddress;
      OwnerTrain.Functions[FunctionAddress] := SourceMessage.TractionExtractFunctionValue;
    end;
  AdvanceToNextState;
end;

{ TLccActionTractionSetSpeed }

function TLccActionTractionSetSpeed._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  OwnerTrain := Owner as TLccTrainDccNode;
  if OwnerTrain.ControllerAssigned then
    if OwnerTrain.ControllerEquals(DestNodeID, DestAliasID) then
      OwnerTrain.Speed := SourceMessage.TractionExtractSetSpeed;
  AdvanceToNextState;
end;

{ TLccActionTractionQueryFunctionReply }

function TLccActionTractionQueryFunctionReply._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  FunctionAddress: DWORD;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  FunctionAddress := SourceMessage.TractionExtractFunctionAddress;
  WorkerMessage.LoadTractionQueryFunctionReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, FunctionAddress, (Owner as TLccTrainDccNode).Functions[FunctionAddress]);
  SendMessage(Owner, WorkerMessage);
  AdvanceToNextState;
end;

{ TLccActionTractionQuerySpeedReply }

function TLccActionTractionQuerySpeedReply._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  OwnerTrain := Owner as TLccTrainDccNode;
  WorkerMessage.LoadTractionQuerySpeedReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, OwnerTrain.Speed, 0, HalfNaN, HalfNaN);
  SendMessage(Owner, WorkerMessage);
  AdvanceToNextState;
end;

{ TLccActionTractionControllerAssignReply }

procedure TLccActionTractionControllerAssignReply.LoadStateArray;
begin
  SetStateArrayLength(6);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1TestForAnotherControllerAssigned;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2WaitForChangeNotify;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_3SendAssignReply;
  States[4] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup
end;

function TLccActionTractionControllerAssignReply._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  LocalRequestingControllerNodeID: TNodeID;
  AliasMapping: TLccAliasMapping;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  case SourceMessage.MTI of
     MTI_TRACTION_REQUEST :
       begin
         case SourceMessage.DataArray[0] of
            TRACTION_CONTROLLER_CONFIG :
              begin;
                case SourceMessage.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN :
                    begin
                      // No reservation required
                      // SMS Quirk
                      LocalRequestingControllerNodeID := NULL_NODE_ID;
                      LocalRequestingControllerNodeID := SourceMessage.ExtractDataBytesAsNodeID(3, LocalRequestingControllerNodeID);

                      // LccNode ensures that this mapping is valid before calling any of the messages that kick off these Actions
                      if Owner.GridConnect then
                      begin
                        AliasMapping := Owner.AliasServer.FindMapping(LocalRequestingControllerNodeID);
                        RequestingControllerAliasID := AliasMapping.NodeAlias;
                        RequestingControllerNodeID := AliasMapping.NodeID;
                      end;
                      AdvanceToNextState;
                    end;
                 end
              end;
         end;
       end;
  end;
end;

function TLccActionTractionControllerAssignReply._1TestForAnotherControllerAssigned(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainDccNode;
begin
  Result := False;

  // SourceMessage is not used here and this get clocked by the timer pump
  OwnerTrain := Owner as TLccTrainDccNode;

  if OwnerTrain.ControllerAssigned and not OwnerTrain.ControllerEquals(RequestingControllerNodeID, FRequestingControllerAliasID) then
  begin
    WorkerMessage.LoadTractionControllerChangingNotify(SourceNodeID, SourceAliasID, OwnerTrain.AttachedController.NodeID, OwnerTrain.AttachedController.AliasID, RequestingControllerNodeID);
    SendMessage(Owner, WorkerMessage);
    SetTimoutCountThreshold(TIMEOUT_CONTROLLER_NOTIFY_WAIT, True);
    AssignResult := TRACTION_CONTROLLER_CONFIG_REPLY_PENDING;
  end else
    AssignResult := TRACTION_CONTROLLER_CONFIG_REPLY_OK;

  AdvanceToNextState;
end;

function TLccActionTractionControllerAssignReply._2WaitForChangeNotify(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  if AssignResult = TRACTION_CONTROLLER_CONFIG_REPLY_PENDING then
  begin
    if Assigned(SourceMessage) then   // Could be time tick calling and we need to wait for a real message
    begin
      case SourceMessage.MTI of
       MTI_TRACTION_REPLY :
         begin
           case SourceMessage.DataArray[0] of
             TRACTION_CONTROLLER_CONFIG :
               begin
                 case SourceMessage.DataArray[1] of
                   TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY :
                     begin
                       AssignResult := SourceMessage.DataArray[2]; // Could be TRACTION_CONTROLLER_CONFIG_REPLY_OK or TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER
                       AdvanceToNextState;
                     end;
                 end;
               end;
           end;
         end;
       end;
    end;

    if TimeoutExpired then
    begin
      AssignResult := TRACTION_CONTROLLER_CONFIG_REPLY_OK;  // No answer, must be ok
      AdvanceToNextState;
    end;

  end else
  begin
    AssignResult := TRACTION_CONTROLLER_CONFIG_REPLY_OK;
    AdvanceToNextState;  // nothing to wait for
  end;
end;

function TLccActionTractionControllerAssignReply._3SendAssignReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  // Don't need the SourceMessage and will get clocked by the timer pump
  if AssignResult = TRACTION_CONTROLLER_CONFIG_REPLY_OK then
  begin
    (Owner as TLccTrainDccNode).ClearAttachedController;
    (Owner as TLccTrainDccNode).FAttachedController.NodeID := RequestingControllerNodeID;
    (Owner as TLccTrainDccNode).FAttachedController.AliasID := RequestingControllerAliasID;
  end;
  WorkerMessage.LoadTractionControllerAssignReply(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, AssignResult);
  SendMessage(Owner, WorkerMessage);
  AdvanceToNextState;
end;

{ TLccTrainDccNode }

constructor TLccTrainDccNode.Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: String; GridConnectLink: Boolean);
begin
  inherited Create(ASendMessageFunc, ANodeManager, CdiXML, GridConnectLink);
  FListeners := TListenerList.Create;
end;

procedure TLccTrainDccNode.BeforeLogin;
var
  DccAddressStr: string;
  i: Integer;
  {$IFDEF DELPHI}
  B: Byte;
  {$ENDIF}
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.MemConfig := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;
  ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := True;
  ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := True;
  ProtocolSupportedProtocols.TractionFunctionConfiguration := True;

  ProtocolEventsProduced.Add(EVENT_IS_TRAIN, evs_Valid);
  ProtocolEventConsumed.Add(EVENT_EMERGENCY_STOP, evs_InValid);

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_TRACTION_FDI, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_TRACTION_FUNCTION_CONFIG, True, False, True, 0, $FFFFFFFF);

  ProtocolMemoryOptions.WriteUnderMask := True;
  ProtocolMemoryOptions.UnAlignedReads := True;
  ProtocolMemoryOptions.UnAlignedWrites := True;
  ProtocolMemoryOptions.SupportACDIMfgRead := True;
  ProtocolMemoryOptions.SupportACDIUserRead := True;
  ProtocolMemoryOptions.SupportACDIUserWrite := True;
  ProtocolMemoryOptions.WriteLenOneByte := True;
  ProtocolMemoryOptions.WriteLenTwoBytes := True;
  ProtocolMemoryOptions.WriteLenFourBytes := True;
  ProtocolMemoryOptions.WriteLenSixyFourBytes := True;
  ProtocolMemoryOptions.WriteArbitraryBytes := True;
  ProtocolMemoryOptions.WriteStream := False;
  ProtocolMemoryOptions.HighSpace := MSI_CDI;
  ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;

  // TODO... this should be a persistent Stream if the user changes the name in the Configuration Dialog
  //         save to a file with the DccAddress as the filename??????
  StreamConfig.Position := ADDRESS_USER_NAME;
  {$IFDEF DELPHI}
  StreamConfig.ReadData(B);
  if B = 0 then
  {$ELSE}
  if StreamConfig.ReadByte = 0 then
  {$ENDIF}
  begin
    StreamConfig.Position := ADDRESS_USER_NAME;

    DccAddressStr := IntToStr(DccAddress);
    if DccAddress < 128 then
    begin
      if DccLongAddress then
        DccAddressStr := DccAddressStr + 'L'
      else
        DccAddressStr := DccAddressStr + 'S';
    end;

    for i := 1 to Length(DccAddressStr) do
    {$IFDEF LCC_MOBILE}
      StreamWriteByte(StreamConfig, Ord(DccAddressStr[i-1]))
    {$ELSE}
      StreamWriteByte(StreamConfig, Ord(DccAddressStr[i]))
    {$ENDIF}
  end;
end;

function TLccTrainDccNode.EncodeFunctionValuesDccStyle: DWORD;
var
  i: Integer;
begin
  Result := 0;
  for i := MAX_DCC_FUNCTIONS - 1 downto 0 do
  begin
    if Functions[i] > 0 then
      Result := Result or $00000001;
    if i > 0 then
      Result := Result shl 1
  end;
end;

function TLccTrainDccNode.EncodeToDccGridConnect(DccPacket: TDCCPacket): String;
var
  i: Integer;
begin
  Result := ':R' + IntToHex(DccPacket.Flags, 8) +  'N';   // make it the same size header as OpenLCB
  for i := 0 to DccPacket.Flags - 1 do
    Result := Result + IntToHex(DccPacket.PacketBytes[i], 2);
  Result := Result + ';';
end;

procedure TLccTrainDccNode.DoSendMessageComPort(GridConnectString: string);
begin
  if Assigned(OnSendMessageComPort) then
    OnSendMessageComPort(Self, GridConnectString);
end;

function TLccTrainDccNode.DccFunctionHandler(DccAddress: Word;
  LongAddress: Boolean; FunctionAddress: DWORD; AllDccFunctionBitsEncoded: DWORD
  ): TDCCPacket;
var
  FunctionMask, FunctionExtendedCode: Byte;
  AddressHi, AddressLo: Byte;
begin
  Result.Flags := 0;
  FunctionMask := 0;
  FunctionExtendedCode := 0;

  // Split the address to make clear when loading bytes
  AddressHi := (DccAddress shr 8) and $00FF;
  if LongAddress then
    AddressHi := AddressHi or NMRA_LONGADDRESS_MASK_BYTE;
  AddressLo := DccAddress and $00FF;

  if FunctionAddress < 29 then
  begin
    if FunctionAddress < 5 then
    begin
        // a bit weird that bit 0 is Function 1 and bit 4 could be Function 0 if CV29 is set right
        FunctionMask := (AllDccFunctionBitsEncoded and $0F) shr 1;
        if AllDccFunctionBitsEncoded and $00000001 = 0 then                      // Look at Function 0, it is contained in Bit 4
          FunctionMask := FunctionMask and not $10                                // Clear Bit 4
        else
          FunctionMask := FunctionMask or $10;                                    // Set Bit 4
      FunctionMask := FunctionMask or {$IFNDEF FPC}$80{$ELSE}%10000000{$ENDIF};                                // Opcode bits
    end else
    if FunctionAddress < 9 then
    begin
      FunctionMask := (AllDccFunctionBitsEncoded shr 5) and $0F;
      FunctionMask := FunctionMask or  {$IFNDEF FPC}$B0{$ELSE}%10110000{$ENDIF};                               // Opcode bits
    end else
    if FunctionAddress < 13 then
    begin
      FunctionMask := (AllDccFunctionBitsEncoded shr 9) and $0F;
      FunctionMask := FunctionMask or {$IFNDEF FPC}$A0{$ELSE}%10100000{$ENDIF};                               // Opcode bits
    end else
    if FunctionAddress < 21 then
    begin
      FunctionMask := AllDccFunctionBitsEncoded shr 13;
      FunctionExtendedCode := {$IFNDEF FPC}$DE{$ELSE}%11011110{$ENDIF};
    end else
    if FunctionAddress < 29 then
    begin
      FunctionMask := AllDccFunctionBitsEncoded shr 21;
      FunctionExtendedCode := {$IFNDEF FPC}$DF{$ELSE}%11011111{$ENDIF};
    end;

    // Now create the DCC Packet
    if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
    begin
      if FunctionAddress < 13 then
        DccLoadPacket(Result, AddressHi, AddressLo, FunctionMask, 0, 0, 3)
      else
        DccLoadPacket(Result, AddressHi, AddressLo, FunctionExtendedCode, FunctionMask, 0, 4)
    end else
    begin
      if FunctionAddress < 13 then
        DccLoadPacket(Result, AddressLo, FunctionMask, 0, 0, 0, 2)
      else
        DccLoadPacket(Result, AddressLo, FunctionExtendedCode, FunctionMask, 0, 0, 3)
    end;
  end
end;

function TLccTrainDccNode.DccSpeedDirHandler(DccAddress: Word;
  LongAddress: Boolean; SpeedDir: THalfFloat; DccSpeedStep: TLccDccSpeedStep
  ): TDCCPacket;
var
  IsForward: Boolean;
  AbsoluteSpeed: single;
  LocalSpeedStep: Word;
  AddressHi, AddressLo: Byte;
begin
  Result.Flags := 0;

  IsForward := SpeedDir and $8000 <> $8000;                                                      // Split the Speed and Direction
  AbsoluteSpeed := HalfToFloat( SpeedDir and not $8000);
  if LongAddress then
    DccAddress := DccAddress or NMRA_LONGADDRESS_MASK_WORD;

  // Split the address to make clear when loading bytes
  AddressHi := Hi(DccAddress);
  AddressLo := Lo(DccAddress);

  case DccSpeedStep of
    ldssDefault,
    ldss14 :
          begin
            AbsoluteSpeed := (14/100) * AbsoluteSpeed;
            LocalSpeedStep := Trunc(AbsoluteSpeed);

            LocalSpeedStep := _14_STEP_TABLE[LocalSpeedStep];
            if IsForward then
              LocalSpeedStep := LocalSpeedStep or $60
            else
              LocalSpeedStep := LocalSpeedStep or $40;
            if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
              DccLoadPacket(Result, AddressHi, AddressLo, LocalSpeedStep, 0, 0, 3)
            else
              DccLoadPacket(Result, AddressLo, LocalSpeedStep, 0, 0, 0, 2);
          end;
    ldss28  :
          begin
            AbsoluteSpeed := (28/100) * AbsoluteSpeed;
            LocalSpeedStep := Trunc(AbsoluteSpeed);
            LocalSpeedStep := _28_STEP_TABLE[LocalSpeedStep];
            if IsForward then
              LocalSpeedStep := LocalSpeedStep or $60
            else
              LocalSpeedStep := LocalSpeedStep or $40;

            if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
              DccLoadPacket(Result, AddressHi, AddressLo, LocalSpeedStep, 0, 0, 3)
            else
              DccLoadPacket(Result, AddressLo, LocalSpeedStep, 0, 0, 0, 2);

          end;
    ldss128 :
          begin
             // Allow a mistaken short address to work here by adding the $C0  Per Tim
            AddressHi := AddressHi or NMRA_LONGADDRESS_MASK_BYTE;

            AbsoluteSpeed := (127/100) * AbsoluteSpeed;
            LocalSpeedStep := Trunc(AbsoluteSpeed);
            if LocalSpeedStep > 0 then
              Inc(LocalSpeedStep);   // 1 = EStop
            if IsForward then
              LocalSpeedStep := LocalSpeedStep or $80;
            DccLoadPacket(Result, AddressHi, AddressLo, {$IFNDEF FPC}$3F{$ELSE}%00111111{$ENDIF}, LocalSpeedStep, 0, 4);
          end;

  end;
end;

destructor TLccTrainDccNode.Destroy;
begin
  {$IFDEF DWSCRIPT}
  FListeners.Free;
  {$ELSE}
  FreeAndNil(FListeners);
  {$ENDIF}
  inherited Destroy;
end;

procedure TLccTrainDccNode.DccLoadPacket(var NewMessage: TDCCPacket; Data1,
  Data2, Data3, Data4, Data5, ValidDataByes: Byte);
begin
  NewMessage.PacketBytes[0] := Data1;
  NewMessage.PacketBytes[1] := Data2;
  NewMessage.PacketBytes[2] := Data3;
  NewMessage.PacketBytes[3] := Data4;
  NewMessage.PacketBytes[4] := Data5;
  NewMessage.Flags := ValidDataByes;
end;

procedure TLccTrainDccNode.ClearAttachedController;
begin
  FAttachedController.NodeID := NULL_NODE_ID;
  FAttachedController.AliasID := 0;
  FAttachedController.NodeID := NULL_NODE_ID;
  FAttachedController.AttachNotifyAliasID := 0;
end;

procedure TLccTrainDccNode.ClearReservationNode;
begin
  FReservationNodeID := NULL_NODE_ID;
  FReservationAliasID := 0;
end;

function TLccTrainDccNode.ControllerAssigned: Boolean;
begin
  Result := ((AttachedController.NodeID[0] <> 0) and (AttachedController.NodeID[1] <> 0) or (AttachedController.AliasID <> 0))
end;

function TLccTrainDccNode.ControllerEquals(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
begin
  Result := ((AttachedController.NodeID[0] = ATestNodeID[0]) and (AttachedController.NodeID[1] = ATestNodeID[1])) or
            (AttachedController.AliasID = ATestNodeAlias)
end;

function TLccTrainDccNode.GetCdiFile: string;
begin
  Result := CDI_XML_TRAIN_NODE
end;

function TLccTrainDccNode.GetDirection: TLccTrainDirection;
begin
  if HalfIsNegative(Speed) then
    Result := tdReverse
  else
    Result := tdForward;
end;

function TLccTrainDccNode.GetFunctions(Index: Integer): Word;
begin
  if (Index >= 0) and (Index < Length(FFunctions)) then
    Result := FFunctions[Index]
  else
    Result := $FFFF
end;

function TLccTrainDccNode.IsReserved: Boolean;
begin
  Result := (ReservationNodeID[0] <> 0) or (ReservationNodeID[1] <> 0) or (ReservationAliasID <> 0);
end;

function TLccTrainDccNode.IsReservedBy(SourceMessage: TLccMessage): Boolean;
begin
  Result := EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ReservationNodeID, ReservationAliasID, True);
end;

function TLccTrainDccNode.ProcessMessageLCC(SourceMessage: TLccMessage
  ): Boolean;
begin
  Result := inherited ProcessMessageLcc(SourceMessage);

  if SourceMessage.HasDestination then
  begin
    if not EqualNode(NodeID,  AliasID, SourceMessage.DestID, SourceMessage.CAN.DestAlias, True) then
      Exit;
  end;

  // We only are dealing with messages with destinations for us from here on

  // Some of these could be done without creating an Action but doing it this way ensures nice encapulation of the code in the Action object and keeps this message handler very clean
  case SourceMessage.MTI of
    MTI_DATAGRAM :
      begin
        case SourceMessage.DataArray[DATAGRAM_PROTOCOL_CONFIGURATION] of
          MCP_READ : Result := LccActions.RegisterAndKickOffAction(TLccActionMemoryConfigurationRead.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
     //     MCP_WRITE : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionSetSpeed.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
        end
      end;
    MTI_TRACTION_REQUEST :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_SET_SPEED_DIR       : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionSetSpeed.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
          TRACTION_SET_FUNCTION        : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionSetFunction.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
          TRACTION_SET_E_STOP          : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionEmergencyStop.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), nil);
          TRACTION_QUERY_SPEED     : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionQuerySpeedReply.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), nil);
          TRACTION_QUERY_FUNCTION  : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionQueryFunctionReply.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN  : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionControllerAssignReply.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
                TRACTION_CONTROLLER_CONFIG_RELEASE : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionControllerRelease.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), nil);
                TRACTION_CONTROLLER_CONFIG_QUERY   : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionQueryController.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), nil);
               end
            end;
          TRACTION_LISTENER :
            begin
              if IsReservedBy(SourceMessage) then
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_LISTENER_ATTACH : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionListenerAttachAndReply.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
                  TRACTION_LISTENER_DETACH : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionListenerDetachAndReply.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
                  TRACTION_LISTENER_QUERY  : Result := LccActions.RegisterAndKickOffAction(TLccActionTractionQueryListenerAndReply.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage);
                end;
              end
            end;
          TRACTION_MANAGE :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE : begin LccActions.RegisterAndKickOffAction(TLccActionTractionManageReserve.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage); end;
                TRACTION_MANAGE_RELEASE : begin LccActions.RegisterAndKickOffAction(TLccActionTractionManageRelease.Create(Self, SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, 0), SourceMessage); end;
              end
            end;
        end;
      end;
  end;
end;

function TLccTrainDccNode.ReservationEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
begin
  Result := ((ReservationNodeID[0] = ATestNodeID[0]) and (ReservationNodeID[1] = ATestNodeID[1])) or (ATestAlias = ReservationAliasID)
end;

procedure TLccTrainDccNode.SetDccAddress(AValue: Word);
begin
  if FDccAddress = AValue then Exit;
  FDccAddress := AValue;
end;

procedure TLccTrainDccNode.SetDccLongAddress(AValue: Boolean);
begin
  if FDccLongAddress = AValue then Exit;
  FDccLongAddress := AValue;
end;

procedure TLccTrainDccNode.SetDirection(AValue: TLccTrainDirection);
begin
  if AValue = tdReverse then
    Speed := FSpeed or $8000
  else
    Speed := FSpeed and $7FFF;
end;

procedure TLccTrainDccNode.SetFunctions(Index: Integer; AValue: Word);
var
  DccPacket: TDCCPacket;
  DccGridConnect: string;
begin
  if (Index >= 0) and (Index < Length(FFunctions)) then
  begin
    FFunctions[Index] := AValue;
    DccPacket := DccFunctionHandler(DccAddress, DccLongAddress, Index, EncodeFunctionValuesDccStyle);
    DccGridConnect := EncodeToDccGridConnect( DccPacket);
    DoSendMessageComPort(DccGridConnect);
  end;
end;

procedure TLccTrainDccNode.SetSpeed(AValue: THalfFloat);
var
  DccPacket: TDCCPacket;
  DccGridConnect: string;
begin
  if AValue = Speed then Exit;
  FSpeed := AValue;
  DccPacket := DccSpeedDirHandler(DccAddress, DccLongAddress, Speed, DccSpeedStep);
  DccGridConnect := EncodeToDccGridConnect( DccPacket);
  DoSendMessageComPort(DccGridConnect);
end;

procedure TLccTrainDccNode.SetDccSpeedStep(AValue: TLccDccSpeedStep);
begin
  if FDccSpeedStep = AValue then Exit;
  FDccSpeedStep := AValue;
end;

end.

