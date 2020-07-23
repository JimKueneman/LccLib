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
      ExtCtrls,
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

   { TLccAssignTrainReplyAction }

   TLccAssignTrainReplyAction = class(TLccAction)
   private
     FAssignResult: Byte;
     FRequestingControllerAliasID: Word;
     FRequestingControllerNodeID: TNodeID;
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
     function _1WaitForChangeNotify(Sender: TObject; SourceMessage: TLccMessage): Boolean;
     function _2SendAssignReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

     procedure LoadStateArray; override;

     property AssignResult: Byte read FAssignResult write FAssignResult;
     property RequestingControllerNodeID: TNodeID read FRequestingControllerNodeID write FRequestingControllerNodeID;
     property RequestingControllerAliasID: Word read FRequestingControllerAliasID write FRequestingControllerAliasID;
   end;

   { TLccReleaseTrainAction }

   TLccReleaseTrainAction = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
   end;

   { TLccQueryTrainAction }

   TLccQueryTrainAction = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccQueryFunctionTrainReplyAction }

   TLccQueryFunctionTrainReplyAction = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccQuerySpeedTrainReplyAction }

   TLccQuerySpeedTrainReplyAction = class(TLccAction)
   protected
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

   { TLccSetSpeedTrainAction }

   TLccSetSpeedTrainAction = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
   end;

   { TLccSetFunctionTrainAction }

   TLccSetFunctionTrainAction = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
   end;

   { TLccEmergencyStopAction }

   TLccEmergencyStopAction = class(TLccAction)
     function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;  override;
   end;

type
  TLccTrainDirection = (tdForward, tdReverse);
  TLccFunctions = array[0..28] of Word;
  TMessageComPort = procedure(Sender: TObject; var GridConnectStyleMessage: string) of object;

  TAttachedController = record
    NodeID: TNodeID;
    AliasID: Word;
    ReservationNodeID: TNodeID;
    ReservationAliasID: Word;
    AttatchNotifyNodeID: TNodeID;
    AttachNotifyAliasID: Word;
  end;

type

  { TLccTrainCanNode }

  TLccTrainCanNode = class(TLccCanNode)
  private
    FAttachedController: TAttachedController;
    FDccAddress: Word;
    FDccLongAddress: Boolean;
    FFunctions: TLccFunctions;
    FName: string;
    FOnSendMessageComPort: TMessageComPort;
    FReserveWatchDogTimer: TLccTimer;
    FRoadNumber: string;
    FSearchEvent: TEventID;
    FSpeed: THalfFloat;
    FSpeedStep: TLccDccSpeedStep;
    function GetDirection: TLccTrainDirection;
    function GetFunctions(Index: Integer): Word;
    procedure SetDccAddress(AValue: Word);
    procedure SetDccLongAddress(AValue: Boolean);
    procedure SetDirection(AValue: TLccTrainDirection);
    procedure SetFunctions(Index: Integer; AValue: Word);
    procedure SetName(AValue: string);
    procedure SetRoadNumber(AValue: string);
    procedure SetSpeed(AValue: THalfFloat);
    procedure SetSpeedStep(AValue: TLccDccSpeedStep);
  protected
    property AttachedController: TAttachedController read FAttachedController write FAttachedController;
    property ReserveWatchDogTimer: TLccTimer read FReserveWatchDogTimer write FReserveWatchDogTimer;
   private
     FAssignResult: Byte;
     FRequestingControllerAliasID: Word;
     FRequestingControllerNodeID: TNodeID;
   protected
     property AssignResult: Byte read FAssignResult write FAssignResult;
     property RequestingControllerNodeID: TNodeID read FRequestingControllerNodeID write FRequestingControllerNodeID;
     property RequestingControllerAliasID: Word read FRequestingControllerAliasID write FRequestingControllerAliasID;

    procedure ClearAttachedController;
    procedure OnReserveWatchDogTimer(Sender: TObject);

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
    property Name: string read FName write SetName;
    property RoadNumber: string read FRoadNumber write SetRoadNumber;
    property SpeedStep: TLccDccSpeedStep read FSpeedStep write SetSpeedStep;
    property Speed: THalfFloat read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read GetDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;
    property OnSendMessageComPort: TMessageComPort read FOnSendMessageComPort write FOnSendMessageComPort;

    // The Search Event that may have created this Train Node, used as storage for the Command Station to create and wait for Initilization to complete
    property SearchEvent: TEventID read FSearchEvent write FSearchEvent;

    constructor Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string); override;
    destructor Destroy; override;

    function ControllerEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
    function IsReservedBy(SourceMessage: TLccMessage): Boolean;
    function IsReserved: Boolean;
    function ReservationEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
    function ControllerAssigned: Boolean;
    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccTrainCanNodeClass = class of TLccTrainCanNode;

  function SpeedStepToString(SpeedStep: TLccDccSpeedStep; Verbose: Boolean): string;
  function SpeedStepToIndex(SpeedStep: TLccDccSpeedStep): Integer;
  function IndexToSpeedStep(Index: Integer): TLccDccSpeedStep;
  function AddressBooleanToText(IsLong: Boolean; Verbose: Boolean): string;

implementation

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

{ TLccQueryTrainAction }

function TLccQueryTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  AttachedController: TAttachedController;
begin
  Result := False;
  AttachedController := (Owner as TLccTrainCanNode).AttachedController;

  if (AttachedController.NodeID[0] = 0) and (AttachedController.NodeID[1] = 0) and (AttachedController.AliasID = 0) then
   WorkerMessage.LoadTractionConsistQuery(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, NULL_NODE_ID, 0)
  else
   WorkerMessage.LoadTractionConsistQuery(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, AttachedController.NodeID, AttachedController.AliasID);
  SendMessage(Self, WorkerMessage);
end;

{ TLccReleaseTrainAction }

function TLccReleaseTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainCanNode;
begin
  Result := False;

  OwnerTrain := Owner as TLccTrainCanNode;

  if OwnerTrain.ControllerEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
    OwnerTrain.ClearAttachedController;

  UnRegisterSelf;
end;

{ TLccEmergencyStopAction }

function TLccEmergencyStopAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainCanNode;
begin
  Result := False;
  OwnerTrain := Owner as TLccTrainCanNode;
  OwnerTrain.Speed := 0;

  UnRegisterSelf;
end;

{ TLccSetFunctionTrainAction }

function TLccSetFunctionTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainCanNode;
  FunctionAddress: DWORD;
begin
  Result := False;
  OwnerTrain := Owner as TLccTrainCanNode;

  if OwnerTrain.ControllerAssigned then
    if OwnerTrain.ControllerEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
    begin
      FunctionAddress := SourceMessage.TractionExtractFunctionAddress;
      OwnerTrain.Functions[FunctionAddress] := SourceMessage.TractionExtractFunctionValue;
    end;

  UnRegisterSelf;
end;

{ TLccSetSpeedTrainAction }

function TLccSetSpeedTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainCanNode;
begin
  Result := False;
  OwnerTrain := Owner as TLccTrainCanNode;
  if OwnerTrain.ControllerAssigned then
    if OwnerTrain.ControllerEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
      OwnerTrain.Speed := SourceMessage.TractionExtractSetSpeed;

  UnRegisterSelf;
end;

{ TLccQueryFunctionTrainReplyAction }

function TLccQueryFunctionTrainReplyAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  FunctionAddress: DWORD;
begin
  Result := False;
  FunctionAddress := SourceMessage.TractionExtractFunctionAddress;
  WorkerMessage.LoadTractionQueryFunctionReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, FunctionAddress, (Owner as TLccTrainCanNode).Functions[FunctionAddress]);
  SendMessage(Owner, WorkerMessage);

  UnRegisterSelf;
end;

{ TLccQuerySpeedTrainReplyAction }

function TLccQuerySpeedTrainReplyAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainCanNode;
begin
  Result := False;
  OwnerTrain := Owner as TLccTrainCanNode;
  WorkerMessage.LoadTractionQuerySpeedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, OwnerTrain.Speed, 0, HalfNaN, HalfNaN);
  SendMessage(Owner, WorkerMessage);

  UnRegisterSelf;
end;

{ TLccAssignTrainReplyAction }

procedure TLccAssignTrainReplyAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1WaitForChangeNotify;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2SendAssignReply;
end;

function TLccAssignTrainReplyAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  OwnerTrain: TLccTrainCanNode;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  if Assigned(SourceMessage) then // Could be from TimeTick
  begin
    case SourceMessage.MTI of
       MTI_TRACTION_REQUEST :
         begin
           OwnerTrain := Owner as TLccTrainCanNode;

           case SourceMessage.DataArray[0] of
              TRACTION_CONTROLLER_CONFIG :
                begin;
                  case SourceMessage.DataArray[1] of
                    TRACTION_CONTROLLER_CONFIG_ASSIGN :
                      begin
                        // No reservation required
                        RequestingControllerNodeID := SourceMessage.SourceID;
                        RequestingControllerAliasID := SourceMessage.CAN.SourceAlias;

                        if OwnerTrain.ControllerAssigned and not OwnerTrain.ControllerEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
                        begin
                          WorkerMessage.LoadTractionControllerChangingNotify(NodeID, AliasID, OwnerTrain.AttachedController.NodeID, OwnerTrain.AttachedController.AliasID, RequestingControllerNodeID, RequestingControllerAliasID);
                          SendMessage(Owner, WorkerMessage);
                          AdvanceToNextState;
                        end else
                        begin
                          WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, RequestingControllerNodeID, RequestingControllerAliasID, S_OK);
                          SendMessage(Owner, WorkerMessage);
                          AssignResult := S_OK;
                          AdvanceToNextState(2);  // Jump over the Wait for the Assigned Controller to Reply
                          // Nothing to clock the next state so do it manually
                          _2SendAssignReply(Sender, SourceMessage);
                        end;
                      end;
                   end
                end;
           end;
         end;
    end;
  end else
    _2SendAssignReply(nil, nil);   // Something is wrong exit the statemachine

  SetTimoutCountThreshold(TIMEOUT_CONTROLLER_NOTIFY_WAIT, True);
end;

function TLccAssignTrainReplyAction._1WaitForChangeNotify(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  if Assigned(SourceMessage) then   // Could be time tick calling
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
                     AssignResult := S_OK;
                     case SourceMessage.DataArray[2] of
                       TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER : AssignResult := TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER;
                       // TODO :  What would cause a TRAIN REFUSED?
                     end;
                     AdvanceToNextState;
                     _2SendAssignReply(Sender, SourceMessage);   // Nothing to clock the next state so do it manually
                   end;
               end;
             end;
           TRACTION_MANAGE :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE : begin
                    WorkerMessage.LoadTractionManageReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True);
                    SendMessage(Self, WorkerMessage)
                end;
                TRACTION_MANAGE_RELEASE : begin end;
              end
            end;
         end;
       end;
     end;
  end;

  if TimeoutExpired then
  begin
    {$IFNDEF DISABLE_STATE_TIMEOUTS_FOR_DEBUG}
    AssignResult := S_OK;  // No answer, must be ok
    AdvanceToNextState;
    // Nothing to clock the next state so do it manually
    _2SendAssignReply(Sender, WorkerMessage);
    {$ENDIF}
  end;
end;

function TLccAssignTrainReplyAction._2SendAssignReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  if AssignResult = S_OK then
  begin
    (Owner as TLccTrainCanNode).ClearAttachedController;
    (Owner as TLccTrainCanNode).FAttachedController.NodeID := RequestingControllerNodeID;
    (Owner as TLccTrainCanNode).FAttachedController.AliasID := RequestingControllerAliasID;
  end;
  WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, RequestingControllerNodeID, RequestingControllerAliasID, AssignResult);
  SendMessage(Owner, WorkerMessage);


  UnRegisterSelf;
end;

{ TLccTrainCanNode }

constructor TLccTrainCanNode.Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string);
begin
  inherited Create(ASendMessageFunc, ANodeManager, CdiXML);
end;

procedure TLccTrainCanNode.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;
  ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := True;
  ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := True;
  ProtocolSupportedProtocols.TractionFunctionConfiguration := True;

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
end;

function TLccTrainCanNode.EncodeFunctionValuesDccStyle: DWORD;
var
  i: Integer;
begin
  Result := 0;
  for i := MAX_DCC_FUNCTIONS - 1 downto 0 do
  begin
    if Functions[i] > 0 then
      Result := Result or $00000001;
    Result := Result shl 1
  end;
end;

function TLccTrainCanNode.EncodeToDccGridConnect(DccPacket: TDCCPacket): String;
var
  i: Integer;
begin
  Result := ':R' + IntToHex(DccPacket.Flags, 8) +  'N';   // make it the same size header as OpenLCB
  for i := 0 to DccPacket.Flags - 1 do
    Result := Result + IntToHex(DccPacket.PacketBytes[i], 2);
  Result := Result + ';';
end;

procedure TLccTrainCanNode.DoSendMessageComPort(GridConnectString: string);
begin
  if Assigned(OnSendMessageComPort) then
    OnSendMessageComPort(Self, GridConnectString);
end;

function TLccTrainCanNode.DccFunctionHandler(DccAddress: Word;
  LongAddress: Boolean; FunctionAddress: DWORD; AllDccFunctionBitsEncoded: DWORD
  ): TDCCPacket;
var
  FunctionMask, FunctionExtendedCode: Byte;
  AddressHi, AddressLo: Byte;
begin
  Result.Flags := 0;

  // Split the address to make clear when loading bytes
  AddressHi := (DccAddress shr 8) and $00FF;
  if LongAddress then
    AddressHi := AddressHi or NMRA_LONGADDRESS_MASK_BYTE;
  AddressLo := DccAddress and $00FF;

  if FunctionAddress < 29 then
  begin
    if FunctionAddress < 5 then
    begin
      FunctionMask := (AllDccFunctionBitsEncoded shr 1) and $0F;
      if AllDccFunctionBitsEncoded and $00000001 = 0 then
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
    end
  end else
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
end;

function TLccTrainCanNode.DccSpeedDirHandler(DccAddress: Word;
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

destructor TLccTrainCanNode.Destroy;
begin
  inherited Destroy;
end;

procedure TLccTrainCanNode.DccLoadPacket(var NewMessage: TDCCPacket; Data1,
  Data2, Data3, Data4, Data5, ValidDataByes: Byte);
begin
  NewMessage.PacketBytes[0] := Data1;
  NewMessage.PacketBytes[1] := Data2;
  NewMessage.PacketBytes[2] := Data3;
  NewMessage.PacketBytes[3] := Data4;
  NewMessage.PacketBytes[4] := Data5;
  NewMessage.Flags := ValidDataByes;
end;

procedure TLccTrainCanNode.ClearAttachedController;
begin
  FAttachedController.NodeID := NULL_NODE_ID;
  FAttachedController.AliasID := 0;
  FAttachedController.NodeID := NULL_NODE_ID;
  FAttachedController.ReservationAliasID := 0;
  FAttachedController.AttatchNotifyNodeID := NULL_NODE_ID;
  FAttachedController.AttachNotifyAliasID := 0;
end;

function TLccTrainCanNode.ControllerAssigned: Boolean;
begin
  Result := ((AttachedController.NodeID[0] <> 0) and (AttachedController.NodeID[1] <> 0) or (AttachedController.AliasID <> 0))
end;

function TLccTrainCanNode.ControllerEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
begin
  Result := (((AttachedController.NodeID[0] = ATestNodeID[0]) and (AttachedController.NodeID[1] = ATestNodeID[1])) and (ATestAlias = AttachedController.AliasID))
end;

function TLccTrainCanNode.GetCdiFile: string;
begin
  Result := CDI_XML_TRAIN_NODE
end;

function TLccTrainCanNode.GetDirection: TLccTrainDirection;
begin
  if HalfIsNegative(Speed) then
    Result := tdReverse
  else
    Result := tdForward;
end;

function TLccTrainCanNode.GetFunctions(Index: Integer): Word;
begin
  if (Index >= 0) and (Index < Length(FFunctions)) then
    Result := FFunctions[Index]
  else
    Result := $FFFF
end;

function TLccTrainCanNode.IsReserved: Boolean;
begin
  Result := (AttachedController.ReservationNodeID[0] <> 0) or (AttachedController.ReservationNodeID[1] <> 0) or (AttachedController.ReservationAliasID <> 0);
end;

function TLccTrainCanNode.IsReservedBy(SourceMessage: TLccMessage): Boolean;
begin
  Result := EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, AttachedController.ReservationNodeID, AttachedController.ReservationAliasID);
end;

procedure TLccTrainCanNode.OnReserveWatchDogTimer(Sender: TObject);
begin
  ReserveWatchDogTimer.Enabled := False;
  FAttachedController.ReservationAliasID := 0;
  FAttachedController.ReservationNodeID := NULL_NODE_ID;
end;

function TLccTrainCanNode.ProcessMessage(SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited ProcessMessage(SourceMessage);

  // We only are dealing with messages with destinations for us from here on
  if SourceMessage.HasDestination then
  begin
    if not IsDestinationEqual(SourceMessage) then
      Exit;
  end;

  case SourceMessage.MTI of
    MTI_TRACTION_REQUEST :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_SPEED_DIR       : Result := LccActions.RegisterAction(Self, SourceMessage, TLccSetSpeedTrainAction.Create(Self, NodeID, AliasID));
          TRACTION_FUNCTION        : Result := LccActions.RegisterAction(Self, SourceMessage, TLccSetFunctionTrainAction.Create(Self, NodeID, AliasID));
          TRACTION_E_STOP          : Result := LccActions.RegisterAction(Self, SourceMessage, TLccEmergencyStopAction.Create(Self, NodeID, AliasID));
          TRACTION_QUERY_SPEED     : Result := LccActions.RegisterAction(Self, SourceMessage, TLccQuerySpeedTrainReplyAction.Create(Self, NodeID, AliasID));
          TRACTION_QUERY_FUNCTION  : Result := LccActions.RegisterAction(Self, SourceMessage, TLccQueryFunctionTrainReplyAction.Create(Self, NodeID, AliasID));
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN  : Result := LccActions.RegisterAction(Self, SourceMessage, TLccAssignTrainReplyAction.Create(Self, NodeID, AliasID));
                TRACTION_CONTROLLER_CONFIG_RELEASE : Result := LccActions.RegisterAction(Self, SourceMessage, TLccReleaseTrainAction.Create(Self, NodeID, AliasID));
                TRACTION_CONTROLLER_CONFIG_QUERY   : Result := LccActions.RegisterAction(Self, SourceMessage, TLccQueryTrainAction.Create(Self, NodeID, AliasID));
               end
            end;
          TRACTION_LISTENER :
            begin
              if IsReservedBy(SourceMessage) then
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_LISTENER_ATTACH : begin end;
                  TRACTION_LISTENER_DETACH : begin end;
                  TRACTION_LISTENER_QUERY  : begin end;
                end;
              end
            end;
          TRACTION_MANAGE :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE : begin WorkerMessage.LoadTractionManageReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True); SendMessageFunc(Self, WorkerMessage) end;
                TRACTION_MANAGE_RELEASE : begin end;
              end
            end;
        end;
      end;
  end;
end;

function TLccTrainCanNode.ReservationEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
begin
  Result := ((AttachedController.ReservationNodeID[0] = ATestNodeID[0]) and (AttachedController.ReservationNodeID[1] = ATestNodeID[1])) or (ATestAlias = AttachedController.ReservationAliasID)
end;

procedure TLccTrainCanNode.SetDccAddress(AValue: Word);
begin
  if FDccAddress = AValue then Exit;
  FDccAddress := AValue;
end;

procedure TLccTrainCanNode.SetDccLongAddress(AValue: Boolean);
begin
  if FDccLongAddress = AValue then Exit;
  FDccLongAddress := AValue;
end;

procedure TLccTrainCanNode.SetDirection(AValue: TLccTrainDirection);
begin
  if AValue = tdReverse then
    Speed := FSpeed or $8000
  else
    Speed := FSpeed and $7FFF;
end;

procedure TLccTrainCanNode.SetFunctions(Index: Integer; AValue: Word);
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

procedure TLccTrainCanNode.SetName(AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TLccTrainCanNode.SetRoadNumber(AValue: string);
begin
  if FRoadNumber = AValue then Exit;
  FRoadNumber := AValue;
end;

procedure TLccTrainCanNode.SetSpeed(AValue: THalfFloat);
var
  DccPacket: TDCCPacket;
  DccGridConnect: string;
begin
  if AValue = Speed then Exit;
  FSpeed := AValue;
  DccPacket := DccSpeedDirHandler(DccAddress, DccLongAddress, Speed, SpeedStep);
  DccGridConnect := EncodeToDccGridConnect( DccPacket);
  DoSendMessageComPort(DccGridConnect);
end;

procedure TLccTrainCanNode.SetSpeedStep(AValue: TLccDccSpeedStep);
begin
  if FSpeedStep = AValue then Exit;
  FSpeedStep := AValue;
end;

end.

