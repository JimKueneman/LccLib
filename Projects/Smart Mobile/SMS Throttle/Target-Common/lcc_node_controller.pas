unit lcc_node_controller;
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
  lcc_node_messages,
  lcc_math_float16,
  lcc_node,
  lcc_node_train,
  lcc_utilities;

const
  CDI_XML_CONTROLLER: string = (
  '<?xml version="1.0" encoding="utf-8"?>'+
  '<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
  '<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
         '<identification>'+
       	  '<manufacturer>Mustangpeak</manufacturer>'+
       	  '<model>TCN1000</model>'+
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

  TControllerTrainAssignResult = (tarAssigned, tarFailTrainRefused, tarFailControllerRefused);

  TLccActionSearchAndAssignTrain = class;

  { TDccSearchCriteria }

  TDccSearchCriteria = class
  private
    FAddress: Word;
    FLongAddress: Boolean;
    FSearchStr: string;       // Set Address to 0 to use the Search string
    FSpeedStep: TLccDccSpeedStep;
  public
    constructor Create(ASearchString: string; AnAddress: Word; ASpeedStep: TLccDccSpeedStep; IsLongAddress: Boolean);
    property Address: Word read FAddress write FAddress;
    property SearchStr: string read FSearchStr write FSearchStr;
    property SpeedStep: TLccDccSpeedStep read FSpeedStep write FSpeedStep;
    property LongAddress: Boolean read FLongAddress write FLongAddress;

    function Clone: TDccSearchCriteria;
  end;

  { TLccEncodedSearchCriteria }

  TLccEncodedSearchCriteria = class
  private
    FCriteria: DWORD;
  public
    property Criteria: DWORD read FCriteria write FCriteria;

    constructor Create(ACriteria: DWORD);
  end;

  { TLccActionSearchGatherAndSelectTrain }

  // Trains property contains the selected train when the action completes
  TLccActionSearchGatherAndSelectTrain = class(TLccActionTrain)
  private
    FSearchCriteria: DWord;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionGatherSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionReportSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  public
    property SearchCriteria: DWord read FSearchCriteria write FSearchCriteria;
  end;

  { TLccActionSearchGatherAndSelectTrains }

  // Trains property contains the selected trains when the action completes
  // Calls TLccActionSearchGatherAndSelectTrain for all items in the SearchCriteria property
  TLccActionSearchGatherAndSelectTrains = class(TLccActionTrain)
  private
    FSearchCriteria: TObjectList;
    FSearchCriteriaIndex: Integer;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionGatherSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionWaitForSpawnedAction(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _3ActionReportSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
    procedure CompleteCallback(SourceAction: TLccAction); override;

    property SearchCriteriaIndex: Integer read FSearchCriteriaIndex write FSearchCriteriaIndex;
  public
    property SearchCriteria: TObjectList read FSearchCriteria write FSearchCriteria;  // TLccEncodedSearchCriteria objects

    constructor Create(AnOwner: TLccNode; ASourceNodeID: TNodeID; ASourceAliasID: Word; ADestNodeID: TNodeID; ADestAliasID: Word); override;
    destructor Destroy; override;
  end;

  { TLccActionAssignTrain }

  // Sends an Assign message and waits for a result
  TLccActionAssignTrain = class(TLccActionTrain)
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  end;

  { TLccActionSearchAndAssignTrain }

  // Combines Search and Assign Actions above
  TLccActionSearchAndAssignTrain = class(TLccActionTrain)
  private
    FSearchCriteria: DWORD;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWait(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure CompleteCallback(SourceAction: TLccAction); override;
    procedure LoadStateArray; override;
  public
    property SearchCriteria: DWORD read FSearchCriteria write FSearchCriteria;
  end;

  { TLccActionTractionReleaseTrain }

  TLccActionTractionReleaseTrain = class(TLccActionTrain)
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;

    procedure LoadStateArray; override;
  end;

  { TLccActionTractionQuerySpeed }

  // Queries the Train Node for Speed and waits for the reply
  TLccActionTractionQuerySpeed = class(TLccActionTrain)
  private
    FSpeed: THalfFloat;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForQuerySpeedResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  public
    property Speed: THalfFloat read FSpeed;
  end;

  { TLccActionTractionQueryFunction }

  TLccActionTractionQueryFunction = class(TLccActionTrain)
  private
    FAddress: DWORD;
    FValue: Word;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForQueryFunctionResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  public
    property Address: DWORD read FAddress write FAddress;
    property Value: Word read FValue;
  end;

  { TLccActionTractionListenerAttach }

  TLccActionTractionListenerAttach = class(TLccActionTrain)
  private
    FFlags: Byte;
    FListenerNodeID: TNodeID;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  public
    property ListenerNodeID: TNodeID read FListenerNodeID write FListenerNodeID;
    property Flags: Byte read FFlags write FFlags;
  end;

  { TLccActionTractionListenerDetach }

  TLccActionTractionListenerDetach = class(TLccActionTrain)
  private
    FFlags: Byte;
      FListenerNodeID: TNodeID;
    protected
      function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
      function _1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

      procedure LoadStateArray; override;
    public
      property ListenerNodeID: TNodeID read FListenerNodeID write FListenerNodeID;
      property Flags: Byte read FFlags write FFlags;
    end;

  { TLccActionTractionListenerQuery }

  TLccActionTractionListenerQuery = class(TLccActionTrain)

  end;


type

  TOnControllerSearchResult = procedure(Sender: TLccActionTrain; TrainList: TLccActionTrainList; var SelectedResultIndex: Integer) of object;
  TOnControllerSearchMultiResult = procedure(Sender: TLccActionTrain; Trains: TLccActionTrainList) of object;
  TOnControllerTrainAssignedReply = procedure(Sender: TLccNode; Reason: TControllerTrainAssignResult) of object;
  TOnControllerTrainReleasedReply = procedure(Sender: TLccNode) of object;
  TOnControllerQuerySpeedReply = procedure(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte) of object;
  TOnControllerQueryFunctionReply = procedure(Sender: TLccNode; Address: DWORD; Value: Word) of object;
  TOnControllerRequestTakeover = procedure(Sender: TLccNode; var Allow: Boolean) of object;
  TOnControllerAttachListenerReply = procedure(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word) of object;
  TOnControllerDetachListenerReply = procedure(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word) of object;
  TOnControllerQueryListenerGetCount = procedure(Sender: TLccNode; ListenerCount: Byte) of object;
  TOnControllerQueryListenerIndex = procedure(Sender: TLccNode; ListenerIndex: Byte; ListenerFlags: Byte; ListenerNodeID: TNodeID) of object;

  // ******************************************************************************

  TAttachedTrainReservationState = (trsNotReserved, trsReserving, trsReserved);
  TAttachedTrainAssignmentState = (tasNotAssigned, tasAssigning, tasAssigned, tasUnAssigning);
  TAttachedTrainSearchState = (tssNotSearching, tssSearching);

  // WARNING: For SMS these are zeroed manually in the Clear method.   If you add
  // more data, make sure it is cleared in the method for SMS
  TAttachedTrain = record
    NodeID: TNodeID;
    AliasID: Word;
    RequestedSearchData,
    RepliedSearchData: DWORD;
    SearchString: string;
    Listeners: array of TAttachedTrain
  end;

  { TLccTrainController }

  TLccTrainController = class(TLccNode)
  private
    FAssignedTrain: TAttachedTrain;

    FDirection: TLccTrainDirection;
    FFunctionArray: TLccFunctions;
    FOnAttachListenerReply: TOnControllerAttachListenerReply;
    FOnControllerRequestTakeover: TOnControllerRequestTakeover;
    FOnDetachListenerReply: TOnControllerDetachListenerReply;
    FOnQueryFunctionReply: TOnControllerQueryFunctionReply;
    FOnQueryListenerGetCount: TOnControllerQueryListenerGetCount;
    FOnQueryListenerIndex: TOnControllerQueryListenerIndex;
    FOnQuerySpeedReply: TOnControllerQuerySpeedReply;
    FOnSearchMultiResult: TOnControllerSearchMultiResult;
    FOnSearchResult: TOnControllerSearchResult;
    FOnTrainAssigned: TOnControllerTrainAssignedReply;
    FOnTrainReleased: TOnControllerTrainReleasedReply;
    FSpeed: single;
    function GetFunctions(Index: Integer): Word;
    procedure SetDirection(AValue: TLccTrainDirection);
    procedure SetFunctions(Index: Integer; AValue: Word);
    procedure SetSpeed(AValue: single);

  protected
    property FunctionArray: TLccFunctions read FFunctionArray write FFunctionArray;

    procedure ClearAssignedTrain;
    function GetCdiFile: string; override;
    procedure BeforeLogin; override;
    procedure DoTrainAssigned(Reason: TControllerTrainAssignResult); virtual;
    procedure DoTrainReleased; virtual;
    procedure DoQuerySpeedReply(ASetSpeed, ACommandSpeed, AnActualSpeed: THalfFloat; Status: Byte); virtual;
    procedure DoQueryFunctionReply(Address: DWORD; Value: Word); virtual;
    procedure DoControllerTakeOver(var Allow: Boolean); virtual;
    procedure DoSearchResult(Sender: TLccActionTrain; TrainList: TLccActionTrainList; var SelectedResultIndex: Integer); virtual;
    procedure DoSearchMultiResult(Sender: TLccActionTrain; Trains: TLccActionTrainList); virtual;
    procedure DoControllerAttachListenerReply(ListenerNodeID: TNodeID; ReplyCode: Byte); virtual;
    procedure DoControllerDetachListenerReply(ListenerNodeID: TNodeID; ReplyCode: Byte); virtual;
    procedure DoControllerQueryListenerGetCount(ListenerCount: Byte); virtual;
    procedure DoControllerQueryListenerIndex(ListenerIndex: Byte; ListenerFlags: Byte; ListenerNodeID: TNodeID); virtual;

  public
    property AssignedTrain: TAttachedTrain read FAssignedTrain write FAssignedTrain;
    property Speed: single read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;

    property OnTrainAssigned: TOnControllerTrainAssignedReply read FOnTrainAssigned write FOnTrainAssigned;
    property OnTrainReleased: TOnControllerTrainReleasedReply read FOnTrainReleased write FOnTrainReleased;
    property OnQuerySpeedReply: TOnControllerQuerySpeedReply read FOnQuerySpeedReply write FOnQuerySpeedReply;
    property OnQueryFunctionReply: TOnControllerQueryFunctionReply read FOnQueryFunctionReply write FOnQueryFunctionReply;
    property OnControllerRequestTakeover: TOnControllerRequestTakeover read FOnControllerRequestTakeover write FOnControllerRequestTakeover;
    property OnSearchResult: TOnControllerSearchResult read FOnSearchResult write FOnSearchResult;
    property OnSearchMultiResult: TOnControllerSearchMultiResult read FOnSearchMultiResult write FOnSearchMultiResult;
    property OnAttachListenerReply: TOnControllerAttachListenerReply read FOnAttachListenerReply write FOnAttachListenerReply;
    property OnDetachListenerReply: TOnControllerDetachListenerReply read FOnDetachListenerReply write FOnDetachListenerReply;
    property OnQueryListenerGetCount: TOnControllerQueryListenerGetCount read FOnQueryListenerGetCount write FOnQueryListenerGetCount;
    property OnQueryListenerIndex: TOnControllerQueryListenerIndex read FOnQueryListenerIndex write FOnQueryListenerIndex;

    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
    procedure AttachListener(ATrainNodeID: TNodeID; ATrainNodeAliasID: Word; AListenerNodeID: TNodeID);
    procedure DetachListener(ATrainNodeID: TNodeID; ATrainNodeAliasID: Word; AListenerNodeID: TNodeID);
    procedure QueryListeners(ATrainNodeID: TNodeID);
    procedure ReleaseTrain;
    procedure QuerySpeed;
    procedure QueryFunction(Address: Word);
    procedure QueryFunctions;
    procedure SearchTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure SearchTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure SearchTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
    procedure SearchTrainsByDccAddress(TrainCriteria: TObjectList);  // Pass TDccSearchCriteria objects
    procedure EmergencyStop;
    function IsTrainAssigned: Boolean;

    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TLccEncodedSearchCriteria }

constructor TLccEncodedSearchCriteria.Create(ACriteria: DWORD);
begin
  FCriteria := ACriteria;
end;

{ TDccSearchCriteria }

constructor TDccSearchCriteria.Create(ASearchString: string; AnAddress: Word;
  ASpeedStep: TLccDccSpeedStep; IsLongAddress: Boolean);
begin
  FSpeedStep := ASpeedStep;
  FAddress := AnAddress;
  FLongAddress := IsLongAddress;
  FSearchStr := ASearchString;
end;

function TDccSearchCriteria.Clone: TDccSearchCriteria;
begin
  Result := TDccSearchCriteria.Create(SearchStr, Address, SpeedStep, LongAddress);
end;

{ TLccActionSearchGatherAndSelectTrains }

function TLccActionSearchGatherAndSelectTrains._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);
  AdvanceToNextState;
end;

function TLccActionSearchGatherAndSelectTrains._1ActionGatherSearchResults( Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  LccSearchTrainAction: TLccActionSearchGatherAndSelectTrain;
begin
  Result := False;
  // this is called recursivly until the Callback detects we are done with all the search items and it moves us out of this state
  // Spawn a child task and wait for it to end in the next state
  LccSearchTrainAction := TLccActionSearchGatherAndSelectTrain.Create(Owner, SourceNodeID, SourceAliasID, NULL_NODE_ID, 0);
  LccSearchTrainAction.SearchCriteria := (SearchCriteria[SearchCriteriaIndex] as TLccEncodedSearchCriteria).Criteria;
  LccSearchTrainAction.RegisterCallBack(Self);
  Owner.LccActions.RegisterAndKickOffAction(LccSearchTrainAction, nil);
  AdvanceToNextState;
end;

function TLccActionSearchGatherAndSelectTrains._2ActionWaitForSpawnedAction(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  // We just spin here waiting for each search task to complete.  We are moved on in the Callback when ready
end;

function TLccActionSearchGatherAndSelectTrains._3ActionReportSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := False;
  if ErrorCode = laecOk then
  begin
    // Report the results
    IgnoreTimer := True;  // Stop reentrancy
    try
      ControllerNode := Owner as TLccTrainController;
      ControllerNode.DoSearchMultiResult(Self, Trains);
    finally
      AdvanceToNextState;
      IgnoreTimer := False;
    end;
  end else
    AdvanceToNextState;  // Error, just finish up
end;

procedure TLccActionSearchGatherAndSelectTrains.LoadStateArray;
begin
  SetStateArrayLength(5);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionGatherSearchResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2ActionWaitForSpawnedAction;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_3ActionReportSearchResults;
  States[4] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

constructor TLccActionSearchGatherAndSelectTrains.Create(AnOwner: TLccNode;
  ASourceNodeID: TNodeID; ASourceAliasID: Word; ADestNodeID: TNodeID;
  ADestAliasID: Word);
begin
  inherited Create(AnOwner, ASourceNodeID, ASourceAliasID, ADestNodeID, ADestAliasID);
  FSearchCriteria := TObjectList.Create;
  {$IFNDEF GWSCRIPT}
  SearchCriteria.OwnsObjects := True;
  {$ENDIF}
  SearchCriteriaIndex := 0;
end;

procedure TLccActionSearchGatherAndSelectTrains.CompleteCallback(SourceAction: TLccAction);
var
  SpawnedSearchAction: TLccActionSearchGatherAndSelectTrain;
begin
  SpawnedSearchAction := SourceAction as TLccActionSearchGatherAndSelectTrain;

  if SpawnedSearchAction.ErrorCode = laecOk then
  begin
    if SpawnedSearchAction.Trains.Count = 1 then
      Trains.Add(SpawnedSearchAction.Trains[0].Clone);

    // Move to the next Criteria in our list
    Inc(FSearchCriteriaIndex);
    // if we are done move on else backup one to create a new search task
    if SearchCriteriaIndex < SearchCriteria.Count then
      AdvanceToNextState(-1)
    else
      AdvanceToNextState;
  end else
  begin
    // The task had and error code so just report it back to the originator of this
    // instance and exit.
    Trains.Clear;
    ErrorCode := SpawnedSearchAction.ErrorCode;
    AdvanceToNextState;
  end;
end;

destructor TLccActionSearchGatherAndSelectTrains.Destroy;
begin
  FreeAndNil(FSearchCriteria);
  inherited Destroy;
end;

{ TLccActionAssignTrain }

function TLccActionAssignTrain._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  Assert(Trains.Count > 0, 'TLccActionAssignTrain: Must have a train in the Trains Property to Assign');

  //  Attept to assigne the controller to the Train stored in the Trains property
  if Trains.Count > 0 then
  begin
    WorkerMessage.LoadTractionControllerAssign(SourceNodeID, SourceAliasID, Trains[0].NodeID, Trains[0].AliasID, SourceNodeID);
    SendMessage(Owner, WorkerMessage);
    SetTimoutCountThreshold(TIMEOUT_CONTROLLER_NOTIFY_WAIT * 2); // seconds to assign the train, the command station will give 5 seconds to receive a reply from an existing controller to give it up
  end;

  AdvanceToNextState;
end;

function TLccActionAssignTrain._1ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := False;

  // SourceMessage could be nil from a timer
  // This code assumes that index Trains[0] is the selected train
  if Assigned(SourceMessage) and (Trains.Count > 0) then
  begin // Only care if it is coming from our potental Train
    if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, Trains[0].NodeID, Trains[0].AliasID, True) then
    begin
      case SourceMessage.MTI of
         MTI_TRACTION_REPLY :
           begin
             case SourceMessage.DataArray[0] of
               TRACTION_CONTROLLER_CONFIG :
                 begin
                   case SourceMessage.DataArray[1] of
                     TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY :
                       begin
                         IgnoreTimer := True; // keep the timer from being reentrant during event calls with blocking code (dialogs)
                         try
                           case SourceMessage.DataArray[2] of
                             TRACTION_CONTROLLER_CONFIG_REPLY_OK :
                               begin
                                 ControllerNode := Owner as TLccTrainController;
                                  if Assigned(ControllerNode) then
                                  begin
                                    ControllerNode.FAssignedTrain.NodeID := DestNodeID;
                                    ControllerNode.FAssignedTrain.AliasID := DestAliasID;
                                    ControllerNode.FAssignedTrain.RepliedSearchData := Trains[0].SearchCriteria;
                                    (Owner as TLccTrainController).DoTrainAssigned(tarAssigned)
                                 end;
                               end;
                             TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER : (Owner as TLccTrainController).DoTrainAssigned(tarFailTrainRefused);
                             TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN               : (Owner as TLccTrainController).DoTrainAssigned(tarFailControllerRefused);
                           end;
                         finally
                           AdvanceToNextState;
                           IgnoreTimer := False;
                         end
                       end;
                   end;
                 end;
             end;
           end;
      end;
    end;
  end;

  if TimeoutExpired then
  begin
    // Just in case, can't hurt
    WorkerMessage.LoadTractionControllerRelease(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, SourceNodeID, SourceAliasID);
    SendMessage(Owner, WorkerMessage);
    ErrorCode := laecTimedOut;
    AdvanceToNextState;
  end;
end;

procedure TLccActionAssignTrain.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForAssignThrottleResult;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccActionSearchGatherAndSelectTrain }

function TLccActionSearchGatherAndSelectTrain._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  // Send the search message
  WorkerMessage.LoadTractionSearch(SourceNodeID, SourceAliasID, SearchCriteria);
  SendMessage(Owner, WorkerMessage);

  SetTimoutCountThreshold(Round(2000)); // milliseconds to allow trains to resspond; If the train node needs to be created then it can take >750ms
  AdvanceToNextState;
end;

function TLccActionSearchGatherAndSelectTrain._1ActionGatherSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  TrainVersion: Byte;
  TrainRoadName,
  TrainClass,
  TrainRoadNumber,
  TrainName,
  TrainManufacturer,
  TrainOwner: string;
  LocalTrain: TLccActionTrainInfo;
begin
  Result := False;

  // SourceMessagfe could be nil from a timer
  if Assigned(SourceMessage) then
  begin
    case SourceMessage.MTI of
       MTI_PRODUCER_IDENTIFIED_CLEAR,
       MTI_PRODUCER_IDENTIFIED_SET,
       MTI_PRODUCER_IDENTIFIED_UNKNOWN :
         begin
           if SourceMessage.TractionSearchIsEvent and (SourceMessage.TractionSearchExtractSearchData = SearchCriteria) then
           begin
             LocalTrain := Trains.CreateNew(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
             if Assigned(LocalTrain) then
             begin
               LocalTrain.SearchCriteria := SourceMessage.TractionSearchExtractSearchData;
               LocalTrain.SearchCriteriaFound := True;
               LocalTrain.SNIP_Valid := False;
               // Send a message back to the Train Node from this controller asking for the STNIP
               WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(SourceNodeID, SourceAliasID, LocalTrain.NodeID, LocalTrain.AliasID);
               SendMessage(Owner, WorkerMessage);
               SetTimoutCountThreshold(Round(500)); // milliseconds to allow trains to resspond to the STNIP
             end
           end
        end;
       MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY :
         begin
            // find the right Node that this SNIP belongs to
            // Expectation is that we recieved the producer identified first because we asked for this above after the slot has been created
            LocalTrain := Trains.MatchingNodeAndSearchCriteria(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
            if Assigned(LocalTrain) then
            begin
              // all this claptrap for SMS and var parameters....
              TrainVersion := 0;
              TrainRoadName := '';
              TrainClass := '';
              TrainRoadNumber := '';
              TrainName := '';
              TrainManufacturer := '';
              TrainOwner := '';
              SourceMessage.ExtractSimpleTrainNodeIdentInfoReply(TrainVersion, TrainRoadName, TrainClass, TrainRoadNumber, TrainName, TrainManufacturer, TrainOwner);
              LocalTrain.SNIP_Manufacturer := TrainManufacturer;
              LocalTrain.SNIP_Owner := TrainOwner;
              LocalTrain.SNIP_Roadname := TrainRoadName;
              LocalTrain.SNIP_RoadNumber := TrainRoadNumber;
              LocalTrain.SNIP_TrainClass := TrainClass;
              LocalTrain.SNIP_TrainName := TrainName;
              LocalTrain.SNIP_Version := TrainVersion;
              LocalTrain.SNIP_Valid := True;
            end;
         end;
    end;
  end;

  if TimeoutExpired then    // Times up, report out....
  begin
    AdvanceToNextState;  // Timeout is NOT an error here, it is normal
  end;
end;

function TLccActionSearchGatherAndSelectTrain._2ActionReportSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
  iSelectedTrain: Integer;
  SelectedTrain: TLccActionTrainInfo;
begin
  Result := False;

  IgnoreTimer := True;  // Stop reentrancy
  try
    ControllerNode := Owner as TLccTrainController;

    if Trains.Count > 0 then
      iSelectedTrain := 0
    else
      iSelectedTrain := -1;

      // Let the handler deal with no trains as an error code
      ControllerNode.DoSearchResult(Self, Trains, iSelectedTrain);

      // If there are no trains don't do anything
      if Trains.Count > 0 then
      begin
        // Validate what the program returned for an index
        if (iSelectedTrain < 0) or (iSelectedTrain >= Trains.Count) then
          iSelectedTrain := 0;

        // Only have the selected train in the Trains property
        SelectedTrain := Trains[iSelectedTrain];
        Trains.Remove(SelectedTrain);
        Trains.Clear;
        Trains.Add(SelectedTrain);
      end else
        ErrorCode := laecNoTrainFound; // set a code for no train found
  finally
    AdvanceToNextState;
    IgnoreTimer := False;
  end;
end;

procedure TLccActionSearchGatherAndSelectTrain.LoadStateArray;
begin
  SetStateArrayLength(4);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionGatherSearchResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2ActionReportSearchResults;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;


{ TLccActionTractionListenerDetach }

function TLccActionTractionListenerDetach._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionListenerDetach(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, Flags, ListenerNodeID);
  SendMessage(Self, WorkerMessage);
  SetTimoutCountThreshold(1000);
  AdvanceToNextState;
end;

function TLccActionTractionListenerDetach._1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
  TempNodeID: TNodeID;
  ReplyCode: Word;
begin
  Result := False;

  ControllerNode := Owner as TLccTrainController;

  if EqualNode(SourceMessage.DestID, SourceMessage.CAN.DestAlias, DestNodeID, DestAliasID, True) then
  begin
    case SourceMessage.MTI of
       MTI_TRACTION_REPLY :
         begin
           case SourceMessage.DataArray[0] of
             TRACTION_LISTENER :
                 begin
                   case SourceMessage.DataArray[1] of
                     TRACTION_LISTENER_ATTACH :
                       begin
                         IgnoreTimer := True;
                         try
                           TempNodeID := NULL_NODE_ID;
                           SourceMessage.ExtractDataBytesAsNodeID(2, TempNodeID);
                           ReplyCode := SourceMessage.ExtractDataBytesAsWord(8);
                           ControllerNode.DoControllerDetachListenerReply(TempNodeID, ReplyCode);
                         finally
                           AdvanceToNextState;
                           IgnoreTimer := False;;
                         end;
                       end;
                   end;

                 end;
           end;
         end;
    end;
  end;

  if TimeoutExpired then
  begin
    ErrorCode := laecTimedOut;
    AdvanceToNextState;
  end;
end;

procedure TLccActionTractionListenerDetach.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1WaitforReply;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccActionTractionListenerAttach }

function TLccActionTractionListenerAttach._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionListenerAttach(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, Flags, ListenerNodeID);
  SendMessage(Self, WorkerMessage);
  SetTimoutCountThreshold(2000);
  AdvanceToNextState;
end;

function TLccActionTractionListenerAttach._1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
  TempNodeID: TNodeID;
  ReplyCode: Word;
begin
  Result := False;

  ControllerNode := Owner as TLccTrainController;

  if EqualNode(SourceMessage.DestID, SourceMessage.CAN.DestAlias, DestNodeID, DestAliasID, True) then
  begin
    case SourceMessage.MTI of
       MTI_TRACTION_REPLY :
         begin
           case SourceMessage.DataArray[0] of
             TRACTION_LISTENER :
                 begin
                   case SourceMessage.DataArray[1] of
                     TRACTION_LISTENER_ATTACH :
                       begin
                         IgnoreTimer := True;
                         try
                           TempNodeID := NULL_NODE_ID;
                           SourceMessage.ExtractDataBytesAsNodeID(2, TempNodeID);
                           ReplyCode := SourceMessage.ExtractDataBytesAsWord(8);
                           ControllerNode.DoControllerAttachListenerReply(TempNodeID, ReplyCode);
                         finally
                           AdvanceToNextState;
                           IgnoreTimer := False;
                         end;
                       end;
                   end;

                 end;
           end;
         end;
    end;
  end;

  if TimeoutExpired then
  begin
    ErrorCode := laecTimedOut;
    AdvanceToNextState;
  end;
end;

procedure TLccActionTractionListenerAttach.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1WaitforReply;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccActionTractionReleaseTrain }

function TLccActionTractionReleaseTrain._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  ControllerNode := Owner as TLccTrainController;
  if ControllerNode.IsTrainAssigned then
  begin
    WorkerMessage.LoadTractionControllerRelease(SourceNodeID, SourceAliasID, ControllerNode.AssignedTrain.NodeID, ControllerNode.AssignedTrain.AliasID, SourceNodeID, SourceAliasID);
    SendMessage(ControllerNode, WorkerMessage);
    ControllerNode.ClearAssignedTrain;
  end;
  IgnoreTimer := True;
  try
    ControllerNode.DoTrainReleased; // Should return if we tried to release a train not assigned to us?   Continue reviewing this for the Error codes....
  finally
    AdvanceToNextState;
    IgnoreTimer := False;
  end;
end;

procedure TLccActionTractionReleaseTrain.LoadStateArray;
begin
  inherited LoadStateArray;
end;

{ TLccActionTractionQueryFunction }

function TLccActionTractionQueryFunction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  ControllerNode := Owner as TLccTrainController;
  if Assigned(ControllerNode) then
  begin
    if ControllerNode.IsTrainAssigned then
    begin
      WorkerMessage.LoadTractionQueryFunction(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, Address);
      SendMessage(Owner, WorkerMessage);
    end;
  end;
  AdvanceToNextState;
  SetTimoutCountThreshold(2000);
end;

function TLccActionTractionQueryFunction._1ActionWaitForQueryFunctionResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := False;

  if Assigned(SourceMessage) then
  begin
    ControllerNode := Owner as TLccTrainController;
    if Assigned(ControllerNode) then
    begin
      // Only care if coming from our Assigned Train
      if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, DestNodeID, DestAliasID, True) then
      begin
        case SourceMessage.MTI of
          MTI_TRACTION_REPLY :
            begin
              case SourceMessage.DataArray[0] of
                TRACTION_QUERY_FUNCTION_REPLY :
                  begin  // this can be turned into a request/reply action
                    if Address = SourceMessage.TractionExtractFunctionAddress then
                    begin
                      IgnoreTimer := True; // keep the timer from coming in and freeing the action before DoAssigned has returned
                      try
                        ControllerNode.DoQueryFunctionReply(Address, SourceMessage.TractionExtractFunctionValue);
                      finally
                        AdvanceToNextState;
                        IgnoreTimer := False;
                      end;
                    end;
                  end;
              end;
            end;
          end
      end;
    end;
  end;

  if TimeoutExpired then
  begin
    ErrorCode := laecTimedOut;
    AdvanceToNextState;
  end;
end;

procedure TLccActionTractionQueryFunction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForQueryFunctionResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccActionTractionQuerySpeed }

function TLccActionTractionQuerySpeed._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  ControllerNode := Owner as TLccTrainController;
  if Assigned(ControllerNode) then
  begin
    if ControllerNode.IsTrainAssigned then
    begin
      WorkerMessage.LoadTractionQuerySpeed(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID);
      SendMessage(ControllerNode, WorkerMessage);
    end;
  end;
  AdvanceToNextState;
  SetTimoutCountThreshold(2000);
end;

function TLccActionTractionQuerySpeed._1ActionWaitForQuerySpeedResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := False;

  if Assigned(SourceMessage) then
  begin
    ControllerNode := Owner as TLccTrainController;
    if Assigned(ControllerNode) then
    begin  // Only care if coming from our Assigned Train
      if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, DestNodeID, DestAliasID, True) then
      begin
        case SourceMessage.MTI of
          MTI_TRACTION_REPLY :
            begin
              case SourceMessage.DataArray[0] of
                TRACTION_QUERY_SPEED_REPLY :
                  begin  // this can be turned into a request/reply action
                    IgnoreTimer := True; // keep the timer from coming in and freeing the action before DoAssigned has returned
                    try
                      ControllerNode.DoQuerySpeedReply(SourceMessage.TractionExtractSetSpeed, SourceMessage.TractionExtractCommandedSpeed, SourceMessage.TractionExtractActualSpeed, SourceMessage.TractionExtractSpeedStatus);
                    finally
                      AdvanceToNextState;
                      IgnoreTimer := False;
                    end;
                  end;
              end;
            end;
          end
      end;
    end;
  end;

  if TimeoutExpired then
  begin
    ErrorCode := laecTimedOut;
    AdvanceToNextState;
  end;
end;

procedure TLccActionTractionQuerySpeed.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForQuerySpeedResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccActionSearchAndAssignTrain }

function TLccActionSearchAndAssignTrain._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  LocalAction: TLccActionSearchGatherAndSelectTrain;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  LocalAction := TLccActionSearchGatherAndSelectTrain.Create(Owner, SourceNodeID, SourceAliasID, NULL_NODE_ID, 0);
  LocalAction.RegisterCallBack(Self);
  LocalAction.SearchCriteria := SearchCriteria;
  Owner.LccActions.RegisterAndKickOffAction(LocalAction, nil);

  SetTimoutCountThreshold(5000); // seconds to collect trains, end that Action and kick off another before this times out!!!
  AdvanceToNextState;
end;


function TLccActionSearchAndAssignTrain._1ActionWait(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  // All is handled in the callback, just spin here
end;

procedure TLccActionSearchAndAssignTrain.CompleteCallback(SourceAction: TLccAction);
var
  NextSpawnedAction: TLccActionAssignTrain;
  SpawnedSearchAction: TLccActionSearchGatherAndSelectTrain;
begin
  // The first step of this action is to launch a child action to do the search and select of a train
  // so when that is finished it calls back with the results or an error
  if SourceAction is TLccActionSearchGatherAndSelectTrain then
  begin
    SpawnedSearchAction := SourceAction as TLccActionSearchGatherAndSelectTrain;
    // Selected a train in the child Action now Assign it
    if SpawnedSearchAction.ErrorCode = laecOk then
    begin
      NextSpawnedAction := TLccActionAssignTrain.Create(Owner, SourceNodeID, SourceAliasID, SpawnedSearchAction.Trains[0].NodeID, SpawnedSearchAction.Trains[0].AliasID);
      NextSpawnedAction.Trains.Add(SpawnedSearchAction.Trains[0].Clone);
      NextSpawnedAction.RegisterCallBack(Self);
      Owner.LccActions.RegisterAndKickOffAction(NextSpawnedAction, nil);
      // We stay in the same wait state until this task returns
    end else
    begin
      // Search failed so move on
      ErrorCode := SpawnedSearchAction.ErrorCode;
      AdvanceToNextState;
    end;
  end else
  if SourceAction is TLccActionAssignTrain then
  begin
    ErrorCode := (SourceAction as TLccActionAssignTrain).ErrorCode;
    AdvanceToNextState;
  end;
end;

procedure TLccActionSearchAndAssignTrain.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWait;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;


{ TLccTrainController }

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
var
  LocalSearchCriteria: DWORD;
  LccAssignTrainAction: TLccActionSearchAndAssignTrain;
begin
  ClearAssignedTrain;
  FAssignedTrain.RepliedSearchData := 0;
  FAssignedTrain.RequestedSearchData := 0;
  LocalSearchCriteria := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchCriteria);
  FAssignedTrain.RequestedSearchData := LocalSearchCriteria;

  LccAssignTrainAction := TLccActionSearchAndAssignTrain.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0);
  LccAssignTrainAction.SearchCriteria := LocalSearchCriteria;
  LccActions.RegisterAndKickOffAction(LccAssignTrainAction, nil);
end;

procedure TLccTrainController.AttachListener(ATrainNodeID: TNodeID; ATrainNodeAliasID: Word; AListenerNodeID: TNodeID);
var
  LccTractionAttachListenerAction: TLccActionTractionListenerAttach;
begin
  if IsTrainAssigned then
  begin
    LccTractionAttachListenerAction := TLccActionTractionListenerAttach.Create(Self, NodeID, AliasID, ATrainNodeID, ATrainNodeAliasID);
    LccTractionAttachListenerAction.ListenerNodeID := AListenerNodeID;
    LccActions.RegisterAndKickOffAction(LccTractionAttachListenerAction, nil);
  end;
end;

procedure TLccTrainController.DetachListener(ATrainNodeID: TNodeID; ATrainNodeAliasID: Word; AListenerNodeID: TNodeID);
var
  LccTractionDetachListenerAction: TLccActionTractionListenerDetach;
begin
  if IsTrainAssigned then
  begin
    LccTractionDetachListenerAction := TLccActionTractionListenerDetach.Create(Self, NodeID, AliasID, ATrainNodeID, ATrainNodeAliasID);
    LccTractionDetachListenerAction.ListenerNodeID := AListenerNodeID;
    LccActions.RegisterAndKickOffAction(LccTractionDetachListenerAction, nil);
  end;
end;

procedure TLccTrainController.QueryListeners(ATrainNodeID: TNodeID);
begin

 // what is the result here all the listerners in a list? retain all the parent/child relationships like Balazs suggested?  Need to discuss with him the value of that

  if IsTrainAssigned then
  begin

  end;
end;

procedure TLccTrainController.ReleaseTrain;
begin
  if IsTrainAssigned then
    LccActions.RegisterAndKickOffAction(TLccActionTractionReleaseTrain.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID), nil);
end;

procedure TLccTrainController.SetDirection(AValue: TLccTrainDirection);
begin
  FDirection := AValue;
  if IsTrainAssigned then
  begin
    if Direction = tdForward then
      WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, Speed)
    else
      WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, -Speed);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccTrainController.SetFunctions(Index: Integer; AValue: Word);
begin
  if (Index >= 0) and (Index <= High(FunctionArray)) then
  begin
    FFunctionArray[Index] := AValue;
    if IsTrainAssigned then
    begin
      WorkerMessage.LoadTractionSetFunction(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, Index, AValue);
      SendMessageFunc(Self, WorkerMessage);
    end;
  end;
end;

procedure TLccTrainController.SetSpeed(AValue: single);
begin
  FSpeed := Abs(AValue);
  if IsTrainAssigned then
  begin
    if Direction = tdForward then
      WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, AValue)
    else
      WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, -AValue);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

function TLccTrainController.ProcessMessage(SourceMessage: TLccMessage): Boolean;
var
  AllowTakeOver: Boolean;
begin
  Result :=inherited ProcessMessage(SourceMessage);

  // We only are dealing with messages with destinations for us from here on
  if SourceMessage.HasDestination then
  begin
    if not EqualNode(NodeID,  AliasID, SourceMessage.DestID, SourceMessage.CAN.DestAlias, True) then
      Exit;
  end;

  // Only care if coming from our Assigned Train
  if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, AssignedTrain.NodeID, AssignedTrain.AliasID, True) then
  begin
    case SourceMessage.MTI of
      MTI_TRACTION_REQUEST :
        begin
          case SourceMessage.DataArray[0] of
            TRACTION_CONTROLLER_CONFIG :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY :
                  begin  // This message comes unannounced so it must be handled here, or an action that continousely monitors for it
                    AllowTakeover := True;
                    DoControllerTakeOver(AllowTakeover);
                    if AllowTakeover then
                    begin
                      ClearAssignedTrain;
                      WorkerMessage.LoadTractionControllerChangedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True);
                      SendMessageFunc(Self, WorkerMessage);
                      DoTrainReleased;
                    end else
                    begin
                      WorkerMessage.LoadTractionControllerChangedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, False );
                      SendMessageFunc(Self, WorkerMessage);
                    end;
                  end;
                end;
              end;
          end
        end
      end
  end;
end;

procedure TLccTrainController.QueryFunction(Address: Word);
var
  LccQueryFunctionAction: TLccActionTractionQueryFunction;
begin
  if IsTrainAssigned then
  begin
    LccQueryFunctionAction := TLccActionTractionQueryFunction.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID);
    LccQueryFunctionAction.Address := Address;
    LccActions.RegisterAndKickOffAction(LccQueryFunctionAction, nil);
  end;
end;

procedure TLccTrainController.QueryFunctions;
var
  i: Integer;
begin
  if IsTrainAssigned then
  begin
    for i := 0 to 28 do
      QueryFunction(i);
  end;
end;

procedure TLccTrainController.SearchTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
begin
  SearchTrainByDccTrain(IntToStr(DccAddress), IsLongAddress, SpeedSteps);
end;

procedure TLccTrainController.SearchTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
var
  TrackProtocolFlags: Word;
begin

  TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or TRACTION_SEARCH_ALLOCATE_FORCE or TRACTION_SEARCH_TYPE_EXACT_MATCH or
                        TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;

  if IsLongAddress then
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG
  else
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT;

  case SpeedSteps of
     ldssDefault : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
     ldss14      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
     ldss28      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
     ldss128     : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
  end;

  SearchTrainByOpenLCB(SearchString, TrackProtocolFlags);
end;

procedure TLccTrainController.SearchTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
var
  LocalSearchCriteria: DWORD;
  LccSearchTrainAction: TLccActionSearchGatherAndSelectTrain;
begin
  LocalSearchCriteria := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchCriteria);

  LccSearchTrainAction := TLccActionSearchGatherAndSelectTrain.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0);
  LccSearchTrainAction.SearchCriteria := LocalSearchCriteria;
  LccActions.RegisterAndKickOffAction(LccSearchTrainAction, nil);
end;

procedure TLccTrainController.SearchTrainsByDccAddress(TrainCriteria: TObjectList);
var
  i: Integer;
  DccCriteria: TDccSearchCriteria;
  TrackProtocolFlags: Word;
  SearchData: DWORD;
  SearchTrainsAction: TLccActionSearchGatherAndSelectTrains;
begin
  if TrainCriteria.Count > 0 then
  begin
    SearchTrainsAction := TLccActionSearchGatherAndSelectTrains.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0);

    // Need to translate the DccSearch criteria into LccSearch encoded criteria to launch the search.
    for i := 0 to TrainCriteria.Count - 1 do
    begin
      DccCriteria := TrainCriteria[i] as TDccSearchCriteria;

      if DccCriteria.Address > 0 then
        TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or
                              TRACTION_SEARCH_ALLOCATE_FORCE or
                              TRACTION_SEARCH_TYPE_EXACT_MATCH or
                              TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY
      else
        TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or
                              TRACTION_SEARCH_ALLOCATE_FORCE or
                              TRACTION_SEARCH_TYPE_ALL_MATCH or
                              TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;


      if DccCriteria.LongAddress then
        TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG
      else
        TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT;

      case DccCriteria.SpeedStep of
         ldssDefault : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
         ldss14      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
         ldss28      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
         ldss128     : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
      end;

      SearchData := 0;
      if DccCriteria.Address > 0 then
        WorkerMessage.TractionSearchEncodeSearchString(IntToStr(DccCriteria.Address), TrackProtocolFlags, SearchData)
      else
        WorkerMessage.TractionSearchEncodeSearchString(DccCriteria.SearchStr, TrackProtocolFlags, SearchData);

      SearchTrainsAction.SearchCriteria.Add( TLccEncodedSearchCriteria.Create(SearchData));
    end;

    LccActions.RegisterAndKickOffAction(SearchTrainsAction, nil);
  end;
end;

procedure TLccTrainController.QuerySpeed;
begin
  if IsTrainAssigned then
    LccActions.RegisterAndKickOffAction(TLccActionTractionQuerySpeed.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID), nil);
end;

procedure TLccTrainController.AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
begin
  AssignTrainByDccTrain(IntToStr(DccAddress), IsLongAddress, SpeedSteps);
end;

procedure TLccTrainController.AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
var
  TrackProtocolFlags: Word;
begin

  TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or TRACTION_SEARCH_ALLOCATE_FORCE or TRACTION_SEARCH_TYPE_EXACT_MATCH or
                        TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;

  if IsLongAddress then
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG
  else
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT;

  case SpeedSteps of
     ldssDefault : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
     ldss14      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
     ldss28      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
     ldss128     : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
  end;

  AssignTrainByOpenLCB(SearchString, TrackProtocolFlags);
end;

procedure TLccTrainController.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;

  ProtocolEventsProduced.Add(EVENT_EMERGENCY_STOP, evs_InValid);

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);

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

procedure TLccTrainController.DoTrainAssigned(Reason: TControllerTrainAssignResult);
begin
  if Assigned(OnTrainAssigned) then
    OnTrainAssigned(Self, Reason);
end;

procedure TLccTrainController.DoTrainReleased;
begin
  if Assigned(OnTrainReleased) then
    OnTrainReleased(Self);
end;

procedure TLccTrainController.DoQueryFunctionReply(Address: DWORD; Value: Word);
begin
  if Assigned(OnQueryFunctionReply) then
    OnQueryFunctionReply(Self, Address, Value);
end;

procedure TLccTrainController.DoQuerySpeedReply(ASetSpeed, ACommandSpeed,
  AnActualSpeed: THalfFloat; Status: Byte);
begin
  if Assigned(OnQuerySpeedReply) then
    OnQuerySpeedReply(Self, ASetSpeed, ACommandSpeed, AnActualSpeed, Status);
end;

procedure TLccTrainController.EmergencyStop;
begin
  if IsTrainAssigned then
  begin
    WorkerMessage.LoadTractionEStop(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID);
    SendMessageFunc(Self, WorkerMessage);
    SendMessageFunc(Self, WorkerMessage);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccTrainController.ClearAssignedTrain;
begin
  FAssignedTrain.SearchString := '';
  FAssignedTrain.Listeners := nil;
  FAssignedTrain.NodeID := NULL_NODE_ID;  //   TODO Need to do this manually in SMS so do it forall
  FAssignedTrain.AliasID := 0;
  FAssignedTrain.RequestedSearchData := 0;
  FAssignedTrain.RepliedSearchData := 0;
  FAssignedTrain.SearchString := '';
  FAssignedTrain.Listeners := nil;
end;

procedure TLccTrainController.DoControllerTakeOver(var Allow: Boolean);
begin
  If Assigned(OnControllerRequestTakeover) then
    OnControllerRequestTakeover(Self, Allow);
end;

procedure TLccTrainController.DoSearchResult(Sender: TLccActionTrain; TrainList: TLccActionTrainList; var SelectedResultIndex: Integer);
begin
  if Assigned(OnSearchResult) then
    OnSearchResult(Sender, TrainList, SelectedResultIndex);
end;

procedure TLccTrainController.DoSearchMultiResult(Sender: TLccActionTrain; Trains: TLccActionTrainList);
begin
  if Assigned(OnSearchMultiResult) then
    OnSearchMultiResult(Sender, Trains);
end;

procedure TLccTrainController.DoControllerAttachListenerReply(ListenerNodeID: TNodeID; ReplyCode: Byte);
begin
  if Assigned(OnAttachListenerReply) then
    OnAttachListenerReply(Self, ListenerNodeID, ReplyCode);
end;

procedure TLccTrainController.DoControllerDetachListenerReply(ListenerNodeID: TNodeID; ReplyCode: Byte);
begin
  if Assigned(OnDetachListenerReply) then
    OnDetachListenerReply(Self, ListenerNodeID, ReplyCode);
end;

procedure TLccTrainController.DoControllerQueryListenerGetCount(ListenerCount: Byte);
begin
 if Assigned(OnQueryListenerGetCount) then
    OnQueryListenerGetCount(Self, ListenerCount);
end;

procedure TLccTrainController.DoControllerQueryListenerIndex(ListenerIndex: Byte; ListenerFlags: Byte; ListenerNodeID: TNodeID);
begin
  if Assigned(FOnQueryListenerIndex) then
    FOnQueryListenerIndex(Self, ListenerIndex, ListenerFlags, ListenerNodeID);
end;

function TLccTrainController.GetCdiFile: string;
begin
  Result := CDI_XML_CONTROLLER;
end;

function TLccTrainController.GetFunctions(Index: Integer): Word;
begin
  Result := 0;
  if (Index >= 0) and (Index <= High(FunctionArray)) then
    Result := FunctionArray[Index];
end;

function TLccTrainController.IsTrainAssigned: Boolean;
begin
  Result := (AssignedTrain.NodeID[0] <> 0) or (AssignedTrain.NodeID[1] <> 0) or (AssignedTrain.AliasID <> 0);
end;


end.

