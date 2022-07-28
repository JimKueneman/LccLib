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
  lcc_utilities,
  lcc_alias_mappings;

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

  TControllerTrainAssignResult = (tarAssigned, tarFailTrainRefused, tarFailControllerRefused, tarReserveFailed);

  TLccActionSearchAndAssignTrain = class;

  // Used internally to TLccTrainController for multiple train searches, eventually converted into a TLccEncodedSearchCriteria object
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


  // Used internally to TLccActionSearchGatherAndSelectTrains for multiple train searches

  { TLccDccEncodedSearchCriteria }

  TLccDccEncodedSearchCriteria = class
  private
    FCriteria: DWORD;
  public
    property Criteria: DWORD read FCriteria write FCriteria;

    function EncodeDccCriteria(ASearchString: string; AnAddress: Word; ASpeedStep: TLccDccSpeedStep; IsLongAddress: Boolean): DWORD;

    constructor Create(ACriteria: DWORD);
    constructor Create(ASearchString: string; AnAddress: Word; ASpeedStep: TLccDccSpeedStep; IsLongAddress: Boolean);
    constructor Create(DccSearchCriteria: TDccSearchCriteria);
    function Clone: TLccDccEncodedSearchCriteria;
  end;

  { TLccActionSearchGatherAndSelectTrain }

  // Trains[0]/Train property will contain the selected train when the action completes
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
    {$IFDEF DELPHI}
    FEncodedSearchCriteria: TObjectList<TLccEncodedSearchCriteria>;
    {$ELSE}
    FEncodedSearchCriteria: TObjectList;
    {$ENDIF}
    FiEncodedSearchCriteria: Integer;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionGatherSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionWaitForSpawnedAction(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _3ActionReportSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
    procedure CompleteCallback(SourceAction: TLccAction); override;

    property iEncodedSearchCriteria: Integer read FiEncodedSearchCriteria write FiEncodedSearchCriteria;
  public
    {$IFDEF DELPHI}
    property EncodedSearchCriteria: TObjectList<TLccEncodedSearchCriteria> read FEncodedSearchCriteria write FEncodedSearchCriteria;  // TLccEncodedSearchCriteria objects
    {$ELSE}
    property EncodedSearchCriteria: TObjectList read FEncodedSearchCriteria write FEncodedSearchCriteria;  // TLccDccEncodedSearchCriteria objects
    {$ENDIF}

    constructor Create(AnOwner: TLccNode; ASourceNodeID: TNodeID; ASourceAliasID: Word; ADestNodeID: TNodeID; ADestAliasID: Word; AnUniqueID: Integer); override;
    destructor Destroy; override;
  end;

  { TLccActionConsistTrains }

  // Trains property contains the selected trains when the action completes
  // Calls TLccActionSearchGatherAndSelectTrain for all items in the SearchCriteria property
  TLccActionConsistTrains = class(TLccActionTrain)
  private
    FAttachFailedReplyCount: Integer;
    FAttachSuccessfulReplyCount: Integer;
    {$IFDEF DELPHI}
    FEncodedSearchCriteria: TObjectList<TLccEncodedSearchCriteria>;
    {$ELSE}
    FEncodedSearchCriteria: TObjectList;
    {$ENDIF}
    FiEncodedSearchCriteria: Integer;
    FAttachRequestCount: Integer;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForSearchGatherAndSelectTrainsAction(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionListenerAttach(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _3WaitForListenerAttachReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _4ActionListenerDetachOnError(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
    procedure CompleteCallback(SourceAction: TLccAction); override;

    property iEncodedSearchCriteria: Integer read FiEncodedSearchCriteria write FiEncodedSearchCriteria;
    property AttachRequestCount: Integer read FAttachRequestCount write FAttachRequestCount;
    property AttachSuccessfulReplyCount: Integer read FAttachSuccessfulReplyCount write FAttachSuccessfulReplyCount;
    property AttachFailedReplyCount: Integer read FAttachFailedReplyCount write FAttachFailedReplyCount;
  public
    {$IFDEF DELPHI}
    property EncodedSearchCriteria: TObjectList<TLccEncodedSearchCriteria> read FEncodedSearchCriteria write FEncodedSearchCriteria;  // TLccEncodedSearchCriteria objects
    {$ELSE}
    property EncodedSearchCriteria: TObjectList read FEncodedSearchCriteria write FEncodedSearchCriteria;  // TLccDccEncodedSearchCriteria objects
    {$ENDIF}

    constructor Create(AnOwner: TLccNode; ASourceNodeID: TNodeID; ASourceAliasID: Word; ADestNodeID: TNodeID; ADestAliasID: Word; AnUniqueID: Integer); override;
    destructor Destroy; override;
  end;

  { TLccActionAssignTrain }

  // Sends an Assign message and waits for a result.  Assumes the Trains[0] index is the train and must be valid
  TLccActionAssignTrain = class(TLccActionTrain)
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForReservationAndAssignThrottle(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;

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
    FFlags: Byte;  // TRACTION_LISTENER_FLAG_xxxx constants
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
      FFlags: Byte;   // TRACTION_LISTENER_FLAG_xxxx constants
      FListenerNodeID: TNodeID;
    protected
      function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
      function _1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

      procedure LoadStateArray; override;
    public
      property ListenerNodeID: TNodeID read FListenerNodeID write FListenerNodeID;
      property Flags: Byte read FFlags write FFlags;
    end;

  { TLccActionTractionListenerQueryCount }

  TLccActionTractionListenerQueryCount = class(TLccActionTrain)
    protected
      function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
      function _1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

      procedure LoadStateArray; override;
  end;


  TLccActionTractionListenerQuery = class(TLccActionTrain)
  private
    FiListenerQuery: Byte;  // The index of the Listerner to query information about
    protected
      function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
      function _1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

      procedure LoadStateArray; override;
    public
      property iListenerQuery: Byte read FiListenerQuery write FiListenerQuery;
  end;


type

  TLccTrainController = class;

  TOnControllerConsistsResults = procedure(Sender: TLccTrainController) of object;
  TOnControllerSearchResult = procedure(Sender: TLccTrainController; TrainList: TLccActionTrainInfoList; var SelectedResultIndex: Integer) of object;
  TOnControllerSearchMultiResult = procedure(Sender: TLccTrainController; Trains: TLccActionTrainInfoList) of object;
  TOnControllerTrainAssignedReply = procedure(Sender: TLccTrainController; Reason: TControllerTrainAssignResult) of object;
  TOnControllerTrainReleasedReply = procedure(Sender: TLccTrainController) of object;
  TOnControllerQuerySpeedReply = procedure(Sender: TLccTrainController; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte) of object;
  TOnControllerQueryFunctionReply = procedure(Sender: TLccTrainController; Address: DWORD; Value: Word) of object;
  TOnControllerRequestTakeover = procedure(Sender: TLccTrainController; var Allow: Boolean) of object;
  TOnControllerAttachListenerReply = procedure(Sender: TLccTrainController; Listener: TLccActionTrainInfo; ReplyCode: Word) of object;
  TOnControllerDetachListenerReply = procedure(Sender: TLccTrainController; Listener: TLccActionTrainInfo; ReplyCode: Word) of object;
  TOnControllerQueryListenerGetCount = procedure(Sender: TLccTrainController; ListenerCount: Byte) of object;
  TOnControllerQueryListenerIndex = procedure(Sender: TLccTrainController; ListenerCount, ListenerIndex, ListenerFlags: Byte; ListenerNodeID: TNodeID) of object;

  // ******************************************************************************

//  TAttachedTrainReservationState = (trsNotReserved, trsReserving, trsReserved);
//  TAttachedTrainAssignmentState = (tasNotAssigned, tasAssigning, tasAssigned, tasUnAssigning);
//  TAttachedTrainSearchState = (tssNotSearching, tssSearching);

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
    FOnConsistResults: TOnControllerConsistsResults;
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
    procedure DoConsist; virtual;
    procedure DoControllerTakeOver(var Allow: Boolean); virtual;
    procedure DoSearchResult(TrainList: TLccActionTrainInfoList; var SelectedResultIndex: Integer); virtual;
    procedure DoSearchMultiResult(Trains: TLccActionTrainInfoList); virtual;
    procedure DoControllerAttachListenerReply(Listener: TLccActionTrainInfo; ReplyCode: Word); virtual;
    procedure DoControllerDetachListenerReply(Listener: TLccActionTrainInfo; ReplyCode: Word); virtual;
    procedure DoControllerQueryListenerGetCount(ListenerCount: Byte); virtual;
    procedure DoControllerQueryListenerIndex(ListenerCount, ListenerIndex, ListenerFlags: Byte; ListenerNodeID: TNodeID); virtual;

  public
    property AssignedTrain: TAttachedTrain read FAssignedTrain write FAssignedTrain;
    property Speed: single read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;

    property OnConsistResults: TOnControllerConsistsResults read FOnConsistResults write FOnConsistResults;
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

    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; UniqueID: Integer = 0);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; UniqueID: Integer = 0);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word; UniqueID: Integer = 0);
    procedure ReleaseTrain(UniqueID: Integer = 0);

    procedure QuerySpeed(UniqueID: Integer = 0);
    procedure QueryFunction(Address: Word; UniqueID: Integer = 0);
    procedure QueryFunctions(UniqueID: Integer = 0);

    procedure SearchTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; UniqueID: Integer = 0);
    procedure SearchTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; UniqueID: Integer = 0);
    procedure SearchTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word; UniqueID: Integer = 0);
    {$IFDEF DELPHI}
    procedure SearchTrainsByDccAddress(DccSearchCriteria: TObjectList<TDccSearchCriteria>; UniqueID: Integer = 0);  // Pass TDccSearchCriteria objects
    {$ELSE}
    procedure SearchTrainsByDccAddress(DccSearchCriteria: TObjectList; UniqueID: Integer = 0);  // Pass TDccSearchCriteria objects
    {$ENDIF}

    {$IFDEF DELPHI}
    procedure ConsistTrainsByDccAddress(DccSearchCriteria: TObjectList<TDccSearchCriteria>; UniqueID: Integer = 0);  // Pass TDccSearchCriteria objects
    {$ELSE}
    procedure ConsistTrainsByDccAddress(DccSearchCriteria: TObjectList; UniqueID: Integer = 0);  // Pass TDccSearchCriteria objects
    {$ENDIF}

    procedure EmergencyStop(UniqueID: Integer = 0);

    function IsTrainAssigned: Boolean;

    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TLccActionConsistTrains }

function TLccActionConsistTrains._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  SpawnedSearchAndGatherTrainsAction: TLccActionSearchGatherAndSelectTrains;
  i: Integer;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  SpawnedSearchAndGatherTrainsAction := TLccActionSearchGatherAndSelectTrains.Create(Owner, SourceNodeID, SourceAliasID, NULL_NODE_ID, 0, UniqueID);
  for i := 0 to EncodedSearchCriteria.Count - 1 do
    SpawnedSearchAndGatherTrainsAction.EncodedSearchCriteria.Add( (EncodedSearchCriteria[i] as TLccDccEncodedSearchCriteria).Clone);

  SpawnedSearchAndGatherTrainsAction.RegisterCallBackOnExit(Self);
  Owner.LccActions.RegisterAndKickOffAction(SpawnedSearchAndGatherTrainsAction, nil);
  SetTimoutCountThreshold( Trunc(TIMEOUT_CREATE_TRAIN_WAIT * EncodedSearchCriteria.Count));  // 1 Wait Unit for each train we are trying to create
  AdvanceToNextState;
end;

function TLccActionConsistTrains._1ActionWaitForSearchGatherAndSelectTrainsAction(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  // Spinning here
  // Will break out from the Callback even from timeouts
end;

function TLccActionConsistTrains._2ActionListenerAttach(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i: Integer;
  ActionListenerAttach: TLccActionTractionListenerAttach;
begin
  Result := False;
  AttachSuccessfulReplyCount := 0;
  AttachFailedReplyCount := 0;


  AttachRequestCount := (2 * EncodedSearchCriteria.Count) - 2;
  for i := 0 to Trains.Count - 2 do
  begin
    ActionLIstenerAttach := TLccActionTractionListenerAttach.Create(Owner, SourceNodeID, SourceAliasID, Trains[i].NodeID, Trains[i].AliasID, 0);
    ActionListenerAttach.ListenerNodeID := Trains[i+1].NodeID;
    ActionListenerAttach.RegisterCallBackOnExit(Self);
    Owner.LccActions.RegisterAndKickOffAction(ActionListenerAttach, nil);

    ActionLIstenerAttach := TLccActionTractionListenerAttach.Create(Owner, SourceNodeID, SourceAliasID, Trains[i+1].NodeID, Trains[i+1].AliasID, 0);
    ActionListenerAttach.ListenerNodeID := Trains[i].NodeID;
    ActionListenerAttach.RegisterCallBackOnExit(Self);
    Owner.LccActions.RegisterAndKickOffAction(ActionListenerAttach, nil);
  end;
  SetTimoutCountThreshold( Trunc(TIMEOUT_LISTENER_ATTACH_TRAIN_WAIT * EncodedSearchCriteria.Count));
  AdvanceToNextState;
end;

function TLccActionConsistTrains._3WaitForListenerAttachReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  // Spinning here
  // Will break out from the Callback even from timeouts
end;

function TLccActionConsistTrains._4ActionListenerDetachOnError(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i: Integer;
  ActionListenerDetach: TLccActionTractionListenerDetach;
begin
  Result := False;

  for i := 0 to Trains.Count - 2 do
  begin
    ActionLIstenerDetach := TLccActionTractionListenerDetach.Create(Owner, SourceNodeID, SourceAliasID, Trains[i].NodeID, Trains[i].AliasID, 0);
    ActionListenerDetach.ListenerNodeID := Trains[i+1].NodeID;
    ActionListenerDetach.RegisterCallBackOnExit(Self);
    Owner.LccActions.RegisterAndKickOffAction(ActionListenerDetach, nil);

    ActionLIstenerDetach := TLccActionTractionListenerDetach.Create(Owner, SourceNodeID, SourceAliasID, Trains[i+1].NodeID, Trains[i+1].AliasID, 0);
    ActionListenerDetach.ListenerNodeID := Trains[i].NodeID;
    ActionListenerDetach.RegisterCallBackOnExit(Self);
    Owner.LccActions.RegisterAndKickOffAction(ActionListenerDetach, nil);
  end;

  AdvanceToNextState;
end;

procedure TLccActionConsistTrains.CompleteCallback(SourceAction: TLccAction);
var
  SpawnedSearchTrains: TLccActionSearchGatherAndSelectTrains;
  SpawnedListenerAttach: TLccActionTractionListenerAttach;
  i: Integer;
begin
  if SourceAction is TLccActionSearchGatherAndSelectTrains then
  begin
    SpawnedSearchTrains := SourceAction as TLccActionSearchGatherAndSelectTrains;
    if SpawnedSearchTrains.ErrorCode = laecOk then
    begin
      // If a train could not be created then the ErrorCode will reflect that so this
      // assumes that SpawnedSearchTrains.Trains.Count = Number of Search Requests sent
      for i := 0 to SpawnedSearchTrains.Trains.Count - 1 do
        Trains.Add( SpawnedSearchTrains.Trains[i].Clone);
      AdvanceToNextState;
    end else
    begin
      // If timed out then that will be the error code
      ErrorCode := SpawnedSearchTrains.ErrorCode;
      (Owner as TLccTrainController).DoConsist();
      AdvanceToLastState;
    end;
  end else
  if SourceAction is TLccActionTractionListenerAttach then
  begin
    SpawnedListenerAttach := SourceAction as TLccActionTractionListenerAttach;
    case SpawnedListenerAttach.ErrorCode of
      laecOk                   : Inc(FAttachSuccessfulReplyCount);
    else
       Inc(FAttachFailedReplyCount);
    end;

    if Trains.Count = AttachSuccessfulReplyCount then
      AdvanceToLastState     // Sucessful and Done
    else
    if Trains.Count = (AttachSuccessfulReplyCount + AttachFailedReplyCount) then
      AdvanceToNextState;   // Unsucessful, need to clean up
  end;
end;

procedure TLccActionConsistTrains.LoadStateArray;
begin
  SetStateArrayLength(6);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForSearchGatherAndSelectTrainsAction;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2ActionListenerAttach;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_3WaitForListenerAttachReply;
  States[4] := {$IFNDEF DELPHI}@{$ENDIF}_4ActionListenerDetachOnError;
  States[5] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;

end;

constructor TLccActionConsistTrains.Create(AnOwner: TLccNode; ASourceNodeID: TNodeID; ASourceAliasID: Word; ADestNodeID: TNodeID; ADestAliasID: Word; AnUniqueID: Integer);
begin
  inherited Create(AnOwner, ASourceNodeID, ASourceAliasID, ADestNodeID, ADestAliasID, AnUniqueID);

  {$IFDEF DELPHI}
  FEncodedSearchCriteria := TObjectList<TLccEncodedSearchCriteria>.Create;
  {$ELSE}
  FEncodedSearchCriteria := TObjectList.Create;
  {$ENDIF}
  {$IFNDEF DWSCRIPT}
  EncodedSearchCriteria.OwnsObjects := True;
  {$ENDIF}
  iEncodedSearchCriteria := 0;
end;

destructor TLccActionConsistTrains.Destroy;
begin
  FreeAndNil(FEncodedSearchCriteria);
  inherited Destroy;
end;

{ TLccActionTractionListenerQuery }

procedure TLccActionTractionListenerQuery.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1WaitforReply;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

function TLccActionTractionListenerQuery._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionListenerQuery(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, iListenerQuery);
  SendMessage(Self, WorkerMessage);
  SetTimoutCountThreshold(1000);
  AdvanceToNextState;
end;

function TLccActionTractionListenerQuery._1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
  ListenerCount: Byte;
  ListenerNodeID: TNodeID;
  ListenerFlags: Byte;
  ListenerIndex: Byte;
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
                     TRACTION_LISTENER_QUERY_REPLY :
                       begin
                         FreezeTimer := True;
                         try
                           if SourceMessage.DataCount = 11 then
                           begin
                             ListenerCount := SourceMessage.DataArrayIndexer[2];
                             ListenerNodeID := NULL_NODE_ID;
                             SourceMessage.ExtractDataBytesAsNodeID(5, ListenerNodeID);
                             ListenerFlags := SourceMessage.DataArrayIndexer[4];
                             ListenerIndex := SourceMessage.DataArrayIndexer[3];
                             ControllerNode.DoControllerQueryListenerIndex(ListenerCount, ListenerIndex, ListenerFlags, ListenerNodeID);
                           end;
                         finally
                           AdvanceToNextState;
                           FreezeTimer := False;;
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

{ TLccActionTractionListenerQueryCount }

procedure TLccActionTractionListenerQueryCount.LoadStateArray;
begin
  SetStateArrayLength(3);
   States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
   States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1WaitforReply;
   States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

function TLccActionTractionListenerQueryCount._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionListenerQueryCount(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID);
  SendMessage(Self, WorkerMessage);
  SetTimoutCountThreshold(1000);
  AdvanceToNextState;
end;

function TLccActionTractionListenerQueryCount._1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
  ListenerCount: Byte;
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
                     TRACTION_LISTENER_QUERY_REPLY :
                       begin
                         FreezeTimer := True;
                         try
                           if SourceMessage.DataCount = 3 then
                           begin
                             ListenerCount := SourceMessage.DataArrayIndexer[2];
                             ControllerNode.DoControllerQueryListenerGetCount(ListenerCount);
                           end;
                         finally
                           AdvanceToNextState;
                           FreezeTimer := False;;
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

{ TLccDccEncodedSearchCriteria }

function TLccDccEncodedSearchCriteria.EncodeDccCriteria(ASearchString: string; AnAddress: Word; ASpeedStep: TLccDccSpeedStep; IsLongAddress: Boolean): DWORD;
var
  i: Integer;
  TrackProtocolFlags: Word;
begin
  Result := 0;

  // Need to translate the DCC criteria into LccSearch encoded criteria to launch the search.
  if AnAddress > 0 then
    TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or
                          TRACTION_SEARCH_ALLOCATE_FORCE or
                          TRACTION_SEARCH_TYPE_EXACT_MATCH or
                          TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY
  else
    TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or
                          TRACTION_SEARCH_ALLOCATE_FORCE or
                          TRACTION_SEARCH_TYPE_ALL_MATCH or
                          TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;


  if IsLongAddress then
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG
  else
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT;

  case ASpeedStep of
     ldssDefault : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
     ldss14      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
     ldss28      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
     ldss128     : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
  end;

  if AnAddress > 0 then
    TLccMessage.TractionSearchEncodeSearchString(IntToStr(AnAddress), TrackProtocolFlags, Result)
  else
    TLccMessage.TractionSearchEncodeSearchString(ASearchString, TrackProtocolFlags, Result);

end;

constructor TLccDccEncodedSearchCriteria.Create(ACriteria: DWORD);
begin
  FCriteria := ACriteria;
end;

constructor TLccDccEncodedSearchCriteria.Create(ASearchString: string; AnAddress: Word; ASpeedStep: TLccDccSpeedStep; IsLongAddress: Boolean);
begin
  Criteria := EncodeDccCriteria(ASearchString, AnAddress, ASpeedStep, IsLongAddress);
end;

constructor TLccDccEncodedSearchCriteria.Create(DccSearchCriteria: TDccSearchCriteria);
begin
  Criteria := EncodeDccCriteria(DccSearchCriteria.SearchStr, DccSearchCriteria.Address, DccSearchCriteria.SpeedStep, DccSearchCriteria.LongAddress);
end;

function TLccDccEncodedSearchCriteria.Clone: TLccDccEncodedSearchCriteria;
begin
  Result := TLccDccEncodedSearchCriteria.Create(Criteria);
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
  LccSearchTrainAction := TLccActionSearchGatherAndSelectTrain.Create(Owner, SourceNodeID, SourceAliasID, NULL_NODE_ID, 0, UniqueID);
  LccSearchTrainAction.SearchCriteria := (EncodedSearchCriteria[iEncodedSearchCriteria] as TLccDccEncodedSearchCriteria).Criteria;
  LccSearchTrainAction.RegisterCallBackOnExit(Self);
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
    FreezeTimer := True;  // Stop reentrancy
    try
      ControllerNode := Owner as TLccTrainController;
      ControllerNode.DoSearchMultiResult(Trains);
    finally
      AdvanceToNextState;
      FreezeTimer := False;
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
  ADestAliasID: Word; AnUniqueID: Integer);
begin
  inherited Create(AnOwner, ASourceNodeID, ASourceAliasID, ADestNodeID, ADestAliasID, AnUniqueID);

  {$IFDEF DELPHI}
  FEncodedSearchCriteria := TObjectList<TLccEncodedSearchCriteria>.Create;
  {$ELSE}
  FEncodedSearchCriteria := TObjectList.Create;
  {$ENDIF}
  {$IFNDEF DWSCRIPT}
  EncodedSearchCriteria.OwnsObjects := True;
  {$ENDIF}
  iEncodedSearchCriteria := 0;
end;

procedure TLccActionSearchGatherAndSelectTrains.CompleteCallback(SourceAction: TLccAction);
var
  SpawnedSearchAction: TLccActionSearchGatherAndSelectTrain;
begin
  SpawnedSearchAction := SourceAction as TLccActionSearchGatherAndSelectTrain;

  if SpawnedSearchAction.ErrorCode = laecOk then
  begin
    if SpawnedSearchAction.Trains.Count = 1 then
      Trains.Add(SpawnedSearchAction.Train.Clone);

    // Move to the next Criteria in our list
    Inc(FiEncodedSearchCriteria);
    // if we are done move on else backup one to create a new search task
    if iEncodedSearchCriteria < EncodedSearchCriteria.Count then
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
  FreeAndNil(FEncodedSearchCriteria);
  inherited Destroy;
end;

{ TLccActionAssignTrain }

function TLccActionAssignTrain._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  Assert(Trains.Count > 0, 'TLccActionAssignTrain: Must have a train in the Trains Property to Assign');

  //  Attempt to assign the controller to the Train stored in the Trains[0]/Train property
  if Trains.Count > 0 then
  begin
    // Reserve the Train first
    WorkerMessage.LoadTractionManage(SourceNodeID, SourceAliasID, Train.NodeID, Train.AliasID, True);
    SendMessage(Owner, WorkerMessage);
    SetTimoutCountThreshold(TIMEOUT_CONTROLLER_RESERVE_WAIT);
  end;
  AdvanceToNextState;
end;

function TLccActionAssignTrain._1ActionWaitForReservationAndAssignThrottle(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  // SourceMessage could be nil from a timer
  // This code assumes that index Trains[0]/Train is the selected train
  if Assigned(SourceMessage) and (Trains.Count > 0) then
  begin // Only care if it is coming from our potental Train
    if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, Train.NodeID, Train.AliasID, True) then
    begin
      case SourceMessage.MTI of
         MTI_TRACTION_REPLY :
           begin
             case SourceMessage.DataArray[0] of
               TRACTION_MANAGE :
                 begin
                   case SourceMessage.DataArray[1] of
                     TRACTION_RESERVE_REPLY :
                       begin
                         // keep the timer from being reentrant during event calls with blocking code (dialogs)
                         FreezeTimer := True;
                         try
                           case SourceMessage.DataArray[2] of
                             TRACTION_MANAGE_RESERVE_REPLY_OK :
                               begin
                                 // Train is Reserved time to Assign it to the Controller
                                 // Timeout allows time if the Train is connected to another controller to ask that controller to release it
                                 WorkerMessage.LoadTractionControllerAssign(SourceNodeID, SourceAliasID, Train.NodeID, Train.AliasID, SourceNodeID);
                                 SendMessage(Owner, WorkerMessage);
                                 SetTimoutCountThreshold(TIMEOUT_CONTROLLER_NOTIFY_WAIT * 2);
                                 AdvanceToNextState;
                               end;
                             else begin
                               if Assigned(Owner) then
                                (Owner as TLccTrainController).DoTrainAssigned(tarReserveFailed);
                               ErrorCode := laecReservedFailed;
                               AdvanceToNextState(2);
                             end;
                           end;
                         finally
                           FreezeTimer := False;
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
    WorkerMessage.LoadTractionManage(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, False);
    SendMessage(Owner, WorkerMessage);
    ErrorCode := laecTimedOut;
    AdvanceToNextState(2);
  end;
end;

function TLccActionAssignTrain._2ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := False;

  // SourceMessage could be nil from a timer
  // This code assumes that index Trains[0] is the selected train
  if Assigned(SourceMessage) and (Trains.Count > 0) then
  begin // Only care if it is coming from our potental Train
    if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, Train.NodeID, Train.AliasID, True) then
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
                         // keep the timer from being reentrant during event calls with blocking code (dialogs)
                         FreezeTimer := True;
                         if Assigned(Owner) then
                           ControllerNode := Owner as TLccTrainController;
                         try
                           case SourceMessage.DataArray[2] of
                             TRACTION_CONTROLLER_CONFIG_REPLY_OK :
                               begin
                                  if Assigned(ControllerNode) then
                                  begin
                                    ControllerNode.FAssignedTrain.NodeID := DestNodeID;
                                    ControllerNode.FAssignedTrain.AliasID := DestAliasID;
                                    ControllerNode.FAssignedTrain.RepliedSearchData := Train.SearchCriteria;
                                    ControllerNode.DoTrainAssigned(tarAssigned);
                                    if ReleaseTrain then
                                    begin
                                      WorkerMessage.LoadTractionManage(SourceMessage.DestID, SourceMessage.CAN.DestAlias, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, False);
                                      SendMessage(Owner, WorkerMessage);
                                    end;
                                 end;
                               end;
                             TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
                               begin
                                  if Assigned(ControllerNode) then
                                    ControllerNode.DoTrainAssigned(tarFailTrainRefused);
                               end;
                             TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN               :
                               begin
                                  if Assigned(ControllerNode) then
                                    ControllerNode.DoTrainAssigned(tarFailControllerRefused);
                               end;
                           end;
                         finally
                           // Release the Train if desired.  Where this is false would be if collecting trains for a consist and wanting to keep them locked
                           if ReleaseTrain then
                           begin
                             WorkerMessage.LoadTractionManage(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, False);
                             SendMessage(Owner, WorkerMessage);
                           end;
                           AdvanceToNextState;
                           FreezeTimer := False;
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
    WorkerMessage.LoadTractionManage(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, False);
    SendMessage(Owner, WorkerMessage);
    ErrorCode := laecTimedOut;
    AdvanceToNextState;
  end;
end;

procedure TLccActionAssignTrain.LoadStateArray;
begin
  SetStateArrayLength(4);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForReservationAndAssignThrottle;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2ActionWaitForAssignThrottleResult;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup
end;

{ TLccActionSearchGatherAndSelectTrain }

function TLccActionSearchGatherAndSelectTrain._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  // Send the search message
  WorkerMessage.LoadTractionSearch(SourceNodeID, SourceAliasID, SearchCriteria);
  SendMessage(Owner, WorkerMessage);

  SetTimoutCountThreshold(TIMEOUT_CREATE_TRAIN_WAIT); // milliseconds to allow trains to respond; If the train node needs to be created then it can take >750ms
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

  Version,
  UserVersion: byte;
  Manufacturer,
  Model,
  HardwareVersion,
  SoftwareVersion,
  UserName,
  UserDescription: string;

  LocalTrain: TLccActionTrainInfo;
  AliasMapping: TLccAliasMapping;
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
               // Need to make sure the NodeID property is valid once we add it to the Trains list as well
               if Owner.GridConnect then
                 ValidateAliasMapping(SourceMessage.CAN.SourceAlias, True);

               LocalTrain.SearchCriteria := SourceMessage.TractionSearchExtractSearchData;
               LocalTrain.SearchCriteriaFound := True;

               // Send a message back to the Train Node from this controller asking for the number of Listeners it has
               WorkerMessage.LoadTractionListenerQueryCount(SourceNodeID, SourceAliasID, LocalTrain.NodeID, LocalTrain.AliasID);
               SendMessage(Owner, WorkerMessage);

               LocalTrain.TrainSNIP.Valid := False;
               // Send a message back to the Train Node from this controller asking for the STNIP
               WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(SourceNodeID, SourceAliasID, LocalTrain.NodeID, LocalTrain.AliasID);
               SendMessage(Owner, WorkerMessage);

               LocalTrain.SNIP.Valid := False;
               // Send a message back to the Train Node from this controller asking for the STNIP
               WorkerMessage.LoadSimpleNodeIdentInfoRequest(SourceNodeID, SourceAliasID, LocalTrain.NodeID, LocalTrain.AliasID);
               SendMessage(Owner, WorkerMessage);

               SetTimoutCountThreshold(Round(TIMEOUT_SNIP_REPONSE_WAIT)); // milliseconds to allow trains to resspond to the STNIP _AND_ the Alias Mapping AME call
             end
           end
        end;
       MTI_SIMPLE_NODE_INFO_REPLY :
         begin
           // find the right Node that this SNIP belongs to
            // Expectation is that we recieved the producer identified first because we asked for this above after the slot has been created
            LocalTrain := Trains.MatchingNodeAndSearchCriteria(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
            if Assigned(LocalTrain) then
            begin
              // all this claptrap for SMS and var parameters....
              Version := 0;
              UserVersion := 0;
              Manufacturer := '';
              Model := '';
              HardwareVersion := '';
              SoftwareVersion := '';
              UserName := '';
              UserDescription := '';
              SourceMessage.ExtractSimpleNodeIdentInfo(Version, Manufacturer, Model, HardwareVersion, SoftwareVersion, UserVersion, UserName, UserDescription);
              LocalTrain.SNIP.Version := Version;
              LocalTrain.SNIP.Manufacturer := Manufacturer;
              LocalTrain.SNIP.Model := Model;
              LocalTrain.SNIP.HardwareVersion := HardwareVersion;
              LocalTrain.SNIP.SoftwareVersion := SoftwareVersion;
              LocalTrain.SNIP.UserVersion := UserVersion;
              LocalTrain.SNIP.UserName := UserName;
              LocalTrain.SNIP.UserDescription := UserDescription;
              LocalTrain.SNIP.Valid := True;
            end;
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
              LocalTrain.TrainSNIP.Manufacturer := TrainManufacturer;
              LocalTrain.TrainSNIP.Owner := TrainOwner;
              LocalTrain.TrainSNIP.Roadname := TrainRoadName;
              LocalTrain.TrainSNIP.RoadNumber := TrainRoadNumber;
              LocalTrain.TrainSNIP.TrainClass := TrainClass;
              LocalTrain.TrainSNIP.TrainName := TrainName;
              LocalTrain.TrainSNIP.Version := TrainVersion;
              LocalTrain.TrainSNIP.Valid := True;
            end;
         end;
       MTI_TRACTION_REPLY :
         begin
           case SourceMessage.DataArrayIndexer[0] of
              TRACTION_LISTENER :
                begin
                  case SourceMessage.DataArrayIndexer[1] of
                     TRACTION_LISTENER_QUERY_REPLY :
                       begin
                         if SourceMessage.DataCount = 3 then
                         begin
                           LocalTrain := Trains.MatchingNodeAndSearchCriteria(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
                           if Assigned(LocalTrain) then
                           begin
                             LocalTrain.Listener.Count := SourceMessage.DataArrayIndexer[2]
                           end
                         end;
                       end;
                  end
                end;
           end
         end
    end;
  end;

  if TimeoutExpired then    // Times up, report out....
  begin
    AliasMapping := ValidateAliasMappingWait;
    if Assigned(AliasMapping) then
    begin
      LocalTrain := Trains.MatchingAlias(AliasMapping.AnAlias);
      if Assigned(LocalTrain) then
      begin
        LocalTrain.NodeID := AliasMapping.AnID;
        AdvanceToNextState  // Timeout is NOT an error here, it is normal
      end else
      begin
        ErrorCode := laecUnableToCreateAliasMapping;
        AdvanceToLastState;
      end
    end else
    begin
      ErrorCode := laecUnableToCreateAliasMapping;
      AdvanceToLastState;
    end
  end
end;

function TLccActionSearchGatherAndSelectTrain._2ActionReportSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
  iSelectedTrain: Integer;
  SelectedTrain: TLccActionTrainInfo;
var
  i: Integer;
begin
  Result := False;

  FreezeTimer := True;  // Stop reentrancy
  try
    ControllerNode := Owner as TLccTrainController;

    if Trains.Count > 0 then
      iSelectedTrain := 0
    else
      iSelectedTrain := -1;

      // Let the handler deal with no trains as an error code
      ControllerNode.DoSearchResult(Trains, iSelectedTrain);

      // If there are no trains don't do anything
      if Trains.Count > 0 then
      begin
        // Validate what the program returned for an index
        if (iSelectedTrain < 0) or (iSelectedTrain >= Trains.Count) then
          iSelectedTrain := 0;

        // Only have the selected train in the Trains property
        SelectedTrain := Trains[iSelectedTrain];
        for i := Trains.Count - 1 downto 0 do
        begin
          if SelectedTrain <> Trains[iSelectedTrain] then
            Trains.Remove(SelectedTrain);
        end;
      end else
        ErrorCode := laecNoTrainFound; // set a code for no train found
  finally
    AdvanceToNextState;
    FreezeTimer := False;
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
  ReplyCode: Word;
begin
  Result := False;

  ControllerNode := Owner as TLccTrainController;

  if Assigned(SourceMessage) then
  begin
    if EqualNode(SourceMessage.DestID, SourceMessage.CAN.DestAlias, DestNodeID, DestAliasID, True) then
    begin
      FreezeTimer := True;
      try
        case SourceMessage.MTI of
           MTI_TRACTION_REPLY :
             begin
               case SourceMessage.DataArray[0] of
                 TRACTION_LISTENER :
                     begin
                       case SourceMessage.DataArray[1] of
                         TRACTION_LISTENER_DETACH_REPLY :
                           begin
                             ReplyCode := SourceMessage.ExtractDataBytesAsWord(8);
                             ControllerNode.DoControllerDetachListenerReply(nil, ReplyCode);
                             AdvanceToNextState;
                           end;
                       end;

                     end;
               end;
             end;
        end;
      finally
        FreezeTimer := False;
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
  SetTimoutCountThreshold(TIMEOUT_LISTENER_ATTACH_TRAIN_WAIT);
  AdvanceToNextState;
end;

function TLccActionTractionListenerAttach._1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
  ReplyCode: Word;
begin
  Result := False;

  ControllerNode := Owner as TLccTrainController;

  if Assigned(SourceMessage) then
  begin
    if EqualNode(SourceMessage.DestID, SourceMessage.CAN.DestAlias, DestNodeID, DestAliasID, True) then
    begin
      FreezeTimer := True;
      try
        case SourceMessage.MTI of
           MTI_TRACTION_REPLY :
             begin
               case SourceMessage.DataArray[0] of
                 TRACTION_LISTENER :
                     begin
                       case SourceMessage.DataArray[1] of
                         TRACTION_LISTENER_DETACH_REPLY :
                           begin
                             ReplyCode := SourceMessage.ExtractDataBytesAsWord(8);
                             ControllerNode.DoControllerAttachListenerReply(nil, ReplyCode);
                             AdvanceToNextState;
                           end else
                             ErrorCode := laecListenerAttachFailed;
                       end;

                     end;
               end;
             end;
        end;
      finally
        FreezeTimer := False;
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
  FreezeTimer := True;
  try
    ControllerNode.DoTrainReleased; // Should return if we tried to release a train not assigned to us?   Continue reviewing this for the Error codes....
  finally
    AdvanceToNextState;
    FreezeTimer := False;
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
                      FreezeTimer := True; // keep the timer from coming in and freeing the action before DoAssigned has returned
                      try
                        ControllerNode.DoQueryFunctionReply(Address, SourceMessage.TractionExtractFunctionValue);
                      finally
                        AdvanceToNextState;
                        FreezeTimer := False;
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
                    FreezeTimer := True; // keep the timer from coming in and freeing the action before DoAssigned has returned
                    try
                      ControllerNode.DoQuerySpeedReply(SourceMessage.TractionExtractSetSpeed, SourceMessage.TractionExtractCommandedSpeed, SourceMessage.TractionExtractActualSpeed, SourceMessage.TractionExtractSpeedStatus);
                    finally
                      AdvanceToNextState;
                      FreezeTimer := False;
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

  // First Search for the the train wanted
  LocalAction := TLccActionSearchGatherAndSelectTrain.Create(Owner, SourceNodeID, SourceAliasID, NULL_NODE_ID, 0, UniqueID);
  LocalAction.RegisterCallBackOnExit(Self);
  LocalAction.SearchCriteria := SearchCriteria;
  Owner.LccActions.RegisterAndKickOffAction(LocalAction, nil);

  // seconds to collect trains, end that Action and kick off another before this times out!!!
  SetTimoutCountThreshold(5000);
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
    if (SpawnedSearchAction.ErrorCode = laecOk) and Assigned(SpawnedSearchAction.Train) then
    begin
      NextSpawnedAction := TLccActionAssignTrain.Create(Owner, SourceNodeID, SourceAliasID, SpawnedSearchAction.Train.NodeID, SpawnedSearchAction.Train.AliasID, UniqueID);
      NextSpawnedAction.ReleaseTrain := ReleaseTrain;
      NextSpawnedAction.Trains.Add(SpawnedSearchAction.Train.Clone);
      NextSpawnedAction.RegisterCallBackOnExit(Self);
      Owner.LccActions.RegisterAndKickOffAction(NextSpawnedAction, nil);
      // We stay in the same wait state until this task Exits
    end else
    begin
      // Search failed so move on
      ErrorCode := SpawnedSearchAction.ErrorCode;
      SpawnedSearchAction.Trains.Clear;
      AdvanceToNextState;
    end;
  end else
  if SourceAction is TLccActionAssignTrain then
  begin
    // The Action has tried to clean up on its own on an error so we can just pass the errorcode
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

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string;
  TrackProtocolFlags: Word; UniqueID: Integer);
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

  LccAssignTrainAction := TLccActionSearchAndAssignTrain.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0, UniqueID);
  LccAssignTrainAction.SearchCriteria := LocalSearchCriteria;
  LccActions.RegisterAndKickOffAction(LccAssignTrainAction, nil);
end;

procedure TLccTrainController.ReleaseTrain(UniqueID: Integer);
begin
  if IsTrainAssigned then
    LccActions.RegisterAndKickOffAction(TLccActionTractionReleaseTrain.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, UniqueID), nil);
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

procedure TLccTrainController.QueryFunction(Address: Word; UniqueID: Integer);
var
  LccQueryFunctionAction: TLccActionTractionQueryFunction;
begin
  if IsTrainAssigned then
  begin
    LccQueryFunctionAction := TLccActionTractionQueryFunction.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, UniqueID);
    LccQueryFunctionAction.Address := Address;
    LccActions.RegisterAndKickOffAction(LccQueryFunctionAction, nil);
  end;
end;

procedure TLccTrainController.QueryFunctions(UniqueID: Integer);
var
  i: Integer;
begin
  if IsTrainAssigned then
  begin
    for i := 0 to 28 do
      QueryFunction(i);
  end;
end;

procedure TLccTrainController.SearchTrainByDccAddress(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; UniqueID: Integer);
begin
  SearchTrainByDccTrain(IntToStr(DccAddress), IsLongAddress, SpeedSteps);
end;

procedure TLccTrainController.SearchTrainByDccTrain(SearchString: string;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; UniqueID: Integer);
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

procedure TLccTrainController.SearchTrainByOpenLCB(SearchString: string;
  TrackProtocolFlags: Word; UniqueID: Integer);
var
  LocalSearchCriteria: DWORD;
  LccSearchTrainAction: TLccActionSearchGatherAndSelectTrain;
begin
  LocalSearchCriteria := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchCriteria);

  LccSearchTrainAction := TLccActionSearchGatherAndSelectTrain.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0, UniqueID);
  LccSearchTrainAction.SearchCriteria := LocalSearchCriteria;
  LccActions.RegisterAndKickOffAction(LccSearchTrainAction, nil);
end;

{$IFDEF DELPHI}
procedure TLccTrainController.SearchTrainsByDccAddress(DccSearchCriteria: TObjectList<TDccSearchCriteria>; UniqueID: Integer);
{$ELSE}
procedure TLccTrainController.SearchTrainsByDccAddress(DccSearchCriteria: TObjectList; UniqueID: Integer);
{$ENDIF}
var
  i: Integer;
  DccCriteria: TDccSearchCriteria;
  TrackProtocolFlags: Word;
  SearchData: DWORD;
  SearchTrainsAction: TLccActionSearchGatherAndSelectTrains;
begin
  if DccSearchCriteria.Count > 0 then
  begin
    SearchTrainsAction := TLccActionSearchGatherAndSelectTrains.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0, UniqueID);

    // Need to translate the DccSearch criteria into LccSearch encoded criteria to launch the search.
    for i := 0 to DccSearchCriteria.Count - 1 do
    begin
      {$IFDEF DELPHI}
      DccCriteria := TrainCriteria[i];
      {$ELSE}
      DccCriteria := DccSearchCriteria[i] as TDccSearchCriteria;
      {$ENDIF}
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

      SearchTrainsAction.EncodedSearchCriteria.Add( TLccDccEncodedSearchCriteria.Create(SearchData));
    end;

    LccActions.RegisterAndKickOffAction(SearchTrainsAction, nil);
  end;
end;

{$IFDEF DELPHI}
procedure TLccTrainController.ConsistTrainsByDccAddress(DccSearchCriteria: TObjectList<TDccSearchCriteria>; UniqueID: Integer);
{$ELSE}
procedure TLccTrainController.ConsistTrainsByDccAddress(DccSearchCriteria: TObjectList; UniqueID: Integer);
{$ENDIF}
var
  ActionConsistTrains: TLccActionConsistTrains;
  i: Integer;
begin

  ActionConsistTrains := TLccActionConsistTrains.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0, 0);

  for i := 0 to DccSearchCriteria.Count - 1 do
    ActionConsistTrains.EncodedSearchCriteria.Add( TLccDccEncodedSearchCriteria.Create( DccSearchCriteria[i] as TDccSearchCriteria));
  LccActions.RegisterAndKickOffAction(ActionConsistTrains, nil)
end;

procedure TLccTrainController.QuerySpeed(UniqueID: Integer);
begin
  if IsTrainAssigned then
    LccActions.RegisterAndKickOffAction(TLccActionTractionQuerySpeed.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, UniqueID), nil);
end;

procedure TLccTrainController.AssignTrainByDccAddress(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; UniqueID: Integer);
begin
  AssignTrainByDccTrain(IntToStr(DccAddress), IsLongAddress, SpeedSteps);
end;

procedure TLccTrainController.AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; UniqueID: Integer);
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

procedure TLccTrainController.DoConsist;
begin
  if Assigned(OnConsistResults) then
    OnConsistResults(Self);
end;

procedure TLccTrainController.DoQuerySpeedReply(ASetSpeed, ACommandSpeed, AnActualSpeed: THalfFloat; Status: Byte);
begin
  if Assigned(OnQuerySpeedReply) then
    OnQuerySpeedReply(Self, ASetSpeed, ACommandSpeed, AnActualSpeed, Status);
end;

procedure TLccTrainController.EmergencyStop(UniqueID: Integer);
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

procedure TLccTrainController.DoSearchResult(
  TrainList: TLccActionTrainInfoList; var SelectedResultIndex: Integer);
begin
  if Assigned(OnSearchResult) then
    OnSearchResult(Self, TrainList, SelectedResultIndex);
end;

procedure TLccTrainController.DoSearchMultiResult(
  Trains: TLccActionTrainInfoList);
begin
  if Assigned(OnSearchMultiResult) then
    OnSearchMultiResult(Self, Trains);
end;

procedure TLccTrainController.DoControllerAttachListenerReply(
  Listener: TLccActionTrainInfo; ReplyCode: Word);
begin
  if Assigned(OnAttachListenerReply) then
    OnAttachListenerReply(Self, Listener, ReplyCode);
end;

procedure TLccTrainController.DoControllerDetachListenerReply(
  Listener: TLccActionTrainInfo; ReplyCode: Word);
begin
  if Assigned(OnDetachListenerReply) then
    OnDetachListenerReply(Self, Listener, ReplyCode);
end;

procedure TLccTrainController.DoControllerQueryListenerGetCount(ListenerCount: Byte);
begin
 if Assigned(OnQueryListenerGetCount) then
    OnQueryListenerGetCount(Self, ListenerCount);
end;

procedure TLccTrainController.DoControllerQueryListenerIndex(ListenerCount,
  ListenerIndex, ListenerFlags: Byte; ListenerNodeID: TNodeID);
begin
  if Assigned(FOnQueryListenerIndex) then
    FOnQueryListenerIndex(Self, ListenerCount, ListenerIndex, ListenerFlags, ListenerNodeID);
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

