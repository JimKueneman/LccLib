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

  TLccSTNIP = record
    Version: Word;
    Roadname: string;
    TrainClass: string;
    RoadNumber: string;
    TrainName: string;
    Manufacturer: string;
    Owner: string;
  end;

  TLccSearchReplyRec = record
    SearchData: DWORD;
    NodeID: TNodeID;
    NodeAlias: Word;
    HasSTNIP: Boolean;
    STNIP: TLccSTNIP;
  end;

  TDccTrain = class(TObject)
  public
    DccAddress: Word;
    SpeedStep: TLccDccSpeedStep;
    LongAddress: Boolean;
  end;

  TLccSearchResultsArray = array of TLccSearchReplyRec;
  TControllerTrainAssignResult = (tarAssigned, tarFailTrainRefused, tarFailControllerRefused);

  TLccSearchAndAssignTrainAction = class;

  { TDccTrainList }

  TDccTrainList = class(TObjectList)
  private
    function GetTrains(Index: Integer): TDccTrain;
    procedure SetTrains(Index: Integer; AValue: TDccTrain);
  public
    property Trains[Index: Integer]: TDccTrain read GetTrains write SetTrains;
  end;

  { TLccTractionBuildConsist }

  TLccTractionBuildConsist = class(TLccTrainAction)
  private
    FRepliedSearchCriteria: TLccSearchResultsArray;
    FRequestedSearchData: DWORD;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionGatherSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionWaitOnVerifyNodeID(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _3ActionBuildConsist(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  public
    property RequestedSearchData: DWORD read FRequestedSearchData write FRequestedSearchData;
    property RepliedSearchCriteria: TLccSearchResultsArray read FRepliedSearchCriteria;

    function AddTrainSearchCritera(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep): TLccSearchReplyRec;
  end;

  { TLccSearchAndGatherTrainsAction }

  TLccSearchAndGatherTrainsAction = class(TLccTrainAction)
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

  { TLccAssignTrainAction }

  TLccAssignTrainAction = class(TLccTrainAction)
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure DoAssigned(ResultCode: TControllerTrainAssignResult); virtual;

    procedure LoadStateArray; override;
  end;

  { TLccSearchAndAssignTrainAction }

  TLccSearchAndAssignTrainAction = class(TLccTrainAction)
  private
    FSearchCriteria: DWORD;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForTrainSearch(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionWaitForAssignTrain(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure CompleteCallback(SourceAction: TLccAction); override;
    procedure LoadStateArray; override;
  public
    property SearchCriteria: DWORD read FSearchCriteria write FSearchCriteria;
  end;

  { TLccTractionReleaseTrainAction }

  TLccTractionReleaseTrainAction = class(TLccTrainAction)
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;

    procedure LoadStateArray; override;
  end;

  { TLccTractionQuerySpeedAction }

  TLccTractionQuerySpeedAction = class(TLccTrainAction)
  private
    FSpeed: THalfFloat;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForQuerySpeedResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  public
    property Speed: THalfFloat read FSpeed;
  end;

  { TLccTractionQueryFunctionAction }

  TLccTractionQueryFunctionAction = class(TLccTrainAction)
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

  { TLccTractionAttachListenerAction }

  TLccTractionAttachListenerAction = class(TLccTrainAction)
  private
    FListenerNodeID: TNodeID;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  public
    property ListenerNodeID: TNodeID read FListenerNodeID write FListenerNodeID;
  end;

  { TLccTractionDetachListenerAction }

  TLccTractionDetachListenerAction = class(TLccTrainAction)
  private
      FListenerNodeID: TNodeID;
    protected
      function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
      function _1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;

      procedure LoadStateArray; override;
    public
      property ListenerNodeID: TNodeID read FListenerNodeID write FListenerNodeID;
    end;

  { TLccTractionQueryListenerAction }

  TLccTractionQueryListenerAction = class(TLccTrainAction)

  end;


type

  TOnControllerSearchResult = procedure(Sender: TLccTrainAction; var SelectedResultIndex: Integer) of object;
  TOnControllerTrainAssigned = procedure(Sender: TLccNode; Reason: TControllerTrainAssignResult) of object;
  TOnControllerTrainReleased = procedure(Sender: TLccNode) of object;
  TOnControllerQuerySpeedReply = procedure(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte) of object;
  TOnControllerQueryFunctionReply = procedure(Sender: TLccNode; Address: DWORD; Value: Word) of object;
  TOnControllerRequestTakeover = procedure(Sender: TLccNode; var Allow: Boolean) of object;
  TOnControllerAttachListener = procedure(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word) of object;
  TOnControllerDetachListener = procedure(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word) of object;
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
    FOnAttachListener: TOnControllerAttachListener;
    FOnControllerRequestTakeover: TOnControllerRequestTakeover;
    FOnDetachListener: TOnControllerDetachListener;
    FOnQueryFunctionReply: TOnControllerQueryFunctionReply;
    FOnQueryListenerGetCount: TOnControllerQueryListenerGetCount;
    FOnQueryListenerIndex: TOnControllerQueryListenerIndex;
    FOnQuerySpeedReply: TOnControllerQuerySpeedReply;
    FOnSearchResult: TOnControllerSearchResult;
    FOnTrainAssigned: TOnControllerTrainAssigned;
    FOnTrainReleased: TOnControllerTrainReleased;
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
    procedure DoSearchResult(Sender: TLccTrainAction; var SelectedResultIndex: Integer); virtual;
    procedure DoControllerAttachListener(ListenerNodeID: TNodeID; ReplyCode: Byte); virtual;
    procedure DoControllerDetachListener(ListenerNodeID: TNodeID; ReplyCode: Byte); virtual;
    procedure DoControllerQueryListenerGetCount(ListenerCount: Byte); virtual;
    procedure DoControllerQueryListenerIndex(ListenerIndex: Byte; ListenerFlags: Byte; ListenerNodeID: TNodeID); virtual;

  public
    property AssignedTrain: TAttachedTrain read FAssignedTrain write FAssignedTrain;
    property Speed: single read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;

    property OnTrainAssigned: TOnControllerTrainAssigned read FOnTrainAssigned write FOnTrainAssigned;
    property OnTrainReleased: TOnControllerTrainReleased read FOnTrainReleased write FOnTrainReleased;
    property OnQuerySpeedReply: TOnControllerQuerySpeedReply read FOnQuerySpeedReply write FOnQuerySpeedReply;
    property OnQueryFunctionReply: TOnControllerQueryFunctionReply read FOnQueryFunctionReply write FOnQueryFunctionReply;
    property OnControllerRequestTakeover: TOnControllerRequestTakeover read FOnControllerRequestTakeover write FOnControllerRequestTakeover;
    property OnSearchResult: TOnControllerSearchResult read FOnSearchResult write FOnSearchResult;
    property OnAttachListener: TOnControllerAttachListener read FOnAttachListener write FOnAttachListener;
    property OnDetachListener: TOnControllerDetachListener read FOnDetachListener write FOnDetachListener;
    property OnQueryListenerGetCount: TOnControllerQueryListenerGetCount read FOnQueryListenerGetCount write FOnQueryListenerGetCount;
    property OnQueryListenerIndex: TOnControllerQueryListenerIndex read FOnQueryListenerIndex write FOnQueryListenerIndex;

    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
    procedure AssignConsist(TrainList: TDccTrainList);
    procedure AttachListener(ATrainNodeID: TNodeID; ATrainNodeAliasID: Word; AListenerNodeID: TNodeID);
    procedure DetachListener(ATrainNodeID: TNodeID; ATrainNodeAliasID: Word; AListenerNodeID: TNodeID);
    procedure QueryListeners(ATrainNodeID: TNodeID);
    procedure ReleaseTrain;
    procedure QuerySpeed;
    procedure QueryFunction(Address: Word);
    procedure QueryFunctions;
    procedure EmergencyStop;
    function IsTrainAssigned: Boolean;

    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TLccAssignTrainAction }

function TLccAssignTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  // Don't use SourceMessge so no need to check for nil
  if Trains.Count > 0 then
  begin
    WorkerMessage.LoadTractionControllerAssign(SourceNodeID, SourceAliasID, Trains[0].NodeID, Trains[0].AliasID, SourceNodeID);
    SendMessage(Owner, WorkerMessage);
    SetTimoutCountThreshold(TIMEOUT_CONTROLLER_NOTIFY_WAIT * 2); // seconds to assign the train, the command station will give 5 seconds to receive a reply from an existing controller to give it up
  end;

  AdvanceToNextState;
end;

function TLccAssignTrainAction._1ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := False;

  // SourceMessagfe could be nil from a timer
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
                                    DoAssigned(tarAssigned);
                                 end;
                               end;
                             TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER : DoAssigned(tarFailTrainRefused);
                             TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN               : DoAssigned(tarFailControllerRefused);
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
    AdvanceToNextState;
  end;
end;

procedure TLccAssignTrainAction.DoAssigned(ResultCode: TControllerTrainAssignResult);
var
  ControllerNode: TLccTrainController;
begin
  ControllerNode := Owner as TLccTrainController;
  if Assigned(ControllerNode) then
  begin
    if Assigned(ControllerNode.OnTrainAssigned) then
    begin
      ControllerNode.OnTrainAssigned(ControllerNode, ResultCode);
    end;
  end;
end;

procedure TLccAssignTrainAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForAssignThrottleResult;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccSearchAndGatherTrainsAction }

function TLccSearchAndGatherTrainsAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i: Integer;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  // Don't use SourceMessge so no need to check for nil
  WorkerMessage.LoadTractionSearch(SourceNodeID, SourceAliasID, SearchCriteria);
  SendMessage(Owner, WorkerMessage);

  SetTimoutCountThreshold(Round(1000)); // seconds to collect trains
  AdvanceToNextState;
end;

function TLccSearchAndGatherTrainsAction._1ActionGatherSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  TrainVersion: Byte;
  TrainRoadName,
  TrainClass,
  TrainRoadNumber,
  TrainName,
  TrainManufacturer,
  TrainOwner: string;
  LocalTrain: TLccTrainActionInfo;
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
               // Send a message back to the Train Node from this controller asking for the SNIP
               WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(SourceNodeID, SourceAliasID, LocalTrain.NodeID, LocalTrain.AliasID);
               SendMessage(Owner, WorkerMessage);
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
    AdvanceToNextState;
end;

function TLccSearchAndGatherTrainsAction._2ActionReportSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
  iSelectedTrain: Integer;
  SelectedTrain: TLccTrainActionInfo;
begin
  Result := False;

  IgnoreTimer := True;  // Stop reentrancy
  try
    ControllerNode := Owner as TLccTrainController;

    if Trains.Count > 0 then
    begin
      iSelectedTrain := 0;
      if Assigned(ControllerNode.OnSearchResult) then
      begin
        iSelectedTrain := 0;
        ControllerNode.OnSearchResult(Self, iSelectedTrain);
        if (iSelectedTrain < 0) or (iSelectedTrain >= Trains.Count) then
          iSelectedTrain := 0;
      end;
      // Only have the selected train in the Trains property
      SelectedTrain := Trains[iSelectedTrain];
      Trains.Remove(SelectedTrain);
      Trains.Clear;
      Trains.Add(SelectedTrain);
    end;
  finally
    AdvanceToNextState;
    IgnoreTimer := False;
  end;
end;

procedure TLccSearchAndGatherTrainsAction.LoadStateArray;
begin
  SetStateArrayLength(4);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionGatherSearchResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2ActionReportSearchResults;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TDccTrainList }

function TDccTrainList.GetTrains(Index: Integer): TDccTrain;
begin
  Result := Items[Index] as TDccTrain;
end;

procedure TDccTrainList.SetTrains(Index: Integer; AValue: TDccTrain);
begin
  Items[Index] := AValue
end;

{ TLccTractionBuildConsist }

procedure TLccTractionBuildConsist.LoadStateArray;
begin
  SetStateArrayLength(5);

  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionGatherSearchResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2ActionWaitOnVerifyNodeID;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_3ActionBuildConsist;
  States[4] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

function TLccTractionBuildConsist.AddTrainSearchCritera(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep): TLccSearchReplyRec;
var
  TrackProtocolFlags: Word;
begin
  {$IFDEF DWSCRIPT}
  FRepliedSearchCriteria.SetLength( FRepliedSearchCriteria.Length + 1);
  {$ELSE}
  SetLength(FRepliedSearchCriteria, Length(FRepliedSearchCriteria) + 1);
  {$ENDIF}

  Result := RepliedSearchCriteria[Length(RepliedSearchCriteria) - 1];

  Result.NodeID := NULL_NODE_ID;
  Result.HasSTNIP := False;
  Result.NodeAlias := 0;
  Result.SearchData := 0;
  Result.STNIP.Manufacturer := '';
  Result.STNIP.Owner := '';
  Result.STNIP.Roadname := '';
  Result.STNIP.Manufacturer := '';
  Result.STNIP.TrainName := '';
  Result.STNIP.RoadNumber := '';
  Result.STNIP.Version := 0;

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

  WorkerMessage.TractionSearchEncodeSearchString(IntToStr(DccAddress), TrackProtocolFlags, Result.SearchData);
end;

function TLccTractionBuildConsist._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i: Integer;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  // Gather up the Trains
  for i := 0 to Length(RepliedSearchCriteria) do
  begin
    WorkerMessage.LoadTractionSearch(SourceNodeID, SourceAliasID, RepliedSearchCriteria[i].SearchData);
    SendMessage(Owner, WorkerMessage);
  end;
  SetTimoutCountThreshold(1000); // seconds to collect trains
  AdvanceToNextState;
end;

function TLccTractionBuildConsist._1ActionGatherSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i, ReturnedCount: Integer;
begin
  Result := False;

  IgnoreTimer := True;
  try
    if Assigned(SourceMessage) then
    begin
      case SourceMessage.MTI of
         MTI_PRODUCER_IDENTIFIED_CLEAR,
         MTI_PRODUCER_IDENTIFIED_SET,
         MTI_PRODUCER_IDENTIFIED_UNKNOWN :
           begin
             if SourceMessage.TractionSearchIsEvent then
             begin
               ReturnedCount := 0;
               i := 0;
               while i < Length(RepliedSearchCriteria) do
               begin
                 if RepliedSearchCriteria[i].SearchData = SourceMessage.TractionSearchExtractSearchData then
                 begin
                   if Owner.GridConnect then
                   begin   // Need to get the NodeID in a GridConnect CAN connection
                     RepliedSearchCriteria[i].NodeAlias := SourceMessage.CAN.SourceAlias;
                     WorkerMessage.LoadVerifyNodeID(SourceNodeID, SourceAliasID, NULL_NODE_ID);
                     SendMessage(Self, WorkerMessage);
                   end else // TCP so the NodeID is valid
                     RepliedSearchCriteria[i].NodeID := SourceMessage.SourceID;
                   i := Length(RepliedSearchCriteria);
                 end;

                 // Count the number of Trains that have returned from the Search Reqeust
                 if (RepliedSearchCriteria[i].NodeAlias <> 0) or (not NullNodeID(RepliedSearchCriteria[i].NodeID)) then
                   Inc(ReturnedCount);
                 Inc(i);
               end;
               // is every one returned?
               if ReturnedCount = Length(RepliedSearchCriteria) then
               begin
                 SetTimoutCountThreshold(1000); // seconds to collect trains
                 AdvanceToNextState;
               end;
             end;
           end;
      end
    end;

  finally
    IgnoreTimer := False;
  end;


  if TimeoutExpired then    // Times up, report out....
    AdvanceToNextState;
end;

function TLccTractionBuildConsist._2ActionWaitOnVerifyNodeID(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i, ReturnedCount: Integer;
begin
  Result := False;

  if Owner.GridConnect then
  begin
    if Assigned(SourceMessage) then   // Could be time tick calling and we need to wait for a real message
    begin
      case SourceMessage.MTI of
        MTI_VERIFIED_NODE_ID_NUMBER :
          begin
            ReturnedCount := 0;
            i := 0;
            while i < Length(RepliedSearchCriteria) do
            begin
              if RepliedSearchCriteria[i].NodeAlias = SourceMessage.CAN.SourceAlias then
              begin
                SourceMessage.ExtractDataBytesAsNodeID(0, RepliedSearchCriteria[i].NodeID);
                i := Length(RepliedSearchCriteria);
                SetTimoutCountThreshold(1000); // Reset the Wait for the next one
              end;
              // Count the number of valid Trains returned
              if (RepliedSearchCriteria[i].NodeAlias <> 0) and (not NullNodeID(RepliedSearchCriteria[i].NodeID)) then
                Inc(ReturnedCount);
              Inc(i);
            end;

            // is every one returned a Verified Node?
            if ReturnedCount = Length(RepliedSearchCriteria) then
              AdvanceToNextState;
          end;
      end;
    end;

    if TimeoutExpired then    // Times up, report out....
      AdvanceToNextState;
  end else
    AdvanceToNextState;
end;

function TLccTractionBuildConsist._3ActionBuildConsist(Sender: TObject;
  SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  AdvanceToNextState;
end;

{ TLccTractionDetachListenerAction }

function TLccTractionDetachListenerAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionListenerDetach(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, ListenerNodeID);
  SendMessage(Self, WorkerMessage);
  SetTimoutCountThreshold(1000);
  AdvanceToNextState;
end;

function TLccTractionDetachListenerAction._1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
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
                           ControllerNode.DoControllerDetachListener(TempNodeID, ReplyCode);
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
    AdvanceToNextState;
  end;
end;

procedure TLccTractionDetachListenerAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1WaitforReply;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccTractionAttachListenerAction }

function TLccTractionAttachListenerAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result:=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionListenerAttach(SourceNodeID, SourceAliasID, DestNodeID, DestAliasID, ListenerNodeID);
  SendMessage(Self, WorkerMessage);
  SetTimoutCountThreshold(1000);
  AdvanceToNextState;
end;

function TLccTractionAttachListenerAction._1WaitforReply(Sender: TObject; SourceMessage: TLccMessage): Boolean;
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
                           ControllerNode.DoControllerAttachListener(TempNodeID, ReplyCode);
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
    AdvanceToNextState;
  end;
end;

procedure TLccTractionAttachListenerAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1WaitforReply;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccTractionReleaseTrainAction }

function TLccTractionReleaseTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  ControllerNode := Owner as TLccTrainController;
  if Assigned(ControllerNode) then
  begin
    if ControllerNode.IsTrainAssigned then
    begin
      WorkerMessage.LoadTractionControllerRelease(SourceNodeID, SourceAliasID, ControllerNode.AssignedTrain.NodeID, ControllerNode.AssignedTrain.AliasID, SourceNodeID, SourceAliasID);
      SendMessage(ControllerNode, WorkerMessage);
      ControllerNode.ClearAssignedTrain;
    end;

    IgnoreTimer := True;
    try
      ControllerNode.DoTrainReleased;
    finally
      AdvanceToNextState;
      IgnoreTimer := False;
    end;
  end;
end;

procedure TLccTractionReleaseTrainAction.LoadStateArray;
begin
  inherited LoadStateArray;
end;

{ TLccTractionQueryFunctionAction }

function TLccTractionQueryFunctionAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
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
  SetTimoutCountThreshold(20000);
end;

function TLccTractionQueryFunctionAction._1ActionWaitForQueryFunctionResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
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
    AdvanceToNextState;
end;

procedure TLccTractionQueryFunctionAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForQueryFunctionResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccTractionQuerySpeedAction }

function TLccTractionQuerySpeedAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
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
  SetTimoutCountThreshold(20000);
end;

function TLccTractionQuerySpeedAction._1ActionWaitForQuerySpeedResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
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
    _NFinalStateCleanup(Sender, SourceMessage);
end;

procedure TLccTractionQuerySpeedAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForQuerySpeedResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccSearchAndAssignTrainAction }

function TLccSearchAndAssignTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  LocalAction: TLccSearchAndGatherTrainsAction;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  Trains := TLccActionTrainList.Create;
  LocalAction := TLccSearchAndGatherTrainsAction.Create(Owner, SourceNodeID, SourceAliasID, NULL_NODE_ID, 0, Trains);
  LocalAction.RegisterCallBack(Self);
  LocalAction.SearchCriteria := SearchCriteria;
  Owner.LccActions.RegisterAndKickOffAction(LocalAction, nil);

  SetTimoutCountThreshold(1000); // seconds to collect trains
  AdvanceToNextState;
end;


function TLccSearchAndAssignTrainAction._1ActionWaitForTrainSearch(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  if Terminated then
  begin
    AdvanceToNextState;
    Exit;
  end;

   // Waiting for the CompleteCallback or time expires

  if TimeoutExpired then    // Times up, report out....
    AdvanceToNextState;
end;

function TLccSearchAndAssignTrainAction._2ActionWaitForAssignTrain(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;
  if Terminated then
  begin
    AdvanceToNextState;
    Exit;
  end;

  //

  if TimeoutExpired then
  begin
    AdvanceToNextState;
  end;
end;

procedure TLccSearchAndAssignTrainAction.CompleteCallback(SourceAction: TLccAction);
var
  LocalAction: TLccAssignTrainAction;
begin
  if SourceAction is TLccSearchAndGatherTrainsAction then
  begin
    if Trains.Count > 0 then
    begin
      LocalAction := TLccAssignTrainAction.Create(Owner, SourceNodeID, SourceAliasID, Trains[0].NodeID, Trains[0].AliasID, Trains);
      LocalAction.RegisterCallBack(Self);
      Owner.LccActions.RegisterAndKickOffAction(LocalAction, nil);
      SetTimoutCountThreshold(1000); // seconds to collect Assign Train
    end else
      Terminated := True;
    AdvanceToNextState;
  end else
  if SourceAction is TLccAssignTrainAction then
  begin
    AdvanceToNextState;
  end;
end;

procedure TLccSearchAndAssignTrainAction.LoadStateArray;
begin
  SetStateArrayLength(4);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForTrainSearch;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2ActionWaitForAssignTrain;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;


{ TLccTrainController }

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
var
  LocalSearchData: DWORD;
  LccAssignTrainAction: TLccSearchAndAssignTrainAction;
begin
  ClearAssignedTrain;
  FAssignedTrain.RepliedSearchData := 0;
  FAssignedTrain.RequestedSearchData := 0;
  LocalSearchData := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchData);
  FAssignedTrain.RequestedSearchData := LocalSearchData;

  LccAssignTrainAction := TLccSearchAndAssignTrainAction.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0, nil);
  LccAssignTrainAction.SearchCriteria := LocalSearchData;
  LccActions.RegisterAndKickOffAction(LccAssignTrainAction, nil);
end;

procedure TLccTrainController.AssignConsist(TrainList: TDccTrainList);
var
  LccTractionBuildConsist: TLccTractionBuildConsist;
  i: Integer;
begin
  LccTractionBuildConsist := TLccTractionBuildConsist.Create(Self, NodeID, AliasID, NULL_NODE_ID, 0, nil);
  for i := 0 to TrainList.Count - 1 do
    LccTractionBuildConsist.AddTrainSearchCritera(TrainList.Trains[i].DccAddress, TrainList.Trains[i].LongAddress, TrainList.Trains[i].SpeedStep);
  LccActions.RegisterAndKickOffAction(LccTractionBuildConsist, nil);
end;

procedure TLccTrainController.AttachListener(ATrainNodeID: TNodeID; ATrainNodeAliasID: Word; AListenerNodeID: TNodeID);
var
  LccTractionAttachListenerAction: TLccTractionAttachListenerAction;
begin
  if IsTrainAssigned then
  begin
    LccTractionAttachListenerAction := TLccTractionAttachListenerAction.Create(Self, NodeID, AliasID, ATrainNodeID, ATrainNodeAliasID, nil);
    LccTractionAttachListenerAction.ListenerNodeID := AListenerNodeID;
    LccActions.RegisterAndKickOffAction(LccTractionAttachListenerAction, nil);
  end;
end;

procedure TLccTrainController.DetachListener(ATrainNodeID: TNodeID; ATrainNodeAliasID: Word; AListenerNodeID: TNodeID);
var
  LccTractionDetachListenerAction: TLccTractionDetachListenerAction;
begin
  if IsTrainAssigned then
  begin
    LccTractionDetachListenerAction := TLccTractionDetachListenerAction.Create(Self, NodeID, AliasID, ATrainNodeID, ATrainNodeAliasID, nil);
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
    LccActions.RegisterAndKickOffAction(TLccTractionReleaseTrainAction.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, nil), nil);
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
  LccQueryFunctionAction: TLccTractionQueryFunctionAction;
begin
  if IsTrainAssigned then
  begin
    LccQueryFunctionAction := TLccTractionQueryFunctionAction.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, nil);
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

procedure TLccTrainController.QuerySpeed;
begin
  if IsTrainAssigned then
    LccActions.RegisterAndKickOffAction(TLccTractionQuerySpeedAction.Create(Self, NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, nil), nil);
end;

procedure TLccTrainController.AssignTrainByDccAddress(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
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

procedure TLccTrainController.DoSearchResult(Sender: TLccTrainAction; var SelectedResultIndex: Integer);
begin
  if Assigned(OnSearchResult) then
    OnSearchResult(Sender, SelectedResultIndex);
end;

procedure TLccTrainController.DoControllerAttachListener(ListenerNodeID: TNodeID; ReplyCode: Byte);
begin
  if Assigned(OnAttachListener) then
    OnAttachListener(Self, ListenerNodeID, ReplyCode);
end;

procedure TLccTrainController.DoControllerDetachListener(ListenerNodeID: TNodeID; ReplyCode: Byte);
begin
  if Assigned(OnDetachListener) then
    OnDetachListener(Self, ListenerNodeID, ReplyCode);
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

