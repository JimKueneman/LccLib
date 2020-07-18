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

  TLccSearchResultsArray = array of TLccSearchReplyRec;

  TLccAssignTrainAction = class;

  TOnLccAssignTrainMultipleSearchResults = procedure(Sender: TLccAssignTrainAction; Results: TLccSearchResultsArray; var SelectedResultIndex: Integer) of object;
  TOnLccCommandFailed = procedure(Sender: TLccAssignTrainAction; ErrorCode: Byte) of object;
  TOnLccAssignTrain = procedure(Sender: TLccAssignTrainAction; Train: TLccSearchReplyRec) of object;

  { TLccAssignTrainAction }

  TLccAssignTrainAction = class(TLccAction)
  private
    FOnAssignFailed: TOnLccCommandFailed;
    FOnAssignTrain: TOnLccAssignTrain;
    FOnMultipleSearchResults: TOnLccAssignTrainMultipleSearchResults;
    FOnNoSearchResults: TOnLccCommandFailed;
    FRepliedSearchCriteria: TLccSearchResultsArray;
    FRepliedSearchCriterialCount: Integer;
    FRequestedSearchData: DWORD;
    FSelectedSearchResultIndex: Integer;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure DoMultipleSearchResults; virtual;
    procedure DoAssignFailed(ErrorCode: Byte); virtual;
    procedure DoNoSearchResults; virtual;
    procedure DoAssignTrain; virtual;

    procedure LoadStateArray; override;
  public
    property RequestedSearchData: DWORD read FRequestedSearchData write FRequestedSearchData;
    property RepliedSearchCriterialCount: Integer read FRepliedSearchCriterialCount;
    property RepliedSearchCriteria: TLccSearchResultsArray read FRepliedSearchCriteria;
    property SelectedSearchResultIndex: Integer read FSelectedSearchResultIndex write FSelectedSearchResultIndex;

    // Possible callback that could occur that would require user input
    property OnMultipleSearchResults: TOnLccAssignTrainMultipleSearchResults read FOnMultipleSearchResults write FOnMultipleSearchResults;
    property OnAssignFailed: TOnLccCommandFailed read FOnAssignFailed write FOnAssignFailed;
    property OnNoSearchResults: TOnLccCommandFailed read FOnNoSearchResults write FOnNoSearchResults;
    property OnAssignTrain: TOnLccAssignTrain read FOnAssignTrain write FOnAssignTrain;
  end;

  { TLccReleaseTrainAction }

  TLccReleaseTrainAction = class(TLccAction)
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;

    procedure LoadStateArray; override;
  end;

  { TLccQuerySpeedAction }

  TLccQuerySpeedAction = class(TLccAction)
  private
    FSpeed: THalfFloat;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForQuerySpeedResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure LoadStateArray; override;
  public
    property Speed: THalfFloat read FSpeed;
  end;

  { TLccQueryFunctionAction }

  TLccQueryFunctionAction = class(TLccAction)
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

type

  TControllerTrainAssignResult = (tarAssigned, tarFailTrainRefused, tarFailControllerRefused);
  TOnControllerTrainAssigned = procedure(Sender: TLccNode; Reason: TControllerTrainAssignResult) of object;
  TOnControllerTrainReleased = procedure(Sender: TLccNode) of object;
  TOnControllerQuerySpeedReply = procedure(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte) of object;
  TOnControllerQueryFunctionReply = procedure(Sender: TLccNode; Address: DWORD; Value: Word) of object;
  TOnControllerRequestTakeover = procedure(Sender: TLccNode; var Allow: Boolean) of object;

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
 //   ReservedState: TAttachedTrainReservationState;
 //   AttachedState: TAttachedTrainAssignmentState;
 //   SearchState: TAttachedTrainSearchState;
    Listeners: array of TAttachedTrain
  end;

  { TLccTrainController }

  TLccTrainController = class(TLccCanNode)
  private
    FAssignedTrain: TAttachedTrain;

    FDirection: TLccTrainDirection;
    FFunctionArray: TLccFunctions;
    FOnControllerRequestTakeover: TOnControllerRequestTakeover;
    FOnQueryFunctionReply: TOnControllerQueryFunctionReply;
    FOnQuerySpeedReply: TOnControllerQuerySpeedReply;
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

  public
    property LccAssignTrainAction: TLccAssignTrainAction read FLccAssignTrainAction write FLccAssignTrainAction;

    property AssignedTrain: TAttachedTrain read FAssignedTrain write FAssignedTrain;
    property Speed: single read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;

    property OnTrainAssigned: TOnControllerTrainAssigned read FOnTrainAssigned write FOnTrainAssigned;
    property OnTrainReleased: TOnControllerTrainReleased read FOnTrainReleased write FOnTrainReleased;
    property OnQuerySpeedReply: TOnControllerQuerySpeedReply read FOnQuerySpeedReply write FOnQuerySpeedReply;
    property OnQueryFunctionReply: TOnControllerQueryFunctionReply read FOnQueryFunctionReply write FOnQueryFunctionReply;
    property OnControllerRequestTakeover: TOnControllerRequestTakeover read FOnControllerRequestTakeover write FOnControllerRequestTakeover;

    constructor Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string); override;
    destructor Destroy; override;
    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
    procedure ReleaseTrain;
    procedure QuerySpeed;
    procedure QueryFunction(Address: Word);
    procedure EmergencyStop;
    function IsTrainAssigned: Boolean;

    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TLccReleaseTrainAction }

function TLccReleaseTrainAction._0ReceiveFirstMessage(Sender: TObject;
  SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

end;

procedure TLccReleaseTrainAction.LoadStateArray;
begin
  inherited LoadStateArray;
end;

{ TLccQueryFunctionAction }

function TLccQueryFunctionAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result :=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionQuerySpeed(NodeID, AliasID, TargetNodeID, TargetAliasID);
  SendMessage(Owner, WorkerMessage);
end;

function TLccQueryFunctionAction._1ActionWaitForQueryFunctionResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  // Only care if coming from our Assigned Train
  if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, TargetNodeID, TargetAliasID) then
  begin
    case SourceMessage.MTI of
      MTI_TRACTION_REPLY :
        begin
          case SourceMessage.DataArray[0] of
            TRACTION_QUERY_SPEED_REPLY :
              begin  // this can be turned into a request/reply action
                if Address = SourceMessage.TractionExtractFunctionAddress then
                  (Owner as TLccTrainController).DoQuerySpeedReply(SourceMessage.TractionExtractSetSpeed, SourceMessage.TractionExtractCommandedSpeed, SourceMessage.TractionExtractActualSpeed, SourceMessage.TractionExtractSpeedStatus);
              end;
          end;
        end;
      end
  end;

  if TimeoutExpired then
    _NFinalStateCleanup(Sender, SourceMessage);
end;

procedure TLccQueryFunctionAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := @_0ReceiveFirstMessage;
  States[1] := @_1ActionWaitForQueryFunctionResults;
  States[2] := @_NFinalStateCleanup;
end;

{ TLccQuerySpeedAction }

function TLccQuerySpeedAction._0ReceiveFirstMessage(Sender: TObject;
  SourceMessage: TLccMessage): Boolean;
begin
  Result :=inherited _0ReceiveFirstMessage(Sender, SourceMessage);
end;

function TLccQuerySpeedAction._1ActionWaitForQuerySpeedResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  // Only care if coming from our Assigned Train
  if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, TargetNodeID, TargetAliasID) then
  begin
    case SourceMessage.MTI of
      MTI_TRACTION_REPLY :
        begin
          case SourceMessage.DataArray[0] of
            TRACTION_QUERY_SPEED_REPLY :
              begin  // this can be turned into a request/reply action
                (Owner as TLccTrainController).DoQuerySpeedReply(SourceMessage.TractionExtractSetSpeed, SourceMessage.TractionExtractCommandedSpeed, SourceMessage.TractionExtractActualSpeed, SourceMessage.TractionExtractSpeedStatus);
              end;
          end;
        end;
      end
  end;

  if TimeoutExpired then
    _NFinalStateCleanup(Sender, SourceMessage);
end;

procedure TLccQuerySpeedAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := @_0ReceiveFirstMessage;
  States[1] := @_1ActionWaitForQuerySpeedResults;
  States[2] := @_NFinalStateCleanup;
end;

{ TLccAssignTrainAction }

function TLccAssignTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionSearch(NodeID, AliasID, RequestedSearchData);
  SendMessage(Owner, WorkerMessage);
  SetTimoutCountThreshold(1000); // seconds to collect trains
  AdvanceToNextState;
end;


function TLccAssignTrainAction._1ActionWaitForSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i: Integer;
begin
  Result := False;

  if Assigned(SourceMessage) then
  begin
    case SourceMessage.MTI of
       MTI_PRODUCER_IDENTIFIED_CLEAR,
       MTI_PRODUCER_IDENTIFIED_SET,
       MTI_PRODUCER_IDENTIFIED_UNKNOWN :
         begin
            // Search Protocol is flawed, there is no way to uniquely identify this result was from my inital call
            // I must validate the search result actually matches what I asked for .....

           WorkerMessage.LoadTractionSearch(NULL_NODE_ID, 0, RequestedSearchData);
           if SourceMessage.TractionSearchDecodeSearchString = WorkerMessage.TractionSearchDecodeSearchString then
           begin
             if RepliedSearchCriterialCount < Length(RepliedSearchCriteria) then
             begin
               RepliedSearchCriteria[RepliedSearchCriterialCount].NodeID := SourceMessage.SourceID;
               RepliedSearchCriteria[RepliedSearchCriterialCount].NodeAlias := SourceMessage.CAN.SourceAlias;
               RepliedSearchCriteria[RepliedSearchCriterialCount].SearchData := SourceMessage.TractionSearchExtractSearchData;
               RepliedSearchCriteria[RepliedSearchCriterialCount].HasSTNIP := False;
               RepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Manufacturer := '';
               RepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Owner := '';
               RepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Roadname := '';
               RepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.RoadNumber := '';
               RepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.TrainClass := '';
               RepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.TrainName := '';
               RepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Version := 0;
               WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(NodeID, AliasID, RepliedSearchCriteria[RepliedSearchCriterialCount].NodeID, RepliedSearchCriteria[RepliedSearchCriterialCount].NodeAlias);
               SendMessage(Owner, WorkerMessage);
               Inc(FRepliedSearchCriterialCount);
             end else
               AdvanceToNextState;    // No more slots to hold results
           end
        end;
       MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY :
         begin
            i := 0;
            // find the right Node that this SNIP belongs to
            while i < RepliedSearchCriterialCount do
            begin
              if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, RepliedSearchCriteria[RepliedSearchCriterialCount].NodeID, RepliedSearchCriteria[RepliedSearchCriterialCount].NodeAlias) then
              begin
                RepliedSearchCriteria[RepliedSearchCriterialCount].HasSTNIP := True;
                with RepliedSearchCriteria[RepliedSearchCriterialCount].STNIP do
                  SourceMessage.ExtractSimpleTrainNodeIdentInfoReply(Version, RoadName, TrainClass, RoadNumber, TrainName, Manufacturer, Owner);
              end;
               Inc(i);
            end;
         end;
    end;
  end;


  if Cancel then
  begin
    _NFinalStateCleanup(Sender, SourceMessage);
    Exit;
  end;

  if TimeoutExpired then    // Times up, gather them up.....
  begin
    if RepliedSearchCriterialCount > 0 then
    begin
      if RepliedSearchCriterialCount = 1 then
      begin
        SelectedSearchResultIndex := 0;
          // Atomic action no need for Reservation (Manage)
        WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeAlias, NodeID, AliasID);
        SendMessage(Owner, WorkerMessage);
        SetTimoutCountThreshold(5000); // 5 seconds to assign the train
        AdvanceToNextState;
      end else
      begin
        DoMultipleSearchResults;
        if (SelectedSearchResultIndex < 0) or (SelectedSearchResultIndex > Length(RepliedSearchCriteria)-1) then
          _NFinalStateCleanup(Sender, SourceMessage)    // out of bounds, just quit
        else begin  // Reply the item at SelectedSearchResultIndex
          WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeAlias, NodeID, AliasID);
          SendMessage(Owner, WorkerMessage);
          SetTimoutCountThreshold(5000); // 5 seconds to assign the train
          AdvanceToNextState;
        end
      end;
    end else
    begin // Found Nothing, just quit
      DoNoSearchResults;
      _NFinalStateCleanup(Sender, SourceMessage);
    end;
  end;
end;

function TLccAssignTrainAction._2ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  if Assigned(SourceMessage) then
  begin // Only care if it is coming from our potental Train
    if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, RepliedSearchCriteria[SelectedSearchResultIndex].NodeID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeAlias) then
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
                         case SourceMessage.DataArray[2] of
                           S_OK :
                             begin
                               DoAssignTrain;
                                _NFinalStateCleanup(Sender, SourceMessage);
                             end;
                           TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
                             begin
                                DoAssignFailed(TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER);
                                _NFinalStateCleanup(Sender,SourceMessage);
                             end;
                           TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                             begin
                                DoAssignFailed(TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN);
                                _NFinalStateCleanup(Sender, SourceMessage);
                             end;
                         end;
                       end;
                   end;
                 end;
             end;
           end;
      end;
    end;
  end;

  if TimeoutExpired or Cancel then
  begin // Just in case, can't hurt
    if (SelectedSearchResultIndex > -1) and (SelectedSearchResultIndex < Length(FRepliedSearchCriteria)) then
    begin
      WorkerMessage.LoadTractionControllerRelease(NodeID, AliasID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeAlias, NodeID, AliasID);
      SendMessage(Owner, WorkerMessage);
    end;
    _NFinalStateCleanup(Self, SourceMessage)
  end;
end;

procedure TLccAssignTrainAction.DoAssignFailed(ErrorCode: Byte);
begin
  if Assigned(OnAssignFailed) then
    OnAssignFailed(Self, ErrorCode);
end;

procedure TLccAssignTrainAction.DoAssignTrain;
begin
  if Assigned(OnAssignTrain) then
    OnAssignTrain(Self, RepliedSearchCriteria[SelectedSearchResultIndex]);
end;

procedure TLccAssignTrainAction.DoMultipleSearchResults;
begin
  FSelectedSearchResultIndex := 0;
  if Assigned(OnMultipleSearchResults) then
    OnMultipleSearchResults(Self, RepliedSearchCriteria, FSelectedSearchResultIndex);
end;

procedure TLccAssignTrainAction.DoNoSearchResults;
begin
  if Assigned(OnNoSearchResults) then
    OnNoSearchResults(Self, 0);
end;

procedure TLccAssignTrainAction.LoadStateArray;
begin
  SetStateArrayLength(4);
  States[0] := @_0ReceiveFirstMessage;
  States[1] := @_1ActionWaitForSearchResults;
  States[2] := @_2ActionWaitForAssignThrottleResult;
  States[3] := @_NFinalStateCleanup;

  SetLength(FRepliedSearchCriteria, 20);  // 20 Results is enough
  FRepliedSearchCriterialCount := 0;
  FSelectedSearchResultIndex := -1;
end;


{ TLccTrainController }

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
var
  LocalSearchData: DWORD;
begin
 { ClearAssignedTrain;
  FAssignedTrain.RepliedSearchData := 0;
  FAssignedTrain.RequestedSearchData := 0;
  FAssignedTrain.SearchState := tssSearching;
  LocalSearchData := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchData);
  FAssignedTrain.RequestedSearchData := LocalSearchData;

  LccAssignTrainAction.RequestedSearchData := LocalSearchData;
  LccActions.RegisterAction(Self, nil, LccAssignTrainAction);  }
end;

procedure TLccTrainController.ReleaseTrain;
begin
{  if AssignedTrain.AttachedState = tasAssigned then
  begin
    FAssignedTrain.AttachedState := tasUnAssigning;
    FAssignedTrain.ReservedState := trsReserving;
    WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, True);
    SendMessageFunc(Self, WorkerMessage);
    // Wait for Result of Manage
  end;    }
end;

procedure TLccTrainController.SetDirection(AValue: TLccTrainDirection);
begin
{  FDirection := AValue;
  if AssignedTrain.AttachedState = tasAssigned then
  begin
    if Direction = tdForward then
      WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, Speed)
    else
      WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, -Speed);
    SendMessageFunc(Self, WorkerMessage);
  end;  }
end;

procedure TLccTrainController.SetFunctions(Index: Integer; AValue: Word);
begin
 { if (Index >= 0) and (Index < High(FunctionArray)) then
  begin
    FFunctionArray[Index] := AValue;
    if AssignedTrain.AttachedState = tasAssigned then
    begin
      WorkerMessage.LoadTractionSetFunction(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, Index, AValue);
      SendMessageFunc(Self, WorkerMessage);
    end;
  end;  }
end;

procedure TLccTrainController.SetSpeed(AValue: single);
begin
 { FSpeed := Abs(AValue);
  if AssignedTrain.AttachedState = tasAssigned then
  begin
    if Direction = tdForward then
      WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, AValue)
    else
      WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, -AValue);
    SendMessageFunc(Self, WorkerMessage);
  end;  }
end;

function TLccTrainController.ProcessMessage(SourceMessage: TLccMessage): Boolean;
var
  AllowTakeOver: Boolean;
begin
  Result :=inherited ProcessMessage(SourceMessage);

  // We only are dealing with messages with destinations for us from here on
  if SourceMessage.HasDestination then
  begin
    if not IsDestinationEqual(SourceMessage) then
      Exit;
  end;

  // Only care if coming from our Assigned Train
  if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, AssignedTrain.NodeID, AssignedTrain.AliasID) then
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
begin
  if IsTrainAssigned then
  begin
    WorkerMessage.LoadTractionQueryFunction(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, Address);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccTrainController.QuerySpeed;
begin
  if IsTrainAssigned then
    LccActions.RegisterAction(Self, nil, TLccQueryFunctionAction.Create(Self, NodeID, AliasID));
end;

constructor TLccTrainController.Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string);
begin
  inherited Create(ASendMessageFunc, ANodeManager, CdiXML);
  FLccAssignTrainAction := TLccAssignTrainAction.Create(Self, NodeID, AliasID);
end;

procedure TLccTrainController.AssignTrainByDccAddress(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
begin
  AssignTrainByDccTrain(IntToStr(DccAddress), IsLongAddress, SpeedSteps);
end;

procedure TLccTrainController.AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
var
  TrackProtocolFlags: Word;
  LocalSearchData: DWORD;
begin

  TrackProtocolFlags := TRACTION_SEARCH_TARGET_ANY_MATCH or TRACTION_SEARCH_ALLOCATE_FORCE or TRACTION_SEARCH_TYPE_ALL_MATCH or
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
  {$IFNDEF DWSCRIPT}
  FillChar(FAssignedTrain, Sizeof(AssignedTrain), 0);
  {$ELSE}
  //   TODO Need to do this manually in SMS
  FAssignedTrain.NodeID := NULL_NODE_ID;
  FAssignedTrain.AliasID := 0;
  FAssignedTrain.RequestedSearchData := 0;
  FAssignedTrain.RepliedSearchData := 0;
  FAssignedTrain.SearchString := '';
  FAssignedTrain.ReservedState := trsNotReserved;
  FAssignedTrain.AttachedState := tasNotAssigned;
  FAssignedTrain.SearchState := tssNotSearching;
  FAssignedTrain.Listeners := nil;
  {$ENDIF}
end;

destructor TLccTrainController.Destroy;
begin
  {$IFDEF GWSCRIPT}
  LccAssignTrainAction.Free;
  {$ELSE}
  FreeAndNil(FLccAssignTrainAction);
  {$ENDIF}
  inherited Destroy;
end;

procedure TLccTrainController.DoControllerTakeOver(var Allow: Boolean);
begin
  If Assigned(OnControllerRequestTakeover) then
    OnControllerRequestTakeover(Self, Allow);
end;

function TLccTrainController.GetCdiFile: string;
begin
  Result := CDI_XML_CONTROLLER;
end;

function TLccTrainController.GetFunctions(Index: Integer): Word;
begin
  Result := 0;
  if (Index >= 0) and (Index < High(FunctionArray)) then
    Result := FunctionArray[Index];
end;

function TLccTrainController.IsTrainAssigned: Boolean;
begin
  Result := (AssignedTrain.NodeID[0] <> 0) or (AssignedTrain.NodeID[1] <> 0) or (AssignedTrain.AliasID <> 0);
end;


end.

