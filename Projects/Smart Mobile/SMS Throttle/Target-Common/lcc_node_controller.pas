unit lcc_node_controller;
interface

{$IFNDEF DWSCRIPT}
{$I lcc_compilers.inc}
{$ENDIF}

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
    SearchData: Word;
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
    function _2ActionShowUserSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _3ActionSendAssignThrottle(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _4ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _5ActionAssignTrain(Sender: TObject; SourceMessage: TLccMessage): Boolean;

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

     property OnMultipleSearchResults: TOnLccAssignTrainMultipleSearchResults read FOnMultipleSearchResults write FOnMultipleSearchResults;
     property OnAssignFailed: TOnLccCommandFailed read FOnAssignFailed write FOnAssignFailed;
     property OnNoSearchResults: TOnLccCommandFailed read FOnNoSearchResults write FOnNoSearchResults;
     property OnAssignTrain: TOnLccAssignTrain read FOnAssignTrain write FOnAssignTrain;
  end;

type

  TControllerCallBackMessages = (ccbReservedFail, ccbAssignFailTrainRefused, ccbAssignFailControllerRefused, ccbControllerAssigned, ccbControllerUnassigned);
  TOnControllerCallBack = procedure(Sender: TLccNode; Reason: TControllerCallBackMessages) of object;
  TOnControllerQuerySpeed = procedure(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte) of object;
  TOnControllerQueryFunction = procedure(Sender: TLccNode; Address: DWORD; Value: Word) of object;
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
    ReservedState: TAttachedTrainReservationState;
    AttachedState: TAttachedTrainAssignmentState;
    SearchState: TAttachedTrainSearchState;
    Listeners: array of TAttachedTrain
  end;

  { TLccTrainController }

  TLccTrainController = class(TLccCanNode)
  private
    FAssignedTrain: TAttachedTrain;
    FDirection: TLccTrainDirection;
    FFunctionArray: TLccFunctions;
    FOnControllerRequestTakeover: TOnControllerRequestTakeover;
    FOnMessageCallback: TOnControllerCallBack;
    FOnQueryFunction: TOnControllerQueryFunction;
    FOnQuerySpeed: TOnControllerQuerySpeed;
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
    procedure DoMessageCallback(AMessage: TControllerCallBackMessages); virtual;
    procedure DoQuerySpeed(ASetSpeed, ACommandSpeed, AnActualSpeed: THalfFloat; Status: Byte); virtual;
    procedure DoQueryFunction(Address: DWORD; Value: Word); virtual;
    procedure DoControllerTakeOver(var Allow: Boolean); virtual;

  public
    property AssignedTrain: TAttachedTrain read FAssignedTrain write FAssignedTrain;
    property Speed: single read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;
    property OnMessageCallback: TOnControllerCallBack read FOnMessageCallback write FOnMessageCallback;
    property OnQuerySpeed: TOnControllerQuerySpeed read FOnQuerySpeed write FOnQuerySpeed;
    property OnQueryFunction: TOnControllerQueryFunction read FOnQueryFunction write FOnQueryFunction;
    property OnControllerRequestTakeover: TOnControllerRequestTakeover read FOnControllerRequestTakeover write FOnControllerRequestTakeover;

    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
    procedure ReleaseTrain;
    procedure QuerySpeed;
    procedure QueryFunction(Address: Word);
    procedure EmergencyStop;
    function IsTrainAssigned: Boolean;

    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
    // TODO Need a watchdog timer to make sure it does not get hung forever
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TLccAssignTrainAction }

function TLccAssignTrainAction._0ReceiveFirstMessage(Sender: TObject;
  SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  WorkerMessage.LoadTractionSearch(NodeID, AliasID, RequestedSearchData);
  SendMessage(Owner, WorkerMessage);
  SetTimoutCountThreshold(5000); // 5 seconds to collect trains
  AdvanceToNextState;
end;


function TLccAssignTrainAction._1ActionWaitForSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i: Integer;
begin
  Result := False;

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
             Inc(FRepliedSearchCriterialCount);
             WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(NodeID, AliasID, RepliedSearchCriteria[RepliedSearchCriterialCount].NodeID, RepliedSearchCriteria[RepliedSearchCriterialCount].NodeAlias);
             SendMessage(Owner, WorkerMessage);
           end else
             AdvanceToNextState;    // No more slots to hold results
         end
      end;
     MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY :
       begin
          i := 0;
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

 // if Cancel then
 //   AdvanceToFinalState else
 // if TimeoutExpired then    // Not an error to time out
 //   AdvanceToNextState;
end;

function TLccAssignTrainAction._2ActionShowUserSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  if RepliedSearchCriterialCount > 0 then
  begin
    if RepliedSearchCriterialCount = 1 then
      AdvanceToNextState
    else begin
      DoMultipleSearchResults;
  //    if (SelectedSearchResultIndex < 0) or (SelectedSearchResultIndex > Length(RepliedSearchCriteria)-1) then
  //      AdvanceToFinalState
  //    else
        AdvanceToNextState;
    end;
  end else
  begin
    DoNoSearchResults;
  //  AdvanceToFinalState;
  end;
end;

function TLccAssignTrainAction._3ActionSendAssignThrottle(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  // Atomic action no need for Reservation (Manage)
  WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeAlias, NodeID, AliasID);
  SendMessage(Owner, WorkerMessage);
  SetTimoutCountThreshold(5000); // 5 seconds to assign the train
  AdvanceToNextState;
end;

function TLccAssignTrainAction._4ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  case SourceMessage.MTI of
     MTI_TRACTION_REPLY :
       begin
         case SourceMessage.DataArray[0] of
           TRACTION_CONTROLLER_CONFIG :
             begin
               case SourceMessage.DataArray[1] of
                 TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY :
                   begin
                     case SourceMessage.DataArray[1] of
                       S_OK :
                         begin
                           AdvanceToNextState;
                         end;
                       TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
                         begin
                            DoAssignFailed(TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER);
               //             AdvanceToFinalState;
                         end;
                       TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                         begin
                            DoAssignFailed(TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN);
               //             AdvanceToFinalState
                         end;
                     end;
                   end;
               end;
             end;
         end;
       end;
  end;

  if TimeoutExpired or Cancel then
  begin
    // Just in case
    WorkerMessage.LoadTractionControllerRelease(NodeID, AliasID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeAlias, NodeID, AliasID);
    SendMessage(Owner, WorkerMessage);
    if not Cancel then
      DoTimeoutExpired;
   // AdvanceToFinalState;
  end;
end;

function TLccAssignTrainAction._5ActionAssignTrain(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := False;

  DoAssignTrain;

  UnRegisterSelf;
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
  SetStateArrayLength(6);
  States[0] := @_0ReceiveFirstMessage;
  States[1] := @_1ActionWaitForSearchResults;
  States[2] := @_2ActionShowUserSearchResults;
  States[3] := @_3ActionSendAssignThrottle;
  States[4] := @_4ActionWaitForAssignThrottleResult;
  States[5] := @_5ActionAssignTrain;

  SetLength(FRepliedSearchCriteria, 20);  // 7 States
  FRepliedSearchCriterialCount := 0;
  FSelectedSearchResultIndex := -1;
end;


{ TLccTrainController }

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
var
  SearchData: DWORD;
begin
  ClearAssignedTrain;
  FAssignedTrain.RepliedSearchData := 0;
  FAssignedTrain.RequestedSearchData := 0;
  FAssignedTrain.SearchState := tssSearching;
  SearchData := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, SearchData);
  FAssignedTrain.RequestedSearchData := SearchData;
  WorkerMessage.LoadTractionSearch(NodeID, AliasID, AssignedTrain.RequestedSearchData);
  SendMessageFunc(Self, WorkerMessage);
  // Now wait for the Event Identified Messages
end;

procedure TLccTrainController.ReleaseTrain;
begin
  if AssignedTrain.AttachedState = tasAssigned then
  begin
    FAssignedTrain.AttachedState := tasUnAssigning;
    FAssignedTrain.ReservedState := trsReserving;
    WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, True);
    SendMessageFunc(Self, WorkerMessage);
    // Wait for Result of Manage
  end;
end;

procedure TLccTrainController.SetDirection(AValue: TLccTrainDirection);
begin
  FDirection := AValue;
  if AssignedTrain.AttachedState = tasAssigned then
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
  if (Index >= 0) and (Index < High(FunctionArray)) then
  begin
    FFunctionArray[Index] := AValue;
    if AssignedTrain.AttachedState = tasAssigned then
    begin
      WorkerMessage.LoadTractionSetFunction(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, Index, AValue);
      SendMessageFunc(Self, WorkerMessage);
    end;
  end;
end;

procedure TLccTrainController.SetSpeed(AValue: single);
begin
  FSpeed := Abs(AValue);
  if AssignedTrain.AttachedState = tasAssigned then
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
    if not IsDestinationEqual(SourceMessage) then
      Exit;
  end;

  // Event is global and unaddressed so it can come from anywhere so no check for Target yet
  // The response here will give us our target.
  case SourceMessage.MTI of
    MTI_PRODUCER_IDENTIFIED_CLEAR,
    MTI_PRODUCER_IDENTIFIED_SET,
    MTI_PRODUCER_IDENTIFIED_UNKNOWN :
      begin
        // Note many results potentailly be returned this only grabs the first one
        if FAssignedTrain.SearchState = tssSearching then
        begin
          if SourceMessage.TractionSearchIsEvent and EqualNode(SourceMessage.SourceID, SourceMessage.CAN.DestAlias, AssignedTrain.NodeID, AssignedTrain.AliasID) then
          begin
            // Look at the AssignedTrain.RepliedSearchData for what the train actually implemented
            FAssignedTrain.RepliedSearchData := AssignedTrain.RepliedSearchData;
            FAssignedTrain.NodeID := SourceMessage.SourceID;
            FAssignedTrain.AliasID := SourceMessage.CAN.SourceAlias;
            FAssignedTrain.ReservedState := trsReserving;
            FAssignedTrain.SearchState := tssNotSearching;
            WorkerMessage.LoadTractionManage(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True);
            SendMessageFunc(Self, WorkerMessage);
          end;
        end;
      end;
  end;

    // Only care if coming from our potential Target Train
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
                  begin
                    AllowTakeover := True;
                    DoControllerTakeOver(AllowTakeover);
                    if AllowTakeover then
                    begin
                      ClearAssignedTrain;
                      WorkerMessage.LoadTractionControllerChangedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True);
                      SendMessageFunc(Self, WorkerMessage);
                      DoMessageCallback(ccbControllerUnassigned);
                    end else
                    begin
                      WorkerMessage.LoadTractionControllerChangedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, False );
                      SendMessageFunc(Self, WorkerMessage);
                    end;
                  end;
                end;
              end;
          end
        end;
      MTI_TRACTION_REPLY :
        begin
          case SourceMessage.DataArray[0] of
            TRACTION_QUERY_SPEED_REPLY :
              begin
                DoQuerySpeed(SourceMessage.TractionExtractSetSpeed, SourceMessage.TractionExtractCommandedSpeed, SourceMessage.TractionExtractActualSpeed, SourceMessage.TractionExtractSpeedStatus);
              end;
            TRACTION_QUERY_FUNCTION_REPLY :
              begin
                DoQueryFunction(SourceMessage.TractionExtractFunctionAddress, SourceMessage.TractionExtractFunctionValue);
              end;
            TRACTION_MANAGE :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_MANAGE_RESERVE :
                    begin
                      case SourceMessage.DataArray[2] of
                        TRACTION_MANAGE_RESERVE_REPLY_OK :
                          begin
                            if AssignedTrain.ReservedState = trsReserving then
                            begin
                              if AssignedTrain.AttachedState = tasUnAssigning then
                              begin
                                // This reserve was to Release the train
                                WorkerMessage.LoadTractionControllerRelease(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, NodeID, AliasID);
                                SendMessageFunc(Self, WorkerMessage);
                                WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, False);
                                SendMessageFunc(Self, WorkerMessage);
                                ClearAssignedTrain;
                                DoMessageCallback(ccbControllerUnassigned);
                              end else
                              if AssignedTrain.AttachedState = tasNotAssigned then
                              begin
                                // This reserve was the Assign the train
                                WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, NodeID, AliasID);
                                SendMessageFunc(Self, WorkerMessage);
                                FAssignedTrain.AttachedState := tasAssigning;
                                FAssignedTrain.ReservedState := trsReserved;
                              end;
                            end
                          end else
                          begin
                            DoMessageCallback(ccbReservedFail);
                          end
                      end;
                    end;
                end;
              end;
            TRACTION_CONTROLLER_CONFIG_REPLY :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN :
                    begin
                      case SourceMessage.DataArray[2] of
                        TRACTION_CONTROLLER_CONFIG_REPLY_OK :
                          begin
                            WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, False);
                            SendMessageFunc(Self, WorkerMessage);
                            FAssignedTrain.ReservedState := trsNotReserved;
                            FAssignedTrain.AttachedState := tasAssigned;
                            DoMessageCallback(ccbControllerAssigned);
                          end;
                        TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
                          begin
                            WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, False);
                            SendMessageFunc(Self, WorkerMessage);
                            ClearAssignedTrain;
                            DoMessageCallback(ccbAssignFailControllerRefused);
                          end;
                        TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                          begin
                            WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, False);
                            SendMessageFunc(Self, WorkerMessage);
                            ClearAssignedTrain;
                            DoMessageCallback(ccbAssignFailTrainRefused);
                          end;
                      end;
                    end;
                end
              end;
          end;
        end;
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
  begin
    WorkerMessage.LoadTractionQuerySpeed(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID);
    SendMessageFunc(Self, WorkerMessage);
  end;
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

  ClearAssignedTrain;
  FAssignedTrain.RequestedSearchData := 0;
  FAssignedTrain.RepliedSearchData := 0;
  FAssignedTrain.SearchState := tssSearching;
  LocalSearchData := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchData);
  FAssignedTrain.RequestedSearchData := LocalSearchData;
  WorkerMessage.LoadTractionSearch(NodeID, AliasID, AssignedTrain.RequestedSearchData);
  SendMessageFunc(Self, WorkerMessage);
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

procedure TLccTrainController.DoMessageCallback(AMessage: TControllerCallBackMessages);
begin
  if Assigned(OnMessageCallback) then
    OnMessageCallBack(Self, AMessage);
end;

procedure TLccTrainController.DoQueryFunction(Address: DWORD; Value: Word);
begin
  if Assigned(OnQueryFunction) then
    OnQueryFunction(Self, Address, Value);
end;

procedure TLccTrainController.DoQuerySpeed(ASetSpeed, ACommandSpeed,
  AnActualSpeed: THalfFloat; Status: Byte);
begin
  if Assigned(OnQuerySpeed) then
    OnQuerySpeed(Self, ASetSpeed, ACommandSpeed, AnActualSpeed, Status);
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

