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
  TControllerTrainAssignResult = (tarAssigned, tarFailTrainRefused, tarFailControllerRefused);

  TLccAssignTrainAction = class;

  { TLccAssignTrainAction }

  TLccAssignTrainAction = class(TLccAction)
  private
    FRepliedSearchCriteria: TLccSearchResultsArray;
    FRepliedSearchCriterialCount: Integer;
    FRequestedSearchData: DWORD;
    FSelectedSearchResultIndex: Integer;
  protected
    function _0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean; override;
    function _1ActionWaitForSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
    function _2ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;

    procedure DoSearchResults; virtual;
    procedure DoAssigned(ResultCode: TControllerTrainAssignResult); virtual;

    procedure LoadStateArray; override;
  public
    property RequestedSearchData: DWORD read FRequestedSearchData write FRequestedSearchData;
    property RepliedSearchCriterialCount: Integer read FRepliedSearchCriterialCount;
    property RepliedSearchCriteria: TLccSearchResultsArray read FRepliedSearchCriteria;
    property SelectedSearchResultIndex: Integer read FSelectedSearchResultIndex write FSelectedSearchResultIndex;
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

  TOnControllerSearchResult = procedure(Sender: TLccAssignTrainAction; Results: TLccSearchResultsArray; SearchResultCount: Integer; var SelectedResultIndex: Integer) of object;
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
    procedure DoSearchResult(AssignAction: TLccAssignTrainAction; SearchResults: TLccSearchResultsArray; SearchResultCount: Integer; var SelectedIndex: Integer); virtual;

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

    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
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

{ TLccReleaseTrainAction }

function TLccReleaseTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  Assert(SourceMessage <> nil, 'SourceMessage is NIL, unexpected, single state statemachine');

  ControllerNode := Owner as TLccTrainController;
  if Assigned(ControllerNode) then
  begin
    if ControllerNode.IsTrainAssigned then
    begin
      WorkerMessage.LoadTractionControllerRelease(NodeID, AliasID, ControllerNode.AssignedTrain.NodeID, ControllerNode.AssignedTrain.AliasID, NodeID, AliasID);
      SendMessage(ControllerNode, WorkerMessage);
      ControllerNode.ClearAssignedTrain;
    end;
    ControllerNode.DoTrainReleased;
  end;

  _NFinalStateCleanup(Sender, SourceMessage);
end;

procedure TLccReleaseTrainAction.LoadStateArray;
begin
  inherited LoadStateArray;
end;

{ TLccQueryFunctionAction }

function TLccQueryFunctionAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result :=inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  WorkerMessage.LoadTractionQueryFunction(NodeID, AliasID, TargetNodeID, TargetAliasID, Address);
  SendMessage(Owner, WorkerMessage);
  AdvanceToNextState;
end;

function TLccQueryFunctionAction._1ActionWaitForQueryFunctionResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
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
      if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, TargetNodeID, TargetAliasID) then
      begin
        case SourceMessage.MTI of
          MTI_TRACTION_REPLY :
            begin
              case SourceMessage.DataArray[0] of
                TRACTION_QUERY_FUNCTION_REPLY :
                  begin  // this can be turned into a request/reply action
                    if Address = SourceMessage.TractionExtractFunctionAddress then
                    begin
                      ControllerNode.DoQueryFunctionReply(Address, SourceMessage.TractionExtractFunctionValue);
                      _NFinalStateCleanup(Sender, SourceMessage);
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

procedure TLccQueryFunctionAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForQueryFunctionResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccQuerySpeedAction }

function TLccQuerySpeedAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);
  ControllerNode := Owner as TLccTrainController;
  if Assigned(ControllerNode) then
  begin
    if ControllerNode.IsTrainAssigned then
    begin
      WorkerMessage.LoadTractionQuerySpeed(NodeID, AliasID, TargetNodeID, TargetAliasID);
      SendMessage(ControllerNode, WorkerMessage);
    end;
  end;
  AdvanceToNextState;
  SetTimoutCountThreshold(20000);
end;

function TLccQuerySpeedAction._1ActionWaitForQuerySpeedResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
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
      if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, TargetNodeID, TargetAliasID) then
      begin
        case SourceMessage.MTI of
          MTI_TRACTION_REPLY :
            begin
              case SourceMessage.DataArray[0] of
                TRACTION_QUERY_SPEED_REPLY :
                  begin  // this can be turned into a request/reply action
                    ControllerNode.DoQuerySpeedReply(SourceMessage.TractionExtractSetSpeed, SourceMessage.TractionExtractCommandedSpeed, SourceMessage.TractionExtractActualSpeed, SourceMessage.TractionExtractSpeedStatus);
                    _NFinalStateCleanup(Sender, SourceMessage);
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

procedure TLccQuerySpeedAction.LoadStateArray;
begin
  SetStateArrayLength(3);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForQuerySpeedResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;
end;

{ TLccAssignTrainAction }

function TLccAssignTrainAction._0ReceiveFirstMessage(Sender: TObject; SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited _0ReceiveFirstMessage(Sender, SourceMessage);

  FSelectedSearchResultIndex := 0;
  FRepliedSearchCriterialCount := 0;
  WorkerMessage.LoadTractionSearch(NodeID, AliasID, RequestedSearchData);
  SendMessage(Owner, WorkerMessage);
  SetTimoutCountThreshold(1000); // seconds to collect trains
  AdvanceToNextState;
end;


function TLccAssignTrainAction._1ActionWaitForSearchResults(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  i: Integer;
  TrainVersion: Byte;
  TrainRoadName,
  TrainClass,
  TrainRoadNumber,
  TrainName,
  TrainManufacturer,
  TrainOwner: string;
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
               FRepliedSearchCriteria[RepliedSearchCriterialCount].NodeID := SourceMessage.SourceID;
               FRepliedSearchCriteria[RepliedSearchCriterialCount].NodeAlias := SourceMessage.CAN.SourceAlias;
               FRepliedSearchCriteria[RepliedSearchCriterialCount].SearchData := SourceMessage.TractionSearchExtractSearchData;
               FRepliedSearchCriteria[RepliedSearchCriterialCount].HasSTNIP := False;
               FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Manufacturer := '';
               FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Owner := '';
               FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Roadname := '';
               FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.RoadNumber := '';
               FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.TrainClass := '';
               FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.TrainName := '';
               FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Version := 0;
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
                // all this claptrap for SMS and var parameters....
                TrainVersion := 0;
                TrainRoadName := '';
                TrainClass := '';
                TrainRoadNumber := '';
                TrainName := '';
                TrainManufacturer := '';
                TrainOwner := '';
                SourceMessage.ExtractSimpleTrainNodeIdentInfoReply(TrainVersion, TrainRoadName, TrainClass, TrainRoadNumber, TrainName, TrainManufacturer, TrainOwner);
                FRepliedSearchCriteria[RepliedSearchCriterialCount].HasSTNIP := True;
                FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Manufacturer := TrainManufacturer;
                FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Owner := TrainOwner;
                FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Roadname := TrainRoadName;
                FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.RoadNumber := TrainRoadNumber;
                FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.TrainClass := TrainClass;
                FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.TrainName := TrainName;
                FRepliedSearchCriteria[RepliedSearchCriterialCount].STNIP.Version := TrainVersion;
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
        SetTimoutCountThreshold(TIMEOUT_CONTROLLER_NOTIFY_WAIT * 2); // seconds to assign the train, the command station will give 5 seconds to receive a reply from an existing controller to give it up
        AdvanceToNextState;
      end else
      begin
        DoSearchResults;
        if (SelectedSearchResultIndex < 0) or (SelectedSearchResultIndex >= Length(RepliedSearchCriteria)) then
          _NFinalStateCleanup(Sender, SourceMessage)    // out of bounds, just quit
        else begin  // Reply the item at SelectedSearchResultIndex
          WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeID, RepliedSearchCriteria[SelectedSearchResultIndex].NodeAlias, NodeID, AliasID);
          SendMessage(Owner, WorkerMessage);
          SetTimoutCountThreshold(TIMEOUT_CONTROLLER_NOTIFY_WAIT * 2); // seconds to assign the train, the command station will give 5 seconds to receive a reply from an existing controller to give it up
          AdvanceToNextState;
        end
      end;
    end else
    begin // Found Nothing, just quit
      DoSearchResults;
      _NFinalStateCleanup(Sender, SourceMessage);
    end;
  end;
end;

function TLccAssignTrainAction._2ActionWaitForAssignThrottleResult(Sender: TObject; SourceMessage: TLccMessage): Boolean;
var
  ControllerNode: TLccTrainController;
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
                               ControllerNode := Owner as TLccTrainController;
                                if Assigned(ControllerNode) then
                                begin
                                  ControllerNode.FAssignedTrain.NodeID := RepliedSearchCriteria[SelectedSearchResultIndex].NodeID;
                                  ControllerNode.FAssignedTrain.AliasID := RepliedSearchCriteria[SelectedSearchResultIndex].NodeAlias;
                                  ControllerNode.FAssignedTrain.RepliedSearchData := RepliedSearchCriteria[SelectedSearchResultIndex].SearchData;
                                  DoAssigned(tarAssigned);
                               end;
                               _NFinalStateCleanup(Sender, SourceMessage);
                             end;
                           TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
                             begin
                               DoAssigned(tarFailTrainRefused);
                               _NFinalStateCleanup(Sender,SourceMessage);
                             end;
                           TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                             begin
                                DoAssigned(tarFailControllerRefused);
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

procedure TLccAssignTrainAction.DoAssigned(ResultCode: TControllerTrainAssignResult);
var
  ControllerNode: TLccTrainController;
begin
  ControllerNode := Owner as TLccTrainController;
  if Assigned(ControllerNode) then
  begin
    if Assigned(ControllerNode.OnTrainAssigned) then
      ControllerNode.OnTrainAssigned(ControllerNode, ResultCode);
  end;
end;

procedure TLccAssignTrainAction.DoSearchResults;
var
  NewIndex: Integer;
  ControllerNode: TLccTrainController;
begin
  ControllerNode := Owner as TLccTrainController;
  // SMS claptrap
  if Length(RepliedSearchCriteria) > 0 then
    NewIndex := 0
  else
    NewIndex := -1;
  if Assigned(ControllerNode) then
    if Assigned(ControllerNode.OnSearchResult) then
    begin
      ControllerNode.OnSearchResult(Self, RepliedSearchCriteria, RepliedSearchCriterialCount, NewIndex);
      if ((NewIndex > -1) and (NewIndex < Length(RepliedSearchCriteria))) then
        FSelectedSearchResultIndex := NewIndex;
    end;
  FSelectedSearchResultIndex := NewIndex;
end;

procedure TLccAssignTrainAction.LoadStateArray;
begin
  SetStateArrayLength(4);
  States[0] := {$IFNDEF DELPHI}@{$ENDIF}_0ReceiveFirstMessage;
  States[1] := {$IFNDEF DELPHI}@{$ENDIF}_1ActionWaitForSearchResults;
  States[2] := {$IFNDEF DELPHI}@{$ENDIF}_2ActionWaitForAssignThrottleResult;
  States[3] := {$IFNDEF DELPHI}@{$ENDIF}_NFinalStateCleanup;

  {$IFDEF DWSCRIPT}
  FRepliedSearchCriteria.SetLength(20);
  {$ELSE}
  SetLength(FRepliedSearchCriteria, 20);  // 20 Results is enough
  {$ENDIF}
  FRepliedSearchCriterialCount := 0;
  FSelectedSearchResultIndex := -1;
end;


{ TLccTrainController }

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
var
  LocalSearchData: DWORD;
  LccAssignTrainAction: TLccAssignTrainAction;
begin
  ClearAssignedTrain;
  FAssignedTrain.RepliedSearchData := 0;
  FAssignedTrain.RequestedSearchData := 0;
  LocalSearchData := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchData);
  FAssignedTrain.RequestedSearchData := LocalSearchData;

  LccAssignTrainAction := TLccAssignTrainAction.Create(Self, NodeID, AliasID);
  LccAssignTrainAction.RequestedSearchData := LocalSearchData;
  LccActions.RegisterAction(Self, nil, LccAssignTrainAction);
end;

procedure TLccTrainController.ReleaseTrain;
begin
  if IsTrainAssigned then
    LccActions.RegisterAction(Self, AssignedTrain.NodeID, AssignedTrain.AliasID, TLccReleaseTrainAction.Create(Self, NodeID, AliasID));
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
                      DoTrainReleased;
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
  LccQueryFunctionAction: TLccQueryFunctionAction;
begin
  if IsTrainAssigned then
  begin
    LccQueryFunctionAction := TLccQueryFunctionAction.Create(Self, NodeID, AliasID);
    LccQueryFunctionAction.Address := Address;
    LccActions.RegisterAction(Self, AssignedTrain.NodeID, AssignedTrain.AliasID, LccQueryFunctionAction);
  end;
end;

procedure TLccTrainController.QueryFunctions;
var
  i: Integer;
begin
  for i := 0 to 28 do
    QueryFunction(i);
end;

procedure TLccTrainController.QuerySpeed;
begin
  if IsTrainAssigned then
    LccActions.RegisterAction(Self, AssignedTrain.NodeID, AssignedTrain.AliasID, TLccQuerySpeedAction.Create(Self, NodeID, AliasID));
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
  FAssignedTrain.Listeners := nil;
  {$ENDIF}
end;

procedure TLccTrainController.DoControllerTakeOver(var Allow: Boolean);
begin
  If Assigned(OnControllerRequestTakeover) then
    OnControllerRequestTakeover(Self, Allow);
end;

procedure TLccTrainController.DoSearchResult(
  AssignAction: TLccAssignTrainAction; SearchResults: TLccSearchResultsArray;
  SearchResultCount: Integer; var SelectedIndex: Integer);
begin
  if Assigned(OnSearchResult) then
    OnSearchResult(AssignAction, SearchResults, SearchResultCount, SelectedIndex);
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

