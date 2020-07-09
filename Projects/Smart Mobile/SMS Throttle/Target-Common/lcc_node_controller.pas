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
  //    contnrs,
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


// ******************************************************************************

  TAttachedTrainReservationState = (trsNotReserved, trsReserving, trsReserved);
  TAttachedTrainAssignmentState = (tasNotAssigned, tasAssigning, tasAssigned, tasUnAssigning);
  TAttachedTrainSearchState = (tssNotSearching, tssSearching);

  // WARNING: For SMS these are zeroed manually in the Clear method.   If you add
  // more data, make sure it is cleared in the method for SMS
  TAttachedTrain = record
    NodeID: TNodeID;
    AliasID: Word;
    SearchData: DWORD;
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

  public
    property AssignedTrain: TAttachedTrain read FAssignedTrain write FAssignedTrain;
    property Speed: single read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;

    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
    procedure ReleaseTrain;

    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
    // TODO Need a watchdog timer to make sure it does not get hung forever
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TLccTrainController }

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
var
  SearchData: DWORD;
begin
  ClearAssignedTrain;
  FAssignedTrain.SearchData := 0;
  FAssignedTrain.SearchState := tssSearching;
  SearchData := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, SearchData);
  FAssignedTrain.SearchData := SearchData;
  WorkerMessage.LoadTractionSearch(NodeID, AliasID, AssignedTrain.SearchData);
  SendMessageFunc(Self, WorkerMessage);

  // Now wait for the Event Identified Messages

 { NewTask := TLccTaskControllerTrainSearch.Create(Self);
  NewTask.SearchString := SearchString;
  NewTask.TrackProtocolFlags := TrackProtocolFlags;
  NewTask.AutoAssignToFirstFound := True;
  ActiveTask := NewTask;
  NewTask.Start(nil, False);     }
end;

procedure TLccTrainController.ReleaseTrain;
begin
  if AssignedTrain.AttachedState = tasAssigned then
  begin
    FAssignedTrain.AttachedState := tasUnAssigning;
    FAssignedTrain.ReservedState := trsReserving;
    WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, True);
    SendMessageFunc(Self, WorkerMessage);
  end;  ;
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
begin
  Result :=inherited ProcessMessage(SourceMessage);

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
          if SourceMessage.TractionSearchIsEvent and (SourceMessage.TractionSearchExtractSearchData = AssignedTrain.SearchData) then
          begin
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
                    // TODO: NEED TO POP A MESSAGE HERE
                    WorkerMessage.LoadTractionControllerChangedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True);
                    SendMessageFunc(Self, WorkerMessage);
                  end;
                end;
              end;
          end
        end;
      MTI_TRACTION_REPLY :
        begin
          case SourceMessage.DataArray[0] of
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
                                WorkerMessage.LoadTractionControllerRelease(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, NodeID, AliasID);
                                SendMessageFunc(Self, WorkerMessage);
                                WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, False);
                                SendMessageFunc(Self, WorkerMessage);
                                ClearAssignedTrain;
                              end else
                              if AssignedTrain.AttachedState = tasNotAssigned then
                              begin
                                WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, NodeID, AliasID);
                                SendMessageFunc(Self, WorkerMessage);
                                FAssignedTrain.AttachedState := tasAssigning;
                                FAssignedTrain.ReservedState := trsReserved;
                              end;
                            end
                          end else
                          begin

                            // TODO Call back to tell the app it failed.
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
                          end;
                        TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
                          begin
                            // TODO Call back to tell the app it failed.
                            WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, False);
                            SendMessageFunc(Self, WorkerMessage);
                            ClearAssignedTrain;
                          end;
                        TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                          begin
                            // TODO Call back to tell the app it failed.
                            WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrain.NodeID, AssignedTrain.AliasID, False);
                            SendMessageFunc(Self, WorkerMessage);
                            ClearAssignedTrain;
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
     ldssDefault : TrackProtocolFlags := TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
     ldss14      : TrackProtocolFlags := TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
     ldss28      : TrackProtocolFlags := TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
     ldss128      : TrackProtocolFlags := TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
  end;

  ClearAssignedTrain;
  FAssignedTrain.SearchData := 0;
  FAssignedTrain.SearchState := tssSearching;
  LocalSearchData := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, LocalSearchData);
  FAssignedTrain.SearchData := LocalSearchData;
  WorkerMessage.LoadTractionSearch(NodeID, AliasID, AssignedTrain.SearchData);
  SendMessageFunc(Self, WorkerMessage);

 // AssignTrainByOpenLCB(SearchString, TrackProtocolFlags);
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
  FAssignedTrain.SearchData := 0;
  FAssignedTrain.SearchString := '';
  FAssignedTrain.ReservedState := trsNotReserved;
  FAssignedTrain.AttachedState := tasNotAssigned;
  FAssignedTrain.SearchState := tssNotSearching;
  FAssignedTrain.Listeners := nil;
  {$ENDIF}
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


end.

