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
  lcc_node_tasks,
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
  TTaskProcedure = function(Node: TLccNode; SourceMessage: TLccMessage): Word;
  TTrainControllerAssignResults = (afrOk, afrAssignedControllerRefused, afrTrainRefused);
  TLccTrainControllerState = (tcsNone, tcsSearching, tcsAssigning, tcsAssigned, tcsReleasing);

type


{ TTaskControllerManageReserve }

  TTaskControllerManageReserve = class(TNodeTaskBase)
  private
    FIsReserved: Boolean;
  public
    property IsReserved: Boolean read FIsReserved;

    procedure ProcessMessage(SourceMessage: TLccMessage); override;
    procedure Start(ANode: TLccCanNode; ATargetNode: TNodeIdentifier); override;
  end;

  { TTaskControllerTrainSearch }

  TTaskControllerTrainSearch = class(TNodeTaskBase)
  private
    FSearchData: DWORD;
    FSearchString: string;
    FTrackProtocolFlags: Word;
  public
    property TrackProtocolFlags: Word read FTrackProtocolFlags write FTrackProtocolFlags;
    property SearchString: string read FSearchString write FSearchString;
    property SearchData: DWORD read FSearchData write FSearchData;

    procedure ProcessMessage(SourceMessage: TLccMessage); override;
    procedure Start(ANode: TLccCanNode; ATargetNode: TNodeIdentifier); override;
  end;

  { TLccTrainController }

  TLccTrainController = class(TLccCanNode)
  private
    FAssignedTrainAliasID: Word;
    FAssignedTrainNodeId: TNodeID;
    FState: TLccTrainControllerState;
  protected
    function GetCdiFile: string; override;
    procedure BeforeLogin; override;
    procedure ClearSearch;
  //  procedure ReleaseTrain;
  public
    property State: TLccTrainControllerState read FState;
    property AssignedTrainNodeId: TNodeID read FAssignedTrainNodeId write FAssignedTrainNodeId;
    property AssignedTrainAliasID: Word read FAssignedTrainAliasID write FAssignedTrainAliasID;

    procedure AssignTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
    procedure AssignTrainByOpenLCB(SearchString: string; TrackProtocolFlags: Word);
    function ProcessMessage(SourceLccMessage: TLccMessage): Boolean; override;

    // TODO Need a watchdog timer to make sure it does not get hung forever
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TTaskControllerManageReserve }

procedure TTaskControllerManageReserve.ProcessMessage(SourceMessage: TLccMessage);
begin
  case SourceMessage.MTI of
     MTI_TRACTION_REPLY :
       begin
         case SourceMessage.DataArray[1] of
            TRACTION_CONTROLLER_CONFIG_ASSIGN :
              begin
                FIsReserved := SourceMessage.DataArray[2] = S_OK;
                FState := ltsComplete;
              end;
         end;
       end;
  end;
end;

procedure TTaskControllerManageReserve.Start(ANode: TLccCanNode; ATargetNode: TNodeIdentifier);
begin
  inherited Start(ANode, ATargetNode);
  WorkerMessage.LoadTractionManage(OwnerNode.NodeID, OwnerNode.AliasID, TargetNode.NodeID, TargetNode.AliasID, True);
  SendMessage(WorkerMessage);
  // Wait for the reply for the reservation
end;

{ TTaskControllerTrainSearch }

procedure TTaskControllerTrainSearch.ProcessMessage(SourceMessage: TLccMessage);
begin
  case SourceMessage.MTI of
    MTI_VERIFIED_NODE_ID_NUMBER :
      begin
        if TargetNode.AliasID = SourceMessage.CAN.SourceAlias then
        begin
          SourceMessage.ExtractDataBytesAsNodeID(0, FTargetNode.NodeID);
          FState := ltsComplete;
        end;
      end;
    MTI_PRODUCER_IDENTIFIED_CLEAR,
    MTI_PRODUCER_IDENTIFIED_SET,
    MTI_PRODUCER_IDENTIFIED_UNKNOWN :
      begin
        // Note many results potentailly be returned this only grabs the first one
        // TODO:  Make this stay watching and queue up the results before call it complete
        if (State = ltsRunning) and SourceMessage.TractionSearchIsEvent and (SourceMessage.TractionSearchExtractSearchData = SearchData) then
        begin
          if EqualNodeID( SourceMessage.SourceID, NULL_NODE_ID, True) then
          begin
            FTargetNode.AliasID := SourceMessage.CAN.SourceAlias;
            WorkerMessage.LoadVerifyNodeIDAddressed(OwnerNode.NodeID, OwnerNode.AliasID, NULL_NODE_ID, SourceMessage.CAN.SourceAlias);
            SendMessage(WorkerMessage);
          end;
          FState := ltsComplete;
        end;
      end;
  end;
end;

procedure TTaskControllerTrainSearch.Start(ANode: TLccCanNode; ATargetNode: TNodeIdentifier);
begin
  inherited Start(ANode, ATargetNode);
  SearchData := 0;
  WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, FSearchData);
  WorkerMessage.LoadTractionSearch(OwnerNode.NodeID, OwnerNode.AliasID, FSearchData);
  SendMessage(WorkerMessage);
  // Now wait for the Event Identified Messages
end;

{ TLccTrainController }

procedure TLccTrainController.AssignTrainByOpenLCB(SearchString: string;
  TrackProtocolFlags: Word);
var
  SearchData: DWord;
begin
 { if tcsAssigned in State then
    R;
  if not SearchInitiated then
  begin
    if AssignedToTrain then
      ReleaseTrain;

    ClearSearch;

    FSearchInitiated := True;

    SearchData := 0;
    WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, SearchData);
    WorkerMessage.LoadTractionSearch(NodeID, AliasID, SearchData);
    SendMessageFunc(Self, WorkerMessage);
  end;    }
end;

procedure TLccTrainController.AssignTrainByDccAddress(DccAddress: Word;
  IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
var
  TrackProtocolFlags: Word;
  SearchData: DWord;
begin
 { if not SearchInitiated then
  begin;
    if AssignedToTrain then
      ReleaseTrain;

    ClearSearch;

    FSearchInitiated := True;

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

    SearchData := 0;
    WorkerMessage.TractionSearchEncodeSearchString(IntToStr(DccAddress), TrackProtocolFlags, SearchData);
    WorkerMessage.LoadTractionSearch(NodeID, AliasID, SearchData);
    SendMessageFunc(Self, WorkerMessage);
  end;    }
end;

procedure TLccTrainController.AssignTrainByDccTrain(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep);
var
  TrackProtocolFlags: Word;
  SearchData: DWord;
begin
 { if not SearchInitiated then
  begin
    if AssignedToTrain then
      ReleaseTrain;

    ClearSearch;

    FSearchInitiated := True;

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

    SearchData := 0;
    WorkerMessage.TractionSearchEncodeSearchString(SearchString, TrackProtocolFlags, SearchData);
    WorkerMessage.LoadTractionSearch(NodeID, AliasID, SearchData);
    SendMessageFunc(Self, WorkerMessage);
  end;                          }
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

procedure TLccTrainController.ClearSearch;
begin
{  FSearchInitiated := False;
  FAssignedToTrain := False;
  AssignedTrainNodeId := NULL_NODE_ID;
  AssignedTrainAliasID := 0;     }
end;


function TLccTrainController.GetCdiFile: string;
begin
  Result := CDI_XML_CONTROLLER;
end;

function TLccTrainController.ProcessMessage(SourceLccMessage: TLccMessage): Boolean;
begin
  Result := inherited ProcessMessage(SourceLccMessage);
  case SourceLccMessage.MTI of
    MTI_PRODUCER_IDENTIFIED_CLEAR,
    MTI_PRODUCER_IDENTIFIED_SET,
    MTI_PRODUCER_IDENTIFIED_UNKNOWN :  // Result of the Traction Search
      begin
  //      if SearchInitiated and SourceLccMessage.TractionSearchIsEvent then
        begin
          AssignedTrainNodeId := SourceLccMessage.SourceID;
          AssignedTrainAliasID := SourceLccMessage.CAN.SourceAlias;
          WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrainNodeId, AssignedTrainAliasID, True);
          SendMessageFunc(Self, WorkerMessage);
           // Now wait for Release Reply
        end;
      end;
    MTI_TRACTION_REPLY :
      begin
        case SourceLccMessage.DataArray[0] of
          TRACTION_CONTROLLER_CONFIG_REPLY :
            begin
              case SourceLccMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE_REPLY_OK :
                  begin
     //               if Ass;
                    WorkerMessage.LoadTractionControllerAssign(NodeID, AliasID, AssignedTrainNodeId, AssignedTrainAliasID, NodeID, AliasID);
                    SendMessageFunc(Self, WorkerMessage);
                  end;
                TRACTION_MANAGE_RESERVE_REPLY_FAIL :
                  begin
                    ClearSearch;
                    // TODO Need feedback that it failed.
                  end;
                TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY :
                  begin
                    case SourceLccMessage.DataArray[2] of
                      S_OK :
                        begin
                          // Send a Reserve Release
                          WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrainNodeId, AssignedTrainAliasID, False);
                          SendMessageFunc(Self, WorkerMessage);
             //             FAssignedToTrain := True;
                        end;
                      TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
                        begin
                          // Send a Reserve Release
                          WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrainNodeId, AssignedTrainAliasID, False);
                          SendMessageFunc(Self, WorkerMessage);
                           // TODO Need feedback that it failed.
                        end;
                      TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                        begin
                          WorkerMessage.LoadTractionManage(NodeID, AliasID, AssignedTrainNodeId, AssignedTrainAliasID, False);
                          SendMessageFunc(Self, WorkerMessage);
                           // TODO Need feedback that it failed.
                        end;
                    end;
                  end;
              end;
            end;
        end;
      end;
  end;
end;

end.

