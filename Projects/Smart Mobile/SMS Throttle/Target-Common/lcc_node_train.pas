unit lcc_node_train;

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
  lcc_utilities,
  lcc_protocol_traction,
  lcc_protocol_traction_simpletrainnodeinfo,
  lcc_protocol_traction_configuation_functiondefinitioninfo,
  lcc_protocol_traction_configuration_functions,
  lcc_protocol_memory_configuration,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_protocol_memory_options,
  lcc_protocol_memory_information,
  lcc_protocol_simplenodeinfo,
  lcc_protocol_acdi,
  lcc_protocol_events,
  lcc_protocol_supportedprotocols,
  lcc_protocol_datagram,
  lcc_protocol_base,
  lcc_math_float16,
  lcc_node;

type
  TLccTrainDirection = (tdForward, tdBackward);

type

  { TLccTrainCanNode }

  TLccTrainCanNode = class(TLccCanNode)
  private
    FAssignedControllerAliasID: Word;
    FAssignedControllerNodeID: TNodeID;
    FDirection: TLccTrainDirection;
    FSpeed: THalfFloat;
    procedure SetDirection(AValue: TLccTrainDirection);
    procedure SetSpeed(AValue: THalfFloat);
  public
    property AssignedControllerNodeID: TNodeID read FAssignedControllerNodeID write FAssignedControllerNodeID;
    property AssignedControllerAliasID: Word read FAssignedControllerAliasID write FAssignedControllerAliasID;
    property Speed: THalfFloat read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;

    function ControllerAssigned: Boolean;
    function ControllerEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
    function ProcessMessage(SourceLccMessage: TLccMessage): Boolean; override;
  end;


implementation

{ TLccTrainCanNode }

function TLccTrainCanNode.ControllerAssigned: Boolean;
begin
  Result := ((AssignedControllerNodeID[0] = 0) and (AssignedControllerNodeID[1] = 0) and (AssignedControllerAliasID = 0))
end;

function TLccTrainCanNode.ControllerEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
begin
  Result := (((AssignedControllerNodeID[0] = ATestNodeID[0]) and (AssignedControllerNodeID[1] = ATestNodeID[1])) and (ATestAlias = AssignedControllerAliasID))
end;

function TLccTrainCanNode.ProcessMessage(SourceLccMessage: TLccMessage): Boolean;
var
  FunctionAddress: LongWord;
begin
  Result := inherited ProcessMessage(SourceLccMessage);
  case SourceLccMessage.MTI of
    MTI_TRACTION_REQUEST :
      begin
        case SourceLccMessage.DataArray[0] of
          TRACTION_MANAGE :
            begin
               WorkerMessage.LoadTractionManageReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, True);
               SendMessageFunc(WorkerMessage);
            end;
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceLccMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN :
                  begin
                    // If a controller is not assigned or the controller is the assigned controller then just simply assign and send
                    if not ControllerAssigned or ControllerEquals(SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias) then
                    begin
                      AssignedControllerNodeID := SourceLccMessage.SourceID;
                      AssignedControllerAliasID := SourceLccMessage.CAN.SourceAlias;
                      WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, AssignedControllerNodeID, AssignedControllerAliasID, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
                    end else
                    begin
                      // Run a task as we need to ask the assigned controller if it is ok to release it and if so assign else fail......
                      // TODO

                      // for now just allow the steal.......
                      AssignedControllerNodeID := SourceLccMessage.SourceID;
                      AssignedControllerAliasID := SourceLccMessage.CAN.SourceAlias;
                      WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, AssignedControllerNodeID, AssignedControllerAliasID, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
                    end;
                  end;
                TRACTION_CONTROLLER_CONFIG_RELEASE :
                  begin
                    if ControllerEquals(SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias) then
                    begin
                      AssignedControllerNodeID := NULL_NODE_ID;
                      AssignedControllerAliasID := 0;;
                    end;
                  end;
                TRACTION_CONTROLLER_CONFIG_QUERY :
                  begin
                    if (AssignedControllerNodeID[0] = 0) and (AssignedControllerNodeID[1] = 0) and (AssignedControllerAliasID = 0) then
                      WorkerMessage.LoadTractionConsistQuery(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, NULL_NODE_ID, 0)
                    else
                      WorkerMessage.LoadTractionConsistQuery(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, AssignedControllerNodeID, AssignedControllerAliasID);
                    SendMessageFunc(WorkerMessage);
                  end;
                TRACTION_CONTROLLER_CONFIG_CHANGING_NOTIFY :
                  begin

                  end;
                TRACTION_SPEED_DIR :
                  begin
                    Speed := SourceLccMessage.TractionExtractSpeed;
                  end;
                TRACTION_QUERY_SPEED :
                  begin
                     WorkerMessage.LoadTractionQuerySpeedReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, Speed, 0, Speed, Speed);
                     SendMessageFunc(WorkerMessage);
                  end;
                TRACTION_QUERY_FUNCTION :
                  begin
                    FunctionAddress := SourceLccMessage.TractionExtractFunction;

                    // TODO CREATE THE FUNCTION TABLE

                    WorkerMessage.LoadTractionQueryFunctionReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, FunctionAddress, $FFFF);
                    SendMessageFunc(WorkerMessage);
                  end;
              end;
            end;
        end;
      end;
    MTI_TRACTION_REPLY :
      begin
        case SourceLccMessage.DataArray[0] of
          TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY :
          begin
            case SourceLccMessage.DataArray[1] of
              TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY :
              begin
                // The controllers response to asking it if we can steal the throttle....
              end;
            end;
          end;
        end;
      end;
  end;
end;

procedure TLccTrainCanNode.SetDirection(AValue: TLccTrainDirection);
begin
  if AValue = FDirection then Exit;
  FDirection := AValue;
end;

procedure TLccTrainCanNode.SetSpeed(AValue: THalfFloat);
begin
  if AValue = Speed then Exit;
  FSpeed := AValue;
end;

end.

