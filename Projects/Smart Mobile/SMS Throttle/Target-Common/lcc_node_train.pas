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
  lcc_utilities,
  lcc_defines,
  lcc_node_messages,
  lcc_math_float16,
  lcc_node;

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
  TLccTrainDirection = (tdForward, tdReverse);
  TLccFunctions = array[0..27] of Word;

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
    FDirection: TLccTrainDirection;
    FFunctions: TLccFunctions;
    FName: string;
    FReserveWatchDogTimer: TLccTimer;
    FRoadNumber: string;
    FSearchEvent: TEventID;
    FSpeed: THalfFloat;
    FSpeedStep: TLccDccSpeedStep;
    function GetFunctions(Index: Integer): Word;
    procedure SetDccAdddress(AValue: Word);
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

    procedure ClearAttachedController;
    procedure OnReserveWatchDogTimer(Sender: TObject);
    function GetCdiFile: string; override;
    procedure BeforeLogin; override;
  public

    property DccAddress: Word read FDccAddress write SetDccAdddress;
    property DccLongAddress: Boolean read FDccLongAddress write SetDccLongAddress;
    property Name: string read FName write SetName;
    property RoadNumber: string read FRoadNumber write SetRoadNumber;
    property SpeedStep: TLccDccSpeedStep read FSpeedStep write SetSpeedStep;
    property Speed: THalfFloat read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;
    property Functions[Index: Integer]: Word read GetFunctions write SetFunctions;

    // The Search Event that may have created this Train Node, used as storage for the Command Station to create and wait for Initilization to complete
    property SearchEvent: TEventID read FSearchEvent write FSearchEvent;

    function ControllerAssigned: Boolean;
    function ControllerEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
    function IsReservedBy(SourceMessage: TLccMessage): Boolean;
    function IsReserved: Boolean;
    function ReservationEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccTrainCanNodeClass = class of TLccTrainCanNode;


implementation

{ TLccTrainCanNode }

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
var
  FunctionAddress: LongWord;
begin
  Result := inherited ProcessMessage(SourceMessage);

  if SourceMessage.HasDestination then
  begin
    if not IsDestinationEqual(SourceMessage) then
      Exit;
  end;

  case SourceMessage.MTI of
    MTI_TRACTION_REQUEST :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_SPEED_DIR :
            begin
              if ControllerAssigned then
                if ControllerEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
                  Speed := SourceMessage.TractionExtractSpeed;
            end;
          TRACTION_FUNCTION :
            begin
              if ControllerAssigned then
                if ControllerEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
                begin
                  FunctionAddress := SourceMessage.TractionExtractFunctionAddress;
                  Functions[FunctionAddress] := SourceMessage.TractionExtractFunctionValue;
                end
            end;
          TRACTION_E_STOP :
            begin
              Speed := 0;
            end;
          TRACTION_QUERY_SPEED :
            begin
               WorkerMessage.LoadTractionQuerySpeedReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, Speed, 0, Speed, Speed);
               SendMessageFunc(Self, WorkerMessage);
            end;
          TRACTION_QUERY_FUNCTION :
            begin
              FunctionAddress := SourceMessage.TractionExtractFunctionAddress;
              WorkerMessage.LoadTractionQueryFunctionReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, FunctionAddress, Functions[FunctionAddress]);
              SendMessageFunc(Self, WorkerMessage);
            end;
          TRACTION_CONTROLLER_CONFIG :
            begin
              if IsReservedBy(SourceMessage) then
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_CONTROLLER_CONFIG_ASSIGN :
                    begin
                      // Reservation is checked above
                      if not ControllerAssigned or ControllerEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
                      begin
                        FAttachedController.NodeID := SourceMessage.SourceID;
                        FAttachedController.AliasID := SourceMessage.CAN.SourceAlias;
                        WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
                        SendMessageFunc(Self, WorkerMessage);
                      end else
                      begin
                        // Ask the assigned controller if it is ok a new controller takes us over
                        FAttachedController.AttatchNotifyNodeID := SourceMessage.SourceID;
                        FAttachedController.AttachNotifyAliasID := SourceMessage.CAN.SourceAlias;
                        WorkerMessage.LoadTractionControllerChangingNotify(NodeID, AliasID, AttachedController.NodeID, AttachedController.AliasID, AttachedController.AttatchNotifyNodeID, AttachedController.AttachNotifyAliasID);
                        SendMessageFunc(Self, WorkerMessage);
                      end;
                    end;
                  TRACTION_CONTROLLER_CONFIG_RELEASE :
                    begin
                        // Reservation is checked above
                        if ControllerEquals(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias) then
                          ClearAttachedController;
                    end;
                  TRACTION_CONTROLLER_CONFIG_QUERY :
                    begin
                      // Reservation is checked above
                      if (AttachedController.NodeID[0] = 0) and (AttachedController.NodeID[1] = 0) and (AttachedController.AliasID = 0) then
                        WorkerMessage.LoadTractionConsistQuery(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, NULL_NODE_ID, 0)
                      else
                        WorkerMessage.LoadTractionConsistQuery(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, AttachedController.NodeID, AttachedController.AliasID);
                      SendMessageFunc(Self, WorkerMessage);
                    end;
                end;
             end
            end;
          TRACTION_LISTENER :
            begin
              if IsReservedBy(SourceMessage) then
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_LISTENER_ATTACH :
                    begin
                    end;
                  TRACTION_LISTENER_DETACH :
                    begin
                    end;
                  TRACTION_LISTENER_QUERY :
                    begin
                    end;
                end;
              end
            end;
          TRACTION_MANAGE :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE :
                  begin
                    if not IsReserved or IsReservedBy(SourceMessage) then
                    begin
                      FAttachedController.ReservationNodeID := SourceMessage.SourceID;
                      FAttachedController.ReservationAliasID := SourceMessage.CAN.SourceAlias;
                      WorkerMessage.LoadTractionManageReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, True);
                    end else                                                               // TODO I think we should define some result values for reasons it won't reserve
                       WorkerMessage.LoadTractionManageReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, False);
                    SendMessageFunc(Self, WorkerMessage);
                  end;
                TRACTION_MANAGE_RELEASE :
                  begin
                    FAttachedController.ReservationNodeID := NULL_NODE_ID;
                    FAttachedController.ReservationAliasID := 0;
                  end;
              end;
            end;
        end;
      end;
    MTI_TRACTION_REPLY :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_CONTROLLER_CONFIG :
          begin;
            case SourceMessage.DataArray[1] of
              TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY :
              begin
                if EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, AttachedController.NodeID, AttachedController.AliasID) then
                begin
                  case SourceMessage.DataArray[2] of
                    S_OK :
                      begin
                        // Let the throttle change occur
                        FAttachedController.NodeID := AttachedController.AttatchNotifyNodeID;
                        FAttachedController.AliasID :=  AttachedController.AttachNotifyAliasID;
                        FAttachedController.AttatchNotifyNodeID := NULL_NODE_ID;
                        FAttachedController.AttachNotifyAliasID := 0;
                        WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, FAttachedController.NodeID, FAttachedController.AliasID, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
                        SendMessageFunc(Self, WorkerMessage);
                      end
                     else begin // Assigned controller said no way I'm giving you up
                        WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, AttachedController.AttatchNotifyNodeID, AttachedController.AttachNotifyAliasID, TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER);
                        FAttachedController.AttatchNotifyNodeID := NULL_NODE_ID;
                        FAttachedController.AttachNotifyAliasID := 0;
                        SendMessageFunc(Self, WorkerMessage);
                     end
                   end
                end
              end
            end
          end
        end
      end;
  end;
end;

function TLccTrainCanNode.ReservationEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
begin
    Result := ((AttachedController.ReservationNodeID[0] = ATestNodeID[0]) and (AttachedController.ReservationNodeID[1] = ATestNodeID[1])) or (ATestAlias = AttachedController.ReservationAliasID)
end;

procedure TLccTrainCanNode.SetDccAdddress(AValue: Word);
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
  if AValue = FDirection then Exit;
  FDirection := AValue;
end;

procedure TLccTrainCanNode.SetFunctions(Index: Integer; AValue: Word);
begin
  if (Index >= 0) and (Index < Length(FFunctions)) then
    FFunctions[Index] := AValue
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
begin
  if AValue = Speed then Exit;
  FSpeed := AValue;
end;

procedure TLccTrainCanNode.SetSpeedStep(AValue: TLccDccSpeedStep);
begin
  if FSpeedStep = AValue then Exit;
  FSpeedStep := AValue;
end;

end.

