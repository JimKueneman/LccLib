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
  TLccTrainDirection = (tdForward, tdBackward);
  TLccFunctions = array[0..27] of Word;

type

  { TLccTrainCanNode }

  TLccTrainCanNode = class(TLccCanNode)
  private
    FAssignedControllerAliasID: Word;
    FAssignedControllerNodeID: TNodeID;
    FDccAddress: Word;
    FDccLongAddress: Boolean;
    FDirection: TLccTrainDirection;
    FFunctions: TLccFunctions;
    FName: string;
    FReservationAliasID: Word;
    FReservationNodeID: TNodeID;
    FReserveWatchDogTimer: TLccTimer;
    FRoadNumber: string;
    FSpeed: THalfFloat;
    FSpeedStep: TLccDccSpeedStep;
    procedure SetDccAdddress(AValue: Word);
    procedure SetDccLongAddress(AValue: Boolean);
    procedure SetDirection(AValue: TLccTrainDirection);
    procedure SetFunctions(AValue: TLccFunctions);
    procedure SetName(AValue: string);
    procedure SetRoadNumber(AValue: string);
    procedure SetSpeed(AValue: THalfFloat);
    procedure SetSpeedStep(AValue: TLccDccSpeedStep);
  protected
    property ReserveWatchDogTimer: TLccTimer read FReserveWatchDogTimer write FReserveWatchDogTimer;

    procedure OnReserveWatchDogTimer(Sender: TObject);
  public
    property AssignedControllerNodeID: TNodeID read FAssignedControllerNodeID write FAssignedControllerNodeID;
    property AssignedControllerAliasID: Word read FAssignedControllerAliasID write FAssignedControllerAliasID;
    property ReservationNodeID: TNodeID read FReservationNodeID write FReservationNodeID;
    property ReservationAliasID: Word read FReservationAliasID write FReservationAliasID;

    property DccAddress: Word read FDccAddress write SetDccAdddress;
    property DccLongAddress: Boolean read FDccLongAddress write SetDccLongAddress;
    property Name: string read FName write SetName;
    property RoadNumber: string read FRoadNumber write SetRoadNumber;
    property SpeedStep: TLccDccSpeedStep read FSpeedStep write SetSpeedStep;
    property Speed: THalfFloat read FSpeed write SetSpeed;
    property Direction: TLccTrainDirection read FDirection write SetDirection;
    property Functions: TLccFunctions read FFunctions write SetFunctions;

    function ControllerAssigned: Boolean;
    function ControllerEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
    function ReservationEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
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

procedure TLccTrainCanNode.OnReserveWatchDogTimer(Sender: TObject);
begin
  ReserveWatchDogTimer.Enabled := False;
  ReservationAliasID := 0;
  ReservationNodeID := NULL_NODE_ID;
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
          TRACTION_SPEED_DIR :
            begin
              Speed := SourceLccMessage.TractionExtractSpeed;
            end;
          TRACTION_FUNCTION :
            begin
              FunctionAddress := SourceLccMessage.TractionExtractFunctionAddress;
              if FunctionAddress < Length(FFunctions) - 1 then
                FFunctions[FunctionAddress] := SourceLccMessage.TractionExtractFunctionValue;
            end;
          TRACTION_E_STOP :
            begin
              Speed := 0;
            end;
          TRACTION_QUERY_SPEED :
            begin
               WorkerMessage.LoadTractionQuerySpeedReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, Speed, 0, Speed, Speed);
               SendMessageFunc(WorkerMessage);
            end;
          TRACTION_QUERY_FUNCTION :
            begin
              FunctionAddress := SourceLccMessage.TractionExtractFunctionAddress;
              if FunctionAddress < Length(FFunctions) - 1 then
                WorkerMessage.LoadTractionQueryFunctionReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, FunctionAddress, Functions[FunctionAddress])
              else
                WorkerMessage.LoadTractionQueryFunctionReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, FunctionAddress, 0);
              SendMessageFunc(WorkerMessage);
            end;
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceLccMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN :
                  begin
                    if ReservationEquals(SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias) then
                    begin
                      // If a controller is not assigned or the controller is the assigned controller then just simply assign and send
                      if not ControllerAssigned or ControllerEquals(SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias) then
                      begin
                        AssignedControllerNodeID := SourceLccMessage.SourceID;
                        AssignedControllerAliasID := SourceLccMessage.CAN.SourceAlias;
                        WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
                      end else
                      begin
                        // Run a task as we need to ask the assigned controller if it is ok to release it and if so assign else fail......
                        // TODO

                        // for now just allow the steal.......
                        AssignedControllerNodeID := SourceLccMessage.SourceID;
                        AssignedControllerAliasID := SourceLccMessage.CAN.SourceAlias;
                        WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
                      end;
                    end
                  end;
                TRACTION_CONTROLLER_CONFIG_RELEASE :
                  begin
                    if ReservationEquals(SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias) then
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
                    if ControllerAssigned then
                    begin
                      WorkerMessage.LoadTractionControllerChangingNotify(NodeID, AliasID, AssignedControllerNodeID, AssignedControllerAliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias);
                      SendMessageFunc(WorkerMessage);
                    end;
                  end;
              end;
            end;
          TRACTION_LISTENER :
            begin
              case SourceLccMessage.DataArray[1] of
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
            end;
          TRACTION_MANAGE :
            begin
              case SourceLccMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE :
                  begin
                    ReservationNodeID := SourceLccMessage.SourceID;
                    ReservationAliasID := SourceLccMessage.CAN.SourceAlias;
                    WorkerMessage.LoadTractionManageReply(NodeID, AliasID, SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias, True);
                    SendMessageFunc(WorkerMessage);
                    // Start a timer to clear the Reserve Node if it never Releases
                    if not Assigned(FReserveWatchDogTimer) then
                    begin
                      ReserveWatchDogTimer := TLccTimer.Create(nil);
                      {$IFDEF DWSCRIPT}
                      ReserveWatchDogTimer.Delay := 5000;
                      ReserveWatchDogTimer.OnTime := @OnReserveWatchDogTimer;
                      {$ELSE}
                       ReserveWatchDogTimer.Interval := 5000;  // 5 seconds allowed to complete
                       ReserveWatchDogTimer.OnTimer := {$IFNDEF DELPHI}@{$ENDIF}OnReserveWatchDogTimer;
                      {$ENDIF}
                    end;
                    ReserveWatchDogTimer.Enabled := True;
                  end;
                TRACTION_MANAGE_RELEASE :
                  begin
                    ReservationNodeID := NULL_NODE_ID;
                    ReservationAliasID := 0;
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

function TLccTrainCanNode.ReservationEquals(ATestNodeID: TNodeID; ATestAlias: Word): Boolean;
begin
    Result := (((ReservationNodeID[0] = ATestNodeID[0]) and (ReservationNodeID[1] = ATestNodeID[1])) and (ATestAlias = ReservationAliasID))
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

procedure TLccTrainCanNode.SetFunctions(AValue: TLccFunctions);
begin

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

