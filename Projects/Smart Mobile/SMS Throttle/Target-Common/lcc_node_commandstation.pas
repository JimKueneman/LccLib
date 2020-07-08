unit lcc_node_commandstation;

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
  lcc_node,
  lcc_node_train,
  lcc_utilities;

const
  CDI_XML_COMMANDSTATION: string = (
  '<?xml version="1.0" encoding="utf-8"?>'+
  '<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
  '<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
         '<identification>'+
       	  '<manufacturer>Mustangpeak</manufacturer>'+
       	  '<model>CSN1000</model>'+
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

  { TLccTaskCommandStationTrainSearch }

  TLccTaskCommandStationTrainSearch = class(TLccTaskBase)
  private
    FEvent: TEventID;
    FNewTrainNode: TNodeIdentifier;
    FSearchString: string;
    FTrackProtocolFlags: Word;
  public
    property TrackProtocolFlags: Word read FTrackProtocolFlags;
    property SearchString: string read FSearchString;
    property NewTrainNode: TNodeIdentifier read FNewTrainNode;
    property Event: TEventID read FEvent write FEvent;

    procedure ProcessMessage(SourceMessage: TLccMessage); override;
    procedure Start(SourceMessage: TLccMessage; CallProcessMessage: Boolean); override;
  end;


  { TLccTaskCommandStationAssignThrottle }
  {
  TLccTaskCommandStationAssignThrottle = class(TLccTaskBase)
  public
     procedure ProcessMessage(SourceMessage: TLccMessage); override;
    procedure Start(SourceMessage: TLccMessage; CallProcessMessage: Boolean); override;
  end;    }

  { TLccCommandStationNode }

  TLccCommandStationNode = class(TLccCanNode)
  protected
    procedure Creating; override;
    function GetCdiFile: string; override;
    procedure BeforeLogin; override;

  public

    destructor Destroy; override;
    function AddTrain(ARoadName, ARoadNumber: string; ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep): TLccTrainCanNode;
    procedure ClearTrains;
    function FindTrainByLccID(TestNode: TLccMessage): TLccTrainCanNode;
    function FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainCanNode;
    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccCommandStationNodeClass = class of TLccCommandStationNode;


implementation

uses
  lcc_node_manager;

{ TLccTaskCommandStationAssignThrottle }
{
procedure TLccTaskCommandStationAssignThrottle.ProcessMessage(SourceMessage: TLccMessage);
begin
  // If it is not from the Target then we have no use for it
  if not EqualNode(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, TargetNodeID, TargetAliasID) then Exit;

  case SourceMessage.MTI of
    MTI_TRACTION_REQUEST :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN :
                  begin
                    SourceMessage.ExtractDataBytesAsNodeID(3, FTargetNodeID);
                    if SourceMessage.DataArray[2] = TRACTION_FLAGS_ALIAS_INCLUDED then
                      TargetAliasID := SourceMessage.ExtractDataBytesAsWord(9)
                    else
                      TargetAliasID := 0;
                    WorkerMessage.LoadTractionControllerAssignReply(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAliasID, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
                    SendMessage(WorkerMessage);
                  end;
              end;
            end;
          TRACTION_MANAGE :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE :
                  begin
                    TargetNodeID := SourceMessage.SourceID;
                    TargetAliasID := SourceMessage.CAN.SourceAlias;
                    WorkerMessage.LoadTractionManageReply(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAliasID, True);
                    SendMessage(WorkerMessage);
                  end;
                TRACTION_MANAGE_RELEASE :
                  begin
                     FState := ltsComplete
                  end;
              end;
            end;
        end;
      end;
  end;
end;

procedure TLccTaskCommandStationAssignThrottle.Start(
  SourceMessage: TLccMessage; CallProcessMessage: Boolean);
begin
  inherited Start(SourceMessage, CallProcessMessage);
end; }

{ TLccTaskCommandStationTrainSearch }

procedure TLccTaskCommandStationTrainSearch.ProcessMessage(SourceMessage: TLccMessage);
var
  NMRA_SpeedStep: TLccDccSpeedStep;
  NMRA_ForceLongAddress: Boolean;
  SearchStr: string;
  {$IFDEF DWSCRIPT}
  SearchDccAddress: Integer;
  {$ELSE}
  SearchDccAddress: LongInt;
  {$ENDIF}
  ForceLongAddress: Boolean;
  SpeedStep: TLccDccSpeedStep;
  ATrain: TLccTrainCanNode;
  ANodeID: TNodeID;
  LocalEvent: TEventID;
begin
  case SourceMessage.MTI of
    MTI_INITIALIZATION_COMPLETE :
      begin  // Need to wait for new trains to fully initalize before returning from the Traction Search Event........
        ANodeID := NULL_NODE_ID;
        if EqualNodeID(TargetNodeID, SourceMessage.ExtractDataBytesAsNodeID(0, ANodeID), False) then
        begin
          ATrain := (OwnerNode as TLccCommandStationNode).FindTrainByLccID(SourceMessage);
          if Assigned(ATrain) then
          begin
            LocalEvent := FEvent;
            WorkerMessage.LoadProducerIdentified(ATrain.NodeID, ATrain.AliasID, LocalEvent, evs_Valid);
            SendMessage(WorkerMessage);
            FState := ltsComplete;
          end
        end;
      end;
    MTI_PRODUCER_IDENDIFY :
      begin
        if SourceMessage.TractionSearchIsEvent then    // Is the the event for for traction search?
        begin
          NMRA_ForceLongAddress := False;
          NMRA_SpeedStep := ldssDefault;

          SearchStr := SourceMessage.TractionSearchDecodeSearchString;

          if TryStrToInt(SearchStr, SearchDccAddress) then                       // Gaurd against an empty string
          begin
            SearchDccAddress := StrToInt(SearchStr);
            ForceLongAddress := False;                          // Setup up what we call defaults
            SpeedStep := ldss14;                                // Setup up what we call defaults

            if SourceMessage.TractionSearchIsProtocolAny then
            begin

            end else
            if SourceMessage.TractionSearchIsProtocolDCC(NMRA_ForceLongAddress, NMRA_SpeedStep) then
            begin
              // Was a NMRA DCC message so look for the DCC specific information that overrides our defaults
              SourceMessage.TractionSearchIsProtocolDCC(ForceLongAddress, SpeedStep);

              // Look for an existing Train
              ATrain := (OwnerNode as TLccCommandStationNode).FindTrainByDccAddress(SearchDccAddress, ForceLongAddress);

              if (ATrain = nil) and SourceMessage.TractionSearchIsForceAllocate then
              begin
                ATrain := (OwnerNode as TLccCommandStationNode).AddTrain('New ATrain', SearchStr, SearchDccAddress, ForceLongAddress, SpeedStep);
                FEvent := SourceMessage.ExtractDataBytesAsEventID(0);
                ATrain.Login(NULL_NODE_ID);
                TargetNodeID := ATrain.NodeID;
                // Alias will change
              end else
              begin  // Send back the existing node
                FEvent := SourceMessage.ExtractDataBytesAsEventID(0);
                LocalEvent := FEvent;
                WorkerMessage.LoadProducerIdentified(ATrain.NodeID, ATrain.AliasID, LocalEvent, evs_Valid);
                OwnerNode.SendMessageFunc(ATrain, WorkerMessage);      // Need to send the train for this one
                FState := ltsComplete;
              end;
            end
          end
        end;
      end;
    end;
end;

procedure TLccTaskCommandStationTrainSearch.Start(SourceMessage: TLccMessage;
  CallProcessMessage: Boolean);
begin
  inherited Start(SourceMessage, CallProcessMessage);
end;


{ TLccCommandStationNode }

function TLccCommandStationNode.AddTrain(ARoadName, ARoadNumber: string;
  ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep
  ): TLccTrainCanNode;
begin
  Result := (NodeManager as INodeManager).AddNodeByClass('', TLccTrainCanNode, False) as TLccTrainCanNode;
  if Assigned(Result) then
  begin
    Result.Name := ARoadName;
    Result.RoadNumber := ARoadNumber;
    Result.DccAddress := ADccAddress;
    Result.DccLongAddress := ALongAddress;
    Result.SpeedStep := ASpeedStep;
  end;
end;

procedure TLccCommandStationNode.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;

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

procedure TLccCommandStationNode.ClearTrains;
var
  i: Integer;
  LocalNodeManager: INodeManager;
  LocalNode: TLccNode;
begin
  LocalNodeManager := NodeManager as INodeManager;
  for i := LocalNodeManager.GetNodeCount - 1 downto 0 do
   begin
     if LocalNodeManager.GetNode(i) is TLccTrainCanNode then
     begin
       LocalNode := LocalNodeManager.ExtractNode(i);
       LocalNode.Free;
     end;
   end;
end;

procedure TLccCommandStationNode.Creating;
begin
  inherited Creating;
end;

destructor TLccCommandStationNode.Destroy;
begin
  inherited Destroy;
end;

function TLccCommandStationNode.FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainCanNode;
var
  i: Integer;
  LocalNodeManager: INodeManager;
begin
  Result := nil;
  LocalNodeManager := NodeManager as INodeManager;
  for i := 0 to LocalNodeManager.GetNodeCount - 1 do
   begin
     if LocalNodeManager.GetNode(i) is TLccTrainCanNode then
     begin
       Result := LocalNodeManager.GetNode(i) as TLccTrainCanNode;
       if (DccAddress = Result.DccAddress) and (IsLongAddress = Result.DccLongAddress) then
         Break;
     end;
   end;
end;

function TLccCommandStationNode.FindTrainByLccID(TestNode: TLccMessage): TLccTrainCanNode;
var
  i: Integer;
  LocalNodeManager: INodeManager;
begin
  Result := nil;
  LocalNodeManager := NodeManager as INodeManager;
  for i := 0 to LocalNodeManager.GetNodeCount - 1 do
   begin
     if LocalNodeManager.GetNode(i) is TLccTrainCanNode then
     begin
       Result := LocalNodeManager.GetNode(i) as TLccTrainCanNode;
       if EqualNode(TestNode.SourceID, TestNode.CAN.SourceAlias, Result.NodeID, Result.AliasID) then
         Break;
     end;
   end;
end;

function TLccCommandStationNode.GetCdiFile: string;
begin
  Result := CDI_XML_COMMANDSTATION;
end;

function TLccCommandStationNode.ProcessMessage(SourceMessage: TLccMessage): Boolean;
var
  NewTask: TLccTaskCommandStationTrainSearch;
begin
  Result := inherited ProcessMessage(SourceMessage);
  case SourceMessage.MTI of
    MTI_PRODUCER_IDENDIFY :
      begin
        if SourceMessage.TractionSearchIsEvent then    // Is the the event for for traction search?
        begin
          NewTask := TLccTaskCommandStationTrainSearch.Create(Self, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
          ActiveTask := NewTask;
          NewTask.Start(SourceMessage, True);
        end;
      end;
  end;
end;

end.

