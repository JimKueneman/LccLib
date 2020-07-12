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
    function FindTrainByLccNodeID(ANodeID: TNodeID): TLccTrainCanNode;
    function FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainCanNode;
    function ProcessMessage(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccCommandStationNodeClass = class of TLccCommandStationNode;


implementation

uses
  lcc_node_manager;

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
  TempTrain: TLccTrainCanNode;
begin
  Result := nil;
  LocalNodeManager := NodeManager as INodeManager;
  for i := 0 to LocalNodeManager.GetNodeCount - 1 do
   begin
     if LocalNodeManager.GetNode(i) is TLccTrainCanNode then
     begin
       TempTrain := LocalNodeManager.GetNode(i) as TLccTrainCanNode;
       if (DccAddress = TempTrain.DccAddress) and (IsLongAddress = TempTrain.DccLongAddress) then
       begin
         Result := TempTrain;
         Break;
       end;
     end;
   end;
end;

function TLccCommandStationNode.FindTrainByLccNodeID(ANodeID: TNodeID): TLccTrainCanNode;
var
  i: Integer;
  LocalNodeManager: INodeManager;
  LocalTrainNode: TLccTrainCanNode;
begin
  Result := nil;
  LocalNodeManager := NodeManager as INodeManager;
  for i := 0 to LocalNodeManager.GetNodeCount - 1 do
   begin
     if LocalNodeManager.GetNode(i) is TLccTrainCanNode then
     begin
       LocalTrainNode := LocalNodeManager.GetNode(i) as TLccTrainCanNode;
       if EqualNodeID(ANodeID, LocalTrainNode.NodeID, False) then
       begin
         Result := LocalTrainNode;
         Break;
       end;
     end;
   end;
end;

function TLccCommandStationNode.GetCdiFile: string;
begin
  Result := CDI_XML_COMMANDSTATION;
end;

function TLccCommandStationNode.ProcessMessage(SourceMessage: TLccMessage): Boolean;
var
  NMRA_SpeedStep: TLccDccSpeedStep;
  SearchStr: string;
  {$IFDEF DWSCRIPT}
  SearchDccAddress: Integer;
  {$ELSE}
  SearchDccAddress: LongInt;
  {$ENDIF}
  ForceAllocate, ExactMatchOnly, MatchAddressOnly, LongAddressOnly, IsDCC: Boolean;
  ATrain: TLccTrainCanNode;
  ReturnEvent: TEventID;
  TrackProtocolFlags: Byte;
  SearchData: DWORD;
  ANodeID: TNodeID;
begin
  Result := inherited ProcessMessage(SourceMessage);

   // We only are dealing with messages with destinations for us from here on
  if SourceMessage.HasDestination then
  begin
    if not IsDestinationEqual(SourceMessage) then
      Exit;
  end;

  case SourceMessage.MTI of
     MTI_INITIALIZATION_COMPLETE :
      begin  // Need to wait for new trains to fully initalize before returning from the Traction Search Event........
        ANodeID := NULL_NODE_ID;
          ATrain := FindTrainByLccNodeID(SourceMessage.SourceID);
          if Assigned(ATrain) then
          begin
            ReturnEvent := ATrain.SearchEvent;
            WorkerMessage.LoadProducerIdentified(ATrain.NodeID, ATrain.AliasID, ReturnEvent, evs_Valid);
            SendMessageFunc(ATrain, WorkerMessage);
          end
      end;
    MTI_PRODUCER_IDENDIFY :
      begin
        if SourceMessage.TractionSearchIsEvent then    // Is the the event for for traction search?
        begin
          SearchStr := SourceMessage.TractionSearchDecodeSearchString;

          if SearchStr <> '' then                       // Gaurd against an empty string
          begin
            SearchDccAddress := StrToInt(SearchStr);
            LongAddressOnly := False;
            NMRA_SpeedStep := ldss14;

            if SourceMessage.TractionSearchIsProtocolAny then
            begin
              NMRA_SpeedStep := ldss14;
              LongAddressOnly := False;
              IsDcc := True;
            end else
            if SourceMessage.TractionSearchIsProtocolDCC(LongAddressOnly, NMRA_SpeedStep) then
            begin
              if NMRA_SpeedStep = ldssDefault then
                NMRA_SpeedStep := ldss14;
              IsDCC := True;
            end else
              IsDCC := False;                 //    IF I CHANGE TO LONG ADDRESS OR CHANGE SPEED STEP SHOULD THAT BE A NEW TRAIN NODE???????

            if IsDCC then
            begin
              // Create an Event that contains what we actually created from the search parameters
              ForceAllocate := SourceMessage.TractionSearchIsForceAllocate;
              ExactMatchOnly := SourceMessage.TractionSearchIsExactMatchOnly;
              MatchAddressOnly := SourceMessage.TractionSearchIsAddressMatchOnly;
              TrackProtocolFlags := SourceMessage.TractionSearchEncodeNMRA(LongAddressOnly, NMRA_SpeedStep, ForceAllocate, ExactMatchOnly, MatchAddressOnly);
              WorkerMessage.TractionSearchEncodeSearchString(SearchStr, TrackProtocolFlags, SearchData);
              WorkerMessage.LoadTractionSearch(NodeID, AliasID, SearchData);
              ReturnEvent := WorkerMessage.ExtractDataBytesAsEventID(0);

              // Look for an existing Train
              ATrain := FindTrainByDccAddress(SearchDccAddress, LongAddressOnly);

              if (ATrain = nil) and ForceAllocate then
              begin
                ATrain := AddTrain('New ATrain', SearchStr, SearchDccAddress, LongAddressOnly, NMRA_SpeedStep);
                ATrain.SearchEvent := ReturnEvent;
                ATrain.Login(NULL_NODE_ID);
                // Alias will change
              end else
              begin  // Send back the existing node
                if Assigned(ATrain) then
                begin
                  WorkerMessage.LoadProducerIdentified(ATrain.NodeID, ATrain.AliasID, ReturnEvent, evs_Valid);
                  SendMessageFunc(ATrain, WorkerMessage);      // Need to send the train for this one
                end
              end;
            end
          end
        end;
      end;
  end;
end;

end.

