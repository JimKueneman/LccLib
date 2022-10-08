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
      {$IFNDEF FPC_CONSOLE_APP}
        ExtCtrls,
      {$ENDIF}
    {$ELSE}
      System.Types,
      FMX.Types,
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF DWSCRIPT}
    {$IFNDEF FPC}
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
  '</cdi>'
  );

type

  { TLccCommandStationNode }

  TLccCommandStationNode = class(TLccNode)
  protected
    function GetCdiFile: string; override;
    procedure BeforeLogin; override;

  public
    constructor Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); override;
    destructor Destroy; override;
    function AddTrain(ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep): TLccTrainDccNode;
    procedure ClearTrains;
    function FindTrainByLccNodeID(ANodeID: TNodeID): TLccTrainDccNode;
    function FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainDccNode;
    function ProcessMessageLcc(SourceMessage: TLccMessage): Boolean; override;
  end;

  TLccCommandStationNodeClass = class of TLccCommandStationNode;


implementation

uses
  lcc_node_manager;

function TLccCommandStationNode.AddTrain(ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep): TLccTrainDccNode;
begin
  Result := (NodeManager as INodeManager).AddNodeByClass('', TLccTrainDccNode, False, NULL_NODE_ID) as TLccTrainDccNode;
  if Assigned(Result) then
  begin
    Result.DccAddress := ADccAddress;
    Result.DccLongAddress := ALongAddress;
    Result.DccSpeedStep := ASpeedStep;
  end;
end;

procedure TLccCommandStationNode.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.MemConfig := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;

  ProtocolEventConsumed.Add(EVENT_EMERGENCY_STOP, evs_InValid);

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

constructor TLccCommandStationNode.Create(ASendMessageFunc: TOnMessageEvent; ANodeManager: TObject; CdiXML: string; GridConnectLink: Boolean);
begin
  inherited Create(ASendMessageFunc, ANodeManager, CdiXML, GridConnectLink);
  EnableTrainDatabase := True;
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
     if LocalNodeManager.GetNode(i) is TLccTrainDccNode then
     begin
       LocalNode := LocalNodeManager.ExtractNode(i);
       LocalNode.Free;
     end;
   end;
end;

destructor TLccCommandStationNode.Destroy;
begin
  inherited Destroy;
end;

function TLccCommandStationNode.FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainDccNode;
var
  i: Integer;
  LocalNodeManager: INodeManager;
  TempTrain: TLccTrainDccNode;
begin
  Result := nil;
  LocalNodeManager := NodeManager as INodeManager;
  for i := 0 to LocalNodeManager.GetNodeCount - 1 do
   begin
     if LocalNodeManager.GetNode(i) is TLccTrainDccNode then
     begin
       TempTrain := LocalNodeManager.GetNode(i) as TLccTrainDccNode;
       if (DccAddress = TempTrain.DccAddress) and (IsLongAddress = TempTrain.DccLongAddress) then
       begin
         Result := TempTrain;
         Break;
       end;
     end;
   end;
end;

function TLccCommandStationNode.FindTrainByLccNodeID(ANodeID: TNodeID): TLccTrainDccNode;
var
  i: Integer;
  LocalNodeManager: INodeManager;
  LocalTrainNode: TLccTrainDccNode;
begin
  Result := nil;
  LocalNodeManager := NodeManager as INodeManager;
  for i := 0 to LocalNodeManager.GetNodeCount - 1 do
   begin
     if LocalNodeManager.GetNode(i) is TLccTrainDccNode then
     begin
       LocalTrainNode := LocalNodeManager.GetNode(i) as TLccTrainDccNode;
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

function TLccCommandStationNode.ProcessMessageLcc(SourceMessage: TLccMessage): Boolean;
var
  NMRA_SpeedStep: TLccDccSpeedStep;
  SearchStr: string;
  {$IFDEF DWSCRIPT}
  SearchDccAddress: Integer;
  {$ELSE}
  SearchDccAddress: LongInt;
  {$ENDIF}
  LongAddressOnly, IsDCC: Boolean;
  ATrain: TLccTrainDccNode;
  ReturnEvent: TEventID;
begin
  Result := inherited ProcessMessageLcc(SourceMessage);

  case SourceMessage.MTI of
     MTI_INITIALIZATION_COMPLETE :
      begin  // Need to wait for new trains to fully initalize before returning from the Traction Search Event........
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
        if SourceMessage.TractionIsSearchEvent then    // Is the the event for for traction search?
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
              // Look for an existing Train
              ATrain := FindTrainByDccAddress(SearchDccAddress, LongAddressOnly);
              ReturnEvent := SourceMessage.ExtractDataBytesAsEventID(0); // Return the exact same event

              if Assigned(ATrain) then
              begin
                WorkerMessage.LoadProducerIdentified(ATrain.NodeID, ATrain.AliasID, ReturnEvent, evs_Valid);
                SendMessageFunc(ATrain, WorkerMessage);      // Need to send the train for this one
              end else
              if SourceMessage.TractionSearchIsForceAllocate then
              begin
                ATrain := AddTrain(SearchDccAddress, LongAddressOnly, NMRA_SpeedStep);
                ATrain.SearchEvent := ReturnEvent; // Store to send later as we don't have an alias or a login for the node yet
                ATrain.Login(NULL_NODE_ID);
              end
            end
          end
        end;
      end;
  end;
end;

end.

