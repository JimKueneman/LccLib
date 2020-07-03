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

  { TLccWaitingPatient }

  TLccWaitingPatient = class(TObject)
  private
    FNodeWaitingToInitialize: TLccCanNode;
    FWaitingEvent: TEventID;

  public
    property NodeWaitingToInitialize: TLccCanNode read FNodeWaitingToInitialize write FNodeWaitingToInitialize;
    property WaitingEvent: TEventID read FWaitingEvent write FWaitingEvent;
  end;

  { TLccWaitingRoom }

  TLccWaitingRoom = class(TObject)
  private
    FPatients: TObjectList;
    function GetLccWaitingPatients(Index: Integer): TLccWaitingPatient;
    procedure SetLccWaitingPatients(Index: Integer; AValue: TLccWaitingPatient);
  protected
    property Patients: TObjectList read FPatients write FPatients;
  public
    property LccWaitingPatients[Index: Integer]: TLccWaitingPatient read GetLccWaitingPatients write SetLccWaitingPatients;

    constructor Create;
    destructor Destroy; override;

    function AddPatient(NodePatient: TLccCanNode; EventPatient: TEventID): TLccWaitingPatient;
    procedure ClearPatients;
    function FindPatient(NodeID: TNodeID; AliasID: Word): TLccWaitingPatient;
    procedure ReleasePatient(PatientNode: TLccCanNode);
  end;

  { TLccCommandStationNode }

  TLccCommandStationNode = class(TLccCanNode)
  private
    FTrains: TObjectList;
    FWaitingRoom: TLccWaitingRoom;
    function GetTrain(Index: Integer): TLccTrainCanNode;
    procedure SetTrain(Index: Integer; AValue: TLccTrainCanNode);
  protected
    property WaitingRoom: TLccWaitingRoom read FWaitingRoom write FWaitingRoom;
    procedure Creating; override;
  public
    property Trains: TObjectList read FTrains write FTrains;
    property Train[Index: Integer]: TLccTrainCanNode read GetTrain write SetTrain;

    destructor Destroy; override;
    function AddTrain(ARoadName, ARoadNumber: string; ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep): TLccTrainCanNode;
    procedure ClearTrains;
    function FindTrainByLccID(TestNode: TLccMessage): TLccTrainCanNode;
    function FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainCanNode;
    function ProcessMessage(SourceLccMessage: TLccMessage): Boolean; override;
  end;


implementation

{ TLccWaitingRoom }

constructor TLccWaitingRoom.Create;
begin
  inherited;
  FPatients := TObjectList.Create(False);
end;

function TLccWaitingRoom.AddPatient(NodePatient: TLccCanNode;
  EventPatient: TEventID): TLccWaitingPatient;
begin
  Result := TLccWaitingPatient.Create;
  Result.NodeWaitingToInitialize := NodePatient;
  Result.WaitingEvent := EventPatient;
  Patients.Add(Result);
end;

procedure TLccWaitingRoom.ClearPatients;
var
  i: Integer;
  APatient: TObject;
begin
  try
    for i := 0 to Patients.Count - 1 do
    begin
      APatient :=  Patients[i];
      FreeAndNil(APatient);
    end
  finally
     Patients.Clear;
  end;
end;

destructor TLccWaitingRoom.Destroy;
begin
  ClearPatients;
  FreeAndNil(FPatients);
  inherited Destroy;
end;

function TLccWaitingRoom.FindPatient(NodeID: TNodeID; AliasID: Word): TLccWaitingPatient;
var
  i: Integer;
begin
  for i := 0 to Patients.Count - 1 do
  begin
    if EqualNode(NodeID, AliasID, LccWaitingPatients[i].NodeWaitingToInitialize.NodeID, LccWaitingPatients[i].NodeWaitingToInitialize.AliasID) then
    begin
      Result := LccWaitingPatients[i];
      Break
    end;
  end;
end;

function TLccWaitingRoom.GetLccWaitingPatients(Index: Integer): TLccWaitingPatient;
begin
  Result := Patients[Index] as TLccWaitingPatient
end;

procedure TLccWaitingRoom.ReleasePatient(PatientNode: TLccCanNode);
var
  Patient: TLccWaitingPatient;
begin
  Patient := Patients.Extract(PatientNode) as TLccWaitingPatient;
  if Assigned(Patient) then
    Patient.Free
end;

procedure TLccWaitingRoom.SetLccWaitingPatients(Index: Integer;
  AValue: TLccWaitingPatient);
begin
  if Patients[Index] <> nil then
    Patients[Index].Free;
  Patients[Index] := AValue
end;

{ TLccCommandStationNode }

function TLccCommandStationNode.AddTrain(ARoadName, ARoadNumber: string;
  ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep): TLccTrainCanNode;
begin
  Result := TLccTrainCanNode.Create(SendMessageFunc, NodeManager, CDI_XML_TRAIN_NODE);
  Result.DccAddress := ADccAddress;
  Result.DccLongAddress := ALongAddress;
  Result.Name := ARoadName;
  Result.RoadNumber := ARoadNumber;
  Result.SpeedStep := ASpeedStep;

  Result.ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  Result.ProtocolSupportedProtocols.Datagram := True;
  Result.ProtocolSupportedProtocols.EventExchange := True;
  Result.ProtocolSupportedProtocols.SimpleNodeInfo := True;
  Result.ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;
  Result.ProtocolSupportedProtocols.TractionControl := True;
  Result.ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := True;
  Result.ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := True;
  Result.ProtocolSupportedProtocols.TractionFunctionConfiguration := True;

  Result.ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
  Result.ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
  Result.ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  Result.ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  Result.ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);
  Result.ProtocolMemoryInfo.Add(MSI_TRACTION_FDI, True, True, True, 0, $FFFFFFFF);
  Result.ProtocolMemoryInfo.Add(MSI_TRACTION_FUNCTION_CONFIG, True, False, True, 0, $FFFFFFFF);

  Result.ProtocolMemoryOptions.WriteUnderMask := True;
  Result.ProtocolMemoryOptions.UnAlignedReads := True;
  Result.ProtocolMemoryOptions.UnAlignedWrites := True;
  Result.ProtocolMemoryOptions.SupportACDIMfgRead := True;
  Result.ProtocolMemoryOptions.SupportACDIUserRead := True;
  Result.ProtocolMemoryOptions.SupportACDIUserWrite := True;
  Result.ProtocolMemoryOptions.WriteLenOneByte := True;
  Result.ProtocolMemoryOptions.WriteLenTwoBytes := True;
  Result.ProtocolMemoryOptions.WriteLenFourBytes := True;
  Result.ProtocolMemoryOptions.WriteLenSixyFourBytes := True;
  Result.ProtocolMemoryOptions.WriteArbitraryBytes := True;
  Result.ProtocolMemoryOptions.WriteStream := False;
  Result.ProtocolMemoryOptions.HighSpace := MSI_CDI;
  Result.ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;

 // NodeManager.;

 // TODO
 //   NEED TO ADD IT TO THE NODE MANAGER!!!!!!!!!!


  Result.Login(NULL_NODE_ID);
end;

procedure TLccCommandStationNode.ClearTrains;
var
  i: Integer;
  ATrain: TLccTrainCanNode;
begin
  try
    for i := 0 to Trains.Count - 1 do
    begin
      ATrain := Trains[i] as TLccTrainCanNode;
      ATrain.Free;
    end;
  finally
    Trains.Clear;
  end;
end;

procedure TLccCommandStationNode.Creating;
begin
  inherited Creating;
  FTrains := TObjectList.Create(False);
  FWaitingRoom := TLccWaitingRoom.Create;
end;

destructor TLccCommandStationNode.Destroy;
begin
  ClearTrains;
  FreeAndNil(FTrains);
  WaitingRoom.ClearPatients;
  FreeAndNil(FWaitingRoom);
  inherited Destroy;
end;

function TLccCommandStationNode.FindTrainByDccAddress(DccAddress: Word; IsLongAddress: Boolean): TLccTrainCanNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Trains.Count - 1 do
   begin
    if (DccAddress = Train[i].DccAddress) and (IsLongAddress = Train[i].DccLongAddress) then
    begin
      Result := Train[i];
      Break;
    end;
   end;
end;

function TLccCommandStationNode.FindTrainByLccID(TestNode: TLccMessage): TLccTrainCanNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Trains.Count - 1 do
  begin
    if EqualNode(TestNode.SourceID, TestNode.CAN.SourceAlias, Train[i].NodeID, Train[i].AliasID) then
    begin
      Result := Train[i];
      Exit;
    end;
  end;
end;

function TLccCommandStationNode.GetTrain(Index: Integer): TLccTrainCanNode;
begin
  Result := Trains[Index] as TLccTrainCanNode;
end;

function TLccCommandStationNode.ProcessMessage(SourceLccMessage: TLccMessage): Boolean;
var
  NMRA_SpeedStep: TLccDccSpeedStep;
  NMRA_ForceLongAddress: Boolean;
  SearchStr: string;
  SearchDccAddress: LongInt;
  ForceLongAddress: Boolean;
  SpeedStep: TLccDccSpeedStep;
  AnEvent: TEventID;
  ATrain: TLccTrainCanNode;
  APatient: TLccWaitingPatient;
begin
  Result := inherited ProcessMessage(SourceLccMessage);
  case SourceLccMessage.MTI of
    MTI_INITIALIZATION_COMPLETE :
      begin  // Need to wait for new trains to fully initalize before returning from the Traction Search Event........
        APatient := WaitingRoom.FindPatient(SourceLccMessage.SourceID, SourceLccMessage.CAN.SourceAlias);
        if Assigned(APatient) then
        begin
          ATrain := FindTrainByLccID(SourceLccMessage);
          if APatient.NodeWaitingToInitialize = ATrain then
          begin
            WorkerMessage.LoadProducerIdentified(ATrain.NodeID, ATrain.AliasID, APatient.FWaitingEvent, evs_Valid);
            SendMessageFunc(WorkerMessage);
            WaitingRoom.ReleasePatient(APatient.NodeWaitingToInitialize);
          end
        end;
      end;
    MTI_PRODUCER_IDENTIFIED_CLEAR,
    MTI_PRODUCER_IDENTIFIED_SET,
    MTI_PRODUCER_IDENTIFIED_UNKNOWN :
      begin
        if SourceLccMessage.TractionSearchIsEvent then    // Is the the event for for traction search?
        begin
          NMRA_ForceLongAddress := False;
          NMRA_SpeedStep := ldssDefault;

          SearchStr := SourceLccMessage.TractionSearchDecodeSearchString;

          if TryStrToInt(SearchStr, SearchDccAddress) then                       // Gaurd against an empty string
          begin
            SearchDccAddress := StrToInt(SearchStr);
            ForceLongAddress := False;                          // Setup up what we call defaults
            SpeedStep := ldss14;                                // Setup up what we call defaults

            if SourceLccMessage.TractionSearchIsProtocolAny then
            begin

            end else
            if SourceLccMessage.TractionSearchIsProtocolDCC(NMRA_ForceLongAddress, NMRA_SpeedStep) then
            begin
              // Was a NMRA DCC message so look for the DCC specific information that overrides our defaults
              SourceLccMessage.TractionSearchIsProtocolDCC(ForceLongAddress, SpeedStep);

              // Look for an existing Train
              ATrain := FindTrainByDccAddress(SearchDccAddress, ForceLongAddress);

              if (ATrain = nil) and SourceLccMessage.TractionSearchIsForceAllocate then
              begin
                ATrain := AddTrain('New ATrain', SearchStr, SearchDccAddress, ForceLongAddress, SpeedStep);
                AnEvent := SourceLccMessage.ExtractDataBytesAsEventID(0);
                APatient := WaitingRoom.AddPatient(ATrain, AnEvent);
                APatient.NodeWaitingToInitialize := ATrain;
                APatient.FWaitingEvent := SourceLccMessage.ExtractDataBytesAsEventID(0);
              end else
              begin  // Send back the existing node
                AnEvent := SourceLccMessage.ExtractDataBytesAsEventID(0);
                WorkerMessage.LoadProducerIdentified(ATrain.NodeID, ATrain.AliasID, AnEvent, evs_Valid);
                SendMessageFunc(WorkerMessage);
              end;
            end
          end
        end
      end;
  end;
end;

procedure TLccCommandStationNode.SetTrain(Index: Integer; AValue: TLccTrainCanNode);
begin
  if (Trains[Index] <> nil) then
    (Trains[Index] as TLccTrainCanNode).Free;
  Trains[Index] := AValue;
end;

end.

