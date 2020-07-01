unit TrainCommanderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, lcc_ethernet_server, lcc_math_float16, lcc_defines, lcc_node,
  lcc_node_manager, lcc_ethernet_client, lcc_utilities, lcc_node_messages,
  TrainDatabaseUnit;

const

 CDI_XML: string = (
'<?xml version="1.0" encoding="utf-8"?>'+
'<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
'<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
	'<identification>'+
		'<manufacturer>Mustangpeak</manufacturer>'+
		'<model>TC1000</model>'+
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

  { TFormTrainCommander }

  TFormTrainCommander = class(TForm)
    ButtonClear: TButton;
    ButtonManualConnect: TButton;
    CheckBoxLoopBackIP: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    ImageListMain: TImageList;
    ListViewTrains: TListView;
    ListviewConnections: TListView;
    MemoLog: TMemo;
    PanelTrainsHeader: TPanel;
    PanelTrains: TPanel;
    PanelConnections: TPanel;
    PanelDetails: TPanel;
    SplitterTrains: TSplitter;
    SplitterConnections: TSplitter;
    StatusBarMain: TStatusBar;
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonManualConnectClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCommandStationNode: TLccCanNode;
    FLocalLccMessage: TLccMessage;
    FNodeManager: TLccCanNodeManager;
    FLccServer: TLccEthernetServer;
    FTrainDatabase: TLccTrainDatabase;
  protected
    property LocalLccMessage: TLccMessage read FLocalLccMessage write FLocalLccMessage;

    procedure CreateCommandStationNode;
    function CreateTrainNode(ARoadName, ARoadNumber: string; ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep): TLccTrain;
    procedure DestroyCommandStationNode;
    function ConnectServer: Boolean;
    procedure DisconnectServer;


    procedure OnCommandStationConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationClientDisconnect(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);

    procedure OnNodeSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeIdentifyProducers(Sender: TObject; LccSourceNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
  public
    property LccServer: TLccEthernetServer read FLccServer write FLccServer;
    property NodeManager: TLccCanNodeManager read FNodeManager write FNodeManager;
    property TrainDatabase: TLccTrainDatabase read FTrainDatabase write FTrainDatabase;
    property CommandStationNode: TLccCanNode read FCommandStationNode;
  end;

var
  FormTrainCommander: TFormTrainCommander;

implementation

{$R *.lfm}

{ TFormTrainCommander }

procedure TFormTrainCommander.ButtonClearClick(Sender: TObject);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Clear;
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.ButtonManualConnectClick(Sender: TObject);
begin
  if LccServer.Connected then
    DisconnectServer
  else
    ConnectServer;
end;

function TFormTrainCommander.ConnectServer: Boolean;
var
  EthernetRec: TLccEthernetRec;
begin
  Result := False;
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.AutoResolveIP := not CheckBoxLoopBackIP.Checked;
  EthernetRec.ListenerIP := '127.0.0.1';
  EthernetRec.ListenerPort := 12021;
  Result := LccServer.OpenConnection(EthernetRec) <> nil;
end;

procedure TFormTrainCommander.CreateCommandStationNode;

begin
  FCommandStationNode := NodeManager.AddNode(CDI_XML) as TLccCanNode;

  FCommandStationNode.ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  FCommandStationNode.ProtocolSupportedProtocols.Datagram := True;
  FCommandStationNode.ProtocolSupportedProtocols.EventExchange := True;
  FCommandStationNode.ProtocolSupportedProtocols.SimpleNodeInfo := True;
  FCommandStationNode.ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;
  // Command Station is not a train so it gets none of these set
  FCommandStationNode.ProtocolSupportedProtocols.TractionControl := False;
  FCommandStationNode.ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := False;
  FCommandStationNode.ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := False;
  FCommandStationNode.ProtocolSupportedProtocols.TractionFunctionConfiguration := False;

  FCommandStationNode.ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
  FCommandStationNode.ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
  FCommandStationNode.ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  FCommandStationNode.ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  FCommandStationNode.ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);
  FCommandStationNode.ProtocolMemoryInfo.Add(MSI_TRACTION_FDI, True, True, True, 0, $FFFFFFFF);
  FCommandStationNode.ProtocolMemoryInfo.Add(MSI_TRACTION_FUNCTION_CONFIG, True, False, True, 0, $FFFFFFFF);

  FCommandStationNode.ProtocolMemoryOptions.WriteUnderMask := True;
  FCommandStationNode.ProtocolMemoryOptions.UnAlignedReads := True;
  FCommandStationNode.ProtocolMemoryOptions.UnAlignedWrites := True;
  FCommandStationNode.ProtocolMemoryOptions.SupportACDIMfgRead := True;
  FCommandStationNode.ProtocolMemoryOptions.SupportACDIUserRead := True;
  FCommandStationNode.ProtocolMemoryOptions.SupportACDIUserWrite := True;
  FCommandStationNode.ProtocolMemoryOptions.WriteLenOneByte := True;
  FCommandStationNode.ProtocolMemoryOptions.WriteLenTwoBytes := True;
  FCommandStationNode.ProtocolMemoryOptions.WriteLenFourBytes := True;
  FCommandStationNode.ProtocolMemoryOptions.WriteLenSixyFourBytes := True;
  FCommandStationNode.ProtocolMemoryOptions.WriteArbitraryBytes := True;
  FCommandStationNode.ProtocolMemoryOptions.WriteStream := False;
  FCommandStationNode.ProtocolMemoryOptions.HighSpace := MSI_CDI;
  FCommandStationNode.ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;

//    FCommandStationNode.ProtocolEventConsumed.AutoGenerate.Count := 5;
//    FCommandStationNode.ProtocolEventConsumed.AutoGenerate.StartIndex := 0;

//   FCommandStationNode.ProtocolEventsProduced.AutoGenerate.Count := 5;
//   FCommandStationNode.ProtocolEventsProduced.AutoGenerate.StartIndex := 0;

  CommandStationNode.Login(NULL_NODE_ID); // Create our own ID
end;

procedure TFormTrainCommander.DestroyCommandStationNode;
begin
  NodeManager.Clear;
end;

procedure TFormTrainCommander.DisconnectServer;
begin
  LccServer.CloseConnection(nil);
end;

procedure TFormTrainCommander.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  LccServer.CloseConnection(nil);
end;

procedure TFormTrainCommander.FormCreate(Sender: TObject);
begin
  FLccServer := TLccEthernetServer.Create(self);
  LccServer.OnConnectionStateChange := @OnCommandStationConnectionState;
  LccServer.OnClientDisconnect := @OnCommandStationClientDisconnect;
  LccServer.OnErrorMessage := @OnCommandStationErrorMessage;
  LccServer.Gridconnect := True;
  LccServer.Hub := True;

  NodeManager := TLccCanNodeManager.Create(self);
  NodeManager.OnLccMessageReceive := @OnNodeReceiveMessage;
  NodeManager.OnLccMessageSend := @OnNodeSendMessage;
  NodeManager.OnLccNodeProducerIdentify := @OnNodeIdentifyProducers;

  LccServer.NodeManager := NodeManager;

  FTrainDatabase := TLccTrainDatabase.Create;

  FLocalLccMessage := TLccMessage.Create;

end;

procedure TFormTrainCommander.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrainDatabase);
  FreeAndNil(FLocalLccMessage);
end;

procedure TFormTrainCommander.OnCommandStationClientDisconnect(Sender: TObject; EthernetRec: TLccEthernetRec);
begin

end;

procedure TFormTrainCommander.OnCommandStationConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        ButtonManualConnect.Enabled := False;
        StatusBarMain.Panels[0].Text := 'Command Station Disconnecting from Ethernet';
        StatusBarMain.Panels[1].Text := '';
      end;
    ccsListenerConnected :
      begin
        ButtonManualConnect.Enabled := True;
        ButtonManualConnect.Caption := 'Manual Disconnect';
        StatusBarMain.Panels[0].Text := 'Command Station Connected at: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        CreateCommandStationNode;
      end;
    ccsListenerDisconnecting :
      begin
        ButtonManualConnect.Enabled := False;
        ButtonManualConnect.Caption := 'Command Station Disconnecting from Ethernet';
        DestroyCommandStationNode;
      end;
    ccsListenerDisconnected :
      begin
        ButtonManualConnect.Enabled := True;
        ButtonManualConnect.Caption := 'Manual Connect';
        StatusBarMain.Panels[0].Text := 'Command Station Disconnected from Ethernet';
      end;
    ccsListenerClientConnecting :
      begin
         StatusBarMain.Panels[1].Text := 'New Throttle Connecting to Command Station'
      end;
    ccsListenerClientConnected :
      begin
         StatusBarMain.Panels[1].Text := 'New Throttle Connected to Command Station';
         ListItem := ListviewConnections.Items.Add;
         Listitem.Caption := 'Throttle connected: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
         ListItem.ImageIndex := 3;
      end;
    ccsListenerClientDisconnecting :
      begin
        StatusBarMain.Panels[1].Text := 'Throttle Disconnecting from Command Station';
      end;
    ccsListenerClientDisconnected :
      begin
        StatusBarMain.Panels[1].Text := 'Throttle Disconnected from Command Station';
        ListItem := ListviewConnections.FindCaption(0, 'Throttle connected: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort), True, True, True, True);
        if Assigned(ListItem) then
          ListviewConnections.Items.Delete(ListItem.Index);
      end;
  end;
end;

procedure TFormTrainCommander.OnCommandStationErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  StatusBarMain.Panels[1].Text := EthernetRec.MessageStr;
end;

procedure TFormTrainCommander.OnNodeReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Add('R: ' + LccMessage.ConvertToGridConnectStr('', False));
    MemoLog.SelStart := Length(MemoLog.Lines.Text);
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TFormTrainCommander.OnNodeSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  LccServer.SendMessage(LccMessage);
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Add('S: ' + LccMessage.ConvertToGridConnectStr('', False));
    MemoLog.SelStart := Length(MemoLog.Lines.Text);
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

function TFormTrainCommander.CreateTrainNode(ARoadName, ARoadNumber: string;
  ADccAddress: Word; ALongAddress: Boolean; ASpeedStep: TLccDccSpeedStep
  ): TLccTrain;
var
  CanNode: TLccCanNode;
begin
  CanNode := NodeManager.AddNode(CDI_XML_TRAIN_NODE) as TLccCanNode;

  CanNode.ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  CanNode.ProtocolSupportedProtocols.Datagram := True;
  CanNode.ProtocolSupportedProtocols.EventExchange := True;
  CanNode.ProtocolSupportedProtocols.SimpleNodeInfo := True;
  CanNode.ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;
  CanNode.ProtocolSupportedProtocols.TractionControl := True;
  CanNode.ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := True;
  CanNode.ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := True;
  CanNode.ProtocolSupportedProtocols.TractionFunctionConfiguration := True;

  CanNode.ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
  CanNode.ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
  CanNode.ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  CanNode.ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  CanNode.ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);
  CanNode.ProtocolMemoryInfo.Add(MSI_TRACTION_FDI, True, True, True, 0, $FFFFFFFF);
  CanNode.ProtocolMemoryInfo.Add(MSI_TRACTION_FUNCTION_CONFIG, True, False, True, 0, $FFFFFFFF);

  CanNode.ProtocolMemoryOptions.WriteUnderMask := True;
  CanNode.ProtocolMemoryOptions.UnAlignedReads := True;
  CanNode.ProtocolMemoryOptions.UnAlignedWrites := True;
  CanNode.ProtocolMemoryOptions.SupportACDIMfgRead := True;
  CanNode.ProtocolMemoryOptions.SupportACDIUserRead := True;
  CanNode.ProtocolMemoryOptions.SupportACDIUserWrite := True;
  CanNode.ProtocolMemoryOptions.WriteLenOneByte := True;
  CanNode.ProtocolMemoryOptions.WriteLenTwoBytes := True;
  CanNode.ProtocolMemoryOptions.WriteLenFourBytes := True;
  CanNode.ProtocolMemoryOptions.WriteLenSixyFourBytes := True;
  CanNode.ProtocolMemoryOptions.WriteArbitraryBytes := True;
  CanNode.ProtocolMemoryOptions.WriteStream := False;
  CanNode.ProtocolMemoryOptions.HighSpace := MSI_CDI;
  CanNode.ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;

//    CanNode.ProtocolEventConsumed.AutoGenerate.Count := 5;
//    CanNode.ProtocolEventConsumed.AutoGenerate.StartIndex := 0;

//   CanNode.ProtocolEventsProduced.AutoGenerate.Count := 5;
//   CanNode.ProtocolEventsProduced.AutoGenerate.StartIndex := 0;

  CanNode.Login(NULL_NODE_ID);

  Result := TrainDatabase.AddTrain(ARoadName, ARoadNumber, ADccAddress, ALongAddress, ASpeedStep, CanNode);

end;

procedure TFormTrainCommander.OnNodeIdentifyProducers(Sender: TObject;
  LccSourceNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
var
  MarlkinProtocol: TLccMarklinProtocolVersion;
  NMRA_SpeedStep: TLccDccSpeedStep;
  NMRA_ForceLongAddress: Boolean;
  SearchStr: string;
  SearchDccAddress: Word;
  ForceLongAddress: Boolean;
  Train: TLccTrain;
  SpeedStep: TLccDccSpeedStep;
  ListIndex: Integer;
  AnEvent: TEventID;
begin
  // Only the CommandStation replies to this Event
  if LccSourceNode = CommandStationNode then
  begin
    if LccMessage.TractionSearchIsEvent then
    begin
      NMRA_ForceLongAddress := False;
      NMRA_SpeedStep := ldssDefault;

      SearchStr := LccMessage.TractionSearchDecodeSearchString;

      SearchDccAddress := StrToInt(SearchStr);            // Only numbers allowed so has to work
      ForceLongAddress := False;                          // Setup up what we call defaults
      SpeedStep := ldss14;                                // Setup up what we call defaults

      if LccMessage.TractionSearchIsProtocolAny then
      begin

      end else
      if LccMessage.TractionSearchIsProtocolDCC(NMRA_ForceLongAddress, NMRA_SpeedStep) then
      begin
        // Was a NMRA DCC message so look for the DCC specific information that overrides our defaults
        LccMessage.TractionSearchIsProtocolDCC(ForceLongAddress, SpeedStep);
        // Look for an existing Train
        Train := TrainDatabase.FindByDccAddress(SearchDccAddress, ForceLongAddress, ListIndex);

        if (Train = nil) and LccMessage.TractionSearchIsForceAllocate then
          Train := CreateTrainNode('New Train', SearchStr, SearchDccAddress, ForceLongAddress, SpeedStep);

        if (Train <> nil) then
        begin
          AnEvent := LccMessage.ExtractDataBytesAsEventID(0);
          if Train.LccNode is TLccCanNode then
            LocalLccMessage.LoadProducerIdentified(Train.LccNode.NodeID, (Train.LccNode as TLccCanNode).AliasID, AnEvent, evs_Valid )
          else
            LocalLccMessage.LoadProducerIdentified(Train.LccNode.NodeID, 0, AnEvent, evs_Valid );

          NodeManager.SendMessage(LocalLccMessage);
        end;
      end;
    end;
  end;
end;

end.

