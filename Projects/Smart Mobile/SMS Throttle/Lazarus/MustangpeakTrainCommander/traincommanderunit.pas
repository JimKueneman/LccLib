unit TrainCommanderUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, lcc_ethernet_server, lcc_defines, lcc_node,
  lcc_node_manager, lcc_ethernet_client, lcc_node_messages,
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
    ButtonTrainsClear: TButton;
    ButtonClear: TButton;
    ButtonManualConnect: TButton;
    CheckBoxUseSyncronize: TCheckBox;
    CheckBoxLogMessages: TCheckBox;
    CheckBoxLoopBackIP: TCheckBox;
    CheckBoxAutoConnect: TCheckBox;
    ImageListMain: TImageList;
    LabelNodeID: TLabel;
    LabelAliasID: TLabel;
    LabelAliasIDCaption: TLabel;
    LabelNodeIDCaption: TLabel;
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
    TimerIncomingMessagePump: TTimer;
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonManualConnectClick(Sender: TObject);
    procedure ButtonTrainsClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerIncomingMessagePumpTimer(Sender: TObject);
  private
    FCommandStationNode: TLccCanNode;
    FInitializationWait: Boolean;
    FInitializationWaitNode: TLccNode;
    FWorkerMsg: TLccMessage;
    FNodeManager: TLccCanNodeManager;
    FLccServer: TLccEthernetServer;
    FTrainDatabase: TLccTrainDatabase;
  protected
    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
    property InitializationWait: Boolean read FInitializationWait write FInitializationWait;
    property InitializationWaitNode: TLccNode read FInitializationWaitNode write FInitializationWaitNode;

    procedure CreateCommandStationNode;
    function CreateTrainNode: TLccCanNode;
    procedure DestroyCommandStationNode;
    function ConnectServer: Boolean;
    procedure DisconnectServer;

    procedure OnCommandStationConnectionState(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationClientDisconnect(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnCommandStationErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);

    procedure OnNodeSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    procedure OnLccNodeAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnLccNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);

    procedure OnNodeIdentifyProducers(Sender: TObject; LccSourceNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
    procedure OnTractionManage(Sender: TObject; LccSourceNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
    procedure OnNodeInitializationComplete(Sender: TObject; LccSourceNode: TLccNode);

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

procedure TFormTrainCommander.ButtonTrainsClearClick(Sender: TObject);
begin
  ListViewTrains.Clear;
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
  LccServer.UseSynchronize := CheckBoxUseSyncronize.Checked;
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
  NodeManager.OnLccNodeAliasIDChanged := @OnLccNodeAliasIDChanged;
  NodeManager.OnLccNodeIDChanged := @OnLccNodeIDChanged;
  NodeManager.OnLccMessageReceive := @OnNodeReceiveMessage;
  NodeManager.OnLccMessageSend := @OnNodeSendMessage;
  NodeManager.OnLccNodeProducerIdentify := @OnNodeIdentifyProducers;
  NodeManager.OnLccNodeTractionManage := @OnTractionManage;
  NodeManager.OnLccNodeInitializationComplete := @OnNodeInitializationComplete;

  LccServer.NodeManager := NodeManager;

  FTrainDatabase := TLccTrainDatabase.Create;

  FWorkerMsg := TLccMessage.Create;
end;

procedure TFormTrainCommander.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrainDatabase);
  FreeAndNil(FWorkerMsg);
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

procedure TFormTrainCommander.OnLccNodeAliasIDChanged(Sender: TObject;
  LccSourceNode: TLccNode);
begin
  LabelAliasID.Caption := ( LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TFormTrainCommander.OnLccNodeIDChanged(Sender: TObject;
  LccSourceNode: TLccNode);
begin
  LabelNodeID.Caption := LccSourceNode.NodeIDStr;
end;

procedure TFormTrainCommander.OnNodeReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  if CheckBoxLogMessages.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      MemoLog.Lines.Add('R: ' + LccMessage.ConvertToGridConnectStr('', False));
      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainCommander.OnNodeSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  LccServer.SendMessage(LccMessage);

  if CheckBoxLogMessages.Checked then
  begin
    MemoLog.Lines.BeginUpdate;
    try
      MemoLog.Lines.Add('S: ' + LccMessage.ConvertToGridConnectStr('', False));
      MemoLog.SelStart := Length(MemoLog.Lines.Text);
    finally
      MemoLog.Lines.EndUpdate;
    end;
  end;
end;

procedure TFormTrainCommander.OnTractionManage(Sender: TObject;
  LccSourceNode: TLccNode; LccMessage: TLccMessage; IsReply: Boolean);
begin
  if not IsReply then
  begin
    WorkerMsg.LoadTractionManageReply(LccMessage.DestID, LccMessage.CAN.DestAlias, LccMessage.SourceID, LccMessage.CAN.DestAlias, True);
    NodeManager.SendMessage(WorkerMsg);
  end;
end;

procedure TFormTrainCommander.TimerIncomingMessagePumpTimer(Sender: TObject);
var
  List: TStringList;
  i: Integer;
  GridConnectStr: string;
  LocalMsg: TLccMessage;
begin
  LocalMsg := nil;
  List := LccServer.IncomingGridConnect.LockList;
  try
    if List.Count > 0 then
      LocalMsg := TLccMessage.Create;

    for i := 0 to List.Count - 1 do
    begin
      GridConnectStr := List[i];
      LocalMsg.LoadByGridConnectStr(GridConnectStr);
      NodeManager.ProcessMessage(LocalMsg);
    end;
  finally
    List.Clear;
    LccServer.IncomingGridConnect.UnlockList;
    FreeAndNil(LocalMsg);
  end;
end;

function TFormTrainCommander.CreateTrainNode: TLccCanNode;
begin
  Result := nil;
end;

procedure TFormTrainCommander.OnNodeIdentifyProducers(Sender: TObject;
  LccSourceNode: TLccNode; LccMessage: TLccMessage; var DoDefault: Boolean);
begin

end;

procedure TFormTrainCommander.OnNodeInitializationComplete(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = InitializationWaitNode then
  begin
    InitializationWait := False;
    InitializationWaitNode := nil;
  end;
end;

end.

