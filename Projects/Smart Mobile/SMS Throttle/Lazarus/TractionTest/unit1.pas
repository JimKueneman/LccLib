unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ComCtrls, ExtCtrls, Arrow,
  SynEdit,
  lcc_utilities,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_defines,
  lcc_ethernet_server,
  lcc_ethernet_client,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_protocol_traction,
  lcc_protocol_traction_configuration_functions,
  lcc_protocol_traction_configuation_functiondefinitioninfo,
  lcc_protocol_traction_simpletrainnodeinfo,
  lcc_math_float16;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonThrottleF0: TButton;
    ButtonThrottleF1: TButton;
    ButtonThrottleF7: TButton;
    ButtonThrottleF8: TButton;
    ButtonThrottleF9: TButton;
    ButtonThrottleF2: TButton;
    ButtonThrottleF3: TButton;
    ButtonThrottleF4: TButton;
    ButtonThrottleF5: TButton;
    ButtonThrottleF6: TButton;
    ButtonThrottleF11: TButton;
    ButtonThrottleF12: TButton;
    ButtonThrottleF10: TButton;
    ButtonThrottleAssignAddress: TButton;
    ButtonHubToolsClear: TButton;
    ButtonHubConnectAndLogin: TButton;
    ButtonTrainConnectAndLogin: TButton;
    ButtonThrottleConnectAndLogin: TButton;
    CheckBoxThrottleLongAddress: TCheckBox;
    CheckBoxHubToolsEnableLog: TCheckBox;
    CheckBoxHubLocalIP: TCheckBox;
    CheckBoxTrainLocalIP: TCheckBox;
    CheckBoxThrottleLocalIP: TCheckBox;
    ColorButtonTrainF0: TColorButton;
    ColorButtonTrainF1: TColorButton;
    ColorButtonTrainF10: TColorButton;
    ColorButtonTrainF11: TColorButton;
    ColorButtonTrainF12: TColorButton;
    ColorButtonTrainF2: TColorButton;
    ColorButtonTrainF3: TColorButton;
    ColorButtonTrainF4: TColorButton;
    ColorButtonTrainF5: TColorButton;
    ColorButtonTrainF6: TColorButton;
    ColorButtonTrainF7: TColorButton;
    ColorButtonTrainF8: TColorButton;
    ColorButtonTrainF9: TColorButton;
    Label1: TLabel;
    LabeledEditThrottleAddress: TLabeledEdit;
    LabelHubClientsConnectedValue: TLabel;
    LabelHubClientsConnected: TLabel;
    LabelHubAliasID: TLabel;
    LabelHubIPAddress: TLabel;
    LabelHubNodeID: TLabel;
    LabelTrainIPAddress: TLabel;
    LabelThrottleIPAddress: TLabel;
    LabelThrottleAliasID: TLabel;
    LabelTrainNodeID: TLabel;
    LabelTrainAliasID: TLabel;
    LabelThrottleNodeID: TLabel;
    PanelThrottleControls: TPanel;
    PanelThrottleBackground: TPanel;
    PanelTrainBackground: TPanel;
    PanelHubTools: TPanel;
    PanelHubTitle: TPanel;
    PanelThrottleTitle: TPanel;
    PanelTrainTitle: TPanel;
    PanelThrottleHeader: TPanel;
    PanelThrottleHeader1: TPanel;
    PanelHubNode: TPanel;
    PanelTrainNode: TPanel;
    PanelTrainHeader: TPanel;
    PanelThrottleNode: TPanel;
    RadioGroup1: TRadioGroup;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    ToggleBoxThrottleForward: TToggleBox;
    ToggleBoxThrottleReverse: TToggleBox;
    TrackBarThrottleSpeed: TTrackBar;
    procedure ButtonHubConnectAndLoginClick(Sender: TObject);
    procedure ButtonHubToolsClearClick(Sender: TObject);
    procedure ButtonThrottleConnectAndLoginClick(Sender: TObject);
    procedure ButtonTrainConnectAndLoginClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
  protected
  public
    HubEthernetServer: TLccEthernetServer;
    HubNodeManager: TLccCanNodeManager;
    TrainEthernetClient: TLccEthernetClient;
    TrainNodeManager: TLccCanNodeManager;
    ThrottleEthernetClient: TLccEthernetClient;
    ThrottleNodeManager: TLccCanNodeManager;

    // Hub
    procedure OnHubEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnHubEthernetErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnHubNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnHubNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure HubSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure HubReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    // Train
    procedure OnTrainEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnTrainEthernetErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnTrainNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnTrainNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure TrainSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure TrainReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    // Throttle
    procedure OnThrottleEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnThrottleEthernetErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnThrottleNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnThrottleNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure ThrottleSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure ThrottleReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonHubConnectAndLoginClick(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.ListenerPort := 12021;
  if HubEthernetServer.Connected then
  begin
    HubNodeManager.LogoutAll;
    HubEthernetServer.CloseConnection(nil);
    ButtonHubConnectAndLogin.Caption := 'Connect and Login';
    CheckBoxHubLocalIP.Enabled := True;
  end else
  begin
    if CheckBoxHubLocalIP.Checked then
    begin
      EthernetRec.ListenerIP := '127.0.0.1';
      EthernetRec.AutoResolveIP := False;
    end else
    begin
      EthernetRec.AutoResolveIP := True;
    end;
    HubEthernetServer.OpenConnection(EthernetRec);
    ButtonHubConnectAndLogin.Caption := 'Disconnect';
    CheckBoxHubLocalIP.Enabled := False;
  end;
end;

procedure TForm1.ButtonHubToolsClearClick(Sender: TObject);
begin
  SynEdit1.Clear;
end;

procedure TForm1.ButtonThrottleConnectAndLoginClick(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.ListenerPort := 12021;
  if ThrottleEthernetClient.Connected then
  begin
    ThrottleNodeManager.LogoutAll;
    ThrottleEthernetClient.CloseConnection(nil);
    ButtonThrottleConnectAndLogin.Caption := 'Connect and Login';
    CheckBoxThrottleLocalIP.Enabled := True;
  end else
  begin
    if CheckBoxThrottleLocalIP.Checked then
    begin
      EthernetRec.ListenerIP := '127.0.0.1';
      EthernetRec.AutoResolveIP := False;
    end else
    begin
      EthernetRec.AutoResolveIP := True;
    end;
    ThrottleEthernetClient.OpenConnection(EthernetRec);
    ButtonThrottleConnectAndLogin.Caption := 'Disconnect';
    CheckBoxThrottleLocalIP.Enabled := False;
  end;
end;

procedure TForm1.ButtonTrainConnectAndLoginClick(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.ListenerPort := 12021;
  if TrainEthernetClient.Connected then
  begin
    TrainNodeManager.LogoutAll;
    TrainEthernetClient.CloseConnection(nil);
    ButtonTrainConnectAndLogin.Caption := 'Connect and Login';
    CheckBoxTrainLocalIP.Enabled := True;
  end else
  begin
    if CheckBoxTrainLocalIP.Checked then
    begin
      EthernetRec.ListenerIP := '127.0.0.1';
      EthernetRec.AutoResolveIP := False;
    end else
    begin
      EthernetRec.AutoResolveIP := True;
    end;
    TrainEthernetClient.OpenConnection(EthernetRec);
    ButtonTrainConnectAndLogin.Caption := 'Disconnect';
    CheckBoxTrainLocalIP.Enabled := False;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose;
  TrainEthernetClient.CloseConnection(nil);
  ThrottleEthernetClient.CloseConnection(nil);


  HubNodeManager.Free;
  // There is a race of CloseSocket here... called twice in thread and in the CloseConnection call
  HubEthernetServer.CloseConnection(nil);
  HubEthernetServer.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Hub node
  HubNodeManager := TLccCanNodeManager.Create(nil);
  HubNodeManager.OnLccNodeAliasIDChanged := @OnHubNodeAliasChange;
  HubNodeManager.OnLccNodeIDChanged := @OnHubNodeIDChange;
  HubNodeManager.OnLccMessageSend := @HubSendMessage;
  HubNodeManager.OnLccMessageReceive := @HubReceiveMessage;

  HubEthernetServer := TLccEthernetServer.Create(nil);
  HubEthernetServer.Gridconnect := True;
  HubEthernetServer.Hub := True;
  HubEthernetServer.NodeManager := HubNodeManager;
  HubEthernetServer.OnConnectionStateChange := @OnHubEthernetConnectionChange;
  HubEthernetServer.OnErrorMessage := @OnHubEthernetErrorMessage;

  // train node
  TrainNodeManager := TLccCanNodeManager.Create(nil);
  TrainNodeManager.OnLccNodeAliasIDChanged := @OnTrainNodeAliasChange;
  TrainNodeManager.OnLccNodeIDChanged := @OnTrainNodeIDChange;
  TrainNodeManager.OnLccMessageSend := @TrainSendMessage;
  TrainNodeManager.OnLccMessageReceive := @TrainReceiveMessage;

  TrainEthernetClient := TLccEthernetClient.Create(nil);
  TrainEthernetClient.Gridconnect := True;
  TrainEthernetClient.NodeManager := TrainNodeManager;
  TrainEthernetClient.OnConnectionStateChange := @OnTrainEthernetConnectionChange;
  TrainEthernetClient.OnErrorMessage := @OnTrainEthernetErrorMessage;

  // throttle node
  ThrottleNodeManager := TLccCanNodeManager.Create(nil);
  ThrottleNodeManager.OnLccNodeAliasIDChanged := @OnThrottleNodeAliasChange;
  ThrottleNodeManager.OnLccNodeIDChanged := @OnThrottleNodeIDChange;
  ThrottleNodeManager.OnLccMessageSend := @ThrottleSendMessage;
  ThrottleNodeManager.OnLccMessageReceive := @ThrottleReceiveMessage;

  ThrottleEthernetClient := TLccEthernetClient.Create(nil);
  ThrottleEthernetClient.Gridconnect := True;
  ThrottleEthernetClient.NodeManager := ThrottleNodeManager;
  ThrottleEthernetClient.OnConnectionStateChange := @OnThrottleEthernetConnectionChange;
  ThrottleEthernetClient.OnErrorMessage := @OnThrottleEthernetErrorMessage;

  SynEdit1.Clear;
end;

procedure TForm1.OnHubNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelHubAliasID.Caption := (LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TForm1.OnHubNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelHubNodeID.Caption := LccSourceNode.NodeIDStr;
end;

procedure TForm1.OnHubEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  CanNode: TLccCanNode;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting:          LabelHubIPAddress.Caption := 'Server Connecting';
    ccsListenerConnected:
      begin
        LabelHubIPAddress.Caption := 'Server Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);

        CanNode := HubNodeManager.AddNode(CDI_XML) as TLccCanNode;

        CanNode.ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
        CanNode.ProtocolSupportedProtocols.Datagram := True;
        CanNode.ProtocolSupportedProtocols.EventExchange := True;
        CanNode.ProtocolSupportedProtocols.SimpleNodeInfo := True;
        CanNode.ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;

        CanNode.ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);

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
        CanNode.ProtocolMemoryOptions.LowSpace := MSI_ACDI_USER;

    //    CanNode.ProtocolEventConsumed.AutoGenerate.Count := 5;
    //    CanNode.ProtocolEventConsumed.AutoGenerate.StartIndex := 0;

    //    CanNode.ProtocolEventsProduced.AutoGenerate.Count := 5;
    //    CanNode.ProtocolEventsProduced.AutoGenerate.StartIndex := 0;

        CanNode.Login(NULL_NODE_ID); // Create our own ID
      end;
    ccsListenerDisconnecting:
      begin
        HubNodeManager.Clear;
        LabelHubIPAddress.Caption := 'Server Disconnecting';
      end;
    ccsListenerDisconnected:        LabelHubIPAddress.Caption := 'Server Disconnected: ';
    ccsListenerClientConnecting:    begin end;
    ccsListenerClientConnected:     LabelHubClientsConnectedValue.Caption := IntToStr( StrToInt(LabelHubClientsConnectedValue.Caption) + 1);
    ccsListenerClientDisconnecting: begin end;
    ccsListenerClientDisconnected:  LabelHubClientsConnectedValue.Caption := IntToStr( StrToInt(LabelHubClientsConnectedValue.Caption) - 1);
  end;
end;

procedure TForm1.OnHubEthernetErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage(EthernetRec.MessageStr);
  LabelHubIPAddress.Caption := 'Server Disconnected';
  ButtonHubConnectAndLogin.Caption := 'Connect and Login';
end;

procedure TForm1.OnTrainEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  CanNode: TLccCanNode;
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting : LabelTrainIPAddress.Caption    := 'IP Address: Connecting';
    ccsClientConnected  :
      begin
        LabelTrainIPAddress.Caption    := 'IP Address: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);

        CanNode := TrainNodeManager.AddNode(CDI_XML) as TLccCanNode;

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

     //   CanNode.ProtocolEventConsumed.AutoGenerate.Count := 5;
     //   CanNode.ProtocolEventConsumed.AutoGenerate.StartIndex := 0;

    //    CanNode.ProtocolEventsProduced.AutoGenerate.Count := 5;
    //    CanNode.ProtocolEventsProduced.AutoGenerate.StartIndex := 0;

        CanNode.Login(NULL_NODE_ID); // Create our own ID
      end;
    ccsClientDisconnecting :
      begin
        TrainNodeManager.Clear;   // Logout
        LabelTrainIPAddress.Caption := 'IP Address: Disconnecting';
      end;
    ccsClientDisconnected : LabelTrainIPAddress.Caption  := 'IP Address: Disconnected';
  end;
end;

procedure TForm1.OnTrainEthernetErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage(EthernetRec.MessageStr);
  LabelTrainIPAddress.Caption := 'IP Address Disconnected';
  ButtonTrainConnectAndLogin.Caption := 'Connect and Login';
end;

procedure TForm1.OnTrainNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelTrainNodeID.Caption := 'NodeID: ' + LccSourceNode.NodeIDStr;
end;

procedure TForm1.OnTrainNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelTrainAliasID.Caption := 'AliasID: ' + (LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TForm1.TrainSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  TrainEthernetClient.SendMessage(LccMessage);
end;

procedure TForm1.TrainReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin

end;

procedure TForm1.OnThrottleEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  CanNode: TLccCanNode;
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting : LabelThrottleIPAddress.Caption    := 'IP Address: Connecting';
    ccsClientConnected  :
      begin
        LabelThrottleIPAddress.Caption    := 'IP Address: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);

        CanNode := ThrottleNodeManager.AddNode(CDI_XML) as TLccCanNode;

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

        CanNode.Login(NULL_NODE_ID); // Create our own ID
      end;
    ccsClientDisconnecting :
      begin
        ThrottleNodeManager.Clear;   // Logout
        LabelThrottleIPAddress.Caption := 'IP Address: Disconnecting';
      end;
    ccsClientDisconnected : LabelThrottleIPAddress.Caption  := 'IP Address: Disconnected';
  end;
end;

procedure TForm1.OnThrottleEthernetErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage(EthernetRec.MessageStr);
  LabelThrottleIPAddress.Caption := 'IP Address Disconnected';
  ButtonThrottleConnectAndLogin.Caption := 'Connect and Login';
end;

procedure TForm1.OnThrottleNodeIDChange(Sender: TObject; LccSourceNode: TLccNode );
begin
  LabelThrottleNodeID.Caption := 'NodeID: ' + LccSourceNode.NodeIDStr;
end;

procedure TForm1.OnThrottleNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelThrottleAliasID.Caption := 'AliasID: ' + (LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TForm1.ThrottleSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  ThrottleEthernetClient.SendMessage(LccMessage);
end;

procedure TForm1.ThrottleReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin

end;

procedure TForm1.HubReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  if CheckBoxHubToolsEnableLog.Checked then
  begin
    SynEdit1.BeginUpdate(False);
    try
      SynEdit1.Lines.Add('R: ' + GridConnectToDetailedGridConnect(LccMessage.ConvertToGridConnectStr(#13)));
      SynEdit1.EndUpdate;
    finally
    end;
  end;
end;

procedure TForm1.HubSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  HubEthernetServer.SendMessage(LccMessage);

  if CheckBoxHubToolsEnableLog.Checked then
  begin
    SynEdit1.BeginUpdate(False);
    try
      SynEdit1.Lines.Add('S: ' + GridConnectToDetailedGridConnect(LccMessage.ConvertToGridConnectStr(#13)));
      SynEdit1.EndUpdate;
    finally
    end;
  end;
end;

end.

