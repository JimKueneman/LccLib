unit LccTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.StdCtrls,
  lcc_utilities,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_defines,
  lcc_ethernet_server,
  lcc_ethernet_client,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_protocol_traction_configuration_functions,
  lcc_protocol_traction_configuation_functiondefinitioninfo,
  lcc_protocol_traction_simpletrainnodeinfo,
  lcc_math_float16, FMX.Controls.Presentation, FMX.Objects;

type
  TForm1 = class(TForm)
    ButtonConnect: TButton;
    ButtonLogin: TButton;
    Button3: TButton;
    CheckBoxLocalIP: TCheckBox;
    StatusBar1: TStatusBar;
    TextStatusPanel0: TText;
    TextStatusPanel1: TText;
    TextNodeID: TText;
    Text1: TText;
    Text2: TText;
    TextAliasID: TText;
    Text3: TText;
    TextNodeCount: TText;
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  protected
    EthernetServer: TLccEthernetServer;
    CanNodeManager: TLccCanNodeManager;
    procedure OnEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure SendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    procedure NodeIDChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeAliasChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeDestroyCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeCreateCallback(Sender: TObject; LccSourceNode: TLccNode);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button3Click(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  s := '0xAA';
  i := StrToInt(s);
  s := '$AA';
  i := StrToInt(s);
end;

procedure TForm1.ButtonConnectClick(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetServer.OnConnectionStateChange := OnEthernetConnectionChange;
  EthernetRec.ListenerPort := 12021;
  if EthernetServer.Connected then
  begin
    if CanNodeManager.Nodes.Count > 0 then
      ButtonLoginClick(ButtonLogin);
    CanNodeManager.LogoutAll;
    EthernetServer.CloseConnection(nil);
    ButtonConnect.Text := 'Connect';
    CheckBoxLocalIP.Enabled := True;
  end else
  begin
    if CheckBoxLocalIP.IsChecked then
    begin
      EthernetRec.ListenerIP := '127.0.0.1';
      EthernetRec.AutoResolveIP := False;
    end else
    begin
      EthernetRec.AutoResolveIP := True;
    end;
    EthernetServer.OpenConnection(EthernetRec);
    ButtonConnect.Text := 'Disconnect';
    CheckBoxLocalIP.Enabled := False;
  end;

end;

procedure TForm1.ButtonLoginClick(Sender: TObject);
var
  CanNode: TLccCanNode;
begin
  if CanNodeManager.Nodes.Count = 0 then
  begin
    CanNode := CanNodeManager.AddNode(CDI_XML) as TLccCanNode;

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

    CanNode.ProtocolEventConsumed.AutoGenerate.Count := 5;
    CanNode.ProtocolEventConsumed.AutoGenerate.StartIndex := 0;

    CanNode.ProtocolEventsProduced.AutoGenerate.Count := 5;
    CanNode.ProtocolEventsProduced.AutoGenerate.StartIndex := 0;

    CanNode.Login(NULL_NODE_ID); // Create our own ID

    lcc_defines.Max_Allowed_Buffers := 1; // HACK ALLERT: Allow OpenLCB Python Scripts to run

  end else
    CanNodeManager.Clear;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CanClose;
  CanNodeManager.Free;
  // There is a race of CloseSocket here... called twice in thread and in the CloseConnection call
  EthernetServer.CloseConnection(nil);
  EthernetServer.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EthernetServer := TLccEthernetServer.Create(nil);
  EthernetServer.Gridconnect := True;
  CanNodeManager := TLccCanNodeManager.Create(nil);
  // Setup callbacks
  CanNodeManager.OnLccNodeAliasIDChanged := NodeAliasChangeCallback;
  CanNodeManager.OnLccNodeIDChanged := NodeIDChangeCallback;
  CanNodeManager.OnLccNodeDestroy := NodeDestroyCallback;
  CanNodeManager.OnLccNodeCreate := NodeCreateCallback;
  CanNodeManager.OnLccMessageSend := SendMessage;
  CanNodeManager.OnLccMessageReceive := ReceiveMessage;
  EthernetServer.NodeManager := CanNodeManager;
 // SynEdit1.Clear;

end;

procedure TForm1.NodeAliasChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
begin

end;

procedure TForm1.NodeCreateCallback(Sender: TObject; LccSourceNode: TLccNode);
var
  Count: Integer;
begin
  Count := StrToInt(TextNodeCount.Text);
  Inc( Count);
  TextNodeCount.Text := IntToStr( Count);
  ButtonLogin.Text := 'Log Out';
end;

procedure TForm1.NodeDestroyCallback(Sender: TObject; LccSourceNode: TLccNode);
var
  Count: Integer;
begin
  Count := StrToInt(TextNodeCount.Text);
  Dec( Count);
  TextNodeCount.Text := IntToStr( Count);
  if Count = 0 then
  begin
    TextNodeID.Text := 'None';
    TextAliasID.Text := 'None';
  end;
  ButtonLogin.Text := 'Log In';

end;

procedure TForm1.NodeIDChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
begin
  TextNodeID.Text := LccSourceNode.NodeIDStr;
end;

procedure TForm1.OnEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting:          TextStatusPanel0.Text := 'Server Connecting: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    ccsListenerConnected:           TextStatusPanel0.Text := 'Server Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    ccsListenerDisconnecting:       TextStatusPanel0.Text := 'Server Disconnecting: ';
    ccsListenerDisconnected:        TextStatusPanel0.Text := 'Server Disconnected: ';
    ccsListenerClientConnecting:    TextStatusPanel1.Text := 'Client Connecting: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
    ccsListenerClientConnected:     TextStatusPanel1.Text := 'Client Connected: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
    ccsListenerClientDisconnecting: TextStatusPanel1.Text := 'Client Disconnecting: ';
    ccsListenerClientDisconnected:  TextStatusPanel1.Text := 'Client Disconnected: ';
  end;
end;

procedure TForm1.ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin

end;

procedure TForm1.SendMessage(Sender: TObject; LccMessage: TLccMessage);
begin

end;

end.
