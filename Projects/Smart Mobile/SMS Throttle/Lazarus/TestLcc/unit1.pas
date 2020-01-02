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
  ComCtrls,
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
    Button1: TButton;
    ButtonClear: TButton;
    ButtonConnect: TButton;
    ButtonLogin: TButton;
    ButtonDatagramCount: TButton;
    CheckBoxLocalIP: TCheckBox;
    CheckBoxLogging: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelNodeCount: TLabel;
    LabelNodeID: TLabel;
    Label3: TLabel;
    LabelAliasID: TLabel;
    LabelAllcoatedDatagrams: TLabel;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonLoginClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonDatagramCountClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private

  public
    EthernetServer: TLccEthernetServer;
    CanNodeManager: TLccCanNodeManager;
    procedure OnEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure SendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    procedure NodeIDChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeAliasChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeDestroyCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeCreateCallback(Sender: TObject; LccSourceNode: TLccNode);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonConnectClick(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetServer.OnConnectionStateChange := @OnEthernetConnectionChange;
  EthernetRec.ListenerPort := 12021;
  if EthernetServer.Connected then
  begin
    if CanNodeManager.Nodes.Count > 0 then
      ButtonLoginClick(ButtonLogin);
    CanNodeManager.LogoutAll;
    EthernetServer.CloseConnection(nil);
    ButtonConnect.Caption := 'Connect';
    CheckBoxLocalIP.Enabled := True;
  end else
  begin
    if CheckBoxLocalIP.Checked then
    begin
      EthernetRec.ListenerIP := '127.0.0.1';
      EthernetRec.AutoResolveIP := False;
    end else
    begin
      EthernetRec.AutoResolveIP := True;
    end;
    EthernetServer.OpenConnection(EthernetRec);
    ButtonConnect.Caption := 'Disconnect';
    CheckBoxLocalIP.Enabled := False;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: single;
  Float16: THalfFloat;
begin
  s := StrToFloat(Edit1.Text);
  Float16 := FloatToHalf(s);
  Label4.Caption := '0x' + IntToHex(Float16, 8);
  s := HalfToFloat(Float16);
  Label5.Caption := FloatToStr(s);
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

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  SynEdit1.Clear;
end;

type
  TLccCanNodeHack = class(TLccCanNode);

procedure TForm1.ButtonDatagramCountClick(Sender: TObject);
var
  Msg: TLccMessage;
begin
  if CanNodeManager.Nodes.Count > 0 then
  begin
    if TLccCanNodeHack( CanNodeManager.Nodes[0]).InProcessMultiFrameMessage.Count > 0 then
    begin
      Msg := TLccMessage(TLccCanNodeHack( CanNodeManager.Nodes[0]).InProcessMultiFrameMessage[0])
    end;
  end;
  LabelAllcoatedDatagrams.Caption := 'Allocated Buffers: ' + IntToStr(InprocessMessageAllocated);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
  CanNodeManager.OnLccNodeAliasIDChanged := @NodeAliasChangeCallback;
  CanNodeManager.OnLccNodeIDChanged := @NodeIDChangeCallback;
  CanNodeManager.OnLccNodeDestroy := @NodeDestroyCallback;
  CanNodeManager.OnLccNodeCreate := @NodeCreateCallback;
  CanNodeManager.OnLccMessageSend := @SendMessage;
  CanNodeManager.OnLccMessageReceive := @ReceiveMessage;
  EthernetServer.NodeManager := CanNodeManager;
  SynEdit1.Clear;
end;

procedure TForm1.NodeAliasChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelAliasID.Caption := (LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TForm1.NodeCreateCallback(Sender: TObject; LccSourceNode: TLccNode);
var
  Count: Integer;
begin
  Count := StrToInt(LabelNodeCount.Caption);
  Inc( Count);
  LabelNodeCount.Caption := IntToStr( Count);
  ButtonLogin.Caption := 'Log Out';
end;

procedure TForm1.NodeDestroyCallback(Sender: TObject; LccSourceNode: TLccNode);
var
  Count: Integer;
begin
  Count := StrToInt(LabelNodeCount.Caption);
  Dec( Count);
  LabelNodeCount.Caption := IntToStr( Count);
  if Count = 0 then
  begin
    LabelNodeID.Caption := 'None';
    LabelAliasID.Caption := 'None';
  end;
  ButtonLogin.Caption := 'Log In';
end;

procedure TForm1.NodeIDChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelNodeID.Caption := LccSourceNode.NodeIDStr;
end;

procedure TForm1.OnEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting:          StatusBar1.Panels[0].Text := 'Server Connecting: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    ccsListenerConnected:           StatusBar1.Panels[0].Text := 'Server Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    ccsListenerDisconnecting:       StatusBar1.Panels[0].Text := 'Server Disconnecting: ';
    ccsListenerDisconnected:        StatusBar1.Panels[0].Text := 'Server Disconnected: ';
    ccsListenerClientConnecting:    StatusBar1.Panels[1].Text := 'Client Connecting: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
    ccsListenerClientConnected:     StatusBar1.Panels[1].Text := 'Client Connected: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
    ccsListenerClientDisconnecting: StatusBar1.Panels[1].Text := 'Client Disconnecting: ';
    ccsListenerClientDisconnected:  StatusBar1.Panels[1].Text := 'Client Disconnected: ';
  end;
end;

procedure TForm1.ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  if CheckBoxLogging.Checked then
  begin
    SynEdit1.BeginUpdate(False);
    try
      SynEdit1.Lines.Add('R: ' + GridConnectToDetailedGridConnect(LccMessage.ConvertToGridConnectStr(#13)));
      SynEdit1.EndUpdate;
    finally
    end;
  end;
end;

procedure TForm1.SendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  EthernetServer.SendMessage(LccMessage);

  if CheckBoxLogging.Checked then
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

