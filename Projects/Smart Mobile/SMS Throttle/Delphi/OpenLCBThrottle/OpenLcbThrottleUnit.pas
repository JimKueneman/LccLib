unit OpenLcbThrottleUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.ScrollBox, FMX.StdCtrls, FMX.Gestures, FMX.Controls.Presentation,
  FMX.Edit, FMX.Objects, FMX.ListBox, FMX.Layouts,
  FMX.Memo, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Colors, FMX.ListView, FMX.MultiView, FMX.Ani,
  FMX.Effects, FMX.Filter.Effects, System.Math.Vectors, FMX.Controls3D,
  FMX.Layers3D,
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
  TOpenLcbSettings = class(TPersistent)
  private
    FMaxLoggingLines: Integer;
    FIpServerAddress: string;
    FPort: Word;
    FGridConnect: Boolean;
    FLog: Boolean;
  published
    constructor Create;
    property MaxLoggingLines: Integer read FMaxLoggingLines write FMaxLoggingLines;
    property IpServerAddress: string read FIpServerAddress write FIpServerAddress;
    property Port: Word read FPort write FPort;
    property GridConnect: Boolean read FGridConnect write FGridConnect;
    property Log: Boolean read FLog write FLog;
  end;

type
  TOpenLcbThrottleForm = class(TForm)
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControlMain: TTabControl;
    TabItemTrains: TTabItem;
    TabItemOpenLCB: TTabItem;
    TabItemSettings: TTabItem;
    GestureManager1: TGestureManager;
    ListBox1: TListBox;
    ListBoxSettingsItemServer: TListBoxItem;
    Text1: TText;
    EditSettingsServerIP: TEdit;
    ListBoxItemSettingServerPort: TListBoxItem;
    TextSettingsServerPort: TText;
    EditSettingsServerPort: TEdit;
    ListBoxGroupHeaderSettingsNetwork: TListBoxGroupHeader;
    ListBoxGroupHeaderSettingsOLCB: TListBoxGroupHeader;
    ListBoxItemSettingsOLCBMonitorDepth: TListBoxItem;
    Text2: TText;
    EditSettingsBufferDepth: TEdit;
    ListBoxItemDataFormat: TListBoxItem;
    TextSettingsOCLBDataFormat: TText;
    SwitchSettingsDataFormatGridConnect: TSwitch;
    SpeedButtonOpenLCBClear: TSpeedButton;
    SwitchOpenLCBLog: TSwitch;
    Text3: TText;
    MemoOpenLCB: TMemo;
    MultiViewTrains: TMultiView;
    ListView1: TListView;
    ColorKeyAlphaEffect1: TColorKeyAlphaEffect;
    GridLayout1: TGridLayout;
    SpeedButtonMore: TSpeedButton;
    LayoutMainTrain: TLayout;
    GridLayoutFunctions: TGridLayout;
    CornerButtonF0: TCornerButton;
    CornerButtonF1: TCornerButton;
    CornerButtonF2: TCornerButton;
    CornerButtonF3: TCornerButton;
    CornerButtonF4: TCornerButton;
    CornerButtonF5: TCornerButton;
    CornerButtonF6: TCornerButton;
    CornerButtonF7: TCornerButton;
    CornerButtonF8: TCornerButton;
    CornerButtonF9: TCornerButton;
    CornerButton10: TCornerButton;
    CornerButton11: TCornerButton;
    LayoutThrottle: TLayout;
    TextSpeed: TText;
    Text4: TText;
    ScrollBarThrottle: TScrollBar;
    CornerButtonForward: TCornerButton;
    TextDirection: TText;
    CornerButtonReverse: TCornerButton;
    LayoutDirection: TLayout;
    RectangleTabTrainBkGnd: TRectangle;
    LayoutTrainsMultiViewToolbar: TLayout;
    SpeedButton1: TSpeedButton;
    Rectangle2: TRectangle;
    SpeedButtonTrainSearch: TSpeedButton;
    LayoutTrainsMultiViewSearch: TLayout;
    EditTrainSearch: TEdit;
    FloatAnimationTrainSearch: TFloatAnimation;
    RectangleTabSettingsBkGnd: TRectangle;
    RectangleTabOpenLcbBkGnd: TRectangle;
    ListBoxGroupHeaderStatus: TListBoxGroupHeader;
    ListBoxItemStatusIP: TListBoxItem;
    ListBoxItemStatusNodes: TListBoxItem;
    Text5: TText;
    TextStatusIPAddress: TText;
    Text6: TText;
    TextStatusOpenLcbNode: TText;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure ScrollBarThrottleChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CornerButtonForwardClick(Sender: TObject);
    procedure CornerButtonReverseClick(Sender: TObject);
    procedure SpeedButtonTrainSearchClick(Sender: TObject);
    procedure MultiViewTrainsStartShowing(Sender: TObject);
    procedure GridLayoutFunctionsResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditSettingsServerPortKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsServerIPKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsBufferDepthKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsServerIPExit(Sender: TObject);
    procedure EditSettingsServerPortExit(Sender: TObject);
    procedure EditSettingsBufferDepthExit(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SwitchOpenLCBLogSwitch(Sender: TObject);
    procedure SpeedButtonOpenLCBClearClick(Sender: TObject);
  private
    FCanNodeManager: TLccCanNodeManager;
    FEthernetServer: TLccEthernetServer;
    FEthernetClient: TLccEthernetClient;
    FOpenLcbSettings: TOpenLcbSettings;
    FNodeID: string;
    FNodeAlias: string;
    { Private declarations }
  protected
    procedure OnConnectionStateChangeClient(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnConnectionStateChangeServer(Sender: TObject; EthernetRec: TLccEthernetRec);

    procedure NodeIDChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeAliasChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeDestroyCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeCreateCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeInitializationCompleteCallback(Sender: TObject; LccSourceNode: TLccNode);
    procedure NodeSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure NodeReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

  public
    { Public declarations }
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property EthernetServer: TLccEthernetServer read FEthernetServer write FEthernetServer;
    property CanNodeManager: TLccCanNodeManager read FCanNodeManager write FCanNodeManager;

    property OpenLcbSettings: TOpenLcbSettings read FOpenLcbSettings write FOpenLcbSettings;

    property NodeID: string read FNodeID write FNodeID;
    property NodeAlias: string read FNodeAlias write FNodeAlias;
  end;

var
  OpenLcbThrottleForm: TOpenLcbThrottleForm;

implementation

{$R *.fmx}

{ TOpenLcbSettings }

constructor TOpenLcbSettings.Create;
begin
  inherited;
  FMaxLoggingLines := 100;
//  FIpServerAddress := '127.0.0.1';
  FIpServerAddress := '10.0.3.154';
  FPort := 12021;
  FGridConnect := True;
end;

procedure TOpenLcbThrottleForm.Button1Click(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  if not EthernetClient.Connected then
  begin
    FillChar(EthernetRec, SizeOf(EthernetRec), #0);
    EthernetRec.AutoResolveIP := True;
    EthernetRec.ListenerIP := OpenLcbSettings.IpServerAddress;
    EthernetRec.ListenerPort := OpenLcbSettings.Port;
    EthernetClient.OpenConnection(EthernetRec);
    EthernetClient.Gridconnect := OpenLcbSettings.GridConnect;
  end;
end;

procedure TOpenLcbThrottleForm.Button2Click(Sender: TObject);
begin
  if EthernetClient.Connected then
  begin
    EthernetClient.CloseConnection(nil);
  end;
end;

procedure TOpenLcbThrottleForm.Button3Click(Sender: TObject);
var
  CanNode: TLccCanNode;
begin
  lcc_defines.Max_Allowed_Buffers := 1; // HACK ALLERT: Allow OpenLCB Python Scripts to run

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
  end
end;

procedure TOpenLcbThrottleForm.Button4Click(Sender: TObject);
begin
  CanNodeManager.Clear;
end;

procedure TOpenLcbThrottleForm.CornerButtonForwardClick(Sender: TObject);
begin
  TextDirection.Text := 'Forward'
end;

procedure TOpenLcbThrottleForm.CornerButtonReverseClick(Sender: TObject);
begin
  TextDirection.Text := 'Reverse';
end;

procedure TOpenLcbThrottleForm.EditSettingsServerIPExit(Sender: TObject);
begin
  OpenLcbSettings.IpServerAddress := EditSettingsServerIP.Text;
end;

procedure TOpenLcbThrottleForm.EditSettingsServerIPKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then   // Return
  begin
    OpenLcbSettings.IpServerAddress := EditSettingsServerIP.Text;
  end else
  begin
    if ((KeyChar < '0') or (KeyChar > '9')) and not(KeyChar = '.') and not(Key = 8) then // Backspace
    begin
      Key := 0;
      KeyChar := #0
    end;
  end;
end;

procedure TOpenLcbThrottleForm.EditSettingsBufferDepthExit(Sender: TObject);
begin
  OpenLcbSettings.MaxLoggingLines := StrToInt(EditSettingsBufferDepth.Text);
end;

procedure TOpenLcbThrottleForm.EditSettingsBufferDepthKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then   // Return
  begin
    OpenLcbSettings.MaxLoggingLines := StrToInt(EditSettingsBufferDepth.Text);
  end else
  begin
    if ((KeyChar < '0') or (KeyChar > '9')) and not(Key = 8)  then    // Backspace
    begin
      Key := 0;
      KeyChar := #0
    end;
  end;
end;

procedure TOpenLcbThrottleForm.EditSettingsServerPortExit(Sender: TObject);
begin
  OpenLcbSettings.Port := StrToInt(EditSettingsServerPort.Text);
end;

procedure TOpenLcbThrottleForm.EditSettingsServerPortKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then  // Return
  begin
    OpenLcbSettings.Port := StrToInt(EditSettingsServerPort.Text);
  end else
  begin
    if ((KeyChar < '0') or (KeyChar > '9')) and not(Key = 8) then    // Backspace
    begin
      Key := 0;
      KeyChar := #0
    end;
  end;
end;

procedure TOpenLcbThrottleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(FOpenLcbSettings);
end;

procedure TOpenLcbThrottleForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  EthernetClient.CloseConnection(nil);
  FreeAndNil(FEthernetClient);
  EthernetServer.CloseConnection(nil);
  FreeAndNil(FEthernetServer);
  CanNodeManager.Clear;
  FreeAndNil(FCanNodeManager);
end;

procedure TOpenLcbThrottleForm.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControlMain.ActiveTab := TabItemTrains;

  FOpenLcbSettings := TOpenLcbSettings.Create;

  FEthernetClient := TLccEthernetClient.Create(nil);
  FEthernetServer := TLccEthernetServer.Create(nil);
  EthernetClient.OnConnectionStateChange := OnConnectionStateChangeClient;
  EthernetServer.OnConnectionStateChange := OnConnectionStateChangeServer;
  EthernetServer.Gridconnect := True;
  EthernetClient.Gridconnect := True;

  FCanNodeManager := TLccCanNodeManager.Create(nil);
  CanNodeManager.OnLccNodeAliasIDChanged := NodeAliasChangeCallback;
  CanNodeManager.OnLccNodeIDChanged := NodeIDChangeCallback;
  CanNodeManager.OnLccNodeDestroy := NodeDestroyCallback;
  CanNodeManager.OnLccNodeCreate := NodeCreateCallback;
  CanNodeManager.OnLccNodeInitializationComplete := NodeInitializationCompleteCallback;

  CanNodeManager.OnLccMessageSend := NodeSendMessage;
  CanNodeManager.OnLccMessageReceive := NodeReceiveMessage;

  EthernetServer.NodeManager := CanNodeManager;
  EthernetClient.NodeManager := CanNodeManager;
end;

procedure TOpenLcbThrottleForm.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if TabControlMain.ActiveTab <> TabControlMain.Tabs[TabControlMain.TabCount-1] then
        TabControlMain.ActiveTab := TabControlMain.Tabs[TabControlMain.TabIndex+1];
      Handled := True;
    end;

    sgiRight:
    begin
      if TabControlMain.ActiveTab <> TabControlMain.Tabs[0] then
        TabControlMain.ActiveTab := TabControlMain.Tabs[TabControlMain.TabIndex-1];
      Handled := True;
    end;
  end;
{$ENDIF}
end;

procedure TOpenLcbThrottleForm.FormShow(Sender: TObject);
begin
  MultiViewTrains.HideMaster;
  SwitchSettingsDataFormatGridConnect.IsChecked := OpenLcbSettings.GridConnect;
  SwitchOpenLCBLog.IsChecked := OpenLcbSettings.Log;
  EditSettingsServerIP.Text := OpenLcbSettings.IpServerAddress;
  EditSettingsServerPort.Text := IntToStr(OpenLcbSettings.Port);
  EditSettingsBufferDepth.Text := IntToStr(OpenLcbSettings.MaxLoggingLines);
end;

procedure TOpenLcbThrottleForm.GridLayoutFunctionsResize(Sender: TObject);
{
  procedure CalculateLayout(Columns, GridClientW, GridClientH: single);
  var
    Rows: Integer;
  begin
    GridLayoutFunctions.ItemWidth := GridClientW/Columns;
    GridLayoutFunctions.ItemHeight := GridLayoutFunctions.ItemWidth;
  end;

var
  Columns, Rows: Integer;
  GridClientW, GridClientH: single;     }
begin
{  Columns := 1;
  GridClientW := GridLayoutFunctions.Width - (GridLayoutFunctions.Padding.Left+GridLayoutFunctions.Padding.Right);
  GridClientH := GridLayoutFunctions.Height - (GridLayoutFunctions.Padding.Top+GridLayoutFunctions.Padding.Bottom);

  repeat
    CalculateLayout(Columns, GridClientW, GridClientH);
    Rows := GridLayoutFunctions.ChildrenCount div Columns + 1;
    Inc(Columns);
  until GridLayoutFunctions.ItemHeight * Rows <= GridClientH;
     }
end;

procedure TOpenLcbThrottleForm.MultiViewTrainsStartShowing(Sender: TObject);
begin
  MultiViewTrains.Width := Width * 0.75
end;

procedure TOpenLcbThrottleForm.NodeAliasChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
begin
  FNodeAlias := (LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TOpenLcbThrottleForm.NodeCreateCallback(Sender: TObject; LccSourceNode: TLccNode);
begin
  TextStatusOpenLcbNode.Text := 'Creating Node';
  Button3.Enabled := False;
  Button4.Enabled := False;
end;

procedure TOpenLcbThrottleForm.NodeDestroyCallback(Sender: TObject; LccSourceNode: TLccNode);
begin
  TextStatusOpenLcbNode.Text := 'Not Connected';
  Button3.Enabled := EthernetClient.Connected;
  Button4.Enabled := False;
  NodeID := '';
  NodeAlias := '';
end;

procedure TOpenLcbThrottleForm.NodeIDChangeCallback(Sender: TObject; LccSourceNode: TLccNode);
begin
  FNodeID := (LccSourceNode as TLccCanNode).NodeIDStr;
end;

procedure TOpenLcbThrottleForm.NodeInitializationCompleteCallback(
  Sender: TObject; LccSourceNode: TLccNode);
begin
  Button3.Enabled := False;
  Button4.Enabled := True;
  TextStatusOpenLcbNode.Text := 'ID: ' + NodeID + ' Alias: ' + NodeAlias;
end;

procedure TOpenLcbThrottleForm.NodeReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  if OpenLcbSettings.Log then
  begin
    MemoOpenLCB.BeginUpdate;
    try
      while MemoOpenLCB.Lines.Count >= OpenLcbSettings.MaxLoggingLines do
        MemoOpenLCB.Lines.Delete(0);
      MemoOpenLCB.Lines.Add('R: ' + LccMessage.ConvertToGridConnectStr('', False));
    finally
      MemoOpenLCB.EndUpdate
    end;
  end;
end;

procedure TOpenLcbThrottleForm.NodeSendMessage(Sender: TObject;LccMessage: TLccMessage);
begin
  if EthernetClient.Connected then
    EthernetClient.SendMessage(LccMessage);
  if EthernetServer.Connected then
    EthernetServer.SendMessage(LccMessage);

  if OpenLcbSettings.Log then
  begin
    MemoOpenLCB.BeginUpdate;
    try
      while MemoOpenLCB.Lines.Count >= OpenLcbSettings.MaxLoggingLines do
        MemoOpenLCB.Lines.Delete(0);
      MemoOpenLCB.Lines.Add('S: ' + LccMessage.ConvertToGridConnectStr('', False));
    finally
      MemoOpenLCB.EndUpdate
    end;
  end;
end;

procedure TOpenLcbThrottleForm.OnConnectionStateChangeClient(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
   case EthernetRec.ConnectionState of
      ccsClientConnecting    :  TextStatusIPAddress.Text := 'Connecting';
      ccsClientConnected     :  begin
                                  TextStatusIPAddress.Text := EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
                                  Button2.Enabled := True;
                                  Button1.Enabled := False;
                                  Button3.Enabled := True;
                                end;
      ccsClientDisconnecting :  begin
                                  TextStatusIPAddress.Text := 'Disconnecting';
                                end;
      ccsClientDisconnected  :  begin
                                  TextStatusIPAddress.Text := 'Not Connected';
                                  Button1.Enabled := True;
                                  Button2.Enabled := False;
                                  Button3.Enabled := False;
                                  Button4.Enabled := False;
                                  CanNodeManager.Clear;
                                end;
   end;
end;

procedure TOpenLcbThrottleForm.OnConnectionStateChangeServer(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
 { case EthernetRec.ConnectionState of
     ccsListenerConnecting          :  HeaderLabel.Text := 'Connecting Server';
     ccsListenerConnected           :  HeaderLabel.Text := 'Connected Server';
     ccsListenerDisconnecting       :  HeaderLabel.Text := 'Disconnecting Server';
     ccsListenerDisconnected        :  HeaderLabel.Text := 'Disconnected Server';
     ccsListenerClientConnecting    :  HeaderLabel.Text := 'Connecting Client';
     ccsListenerClientConnected     :  HeaderLabel.Text := 'Connected Client';
     ccsListenerClientDisconnecting :  HeaderLabel.Text := 'Disconnecting Client';
     ccsListenerClientDisconnected  :  HeaderLabel.Text := 'Disconnected Client';
  end;    }
end;

procedure TOpenLcbThrottleForm.ScrollBarThrottleChange(Sender: TObject);
begin
  TextSpeed.Text := IntToStr( Round(ScrollBarThrottle.Value));
end;

procedure TOpenLcbThrottleForm.SpeedButtonOpenLCBClearClick(Sender: TObject);
begin
  MemoOpenLCB.BeginUpdate;
  try
    MemoOpenLCB.Lines.Clear;
  finally
    MemoOpenLCB.EndUpdate
  end;
end;

procedure TOpenLcbThrottleForm.SpeedButtonTrainSearchClick(Sender: TObject);
begin
  FloatAnimationTrainSearch.Inverse := not FloatAnimationTrainSearch.Inverse;
  FloatAnimationTrainSearch.Start
end;

procedure TOpenLcbThrottleForm.SwitchOpenLCBLogSwitch(Sender: TObject);
begin
  OpenLcbSettings.Log := SwitchSettingsDataFormatGridConnect.IsChecked;
end;

end.
