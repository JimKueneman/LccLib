unit OpenLcbThrottleUnit;

{$i lcc_compilers.inc}

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
  lcc_node_controller,
  lcc_node_train,
  lcc_math_float16, Data.DB, Datasnap.DBClient, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope;


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
    ListBoxSettings: TListBox;
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
    TextOpenLcbLogMessages: TText;
    MemoOpenLCB: TMemo;
    SpeedButtonMore: TSpeedButton;
    LayoutMainTrain: TLayout;
    GridLayoutFunctionControlsGroup0: TGridLayout;
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
    CornerButtonF10: TCornerButton;
    CornerButtonF11: TCornerButton;
    LayoutThrottleControls: TLayout;
    TextSpeed: TText;
    Text4: TText;
    ScrollBarThrottle: TScrollBar;
    CornerButtonForward: TCornerButton;
    TextDirection: TText;
    CornerButtonReverse: TCornerButton;
    LayoutDirectionControls: TLayout;
    RectangleTabSettingsBkGnd: TRectangle;
    RectangleTabOpenLcbBkGnd: TRectangle;
    TabItemNode: TTabItem;
    RectangleTabNodeBkGnd: TRectangle;
    ListBoxNode: TListBox;
    ListBoxGroupHeaderEthernet: TListBoxGroupHeader;
    ListBoxItem7: TListBoxItem;
    TextNodeAddress: TText;
    TextNetworkStatus: TText;
    ListBoxItem8: TListBoxItem;
    ButtonNetworkConnect: TButton;
    ButtonNetworkDisconnect: TButton;
    ListBoxItem9: TListBoxItem;
    TextNodeInfo: TText;
    TextNodeID: TText;
    ListBoxItem10: TListBoxItem;
    ButtonNodeCreate: TButton;
    ButtonNodeDestroy: TButton;
    ListBoxGroupHeaderStatus: TListBoxGroupHeader;
    LayoutOpenLcbTools: TLayout;
    ListBoxGroupHeaderOpenLcbNode: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    Text3: TText;
    TextNodeAlias: TText;
    ListBoxItem2: TListBoxItem;
    Text5: TText;
    TextMyIpAddress: TText;
    LayoutForm: TLayout;
    MultiViewTrains: TMultiView;
    Rectangle2: TRectangle;
    LayoutTrainsMultiViewToolbar: TLayout;
    SpeedButtonAddTrain: TSpeedButton;
    SpeedButtonTrainSearch: TSpeedButton;
    ListViewTrains: TListView;
    ColorKeyAlphaEffect1: TColorKeyAlphaEffect;
    LayoutTrainsMultiViewSearch: TLayout;
    EditTrainSearch: TEdit;
    FloatAnimationTrainSearch: TFloatAnimation;
    LayoutTrainsAdd: TLayout;
    FloatAnimationAddTrain: TFloatAnimation;
    ListBoxAddTrain: TListBox;
    ListBoxItemTrainName: TListBoxItem;
    ListBoxItemAddTrainNameEdit: TListBoxItem;
    ListBoxItemAddTrainRoadNumber: TListBoxItem;
    ListBoxItemAddTrainRoadNumberEdit: TListBoxItem;
    ListBoxItemAddTrainDecoderAddress: TListBoxItem;
    ListBoxItemAddTrainDecoderAddressEdit: TListBoxItem;
    EditAddTrainName: TEdit;
    EditAddTrainRoadNumber: TEdit;
    EditAddTrainDecoderAddress: TEdit;
    ListBoxItemEnter: TListBoxItem;
    ButtonAddTrainAdd: TButton;
    ClientDataSetTrains: TClientDataSet;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    ClientDataSetTrainsName: TStringField;
    ClientDataSetTrainsRoadNumber: TStringField;
    ClientDataSetTrainsDecoderAddress: TIntegerField;
    ClientDataSetTrainsIsLongAddress: TBooleanField;
    ClientDataSetTrainsSpeedStep: TIntegerField;
    CornerButtonF12: TCornerButton;
    CornerButtonF13: TCornerButton;
    CornerButtonF14: TCornerButton;
    CornerButtonF15: TCornerButton;
    CornerButtonF16: TCornerButton;
    CornerButtonF17: TCornerButton;
    CornerButtonF18: TCornerButton;
    CornerButtonF19: TCornerButton;
    CornerButtonF20: TCornerButton;
    CornerButtonF21: TCornerButton;
    CornerButtonF22: TCornerButton;
    CornerButtonF23: TCornerButton;
    CornerButtonF24: TCornerButton;
    CornerButtonF25: TCornerButton;
    CornerButtonF26: TCornerButton;
    CornerButtonF27: TCornerButton;
    CornerButtonF28: TCornerButton;
    TabControlFunctions: TTabControl;
    TabItemFunctionGroup0: TTabItem;
    TabItemFunctionGroup1: TTabItem;
    GridLayoutFunctionControlsGroup1: TGridLayout;
    SpeedButtonEditTrain: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure ScrollBarThrottleChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CornerButtonForwardClick(Sender: TObject);
    procedure CornerButtonReverseClick(Sender: TObject);
    procedure SpeedButtonTrainSearchClick(Sender: TObject);
    procedure MultiViewTrainsStartShowing(Sender: TObject);
    procedure GridLayoutFunctionControlsGroup0Resize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditSettingsServerPortKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsServerIPKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsBufferDepthKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditSettingsServerIPExit(Sender: TObject);
    procedure EditSettingsServerPortExit(Sender: TObject);
    procedure EditSettingsBufferDepthExit(Sender: TObject);
    procedure ButtonNetworkConnectClick(Sender: TObject);
    procedure ButtonNetworkDisconnectClick(Sender: TObject);
    procedure ButtonNodeCreateClick(Sender: TObject);
    procedure ButtonNodeDestroyClick(Sender: TObject);
    procedure SwitchOpenLCBLogSwitch(Sender: TObject);
    procedure SpeedButtonOpenLCBClearClick(Sender: TObject);
    procedure TabItemNodeClick(Sender: TObject);
    procedure SpeedButtonAddTrainClick(Sender: TObject);
    procedure ButtonAddTrainAddClick(Sender: TObject);
    procedure ListViewTrainsChange(Sender: TObject);
    procedure ListViewTrainsItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure CornerButtonFnClick(Sender: TObject);
    procedure GridLayoutFunctionControlsGroup1Resize(Sender: TObject);
    procedure SpeedButtonEditTrainClick(Sender: TObject);
  private
    FNodeManager: TLccCanNodeManager;
    FEthernetServer: TLccEthernetServer;
    FEthernetClient: TLccEthernetClient;
    FOpenLcbSettings: TOpenLcbSettings;
    FNodeID: string;
    FNodeAlias: string;
    FControllerNode: TLccTrainController;
    { Private declarations }
  protected
    // Client/Server Callbacks
    procedure OnClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnClientErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);

    // Node Manager Callbacks
    procedure OnNodeManagerIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerAliasChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeDestroy(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeCreate(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerNodeInitializationComplete(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    // The Controller is the Controller Node created in the NodeManager
    procedure OnControllerTrainAssigned(Sender: TLccNode; Reason: TControllerTrainAssignResult);
    procedure OnControllerTrainReleased(Sender: TLccNode);
    procedure OnControllerQuerySpeedReply(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte);
    procedure OnControllerQueryFunctionReply(Sender: TLccNode; Address: DWORD; Value: Word);
    procedure OnControllerReqestTakeover(Sender: TLccNode; var Allow: Boolean);
    procedure OnControllerSearchResult(Sender: TLccAssignTrainAction; Results: TLccSearchResultsArray; SearchResultCount: Integer; var SelectedResultIndex: Integer);

    procedure ReleaseTrain;

    procedure OpenAddTrainDrawer;
    procedure CloseAddTrainDrawer;

    function IsAddTrainDrawerOpen: Boolean;

  public
    { Public declarations }
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property EthernetServer: TLccEthernetServer read FEthernetServer write FEthernetServer;
    property NodeManager: TLccCanNodeManager read FNodeManager write FNodeManager;
    property ControllerNode: TLccTrainController read FControllerNode write FControllerNode; // First Node created by the NodeManager, it is assigned when the Ethenetlink is established

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

function TOpenLcbThrottleForm.IsAddTrainDrawerOpen: Boolean;
begin
  Result := LayoutTrainsAdd.Height > 0;
end;

procedure TOpenLcbThrottleForm.ButtonAddTrainAddClick(Sender: TObject);
//var
 // Stream: TMemoryStream;
begin
 // ClientDataSetTrains.Open;
  ClientDataSetTrains.DisableControls;
  try
  //  Stream := TMemoryStream.Create;
  //  ImageTrain.Bitmap.SaveToStream(Stream);

  //  Stream.Position := 0;
    ClientDataSetTrains.Append;
    ClientDataSetTrains.FieldByName('Name').AsString := EditAddTrainName.Text;
    ClientDataSetTrains.FieldByName('RoadNumber').AsString := EditAddTrainRoadNumber.Text;
    ClientDataSettrains.FieldByName('DccAddress').AsInteger := StrToInt(EditAddTrainDecoderAddress.Text);
    ClientDataSettrains.FieldByName('IsLongAddress').AsBoolean := True;
    ClientDataSettrains.FieldByName('SpeedStep').AsInteger := 0;   // Type cast to: TLccDccSpeedStep = (ldssDefault, ldss14, ldss28, ldss128);

 //  (ClientDataSetTrains.FieldByName('TrainImage') as TBlobField).LoadFromStream(Stream);
    ClientDataSetTrains.Post;

  finally
    ClientDataSetTrains.MergeChangeLog;
    ClientDataSetTrains.First;
  //  FreeAndNil(Stream);
  //  ClientDataSetTrains.Close;
    ClientDataSetTrains.EnableControls;
  end;

end;

procedure TOpenLcbThrottleForm.ButtonNetworkConnectClick(Sender: TObject);
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

procedure TOpenLcbThrottleForm.ButtonNetworkDisconnectClick(Sender: TObject);
begin
  if EthernetClient.Connected then
  begin
    EthernetClient.CloseConnection(nil);
  end;
end;

procedure TOpenLcbThrottleForm.ButtonNodeCreateClick(Sender: TObject);
var
  CanNode: TLccCanNode;
begin
  lcc_defines.Max_Allowed_Buffers := 1; // HACK ALLERT: Allow OpenLCB Python Scripts to run

  if NodeManager.Nodes.Count = 0 then
  begin
    CanNode := NodeManager.AddNode(CDI_XML) as TLccCanNode;

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

procedure TOpenLcbThrottleForm.ButtonNodeDestroyClick(Sender: TObject);
begin
  NodeManager.Clear;
end;

procedure TOpenLcbThrottleForm.CloseAddTrainDrawer;
begin
  if IsAddTrainDrawerOpen then
  begin
   FloatAnimationAddTrain.Inverse := not FloatAnimationAddTrain.Inverse;
   SpeedButtonAddTrain.StyleLookup := 'composetoolbutton';
    FloatAnimationAddTrain.Start;
  end;
end;

procedure TOpenLcbThrottleForm.CornerButtonFnClick(Sender: TObject);
begin
  if Assigned(ControllerNode) then
    ControllerNode.Functions[(Sender as TCornerButton).Tag] := not  ControllerNode.Functions[(Sender as TCornerButton).Tag]
end;

procedure TOpenLcbThrottleForm.CornerButtonForwardClick(Sender: TObject);
begin
  TextDirection.Text := 'Forward';
  if Assigned(ControllerNode) then
    ControllerNode.Direction := tdForward
end;

procedure TOpenLcbThrottleForm.CornerButtonReverseClick(Sender: TObject);
begin
  TextDirection.Text := 'Reverse';
  if Assigned(ControllerNode) then
    ControllerNode.Direction := tdReverse
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
  NodeManager.Clear;
  FreeAndNil(FNodeManager);
end;

procedure TOpenLcbThrottleForm.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControlMain.ActiveTab := TabItemTrains;

  FOpenLcbSettings := TOpenLcbSettings.Create;

  FEthernetClient := TLccEthernetClient.Create(nil);
  FEthernetServer := TLccEthernetServer.Create(nil);
  EthernetClient.OnConnectionStateChange := OnClientConnectionStateChange;
  EthernetClient.Gridconnect := True;

  FNodeManager := TLccCanNodeManager.Create(nil);
  NodeManager.OnLccNodeAliasIDChanged := OnNodeManagerAliasChange;
  NodeManager.OnLccNodeIDChanged := OnNodeManagerIDChange;
  NodeManager.OnLccNodeDestroy := OnNodeManagerNodeDestroy;
  NodeManager.OnLccNodeCreate := OnNodeManagerNodeCreate;
  NodeManager.OnLccNodeInitializationComplete := OnNodeManagerNodeInitializationComplete;

  NodeManager.OnLccMessageSend := OnNodeManagerSendMessage;
  NodeManager.OnLccMessageReceive := OnNodeManagerReceiveMessage;

  EthernetServer.NodeManager := NodeManager;
  EthernetClient.NodeManager := NodeManager;
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
var
  ListItemCount, i: Integer;
begin
  MultiViewTrains.HideMaster;
  SwitchSettingsDataFormatGridConnect.IsChecked := OpenLcbSettings.GridConnect;
  SwitchOpenLCBLog.IsChecked := OpenLcbSettings.Log;
  EditSettingsServerIP.Text := OpenLcbSettings.IpServerAddress;
  EditSettingsServerPort.Text := IntToStr(OpenLcbSettings.Port);
  EditSettingsBufferDepth.Text := IntToStr(OpenLcbSettings.MaxLoggingLines);

  // set the height of the slide down Add Train
  FloatAnimationAddTrain.StartValue  := 0;
  ListItemCount := ListBoxAddTrain.Items.Count;
  for i := 0 to ListItemCount - 1 do
    FloatAnimationAddTrain.StartValue := FloatAnimationAddTrain.StartValue + ListBoxAddTrain.ListItems[i].Height;
  FloatAnimationAddTrain.StartValue := FloatAnimationAddTrain.StartValue + 4;

  LayoutTrainsAdd.Height := 0;
  LayoutTrainsMultiViewSearch.Height := 0;

  ClientDataSetTrains.CreateDataSet;
  ClientDataSetTrains.Open;
end;

procedure TOpenLcbThrottleForm.ListViewTrainsChange(Sender: TObject);
begin
  ListViewTrains.Repaint;
  ToolBarLabel.Text :=  ClientDataSetTrains.FieldByName('Name').AsString + ': ' +
                        ClientDataSetTrains.FieldByName('RoadNumber').AsString;
  if Assigned(ControllerNode) then
    ControllerNode.AssignTrainByDccAddress(
      ClientDataSetTrains.FieldByName('DccAddress').AsInteger,
      ClientDataSetTrains.FieldByName('IsLongAddress').AsBoolean,
      TLccDccSpeedStep( ClientDataSetTrains.FieldByName('SpeedStep').AsInteger)
      );
end;

procedure TOpenLcbThrottleForm.ListViewTrainsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  ListViewTrains.Repaint;
 // MultiViewTrains.HideMaster;
//  ToggleAddTrainDrawer;
end;

procedure TOpenLcbThrottleForm.MultiViewTrainsStartShowing(Sender: TObject);
begin
  MultiViewTrains.Width := Width * 0.75
end;

procedure TOpenLcbThrottleForm.OnNodeManagerAliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  FNodeAlias := (LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TOpenLcbThrottleForm.OnNodeManagerNodeCreate(Sender: TObject; LccSourceNode: TLccNode);
begin
  TextNodeID.Text := 'Creating Node';
  TextNodeAlias.Text := '';
  ButtonNodeCreate.Enabled := False;
  ButtonNodeDestroy.Enabled := False;
end;

procedure TOpenLcbThrottleForm.OnNodeManagerNodeDestroy(Sender: TObject; LccSourceNode: TLccNode);
begin
  TextNodeID.Text := 'Not Connected';
  TextNodeAlias.Text := '';
  ButtonNodeCreate.Enabled := EthernetClient.Connected;
  ButtonNodeDestroy.Enabled := False;
  NodeID := '';
  NodeAlias := '';
end;

procedure TOpenLcbThrottleForm.OnNodeManagerIDChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  FNodeID := (LccSourceNode as TLccCanNode).NodeIDStr;
end;

procedure TOpenLcbThrottleForm.OnNodeManagerNodeInitializationComplete(
  Sender: TObject; LccSourceNode: TLccNode);
begin
  ButtonNodeCreate.Enabled := False;
  ButtonNodeDestroy.Enabled := True;
  TextNodeID.Text := '0x' + NodeID;
  TextNodeAlias.Text := NodeAlias;
end;

procedure TOpenLcbThrottleForm.OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  if OpenLcbSettings.Log then
  begin
    MemoOpenLCB.BeginUpdate;
    try
      while MemoOpenLCB.Lines.Count >= OpenLcbSettings.MaxLoggingLines do
        MemoOpenLCB.Lines.Delete(0);
      MemoOpenLCB.Lines.Add('R: ' + LccMessage.ConvertToGridConnectStr('', False));
      MemoOpenLCB.SelStart := Length(MemoOpenLCB.Lines.Text) - 1;
    finally
      MemoOpenLCB.EndUpdate
    end;
  end;
end;

procedure TOpenLcbThrottleForm.OnNodeManagerSendMessage(Sender: TObject;LccMessage: TLccMessage);
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
      MemoOpenLCB.SelStart := Length(MemoOpenLCB.Lines.Text) - 1;
    finally
      MemoOpenLCB.EndUpdate
    end;
  end;
end;

procedure TOpenLcbThrottleForm.OpenAddTrainDrawer;
begin
  if not IsAddTrainDrawerOpen then
  begin
   FloatAnimationAddTrain.Inverse := not FloatAnimationAddTrain.Inverse;
   SpeedButtonAddTrain.StyleLookup := 'arrowuptoolbutton';
   FloatAnimationAddTrain.Start;
  end;
end;

procedure TOpenLcbThrottleForm.OnClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
   case EthernetRec.ConnectionState of
      ccsClientConnecting    :  TextNetworkStatus.Text := 'Connecting';
      ccsClientConnected     :  begin
                                  TextNetworkStatus.Text := EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
                                  ButtonNetworkDisconnect.Enabled := True;
                                  ButtonNetworkConnect.Enabled := False;
                                  ButtonNodeCreate.Enabled := True;

                                  ControllerNode := NodeManager.AddNodeByClass('', TLccTrainController, True) as TLccTrainController;
                                  ControllerNode.OnTrainAssigned := OnControllerTrainAssigned;
                                  ControllerNode.OnTrainReleased := OnControllerTrainReleased;
                                  ControllerNode.OnControllerRequestTakeover := OnControllerReqestTakeover;
                                  ControllerNode.OnQuerySpeedReply := OnControllerQuerySpeedReply;
                                  ControllerNode.OnQueryFunctionReply := OnControllerQueryFunctionReply;
                                  ControllerNode.OnSearchResult := OnControllerSearchResult;
                               //   PanelThrottleFace1.Enabled := True;
                                end;
      ccsClientDisconnecting :  begin
                                  TextNetworkStatus.Text := 'Disconnecting';
                                  NodeManager.Clear;   // Logout
                                  ControllerNode := nil;
                                end;
      ccsClientDisconnected  :  begin
                                  TextNetworkStatus.Text := 'Not Connected';
                                  ButtonNetworkConnect.Enabled := True;
                                  ButtonNetworkDisconnect.Enabled := False;
                                  ButtonNodeCreate.Enabled := False;
                                  ButtonNodeDestroy.Enabled := False;
                                  NodeManager.Clear;
                                end;
   end;
end;

procedure TOpenLcbThrottleForm.OnClientErrorMessage(Sender: TObject;
  EthernetRec: TLccEthernetRec);
begin

end;

procedure TOpenLcbThrottleForm.OnControllerQueryFunctionReply(Sender: TLccNode;
  Address: DWORD; Value: Word);
begin
  ControllerNode.Functions[Address] := Value;
  case Address of
    0 : begin if Value = 0 then CornerButtonF0.IsPressed := False else CornerButtonF0.IsPressed := True; end;
    1 : begin if Value = 0 then CornerButtonF1.IsPressed := False else CornerButtonF1.IsPressed := True; end;
    2 : begin if Value = 0 then CornerButtonF2.IsPressed := False else CornerButtonF2.IsPressed := True; end;
    3 : begin if Value = 0 then CornerButtonF3.IsPressed := False else CornerButtonF3.IsPressed := True; end;
    4 : begin if Value = 0 then CornerButtonF4.IsPressed := False else CornerButtonF4.IsPressed := True; end;
    5 : begin if Value = 0 then CornerButtonF5.IsPressed := False else CornerButtonF5.IsPressed := True; end;
    6 : begin if Value = 0 then CornerButtonF6.IsPressed := False else CornerButtonF6.IsPressed := True; end;
    7 : begin if Value = 0 then CornerButtonF7.IsPressed := False else CornerButtonF7.IsPressed := True; end;
    8 : begin if Value = 0 then CornerButtonF8.IsPressed := False else CornerButtonF8.IsPressed := True; end;
    9 : begin if Value = 0 then CornerButtonF9.IsPressed := False else CornerButtonF9.IsPressed := True; end;
    10 : begin if Value = 0 then CornerButtonF10.IsPressed := False else CornerButtonF10.IsPressed := True; end;
    11 : begin if Value = 0 then CornerButtonF11.IsPressed := False else CornerButtonF11.IsPressed := True; end;
    12 : begin if Value = 0 then CornerButtonF12.IsPressed := False else CornerButtonF12.IsPressed := True; end;
    13 : begin if Value = 0 then CornerButtonF13.IsPressed := False else CornerButtonF13.IsPressed := True; end;
    14 : begin if Value = 0 then CornerButtonF14.IsPressed := False else CornerButtonF14.IsPressed := True; end;
    15 : begin if Value = 0 then CornerButtonF15.IsPressed := False else CornerButtonF15.IsPressed := True; end;
    16 : begin if Value = 0 then CornerButtonF16.IsPressed := False else CornerButtonF16.IsPressed := True; end;
    17 : begin if Value = 0 then CornerButtonF17.IsPressed := False else CornerButtonF17.IsPressed := True; end;
    18 : begin if Value = 0 then CornerButtonF18.IsPressed := False else CornerButtonF18.IsPressed := True; end;
    19 : begin if Value = 0 then CornerButtonF19.IsPressed := False else CornerButtonF19.IsPressed := True; end;
    20 : begin if Value = 0 then CornerButtonF20.IsPressed := False else CornerButtonF20.IsPressed := True; end;
    21 : begin if Value = 0 then CornerButtonF21.IsPressed := False else CornerButtonF21.IsPressed := True; end;
    22 : begin if Value = 0 then CornerButtonF22.IsPressed := False else CornerButtonF22.IsPressed := True; end;
    23 : begin if Value = 0 then CornerButtonF23.IsPressed := False else CornerButtonF23.IsPressed := True; end;
    24 : begin if Value = 0 then CornerButtonF24.IsPressed := False else CornerButtonF24.IsPressed := True; end;
    25 : begin if Value = 0 then CornerButtonF25.IsPressed := False else CornerButtonF25.IsPressed := True; end;
    26 : begin if Value = 0 then CornerButtonF26.IsPressed := False else CornerButtonF26.IsPressed := True; end;
    27 : begin if Value = 0 then CornerButtonF27.IsPressed := False else CornerButtonF27.IsPressed := True; end;
    28 : begin if Value = 0 then CornerButtonF28.IsPressed := False else CornerButtonF28.IsPressed := True; end;
  end;
end;

procedure TOpenLcbThrottleForm.OnControllerQuerySpeedReply(Sender: TLccNode;
  SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte);
begin
  ScrollBarThrottle.Value := Abs( Round(HalfToFloat(SetSpeed)));

  if HalfIsNegative(SetSpeed) then
  begin
    CornerButtonForward.IsPressed := False;
    CornerButtonReverse.IsPressed := True;
  end else
  begin
    CornerButtonForward.IsPressed := True;
    CornerButtonReverse.IsPressed := False;
  end;
end;

procedure TOpenLcbThrottleForm.OnControllerReqestTakeover(Sender: TLccNode;
  var Allow: Boolean);
begin
  // Allow :=  FormThrottleTakeover.ShowModal = mrYes
end;

procedure TOpenLcbThrottleForm.OnControllerSearchResult(
  Sender: TLccAssignTrainAction; Results: TLccSearchResultsArray;
  SearchResultCount: Integer; var SelectedResultIndex: Integer);
begin
  case SearchResultCount of
    0: ShowMessage('No Search Results');
    1: SelectedResultIndex := 0;
  else
    ShowMessage('Multiple Search Results: Please Select');
  end;
end;

procedure TOpenLcbThrottleForm.OnControllerTrainAssigned(Sender: TLccNode;
  Reason: TControllerTrainAssignResult);
begin
  case Reason of
    tarAssigned :
      begin
        ControllerNode.QuerySpeed;
        ControllerNode.QueryFunctions;
   //     PanelThrottleKeypad1.Enabled := True;
   //     SpeedButtonThrottleAssign1.Caption := 'Release Train';
      end;
    tarFailTrainRefused      : ShowMessage('Train refused assignment to controller');
    tarFailControllerRefused : ShowMessage('Current controller refused to release train');
  else
    ShowMessage('Unknown ControllerTrainAssigned1 result');
  end;
end;

procedure TOpenLcbThrottleForm.OnControllerTrainReleased(Sender: TLccNode);
begin
 // PanelThrottleKeypad1.Enabled := False;
end;

procedure TOpenLcbThrottleForm.ReleaseTrain;
begin

end;

procedure TOpenLcbThrottleForm.ScrollBarThrottleChange(Sender: TObject);
begin
  TextSpeed.Text := IntToStr( Round(ScrollBarThrottle.Value));
  if Assigned(ControllerNode) then
    ControllerNode.Speed := ScrollBarThrottle.Value;
end;

procedure TOpenLcbThrottleForm.SpeedButtonAddTrainClick(Sender: TObject);
begin
  if IsAddTrainDrawerOpen then CloseAddTrainDrawer else OpenAddTrainDrawer;
end;

procedure TOpenLcbThrottleForm.SpeedButtonEditTrainClick(Sender: TObject);
begin
  ListViewTrains.EditMode := not ListviewTrains.EditMode;
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

procedure TOpenLcbThrottleForm.GridLayoutFunctionControlsGroup0Resize(Sender: TObject);
const
  FIXED_COLUMNS_GROUP_0 = 3;
  FIXED_ROWS_GROUP_0 = 4;
  FIXED_PADDING = 10;
var
  GridClientW, GridClientH: single;
begin
  GridClientW := GridLayoutFunctionControlsGroup0.Width - (GridLayoutFunctionControlsGroup0.Padding.Left+GridLayoutFunctionControlsGroup0.Padding.Right);
  GridClientH := GridLayoutFunctionControlsGroup0.Height - (GridLayoutFunctionControlsGroup0.Padding.Top+GridLayoutFunctionControlsGroup0.Padding.Bottom);

  if (GridClientW > 0) and (GridClientH > 0) then
  begin
    GridLayoutFunctionControlsGroup0.ItemWidth := (GridClientW - FIXED_PADDING)/FIXED_COLUMNS_GROUP_0;
    GridLayoutFunctionControlsGroup0.ItemHeight := (GridClientH - FIXED_PADDING)/FIXED_ROWS_GROUP_0;
  end;

end;

procedure TOpenLcbThrottleForm.GridLayoutFunctionControlsGroup1Resize(Sender: TObject);
const
  FIXED_COLUMNS_GROUP_1 = 3;
  FIXED_ROWS_GROUP_1 = 6;
  FIXED_PADDING = 10;
var
  GridClientW, GridClientH: single;
begin
  GridClientW := GridLayoutFunctionControlsGroup1.Width - (GridLayoutFunctionControlsGroup1.Padding.Left+GridLayoutFunctionControlsGroup1.Padding.Right);
  GridClientH := GridLayoutFunctionControlsGroup1.Height - (GridLayoutFunctionControlsGroup1.Padding.Top+GridLayoutFunctionControlsGroup1.Padding.Bottom);

  if (GridClientW > 0) and (GridClientH > 0) then
  begin
    GridLayoutFunctionControlsGroup1.ItemWidth := (GridClientW - FIXED_PADDING)/FIXED_COLUMNS_GROUP_1;
    GridLayoutFunctionControlsGroup1.ItemHeight := (GridClientH - FIXED_PADDING)/FIXED_ROWS_GROUP_1;
  end
end;

procedure TOpenLcbThrottleForm.TabItemNodeClick(Sender: TObject);
begin
  {$IFDEF LCC_WINDOWS}
    TextMyIpAddress.Text := ResolveWindowsIp;
  {$ELSE}
    TextMyIpAddress.Text := ResolveUnixIp;
  {$ENDIF}
end;

end.
