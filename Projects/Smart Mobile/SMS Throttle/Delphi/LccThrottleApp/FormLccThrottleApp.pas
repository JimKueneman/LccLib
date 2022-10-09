unit FormLccThrottleApp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, FMX.MultiView, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Layouts,
  System.Math.Vectors, FMX.Objects, FMX.Edit, FMX.Controls3D, FMX.Layers3D, System.IOUtils,
  lcc_node_manager,
  lcc_ethernet_server,
  lcc_node_controller,
  lcc_ethernet_client,
  lcc_ethernet_common,
  lcc_common_classes,
  lcc_defines,
  lcc_xmlutilities,
  lcc_file_utilities,
  lcc_utilities,
  lcc_node,
  lcc_node_messages,
  lcc_train_server,
  lcc_alias_server,

  FMX.Menus, FMX.Platform, FMX.ListBox, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Header;

{$I lcc_compilers.inc}

const
  FILENAME_SETTINGS = 'settings.xml';
  FILENAME_MEMORY_CONFIG = 'memconfig.xml';
  FOLDERNAME_APP = 'LccThrottleApp';
  DEFAULT_IP_ADDRESS = '192.168.0.35';
  DEFAULT_PORT = 12021;
  DEFAULT_NODE_ID = '02.02.04.05.0A.0B';

type
  TLccThrottleAppForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ToolBar3: TToolBar;
    lblTitle3: TLabel;
    TabItem3: TTabItem;
    ToolBar4: TToolBar;
    lblTitle4: TLabel;
    TabItem4: TTabItem;
    ToolBar5: TToolBar;
    LabelSettingsHeader: TLabel;
    GestureManager1: TGestureManager;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    MultiViewConsist: TMultiView;
    ListViewTrainRoster: TListView;
    SpeedButtonTab2Hamburger: TSpeedButton;
    LayoutSettingTab: TLayout;
    ButtonResetConnection: TButton;
    Layout3D1: TLayout3D;
    CheckBoxRawTCP: TCheckBox;
    EditNodeID: TEdit;
    TextNodeID: TText;
    TextPort: TText;
    EditPort: TEdit;
    EditIpAddress: TEdit;
    TextIpAddress: TText;
    TimerLogin: TTimer;
    LabelSystemDocumentsPath: TLabel;
    PopupMenuLabelPath: TPopupMenu;
    MenuItemSettingsLabelPath: TMenuItem;
    TextConnectionStatus: TText;
    ButtonDeleteSettingsFile: TButton;
    ButtonDeleteAppFolder: TButton;
    LabelSystemDocumentsPathHeader: TLabel;
    LabelApplicationDocumentsHeader: TLabel;
    LabelApplicationDocumentsPath: TLabel;
    TextDebugHeader: TText;
    Layout1: TLayout;
    TabControlTrainRoster: TTabControl;
    TabItemTrainRosterSelect: TTabItem;
    TabItemTrainRosterDetails: TTabItem;
    ListBoxTrainRoster: TListBox;
    ToolBarTrainRosterDetails: TToolBar;
    LabelTrainRosterHeader: TLabel;
    SpeedButtonTrainRosterBack: TSpeedButton;
    TabItemTrainRosterEdit: TTabItem;
    ToolBarTrainRosterEdit: TToolBar;
    LabelTrainRosterEdit: TLabel;
    SpeedButtonTrainRosterEdit: TSpeedButton;
    ActionTrainRosterTabNext: TNextTabAction;
    ActionTrainRosterTabPrev: TPreviousTabAction;
    LayoutLog: TLayout;
    HeaderLogHeader: THeader;
    LabelLogHeader: TLabel;
    MemoLog: TMemo;
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure TimerLoginTimer(Sender: TObject);
    procedure ButtonResetConnectionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemSettingsLabelPathClick(Sender: TObject);
    procedure EditIpAddressKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditPortKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditNodeIDKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure EditIpAddressExit(Sender: TObject);
    procedure EditPortExit(Sender: TObject);
    procedure EditNodeIDExit(Sender: TObject);
    procedure ButtonDeleteSettingsFileClick(Sender: TObject);
    procedure ButtonDeleteAppFolderClick(Sender: TObject);
    procedure SpeedButtonTrainRosterBackClick(Sender: TObject);
    procedure ListViewTrainRosterItemClickEx(const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure MultiViewConsistHidden(Sender: TObject);
  private
    FNodeManager: TLccNodeManager;
    FEthernetClient: TLccEthernetClient;
    FControllerNode: TLccTrainController;
    FConnectionState: TLccConnectionState;
    FShownOnce: Boolean;
    FCloseQueried: Boolean;
    FClipboard: IFMXClipboardService;
    FCurrentNodeID: TNodeID;
    FCurrentPort: Word;
    FCurrentIpAddress: string;
    FPathSettingsFile: string;
    FPathApplicationFiles: string;
    FPathMemoryConfig: string;
    FActiveTrainObject: TListViewItem;
    { Private declarations }

  protected

    procedure XmlLoadSettingsFromFile;
    procedure XmlWriteDefaultFile;

  public
    { Public declarations }
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property ControllerNode: TLccTrainController read FControllerNode write FControllerNode;
    property ConnectionState: TLccConnectionState read FConnectionState write FConnectionState;
    property ShownOnce: Boolean read FShownOnce;
    property CloseQueried: Boolean read FCloseQueried;
    property Clipboard: IFMXClipboardService read FClipboard write FClipboard;
    property CurrentIpAddress: string read FCurrentIpAddress write FCurrentIpAddress;
    property CurrentPort: Word read FCurrentPort write FCurrentPort;
    property CurrentNodeID: TNodeID read FCurrentNodeID write FCurrentNodeID;
    property PathApplicationFiles: string read FPathApplicationFiles write FPathApplicationFiles;
    property PathSettingsFile: string read FPathSettingsFile write FPathSettingsFile;
    property PathMemoryConfig: string read FPathMemoryConfig write FPathMemoryConfig;
    property ActiveTrainObject: TListViewItem read FActiveTrainObject write FActiveTrainObject;

    // Callbacks
    procedure OnNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnAliasMappingChange(Sender: TObject; LccSourceNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
    procedure OnTrainRegisteringChange(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject; IsRegistered: Boolean);
    procedure OnLccTractionUpdateSNIP(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject);
    procedure OnLccTractionUpdateTrainSNIP(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject);
    procedure OnLccTractionUpdateListenerCount(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject);

    procedure OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    procedure OnClientServerConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnClientServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
    function ValidEditBoxKey(Key: Word): Boolean;
    function FindTrainRosterListItem(TrainObject: TLccTrainObject): TListViewItem;
    function ConnectionLogin: Boolean;
  end;

var
  LccThrottleAppForm: TLccThrottleAppForm;

implementation

{$R *.fmx}

function TLccThrottleAppForm.ConnectionLogin: Boolean;
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  Result := True;

  LocalInfo := TLccEthernetConnectionInfo.Create;
  try
    TextConnectionStatus.Text := 'connecting';
    TextConnectionStatus.TextSettings.FontColor := TAlphaColors.Black;

    LocalInfo.AutoResolveIP := False;
    LocalInfo.ListenerPort := CurrentPort;
    LocalInfo.ListenerIP := CurrentIpAddress;
    LocalInfo.GridConnect := not CheckBoxRawTCP.IsChecked;

    EthernetClient.OpenConnection(LocalInfo);
  finally
    LocalInfo.Free;
  end;
end;

procedure TLccThrottleAppForm.EditIpAddressExit(Sender: TObject);
begin
  if ValidateIPString(EditIpAddress.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'ipaddress', EditIpAddress.Text, True);
end;

procedure TLccThrottleAppForm.EditIpAddressKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if not( CharInSet(KeyChar, ['0'..'9', '.']) or ValidEditBoxKey(Key) ) then
  begin
    Key := 0;
    KeyChar := #0;
  end;
  if (Key = vkReturn) and ValidateIPString(EditIpAddress.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'ipaddress', EditIpAddress.Text, True);
end;

procedure TLccThrottleAppForm.EditNodeIDExit(Sender: TObject);
begin
  if ValidateNodeIDString(EditNodeID.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'nodeid', EditNodeID.Text, True);
end;

procedure TLccThrottleAppForm.EditNodeIDKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if CharInSet(KeyChar, ['a'..'f']) then
   KeyChar := UpCase(KeyChar);

  if not( CharInSet(KeyChar, ['0'..'9', 'A'..'F', '.']) or ValidEditBoxKey(Key) ) then
  begin
    Key := 0;
    KeyChar := #0;
  end;

  if (Key = vkReturn) and ValidateNodeIDString(EditNodeID.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'nodeid', EditNodeID.Text, True);
end;

procedure TLccThrottleAppForm.EditPortExit(Sender: TObject);
begin
  if ValidatePort(EditPort.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'port', EditPort.Text, True);
end;

procedure TLccThrottleAppForm.EditPortKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if not( CharInSet(KeyChar, ['0'..'9']) or ValidEditBoxKey(Key) ) then
  begin
    Key := 0;
    KeyChar := #0;
  end;
  if (Key = vkReturn) and ValidatePort(EditPort.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'port', EditPort.Text, True);
end;

procedure TLccThrottleAppForm.ButtonDeleteAppFolderClick(Sender: TObject);
var
  Files: TStringDynArray;
  i: Integer;
begin
  if TDirectory.Exists(PathApplicationFiles) then
  begin
    Files := TDirectory.GetFiles(PathApplicationFiles);
    for i  := 0 to Length(Files) - 1 do
      TFile.Delete(Files[i]);
    TDirectory.Delete(PathApplicationFiles)
  end;
end;

procedure TLccThrottleAppForm.ButtonDeleteSettingsFileClick(Sender: TObject);
begin
  if TFile.Exists(PathSettingsFile) then
    TFile.Delete(PathSettingsFile)
end;

procedure TLccThrottleAppForm.ButtonResetConnectionClick(Sender: TObject);
begin
  if not ValidateIPString(EditIpAddress.Text) then
  begin
    TextConnectionStatus.Text := 'Invalid IP Address';
    TextConnectionStatus.TextSettings.FontColor := TAlphaColors.Red;
    TextIpAddress.TextSettings.FontColor := TAlphaColors.Red;
    Exit
  end;
  if not ValidateNodeIDString(EditNodeID.Text) then
  begin
    TextConnectionStatus.Text := 'Invalid NodeID';
    TextConnectionStatus.TextSettings.FontColor := TAlphaColors.Red;
    TextNodeID.TextSettings.FontColor := TAlphaColors.Red;
    Exit
  end;
  if ValidatePort(EditPort.Text) then
  begin
    TextConnectionStatus.Text := 'Invalid Port (must be 65535 or less)';
    TextConnectionStatus.TextSettings.FontColor := TAlphaColors.Red;
    TextPort.TextSettings.FontColor := TAlphaColors.Red;
    Exit
  end;

  TextConnectionStatus.TextSettings.FontColor := TAlphaColors.Black;
  TextIpAddress.TextSettings.FontColor := TAlphaColors.Black;
  TextPort.TextSettings.FontColor := TAlphaColors.Black;

  CurrentIpAddress := EditIpAddress.Text;
  CurrentPort := StrToInt(EditPort.Text);
  CurrentNodeID := StrToNodeID(EditNodeID.Text, True);

  NodeManager.LogoutAll;
  EthernetClient.CloseConnection(nil);
  TimerLogin.Enabled := True;
end;

function TLccThrottleAppForm.FindTrainRosterListItem(TrainObject: TLccTrainObject): TListViewItem;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < ListViewTrainRoster.Items.Count - 1 do
  begin
    if TrainObject = TLccTrainObject( ListViewTrainRoster.Items[i].Tag) then
      Result := ListViewTrainRoster.Items[i];
    Inc(i);
  end;
end;

procedure TLccThrottleAppForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FCloseQueried := True;
  TimerLogin.Enabled := False;
  NodeManager.LogoutAll;
  EthernetClient.CloseConnection(nil);
end;

procedure TLccThrottleAppForm.FormCreate(Sender: TObject);
begin

  // Local field setup

  // Lcc library setup
  NodeManager := TLccNodeManager.Create(nil, True);
  EthernetClient := TLccEthernetClient.Create(nil, NodeManager);
  EthernetClient.OnConnectionStateChange := OnClientServerConnectionChange;
  EthernetClient.OnErrorMessage := OnClientServerErrorMessage;

  // Default values to the settings
  CurrentIpAddress := DEFAULT_IP_ADDRESS;
  CurrentPort := DEFAULT_PORT;
  CurrentNodeID := StrToNodeID( DEFAULT_NODE_ID, True);

  // Default Paths for files
  PathApplicationFiles := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP;
  PathSettingsFile := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_SETTINGS;
  PathMemoryConfig := TPath.GetDocumentsPath + TPath.DirectorySeparatorChar + FOLDERNAME_APP + TPath.DirectorySeparatorChar + FILENAME_MEMORY_CONFIG;
end;

procedure TLccThrottleAppForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEthernetClient);
  FreeAndNil(FNodeManager);
end;

procedure TLccThrottleAppForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
 //   if (TabControl1.ActiveTab = TabItem1) and (TabControl2.ActiveTab = TabItem6) then
 //   begin
 //     TabControl2.Previous;
 //     Key := 0;
 //   end;
  end;
end;

procedure TLccThrottleAppForm.FormShow(Sender: TObject);
begin
  if not ShownOnce then
  begin
    FShownOnce := True;

    // Setup common variabled to use
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, FClipboard);
    LabelSystemDocumentsPath.Text := TPath.GetDocumentsPath;
    LabelApplicationDocumentsPath.Text := PathApplicationFiles;

    // Setup components to a standard state in case forgotten in the designer
    TabControl1.ActiveTab := TabItem1;    // This defines the default active tab at runtime
    MultiViewConsist.Mode := TMultiViewMode.Drawer;
    TimerLogin.Enabled := False;
    TabControlTrainRoster.ActiveTab := TabItemTrainRosterSelect;
    TimerLogin.Enabled := True; // Try to connect

    if FileExists(PathSettingsFile) then
      XmlLoadSettingsFromFile
    else
      XmlWriteDefaultFile;

    EditIpAddress.Text := CurrentIpAddress;
    EditPort.Text := IntToStr( CurrentPort);
    EditNodeID.Text := NodeIDToString(CurrentNodeID, True);
  end;
end;

procedure TLccThrottleAppForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[TabControl1.TabCount - 1] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex + 1];
        Handled := True;
      end;

    sgiRight:
      begin
        if TabControl1.ActiveTab <> TabControl1.Tabs[0] then
          TabControl1.ActiveTab := TabControl1.Tabs[TabControl1.TabIndex - 1];
        Handled := True;
      end;
  end;
end;

procedure TLccThrottleAppForm.ListViewTrainRosterItemClickEx(const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
var
  ListItem: TListItem;
begin
  if ItemObject is TListItemAccessory then
    ActionTrainRosterTabNext.Execute
  else begin
    ListItem := ListViewTrainRoster.Items[ItemIndex];
    MultiViewConsist.HideMaster
  end;
end;

procedure TLccThrottleAppForm.MenuItemSettingsLabelPathClick(Sender: TObject);
begin
  if Assigned(Clipboard) then
    Clipboard.SetClipboard(LabelSystemDocumentsPath.Text)
end;

procedure TLccThrottleAppForm.MultiViewConsistHidden(Sender: TObject);
begin
  ActiveTrainObject := nil;
end;

procedure TLccThrottleAppForm.OnAliasMappingChange(Sender: TObject;
  LccSourceNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
begin

end;

procedure TLccThrottleAppForm.OnClientServerConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  if Sender is TLccConnectionThread then
  begin
    ConnectionState := Info.ConnectionState;

    case Info.ConnectionState of
      lcsConnecting :
        begin
          TimerLogin.Enabled := False;
          TextConnectionStatus.Text := 'connecting';
        end;
     lcsConnected :
        begin
          TextConnectionStatus.Text := 'connected';
          ControllerNode := NodeManager.AddNodeByClass('', TLccTrainController, True, CurrentNodeID) as TLccTrainController;

          NodeManager.OnAliasMappingChange := OnAliasMappingChange;
          NodeManager.OnTrainRegisteringChange := OnTrainRegisteringChange;
          NodeManager.OnLccNodeLogin := OnNodeLogin;
          NodeManager.OnLccTractionUpdateSNIP := OnLccTractionUpdateSNIP;
          NodeManager.OnLccTractionUpdateTrainSNIP := OnLccTractionUpdateSNIP;
          NodeManager.OnLccTractionUpdateListenerCount := OnLccTractionUpdateListenerCount;

   //       ControllerNode.OnTrainAssigned := @OnControllerNodeTrainAssigned;
   //       ControllerNode.OnTrainReleased := @OnControllerNodeTrainReleased;
        end;
      lcsDisconnecting :
        begin
          TextConnectionStatus.Text := 'disconnecting';

          NodeManager.OnAliasMappingChange := nil;
          NodeManager.OnTrainRegisteringChange := nil;
          NodeManager.OnLccNodeLogin := nil;
          NodeManager.OnLccTractionUpdateSNIP := nil;
          NodeManager.OnLccTractionUpdateTrainSNIP := nil;
          NodeManager.OnLccTractionUpdateListenerCount := nil;

          NodeManager.Clear;   // Logout
          ControllerNode := nil;
        end;
      lcsDisconnected :
        begin
          TextConnectionStatus.Text := 'disconnected';
          TimerLogin.Enabled := True;  // Try to reconnect
        end;
    end;
  end;
end;

procedure TLccThrottleAppForm.OnClientServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  NodeManager.LogoutAll;
  TimerLogin.Enabled := True
end;

procedure TLccThrottleAppForm.OnLccTractionUpdateListenerCount(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject);
begin

end;

procedure TLccThrottleAppForm.OnLccTractionUpdateSNIP(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject);
var
  ListViewItem: TListViewItem;
begin
  ListViewItem := FindTrainRosterListItem(TrainObject);
  if Assigned(ListViewItem) then
  begin
    ListViewItem.Text := TrainObject.SNIP.UserName
  end;


end;

procedure TLccThrottleAppForm.OnLccTractionUpdateTrainSNIP(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject);
begin

end;

procedure TLccThrottleAppForm.OnNodeLogin(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = ControllerNode then
    ControllerNode.FindAllTrains;
end;

procedure TLccThrottleAppForm.OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Add('R: ' + MessageToDetailedMessage(LccMessage))
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TLccThrottleAppForm.OnNodeManagerSendMessage(Sender: TObject;  LccMessage: TLccMessage);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Add('S: ' + MessageToDetailedMessage(LccMessage))
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TLccThrottleAppForm.OnTrainRegisteringChange(Sender: TObject; LccSourceNode: TLccNode; TrainObject: TLccTrainObject; IsRegistered: Boolean);
var
  TrainListViewItem: TListViewItem;
begin
  if IsRegistered then
  begin
    TrainListViewItem := ListViewTrainRoster.Items.Add;
    TrainListViewItem.Tag := nativeint( TrainObject);   /// HOW DO WE HOLD REFERENCES FOR FUTURE USE OF UPDATES IN EVENTS?

    if TrainObject.SNIP.Valid then
    begin
      TrainListViewItem.Text := 'Train: ' + TrainObject.SNIP.UserName;
    end else
    begin
      TrainListViewItem.Text := 'Train: ' + NodeIDToString(TrainObject.NodeID, True);
    end;
  end else
  begin
    TrainListViewItem := FindTrainRosterListItem(TrainObject);
    if Assigned(TrainListViewItem) then
    begin
      if ActiveTrainObject = TrainListViewItem then
      begin
        TabControlTrainRoster.TabIndex := 0;
        ActiveTrainObject := nil;
      end;

    end;

  end;
end;

procedure TLccThrottleAppForm.SpeedButtonTrainRosterBackClick(Sender: TObject);
begin
  ActionTrainRosterTabPrev.Execute
end;

procedure TLccThrottleAppForm.TimerLoginTimer(Sender: TObject);
begin
  if not CloseQueried and (ConnectionState = lcsDisconnected) then
  begin
    TimerLogin.Enabled := False;
    ConnectionState := lcsConnecting;    // Fake this starting, the statemachine will reset it once the thread starts
    if not ConnectionLogin then
    begin
      TimerLogin.Enabled := True;
      ConnectionState := lcsDisconnected;
    end;
  end;
end;

function TLccThrottleAppForm.ValidEditBoxKey(Key: Word): Boolean;
begin
  // HardwareBack is to handle Android
  Result := (Key = vkReturn) or (Key = vkHardwareBack) or (Key = vkBack) or (Key = vkDelete) or (Key = vkLeft) or (Key = vkRight)
end;

procedure TLccThrottleAppForm.XmlLoadSettingsFromFile;
var
  SettingsXML: LccXmlDocument;
  RootNode, ChildNode: LccXmlNode;
begin
  // Read in the Setting File
  SettingsXML := XmlLoadFromFile(PathSettingsFile);
  RootNode := XmlFindRootNode(SettingsXML, 'settings');
  if Assigned(RootNode) then
  begin
    ChildNode := XmlFindChildNode(RootNode, 'ipaddress');
    if Assigned(ChildNode) then
    begin
      if ValidateIPString( XmlNodeTextContent(ChildNode)) then
        CurrentIpAddress := XmlNodeTextContent(ChildNode);
    end;
    ChildNode := XmlFindChildNode(RootNode, 'port');
    if Assigned(ChildNode) then
    begin
      if ValidatePort( string( XmlNodeTextContent(ChildNode))) then
        CurrentPort := StrToInt( string( XmlNodeTextContent(ChildNode)));
    end;
    ChildNode := XmlFindChildNode(RootNode, 'nodeid');
    if Assigned(ChildNode) then
    begin
      if ValidateNodeIDString(XmlNodeTextContent(ChildNode)) then
        CurrentNodeID := StrToNodeID( XmlNodeTextContent(ChildNode), True);
    end;
  end;
end;

procedure TLccThrottleAppForm.XmlWriteDefaultFile;
var
  SettingsXML: LccXmlDocument;
  RootNode, ChildNode: LccXmlNode;
begin
  if not DirectoryExists(PathApplicationFiles) then
    ForceDirectories(PathApplicationFiles);

  if DirectoryExists(PathApplicationFiles) then
  begin
     SettingsXML := XmlCreateEmptyDocument;
     RootNode := XmlCreateRootNode(SettingsXML, 'settings', '');
     ChildNode := XmlCreateChildNode(SettingsXML, RootNode, 'ipaddress', CurrentIpAddress);
     ChildNode := XmlCreateChildNode(SettingsXML, RootNode, 'port', IntToStr(CurrentPort));
     ChildNode := XmlCreateChildNode(SettingsXML, RootNode, 'nodeid', NodeIDToString( CurrentNodeID, True));
     XmlWriteToFile(PathSettingsFile, SettingsXML);
     XmlFreeDocument(SettingsXML);
  end;
end;

end.

