unit FormLccBaseTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList, FMX.MultiView, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.Layouts,
  System.Math.Vectors, FMX.Objects, FMX.Edit, FMX.Controls3D, FMX.Layers3D, System.IOUtils,
  lcc_node_manager, lcc_ethernet_server, lcc_node_controller, lcc_ethernet_client,
  lcc_ethernet_common, lcc_common_classes, lcc_defines, lcc_xmlutilities, lcc_file_utilities,
  lcc_utilities,
  FMX.Menus, FMX.Platform;

{$I lcc_compilers.inc}

const
  FILENAME_SETTINGS = 'settings.xml';
  FILENAME_MEMORY_CONFIG = 'memconfig.xml';
  FOLDERNAME_APP = 'LccThrottleApp';
  DEFAULT_IP_ADDRESS = '192.168.0.35';
  DEFAULT_PORT = 12021;
  DEFAULT_NODE_ID = '02.02.04.05.0A.0B';

type
  TLccBaseTemplate = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabControl2: TTabControl;
    TabItem5: TTabItem;
    ToolBar1: TToolBar;
    lblTitle1: TLabel;
    btnNext: TSpeedButton;
    TabItem6: TTabItem;
    ToolBar2: TToolBar;
    lblTitle2: TLabel;
    btnBack: TSpeedButton;
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
    ListView1: TListView;
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
    LabelPath: TLabel;
    PopupMenuLabelPath: TPopupMenu;
    MenuItemSettingsLabelPath: TMenuItem;
    TextConnectionStatus: TText;
    ButtonDeleteSettingsFile: TButton;
    ButtonDeleteAppFolder: TButton;
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

    procedure OnClientServerConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnClientServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
    function ValidEditBoxKey(Key: Word): Boolean;

    function ConnectionLogin: Boolean;
  end;

var
  LccBaseTemplate: TLccBaseTemplate;

implementation

{$R *.fmx}

function TLccBaseTemplate.ConnectionLogin: Boolean;
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

procedure TLccBaseTemplate.EditIpAddressExit(Sender: TObject);
begin
  if ValidateIPString(EditIpAddress.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'ipaddress', EditIpAddress.Text, True);
end;

procedure TLccBaseTemplate.EditIpAddressKeyDown(Sender: TObject;
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

procedure TLccBaseTemplate.EditNodeIDExit(Sender: TObject);
begin
  if ValidateNodeIDString(EditNodeID.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'nodeid', EditNodeID.Text, True);
end;

procedure TLccBaseTemplate.EditNodeIDKeyDown(Sender: TObject; var Key: Word;
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

procedure TLccBaseTemplate.EditPortExit(Sender: TObject);
begin
  if ValidatePort(EditPort.Text) then
    XmlNodeSetFirstLevelTextContent(PathSettingsFile, 'settings', 'port', EditPort.Text, True);
end;

procedure TLccBaseTemplate.EditPortKeyDown(Sender: TObject; var Key: Word;
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

procedure TLccBaseTemplate.ButtonDeleteAppFolderClick(Sender: TObject);
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

procedure TLccBaseTemplate.ButtonDeleteSettingsFileClick(Sender: TObject);
begin
  if TFile.Exists(PathSettingsFile) then
    TFile.Delete(PathSettingsFile)
end;

procedure TLccBaseTemplate.ButtonResetConnectionClick(Sender: TObject);
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

procedure TLccBaseTemplate.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FCloseQueried := True;
  TimerLogin.Enabled := False;
  NodeManager.LogoutAll;
  EthernetClient.CloseConnection(nil);
end;

procedure TLccBaseTemplate.FormCreate(Sender: TObject);
begin

  // Firemonkey controls setup
  TabControl1.ActiveTab := TabItem1;    // This defines the default active tab at runtime
  MultiViewConsist.Mode := TMultiViewMode.Drawer;
  TimerLogin.Enabled := False;

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

procedure TLccBaseTemplate.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEthernetClient);
  FreeAndNil(FNodeManager);
end;

procedure TLccBaseTemplate.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    if (TabControl1.ActiveTab = TabItem1) and (TabControl2.ActiveTab = TabItem6) then
    begin
      TabControl2.Previous;
      Key := 0;
    end;
  end;
end;

procedure TLccBaseTemplate.FormShow(Sender: TObject);
begin
  if not ShownOnce then
  begin
    FShownOnce := True;

    TimerLogin.Enabled := True; // Try to connect
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, FClipboard);

    LabelPath.Text := PathApplicationFiles; // Just to help find the settings file

    if FileExists(PathSettingsFile) then
      XmlLoadSettingsFromFile
    else
      XmlWriteDefaultFile;

    EditIpAddress.Text := CurrentIpAddress;
    EditPort.Text := IntToStr( CurrentPort);
    EditNodeID.Text := NodeIDToString(CurrentNodeID, True);
  end;
end;

procedure TLccBaseTemplate.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
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

procedure TLccBaseTemplate.MenuItemSettingsLabelPathClick(Sender: TObject);
begin
  if Assigned(Clipboard) then
    Clipboard.SetClipboard(LabelPath.Text)
end;

procedure TLccBaseTemplate.OnClientServerConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
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
   //       ControllerNode.OnTrainAssigned := @OnControllerNodeTrainAssigned;
   //       ControllerNode.OnTrainReleased := @OnControllerNodeTrainReleased;
        end;
      lcsDisconnecting :
        begin
          TextConnectionStatus.Text := 'disconnecting';
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

procedure TLccBaseTemplate.OnClientServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  NodeManager.LogoutAll;
  TimerLogin.Enabled := True
end;

procedure TLccBaseTemplate.TimerLoginTimer(Sender: TObject);
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

function TLccBaseTemplate.ValidEditBoxKey(Key: Word): Boolean;
begin
  // HardwareBack is to handle Android
  Result := (Key = vkReturn) or (Key = vkHardwareBack) or (Key = vkBack) or (Key = vkDelete) or (Key = vkLeft) or (Key = vkRight)
end;

procedure TLccBaseTemplate.XmlLoadSettingsFromFile;
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

procedure TLccBaseTemplate.XmlWriteDefaultFile;
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
