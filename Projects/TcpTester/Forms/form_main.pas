unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, ExtCtrls, Menus, StdCtrls, Spin, lcc_app_common_settings,
  lcc_comport, lcc_nodemanager, form_settings, file_utilities,
  frame_lcc_logging, lcc_messages, lcc_ethenetserver, lcc_ethernetclient,
  form_logging, lcc_nodeselector, lcc_cdi_parser, lcc_defines, contnrs,
  form_properties, lcc_message_scheduler, IniFiles, form_about, LCLType, types,
  lcc_utilities;

const
    BUNDLENAME  = 'Raspberry Pi Hub';

    STATUS_PANEL_ETHERNET = 0;
    STATUS_PANEL_COMPORT  = 1;
    STATUS_PANEL_NODEID   = 2;


type
  TMouseInfo = record
    Button: TMouseButton;
    Shift: TShiftState;
    X, Y: Integer
  end;

  { TCdiGroup }

  TCdiGroup = class(TPersistent)
  private
    FDataSize: Integer;
    FDataType: TLccConfigDataType;
    FDescription: string;
    FMemOffset: Integer;
    FName: string;
  public
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property MemOffset: Integer read FMemOffset write FMemOffset;
    property DataType: TLccConfigDataType read FDataType write FDataType;
    property DataSize: Integer read FDataSize write FDataSize;
  end;

  { TNetworkNodeCache }

  TNetworkNodeCache = class(TPersistent)
  private
    FHardwareVersion: string;
    FSoftwareVersion: string;
  public
    property HardwareVersion: string read FHardwareVersion write FHardwareVersion;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;

  end;

  { TForm1 }

  TForm1 = class(TForm)
    ActionToolsPreferenceShowMac: TAction;
    ActionHelpAboutShow: TAction;
    ActionEthernetClient: TAction;
    ActionPropertiesWindow: TAction;
    ActionTCP: TAction;
    ActionShowNodeProperties: TAction;
    ActionEditUserStrings: TAction;
    ActionListSelector: TActionList;
    ActionLogWindow: TAction;
    ActionEthernetServer: TAction;
    ActionComPort: TAction;
    ActionToolsSettingsShowWin: TAction;
    ActionList: TActionList;
    ButtonShowOutgoingMessages: TButton;
    ButtonShowWaitingSchedulerMessages: TButton;
    CheckBoxDumbNode: TCheckBox;
    CheckBoxAutoQueryInfo: TCheckBox;
    CheckBoxAutoSendVerifyNodes: TCheckBox;
    ImageListMain: TImageList;
    ImageListNodeList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelMyNodes: TLabel;
    LabelServerConnections: TLabel;
    LccComPort: TLccComPort;
    LccEthernetClient: TLccEthernetClient;
    LccEthernetServer: TLccEthernetServer;
    LccNodeManager: TLccNodeManager;
    LccNodeSelector: TLccNodeSelector;
    LccNodeSelectorProducer1: TLccNodeSelector;
    LccSettings: TLccSettings;
    ListViewServerConnections: TListView;
    MainMenu: TMainMenu;
    MenuItemConnectionDivider0: TMenuItem;
    MenuItemConnectionUseTCP: TMenuItem;
    MenuItemConnectionEthernetServer: TMenuItem;
    MenuItemConnectionEthernetClient: TMenuItem;
    MenuItemConnectionComport: TMenuItem;
    MenuItemConnection: TMenuItem;
    MenuItemToolsLogWindow: TMenuItem;
    MenuItemToolsProperties: TMenuItem;
    MenuItemToolsSettings: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemLastSpace: TMenuItem;
    MenuItemNodeProperties: TMenuItem;
    MenuItemPopupSelectorEditUserStrings: TMenuItem;
    PageControlMain: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelAddOns: TPanel;
    PanelNetworkTree: TPanel;
    PopupMenuSelector: TPopupMenu;
    SplitterMain: TSplitter;
    StatusBarMain: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheetMain: TTabSheet;
    TabSheetDebug: TTabSheet;
    ToolBar1: TToolBar;
    ToolButtonSettings: TToolButton;
    ToolButtonEthClient: TToolButton;
    ToolButtonSpace1: TToolButton;
    ToolButtonComPort: TToolButton;
    ToolButtonLogWindow: TToolButton;
    ToolButtonEthServer: TToolButton;
    ToolButtonEthUseTCP: TToolButton;
    ToolButtonSpace0: TToolButton;
    ToolButtonProperties: TToolButton;
    ToolButtonSpace2: TToolButton;
    procedure ActionComPortExecute(Sender: TObject);
    procedure ActionEditUserStringsExecute(Sender: TObject);
    procedure ActionEthernetClientExecute(Sender: TObject);
    procedure ActionEthernetServerExecute(Sender: TObject);
    procedure ActionHelpAboutShowExecute(Sender: TObject);
    procedure ActionLogWindowExecute(Sender: TObject);
    procedure ActionPropertiesWindowExecute(Sender: TObject);
    procedure ActionToolsSettingsShowWinExecute(Sender: TObject);
    procedure ActionShowNodePropertiesExecute(Sender: TObject);
    procedure ActionTCPExecute(Sender: TObject);
    procedure ActionToolsPreferenceShowMacExecute(Sender: TObject);
    procedure ButtonShowWaitingSchedulerMessagesClick(Sender: TObject);
    procedure CheckBoxAutoSendVerifyNodesChange(Sender: TObject);
    procedure CheckBoxAutoQueryInfoChange(Sender: TObject);
    procedure CheckBoxDumbNodeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LccComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientSchedulerAddOutgoingMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure LccEthernetClientSchedulerAddWaitingForReplyMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure LccEthernetClientSchedulerRemoveOutgoingMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure LccEthernetClientSchedulerRemoveWaitingForReplyMessage(Sender: TObject; LccMessage, LccMessageReply: TLccMessage);
    procedure LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServerSchedulerAddOutgoingMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure LccEthernetServerSchedulerAddWaitingForReplyMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure LccEthernetServerSchedulerRemoveOutgoingMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure LccEthernetServerSchedulerRemoveWaitingForReplyMessage(Sender: TObject; LccMessage, LccMessageReply: TLccMessage);
    procedure LccNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerLccNodeCDI(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
    procedure LccNodeManagerLccNodeConfigMemAddressSpaceInfoReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; AddressSpace: Byte);
    procedure LccNodeManagerLccNodeConfigMemOptionsReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
    procedure LccNodeManagerLccNodeConfigMemReadReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
    procedure LccNodeManagerLccNodeConfigMemWriteReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
    procedure LccNodeManagerLccNodeCreate(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerLccNodeInitializationComplete(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerLccNodeProtocolIdentifyReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
    procedure LccNodeManagerLccNodeSimpleNodeIdentReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
    procedure LccNodeManagerLccNodeVerifiedNodeID(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeSelectorFocusedChanged(Sender: TObject; FocusedNode, OldFocusedNode: TLccGuiNode);
    procedure LccNodeSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LccSettingsLoadFromFile(Sender: TObject; IniFile: TIniFile);
    procedure LccSettingsSaveToFile(Sender: TObject; IniFile: TIniFile);
    procedure PopupMenuSelectorPopup(Sender: TObject);
  private
    FAppAboutCmd: TMenuItem;
    FDumbNode: Boolean;
    FLastMouseDownInfo: TMouseInfo;
    FShownOnce: Boolean;
    FWaitingMessageList: TObjectList;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    {$ENDIF}
    { private declarations }
  protected
    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    property WaitingMessageList: TObjectList read FWaitingMessageList write FWaitingMessageList;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
    procedure TestForDuplicateAndAdd(TestNode: TLccNode);
    procedure UpdateForNodeEnabled(TestNode: TLccNode);
    function TestForAllNodesEnabled: Boolean;
    procedure UpdateNodePropertiesForm(Node: TLccNode);
    procedure SendSnipRequest(Node: TLccNode);
    procedure SendPipRequest(Node: TLccNode);
    procedure SendCdiRequest(Node: TLccNode);
    procedure SendConfigMemOptionsRequest(Node: TLccNode);
    procedure SendConfigMemAddressInfo(Node: TLccNode; AddressSpace: Byte);
    procedure FormLoggingHideNotify(Sender: TObject);
    procedure FormProperitesHideNotify(Sender: TObject);
    procedure ShowSettingsDialog;
  public
    TempSourceNode: TLccNode;


    { public declarations }
    property DumbNode: Boolean read FDumbNode write FDumbNode;
    property LastMouseDownInfo: TMouseInfo read FLastMouseDownInfo;
    property ShownOnce: Boolean read FShownOnce write FShownOnce;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ActionComPortExecute(Sender: TObject);
begin
  if ActionComPort.Checked then
  begin
    LccComPort.OpenComPortWithLccSettings;
    LccNodeManager.HardwareConnection := LccComPort;     // Connect the Node Manager to the Comport Link
  end else
  begin
     LccComPort.CloseComPort(nil);
     LccNodeManager.HardwareConnection := nil;          // DisConnect the Node Manager  Link
  end;
end;

procedure TForm1.ActionEditUserStringsExecute(Sender: TObject);
var
  LccGuiNode: TLccGuiNode;
  LccNode: TLccNode;
begin
  LccGuiNode := LccNodeSelector.ClientPtToVisibleNode(Point( LastMouseDownInfo.X, LastMouseDownInfo.Y), True);
  if Assigned(LccGuiNode) then
  begin
    LccNode := LccNodeManager.FindByGuiNode(LccGuiNode);
    if Assigned(LccNode) then
    begin
    end;
  end;
end;

procedure TForm1.ActionEthernetClientExecute(Sender: TObject);
begin
  if ActionEthernetClient.Checked then
  begin
    LccNodeManager.HardwareConnection := LccEthernetClient;     // Connect the Node Manager to the Comport Link
    LccEthernetClient.OpenEthernetConnectionWithLccSettings;
  end else
  begin
    LccNodeManager.HardwareConnection := nil;                         // DisConnect the Node Manager  Link
    LccEthernetClient.CloseEthernetConnection(nil);
  end
end;

procedure TForm1.ActionEthernetServerExecute(Sender: TObject);
begin
  if ActionEthernetServer.Checked then
  begin
    LccNodeManager.HardwareConnection := LccEthernetServer;     // Connect the Node Manager to the Comport Link
    LccEthernetServer.OpenEthernetConnectionWithLccSettings;
  end else
  begin
    LccNodeManager.HardwareConnection := nil;          // DisConnect the Node Manager  Link
    LccEthernetServer.CloseEthernetConnection(nil);
  end
end;

procedure TForm1.ActionHelpAboutShowExecute(Sender: TObject);
begin
  FormAbout.ShowModal;
end;

procedure TForm1.ActionLogWindowExecute(Sender: TObject);
begin
  if ActionLogWindow.Checked then
  begin
    FormLogging.Show;
    FormLogging.FrameLccLogging.Paused := False;
  end else
  begin
    FormLogging.Hide;
    FormLogging.FrameLccLogging.Paused := True;
  end;
end;

procedure TForm1.ActionPropertiesWindowExecute(Sender: TObject);
begin
   if ActionPropertiesWindow.Checked then
  begin
    FormNodeProperties.Show;
  end else
  begin
    FormNodeProperties.Hide;
  end;
end;

procedure TForm1.ActionToolsSettingsShowWinExecute(Sender: TObject);
begin
  ShowSettingsDialog;
end;

procedure TForm1.ButtonShowWaitingSchedulerMessagesClick(Sender: TObject);
var
  i: Integer;
  LccMessage: TLccMessage;
  Msg: string;
begin
  LccNodeManager.HardwareConnection.FillWaitingMessageList(WaitingMessageList);
  if WaitingMessageList.Count > 0 then
  begin
    Msg := '';
    for i := 0 to WaitingMessageList.Count - 1 do
    begin
      LccMessage := WaitingMessageList[i] as TLccMessage;
      Msg := MTI2String(LccMessage.MTI) + #13+#10;
    end;
    WaitingMessageList.Clear;
  end else
    Msg := 'No Message Found';
  ShowMessage(Msg);
end;

procedure TForm1.ActionShowNodePropertiesExecute(Sender: TObject);
var
  Node: TLccNode;
begin
  Node := LccNodeManager.FindByGuiNode(LccNodeSelector.FocusedNode);
  UpdateNodePropertiesForm(Node);
  if Assigned(Node) then
    FormNodeProperties.Show;
end;

procedure TForm1.ActionTCPExecute(Sender: TObject);
begin
  LccSettings.Ethernet.GridConnect := not ActionTCP.Checked;
  LccEthernetServer.Gridconnect := LccSettings.Ethernet.GridConnect;
  LccEthernetClient.Gridconnect := LccSettings.Ethernet.GridConnect;
  LccSettings.SaveToFile;
end;

procedure TForm1.ActionToolsPreferenceShowMacExecute(Sender: TObject);
begin
  ShowSettingsDialog;
end;

procedure TForm1.CheckBoxAutoSendVerifyNodesChange(Sender: TObject);
begin
  LccNodeManager.AutoSendVerifyNodesOnStart :=  CheckBoxAutoSendVerifyNodes.Checked;
  if CheckBoxAutoSendVerifyNodes.Checked then
    CheckBoxDumbNode.Checked := False;
  LccSettings.SaveToFile;
end;

procedure TForm1.CheckBoxDumbNodeChange(Sender: TObject);
begin
  DumbNode := CheckBoxDumbNode.Checked;
  if DumbNode then
  begin
    CheckBoxAutoSendVerifyNodes.Checked := False;
    CheckBoxAutoQueryInfo.Checked := False;
  end;
  LccSettings.SaveToFile;
end;

procedure TForm1.CheckBoxAutoQueryInfoChange(Sender: TObject);
begin
  LccNodeManager.AutoInterrogateDiscoveredNodes := CheckBoxAutoQueryInfo.Checked;
  if CheckBoxAutoQueryInfo.Checked then
    CheckBoxDumbNode.Checked := False;
  LccSettings.SaveToFile;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // Before shutdown clean up and disconnect from connections
  if ActionComPort.Checked then
    ActionComPort.Execute;                  // Force calling the OnExecute Event to clean up, but only if the Action is enabled
  if ActionEthernetServer.Checked then
    ActionEthernetServer.Execute;           // Force calling the OnExecute Event to clean up, but only if the Action is enabled
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WaitingMessageList := TObjectList.Create;
  WaitingMessageList.OwnsObjects := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWaitingMessageList);
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if not ShownOnce then
  begin
    {$IFDEF DARWIN}
    OSXMenu := TMenuItem.Create(Self);  {Application menu}
    OSXMenu.Caption := #$EF#$A3#$BF;  {Unicode Apple logo char}
    MainMenu.Items.Insert(0, OSXMenu);

    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    AppAboutCmd.Caption := 'About ' + BUNDLENAME;
    OSXMenu.Add(AppAboutCmd);  {Add About as item in application menu}

    OSXSep1Cmd := TMenuItem.Create(Self);
    OSXSep1Cmd.Caption := '-';
    OSXMenu.Add(OSXSep1Cmd);

    ActionToolsPreferenceShowMac.ShortCut := ShortCut(VK_OEM_COMMA, [ssMeta]);
    OSXPrefCmd := TMenuItem.Create(Self);
    OSXPrefCmd.Action := ActionToolsPreferenceShowMac;
    OSXMenu.Add(OSXPrefCmd);
    // Defaults
    ActionToolsSettingsShowWin.Visible := False;
    CheckBoxDumbNode.Checked := True;

    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}

    FormLogging.OnHideNotify := @FormLoggingHideNotify;
    FormNodeProperties.OnHideNotify := @FormProperitesHideNotify;
    LccSettings.FilePath := GetSettingsPath + 'Settings.ini';                     // Setup the file paths to the Settings Object
    LccSettings.LoadFromFile;                                                     // Read in the settings from the file to initialize the object
    FormSettings.FrameLccSettings.LccSettings := LccSettings;                    // Connect the Settings Object to the Settings UI frame
    LccComPort.LoggingFrame := FormLogging.FrameLccLogging;                       // Connect the LoggingFrame to the Connections
    LccEthernetServer.LoggingFrame := FormLogging.FrameLccLogging;
    LccEthernetClient.LoggingFrame := FormLogging.FrameLccLogging;
    LccEthernetClient.LccSettings := LccSettings;
    FormLogging.FrameLccLogging.LccSettings := LccSettings;                       // Allow Logging frame to partake in the Settings to persist logging option
    FormLogging.FrameLccLogging.SyncwithLccSettings;                              // Load the Settings into the Logging Frame
    FormLogging.FrameLccLogging.Paused := True;                                   // Start off Paused since it is hidden
    ActionTCP.Checked := not LccSettings.Ethernet.GridConnect;
    LccEthernetServer.Gridconnect := LccSettings.Ethernet.GridConnect;
    LccEthernetClient.Gridconnect := LccSettings.Ethernet.GridConnect;
    FormSettings.ClientHeight := FormSettings.FrameLccSettings.ButtonOk.Top + FormSettings.FrameLccSettings.ButtonOk.Height + 8; // Now resize the form to fit its child controls
    LccNodeManager.RootNode.Configuration.FilePath := GetSettingsPath + 'Configuration.dat';  // Set the name for the configuration file.  If this is not set the configuration will persist in a local stream object but when the application is closed it will be lost
    LccNodeManager.RootNode.Configuration.LoadFromFile;
    LccNodeManager.RootNode.CDI.LoadFromXml(GetSettingsPath + 'SampleCdi.xml');   // You must place a XML file in the Setting Folder for this to have any effect We also need to syncronize the SNIP to be the same as the <identification> section of the CDI
    LccNodeManager.RootNode.SimpleNodeInfo.LoadFromXml(GetSettingsPath + 'SampleCdi.xml');

    FormNodeProperties.ActiveNode := LccNodeManager.RootNode;
    for i := 0 to LccNodeManager.RootNode.ConfigMemAddressSpaceInfo.Count - 1 do
      FormNodeProperties.LoadConfigMemAddressSpaceInfo(LccNodeManager.RootNode.ConfigMemAddressSpaceInfo.AddressSpace[i]);
    FormNodeProperties.LoadConfigMemOptions(LccNodeManager.RootNode.ConfigMemOptions);
    FormNodeProperties.LoadProtocols(LccNodeManager.RootNode.ProtocolSupport);
    FormNodeProperties.LoadSnip(LccNodeManager.RootNode.SimpleNodeInfo);
    FormNodeProperties.LoadCdi(LccNodeManager.RootNode.CDI);
    {$IFDEF WINDOWS}
    FormLogging.FrameLccLogging.SynEdit.Font.Size := 11;
    {$ENDIF}
    ShownOnce := True;
  end;
end;

procedure TForm1.LccComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  case ComPortRec.ConnectionState of
    ccsComConnecting :
    begin
      StatusBarMain.Panels[STATUS_PANEL_COMPORT].Text := 'Connecting: ' + ComPortRec.ComPort;
      ActionTCP.Enabled := False;
      Label3.Caption := '0';
      Label4.Caption := '0';
    end;
    ccsComConnected :
    begin
      StatusBarMain.Panels[STATUS_PANEL_COMPORT].Text := 'Connected: ' + ComPortRec.ComPort;
      LccNodeSelector.LccNodes.Clear;
      LccNodeManager.Enabled := True;
    end;
    ccsComDisconnecting :
    begin
       LccNodeSelector.LccNodes.Clear;
       LccNodeManager.Enabled := False;
       StatusBarMain.Panels[STATUS_PANEL_COMPORT].Text := 'Disconnecting: ' + ComPortRec.ComPort;
    end;
    ccsComDisconnected :
    begin
       StatusBarMain.Panels[STATUS_PANEL_COMPORT].Text := 'Disconnected:';
       ActionTCP.Enabled := True;
    end;
  end;
end;

procedure TForm1.LccComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  ShowMessage('Error on ' + ComPortRec.ComPort + ' Message: ' + ComPortRec.MessageStr);
  ActionComPort.Checked := False;
end;

procedure TForm1.LccEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting :
      begin
        StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Connecting Ethernet: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
  //      ActionComPort.Enabled := False;  // Disable Comport if Ethernet is active
        ActionEthernetServer.Enabled := False;
        ActionTCP.Enabled := False;
        Label3.Caption := '0';
        Label4.Caption := '0';
      end;
    ccsClientConnected :
      begin
        StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Connected To: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        LccNodeSelector.LccNodes.Clear;
        LccNodeManager.Enabled := True;   ;
      end;
    ccsClientDisconnecting :
      begin
         LccNodeSelector.LccNodes.Clear;
         LccNodeManager.Enabled := False;
         StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Disconnecting Ethernet: '+ EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
      end;
    ccsClientDisconnected :
      begin
         StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Disconnected';
         ActionEthernetClient.Checked := False;
    //     ActionComPort.Enabled := True;        // Reinable Comport
         ActionEthernetServer.Enabled := True;
         ActionTCP.Enabled := True;
      end;
  end;
end;

procedure TForm1.LccEthernetClientErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('Error on ' + EthernetRec.ClientIP + ' Message: ' + EthernetRec.MessageStr);
  ActionEthernetClient.Checked := False;
end;

procedure TForm1.LccEthernetClientSchedulerAddOutgoingMessage(Sender: TObject;LccMessage: TLccMessage);
begin
  Label3.Caption := IntToStr( StrToInt(Label3.Caption) + 1);
end;

procedure TForm1.LccEthernetClientSchedulerAddWaitingForReplyMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  Label4.Caption := IntToStr( StrToInt(Label4.Caption) + 1);
end;

procedure TForm1.LccEthernetClientSchedulerRemoveOutgoingMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  Label3.Caption := IntToStr( StrToInt(Label3.Caption) - 1);
end;

procedure TForm1.LccEthernetClientSchedulerRemoveWaitingForReplyMessage(Sender: TObject; LccMessage, LccMessageReply: TLccMessage);
begin
  Label4.Caption := IntToStr( StrToInt(Label4.Caption) - 1);
end;

procedure TForm1.LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
  i: Integer;
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Connecting Ethernet: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
  //      ActionComPort.Enabled := False;  // Disable Comport if Ethernet is active
        ActionEthernetClient.Enabled := False;
        ActionTCP.Enabled := False;
        Label3.Caption := '0';
        Label4.Caption := '0';
      end;
    ccsListenerConnected :
      begin
        StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Listening: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
        LccNodeSelector.LccNodes.Clear;
        LccNodeManager.Enabled := True;
      end;
    ccsListenerDisconnecting :
      begin
         LccNodeSelector.LccNodes.Clear;
         LccNodeManager.Enabled := False;
         StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Disconnecting Ethernet: '+ EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsListenerDisconnected :
      begin
         StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Disconnected';
         ActionEthernetServer.Checked := False;
  //       ActionComPort.Enabled := True;        // Reinable Comport
         ActionEthernetClient.Enabled := True;
         ActionTCP.Enabled := True;
      end;
    ccsListenerClientConnecting :
      begin
      end;
    ccsListenerClientConnected :
      begin
        ListItem := ListViewServerConnections.Items.Add;
        ListItem.Caption := EthernetRec.ClientIP;
        ListItem.SubItems.Add(IntToStr(EthernetRec.ClientPort));
        ListItem.SubItems.Add(EthernetRec.ListenerIP);
        ListItem.SubItems.Add(IntToStr(EthernetRec.ListenerPort));
      end;
    ccsListenerClientDisconnecting :
      begin
      end;
    ccsListenerClientDisconnected :
      begin
        for i := 0 to ListViewServerConnections.Items.Count - 1 do
        begin
          ListItem := ListViewServerConnections.Items[i];
          if ListItem.Caption = EthernetRec.ClientIP then
            if ListITem.SubItems[0] = IntToStr(EthernetRec.ClientPort) then
            begin
              ListViewServerConnections.Items.Delete(i);
              Break;
            end;
        end;
      end;
  end;
end;

procedure TForm1.LccEthernetServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('Error on ' + EthernetRec.ListenerIP + ' Message: ' + EthernetRec.MessageStr);
  ActionEthernetServer.Checked := False;
end;

procedure TForm1.LccEthernetServerSchedulerAddOutgoingMessage(Sender: TObject; LccMessage: TLccMessage);
begin
   Label3.Caption := IntToStr( StrToInt(Label3.Caption) + 1);
end;

procedure TForm1.LccEthernetServerSchedulerAddWaitingForReplyMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  Label4.Caption := IntToStr( StrToInt(Label4.Caption) + 1);
end;

procedure TForm1.LccEthernetServerSchedulerRemoveOutgoingMessage(Sender: TObject; LccMessage: TLccMessage);
begin
   Label3.Caption := IntToStr( StrToInt(Label3.Caption) - 1);
end;

procedure TForm1.LccEthernetServerSchedulerRemoveWaitingForReplyMessage(Sender: TObject; LccMessage, LccMessageReply: TLccMessage);
begin
  Label4.Caption := IntToStr( StrToInt(Label4.Caption) - 1);
end;

procedure TForm1.LccNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccNodeManager.Enabled and not ActionTCP.Checked then
  begin
    if LccSourceNode = LccNodeManager.RootNode then
      StatusBarMain.Panels[STATUS_PANEL_NODEID].Text := LccSourceNode.NodeIDStr + ': 0x' + IntToHex(LccSourceNode.AliasID, 4);
  end;
end;

procedure TForm1.LccNodeManagerLccNodeCDI(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
begin
  LccSourceNode.UserMsgInFlight := LccSourceNode.UserMsgInFlight - [mif_Cdi];
  UpdateForNodeEnabled(LccSourceNode);
end;

procedure TForm1.LccNodeManagerLccNodeConfigMemAddressSpaceInfoReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode; AddressSpace: Byte);
begin
  FormNodeProperties.LoadConfigMemAddressSpaceInfo(LccSourceNode.ConfigMemAddressSpaceInfo.FindByAddressSpace(AddressSpace));
end;

procedure TForm1.LccNodeManagerLccNodeConfigMemOptionsReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
var
  i: Integer;
begin
  LccSourceNode.UserMsgInFlight := LccSourceNode.UserMsgInFlight - [mif_ConfigMemOptions];
  if LccSourceNode.ConfigMemOptions.Valid then
  begin
    for i := LccSourceNode.ConfigMemOptions.LowSpace to LccSourceNode.ConfigMemOptions.HighSpace do
      SendConfigMemAddressInfo(LccSourceNode, i);
  end;
  UpdateNodePropertiesForm(LccSourceNode);
end;

procedure TForm1.LccNodeManagerLccNodeConfigMemReadReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
begin
  FormNodeProperties.LccCdiParser.DoConfigMemReadReply(LccSourceNode);
end;

procedure TForm1.LccNodeManagerLccNodeConfigMemWriteReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
begin
  FormNodeProperties.LccCdiParser.DoConfigMemReadReply(LccSourceNode);
end;

procedure TForm1.LccNodeManagerLccNodeCreate(Sender: TObject; LccSourceNode: TLccNode);
var
  EventID: TEventID;
  i: Integer;
  EventOffset: Word;
begin
  if LccSourceNode = LccNodeManager.RootNode then
  begin
    EventOffset := 0;
    for i := 0 to 9 do
    begin
      NodeIDToEventID(LccSourceNode.NodeID, EventOffset,  EventID);
      LccSourceNode.EventsConsumed.Add(EventID, evs_Unknown);
      Inc(EventOffset);
    end;
    for i := 0 to 9 do
    begin
      NodeIDToEventID(LccSourceNode.NodeID, EventOffset,  EventID);
      LccSourceNode.EventsConsumed.Add(EventID, evs_Unknown);
      Inc(EventOffset);
    end;
  end;
end;

procedure TForm1.LccNodeManagerLccNodeInitializationComplete(Sender: TObject; LccSourceNode: TLccNode);
begin
  TestForDuplicateAndAdd(LccSourceNode);
end;

procedure TForm1.LccNodeManagerLccNodeProtocolIdentifyReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
begin
  LccSourceNode.UserMsgInFlight := LccSourceNode.UserMsgInFlight - [mif_Pip];
  if LccSourceNode.ProtocolSupport.SimpleNodeInfo then
    SendSnipRequest(LccSourceNode);

  TempSourceNode := LccSourceNode;
  if LccSourceNode.ProtocolSupport.CDI then
    SendCdiRequest(LccSourceNode);
  UpdateForNodeEnabled(LccSourceNode);
end;

procedure TForm1.LccNodeManagerLccNodeSimpleNodeIdentReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
begin
  LccSourceNode.UserMsgInFlight := LccSourceNode.UserMsgInFlight - [mif_Snip];
  if Assigned(LccSourceNode.LccGuiNode) then
  begin
    LccSourceNode.LccGuiNode.Captions.Clear;
    LccSourceNode.LccGuiNode.Captions.Add(LccSourceNode.SimpleNodeInfo.Manufacturer);
    LccSourceNode.LccGuiNode.Captions.Add('Model: ' + LccSourceNode.SimpleNodeInfo.Model);
    LccSourceNode.LccGuiNode.Captions.Add('Software Ver: ' + LccSourceNode.SimpleNodeInfo.SoftwareVersion);
    LccSourceNode.LccGuiNode.Captions.Add('Hardware Ver: ' + LccSourceNode.SimpleNodeInfo.HardwareVersion);
    LccSourceNode.LccGuiNode.Captions.Add('User Name: ' + LccSourceNode.SimpleNodeInfo.UserName);
    LccSourceNode.LccGuiNode.Captions.Add('User Desc: ' + LccSourceNode.SimpleNodeInfo.UserDescription);
    LccSourceNode.LccGuiNode.Invalidate(False);
  end;
  UpdateForNodeEnabled(LccSourceNode);
end;

procedure TForm1.LccNodeManagerLccNodeVerifiedNodeID(Sender: TObject; LccSourceNode: TLccNode);
begin
  TestForDuplicateAndAdd(LccSourceNode);
end;

procedure TForm1.LccNodeManagerNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccNodeManager.Enabled then
  begin
     if LccSourceNode = LccNodeManager.RootNode then
     begin
       if ActionTCP.Checked then
         StatusBarMain.Panels[STATUS_PANEL_NODEID].Text := LccSourceNode.NodeIDStr
       else
         StatusBarMain.Panels[STATUS_PANEL_NODEID].Text := LccSourceNode.NodeIDStr + ': 0x' + IntToHex(LccSourceNode.AliasID, 4);
     end;
  end;
end;

procedure TForm1.LccNodeSelectorFocusedChanged(Sender: TObject; FocusedNode, OldFocusedNode: TLccGuiNode);
var
  Node: TLccNode;
begin
  Node := LccNodeManager.FindByGuiNode(FocusedNode);
  if Assigned(Node) then
  begin
  end;
  FormNodeProperties.ActiveNode := Node;
  if FormNodeProperties.Visible then
    UpdateNodePropertiesForm(Node);
end;

procedure TForm1.LccNodeSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastMouseDownInfo.Button := Button;
  FLastMouseDownInfo.Shift := Shift;
  FLastMouseDownInfo.X := X;
  FLastMouseDownInfo.Y := Y;
end;

procedure TForm1.LccSettingsLoadFromFile(Sender: TObject; IniFile: TIniFile);
begin
  CheckBoxAutoQueryInfo.Checked := IniFile.ReadBool('Custom Settings', 'CheckBoxAutoQueryInfo', False);
  CheckBoxAutoSendVerifyNodes.Checked := IniFile.ReadBool('Custom Settings', 'CheckBoxAutoSendVerifyNodes', False);
  ActionEthernetServer.Visible := IniFile.ReadBool('Custom Settings', 'EthernetServer', True);
  ActionEthernetClient.Visible := IniFile.ReadBool('Custom Settings', 'EthernetClient', False);
  ActionComPort.Visible := IniFile.ReadBool('Custom Settings', 'ComPort', False);

  ActionTCP.Visible := ActionEthernetClient.Visible or ActionEthernetServer.Visible;
  MenuItemConnectionDivider0.Visible := ActionTCP.Visible;
end;

procedure TForm1.LccSettingsSaveToFile(Sender: TObject; IniFile: TIniFile);
begin
  IniFile.WriteBool('Custom Settings', 'CheckBoxAutoQueryInfo', CheckBoxAutoQueryInfo.Checked);
  IniFile.WriteBool('Custom Settings', 'CheckBoxAutoSendVerifyNodes', CheckBoxAutoSendVerifyNodes.Checked);
  IniFile.WriteBool('Custom Settings', 'EthernetServer', ActionEthernetServer.Visible);
  IniFile.WriteBool('Custom Settings', 'EthernetClient', ActionEthernetClient.Visible);
  IniFile.WriteBool('Custom Settings', 'ComPort', ActionComPort.Visible);
end;

procedure TForm1.PopupMenuSelectorPopup(Sender: TObject);
var
  LccGuiNode: TLccGuiNode;
  LccNode: TLccNode;
begin
  ActionEditUserStrings.Enabled := False;
  LccGuiNode := LccNodeSelector.ClientPtToVisibleNode(Point( LastMouseDownInfo.X, LastMouseDownInfo.Y), True);
  if Assigned(LccGuiNode) then
  begin
    LccNode := LccNodeManager.FindByGuiNode(LccGuiNode);
    if Assigned(LccNode) then
      ActionEditUserStrings.Enabled := LccGuiNode.Enabled and LccNode.ProtocolSupport.CDI
  end;
end;

procedure TForm1.TestForDuplicateAndAdd(TestNode: TLccNode);
begin
  if DumbNode then Exit;
  if not Assigned(LccNodeSelector.LccNodes.Find(TestNode.NodeID)) then
  begin
    LccNodeSelector.BeginUpdate;
    try
      TestNode.LccGuiNode := LccNodeSelector.LccNodes.Add(TestNode.NodeID, TestNode.AliasID);
      TestNode.LccGuiNode.Captions.Clear;
      TestNode.LccGuiNode.Captions.Add('NodeID: ' + TestNode.NodeIDStr);
      TestNode.LccGuiNode.Captions.Add('AliasID: ' + TestNode.AliasIDStr);
      TestNode.LccGuiNode.ImageIndex := 0;
    finally
      LccNodeSelector.EndUpdate;
    end;
  end;
  TestNode.LccGuiNode.Captions.Add('Loading Node Info...');
  SendPipRequest(TestNode);
  UpdateForNodeEnabled(TestNode);
end;

procedure TForm1.UpdateForNodeEnabled(TestNode: TLccNode);
begin
  if Assigned(TestNode.LccGuiNode) then
  begin
    TestNode.LccGuiNode.Enabled := TestNode.UserMsgInFlight = [];
  end;
end;

function TForm1.TestForAllNodesEnabled: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to LccNodeSelector.LccNodes.Count - 1 do
  begin
    if not LccNodeSelector.LccNodes[i].Enabled then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TForm1.UpdateNodePropertiesForm(Node: TLccNode);
var
  i: Integer;
begin
  FormNodeProperties.ActiveNode := Node;
  if Assigned(Node) then
  begin
    if not FormNodeProperties.LoadConfigMemOptions(Node.ConfigMemOptions) then
      SendConfigMemOptionsRequest(Node);  // When this replies we send the GetAddressSpaceInfo message
    if not FormNodeProperties.LoadProtocols(Node.ProtocolSupport) then
      SendPipRequest(Node);
    if not FormNodeProperties.LoadCdi(Node.CDI) then
      SendCdiRequest(Node);
    if not FormNodeProperties.LoadSnip(Node.SimpleNodeInfo) then
      SendSnipRequest(Node);
    for i := 0 to Node.ConfigMemAddressSpaceInfo.Count - 1 do
      FormNodeProperties.LoadConfigMemAddressSpaceInfo(Node.ConfigMemAddressSpaceInfo[i]);
  end
end;

procedure TForm1.SendSnipRequest(Node: TLccNode);
begin
  if DumbNode then Exit;
  if Node.UserMsgInFlight * [mif_Snip] = [] then
  begin
    LccNodeManager.UserMessage.LoadSimpleNodeIdentInfoRequest(LccNodeManager.RootNode.NodeID, LccNodeManager.RootNode.AliasID, Node.NodeID, Node.AliasID);
    LccNodeManager.HardwareConnection.SendMessage(LccNodeManager.UserMessage);
    Node.UserMsgInFlight := Node.UserMsgInFlight + [mif_Snip];
  end;
end;

procedure TForm1.ShowSettingsDialog;
begin
  FormSettings.FrameLccSettings.UserSettings.EthernetClient := ActionEthernetClient.Visible;
  FormSettings.FrameLccSettings.UserSettings.ComPort := ActionComPort.Visible;
  FormSettings.FrameLccSettings.UserSettings.EthernetServer := ActionEthernetServer.Visible;
  // Update from video series, need to resync with the Settings each time the
  // dialog is shown as the user may have changed the UI and hit cancel and not
  // just when the program starts up in the FormShow event
  FormSettings.FrameLccSettings.SyncWithLccSettings;
  if FormSettings.ShowModal = mrOK then
  begin

  end;
end;

procedure TForm1.SendPipRequest(Node: TLccNode);
begin
  if DumbNode then Exit;
  if Node.UserMsgInFlight * [mif_Pip] = [] then
  begin
    LccNodeManager.UserMessage.LoadProtocolIdentifyInquiry(LccNodeManager.RootNode.NodeID, LccNodeManager.RootNode.AliasID, Node.NodeID, Node.AliasID);
    LccNodeManager.HardwareConnection.SendMessage(LccNodeManager.UserMessage);
    Node.UserMsgInFlight := Node.UserMsgInFlight + [mif_Pip];
  end;
end;

procedure TForm1.SendCdiRequest(Node: TLccNode);
begin
  if DumbNode then Exit;
  if Node.UserMsgInFlight * [mif_Cdi] = [] then
  begin
    LccNodeManager.UserMessage.LoadCDIRequest(LccNodeManager.RootNode.NodeID, LccNodeManager.RootNode.AliasID, Node.NodeID, Node.AliasID);
    LccNodeManager.HardwareConnection.SendMessage(LccNodeManager.UserMessage);
    Node.UserMsgInFlight := Node.UserMsgInFlight + [mif_Cdi];
  end;
end;

procedure TForm1.SendConfigMemOptionsRequest(Node: TLccNode);
begin
  if DumbNode then Exit;
  if Node.UserMsgInFlight * [mif_ConfigMemOptions] = [] then
  begin
    LccNodeManager.UserMessage.LoadConfigMemOptions(LccNodeManager.RootNode.NodeID, LccNodeManager.RootNode.AliasID, Node.NodeID, Node.AliasID);
    LccNodeManager.HardwareConnection.SendMessage(LccNodeManager.UserMessage);
    Node.UserMsgInFlight := Node.UserMsgInFlight + [mif_ConfigMemOptions];
  end;
end;

procedure TForm1.SendConfigMemAddressInfo(Node: TLccNode; AddressSpace: Byte);
begin
  if DumbNode then Exit;
  LccNodeManager.UserMessage.LoadConfigMemAddressSpaceInfo(LccNodeManager.RootNode.NodeID, LccNodeManager.RootNode.AliasID, Node.NodeID, Node.AliasID, AddressSpace);
  LccNodeManager.HardwareConnection.SendMessage(LccNodeManager.UserMessage);
//  Node.UserMsgInFlight := Node.UserMsgInFlight + [mif_ConfigMemOptions];
end;

procedure TForm1.FormLoggingHideNotify(Sender: TObject);
begin
  if ActionLogWindow.Checked = True then
    ActionLogWindow.Execute;
end;

procedure TForm1.FormProperitesHideNotify(Sender: TObject);
begin
  if ActionPropertiesWindow.Checked = True then
    ActionPropertiesWindow.Execute;
end;

end.

