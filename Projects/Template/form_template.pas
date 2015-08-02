unit form_template;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, Menus, LCLType, StdCtrls, ExtCtrls, lcc_app_common_settings,
  lcc_comport, lcc_ethernetclient, lcc_nodemanager, lcc_ethenetserver,
  form_settings, file_utilities, lcc_messages, form_logging, lcc_defines,
  lcc_utilities, frame_lcc_logging;

const
  BUNDLENAME             = 'LCCAppTemplate';
  PATH_LINUX_APP_FOLDER  = 'lccapptemplate/';
  PATH_SETTINGS_FILE     = 'settings.ini';

type

  { TMyRootNode }

  TMyRootNode = class(TLccOwnedNode)
  public
    constructor Create(AnOwner: TComponent); override;
  end;

  { TFormTemplate }

  TFormTemplate = class(TForm)
    ActionLccLogin: TAction;
    ActionToolsSettingsShowWin: TAction;
    ActionToolsPreferenceShowMac: TAction;
    ActionHelpAboutShow: TAction;
    ActionMsgTrace: TAction;
    ActionComPortConnect: TAction;
    ActionEthenetClientConnect: TAction;
    ActionEthernetServerConnect: TAction;
    ActionList: TActionList;
    ImageListToolbar: TImageList;
    LabelServerConnections: TLabel;
    LccComPort: TLccComPort;
    LccEthernetClient: TLccEthernetClient;
    LccEthernetServer: TLccEthernetServer;
    LccNodeManager: TLccNodeManager;
    LccSettings: TLccSettings;
    ListViewServerConnections: TListView;
    MainMenu: TMainMenu;
    MenuItemToolsSettings: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemHelp: TMenuItem;
    Panel2: TPanel;
    PanelAddOns: TPanel;
    PanelAppSpace: TPanel;
    SplitterApp: TSplitter;
    StatusBarMain: TStatusBar;
    ToolBarMain: TToolBar;
    ToolButtonLccLogIn: TToolButton;
    ToolButtonSeparator: TToolButton;
    ToolButtonMsgTrace: TToolButton;
    ToolButtonComPort: TToolButton;
    ToolButtonEthernetClient: TToolButton;
    ToolButtonEthernetServer: TToolButton;
    procedure ActionComPortConnectExecute(Sender: TObject);
    procedure ActionEthenetClientConnectExecute(Sender: TObject);
    procedure ActionEthernetServerConnectExecute(Sender: TObject);
    procedure ActionLccLoginExecute(Sender: TObject);
    procedure ActionMsgTraceExecute(Sender: TObject);
    procedure ActionToolsPreferenceShowMacExecute(Sender: TObject);
    procedure ActionToolsSettingsShowWinExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure LccComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerLccGetRootNodeClass(Sender: TObject; var NodeClass: TLccOwnedNodeClass);
    procedure LccNodeManagerNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
  private
    FAppAboutCmd: TMenuItem;
    FShownOnce: Boolean;
    {$IFDEF DARWIN}
    FOSXMenu: TMenuItem;
    FOSXSep1Cmd: TMenuItem;
    FOSXPrefCmd: TMenuItem;
    {$ENDIF}
  private
    { private declarations }
    property AppAboutCmd: TMenuItem read FAppAboutCmd write FAppAboutCmd;
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
     property ShownOnce: Boolean read FShownOnce write FShownOnce;
  protected
    procedure OnTraceFormHideEvent(Sender: TObject);
  public
    { public declarations }
  end;

var
  FormTemplate: TFormTemplate;

implementation

{$R *.lfm}

{ TMyRootNode }

constructor TMyRootNode.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  // Define what Protcol this Node Supports

  // Common Protocols
  // We support CDI so we must support datagrams
  ProtocolSupport.Datagram := True;
  // We support CDI so we must support datagrams
  ProtocolSupport.MemConfig := True;
  // We Support CDI
  ProtocolSupport.CDI := True;
  // We support Events
  ProtocolSupport.EventExchange := True;
  // We Support SNIP
  ProtocolSupport.SimpleNodeInfo := True;

  // Setup the SNIP constants, this information MUST be idential to the information
  // in the  <identification> tag of the CDI to comply with the LCC specs
  SimpleNodeInfo.Version := 1;
  SimpleNodeInfo.Manufacturer := 'Mustangpeak';
  SimpleNodeInfo.Model := 'SW100';
  SimpleNodeInfo.SoftwareVersion := '1.0.0.0';
  SimpleNodeInfo.HardwareVersion := '1.0.0.0';
  SimpleNodeInfo.UserVersion := 1;
  SimpleNodeInfo.UserDescription := '';
  SimpleNodeInfo.UserName := '';

  // Load a CDI XML file from the same folder that the Setting.ini is stored
//  CDI.AStream.LoadFromFile( GetSettingsPath + 'TemplateCDI.xml');
//  CDI.Valid := True;

end;

{ TFormTemplate }

procedure TFormTemplate.ActionComPortConnectExecute(Sender: TObject);
begin
  if ActionComPortConnect.Checked then
  begin
    ActionEthenetClientConnect.Checked := False;
    ActionEthernetServerConnect.Checked := False;
    ActionEthenetClientConnect.Enabled := False;
    ActionEthernetServerConnect.Enabled := False;
    LccComPort.OpenComPortWithLccSettings;
  end else
  begin
    LccComPort.CloseComPort(nil);
    ActionEthenetClientConnect.Enabled := True;
    ActionEthernetServerConnect.Enabled := True;
    ActionComPortConnect.Enabled := True;
  end;
end;

procedure TFormTemplate.ActionEthenetClientConnectExecute(Sender: TObject);
begin
  if ActionEthenetClientConnect.Checked then
  begin
    ActionEthernetServerConnect.Checked := False;
    ActionComPortConnect.Checked := False;
    ActionComPortConnect.Enabled := False;
    ActionEthernetServerConnect.Enabled := False;
    LccEthernetClient.OpenEthernetConnectionWithLccSettings;
  end else
  begin
    LccEthernetClient.CloseEthernetConnection(nil);
    ActionEthenetClientConnect.Enabled := True;
    ActionEthernetServerConnect.Enabled := True;
    ActionComPortConnect.Enabled := True;
  end;
end;

procedure TFormTemplate.ActionEthernetServerConnectExecute(Sender: TObject);
begin
  if ActionEthernetServerConnect.Checked then
  begin
    ActionEthenetClientConnect.Checked := False;
    ActionComPortConnect.Checked := False;
    ActionEthenetClientConnect.Enabled := False;
    ActionComPortConnect.Enabled := False;
    LccEthernetServer.OpenEthernetConnectionWithLccSettings;
  end else
  begin
    LccEthernetServer.CloseEthernetConnection(nil);
    ActionEthenetClientConnect.Enabled := True;
    ActionEthernetServerConnect.Enabled := True;
    ActionComPortConnect.Enabled := True;
  end;
end;

procedure TFormTemplate.ActionLccLoginExecute(Sender: TObject);
begin
  LccNodeManager.Enabled := ActionLccLogin.Checked;
  if not LccNodeManager.Enabled then
    StatusBarMain.Panels[1].Text := 'Disconnected';
end;

procedure TFormTemplate.ActionMsgTraceExecute(Sender: TObject);
begin
  if ActionMsgTrace.Checked then
    FormLogging.Show
  else
    FormLogging.Hide
end;

procedure TFormTemplate.ActionToolsPreferenceShowMacExecute(Sender: TObject);
begin
  if FormSettings.ShowModal = mrOK then
    FormSettings.FrameLccSettings.StoreSettings;
end;

procedure TFormTemplate.ActionToolsSettingsShowWinExecute(Sender: TObject);
begin
  if FormSettings.ShowModal = mrOK then
    FormSettings.FrameLccSettings.StoreSettings;
end;

procedure TFormTemplate.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ActionComPortConnect.Checked then
    ActionComPortConnect.Execute;
  if ActionEthenetClientConnect.Checked then
    ActionEthenetClientConnect.Execute;
  if ActionEthernetServerConnect.Checked then
    ActionEthernetServerConnect.Execute;
end;

procedure TFormTemplate.FormShow(Sender: TObject);
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
    ActionToolsSettingsShowWin.Visible := False;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}
    {$IFDEF Linux}
    if LccSettings.FilePath = '' then
      LccSettings.FilePath := GetSettingsPath + {PATH_LINUX_APP_FOLDER + }PATH_SETTINGS_FILE;
    {$ELSE}
    if LccSettings.FilePath = '' then
      LccSettings.FilePath := GetSettingsPath + PATH_SETTINGS_FILE;
    {$ENDIF}
    if FileExists(LccSettings.FilePath) then
      LccSettings.LoadFromFile;
    FormLogging.OnHideNotifyEvent := @OnTraceFormHideEvent;
    FormSettings.FrameLccSettings.LccSettings := LccSettings;
    ActionLccLogin.Enabled := False;
    PanelAddOns.Visible := False;
    ShownOnce := True;
  end;
end;

procedure TFormTemplate.LccComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  case ComPortRec.ConnectionState of
    ccsComConnecting :
      begin
        StatusBarMain.Panels[0].Text := 'Connecting to ' + ComPortRec.ComPort;
      end;
    ccsComConnected  :
      begin
        ActionLccLogin.Enabled := True;
        StatusBarMain.Panels[0].Text := 'Connected to ' + ComPortRec.ComPort;
      end;
    ccsComDisconnecting :
      begin
        StatusBarMain.Panels[0].Text := 'Disconnecting from ' + ComPortRec.ComPort;
      end;
    ccsComDisconnected  :
      begin
        if ActionLccLogIn.Checked then
        ActionLccLogin.Execute;
        ActionLccLogin.Enabled := False;
        StatusBarMain.Panels[0].Text := 'Disconnected';
        if ActionComPortConnect.Checked then
          ActionComPortConnect.Execute;
      end;
  end;
end;

procedure TFormTemplate.LccComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  ShowMessage('Error Connecting to ComPort: ' + ComPortRec.ComPort + ': ' + ComPortRec.MessageStr);
end;

procedure TFormTemplate.LccEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
  ccsClientConnecting :
    begin
      StatusBarMain.Panels[0].Text := 'Connecting to ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    end;
  ccsClientConnected  :
    begin
      ActionLccLogin.Enabled := True;
      StatusBarMain.Panels[0].Text := 'Connected to ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    end;
  ccsClientDisconnecting :
    begin
      StatusBarMain.Panels[0].Text := 'Disconnecting from ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    end;
  ccsClientDisconnected  :
    begin
      if ActionLccLogIn.Checked then
        ActionLccLogin.Execute;
      ActionLccLogin.Enabled := False;
      StatusBarMain.Panels[0].Text := 'Disconnected';
      if ActionEthenetClientConnect.Checked then
        ActionEthenetClientConnect.Execute;
    end;
  end;
end;

procedure TFormTemplate.LccEthernetClientErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('Error Connecting to Server: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort) + ': ' + EthernetRec.MessageStr);
end;

procedure TFormTemplate.LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  ListItem: TListItem;
  i: Integer;
begin
  case EthernetRec.ConnectionState of
  ccsListenerConnecting :
    begin
      StatusBarMain.Panels[0].Text := 'Starting Server ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    end;
  ccsListenerConnected  :
    begin
      ActionLccLogin.Enabled := True;
      PanelAddOns.Visible := True;
      StatusBarMain.Panels[0].Text := 'Server started ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    end;
  ccsListenerDisconnecting :
    begin
      StatusBarMain.Panels[0].Text := 'Stopping Server from ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    end;
  ccsListenerDisconnected  :
    begin
      if ActionLccLogIn.Checked then
        ActionLccLogin.Execute;
      ActionLccLogin.Enabled := False;
      PanelAddOns.Visible := False;
      StatusBarMain.Panels[0].Text := 'Disconnected';
      if ActionEthenetClientConnect.Checked then
        ActionEthenetClientConnect.Execute;
    end;
  ccsListenerClientConnected :
    begin
      ListItem := ListViewServerConnections.Items.Add;
      ListItem.Caption := EthernetRec.ClientIP;
      ListItem.SubItems.Add(IntToStr(EthernetRec.ClientPort));
      ListItem.SubItems.Add(EthernetRec.ListenerIP);
      ListItem.SubItems.Add(IntToStr(EthernetRec.ListenerPort));
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

procedure TFormTemplate.LccNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
var
  Temp: TNodeID;
begin
  if LccSourceNode is TMyRootNode then                                          // If it is our Root Node then save the AliasID
  begin
    if LccSettings.General.AliasIDAsVal <> LccSourceNode.AliasID then
    begin
      LccSettings.General.AliasID := '0x'+ IntToHex(LccSourceNode.AliasID, 4);
      FormSettings.FrameLccSettings.StoreSettings;
    end;
    StatusBarMain.Panels[1].Text := '0x' +IntToHex(LccSourceNode.NodeID[1], 3) + IntToHex(LccSourceNode.NodeID[0], 3) + ': 0x' + IntToHex(LccSourceNode.AliasID, 4)
  end;
end;

procedure TFormTemplate.LccNodeManagerLccGetRootNodeClass(Sender: TObject; var NodeClass: TLccOwnedNodeClass);
begin
  NodeClass := TMyRootNode;
end;

procedure TFormTemplate.LccNodeManagerNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
var
  Temp: TNodeID;
  TempID, TempID1, TempID2: QWord;
begin
  if LccSourceNode is TMyRootNode then                                          // If it is our Root Node then set its NodeID we saved
  begin
    LccSettings.General.NodeIDAsTNodeID(Temp);
    if not EqualNodeID(Temp, LccSourceNode.NodeID, True) then
    begin
       TempID1 := QWord( LccSourceNode.NodeID[0]);
       TempID2 := QWord(LccSourceNode.NodeID[1]);
       TempID2 := TempID2 shl 24;
       TempID := TempID1 or TempID2;
       LccSettings.General.NodeID := '0x'+IntToHex(TempID, 12);
       FormSettings.FrameLccSettings.StoreSettings
    end;
    StatusBarMain.Panels[1].Text := '0x' +IntToHex(LccSourceNode.NodeID[1], 3) + IntToHex(LccSourceNode.NodeID[0], 3) + ': 0x' + IntToHex(LccSourceNode.AliasID, 4)
  end;
end;

procedure TFormTemplate.LccNodeManagerRequestMessageSend(Sender: TObject; LccMessage: TLccMessage);
begin
  // The NodeManager wants to send a message, depending on who is active send the message
  // down that wire
  if ActionComPortConnect.Checked then
    LccComPort.SendMessage(LccMessage)
  else
  if ActionEthenetClientConnect.Checked then
    LccEthernetClient.SendMessage(LccMessage)
  else
  if ActionEthernetServerConnect.Checked then
    LccEthernetServer.SendMessage(LccMessage);
end;

procedure TFormTemplate.OnTraceFormHideEvent(Sender: TObject);
begin
  if ActionMsgTrace.Visible then
    ActionMsgTrace.Execute;
end;

end.

