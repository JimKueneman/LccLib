unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, ExtCtrls, Menus, StdCtrls, Spin, lcc_app_common_settings,
  lcc_comport, lcc_nodemanager, form_settings, file_utilities,
  frame_lcc_logging, lcc_messages, lcc_ethenetserver, lcc_ethernetclient,
  form_logging, lcc_nodeselector, lcc_cdi_parser, lcc_defines, contnrs,
  form_properties, IniFiles, form_about, LCLType, types,
  lcc_utilities, lcc_raspberrypi_spiport;

const
    BUNDLENAME  = 'Raspberry Pi Hub';

    STATUS_PANEL_ETHERNET = 0;
    STATUS_PANEL_COMPORT  = 1;
    STATUS_PANEL_PISPIPORT = 2;
    STATUS_PANEL_NODEID   = 3;


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
    ActionRaspberryPi: TAction;
    ActionToolsPreferenceShowMac: TAction;
    ActionHelpAboutShow: TAction;
    ActionEthernetClient: TAction;
    ActionTCP: TAction;
    ActionLogWindow: TAction;
    ActionEthernetServer: TAction;
    ActionComPort: TAction;
    ActionToolsSettingsShowWin: TAction;
    ActionList: TActionList;
    ImageListMain: TImageList;
    LabelServerConnections: TLabel;
    LccComPort: TLccComPort;
    LccEthernetClient: TLccEthernetClient;
    LccEthernetServer: TLccEthernetServer;
    LccNodeSelectorProducer1: TLccNodeSelector;
    LccRaspberryPiSpiPort: TLccRaspberryPiSpiPort;
    LccSettings: TLccSettings;
    ListViewServerConnections: TListView;
    MainMenu: TMainMenu;
    MenuItemConnectonRPiSPI: TMenuItem;
    MenuItemConnectionDivider0: TMenuItem;
    MenuItemConnectionUseTCP: TMenuItem;
    MenuItemConnectionEthernetServer: TMenuItem;
    MenuItemConnectionEthernetClient: TMenuItem;
    MenuItemConnectionComport: TMenuItem;
    MenuItemConnection: TMenuItem;
    MenuItemToolsLogWindow: TMenuItem;
    MenuItemToolsSettings: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemHelp: TMenuItem;
    PanelAddOns: TPanel;
    StatusBarMain: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButtonSettings: TToolButton;
    ToolButtonEthClient: TToolButton;
    ToolButtonSpace1: TToolButton;
    ToolButtonComPort: TToolButton;
    ToolButtonLogWindow: TToolButton;
    ToolButtonEthServer: TToolButton;
    ToolButtonEthUseTCP: TToolButton;
    ToolButtonSpace0: TToolButton;
    procedure ActionComPortExecute(Sender: TObject);
    procedure ActionEthernetClientExecute(Sender: TObject);
    procedure ActionEthernetServerExecute(Sender: TObject);
    procedure ActionHelpAboutShowExecute(Sender: TObject);
    procedure ActionLogWindowExecute(Sender: TObject);
    procedure ActionRaspberryPiExecute(Sender: TObject);
    procedure ActionToolsSettingsShowWinExecute(Sender: TObject);
    procedure ActionTCPExecute(Sender: TObject);
    procedure ActionToolsPreferenceShowMacExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LccComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccComPortReceiveMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientSchedulerClass(Sender: TObject; var SchedulerClass: TSchedulerBaseClass);
    procedure LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServerReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccRaspberryPiSpiPortConnectionStateChange(Sender: TObject; PiSpiPortRec: TLccRaspberryPiSpiPortRec);
    procedure LccRaspberryPiSpiPortReceiveMessage(Sender: TObject; PiSpiPortRec: TLccRaspberryPiSpiPortRec);
    procedure LccSettingsLoadFromFile(Sender: TObject; IniFile: TIniFile);
    procedure LccSettingsSaveToFile(Sender: TObject; IniFile: TIniFile);
  private
    FAppAboutCmd: TMenuItem;
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
    procedure FormLoggingHideNotify(Sender: TObject);
    procedure ShowSettingsDialog;
  public
    { public declarations }
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
  end else
  begin
     LccComPort.CloseComPort(nil);
  end;
end;

procedure TForm1.ActionEthernetClientExecute(Sender: TObject);
begin
  if ActionEthernetClient.Checked then
  begin
    LccEthernetClient.OpenConnectionWithLccSettings;
  end else
  begin
    LccEthernetClient.CloseConnection(nil);
  end
end;

procedure TForm1.ActionEthernetServerExecute(Sender: TObject);
begin
  if ActionEthernetServer.Checked then
  begin
    LccEthernetServer.OpenConnectionWithLccSettings;
  end else
  begin
    LccEthernetServer.CloseConnection(nil);
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

procedure TForm1.ActionRaspberryPiExecute(Sender: TObject);
begin
  if ActionRaspberryPi.Checked then
  begin
    LccRaspberryPiSpiPort.OpenConnectionWithLccSettings;
  end else
  begin
    LccRaspberryPiSpiPort.CloseConnection(nil);
  end
end;

procedure TForm1.ActionToolsSettingsShowWinExecute(Sender: TObject);
begin
  ShowSettingsDialog;
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

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // Before shutdown clean up and disconnect from connections
  if ActionComPort.Checked then
    ActionComPort.Execute;                  // Force calling the OnExecute Event to clean up, but only if the Action is enabled
  if ActionEthernetServer.Checked then
    ActionEthernetServer.Execute;           // Force calling the OnExecute Event to clean up, but only if the Action is enabled
  if ActionRaspberryPi.Checked then
    ActionRaspberryPi.Execute;
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
    ActionToolsSettingsShowWin.Visible := False;
    {$ELSE}
    AppAboutCmd := TMenuItem.Create(Self);
    AppAboutCmd.Action := ActionHelpAboutShow;
    MenuItemHelp.Add(AppAboutCmd);
    {$ENDIF}

    FormLogging.OnHideNotify := @FormLoggingHideNotify;
    LccSettings.FilePath := GetSettingsPath + 'Settings.ini';                     // Setup the file paths to the Settings Object
    LccSettings.LoadFromFile;                                                     // Read in the settings from the file to initialize the object

    FormSettings.FrameLccSettings.LccSettings := LccSettings;                    // Connect the Settings Object to the Settings UI frame
    FormLogging.FrameLccLogging.LccSettings := LccSettings;                       // Allow Logging frame to partake in the Settings to persist logging option

    LccComPort.LoggingFrame := FormLogging.FrameLccLogging;                       // Connect the LoggingFrame to the Connections
    LccEthernetServer.LoggingFrame := FormLogging.FrameLccLogging;
    LccEthernetClient.LoggingFrame := FormLogging.FrameLccLogging;
    LccRaspberryPiSpiPort.LoggingFrame := FormLogging.FrameLccLogging;

    FormLogging.FrameLccLogging.Paused := True;                                   // Start off Paused since it is hidden
    ActionTCP.Checked := not LccSettings.Ethernet.GridConnect;
    LccEthernetServer.Gridconnect := LccSettings.Ethernet.GridConnect;
    LccEthernetClient.Gridconnect := LccSettings.Ethernet.GridConnect;
    FormSettings.ClientHeight := FormSettings.FrameLccSettings.ButtonOk.Top + FormSettings.FrameLccSettings.ButtonOk.Height + 8; // Now resize the form to fit its child controls

    {$IFDEF WINDOWS}
    FormLogging.FrameLccLogging.SynEdit.Font.Size := 11;
    {$ENDIF}
    ShownOnce := True;
  end;
end;

procedure TForm1.LccComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  case ComPortRec.ConnectionState of
    ccsPortConnecting :
    begin
      StatusBarMain.Panels[STATUS_PANEL_COMPORT].Text := 'Connecting: ' + ComPortRec.ComPort;
      ActionTCP.Enabled := False;
    end;
    ccsPortConnected :
    begin
      StatusBarMain.Panels[STATUS_PANEL_COMPORT].Text := 'Connected: ' + ComPortRec.ComPort;
    end;
    ccsPortDisconnecting :
    begin
       StatusBarMain.Panels[STATUS_PANEL_COMPORT].Text := 'Disconnecting: ' + ComPortRec.ComPort;
    end;
    ccsPortDisconnected :
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

procedure TForm1.LccComPortReceiveMessage(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  LccEthernetServer.SendMessage(ComPortRec.LccMessage);
  LccEthernetClient.SendMessage(ComPortRec.LccMessage);
end;

procedure TForm1.LccComPortSchedulerClass(Sender: TObject;
  var SchedulerClass: TSchedulerBaseClass);
begin
  SchedulerClass := TSchedulerPassThrough;
end;

procedure TForm1.LccEthernetClientConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting :
      begin
        StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Connecting Ethernet: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
        ActionEthernetServer.Enabled := False;
        ActionTCP.Enabled := False;
      end;
    ccsClientConnected :
      begin
        StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Connected To: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsClientDisconnecting :
      begin
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

procedure TForm1.LccEthernetClientReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  LccComPort.SendMessage(EthernetRec.LccMessage);
  LccEthernetServer.SendMessage(EthernetRec.LccMessage);
end;

procedure TForm1.LccEthernetClientSchedulerClass(Sender: TObject;
  var SchedulerClass: TSchedulerBaseClass);
begin
  SchedulerClass := TSchedulerPassThrough;
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
        ActionEthernetClient.Enabled := False;
        ActionTCP.Enabled := False;
      end;
    ccsListenerConnected :
      begin
        StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Listening: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsListenerDisconnecting :
      begin
         StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Disconnecting Ethernet: '+ EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsListenerDisconnected :
      begin
         StatusBarMain.Panels[STATUS_PANEL_ETHERNET].Text := 'Disconnected';
         ActionEthernetServer.Checked := False;
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

procedure TForm1.LccEthernetServerReceiveMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  LccComPort.SendMessage(EthernetRec.LccMessage);
  LccEthernetClient.SendMessage(EthernetRec.LccMessage);
  LccRaspberryPiSpiPort.SendMessage(EthernetRec.LccMessage);
end;

procedure TForm1.LccEthernetServerSchedulerClass(Sender: TObject;
  var SchedulerClass: TSchedulerBaseClass);
begin
  SchedulerClass := TSchedulerPassThrough;
end;

procedure TForm1.LccRaspberryPiSpiPortConnectionStateChange(Sender: TObject; PiSpiPortRec: TLccRaspberryPiSpiPortRec);
begin
  case PiSpiPortRec.ConnectionState of
    ccsPortConnecting :
    begin
      StatusBarMain.Panels[STATUS_PANEL_PISPIPORT].Text := 'Connecting: ' + PiSpiPortRec.Port;
      ActionTCP.Enabled := False;
    end;
    ccsPortConnected :
    begin
      StatusBarMain.Panels[STATUS_PANEL_PISPIPORT].Text := 'Connected: ' + PiSpiPortRec.Port;
    end;
    ccsPortDisconnecting :
    begin
       StatusBarMain.Panels[STATUS_PANEL_PISPIPORT].Text := 'Disconnecting: ' + PiSpiPortRec.Port;
    end;
    ccsPortDisconnected :
    begin
       StatusBarMain.Panels[STATUS_PANEL_PISPIPORT].Text := 'Disconnected:';
       ActionTCP.Enabled := True;
    end;
  end;
end;

procedure TForm1.LccRaspberryPiSpiPortReceiveMessage(Sender: TObject; PiSpiPortRec: TLccRaspberryPiSpiPortRec);
begin
  LccEthernetServer.SendMessage(PiSpiPortRec.LccMessage);
end;

procedure TForm1.LccRaspberryPiSpiPortSchedulerClass(Sender: TObject; var SchedulerClass: TSchedulerBaseClass);
begin
  SchedulerClass := TSchedulerPassThrough;
end;

procedure TForm1.LccSettingsLoadFromFile(Sender: TObject; IniFile: TIniFile);
begin
  ActionEthernetServer.Visible := IniFile.ReadBool('CustomSettings', 'EthernetServer', True);
  ActionEthernetClient.Visible := IniFile.ReadBool('CustomSettings', 'EthernetClient', False);
  ActionComPort.Visible := IniFile.ReadBool('CustomSettings', 'ComPort', False);
  ActionRaspberryPi.Visible := IniFile.ReadBool('CustomSettings', 'RaspberryPiSpi', True);

  ActionTCP.Visible := ActionEthernetClient.Visible or ActionEthernetServer.Visible;
  MenuItemConnectionDivider0.Visible := ActionTCP.Visible;
end;

procedure TForm1.LccSettingsSaveToFile(Sender: TObject; IniFile: TIniFile);
begin
  IniFile.WriteBool('CustomSettings', 'EthernetServer', ActionEthernetServer.Visible);
  IniFile.WriteBool('CustomSettings', 'EthernetClient', ActionEthernetClient.Visible);
  IniFile.WriteBool('CustomSettings', 'ComPort', ActionComPort.Visible);
  IniFile.WriteBool('CustomSettings', 'RaspberryPiSpi', ActionRaspberryPi.Visible);
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  LccEthernetClient.ClearSchedulerQueues;
  LccEthernetServer.ClearSchedulerQueues;
  LccComPort.ClearSchedulerQueues;
end;

procedure TForm1.ShowSettingsDialog;
begin
  FormSettings.FrameLccSettings.UserSettings.EthernetClient := ActionEthernetClient.Visible;
  FormSettings.FrameLccSettings.UserSettings.ComPort := ActionComPort.Visible;
  FormSettings.FrameLccSettings.UserSettings.EthernetServer := ActionEthernetServer.Visible;
  FormSettings.FrameLccSettings.UserSettings.RaspberryPiSpiPort := ActionRaspberryPi.Visible;
  // Update from video series, need to resync with the Settings each time the
  // dialog is shown as the user may have changed the UI and hit cancel and not
  // just when the program starts up in the FormShow event
  FormSettings.FrameLccSettings.SyncWithLccSettings;
  if FormSettings.ShowModal = mrOK then
  begin

  end;
end;

procedure TForm1.FormLoggingHideNotify(Sender: TObject);
begin
  if ActionLogWindow.Checked = True then
    ActionLogWindow.Execute;
end;

end.

