unit form_template;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, Menus, LCLType, lcc_app_common_settings, lcc_comport, lcc_ethernetclient,
  lcc_nodemanager, lcc_ethenetserver, form_trace, form_settings, file_utilities,
  lcc_messages;

const
  BUNDLENAME             = 'LCCAppTemplate';
  PATH_LINUX_APP_FOLDER  = 'lccapptemplate/';
  PATH_SETTINGS_FILE     = 'settings.ini';

type

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
    LccComPort: TLccComPort;
    LccEthernetClient: TLccEthernetClient;
    LccEthernetServer: TLccEthernetServer;
    LccNodeManager: TLccNodeManager;
    LccSettings: TLccSettings;
    MainMenu: TMainMenu;
    MenuItemToolsSettings: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
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
    procedure FormShow(Sender: TObject);
    procedure LccComPortConnectionStateChange(Sender: TObject;
      ComPortRec: TLccComPortRec);
    procedure LccComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec
      );
    procedure LccEthernetClientConnectionStateChange(Sender: TObject;
      EthernetRec: TLccEthernetRec);
    procedure LccEthernetClientErrorMessage(Sender: TObject;
      EthernetRec: TLccEthernetRec);
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
    procedure HidingTraceForm(Sender: TObject);
    {$IFDEF DARWIN}
    property OSXMenu: TMenuItem read FOSXMenu write FOSXMenu;
    property OSXSep1Cmd: TMenuItem read FOSXSep1Cmd write FOSXSep1Cmd;
    property OSXPrefCmd: TMenuItem read FOSXPrefCmd write FOSXPrefCmd;
    {$ENDIF}
     property ShownOnce: Boolean read FShownOnce write FShownOnce;
  public
    { public declarations }
  end;

var
  FormTemplate: TFormTemplate;

implementation

{$R *.lfm}

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
end;

procedure TFormTemplate.ActionMsgTraceExecute(Sender: TObject);
begin
  if ActionMsgTrace.Checked then
    FormMsgTrace.Show
  else
    FormMsgTrace.Hide;
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

procedure TFormTemplate.FormShow(Sender: TObject);
begin
  if not ShownOnce then
  begin
    FormMsgTrace.OnFormHidingNotify := @HidingTraceForm;
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
      LccSettings.FilePath := GetSettingsPath + PATH_LINUX_APP_FOLDER + PATH_SETTINGS_FILE;
    {$ELSE}
    if LccSettings.FilePath = '' then
      LccSettings.FilePath := GetSettingsPath + PATH_SETTINGS_FILE;
    {$ENDIF}
    if FileExists(LccSettings.FilePath) then
      LccSettings.LoadFromFile(UTF8ToSys( LccSettings.FilePath));
    ShownOnce := True;
  end;
end;

procedure TFormTemplate.HidingTraceForm(Sender: TObject);
begin
  ActionMsgTrace.Checked := False;
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
        StatusBarMain.Panels[0].Text := 'Connected to ' + ComPortRec.ComPort;
      end;
    ccsComDisconnecting :
      begin
        StatusBarMain.Panels[0].Text := 'Disconnecting from ' + ComPortRec.ComPort;
      end;
    ccsComDisconnected  :
      begin
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
        StatusBarMain.Panels[0].Text := 'Connected to ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsClientDisconnecting :
      begin
        StatusBarMain.Panels[0].Text := 'Disconnecting from ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsClientDisconnected  :
      begin
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

end.

