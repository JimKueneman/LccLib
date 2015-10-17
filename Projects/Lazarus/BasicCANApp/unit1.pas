unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, lcc_app_common_settings, lcc_comport, lcc_nodemanager, Unit2,
  file_utilities, frame_lcc_logging, lcc_messages;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionLogin: TAction;
    ActionComPort: TAction;
    ActionSettings: TAction;
    ActionList1: TActionList;
    FrameLccLogging1: TFrameLccLogging;
    ImageList1: TImageList;
    LccComPort1: TLccComPort;
    LccNodeManager1: TLccNodeManager;
    LccSettings1: TLccSettings;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure ActionComPortExecute(Sender: TObject);
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LccComPort1ConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccComPort1ErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccNodeManager1AliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManager1NodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
  private
    { private declarations }
  public
    { public declarations }
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
    LccComPort1.OpenComPortWithLccSettings;
  end else
  begin
     LccComPort1.CloseComPort(nil);
  end;
end;

procedure TForm1.ActionLoginExecute(Sender: TObject);
begin
  LccNodeManager1.Enabled := ActionLogin.Checked;
end;

procedure TForm1.ActionSettingsExecute(Sender: TObject);
begin
  // Update from video series, need to resync with the Settings each time the
  // dialog is shown as the user may have changed the UI and hit cancel and not
  // just when the program starts up in the FormShow event
  Form2.FrameLccSettings1.SyncWithLccSettings;
  if Form2.ShowModal = mrOK then
  begin

  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LccSettings1.FilePath := GetSettingsPath + 'Settings.ini';
  LccSettings1.LoadFromFile;
  Form2.FrameLccSettings1.LccSettings := LccSettings1;
  LccComPort1.LoggingFrame := FrameLccLogging1;

  // Updae from video series: Allow Logging frame partake in the Settings to persist logging options
  FrameLccLogging1.LccSettings := LccSettings1;
  // Load the Settings into the Logging Frame
  FrameLccLogging1.SyncwithLccSettings;

  // Update from video series don't show settings that are not valid
  Form2.FrameLccSettings1.UserSettings.EthernetClient := False;
  Form2.FrameLccSettings1.UserSettings.EthernetServer := False;
  // Now resize the form to fit its child controls
  Form2.ClientHeight := Form2.FrameLccSettings1.ButtonOk.Top + Form2.FrameLccSettings1.ButtonOk.Height + 8;
  // Keep Login Button disabled until the ComPort connection is made
  ActionLogin.Enabled := False;

  // Autosave the Configuration File
  LccNodeManager1.RootNode.Configuration.FilePath := GetSettingsPath;
  LccNodeManager1.RootNode.Configuration.AutoSaveOnWrite := True;
end;

procedure TForm1.LccComPort1ConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  case ComPortRec.ConnectionState of
    ccsPortConnecting :
    begin
      StatusBar1.Panels[0].Text := 'Connecting ComPort: ' + ComPortRec.ComPort;
    end;
    ccsPortConnected :
    begin
      StatusBar1.Panels[0].Text := 'Connected ComPort: ' + ComPortRec.ComPort;
      ActionLogin.Enabled := True;          // Allow the user to be able to Login
    end;
    ccsPortDisconnecting :
    begin
       StatusBar1.Panels[0].Text := 'Disconnecting ComPort: ' + ComPortRec.ComPort;
    end;
    ccsPortDisconnected :
    begin
       StatusBar1.Panels[0].Text := 'Disconnected:';
       StatusBar1.Panels[1].Text := 'Disconnected';
       ActionComPort.Checked := False;
       ActionLogin.Enabled := False;         // Disallow the user from being able to Login
    end;
  end;
end;

procedure TForm1.LccComPort1ErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  ShowMessage('Error on ' + ComPortRec.ComPort + ' Message: ' + ComPortRec.MessageStr);
  ActionComPort.Checked := False;
end;

procedure TForm1.LccNodeManager1AliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = LccNodeManager1.RootNode then
  begin
    StatusBar1.Panels[1].Text := LccSourceNode.NodeIDStr + ': 0x' + IntToHex(LccSourceNode.AliasID, 4);
  end;
end;

procedure TForm1.LccNodeManager1NodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = LccNodeManager1.RootNode then
  begin
    StatusBar1.Panels[1].Text := LccSourceNode.NodeIDStr + ': 0x' + IntToHex(LccSourceNode.AliasID, 4);
  end;
end;

end.

