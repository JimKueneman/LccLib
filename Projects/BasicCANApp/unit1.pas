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
    procedure LccComPort1ConnectionStateChange(Sender: TObject;
      ComPortRec: TLccComPortRec);
    procedure LccComPort1ErrorMessage(Sender: TObject;
      ComPortRec: TLccComPortRec);
    procedure LccNodeManager1AliasIDChanged(Sender: TObject;
      LccSourceNode: TLccNode);
    procedure LccNodeManager1NodeIDChanged(Sender: TObject;
      LccSourceNode: TLccNode);
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
  if Form2.ShowModal = mrOK then
  begin

  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LccSettings1.LoadFromFile(GetSettingsPath + 'Settings.ini');
  Form2.FrameLccSettings1.LccSettings := LccSettings1;
  Form2.FrameLccSettings1.SyncWithLccSettings;
  LccComPort1.LoggingFrame := FrameLccLogging1;
end;

procedure TForm1.LccComPort1ConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  case ComPortRec.ConnectionState of
    ccsComConnecting :
    begin
      StatusBar1.Panels[0].Text := 'Connecting ComPort: ' + ComPortRec.ComPort;
    end;
    ccsComConnected :
    begin
      StatusBar1.Panels[0].Text := 'Connected ComPort: ' + ComPortRec.ComPort;
    end;
    ccsComDisconnecting :
    begin
       StatusBar1.Panels[0].Text := 'Disconnecting ComPort: ' + ComPortRec.ComPort;
    end;
    ccsComDisconnected :
    begin
       StatusBar1.Panels[0].Text := 'Disconnected:';
       ActionComPort.Checked := False;
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

