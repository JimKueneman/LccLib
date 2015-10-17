unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, ExtCtrls, StdCtrls, lcc_app_common_settings, lcc_comport,
  lcc_nodemanager, Unit2, file_utilities, frame_lcc_logging,
  lcc_messages, lcc_ethenetserver, lcc_ethernetclient, lcc_nodeselector;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionShowRootNode: TAction;
    ActionShowLocalNodes: TAction;
    ActionConnectNetworkTree: TAction;
    ActionEthernetServer: TAction;
    ActionLogin: TAction;
    ActionComPort: TAction;
    ActionSettings: TAction;
    ActionList1: TActionList;
    CheckBoxShowLocalNodes: TCheckBox;
    CheckBoxShowRootNode: TCheckBox;
    CheckBoxTreeEnable: TCheckBox;
    CheckGroupNetworkTree: TCheckGroup;
    FrameLccLogging1: TFrameLccLogging;
    ImageList1: TImageList;
    LccComPort1: TLccComPort;
    LccEthernetServer1: TLccEthernetServer;
    LccNetworkTree: TLccNetworkTree;
    LccNodeManager1: TLccNodeManager;
    LccSettings1: TLccSettings;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ActionComPortExecute(Sender: TObject);
    procedure ActionConnectNetworkTreeExecute(Sender: TObject);
    procedure ActionEthernetServerExecute(Sender: TObject);
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure ActionShowLocalNodesExecute(Sender: TObject);
    procedure ActionShowRootNodeExecute(Sender: TObject);
    procedure CheckGroupNetworkTreeItemClick(Sender: TObject; Index: integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure LccComPort1ConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccComPort1ErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccEthernetServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
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
    LccNodeManager1.HardwareConnection := LccComPort1;     // Connect the Node Manager to the Comport Link
  end else
  begin
     LccComPort1.CloseComPort(nil);
     LccNodeManager1.HardwareConnection := nil;          // DisConnect the Node Manager  Link
  end;
end;

procedure TForm1.ActionConnectNetworkTreeExecute(Sender: TObject);
begin
  LccNetworkTree.Connected := ActionConnectNetworkTree.Checked;
end;

procedure TForm1.ActionEthernetServerExecute(Sender: TObject);
begin
  if ActionEthernetServer.Checked then
  begin
    LccEthernetServer1.OpenConnectionWithLccSettings;
    LccNodeManager1.HardwareConnection := LccEthernetServer1;     // Connect the Node Manager to the Comport Link
  end else
  begin
    LccEthernetServer1.CloseConnection(nil);
    LccNodeManager1.HardwareConnection := nil;          // DisConnect the Node Manager  Link
  end
end;

procedure TForm1.ActionLoginExecute(Sender: TObject);
begin
  LccNodeManager1.Enabled := ActionLogin.Checked;
  if LccNodeManager1.Enabled = False then
  begin
    StatusBar1.Panels[1].Text := 'Disconnected';
    if ActionConnectNetworkTree.Checked then     // Disconnect Network Tree
      ActionConnectNetworkTree.Execute;
  end;
  ActionConnectNetworkTree.Enabled := ActionLogin.Checked;
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

procedure TForm1.ActionShowLocalNodesExecute(Sender: TObject);
begin
  LccNetworkTree.ShowLocallNodes := CheckBoxShowLocalNodes.Checked;
end;

procedure TForm1.ActionShowRootNodeExecute(Sender: TObject);
begin
  LccNetworkTree.ShowRootNode := CheckBoxShowRootNode.Checked;
end;

procedure TForm1.CheckGroupNetworkTreeItemClick(Sender: TObject; Index: integer);
begin
  if CheckGroupNetworkTree.Checked[0] then
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties + [tp_Protocols]
  else
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties - [tp_Protocols];
  if CheckGroupNetworkTree.Checked[1] then
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties + [tp_Snip]
  else
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties - [tp_Snip];
  if CheckGroupNetworkTree.Checked[2] then
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties + [tp_AliasID]
  else
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties - [tp_AliasID];
  if CheckGroupNetworkTree.Checked[3] then
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties + [tp_ProducedEvents]
  else
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties - [tp_ProducedEvents];
  if CheckGroupNetworkTree.Checked[4] then
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties + [tp_ConsumedEvents]
  else
    LccNetworkTree.NetworkTreeProperties := LccNetworkTree.NetworkTreeProperties - [tp_ConsumedEvents];
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // Before shutdown clean up and disconnect from connections
  if ActionConnectNetworkTree.Checked then
     ActionConnectNetworkTree.Execute;       // Force calling the OnExecute Event to clean up, but only if the Action is enabled
  if ActionComPort.Checked then
    ActionComPort.Execute;                  // Force calling the OnExecute Event to clean up, but only if the Action is enabled
  if ActionEthernetServer.Checked then
    ActionEthernetServer.Execute;           // Force calling the OnExecute Event to clean up, but only if the Action is enabled
end;

procedure TForm1.FormShow(Sender: TObject);
begin

  // Setup the file paths to the Settings Object
  LccSettings1.FilePath := GetSettingsPath + 'Settings.ini';

  // Read in the settings from the file to initialize the object
  LccSettings1.LoadFromFile;

  // Connect the Settings Object to the Settings UI frame
  Form2.FrameLccSettings1.LccSettings := LccSettings1;

  // Connect the LoggingFrame to the Connections
  LccComPort1.LoggingFrame := FrameLccLogging1;
  LccEthernetServer1.LoggingFrame := FrameLccLogging1;

  // Updae from video series: Allow Logging frame partake in the Settings to persist logging options
  FrameLccLogging1.LccSettings := LccSettings1;
  // Load the Settings into the Logging Frame
  FrameLccLogging1.SyncwithLccSettings;

  // Update from video series don't show settings that are not valid
  Form2.FrameLccSettings1.UserSettings.EthernetClient := False;
  // Now resize the form to fit its child controls
  Form2.ClientHeight := Form2.FrameLccSettings1.ButtonOk.Top + Form2.FrameLccSettings1.ButtonOk.Height + 8;
  // Keep Actions disabled until the ComPort connection is made
  ActionLogin.Enabled := False;
  ActionConnectNetworkTree.Enabled := False;

  // Set the name for the configuration file.  If this is not set the configuration will
  // persist in a local stream object but when the application is closed it will be lost
  LccNodeManager1.RootNode.Configuration.FilePath := GetSettingsPath + 'Configuration.dat';
  LccNodeManager1.RootNode.Configuration.LoadFromFile;

  // You must place a XML file in the Setting Folder for this to have any effect
  // We also need to syncronize the SNIP to be the same as the <identification> section of
  // the CDI
  LccNodeManager1.RootNode.CDI.LoadFromXml(GetSettingsPath + 'SampleCdi.xml');
  LccNodeManager1.RootNode.SimpleNodeInfo.LoadFromXml(GetSettingsPath + 'SampleCdi.xml');

  {$IFDEF WINDOWS}
  FrameLccLogging1.SynEdit.Font.Size := 10;
  {$ENDIF}
end;

procedure TForm1.LccComPort1ConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  case ComPortRec.ConnectionState of
    ccsPortConnecting :
    begin
      StatusBar1.Panels[0].Text := 'Connecting ComPort: ' + ComPortRec.ComPort;
      ActionEthernetServer.Enabled := False;    // Disable Ethernet if Comport active
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
       ActionLogin.Enabled := False;          // Disallow the user from being able to Login
       ActionEthernetServer.Enabled := True;  // Reinable Ethernet
    end;
  end;
end;

procedure TForm1.LccComPort1ErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  ShowMessage('Error on ' + ComPortRec.ComPort + ' Message: ' + ComPortRec.MessageStr);
  ActionComPort.Checked := False;
end;

procedure TForm1.LccEthernetServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
    begin
      StatusBar1.Panels[0].Text := 'Connecting Ethernet: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      ActionComPort.Enabled := False;  // Disable Comport if Ethernet is active
    end;
    ccsListenerConnected :
    begin
      StatusBar1.Panels[0].Text := 'Listening: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      ActionLogin.Enabled := True;          // Allow the user to be able to Login
    end;
    ccsListenerDisconnecting :
    begin
       StatusBar1.Panels[0].Text := 'Disconnecting Ethernet: '+ EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
    end;
    ccsListenerDisconnected :
    begin
       StatusBar1.Panels[0].Text := 'Disconnected:';
       StatusBar1.Panels[1].Text := 'Disconnected';
       ActionEthernetServer.Checked := False;
       ActionLogin.Enabled := False;         // Disallow the user from being able to Login
       ActionComPort.Enabled := True;        // Reinable Comport
    end;
    ccsListenerClientConnecting :
    begin
    end;
    ccsListenerClientConnected :
    begin
    end;
    ccsListenerClientDisconnecting :
    begin
    end;
    ccsListenerClientDisconnected :
    begin
    end;
  end;
end;

procedure TForm1.LccEthernetServer1ErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('Error on ' + EthernetRec.ListenerIP + ' Message: ' + EthernetRec.MessageStr);
  ActionEthernetServer.Checked := False;
end;

procedure TForm1.LccNodeManager1AliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = LccNodeManager1.RootNode then
  begin
    ActionConnectNetworkTree.Enabled := True;
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

