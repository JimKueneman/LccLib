unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  ComCtrls, ExtCtrls, Menus, StdCtrls, Spin, lcc_app_common_settings,
  lcc_comport, lcc_nodemanager, form_settings, file_utilities,
  frame_lcc_logging, lcc_messages, lcc_ethenetserver, lcc_ethernetclient,
  form_logging, lcc_nodeselector, lcc_cdi_parser;

type

  TMouseInfo = record
    Button: TMouseButton;
    Shift: TShiftState;
    X, Y: Integer
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ActionEditUserStrings: TAction;
    ActionListSelector: TActionList;
    ActionVisibility: TAction;
    ActionLogWindow: TAction;
    ActionEthernetServer: TAction;
    ActionLogin: TAction;
    ActionComPort: TAction;
    ActionSettings: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    LabelMyNodes: TLabel;
    LccComPort: TLccComPort;
    LccEthernetServer: TLccEthernetServer;
    LccNodeManager: TLccNodeManager;
    LccNodeSelector: TLccNodeSelector;
    LccNodeSelectorConsumer: TLccNodeSelector;
    LccNodeSelectorProducer: TLccNodeSelector;
    LccNodeSelectorProducer1: TLccNodeSelector;
    LccSettings: TLccSettings;
    MenuItemPopupSelectorEditUserStrings: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PanelMain: TPanel;
    PanelNetworkTree: TPanel;
    PopupMenuSelector: TPopupMenu;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    SpinEdit1: TSpinEdit;
    Splitter1: TSplitter;
    SplitterMain: TSplitter;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    TrackBar1: TTrackBar;
    procedure ActionComPortExecute(Sender: TObject);
    procedure ActionEditUserStringsExecute(Sender: TObject);
    procedure ActionEthernetServerExecute(Sender: TObject);
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionLogWindowExecute(Sender: TObject);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure ActionVisibilityExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure LccComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
    procedure LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerLccNodeInitializationComplete(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerLccNodeProtocolIdentifyReply(Sender: TObject;
      LccSourceNode, LccDestNode: TLccNode);
    procedure LccNodeManagerLccNodeSimpleNodeIdentReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
    procedure LccNodeManagerLccNodeVerifiedNodeID(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeManagerNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure LccNodeSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LccNodeSelectorResize(Sender: TObject);
    procedure PopupMenuSelectorPopup(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FLastMouseDownInfo: TMouseInfo;
    { private declarations }
  protected
    procedure TestForDuplicateAndAdd(LccSourceNode: TLccNode);
  public
    { public declarations }
    property LastMouseDownInfo: TMouseInfo read FLastMouseDownInfo;
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
      if LccNode.ProtocolSupport.CDI and not LccNode.CDI.Valid then
      begin
        LccNodeManager.UserMessage.LoadCDIRequest(LccNodeManager.RootNode.NodeID, LccNodeManager.RootNode.AliasID, LccNode.NodeID, LccNode.AliasID);
        LccNodeManager.HardwareConnection.SendMessage(LccNodeManager.UserMessage);
      end;
    end;
  end;
end;

procedure TForm1.ActionEthernetServerExecute(Sender: TObject);
begin
  if ActionEthernetServer.Checked then
  begin
    LccEthernetServer.OpenEthernetConnectionWithLccSettings;
    LccNodeManager.HardwareConnection := LccEthernetServer;     // Connect the Node Manager to the Comport Link
  end else
  begin
    LccEthernetServer.CloseEthernetConnection(nil);
    LccNodeManager.HardwareConnection := nil;          // DisConnect the Node Manager  Link
  end
end;

procedure TForm1.ActionLoginExecute(Sender: TObject);
begin
  LccNodeManager.Enabled := ActionLogin.Checked;
  if LccNodeManager.Enabled = False then
  begin
    StatusBar1.Panels[1].Text := 'Disconnected';
  end else
  begin
    LccNodeSelector.LccNodes.Clear;
  end;
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

procedure TForm1.ActionSettingsExecute(Sender: TObject);
begin
  // Update from video series, need to resync with the Settings each time the
  // dialog is shown as the user may have changed the UI and hit cancel and not
  // just when the program starts up in the FormShow event
  FormSettings.FrameLccSettings1.SyncWithLccSettings;
  if FormSettings.ShowModal = mrOK then
  begin

  end;
end;

procedure TForm1.ActionVisibilityExecute(Sender: TObject);
var
  i: Integer;
begin
  LccNodeSelector.BeginUpdate;
  try
    if ActionVisibility.Checked then
    begin
      for i := 0 to LccNodeSelector.LccNodes.Count - 1 do
     //   LccNodeSelector.LccNodes[i].Visible := i mod 2 <> 0
         LccNodeSelector.LccNodes[i].Visible := False
    end else
    begin
      for i := 0 to LccNodeSelector.LccNodes.Count - 1 do
        LccNodeSelector.LccNodes[i].Visible := True
    end;
  finally
    LccNodeSelector.EndUpdate;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // Before shutdown clean up and disconnect from connections
  if ActionComPort.Checked then
    ActionComPort.Execute;                  // Force calling the OnExecute Event to clean up, but only if the Action is enabled
  if ActionEthernetServer.Checked then
    ActionEthernetServer.Execute;           // Force calling the OnExecute Event to clean up, but only if the Action is enabled
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: Integer;
begin
  // Setup the file paths to the Settings Object
  LccSettings.FilePath := GetSettingsPath + 'Settings.ini';

  // Read in the settings from the file to initialize the object
  LccSettings.LoadFromFile;

  // Connect the Settings Object to the Settings UI frame
  FormSettings.FrameLccSettings1.LccSettings := LccSettings;

  // Connect the LoggingFrame to the Connections
  LccComPort.LoggingFrame := FormLogging.FrameLccLogging;
  LccEthernetServer.LoggingFrame := FormLogging.FrameLccLogging;

  // Allow Logging frame to partake in the Settings to persist logging options
  FormLogging.FrameLccLogging.LccSettings := LccSettings;
  // Load the Settings into the Logging Frame
  FormLogging.FrameLccLogging.SyncwithLccSettings;
  // Start off Paused since it is hidden
  FormLogging.FrameLccLogging.Paused := False;

  // Update from video series don't show settings that are not valid
  FormSettings.FrameLccSettings1.UserSettings.EthernetClient := False;
  // Now resize the form to fit its child controls
  FormSettings.ClientHeight := FormSettings.FrameLccSettings1.ButtonOk.Top + FormSettings.FrameLccSettings1.ButtonOk.Height + 8;
  // Keep Login Button disabled until the ComPort connection is made
  ActionLogin.Enabled := False;

  // Set the name for the configuration file.  If this is not set the configuration will
  // persist in a local stream object but when the application is closed it will be lost
  LccNodeManager.RootNode.Configuration.FilePath := GetSettingsPath + 'Configuration.dat';
  LccNodeManager.RootNode.Configuration.LoadFromFile;

  // You must place a XML file in the Setting Folder for this to have any effect
  // We also need to syncronize the SNIP to be the same as the <identification> section of
  // the CDI
  LccNodeManager.RootNode.CDI.LoadFromXml(GetSettingsPath + 'SampleCdi.xml');
  LccNodeManager.RootNode.SimpleNodeInfo.LoadFromXml(GetSettingsPath + 'SampleCdi.xml');

  {$IFDEF WINDOWS}
  FormLogging.FrameLccLogging.SynEdit.Font.Size := 11;
  {$ENDIF}
end;

procedure TForm1.LccComPortConnectionStateChange(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  case ComPortRec.ConnectionState of
    ccsComConnecting :
    begin
      StatusBar1.Panels[0].Text := 'Connecting ComPort: ' + ComPortRec.ComPort;
      ActionEthernetServer.Enabled := False;    // Disable Ethernet if Comport active
    end;
    ccsComConnected :
    begin
      StatusBar1.Panels[0].Text := 'Connected ComPort: ' + ComPortRec.ComPort;
      ActionLogin.Enabled := True;          // Allow the user to be able to Login
      ActionLogin.Execute;
    end;
    ccsComDisconnecting :
    begin
       StatusBar1.Panels[0].Text := 'Disconnecting ComPort: ' + ComPortRec.ComPort;
    end;
    ccsComDisconnected :
    begin
       StatusBar1.Panels[0].Text := 'Disconnected:';
       StatusBar1.Panels[1].Text := 'Disconnected';
       ActionComPort.Checked := False;
       ActionLogin.Execute;
       ActionLogin.Enabled := False;          // Disallow the user from being able to Login
       ActionEthernetServer.Enabled := True;  // Reinable Ethernet
    end;
  end;
end;

procedure TForm1.LccComPortErrorMessage(Sender: TObject; ComPortRec: TLccComPortRec);
begin
  ShowMessage('Error on ' + ComPortRec.ComPort + ' Message: ' + ComPortRec.MessageStr);
  ActionComPort.Checked := False;
end;

procedure TForm1.LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
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
      ActionLogin.Execute;
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
       ActionLogin.Execute;
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

procedure TForm1.LccEthernetServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('Error on ' + EthernetRec.ListenerIP + ' Message: ' + EthernetRec.MessageStr);
  ActionEthernetServer.Checked := False;
end;

procedure TForm1.LccNodeManagerAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = LccNodeManager.RootNode then
    StatusBar1.Panels[1].Text := LccSourceNode.NodeIDStr + ': 0x' + IntToHex(LccSourceNode.AliasID, 4);
end;

procedure TForm1.LccNodeManagerLccNodeInitializationComplete(Sender: TObject; LccSourceNode: TLccNode);
begin
  TestForDuplicateAndAdd(LccSourceNode);
end;

procedure TForm1.LccNodeManagerLccNodeProtocolIdentifyReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
begin
 // beep;//
end;

procedure TForm1.LccNodeManagerLccNodeSimpleNodeIdentReply(Sender: TObject; LccSourceNode, LccDestNode: TLccNode);
begin
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
end;

procedure TForm1.LccNodeManagerLccNodeVerifiedNodeID(Sender: TObject; LccSourceNode: TLccNode);
begin
  TestForDuplicateAndAdd(LccSourceNode);
end;

procedure TForm1.LccNodeManagerNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  if LccSourceNode = LccNodeManager.RootNode then
    StatusBar1.Panels[1].Text := LccSourceNode.NodeIDStr + ': 0x' + IntToHex(LccSourceNode.AliasID, 4);
end;

procedure TForm1.LccNodeSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastMouseDownInfo.Button := Button;
  FLastMouseDownInfo.Shift := Shift;
  FLastMouseDownInfo.X := X;
  FLastMouseDownInfo.Y := Y;
end;

procedure TForm1.LccNodeSelectorResize(Sender: TObject);
begin
  StatusBar1.Panels[2].text := 'ClientH: ' + IntToStr(LccNodeSelector.ClientHeight) + '  VScroll - Pos: ' + IntToStr(LccNodeSelector.VertScrollBar.Position) + '  Page: ' + IntToStr(LccNodeSelector.VertScrollBar.Page) + '  Range: ' + IntToStr(LccNodeSelector.VertScrollBar.Range);
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
      ActionEditUserStrings.Enabled := LccNode.ProtocolSupport.CDI
  end;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  LccNodeSelector.Alignment := TAlignment( RadioGroup1.ItemIndex);
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  LccNodeSelector.TextLayout := TTextLayout( RadioGroup2.ItemIndex);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  LccNodeSelector.CaptionLineCount := SpinEdit1.Value;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  LccNodeSelector.DefaultNodeHeight := TrackBar1.Position;
end;

procedure TForm1.TestForDuplicateAndAdd(LccSourceNode: TLccNode);
begin
  if not Assigned(LccNodeSelector.LccNodes.Find(LccSourceNode.NodeID)) then
  begin
    LccNodeSelector.BeginUpdate;
    try
      LccSourceNode.LccGuiNode := LccNodeSelector.LccNodes.Add(LccSourceNode.NodeID, LccSourceNode.AliasID);
      LccSourceNode.LccGuiNode.Captions.Clear;
      LccSourceNode.LccGuiNode.Captions.Add('NodeID: ' + LccSourceNode.NodeIDStr);
      LccSourceNode.LccGuiNode.Captions.Add('AliasID: ' + LccSourceNode.AliasIDStr);
      LccSourceNode.LccGuiNode.ImageIndex := 0;
    finally
      LccNodeSelector.EndUpdate;
    end;
  end;
  LccNodeManager.UserMessage.LoadSimpleNodeIdentInfoRequest(LccNodeManager.RootNode.NodeID, LccNodeManager.RootNode.AliasID, LccSourceNode.NodeID, LccSourceNode.AliasID);
  LccNodeManager.HardwareConnection.SendMessage(LccNodeManager.UserMessage);
  LccNodeManager.UserMessage.LoadProtocolIdentifyInquiry(LccNodeManager.RootNode.NodeID, LccNodeManager.RootNode.AliasID, LccSourceNode.NodeID, LccSourceNode.AliasID);
  LccNodeManager.HardwareConnection.SendMessage(LccNodeManager.UserMessage);
end;

end.

