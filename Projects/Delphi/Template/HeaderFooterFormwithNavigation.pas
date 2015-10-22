unit HeaderFooterFormwithNavigation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms, FMX.Dialogs, FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.Objects, FMX.StdCtrls, lcc_nodemanager, lcc_app_common_settings,
  lcc_common_classes, lcc_ethenetserver, FMX.Controls.Presentation, lcc_ethernetclient,
  lcc_messages, FMX.TreeView, FMX.Layouts, lcc_defines, file_utilities;

type
  THeaderFooterwithNavigation = class(TForm)
    ActionList1: TActionList;
    PreviousTabAction1: TPreviousTabAction;
    TitleAction: TControlAction;
    NextTabAction1: TNextTabAction;
    TopToolBar: TToolBar;
    btnBack: TSpeedButton;
    ToolBarLabel: TLabel;
    btnNext: TSpeedButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    BottomToolBar: TToolBar;
    LccEthernetServer: TLccEthernetServer;
    LccSettings: TLccSettings;
    LccNodeManager: TLccNodeManager;
    SpeedButtonConnect: TSpeedButton;
    ActionConnect: TAction;
    LabelStatus: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TitleActionUpdate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ActionConnectExecute(Sender: TObject);
    procedure LccEthernetServerConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServerErrorMessage(Sender: TObject;
      EthernetRec: TLccEthernetRec);
    procedure Button1Click(Sender: TObject);
    procedure LccNodeManagerLccGetRootNodeClass(Sender: TObject;
      var NodeClass: TLccOwnedNodeClass);
    procedure LccNodeManagerNodeIDChanged(Sender: TObject;
      LccSourceNode: TLccNode);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TMyRootNode = class(TLccOwnedNode)
  public
    constructor Create(AnOwner: TComponent); override;
  end;

var
  HeaderFooterwithNavigation: THeaderFooterwithNavigation;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.iPhone4in.fmx IOS}

procedure THeaderFooterwithNavigation.TitleActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    if TabControl1.ActiveTab <> nil then
      TCustomAction(Sender).Text := TabControl1.ActiveTab.Text
    else
      TCustomAction(Sender).Text := '';
  end;
end;

procedure THeaderFooterwithNavigation.ActionConnectExecute(Sender: TObject);
begin
  if ActionConnect.Checked then
  begin
    LccEthernetServer.CloseConnection(nil);
  end else
  begin
    LccEthernetServer.OpenConnectionWithLccSettings;
  end;
end;

procedure THeaderFooterwithNavigation.Button1Click(Sender: TObject);
begin
  LccNodeManager.Enabled := not LccNodeManager.Enabled;
  if LccNodeManager.Enabled then
    Button1.Text := 'Lcc Logout'
  else
    Button1.Text := 'Lcc Login';
end;

procedure THeaderFooterwithNavigation.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControl1.First(TTabTransition.None);
  LccSettings.FilePath := '.\settings.ini';
 // LccSettings.FilePath := GetSettingsPath + 'settings.ini';
end;

procedure THeaderFooterwithNavigation.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkHardwareBack) and (TabControl1.TabIndex <> 0) then
  begin
    TabControl1.First;
    Key := 0;
  end;
end;

procedure THeaderFooterwithNavigation.LccEthernetServerConnectionStateChange(
  Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsListenerConnecting :
      begin
        LabelStatus.Text := 'Connecting...';
      end;
    ccsListenerConnected :
      begin
        LabelStatus.Text := 'Connected: ' + EthernetRec.ListenerIP + ':' + IntToStr(EthernetRec.ListenerPort);
      end;
    ccsListenerDisconnecting :
      begin
        LabelStatus.Text := 'Disconnecting';
      end;
    ccsListenerDisconnected  :
      begin
        LabelStatus.Text := 'Disconnected';
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
    ccsClientConnecting,
    ccsClientConnected,
    ccsClientDisconnecting,
    ccsClientDisconnected,
    ccsPortConnecting,
    ccsPortConnected,
    ccsPortDisconnecting,
    ccsPortDisconnected : begin end;
  end;
end;

procedure THeaderFooterwithNavigation.LccEthernetServerErrorMessage(
  Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('Error: ' + IntToStr(EthernetRec.ErrorCode) + ', ' + EthernetRec.MessageStr);
end;

procedure THeaderFooterwithNavigation.LccNodeManagerLccGetRootNodeClass(Sender: TObject; var NodeClass: TLccOwnedNodeClass);
begin
  NodeClass := TMyRootNode;
end;

procedure THeaderFooterwithNavigation.LccNodeManagerNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  beep
end;

{ TMyRootNode }

constructor TMyRootNode.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);

  // Common Protocols
  ProtocolSupport.Datagram := True;                   // We support CDI so we must support datagrams
  ProtocolSupport.MemConfig := True;                  // We support CDI so we must support datagrams
  ProtocolSupport.CDI := True;                        // We Support CDI
  ProtocolSupport.EventExchange := True;              // We support Events
  ProtocolSupport.SimpleNodeInfo := True;             // We Support SNIP
  ProtocolSupport.ACDI := False;                      // We don't support ACDI
  ProtocolSupport.Stream := False;
  ProtocolSupport.RemoteButton := False;
  ProtocolSupport.Reservation := False;
  ProtocolSupport.Teach_Learn := False;
  ProtocolSupport.Display := False;
  ProtocolSupport.Identification := False;
  ProtocolSupport.Valid := True;

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
  ProtocolSupport.Valid := True;

  // When the node ID is generated the defined events will be generated from the
  // NodeID + StartIndex and incremented up to Count - 1
  EventsConsumed.AutoGenerate.Enable := True;
  EventsConsumed.AutoGenerate.Count := 10;
  EventsConsumed.AutoGenerate.StartIndex := 0;
  EventsConsumed.AutoGenerate.DefaultState := evs_InValid;

  // When the node ID is generated the defined events will be generated from the
  // NodeID + StartIndex and incremented up to Count - 1
  EventsProduced.AutoGenerate.Enable := True;
  EventsProduced.AutoGenerate.Count := 10;
  EventsProduced.AutoGenerate.StartIndex := 0;
  EventsProduced.AutoGenerate.DefaultState := evs_InValid;

  Configuration.FilePath := GetSettingsPath + 'configuration.dat';
  Configuration.Valid := True;

  // Load a CDI XML file from the same folder that the Setting.ini is stored
  CDI.LoadFromXml( GetSettingsPath + 'example_cdi.xml');
  CDI.Valid := True;

  // Setup the Configuraion Memory Options:
  ConfigMemOptions.HighSpace := MSI_CDI;
  ConfigMemOptions.LowSpace := MSI_ACDI_USER;
  ConfigMemOptions.SupportACDIMfgRead := False;
  ConfigMemOptions.SupportACDIUserRead := False;
  ConfigMemOptions.SupportACDIUserWrite := False;
  ConfigMemOptions.UnAlignedReads := True;
  ConfigMemOptions.UnAlignedWrites := True;
  ConfigMemOptions.WriteArbitraryBytes := True;
  ConfigMemOptions.WriteLenFourBytes := True;
  ConfigMemOptions.WriteLenOneByte := True;
  ConfigMemOptions.WriteLenSixyFourBytes := True;
  ConfigMemOptions.WriteLenTwoBytes := True;
  ConfigMemOptions.WriteStream := False;
  ConfigMemOptions.WriteUnderMask := False;
  ConfigMemOptions.Valid := True;

  // Setup the Configuration Memory Addres Space Information
  ConfigMemAddressSpaceInfo.Add(MSI_CDI, True, True, True, $00000000, $FFFFFFFF);
  ConfigMemAddressSpaceInfo.Add(MSI_ALL, True, True, True, $00000000, $FFFFFFFF);
  ConfigMemAddressSpaceInfo.Add(MSI_CONFIG, True, False, True, $00000000, $FFFFFFFF);
  ConfigMemAddressSpaceInfo.Add(MSI_ACDI_MFG, False, True, True, $00000000, $FFFFFFFF);      // We don't support ACDI in this object
  ConfigMemAddressSpaceInfo.Add(MSI_ACDI_USER, False, False, True, $00000000, $FFFFFFFF);    // We don't support ACDI in this object
  ConfigMemAddressSpaceInfo.Valid := True;
end;

end.
