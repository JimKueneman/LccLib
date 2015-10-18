unit HeaderFooterFormwithNavigation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms, FMX.Dialogs, FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.Objects, FMX.StdCtrls, lcc_nodemanager, lcc_app_common_settings,
  lcc_common_classes, lcc_ethenetserver, FMX.Controls.Presentation, lcc_ethernetclient,
  lcc_messages, FMX.TreeView, FMX.Layouts;

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
    LccEthernetServer1: TLccEthernetServer;
    LccSettings1: TLccSettings;
    LccNodeManager1: TLccNodeManager;
    SpeedButtonConnect: TSpeedButton;
    ActionConnect: TAction;
    LabelStatus: TLabel;
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    TreeViewItem8: TTreeViewItem;
    TreeViewItem9: TTreeViewItem;
    TreeViewItem10: TTreeViewItem;
    procedure FormCreate(Sender: TObject);
    procedure TitleActionUpdate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ActionConnectExecute(Sender: TObject);
    procedure LccEthernetServer1ConnectionStateChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure LccEthernetServer1ErrorMessage(Sender: TObject;
      EthernetRec: TLccEthernetRec);
  private
    { Private declarations }
  public
    { Public declarations }
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
    LccEthernetServer1.CloseConnection(nil);
  end else
  begin
    LccEthernetServer1.OpenConnectionWithLccSettings;
  end;
end;

procedure THeaderFooterwithNavigation.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControl1.First(TTabTransition.None);
end;

procedure THeaderFooterwithNavigation.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkHardwareBack) and (TabControl1.TabIndex <> 0) then
  begin
    TabControl1.First;
    Key := 0;
  end;
end;

procedure THeaderFooterwithNavigation.LccEthernetServer1ConnectionStateChange(
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

procedure THeaderFooterwithNavigation.LccEthernetServer1ErrorMessage(
  Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage('Error: ' + IntToStr(EthernetRec.ErrorCode) + ', ' + EthernetRec.MessageStr);
end;

end.
