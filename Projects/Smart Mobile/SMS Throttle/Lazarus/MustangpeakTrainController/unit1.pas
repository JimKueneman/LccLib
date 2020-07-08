unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ComCtrls, ExtCtrls, Arrow,
  SynEdit,
  lcc_utilities,
  lcc_node,
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_ethernet_server,
  lcc_ethernet_client,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_protocol_traction,
  lcc_protocol_traction_configuration_functions,
  lcc_protocol_traction_configuation_functiondefinitioninfo,
  lcc_protocol_traction_simpletrainnodeinfo,
  lcc_math_float16,
  lcc_node_commandstation,
  lcc_node_controller,
  lcc_node_train;

type

  TAllocateTrainRefusedReason = (rrNone, rrAssignedControllerRefused, rrTrainRefused, rrUnknown);

  { TFormTrainController }

  TFormTrainController = class(TForm)
    ButtonReleaseTrain: TButton;
    ButtonHammerTest: TButton;
    ButtonThrottleF0: TButton;
    ButtonThrottleF1: TButton;
    ButtonThrottleF7: TButton;
    ButtonThrottleF8: TButton;
    ButtonThrottleF9: TButton;
    ButtonThrottleF2: TButton;
    ButtonThrottleF3: TButton;
    ButtonThrottleF4: TButton;
    ButtonThrottleF5: TButton;
    ButtonThrottleF6: TButton;
    ButtonThrottleF11: TButton;
    ButtonThrottleF12: TButton;
    ButtonThrottleF10: TButton;
    ButtonThrottleAssignAddress: TButton;
    ButtonThrottleConnectAndLogin: TButton;
    CheckBoxThrottleLocalIP: TCheckBox;
    Edit1: TEdit;
    EditHammerTest: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LabelMyIpAddress: TLabel;
    LabelThrottleTechnologyTitle: TLabel;
    EditThrottleAddress: TLabeledEdit;
    LabelThrottleIPAddress: TLabel;
    LabelThrottleAliasID: TLabel;
    LabelThrottleNodeID: TLabel;
    PageControlThrottleTechnology: TPageControl;
    PanelThrottleTechnology: TPanel;
    PanelThrottleControls: TPanel;
    PanelThrottleBackground: TPanel;
    PanelThrottleTitle: TPanel;
    PanelThrottleHeader: TPanel;
    PanelThrottleNode: TPanel;
    RadioGroupThrottleSearchAllocation: TRadioGroup;
    RadioGroupThrottleSearchMatch: TRadioGroup;
    RadioGroupThrottleSearchMatchTarget: TRadioGroup;
    RadioGroupThrottleTechnologySpeedSteps: TRadioGroup;
    RadioGroupThrottleTechnologyMarklin: TRadioGroup;
    RadioGroupThrottleTechnologyAddress: TRadioGroup;
    RadioGroupThrottleTechnologyOther: TRadioGroup;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    TabSheetThrottleTechnologyOther: TTabSheet;
    TabSheetThrottleTechnologyDCC: TTabSheet;
    TabSheetThrottleTechnologyMarklin: TTabSheet;
    TimerTaskPump: TTimer;
    ToggleBoxThrottleForward: TToggleBox;
    ToggleBoxThrottleReverse: TToggleBox;
    TrackBarThrottleSpeed: TTrackBar;
    procedure ButtonHammerTestClick(Sender: TObject);
    procedure ButtonReleaseTrainClick(Sender: TObject);
    procedure ButtonThrottleAssignAddressClick(Sender: TObject);
    procedure ButtonThrottleConnectAndLoginClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FControllerNode: TLccTrainController;
  protected
  public
    EthernetClient: TLccEthernetClient;
    NodeManager: TLccCanNodeManager;

    property ControllerNode: TLccTrainController read FControllerNode write FControllerNode;

    // Throttle
    procedure OnClientServerConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnClientServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);

    procedure OnNodeManagerIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerAliasChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
  end;


var
  FormTrainController: TFormTrainController;

implementation

{$R *.lfm}

{ TFormTrainController }

procedure TFormTrainController.ButtonThrottleAssignAddressClick(Sender: TObject);
var
  TrackProtocolFlags: Word;  // 5 bits
begin
  TrackProtocolFlags := 0;
  case PageControlThrottleTechnology.ActivePageIndex of
    0  : begin
           TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;
           case RadioGroupThrottleTechnologyAddress.ItemIndex of
             0 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT;
             1 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG;
           end;
           case RadioGroupThrottleTechnologySpeedSteps.ItemIndex of
             0 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
             1 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
             2 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
             3 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
           end;
         end;
    1  : begin
           TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY or TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN;
           case RadioGroupThrottleTechnologyMarklin.ItemIndex of
             0 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_ANY;
             1 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_1;
             2 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2;
             3 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2_F8;
           end;
         end;
    2  : begin
           TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY or TRACTION_SEARCH_TRACK_PROTOCOL_NON_MARKLIN;
           case RadioGroupThrottleTechnologyOther.ItemIndex of
             0 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_ALL;
             1 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_NATIVE_OPENLCB;
             2 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_MFX_M4;
           end;
         end;
  end;
  case RadioGroupThrottleSearchMatch.ItemIndex of
    0 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TYPE_EXACT_MATCH;
    1 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TYPE_ALL_MATCH;
  end;
  case RadioGroupThrottleSearchAllocation.ItemIndex of
    0 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_ALLOCATE_FORCE;
    1 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_ALLOCATE_EXISTING_ONLY;
  end;
  case RadioGroupThrottleSearchMatchTarget.ItemIndex of
    0 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TARGET_ADDRESS_MATCH;
    1 : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TARGET_ANY_MATCH;
  end;

  ControllerNode.AssignTrainByOpenLCB(EditThrottleAddress.Text, TrackProtocolFlags);
end;

procedure TFormTrainController.ButtonHammerTestClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to StrToInt(EditHammerTest.Text) - 1 do
  begin

  end;
end;

procedure TFormTrainController.ButtonReleaseTrainClick(Sender: TObject);
begin
  ControllerNode.ReleaseTrain;
end;

procedure TFormTrainController.ButtonThrottleConnectAndLoginClick(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.ListenerPort := 12021;
  if EthernetClient.Connected then
  begin
    NodeManager.LogoutAll;
    EthernetClient.CloseConnection(nil);
    ButtonThrottleConnectAndLogin.Caption := 'Connect and Login';
    CheckBoxThrottleLocalIP.Enabled := True;
  end else
  begin
    EthernetRec.AutoResolveIP := False;
    if CheckBoxThrottleLocalIP.Checked then
      EthernetRec.ListenerIP := '127.0.0.1'
    else
      EthernetRec.ListenerIP := Edit1.Text;

    EthernetClient.OpenConnection(EthernetRec);
    ButtonThrottleConnectAndLogin.Caption := 'Disconnect';
    CheckBoxThrottleLocalIP.Enabled := False;
  end;
end;

procedure TFormTrainController.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose;
  TimerTaskPump.Enabled := False;
  NodeManager.Clear;
  EthernetClient.CloseConnection(nil);
  while EthernetClient.Connected do
    Sleep(500);
  EthernetClient.Free;
  NodeManager.Free;
end;

procedure TFormTrainController.FormCreate(Sender: TObject);
begin
  // throttle node
  NodeManager := TLccCanNodeManager.Create(nil);
  NodeManager.OnLccNodeAliasIDChanged := @OnNodeManagerAliasChange;
  NodeManager.OnLccNodeIDChanged := @OnNodeManagerIDChange;
  NodeManager.OnLccMessageSend := @OnNodeManagerSendMessage;
  NodeManager.OnLccMessageReceive := @OnNodeManagerReceiveMessage;

  EthernetClient := TLccEthernetClient.Create(nil);
  EthernetClient.Gridconnect := True;
  EthernetClient.NodeManager := NodeManager;
  EthernetClient.OnConnectionStateChange := @OnClientServerConnectionChange;
  EthernetClient.OnErrorMessage := @OnClientServerErrorMessage;
end;

procedure TFormTrainController.FormShow(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  LabelMyIpAddress.Caption := ResolveWindowsIp;
  {$ELSE}
  LabelMyIpAddress.Caption := ResolveUnixIp;
  {$ENDIF}
end;

procedure TFormTrainController.OnClientServerConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting : LabelThrottleIPAddress.Caption    := 'IP Address: Connecting';
    ccsClientConnected  :
      begin
        LabelThrottleIPAddress.Caption    := 'IP Address: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);
        ControllerNode := NodeManager.AddNodeByClass('', TLccTrainController, True) as TLccTrainController;
      end;
    ccsClientDisconnecting :
      begin
        NodeManager.Clear;   // Logout
        ControllerNode := nil;
        LabelThrottleIPAddress.Caption := 'IP Address: Disconnecting';
      end;
    ccsClientDisconnected : LabelThrottleIPAddress.Caption  := 'IP Address: Disconnected';
  end;
end;

procedure TFormTrainController.OnClientServerErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage(EthernetRec.MessageStr);
  LabelThrottleIPAddress.Caption := 'IP Address Disconnected';
  ButtonThrottleConnectAndLogin.Caption := 'Connect and Login';
end;

procedure TFormTrainController.OnNodeManagerIDChange(Sender: TObject; LccSourceNode: TLccNode );
begin
  LabelThrottleNodeID.Caption := 'NodeID: ' + LccSourceNode.NodeIDStr;
end;

procedure TFormTrainController.OnNodeManagerAliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelThrottleAliasID.Caption := 'AliasID: ' + (LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TFormTrainController.OnNodeManagerSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  EthernetClient.SendMessage(LccMessage);
end;

procedure TFormTrainController.OnNodeManagerReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin

end;

end.

