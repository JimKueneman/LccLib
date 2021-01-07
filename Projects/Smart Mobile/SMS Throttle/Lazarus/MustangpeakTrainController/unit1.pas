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
  ComCtrls, ExtCtrls, Arrow, Buttons,
  SynEdit,
  lcc_utilities,
  lcc_node,
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_ethernet_server,
  lcc_ethernet_client,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_node_commandstation,
  lcc_node_controller,
  lcc_node_train,
  lcc_common_classes,
  lcc_ethernet_common;

type

  TAllocateTrainRefusedReason = (rrNone, rrAssignedControllerRefused, rrTrainRefused, rrUnknown);

  { TFormTrainController }

  TFormTrainController = class(TForm)
    ButtonReleaseTrain: TButton;
    ButtonHammerTest: TButton;
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
    SpeedButtonF10: TSpeedButton;
    SpeedButtonF11: TSpeedButton;
    SpeedButtonF2: TSpeedButton;
    SpeedButtonF3: TSpeedButton;
    SpeedButtonF1: TSpeedButton;
    SpeedButtonF4: TSpeedButton;
    SpeedButtonF0: TSpeedButton;
    SpeedButtonF5: TSpeedButton;
    SpeedButtonF6: TSpeedButton;
    SpeedButtonF7: TSpeedButton;
    SpeedButtonF8: TSpeedButton;
    SpeedButtonF9: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    TabSheetThrottleTechnologyOther: TTabSheet;
    TabSheetThrottleTechnologyDCC: TTabSheet;
    TabSheetThrottleTechnologyMarklin: TTabSheet;
    ToggleBoxThrottleForward: TToggleBox;
    ToggleBoxThrottleReverse: TToggleBox;
    TrackBarThrottleSpeed: TTrackBar;
    procedure ButtonHammerTestClick(Sender: TObject);
    procedure ButtonThrottleAssignAddressClick(Sender: TObject);
    procedure ButtonThrottleConnectAndLoginClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButtonF0Click(Sender: TObject);
    procedure SpeedButtonF10Click(Sender: TObject);
    procedure SpeedButtonF11Click(Sender: TObject);
    procedure SpeedButtonF1Click(Sender: TObject);
    procedure SpeedButtonF2Click(Sender: TObject);
    procedure SpeedButtonF3Click(Sender: TObject);
    procedure SpeedButtonF4Click(Sender: TObject);
    procedure SpeedButtonF6Click(Sender: TObject);
    procedure SpeedButtonF7Click(Sender: TObject);
    procedure SpeedButtonF8Click(Sender: TObject);
    procedure SpeedButtonF9Click(Sender: TObject);
    procedure ToggleBoxThrottleForwardChange(Sender: TObject);
    procedure ToggleBoxThrottleReverseChange(Sender: TObject);
    procedure TrackBarThrottleSpeedChange(Sender: TObject);
  private
    FControllerNode: TLccTrainController;
  protected
  public
    EthernetClient: TLccEthernetClient;
    NodeManager: TLccCanNodeManager;

    property ControllerNode: TLccTrainController read FControllerNode write FControllerNode;

    // Throttle
    procedure OnClientServerConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnClientServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);

    procedure OnNodeManagerIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManagerAliasChange(Sender: TObject; LccSourceNode: TLccNode);
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

procedure TFormTrainController.ButtonThrottleConnectAndLoginClick(Sender: TObject);
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  LocalInfo := TLccEthernetConnectionInfo.Create;
  try
    if EthernetClient.Connected then
    begin
      NodeManager.LogoutAll;
      EthernetClient.CloseConnection(nil);
      ButtonThrottleConnectAndLogin.Caption := 'Connect and Login';
      CheckBoxThrottleLocalIP.Enabled := True;
    end else
    begin
      LocalInfo.AutoResolveIP := False;

      LocalInfo.ListenerPort := 12021;
      if CheckBoxThrottleLocalIP.Checked then
        LocalInfo.ListenerIP := '127.0.0.1'
      else
        LocalInfo.ListenerIP := Edit1.Text;
      LocalInfo.GridConnect := True;

      EthernetClient.OpenConnection(LocalInfo);
      ButtonThrottleConnectAndLogin.Caption := 'Disconnect';
      CheckBoxThrottleLocalIP.Enabled := False;
    end;
  finally
    LocalInfo.Free;
  end;
end;

procedure TFormTrainController.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose;
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

  EthernetClient := TLccEthernetClient.Create(nil, NodeManager);
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

procedure TFormTrainController.SpeedButtonF0Click(Sender: TObject);
begin
  ControllerNode.Functions[0] := not ControllerNode.Functions[0];
end;

procedure TFormTrainController.SpeedButtonF10Click(Sender: TObject);
begin
  ControllerNode.Functions[10] := not ControllerNode.Functions[10];
end;

procedure TFormTrainController.SpeedButtonF11Click(Sender: TObject);
begin
  ControllerNode.Functions[11] := not ControllerNode.Functions[11];
end;

procedure TFormTrainController.SpeedButtonF1Click(Sender: TObject);
begin
  ControllerNode.Functions[1] := not ControllerNode.Functions[1];
end;

procedure TFormTrainController.SpeedButtonF2Click(Sender: TObject);
begin
  ControllerNode.Functions[2] := not ControllerNode.Functions[2];
end;

procedure TFormTrainController.SpeedButtonF3Click(Sender: TObject);
begin
  ControllerNode.Functions[3] := not ControllerNode.Functions[4];
end;

procedure TFormTrainController.SpeedButtonF4Click(Sender: TObject);
begin
  ControllerNode.Functions[5] := not ControllerNode.Functions[5];
end;

procedure TFormTrainController.SpeedButtonF6Click(Sender: TObject);
begin
  ControllerNode.Functions[6] := not ControllerNode.Functions[6];
end;

procedure TFormTrainController.SpeedButtonF7Click(Sender: TObject);
begin
  ControllerNode.Functions[7] := not ControllerNode.Functions[7];
end;

procedure TFormTrainController.SpeedButtonF8Click(Sender: TObject);
begin
  ControllerNode.Functions[8] := not ControllerNode.Functions[8];
end;

procedure TFormTrainController.SpeedButtonF9Click(Sender: TObject);
begin
  ControllerNode.Functions[9] := not ControllerNode.Functions[9];
end;

procedure TFormTrainController.OnClientServerConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  if Sender is TLccConnectionThread then
  begin
    case Info.ConnectionState of
      lcsConnecting :
        begin
          LabelThrottleIPAddress.Caption    := 'IP Address: Connecting';
        end;
     lcsConnected :
        begin
          LabelThrottleIPAddress.Caption    := 'IP Address: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
          ControllerNode := NodeManager.AddNodeByClass('', TLccTrainController, True) as TLccTrainController;
        end;
      lcsDisconnecting :
        begin
          NodeManager.Clear;   // Logout
          ControllerNode := nil;
          LabelThrottleIPAddress.Caption := 'IP Address: Disconnecting';
        end;
      lcsDisconnected :
        begin
          LabelThrottleIPAddress.Caption  := 'IP Address: Disconnected';
        end;
    end;
  end;
end;

procedure TFormTrainController.OnClientServerErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  ShowMessage(Info.MessageStr);
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

procedure TFormTrainController.ToggleBoxThrottleForwardChange(Sender: TObject);
begin
  ControllerNode.Direction := tdForward;
end;

procedure TFormTrainController.ToggleBoxThrottleReverseChange(Sender: TObject);
begin
  ControllerNode.Direction := tdReverse;
end;

procedure TFormTrainController.TrackBarThrottleSpeedChange(Sender: TObject);
begin
  ControllerNode.Speed := TrackBarThrottleSpeed.Position;
end;

end.

