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
  lcc_node_manager,
  lcc_node_messages,
  lcc_defines,
  lcc_ethernet_server,
  lcc_ethernet_client,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_protocol_traction,
  lcc_protocol_traction_configuration_functions,
  lcc_protocol_traction_configuation_functiondefinitioninfo,
  lcc_protocol_traction_simpletrainnodeinfo,
  lcc_math_float16;

type
  TTrainTarget = record
    NodeID: TNodeID;
    NodeAlias: Word;
  end;

  { TFormTrainController }

  TFormTrainController = class(TForm)
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
    LabelThrottleTechnologyTitle: TLabel;
    LabeledEditThrottleAddress: TLabeledEdit;
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
    ToggleBoxThrottleForward: TToggleBox;
    ToggleBoxThrottleReverse: TToggleBox;
    TrackBarThrottleSpeed: TTrackBar;
    procedure ButtonThrottleAssignAddressClick(Sender: TObject);
    procedure ButtonThrottleConnectAndLoginClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    FWorkerMsg: TLccMessage;
  protected
    procedure OnLccNodeProducerIdentifiedCallback(Sender: TObject;  LccSourceNode: TLccNode; LccMessage: TLccMessage; var Event: TEventID; State: TEventState);

  public
    ThrottleEthernetClient: TLccEthernetClient;
    ThrottleNodeManager: TLccCanNodeManager;
    TargetTrain: TTrainTarget;

    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;

    // Throttle
    procedure OnThrottleEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnThrottleEthernetErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnThrottleNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnThrottleNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnThrottleSendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure OnThrottleReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
  end;

var
  FormTrainController: TFormTrainController;

implementation

{$R *.lfm}

{ TFormTrainController }

procedure TFormTrainController.ButtonThrottleAssignAddressClick(Sender: TObject);
var
  TrackProtocolFlags: Word;  // 5 bits
  SearchData: DWORD;
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

  SearchData := 0;
  case WorkerMsg .TractionSearchEncodeSearchString(LabeledEditThrottleAddress.Text, TrackProtocolFlags, SearchData) of
    sese_ok :
      begin
        if Assigned (ThrottleNodeManager.Node[0]) then
        begin
          WorkerMsg.LoadTractionSearch(ThrottleNodeManager.Node[0].NodeID, ThrottleNodeManager.CanNode[0].AliasID, SearchData);
          ThrottleNodeManager.SendMessage(WorkerMsg);
        end;
      end;
    sese_TooLong           : ShowMessage('Search String too long, only 6 characters are available');
    sese_InvalidCharacters : ShowMessage('Invalid character in search string, only 0-9 or F are valid');
  end;
end;

procedure TFormTrainController.ButtonThrottleConnectAndLoginClick(Sender: TObject);
var
  EthernetRec: TLccEthernetRec;
begin
  FillChar(EthernetRec, Sizeof(EthernetRec), #0);
  EthernetRec.ListenerPort := 12021;
  if ThrottleEthernetClient.Connected then
  begin
    ThrottleNodeManager.LogoutAll;
    ThrottleEthernetClient.CloseConnection(nil);
    ButtonThrottleConnectAndLogin.Caption := 'Connect and Login';
    CheckBoxThrottleLocalIP.Enabled := True;
  end else
  begin
    if CheckBoxThrottleLocalIP.Checked then
    begin
      EthernetRec.ListenerIP := '127.0.0.1';
      EthernetRec.AutoResolveIP := False;
    end else
    begin
      EthernetRec.AutoResolveIP := True;
    end;
    ThrottleEthernetClient.OpenConnection(EthernetRec);
    ButtonThrottleConnectAndLogin.Caption := 'Disconnect';
    CheckBoxThrottleLocalIP.Enabled := False;
  end;
end;

procedure TFormTrainController.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CanClose;

  ThrottleNodeManager.Clear;
  ThrottleEthernetClient.CloseConnection(nil);
  while ThrottleEthernetClient.Connected do
    Sleep(500);
  ThrottleEthernetClient.Free;
  ThrottleNodeManager.Free;

  WorkerMsg.Free;
end;

procedure TFormTrainController.FormCreate(Sender: TObject);
begin
  FWorkerMsg := TLccMessage.Create;

  // throttle node
  ThrottleNodeManager := TLccCanNodeManager.Create(nil);
  ThrottleNodeManager.OnLccNodeAliasIDChanged := @OnThrottleNodeAliasChange;
  ThrottleNodeManager.OnLccNodeIDChanged := @OnThrottleNodeIDChange;
  ThrottleNodeManager.OnLccMessageSend := @OnThrottleSendMessage;
  ThrottleNodeManager.OnLccMessageReceive := @OnThrottleReceiveMessage;
  ThrottleNodeManager.OnLccNodeProducerIdentified := @OnLccNodeProducerIdentifiedCallback;

  ThrottleEthernetClient := TLccEthernetClient.Create(nil);
  ThrottleEthernetClient.Gridconnect := True;
  ThrottleEthernetClient.NodeManager := ThrottleNodeManager;
  ThrottleEthernetClient.OnConnectionStateChange := @OnThrottleEthernetConnectionChange;
  ThrottleEthernetClient.OnErrorMessage := @OnThrottleEthernetErrorMessage;
end;

procedure TFormTrainController.OnLccNodeProducerIdentifiedCallback(
  Sender: TObject; LccSourceNode: TLccNode;  LccMessage: TLccMessage; var Event: TEventID;
  State: TEventState);
var
  CanNode: TLccCANNode;
begin
  // Look for the Search Protocol in the EventID, we have a train node
  if (Event[0] = $09) and (Event[1] = $00) and (Event[2] = $99) and (Event[3] = $FF) then
  begin
    if LccMessage.HasSourceNodeID then
    begin  // Native TCP :)
      TargetTrain.NodeID := LccMessage.SourceID;
      TargetTrain.NodeAlias := 0;
      WorkerMsg.LoadTractionManage(ThrottleNodeManager.Node[0].NodeID, 0, TargetTrain.NodeID, 0, True);
    end else
    begin // Grid Connect :(
      TargetTrain.NodeID := NULL_NODE_ID;
      TargetTrain.NodeAlias := LccMessage.CAN.SourceAlias;
      CanNode := ThrottleNodeManager.Node[0] as TLccCanNode;;
      WorkerMsg.LoadTractionManage(NULL_NODE_ID, CanNode.AliasID, NULL_NODE_ID, TargetTrain.NodeAlias, True);
    end;
    ThrottleNodeManager.SendMessage(WorkerMsg);
  end;
end;

procedure TFormTrainController.OnThrottleEthernetConnectionChange(Sender: TObject; EthernetRec: TLccEthernetRec);
var
  CanNode: TLccCanNode;
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting : LabelThrottleIPAddress.Caption    := 'IP Address: Connecting';
    ccsClientConnected  :
      begin
        LabelThrottleIPAddress.Caption    := 'IP Address: ' + EthernetRec.ClientIP + ':' + IntToStr(EthernetRec.ClientPort);

        CanNode := ThrottleNodeManager.AddNode(CDI_XML) as TLccCanNode;

        CanNode.ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
        CanNode.ProtocolSupportedProtocols.Datagram := True;
        CanNode.ProtocolSupportedProtocols.EventExchange := True;
        CanNode.ProtocolSupportedProtocols.SimpleNodeInfo := True;
        CanNode.ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;
        CanNode.ProtocolSupportedProtocols.TractionControl := True;
        CanNode.ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := True;
        CanNode.ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := True;
        CanNode.ProtocolSupportedProtocols.TractionFunctionConfiguration := True;

        CanNode.ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_TRACTION_FDI, True, True, True, 0, $FFFFFFFF);
        CanNode.ProtocolMemoryInfo.Add(MSI_TRACTION_FUNCTION_CONFIG, True, False, True, 0, $FFFFFFFF);

        CanNode.ProtocolMemoryOptions.WriteUnderMask := True;
        CanNode.ProtocolMemoryOptions.UnAlignedReads := True;
        CanNode.ProtocolMemoryOptions.UnAlignedWrites := True;
        CanNode.ProtocolMemoryOptions.SupportACDIMfgRead := True;
        CanNode.ProtocolMemoryOptions.SupportACDIUserRead := True;
        CanNode.ProtocolMemoryOptions.SupportACDIUserWrite := True;
        CanNode.ProtocolMemoryOptions.WriteLenOneByte := True;
        CanNode.ProtocolMemoryOptions.WriteLenTwoBytes := True;
        CanNode.ProtocolMemoryOptions.WriteLenFourBytes := True;
        CanNode.ProtocolMemoryOptions.WriteLenSixyFourBytes := True;
        CanNode.ProtocolMemoryOptions.WriteArbitraryBytes := True;
        CanNode.ProtocolMemoryOptions.WriteStream := False;
        CanNode.ProtocolMemoryOptions.HighSpace := MSI_CDI;
        CanNode.ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;

    //    CanNode.ProtocolEventConsumed.AutoGenerate.Count := 5;
    //    CanNode.ProtocolEventConsumed.AutoGenerate.StartIndex := 0;

     //   CanNode.ProtocolEventsProduced.AutoGenerate.Count := 5;
     //   CanNode.ProtocolEventsProduced.AutoGenerate.StartIndex := 0;

        CanNode.Login(NULL_NODE_ID); // Create our own ID
      end;
    ccsClientDisconnecting :
      begin
        ThrottleNodeManager.Clear;   // Logout
        LabelThrottleIPAddress.Caption := 'IP Address: Disconnecting';
      end;
    ccsClientDisconnected : LabelThrottleIPAddress.Caption  := 'IP Address: Disconnected';
  end;
end;

procedure TFormTrainController.OnThrottleEthernetErrorMessage(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  ShowMessage(EthernetRec.MessageStr);
  LabelThrottleIPAddress.Caption := 'IP Address Disconnected';
  ButtonThrottleConnectAndLogin.Caption := 'Connect and Login';
end;

procedure TFormTrainController.OnThrottleNodeIDChange(Sender: TObject; LccSourceNode: TLccNode );
begin
  LabelThrottleNodeID.Caption := 'NodeID: ' + LccSourceNode.NodeIDStr;
end;

procedure TFormTrainController.OnThrottleNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelThrottleAliasID.Caption := 'AliasID: ' + (LccSourceNode as TLccCanNode).AliasIDStr;
end;

procedure TFormTrainController.OnThrottleSendMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  ThrottleEthernetClient.SendMessage(LccMessage);
end;

procedure TFormTrainController.OnThrottleReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin

end;

end.

