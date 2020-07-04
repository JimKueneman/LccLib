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
  lcc_math_float16,lcc_node_commandstation, lcc_node_controller, lcc_node_train;

type
  TLccStateMachine = class;

  TStateMachineCallback = procedure(Sender: TLccStateMachine);

  { TLccStateMachine }

  TLccStateMachine = class
  private
    FDestAlias: Word;
    FDestID: TNodeID;
    FOnEnded: TNotifyEvent;
    FSendMessage: TOnMessageEvent;
    FSourceAlias: Word;
    FSourceID: TNodeID;
    FState: Integer;
    FTimer: TTimer;
    FWatchdogCount: Integer;
    FWatchdogTime: Integer;
    FWorkerMsg: TLccMessage;
  protected
    property WatchdogCount: Integer read FWatchdogCount write FWatchdogCount;
    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
    property OnEnded: TNotifyEvent read FOnEnded write FOnEnded;

    procedure OnTimer(Sender: TObject);
    procedure DoEnded; virtual;

    property SendMessage: TOnMessageEvent read FSendMessage write FSendMessage;
    procedure StateMachineRun; virtual; abstract;
    procedure WatchdogTimedOut; virtual abstract;
  public
    property Timer: TTimer read FTimer write FTimer;
    property State: Integer read FState;
    property WatchdogTime: Integer read FWatchdogTime write FWatchdogTime;
    property SourceID: TNodeID read FSourceID write FSourceID;        // the Source of this conversation with the target node
    property SourceAlias: Word read FSourceAlias write FSourceAlias;
    property DestID: TNodeID read FDestID write FDestID;              // The selected Node to communicate with
    property DestAlias: Word read FDestAlias write FDestAlias;

    constructor Create(ASourceNodeID: TNodeID; ASourceAliasID: Word; SendMessageFunc: TOnMessageEvent; AnOnEndFunc: TNotifyEvent);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

  TAllocateTrainRefusedReason = (rrNone, rrAssignedControllerRefused, rrTrainRefused, rrUnknown);

  { TLccThrottleConnectStateMachine }

  TLccThrottleConnectStateMachine = class(TLccStateMachine)
  protected
    procedure StateMachineRun; override;
    procedure WatchdogTimedOut; override;
  public
    function HandleTrainSearchState(SearchStr: string; TrackProtocolFlags: Word): Boolean;
    function HandleTrainSearchWaitReply(LccMessage: TLccMessage): Boolean;
    function HandleReserveTrainState(LccMessage: TLccMessage): Boolean;
    function HandleReserveTrainStateReply(LccMessage: TLccMessage): Boolean;
    function HandleAllocateThrottleState(LccMessage: TLccMessage): Boolean;
    function HandleAllocateThrottleStateReply(LccMessage: TLccMessage; var RefusedReason: TAllocateTrainRefusedReason): Boolean;
    function HandleReleaseTrainState(LccMessage: TLccMessage): Boolean;
  end;

type
  TTrainTarget = record
    NodeID: TNodeID;
    NodeAlias: Word;
  end;

  { TFormTrainController }

  TFormTrainController = class(TForm)
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
    ToggleBoxThrottleForward: TToggleBox;
    ToggleBoxThrottleReverse: TToggleBox;
    TrackBarThrottleSpeed: TTrackBar;
    procedure ButtonHammerTestClick(Sender: TObject);
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

{ TLccThrottleConnectStateMachine }

function TLccThrottleConnectStateMachine.HandleAllocateThrottleState(
  LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  WorkerMsg.LoadTractionControllerAssign(SourceID, SourceAlias, DestID, DestAlias, SourceID, SourceAlias);
  SendMessage(Self, WorkerMsg);
  Inc(FState);
end;

function TLccThrottleConnectStateMachine.HandleAllocateThrottleStateReply(
  LccMessage: TLccMessage; var RefusedReason: TAllocateTrainRefusedReason
  ): Boolean;
begin
  case LccMessage.DataArray[2] of
    S_OK :
      begin
        Result := True;
        RefusedReason := rrNone;
        Inc(FState)
      end;
    TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER :
      begin
         Result := False;
         RefusedReason := rrAssignedControllerRefused;
      end;
    TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
      begin
        Result := False;
        RefusedReason := rrTrainRefused;
      end else
    begin
      Result := False;
      RefusedReason := rrUnknown
    end;
  end
end;

function TLccThrottleConnectStateMachine.HandleReleaseTrainState(
  LccMessage: TLccMessage): Boolean;
begin
  WorkerMsg.LoadTractionManage(SourceID, SourceAlias, DestID, DestAlias, False);
  SendMessage(Self, WorkerMsg);
  Result := True;
  Timer.Enabled := False;
  DoEnded;
end;

function TLccThrottleConnectStateMachine.HandleReserveTrainState(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  WorkerMsg.LoadTractionManage(SourceID, SourceAlias, DestID, DestAlias, True);
  SendMessage(Self, LccMessage);
  Inc(FState)
end;

function TLccThrottleConnectStateMachine.HandleReserveTrainStateReply(LccMessage: TLccMessage): Boolean;
begin
  if (LccMessage.DataArray[TRACTION_RESERVE_REPLY] = $01) and  (LccMessage.DataArray[2] = S_OK) then
  begin
    Inc(FState)
  end else
  begin
    Timer.Enabled := False;
    Result := False;
  end;
end;

function TLccThrottleConnectStateMachine.HandleTrainSearchState(SearchStr: string; TrackProtocolFlags: Word): Boolean;
var
  SearchData: DWORD;
begin
  Result := False;
  SearchData := 0;
  case WorkerMsg.TractionSearchEncodeSearchString(SearchStr, TrackProtocolFlags, SearchData) of
    sese_ok :
      begin
        WorkerMsg.LoadTractionSearch(SourceID, SourceAlias, SearchData);
        SendMessage(Self, WorkerMsg);
        Start;
        Inc(FState); // Next State to wait
        Result := True;
      end;
    sese_TooLong           : ShowMessage('Search String too long, only 6 characters are available');
    sese_InvalidCharacters : ShowMessage('Invalid character in search string, only 0-9 or F are valid');
  end;
end;

function TLccThrottleConnectStateMachine.HandleTrainSearchWaitReply(LccMessage: TLccMessage): Boolean;
var
  CanNode: TLccCanNode;
begin
  Result := True;
  // Pick up this node we are going to use as our train
  DestID := LccMessage.SourceID;
  DestAlias := LccMessage.CAN.SourceAlias;
  WorkerMsg.LoadTractionManage(SourceID, SourceAlias, DestID, DestAlias, True);
  SendMessage(Self, WorkerMsg);
  Inc(FState); // Next State to wait
end;

procedure TLccThrottleConnectStateMachine.StateMachineRun;
const
  STATE_SEARCH_TRAINS            = 0;
  STATE_SEARCH_TRAINS_WAIT       = 1;
  STATE_RESERVE_TRAIN            = 2;
  STATE_RESERVE_TRAIN_REPLY_WAIT = 3;
  STATE_ALLOCATE_THROTTLE        = 4;
  STATE_ALLOCATE_THROTTLE_WAIT   = 5;
  STATE_RELEASE_TRAIN            = 6;

begin
  case State of
    STATE_SEARCH_TRAINS :
    begin

    end;
    STATE_SEARCH_TRAINS_WAIT :
    begin

    end;
    STATE_RESERVE_TRAIN :
    begin
 //     WorkerMsg.LoadTractionManage(SourceID, SourceAlias, DestID, DestAlias, True);
 //     SendMessage(Self, WorkerMsg);
    end;
    STATE_RESERVE_TRAIN_REPLY_WAIT :
    begin

    end;
    STATE_ALLOCATE_THROTTLE :
    begin

    end;
    STATE_ALLOCATE_THROTTLE_WAIT :
    begin

    end;
    STATE_RELEASE_TRAIN :
    begin

    end;
  end;
end;

procedure TLccThrottleConnectStateMachine.WatchdogTimedOut;
begin

end;

{ TLccStateMachine }

constructor TLccStateMachine.Create(ASourceNodeID: TNodeID;
  ASourceAliasID: Word; SendMessageFunc: TOnMessageEvent;
  AnOnEndFunc: TNotifyEvent);
begin
  FTimer := TTimer.Create(nil);
  FWorkerMsg := TLccMessage.Create;
  Timer.OnStartTimer := @OnTimer;
  Timer.Interval := 50;
  FState := 0;
  FWatchdogCount := 0;
  FWatchdogTime := 200;  // set for 1 second
  FSendMessage := SendMessageFunc;
  FSourceAlias := ASourceAliasID;
  FSourceID := ASourceNodeID;
  FOnEnded := AnOnEndFunc;
end;

destructor TLccStateMachine.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNIl(FWorkerMsg);
  inherited Destroy;
end;

procedure TLccStateMachine.DoEnded;
begin
  if Assigned(OnEnded) then
    OnEnded(Self);
end;

procedure TLccStateMachine.OnTimer(Sender: TObject);
begin
  StateMachineRun;
  WatchdogCount := WatchdogCount + Timer.Interval;
  if WatchdogCount > WatchdogTime then
     beep
end;

procedure TLccStateMachine.Start;
begin
  Timer.Enabled := True;
end;

procedure TLccStateMachine.Stop;
begin
  Timer.Enabled := False;
end;

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

  ControllerNode.AssginTrainByOpenLCB(EditThrottleAddress.Text, TrackProtocolFlags);
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

