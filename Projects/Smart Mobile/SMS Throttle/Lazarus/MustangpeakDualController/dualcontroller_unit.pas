unit dualcontroller_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, lcc_node_manager, lcc_ethernet_client, lcc_node,
  lcc_node_controller, lcc_node_messages, lcc_defines, lcc_node_train, lcc_math_float16,
  throttle_takeover_request_form, lcc_common_classes, lcc_ethernet_common,
  Contnrs;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonConsistRelease1: TButton;
    ButtonConsistRelease2: TButton;
    ButtonBuildConstist1: TButton;
    ButtonBuildConstist2: TButton;
    ButtonReleaseConsist1: TButton;
    ButtonReleaseConsist2: TButton;
    ButtonConnect1: TButton;
    ButtonConnect2: TButton;
    CheckBoxConsistAddress1: TCheckBox;
    CheckBoxConsistAddress2: TCheckBox;
    CheckBoxForwardF0_1: TCheckBox;
    CheckBoxForwardF0_2: TCheckBox;
    CheckBoxForwardFn_1: TCheckBox;
    CheckBoxForwardFn_2: TCheckBox;
    CheckBoxThrottleTakeover1: TCheckBox;
    CheckBoxThrottleLongAddress1: TCheckBox;
    CheckBoxThrottleLongAddress2: TCheckBox;
    CheckBoxThrottleTakeover2: TCheckBox;
    EditConsistAddress1: TEdit;
    EditConsistAddress2: TEdit;
    EditThrottleAddress1: TEdit;
    EditCommandStationIPAddress: TEdit;
    EditThrottleAddress2: TEdit;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label103: TLabel;
    Label101: TLabel;
    Label100: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelAlias2: TLabel;
    LabelNodeID2: TLabel;
    LabelNodeID1: TLabel;
    LabelAlias1: TLabel;
    Label104: TLabel;
    LabelThrottleSpeed1: TLabel;
    LabelThrottleSpeed2: TLabel;
    PageControlConsists1: TPageControl;
    PageControlConsists2: TPageControl;
    PanelConsistCreate1: TPanel;
    PanelConsistCreate2: TPanel;
    PanelConsistRelease1: TPanel;
    PanelConsistRelease2: TPanel;
    PanelConsistEditor1: TPanel;
    PanelConsistEditor2: TPanel;
    PanelConsistWizard1: TPanel;
    PanelConsistWizard2: TPanel;
    PanelThrottleAssign2: TPanel;
    PanelThrottle1: TPanel;
    PanelTop: TPanel;
    PanelThrottle2: TPanel;
    PanelThrottleEthernet: TPanel;
    PanelThrottleAssign1: TPanel;
    PanelThrottleEthernet1: TPanel;
    PanelThrottleFace1: TPanel;
    PanelThrottleFace2: TPanel;
    PanelThrottleKeypad1: TPanel;
    PanelThrottleKeypad2: TPanel;
    RadioGroupConstistSpeedStep1: TRadioGroup;
    RadioGroupConstistSpeedStep2: TRadioGroup;
    RadioGroupThrottleSpeedSteps1: TRadioGroup;
    RadioGroupThrottleSpeedSteps2: TRadioGroup;
    SpeedButtonConsistSubtract1: TSpeedButton;
    SpeedButtonConsistSubtract2: TSpeedButton;
    SpeedButtonConsistTrainAdd1: TSpeedButton;
    SpeedButtonConstistTrainAdd2: TSpeedButton;
    SpeedButtonQuerySpeedThrottle1: TSpeedButton;
    SpeedButtonForward1: TSpeedButton;
    SpeedButtonForward2: TSpeedButton;
    SpeedButtonFunction0: TSpeedButton;
    SpeedButtonFunction1: TSpeedButton;
    SpeedButtonFunction10: TSpeedButton;
    SpeedButtonFunction11: TSpeedButton;
    SpeedButtonFunction12: TSpeedButton;
    SpeedButtonFunction13: TSpeedButton;
    SpeedButtonFunction14: TSpeedButton;
    SpeedButtonFunction15: TSpeedButton;
    SpeedButtonFunction16: TSpeedButton;
    SpeedButtonFunction17: TSpeedButton;
    SpeedButtonFunction18: TSpeedButton;
    SpeedButtonFunction19: TSpeedButton;
    SpeedButtonFunction2: TSpeedButton;
    SpeedButtonFunction20: TSpeedButton;
    SpeedButtonFunction21: TSpeedButton;
    SpeedButtonFunction22: TSpeedButton;
    SpeedButtonFunction23: TSpeedButton;
    SpeedButtonFunction3: TSpeedButton;
    SpeedButtonFunction4: TSpeedButton;
    SpeedButtonFunction5: TSpeedButton;
    SpeedButtonFunction6: TSpeedButton;
    SpeedButtonFunction7: TSpeedButton;
    SpeedButtonFunction8: TSpeedButton;
    SpeedButtonFunction9: TSpeedButton;
    SpeedButtonReverse1: TSpeedButton;
    SpeedButtonReverse2: TSpeedButton;
    SpeedButtonThrottleAssign1: TSpeedButton;
    SpeedButtonThrottleAssign2: TSpeedButton;
    StatusBarThrottle1: TStatusBar;
    StatusBarThrottle2: TStatusBar;
    TabSheetConsistNew1: TTabSheet;
    TabSheetConsists1: TTabSheet;
    TabSheetConsistNew2: TTabSheet;
    TabSheetConsists2: TTabSheet;
    TrackBarThrottle1: TTrackBar;
    TrackBarThrottle2: TTrackBar;
    TreeViewConsists1: TTreeView;
    TreeViewConsists2: TTreeView;
    TreeViewConsistWizard1: TTreeView;
    TreeViewConsistWizard2: TTreeView;
    procedure ButtonBuildConstist1Click(Sender: TObject);
    procedure ButtonBuildConstist2Click(Sender: TObject);
    procedure ButtonConnect1Click(Sender: TObject);
    procedure ButtonConnect2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure PageControlConsists1Change(Sender: TObject);
    procedure PageControlConsists2Change(Sender: TObject);
    procedure SpeedButtonConsistSubtract1Click(Sender: TObject);
    procedure SpeedButtonConsistSubtract2Click(Sender: TObject);
    procedure SpeedButtonConsistTrainAdd1Click(Sender: TObject);
    procedure SpeedButtonConstistTrainAdd2Click(Sender: TObject);
    procedure SpeedButtonForward1Click(Sender: TObject);
    procedure SpeedButtonForward2Click(Sender: TObject);
    procedure SpeedButtonReverse1Click(Sender: TObject);
    procedure SpeedButtonReverse2Click(Sender: TObject);
    procedure SpeedButtonThrottle2FunctionClick(Sender: TObject);
    procedure SpeedButtonThrottle1FunctionClick(Sender: TObject);
    procedure SpeedButtonThrottleAssign1Click(Sender: TObject);
    procedure SpeedButtonThrottleAssign2Click(Sender: TObject);
    procedure TrackBarThrottle1Change(Sender: TObject);
    procedure TrackBarThrottle2Change(Sender: TObject);
    procedure TreeViewConsistWizard1Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewConsistWizard1SelectionChanged(Sender: TObject);
    procedure TreeViewConsistWizard2Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewConsistWizard2SelectionChanged(Sender: TObject);
  private
    FWorkerMessage: TLccMessage;

  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    procedure OnNodeManager1IDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManager1AliasChange(Sender: TObject; LccSourceNode: TLccNode);

    procedure OnNodeManager2IDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManager2AliasChange(Sender: TObject; LccSourceNode: TLccNode);

    procedure OnClientServer1ConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnClientServer1ErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);

    procedure OnClientServer2ConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnClientServer2ErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);

    // The Controller is the Controller Node created in the NodeManager
    procedure ControllerTrainAssigned1(Sender: TLccNode; Reason: TControllerTrainAssignResult);
    procedure ControllerTrainAssigned2(Sender: TLccNode; Reason: TControllerTrainAssignResult);

    procedure ControllerTrainReleased1(Sender: TLccNode);
    procedure ControllerTrainReleased2(Sender: TLccNode);

    procedure OnControllerQuerySpeedReply1(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte);
    procedure OnControllerQueryFunctionReply1(Sender: TLccNode; Address: DWORD; Value: Word);

    procedure OnControllerQuerySpeedReply2(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte);
    procedure OnControllerQueryFunctionReply2(Sender: TLccNode; Address: DWORD; Value: Word);

    procedure OnControllerReqestTakeover1(Sender: TLccNode; var Allow: Boolean);
    procedure OnControllerReqestTakeover2(Sender: TLccNode; var Allow: Boolean);

    procedure OnControllerSearchResult1(Sender: TLccActionTrain; TrainList: TLccActionTrainList; var SelectedResultIndex: Integer);
    procedure OnControllerSearchResult2(Sender: TLccActionTrain; TrainList: TLccActionTrainList; var SelectedResultIndex: Integer);

    procedure OnControllerSearchMultiResult1(Sender: TLccActionTrain; Trains: TLccActionTrainList);
    procedure OnControllerSearchMultiResult2(Sender: TLccActionTrain; Trains: TLccActionTrainList);

    procedure OnControllerAttachListenerReply1(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word);
    procedure OnControllerAttachListenerReply2(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word);

    procedure OnControllerDetachListenerReply1(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word);
    procedure OnControllerDetachListenerReply2(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word);

    procedure OnControllerQueryListenerGetCount1(Sender: TLccNode; ListenerCount: Byte);
    procedure OnControllerQueryListenerGetCount2(Sender: TLccNode; ListenerCount: Byte);

    procedure OnControllerQueryListenerIndex1(Sender: TLccNode; ListenerCount, ListenerIndex: Byte; ListenerFlags: Byte; ListenerNodeID: TNodeID);
    procedure OnControllerQueryListenerIndex2(Sender: TLccNode; ListenerCount, ListenerIndex: Byte; ListenerFlags: Byte; ListenerNodeID: TNodeID);

    procedure ReleaseTrain1;
    procedure ReleaseTrain2;


  public
    NodeManager1: TLccNodeManager;
    ClientServer1: TLccEthernetClient;
    NodeManager2: TLccNodeManager;
    ClientServer2: TLccEthernetClient;

    ControllerNode1: TLccTrainController; // First Node created by the NodeManager, it is assigned when the Ethenetlink is established
    ControllerNode2: TLccTrainController; // First Node created by the NodeManager, it is assigned when the Ethenetlink is established

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonBuildConstist1Click(Sender: TObject);
var
  TreeNode: TTreeNode;
  DccSearchCriteria: TObjectList;
  ConsistItem: TDccSearchCriteria;
begin
  DccSearchCriteria := TObjectList.Create;
  DccSearchCriteria.OwnsObjects := False;
  try
    TreeNode := TreeViewConsistWizard1.Items.GetFirstNode;
    while Assigned(TreeNode) do
    begin
      ConsistItem := (TDccSearchCriteria( TreeNode.Data));
      DccSearchCriteria.Add( ConsistItem);
      TreeNode := TreeNode.GetNext;
    end;
  finally
    ControllerNode1.SearchTrainsByDccAddress(DccSearchCriteria, 100);
    FreeAndNil(DccSearchCriteria);
  end;
end;

procedure TForm1.ButtonBuildConstist2Click(Sender: TObject);
var
  TreeNode: TTreeNode;
  DccSearchCriteria: TObjectList;
  ConsistItem: TDccSearchCriteria;
begin
  DccSearchCriteria := TObjectList.Create;
  DccSearchCriteria.OwnsObjects := False;
  try
    TreeNode := TreeViewConsistWizard2.Items.GetFirstNode;
    while Assigned(TreeNode) do
    begin
      ConsistItem := (TDccSearchCriteria( TreeNode.Data));
      DccSearchCriteria.Add( ConsistItem);
      TreeNode := TreeNode.GetNext;
    end;
  finally
    ControllerNode2.SearchTrainsByDccAddress(DccSearchCriteria);
    FreeAndNil(DccSearchCriteria);
  end;
end;

procedure TForm1.ButtonConnect1Click(Sender: TObject);
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  if ClientServer1.Connected then
  begin
    if ControllerNode1.IsTrainAssigned then
      ReleaseTrain1;
    ClientServer1.CloseConnection(nil)
  end else
  begin
    LocalInfo := TLccEthernetConnectionInfo.Create;
    try
    LocalInfo.ListenerIP := EditCommandStationIPAddress.Text;
    LocalInfo.AutoResolveIP := True;
    LocalInfo.ListenerPort := 12021;
    LocalInfo.Gridconnect := True;
    ClientServer1.OpenConnection(LocalInfo);
    finally
      LocalInfo.Free;
    end
  end;
end;

procedure TForm1.ButtonConnect2Click(Sender: TObject);
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  if ClientServer2.Connected then
  begin
    if ControllerNode2.IsTrainAssigned then
      ReleaseTrain2;
    ClientServer2.CloseConnection(nil)
  end else
  begin
    LocalInfo := TLccEthernetConnectionInfo.Create;
    try
    LocalInfo.ListenerIP := EditCommandStationIPAddress.Text;
    LocalInfo.AutoResolveIP := True;
    LocalInfo.ListenerPort := 12021;
    LocalInfo.Gridconnect := True;
    ClientServer2.OpenConnection(LocalInfo);
    finally
      LocalInfo.Free;
    end;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  NodeManager1.Clear;
  ClientServer1.CloseConnection(nil);
  FreeAndNil(NodeManager1);

  NodeManager2.Clear;
  ClientServer2.CloseConnection(nil);
  FreeAndNil(NodeManager2);

  FreeAndNil(FWorkerMessage);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  NodeManager1 := TLccNodeManager.Create(nil, True);
  ClientServer1 := TLccEthernetClient.Create(nil, NodeManager1);

  NodeManager1.OnLccNodeAliasIDChanged := @OnNodeManager1AliasChange;
  NodeManager1.OnLccNodeIDChanged := @OnNodeManager1IDChange;

  ClientServer1.OnConnectionStateChange := @OnClientServer1ConnectionChange;
  ClientServer1.OnErrorMessage := @OnClientServer1ErrorMessage;

  NodeManager2 := TLccNodeManager.Create(nil, True);
  ClientServer2 := TLccEthernetClient.Create(nil, NodeManager2);

  NodeManager2.OnLccNodeAliasIDChanged := @OnNodeManager2AliasChange;
  NodeManager2.OnLccNodeIDChanged := @OnNodeManager2IDChange;

  ClientServer2.OnConnectionStateChange := @OnClientServer2ConnectionChange;
  ClientServer2.OnErrorMessage := @OnClientServer2ErrorMessage;

  PanelThrottleFace1.Enabled := False;
  PanelThrottleFace2.Enabled := False;
  PanelThrottleKeypad1.Enabled := False;
  PanelThrottleKeypad2.Enabled := False;

  WorkerMessage := TLccMessage.Create;
end;

procedure TForm1.SpeedButtonForward1Click(Sender: TObject);
begin
  if Assigned(ControllerNode1) then
  begin
    ControllerNode1.Direction := tdForward;
    SpeedButtonForward1.ImageIndex := 2;
    SpeedButtonReverse1.ImageIndex := -1;
  end;
end;

procedure TForm1.SpeedButtonForward2Click(Sender: TObject);
begin
  if Assigned(ControllerNode2) then
  begin
    ControllerNode2.Direction := tdForward;
    SpeedButtonForward2.ImageIndex := 2;
    SpeedButtonReverse2.ImageIndex := -1;
  end;
end;

procedure TForm1.SpeedButtonReverse1Click(Sender: TObject);
begin
  if Assigned(ControllerNode1) then
  begin
    ControllerNode1.Direction := tdReverse;
    SpeedButtonForward1.ImageIndex := -1;
    SpeedButtonReverse1.ImageIndex := 2;
  end;
end;

procedure TForm1.SpeedButtonReverse2Click(Sender: TObject);
begin
  if Assigned(ControllerNode2) then
  begin
    ControllerNode2.Direction := tdReverse;
    SpeedButtonForward2.ImageIndex := -1;
    SpeedButtonReverse2.ImageIndex := 2;
  end;
end;

procedure TForm1.SpeedButtonThrottle2FunctionClick(Sender: TObject);
var
  Value: Word;
begin
 if Assigned(ControllerNode2) then
  begin
    ControllerNode2.Functions[(Sender as TSpeedButton).Tag] := not ControllerNode2.Functions[(Sender as TSpeedButton).Tag];
    Value := ControllerNode2.Functions[(Sender as TSpeedButton).Tag];
    case (Sender as TSpeedButton).Tag of
       0 : begin if Value = 0 then SpeedButtonFunction12.ImageIndex := 0 else SpeedButtonFunction12.ImageIndex := 1; end;
       1 : begin if Value = 0 then SpeedButtonFunction13.ImageIndex := 0 else SpeedButtonFunction13.ImageIndex := 1; end;
       2 : begin if Value = 0 then SpeedButtonFunction14.ImageIndex := 0 else SpeedButtonFunction14.ImageIndex := 1; end;
       3 : begin if Value = 0 then SpeedButtonFunction15.ImageIndex := 0 else SpeedButtonFunction15.ImageIndex := 1; end;
       4 : begin if Value = 0 then SpeedButtonFunction16.ImageIndex := 0 else SpeedButtonFunction16.ImageIndex := 1; end;
       5 : begin if Value = 0 then SpeedButtonFunction17.ImageIndex := 0 else SpeedButtonFunction17.ImageIndex := 1; end;
       6 : begin if Value = 0 then SpeedButtonFunction18.ImageIndex := 0 else SpeedButtonFunction18.ImageIndex := 1; end;
       7 : begin if Value = 0 then SpeedButtonFunction19.ImageIndex := 0 else SpeedButtonFunction19.ImageIndex := 1; end;
       8 : begin if Value = 0 then SpeedButtonFunction20.ImageIndex := 0 else SpeedButtonFunction20.ImageIndex := 1; end;
       9 : begin if Value = 0 then SpeedButtonFunction21.ImageIndex := 0 else SpeedButtonFunction21.ImageIndex := 1; end;
       10 : begin if Value = 0 then SpeedButtonFunction22.ImageIndex := 0 else SpeedButtonFunction22.ImageIndex := 1; end;
       11 : begin if Value = 0 then SpeedButtonFunction23.ImageIndex := 0 else SpeedButtonFunction23.ImageIndex := 1; end;
    end;
  end;
end;

procedure TForm1.SpeedButtonThrottle1FunctionClick(Sender: TObject);
var
  Value: Word;
begin
  if Assigned(ControllerNode1) then
  begin
    ControllerNode1.Functions[(Sender as TSpeedButton).Tag] := not ControllerNode1.Functions[(Sender as TSpeedButton).Tag];
    Value := ControllerNode1.Functions[(Sender as TSpeedButton).Tag];
     case (Sender as TSpeedButton).Tag of
       0 : begin if Value = 0 then SpeedButtonFunction0.ImageIndex := 0 else SpeedButtonFunction0.ImageIndex := 1; end;
       1 : begin if Value = 0 then SpeedButtonFunction1.ImageIndex := 0 else SpeedButtonFunction1.ImageIndex := 1; end;
       2 : begin if Value = 0 then SpeedButtonFunction2.ImageIndex := 0 else SpeedButtonFunction2.ImageIndex := 1; end;
       3 : begin if Value = 0 then SpeedButtonFunction3.ImageIndex := 0 else SpeedButtonFunction3.ImageIndex := 1; end;
       4 : begin if Value = 0 then SpeedButtonFunction4.ImageIndex := 0 else SpeedButtonFunction4.ImageIndex := 1; end;
       5 : begin if Value = 0 then SpeedButtonFunction5.ImageIndex := 0 else SpeedButtonFunction5.ImageIndex := 1; end;
       6 : begin if Value = 0 then SpeedButtonFunction6.ImageIndex := 0 else SpeedButtonFunction6.ImageIndex := 1; end;
       7 : begin if Value = 0 then SpeedButtonFunction7.ImageIndex := 0 else SpeedButtonFunction7.ImageIndex := 1; end;
       8 : begin if Value = 0 then SpeedButtonFunction8.ImageIndex := 0 else SpeedButtonFunction8.ImageIndex := 1; end;
       9 : begin if Value = 0 then SpeedButtonFunction9.ImageIndex := 0 else SpeedButtonFunction9.ImageIndex := 1; end;
       10 : begin if Value = 0 then SpeedButtonFunction10.ImageIndex := 0 else SpeedButtonFunction10.ImageIndex := 1; end;
       11 : begin if Value = 0 then SpeedButtonFunction11.ImageIndex := 0 else SpeedButtonFunction11.ImageIndex := 1; end;
     end;
  end;
end;

procedure TForm1.SpeedButtonThrottleAssign1Click(Sender: TObject);
var
  DccAddress: LongInt;
begin
  if Assigned(ControllerNode1) then
  begin
    if ControllerNode1.IsTrainAssigned then
    begin
      ReleaseTrain1;
    end else
    begin
      // We will get a notification callback when the controller is assigned (or refused)
      if TryStrToInt(EditThrottleAddress1.Text, DccAddress) then
        ControllerNode1.AssignTrainByDccAddress(DccAddress, CheckBoxThrottleLongAddress1.Checked, IndexToSpeedStep(RadioGroupThrottleSpeedSteps1.ItemIndex + 1))
      else
        ShowMessage('Invalid Address');
    end;
  end;
end;

procedure TForm1.SpeedButtonThrottleAssign2Click(Sender: TObject);
var
  DccAddress: LongInt;
begin
  if Assigned(ControllerNode2) then
  begin
    if ControllerNode2.IsTrainAssigned then
    begin
      ReleaseTrain2;
    end else
    begin
      // We will get a notification callback when the controller is assigned (or refused)
      if TryStrToInt(EditThrottleAddress2.Text, DccAddress) then
        ControllerNode2.AssignTrainByDccAddress(DccAddress, CheckBoxThrottleLongAddress2.Checked, IndexToSpeedStep(RadioGroupThrottleSpeedSteps2.ItemIndex + 1))
      else
        ShowMessage('Invalid Address');
    end;
  end;
end;

procedure TForm1.TrackBarThrottle1Change(Sender: TObject);
begin
  if Assigned(ControllerNode1) then
  begin
    ControllerNode1.Speed := TrackBarThrottle1.Position;
    LabelThrottleSpeed1.Caption := IntToStr(TrackBarThrottle1.Position);
  end;
end;

procedure TForm1.TrackBarThrottle2Change(Sender: TObject);
begin
  if Assigned(ControllerNode2) then
  begin
    ControllerNode2.Speed := TrackBarThrottle2.Position;
    LabelThrottleSpeed2.Caption := IntToStr(TrackBarThrottle2.Position);
  end;
end;

procedure TForm1.TreeViewConsistWizard1Deletion(Sender: TObject; Node: TTreeNode);
var
  ConsistItem: TDccSearchCriteria;
begin
  ConsistItem := TDccSearchCriteria( Node.Data);
  FreeAndNil(ConsistItem);
end;

procedure TForm1.TreeViewConsistWizard1SelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
  ConsistItem: TDccSearchCriteria;
begin
  Node := TreeViewConsistWizard1.Selected;
  if Assigned(Node) then
  begin
    ConsistItem := TDccSearchCriteria(Node.Data);
    if Assigned(ConsistItem) then
    begin
      CheckBoxConsistAddress1.Checked := ConsistItem.LongAddress;
      RadioGroupConstistSpeedStep1.ItemIndex := SpeedStepToIndex(ConsistItem.SpeedStep) - 1;
      EditConsistAddress1.Text := IntToStr(ConsistItem.Address);
    end
  end else
  begin
    CheckBoxConsistAddress1.Checked := False;
    RadioGroupConstistSpeedStep1.ItemIndex := -1;
    EditConsistAddress1.Text := '';
  end;
end;

procedure TForm1.TreeViewConsistWizard2Deletion(Sender: TObject; Node: TTreeNode);
var
  ConsistItem: TDccSearchCriteria;
begin
  ConsistItem := TDccSearchCriteria( Node.Data);
  FreeAndNil(ConsistItem);
end;

procedure TForm1.TreeViewConsistWizard2SelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
  ConsistItem: TDccSearchCriteria;
begin
  Node := TreeViewConsistWizard2.Selected;
  if Assigned(Node) then
  begin
    ConsistItem := TDccSearchCriteria(Node.Data);
    if Assigned(ConsistItem) then
    begin
      CheckBoxConsistAddress2.Checked := ConsistItem.LongAddress;
      RadioGroupConstistSpeedStep2.ItemIndex := SpeedStepToIndex(ConsistItem.SpeedStep) - 1;
      EditConsistAddress2.Text := IntToStr(ConsistItem.Address);
    end
  end else
  begin
    CheckBoxConsistAddress2.Checked := False;
    RadioGroupConstistSpeedStep2.ItemIndex := -1;
    EditConsistAddress2.Text := '';
  end;
end;

procedure TForm1.OnClientServer1ConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  if Sender is TLccConnectionThread then
  begin
    case (Info as TLccEthernetConnectionInfo).ConnectionState of
      lcsConnecting : StatusBarThrottle1.Panels[0].Text    := 'Connecting';
      lcsConnected  :
        begin
          ButtonConnect1.Caption := 'Disconnect';
          StatusBarThrottle1.Panels[0].Text := 'IP Address: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
          ControllerNode1 := NodeManager1.AddNodeByClass('', TLccTrainController, True) as TLccTrainController;
          ControllerNode1.OnTrainAssigned := @ControllerTrainAssigned1;
          ControllerNode1.OnTrainReleased := @ControllerTrainReleased1;
          ControllerNode1.OnControllerRequestTakeover := @OnControllerReqestTakeover1;
          ControllerNode1.OnQuerySpeedReply := @OnControllerQuerySpeedReply1;
          ControllerNode1.OnQueryFunctionReply := @OnControllerQueryFunctionReply1;
          ControllerNode1.OnSearchResult := @OnControllerSearchResult1;
          ControllerNode1.OnSearchMultiResult := @OnControllerSearchMultiResult1;
          ControllerNode1.OnAttachListenerReply := @OnControllerAttachListenerReply1;
          ControllerNode1.OnDetachListenerReply := @OnControllerDetachListenerReply1;
          ControllerNode1.OnQueryListenerGetCount := @OnControllerQueryListenerGetCount1;
          ControllerNode1.OnQueryListenerIndex := @OnControllerQueryListenerIndex1;
          PanelThrottleFace1.Enabled := True;
        end;
      lcsDisconnecting :
        begin
          StatusBarThrottle1.Panels[0].Text := 'Disconnecting';
          NodeManager1.Clear;
          ControllerNode1 := nil;
        end;
      lcsDisconnected :
        begin
          StatusBarThrottle1.Panels[0].Text := 'Disconnected';
          ButtonConnect1.Caption := 'Connect';
          LabelAlias1.Caption := 'None';
          LabelNodeID1.Caption := 'None';
          PanelThrottleFace1.Enabled := False;
        end;
    end;
  end
end;

procedure TForm1.OnClientServer1ErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  ShowMessage(Info.MessageStr);
  NodeManager1.Clear;
  StatusBarThrottle1.Panels[0].Text := 'Disconnected';
  ButtonConnect1.Caption := 'Connect';
end;

procedure TForm1.OnClientServer2ConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  if Sender is TLccConnectionThread then
  begin
    case (Info as TLccEthernetConnectionInfo).ConnectionState of
      lcsConnecting : StatusBarThrottle2.Panels[0].Text    := 'Connecting';
      lcsConnected  :
        begin
          ButtonConnect2.Caption := 'Disconnect';
          StatusBarThrottle2.Panels[0].Text := 'IP Address: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
          ControllerNode2 := NodeManager2.AddNodeByClass('', TLccTrainController, True) as TLccTrainController;
          ControllerNode2.OnTrainAssigned := @ControllerTrainAssigned2;
          ControllerNode2.OnTrainReleased := @ControllerTrainReleased2;
          ControllerNode2.OnControllerRequestTakeover := @OnControllerReqestTakeover2;
          ControllerNode2.OnQuerySpeedReply := @OnControllerQuerySpeedReply2;
          ControllerNode2.OnQueryFunctionReply := @OnControllerQueryFunctionReply2;
          ControllerNode2.OnSearchResult := @OnControllerSearchResult2;
          ControllerNode2.OnSearchMultiResult := @OnControllerSearchMultiResult2;
          ControllerNode2.OnAttachListenerReply:= @OnControllerAttachListenerReply2;
          ControllerNode2.OnDetachListenerReply := @OnControllerDetachListenerReply2;
          ControllerNode2.OnQueryListenerGetCount := @OnControllerQueryListenerGetCount2;
          ControllerNode2.OnQueryListenerIndex := @OnControllerQueryListenerIndex2;
          PanelThrottleFace2.Enabled := True;
        end;
      lcsDisconnecting :
        begin
          StatusBarThrottle2.Panels[0].Text := 'Disconnecting';
          NodeManager2.Clear;
          ControllerNode2 := nil;
        end;
      lcsDisconnected :
        begin
          StatusBarThrottle2.Panels[0].Text := 'Disconnected';
          ButtonConnect2.Caption := 'Connect';
          LabelAlias2.Caption := 'None';
          LabelNodeID2.Caption := 'None';
          PanelThrottleFace2.Enabled := False;
        end;
    end;
  end;
end;

procedure TForm1.OnClientServer2ErrorMessage(Sender: TObject;
  Info: TLccHardwareConnectionInfo);
begin
  ShowMessage(Info.MessageStr);
  NodeManager2.Clear;
  StatusBarThrottle2.Panels[0].Text := 'Disconnected';
  ButtonConnect2.Caption := 'Connect';
end;

procedure TForm1.ControllerTrainAssigned1(Sender: TLccNode; Reason: TControllerTrainAssignResult);
begin
  case Reason of
    tarAssigned :
      begin
        ControllerNode1.QuerySpeed;
        ControllerNode1.QueryFunctions;
        PanelThrottleKeypad1.Enabled := True;
        SpeedButtonThrottleAssign1.Caption := 'Release Train';
      end;
    tarFailTrainRefused      : ShowMessage('Train refused assignment to controller');
    tarFailControllerRefused : ShowMessage('Current controller refused to release train');
  else
    ShowMessage('Unknown ControllerTrainAssigned1 result');
  end;
end;

procedure TForm1.ControllerTrainAssigned2(Sender: TLccNode; Reason: TControllerTrainAssignResult);
begin
  case Reason of
    tarAssigned :
      begin
        ControllerNode2.QuerySpeed;
        ControllerNode2.QueryFunctions;
        PanelThrottleKeypad2.Enabled := True;
        SpeedButtonThrottleAssign2.Caption := 'Release Train';
      end;
    tarFailTrainRefused      : ShowMessage('Train refused assignment to controller');
    tarFailControllerRefused : ShowMessage('Current controller refused to release train');
  else
    ShowMessage('Unknown ControllerTrainAssigned2 result');
  end;
end;

procedure TForm1.ControllerTrainReleased1(Sender: TLccNode);
begin
  PanelThrottleKeypad1.Enabled := False;
  SpeedButtonThrottleAssign1.Caption := 'Allocate Train';
end;

procedure TForm1.ControllerTrainReleased2(Sender: TLccNode);
begin
  PanelThrottleKeypad2.Enabled := False;
  SpeedButtonThrottleAssign2.Caption := 'Allocate Train';
end;

procedure TForm1.OnControllerQueryFunctionReply1(Sender: TLccNode; Address: DWORD; Value: Word);
begin
  ControllerNode1.Functions[Address] := Value;
  case Address of
    0 : begin if Value = 0 then SpeedButtonFunction0.ImageIndex := 0 else SpeedButtonFunction0.ImageIndex := 1; end;
    1 : begin if Value = 0 then SpeedButtonFunction1.ImageIndex := 0 else SpeedButtonFunction1.ImageIndex := 1; end;
    2 : begin if Value = 0 then SpeedButtonFunction2.ImageIndex := 0 else SpeedButtonFunction2.ImageIndex := 1; end;
    3 : begin if Value = 0 then SpeedButtonFunction3.ImageIndex := 0 else SpeedButtonFunction3.ImageIndex := 1; end;
    4 : begin if Value = 0 then SpeedButtonFunction4.ImageIndex := 0 else SpeedButtonFunction4.ImageIndex := 1; end;
    5 : begin if Value = 0 then SpeedButtonFunction5.ImageIndex := 0 else SpeedButtonFunction5.ImageIndex := 1; end;
    6 : begin if Value = 0 then SpeedButtonFunction6.ImageIndex := 0 else SpeedButtonFunction6.ImageIndex := 1; end;
    7 : begin if Value = 0 then SpeedButtonFunction7.ImageIndex := 0 else SpeedButtonFunction7.ImageIndex := 1; end;
    8 : begin if Value = 0 then SpeedButtonFunction8.ImageIndex := 0 else SpeedButtonFunction8.ImageIndex := 1; end;
    9 : begin if Value = 0 then SpeedButtonFunction9.ImageIndex := 0 else SpeedButtonFunction9.ImageIndex := 1; end;
    10 : begin if Value = 0 then SpeedButtonFunction10.ImageIndex := 0 else SpeedButtonFunction10.ImageIndex := 1; end;
    11 : begin if Value = 0 then SpeedButtonFunction11.ImageIndex := 0 else SpeedButtonFunction11.ImageIndex := 1; end;
  end;
end;

procedure TForm1.OnControllerQueryFunctionReply2(Sender: TLccNode; Address: DWORD; Value: Word);
begin
    ControllerNode2.Functions[Address] := Value;
    case Address of
     0 : begin if Value = 0 then SpeedButtonFunction12.ImageIndex := 0 else SpeedButtonFunction12.ImageIndex := 1; end;
     1 : begin if Value = 0 then SpeedButtonFunction13.ImageIndex := 0 else SpeedButtonFunction13.ImageIndex := 1; end;
     2 : begin if Value = 0 then SpeedButtonFunction14.ImageIndex := 0 else SpeedButtonFunction14.ImageIndex := 1; end;
     3 : begin if Value = 0 then SpeedButtonFunction15.ImageIndex := 0 else SpeedButtonFunction15.ImageIndex := 1; end;
     4 : begin if Value = 0 then SpeedButtonFunction16.ImageIndex := 0 else SpeedButtonFunction16.ImageIndex := 1; end;
     5 : begin if Value = 0 then SpeedButtonFunction17.ImageIndex := 0 else SpeedButtonFunction17.ImageIndex := 1; end;
     6 : begin if Value = 0 then SpeedButtonFunction18.ImageIndex := 0 else SpeedButtonFunction18.ImageIndex := 1; end;
     7 : begin if Value = 0 then SpeedButtonFunction19.ImageIndex := 0 else SpeedButtonFunction19.ImageIndex := 1; end;
     8 : begin if Value = 0 then SpeedButtonFunction20.ImageIndex := 0 else SpeedButtonFunction20.ImageIndex := 1; end;
     9 : begin if Value = 0 then SpeedButtonFunction21.ImageIndex := 0 else SpeedButtonFunction21.ImageIndex := 1; end;
     10 : begin if Value = 0 then SpeedButtonFunction22.ImageIndex := 0 else SpeedButtonFunction22.ImageIndex := 1; end;
     11 : begin if Value = 0 then SpeedButtonFunction23.ImageIndex := 0 else SpeedButtonFunction23.ImageIndex := 1; end;
    end;
end;

procedure TForm1.OnControllerQuerySpeedReply1(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte);
begin
  TrackBarThrottle1.Position := Abs( Round(HalfToFloat(SetSpeed)));

  if HalfIsNegative(SetSpeed) then
  begin
    SpeedButtonForward1.ImageIndex := -1;
    SpeedButtonReverse1.ImageIndex := 2;
  end else
  begin
    SpeedButtonForward1.ImageIndex := 2;
    SpeedButtonReverse1.ImageIndex := -1;
  end;
end;

procedure TForm1.OnControllerQuerySpeedReply2(Sender: TLccNode; SetSpeed, CommandSpeed, ActualSpeed: THalfFloat; Status: Byte);
begin
  TrackBarThrottle2.Position := Abs( Round(HalfToFloat(SetSpeed)));

  if HalfIsNegative(SetSpeed) then
  begin
    SpeedButtonForward2.ImageIndex := -1;
    SpeedButtonReverse2.ImageIndex := 2;
  end else
  begin
    SpeedButtonForward2.ImageIndex := 2;
    SpeedButtonReverse2.ImageIndex := -1;
  end;
end;

procedure TForm1.OnControllerReqestTakeover1(Sender: TLccNode; var Allow: Boolean);
begin
  if CheckBoxThrottleTakeover1.Checked then
    Allow :=  FormThrottleTakeover.ShowModal = mrYes;
  if Allow then
    ReleaseTrain1;
end;

procedure TForm1.OnControllerReqestTakeover2(Sender: TLccNode; var Allow: Boolean);
begin
  if CheckBoxThrottleTakeover2.Checked then
    Allow :=  FormThrottleTakeover.ShowModal = mrYes;
  if Allow then
    ReleaseTrain2;
end;

procedure TForm1.OnControllerSearchMultiResult1(Sender: TLccActionTrain; Trains: TLccActionTrainList);
begin
  if (Sender.UniqueID = 100) and (Trains.Count > 1) then
  begin
    // This is a search for a consist
    ControllerNode1.ListenerAttach(Trains[0].NodeID, Trains[0].AliasID, Trains[1].NodeID, 200);
    ControllerNode1.ListenerAttach(Trains[1].NodeID, Trains[1].AliasID, Trains[0].NodeID, 200);
  end;
  ShowMessage('Found Trains: ' + IntToStr(Trains.Count));
end;

procedure TForm1.OnControllerSearchMultiResult2(Sender: TLccActionTrain; Trains: TLccActionTrainList);
begin
  ShowMessage('Found Trains: ' + IntToStr(Trains.Count));
end;

procedure TForm1.OnControllerSearchResult1(Sender: TLccActionTrain; TrainList: TLccActionTrainList; var SelectedResultIndex: Integer);
begin
  SelectedResultIndex := 0;
  case TrainList.Count of
   0: ShowMessage('No Search Results');
   1: SelectedResultIndex := 0;
  else
    ShowMessage('Multiple Search Results: Please Select');
  end;
end;

procedure TForm1.OnControllerSearchResult2(Sender: TLccActionTrain; TrainList: TLccActionTrainList; var SelectedResultIndex: Integer);
begin
  SelectedResultIndex := 0;
  case TrainList.Count of
   0: ShowMessage('No Search Results');
   1: SelectedResultIndex := 0;
  else
    ShowMessage('Multiple Search Results: Please Select');
  end;
end;

procedure TForm1.OnControllerAttachListenerReply1(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word);
begin

end;

procedure TForm1.OnControllerAttachListenerReply2(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word);
begin

end;

procedure TForm1.OnControllerDetachListenerReply1(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word);
begin

end;

procedure TForm1.OnControllerDetachListenerReply2(Sender: TLccNode; ListenerNodeID: TNodeID; ReplyCode: Word);
begin

end;

procedure TForm1.OnControllerQueryListenerGetCount1(Sender: TLccNode; ListenerCount: Byte);
begin

end;

procedure TForm1.OnControllerQueryListenerGetCount2(Sender: TLccNode;
  ListenerCount: Byte);
begin

end;

procedure TForm1.OnControllerQueryListenerIndex1(Sender: TLccNode;
  ListenerCount, ListenerIndex: Byte; ListenerFlags: Byte;
  ListenerNodeID: TNodeID);
begin

end;

procedure TForm1.OnControllerQueryListenerIndex2(Sender: TLccNode;
  ListenerCount, ListenerIndex: Byte; ListenerFlags: Byte;
  ListenerNodeID: TNodeID);
begin

end;

procedure TForm1.ReleaseTrain1;
begin
  if Assigned(ControllerNode1) then
    ControllerNode1.ReleaseTrain;
end;

procedure TForm1.ReleaseTrain2;
begin
  if Assigned(ControllerNode2) then
    ControllerNode2.ReleaseTrain;
end;

procedure TForm1.SpeedButtonConsistSubtract1Click(Sender: TObject);
var
  TreeNode: TTreeNode;
begin
  TreeNode := TreeViewConsistWizard1.Selected;
  if Assigned(TreeNode) then
    TreeViewConsistWizard1.Items.Delete(TreeNode);  // Object in Data will be free in the deletion callback event
end;

procedure TForm1.SpeedButtonConsistSubtract2Click(Sender: TObject);
var
  TreeNode: TTreeNode;
begin
  TreeNode := TreeViewConsistWizard2.Selected;
  if Assigned(TreeNode) then
    TreeViewConsistWizard2.Items.Delete(TreeNode); // Object in Data will be free in the deletion callback event
end;

procedure TForm1.SpeedButtonConsistTrainAdd1Click(Sender: TObject);
var
  SelectedNode: TTreeNode;
  ConsistItem: TDccSearchCriteria;
  NodeCaption: string;
  DccAddress: Integer;
begin
  if TryStrToInt(EditConsistAddress1.Text, DccAddress) then
  begin
    ConsistItem := TDccSearchCriteria.Create('', DccAddress, IndexToSpeedStep(RadioGroupConstistSpeedStep1.ItemIndex + 1), CheckBoxConsistAddress1.Checked);
    NodeCaption := EditConsistAddress1.Text + ': ' + AddressBooleanToText(ConsistItem.LongAddress, True);

    if TreeViewConsistWizard1.Items.FindNodeWithText(NodeCaption) = nil then // No duplicates
    begin
      SelectedNode := TreeViewConsistWizard1.Items.AddChild(nil, NodeCaption);
      TreeViewConsistWizard1.Selected := SelectedNode;
      SelectedNode.Data := ConsistItem;
    end
  end else
    ShowMessage('Enter a valid DCC Address');
end;

procedure TForm1.SpeedButtonConstistTrainAdd2Click(Sender: TObject);
var
  SelectedNode: TTreeNode;
  ConsistItem: TDccSearchCriteria;
  NodeCaption: string;
  DccAddress: Integer;
begin
  if TryStrToInt(EditConsistAddress2.Text, DccAddress) then
  begin
    ConsistItem := TDccSearchCriteria.Create('', DccAddress, IndexToSpeedStep(RadioGroupConstistSpeedStep2.ItemIndex + 1), CheckBoxConsistAddress2.Checked);
    NodeCaption := EditConsistAddress2.Text + ': ' + AddressBooleanToText(ConsistItem.LongAddress, True);

    if TreeViewConsistWizard2.Items.FindNodeWithText(NodeCaption) = nil then // No duplicates
    begin
      SelectedNode := TreeViewConsistWizard2.Items.AddChild(nil, NodeCaption);
      TreeViewConsistWizard2.Selected := SelectedNode;
      SelectedNode.Data := ConsistItem;
    end
  end else
    ShowMessage('Enter a valid DCC Address');
end;

procedure TForm1.OnNodeManager1AliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelAlias1.Caption := 'NodeID: ' + LccSourceNode.AliasIDStr;
end;

procedure TForm1.OnNodeManager1IDChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelNodeID1.Caption := 'NodeID: ' + LccSourceNode.NodeIDStr;
end;


procedure TForm1.OnNodeManager2AliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelAlias2.Caption := 'NodeID: ' + LccSourceNode.AliasIDStr;
end;

procedure TForm1.OnNodeManager2IDChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelNodeID2.Caption := 'NodeID: ' + LccSourceNode.NodeIDStr;
end;

procedure TForm1.PageControlConsists1Change(Sender: TObject);
begin
  if PageControlConsists1.ActivePage = TabSheetConsistNew1 then
  begin
    PanelConsistCreate1.Enabled := True;
    PanelConsistRelease1.Enabled := False;
  end else
  begin
    PanelConsistCreate1.Enabled := False;
    PanelConsistRelease1.Enabled := True;
  end;
end;

procedure TForm1.PageControlConsists2Change(Sender: TObject);
begin
  if PageControlConsists2.ActivePage = TabSheetConsistNew2 then
  begin
    PanelConsistCreate2.Enabled := True;
    PanelConsistRelease2.Enabled := False;
  end else
  begin
    PanelConsistCreate2.Enabled := False;
    PanelConsistRelease2.Enabled := True;
  end;
end;

end.

