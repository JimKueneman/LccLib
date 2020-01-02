unit TabSettingsForm;

interface

uses 
  System.Types,
  System.Types.Convert,
  System.Objects,
  System.Time,
  System.IOUtils,
  System.Device.Storage,
  SmartCL.System,
  SmartCL.Time,
  SmartCL.Graphics,
  SmartCL.Components,
  SmartCL.FileUtils,
  SmartCL.Device.Storage,
  SmartCL.Forms,
  SmartCL.Fonts,
  SmartCL.Theme,
  SmartCL.Borders,
  SmartCL.Application,
  SmartCL.Controls,
  SmartCL.Net.WebSocket,
  SmartCL.Controls.Panel,
  SmartCL.Controls.EditBox,
  SmartCL.Controls.Label,
  SmartCL.Controls.CheckBox,
  SmartCL.Controls.Button,
  SmartCL.Controls.ListBox,
  System.Memory,
  System.Memory.Views,
  System.Memory.Buffer,
  System.Memory.Allocation,
  System.Streams,
  System.Lists,
  Storage,
  lcc_node_manager,
  lcc_node,
  lcc_node_messages,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_defines,
  lcc_math_float16,
  LccNode;

type
  TTabSettingsForm = class(TW3Form)
    procedure W3ButtonClearMessagesClick(Sender: TObject);
    procedure W3Button1Click(Sender: TObject);
    procedure W3ButtonStartNodeClick(Sender: TObject);
    procedure W3ButtonConnectionClick(Sender: TObject);
  private
    {$I 'TabSettingsForm:intf'}
    FSocket: TW3WebSocket;
    FConnected: Boolean;

  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;

    procedure CallbackNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure CallbackNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure CallbackNodeDestroy(Sender: TObject; LccSourceNode: TLccNode);
    procedure CallbackNodeCreate(Sender: TObject; LccSourceNode: TLccNode);

    procedure SendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
  public
    CanNodeManager: TLccCanNodeManager;
    MessageList: TStringList;
  end;

implementation

{ TTabSettingsForm }

procedure TTabSettingsForm.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  CanNodeManager := TLccCanNodeManager.Create(nil);
  MessageList := TStringList.Create;
  CanNodeManager.OnLccMessageSend := @SendMessage;
  CanNodeManager.OnLccNodeAliasIDChanged := @CallbackNodeAliasChange;
  CanNodeManager.OnLccNodeIDChanged := @CallbackNodeIDChange;
  CanNodeManager.OnLccNodeDestroy := @CallbackNodeDestroy;
  CanNodeManager.OnLccNodeCreate := @CallbackNodeCreate;
  //CanNodeManager.OnLccMessageReceive := @ReceiveMessage;  This causes recurision
end;

procedure TTabSettingsForm.InitializeObject;
var
  LccMessage: TLccMessage;
begin
  inherited;
  {$I 'TabSettingsForm:impl'}
  FSocket := TW3WebSocket.Create;
  FSocket.OnOpen := procedure (Sender: TW3WebSocket)
    begin
      FConnected := True;
    end;

  FSocket.OnClosed := procedure (Sender: TW3WebSocket)
    begin
      FConnected := False;
    end;

  FSocket.OnError := procedure (Sender: TW3WebSocket)
    begin
      ShowMessage('Socket Error');
    end;

  FSocket.OnMessage := procedure (Sender: TW3WebSocket; Message: TWebSocketMessageData)
    begin
       LccMessage := TLccMessage.Create;
       try
         LccMessage.LoadByGridConnectStr(Message.mdText);
         if W3CheckBoxLogging.Checked then
           W3ListBoxLog.Add('R: ' + Trim(Message.mdText));
         WriteLn('R: ' + Message.mdText);
         ReceiveMessage(Self, LccMessage);
       finally
         LccMessage.Free;
       end;
    end;
end;
 
procedure TTabSettingsForm.Resize;
begin
  inherited;
end;


procedure TTabSettingsForm.W3ButtonStartNodeClick(Sender: TObject);
var
  CanNode: TLccCanNode;
  BinaryByteArray: TDynamicByteArray;
begin
  if CanNodeManager.Nodes.Count = 0 then
  begin
    CanNode := CanNodeManager.AddNode(CDI_XML) as TLccCanNode;
    CanNode.ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
    CanNode.ProtocolSupportedProtocols.Datagram := True;
    CanNode.ProtocolSupportedProtocols.EventExchange := True;
    CanNode.ProtocolSupportedProtocols.SimpleNodeInfo := True;

    CanNode.ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
    CanNode.ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
    CanNode.ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);

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
    CanNode.ProtocolMemoryOptions.WriteStream := True;
    CanNode.ProtocolMemoryOptions.HighSpace := MSI_CDI;
    CanNode.ProtocolMemoryOptions.LowSpace := MSI_CONFIG;

    CanNode.ProtocolEventConsumed.AutoGenerate.Count := 5;
    CanNode.ProtocolEventConsumed.AutoGenerate.StartIndex := 0;

    CanNode.ProtocolEventsProduced.AutoGenerate.Count := 5;
    CanNode.ProtocolEventsProduced.AutoGenerate.StartIndex := 0;

    CanNode.Login(NULL_NODE_ID); // Create our own ID

    lcc_defines.Max_Allowed_Buffers := 1; // HACK ALLERT: Allow OpenLCB Python Scripts to run

    W3ButtonStartNode.Caption := 'Stop Node';
  end else
  begin
    W3ButtonStartNode.Caption := 'Start Node';
    CanNodeManager.Clear;
  end;
end;

procedure TTabSettingsForm.W3ButtonClearMessagesClick(Sender: TObject);
begin
  W3ListBoxLog.Clear;
end;

procedure TTabSettingsForm.W3Button1Click(Sender: TObject);
var
  s: single;
  Float16: THalfFloat;
begin
  s := SizeOf(s);
  s := s + 3;
  s := StrToFloat(W3EditBox1.Text);
  Float16 := FloatToHalf(s);
  W3Label3.Caption := '0x' + IntToHex(Float16, 8);
  s := HalfToFloat(Float16);
  W3Label4.Caption := FloatToStr(s);
end;

procedure TTabSettingsForm.W3ButtonConnectionClick(Sender: TObject);
var
  URL: string;

begin
  if FSocket.Connected or FConnected then
  begin
    W3ButtonStartNodeClick(W3ButtonStartNode);
    FSocket.Disconnect(procedure (Socket: TW3WebSocket; Success: boolean)
      begin
        if Success then
        begin
  //        ShowMessage('Success');
          W3ButtonConnection.Caption := 'Open';
        end else
        begin
   //       ShowMessage('Failure');
          W3ButtonConnection.Caption := 'Close'
        end;
      end
    )
  end else
  begin
    URL := 'ws://' + W3EditBoxIpAddress.Text + ':' + W3EditBoxIpPort.Text;
    try
      if W3CheckBoxTcp.Checked then
        FSocket.BinaryMessageDataType := wdtArrayBuffer;
      FSocket.Connect(URL, ['openlcb.websocket'], procedure (Socket: TW3WebSocket; Success: boolean)
        begin
          if Success then
          begin
    //        ShowMessage('Success');
            W3ButtonConnection.Caption := 'Close';
          end else
          begin
    //        ShowMessage('Failure');
            W3ButtonConnection.Caption := 'Open'
          end;
        end
    )
    except
        ShowMessage('Except');
      end;
  end;
end;

procedure TTabSettingsForm.SendMessage(Sender: TObject; LccMessage: TLccMessage);
var
  i: Integer;
begin
  MessageList.Text := LccMessage.ConvertToGridConnectStr(#10);
  for i := 0 to MessageList.Count- 1 do
  begin
    if W3CheckBoxLogging.Checked then
      W3ListBoxLog.Add('S: ' + Trim(MessageList[i]));
    WriteLn('S: ' + MessageList[i]);
    FSocket.Write(MessageList[i]);
  end;
end;

procedure TTabSettingsForm.ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  CanNodeManager.ProcessMessage(LccMessage);
end;

procedure TTabSettingsForm.CallbackNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  W3LabelNodeID.Caption := LccSourceNode.NodeIDStr
end;

procedure TTabSettingsForm.CallbackNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  W3LabelAlias.Caption := (LccSourceNode as TLccCanNode).AliasIDStr
end;

procedure TTabSettingsForm.CallbackNodeDestroy(Sender: TObject; LccSourceNode: TLccNode);
begin
  W3LabelNodeID.Caption := 'None';
  W3LabelAlias.Caption := 'None';
end;

procedure TTabSettingsForm.CallbackNodeCreate(Sender: TObject; LccSourceNode: TLccNode);
begin

end;

initialization
  Forms.RegisterForm({$I %FILE%}, TTabSettingsForm);
end.
