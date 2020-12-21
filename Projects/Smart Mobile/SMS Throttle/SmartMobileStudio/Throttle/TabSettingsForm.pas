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
  LccNode,
  lcc_node_manager,
  lcc_node,
  lcc_node_controller,
  lcc_node_messages,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_defines,
  lcc_math_float16;

type
  TTabSettingsForm = class(TW3Form)
    procedure W3ButtonClearMessagesClick(Sender: TObject);
    procedure W3ButtonStartNodeClick(Sender: TObject);
    procedure W3ButtonConnectionClick(Sender: TObject);
  private
    {$I 'TabSettingsForm:intf'}
    FSocket: TW3WebSocket;
    FConnected: Boolean;

  protected
    ControllerManager: TControllerManager;

    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;

    procedure CallbackNodeIDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure CallbackNodeAliasChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure CallbackNodeDestroy(Sender: TObject; LccSourceNode: TLccNode);

    procedure SendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);

    procedure WebSocketConnectCallback(Socket: TW3WebSocket; Success: boolean);
  public
    MessageList: TStringList;
  end;

implementation

{ TTabSettingsForm }

procedure TTabSettingsForm.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  ControllerManager := GetControllerManager;
  MessageList := TStringList.Create;

    // Javascript limitation, can't do it in InitializeForm or InitialzeObject :(
 // W3ButtonStartNode.Enabled := False;
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
begin
  if not ControllerManager.ControllerCreated then
  begin
    ControllerManager.CreateController;
    W3ButtonStartNode.Caption := 'Stop Node';
  end else
  begin
    W3ButtonStartNode.Caption := 'Start Node';
    ControllerManager.DestroyController;
  end;
end;

procedure TTabSettingsForm.W3ButtonClearMessagesClick(Sender: TObject);
begin
  W3ListBoxLog.Clear;
end;

procedure TTabSettingsForm.W3ButtonConnectionClick(Sender: TObject);
var
  URL: string;

begin
  // Javascript limitation, can't do it in InitializeForm or InitialzeObject :(
  ControllerManager.NodeManager.OnLccMessageSend := @SendMessage;
  ControllerManager.NodeManager.OnLccNodeAliasIDChanged := @CallbackNodeAliasChange;
  ControllerManager.NodeManager.OnLccNodeIDChanged := @CallbackNodeIDChange;
  ControllerManager.NodeManager.OnLccNodeDestroy := @CallbackNodeDestroy;
  //CanNodeManager.OnLccMessageReceive := @ReceiveMessage;  This causes recurision


  if FSocket.Connected or FConnected then
  begin
    FSocket.Disconnect(
      procedure (Socket: TW3WebSocket; Success: boolean)
      begin
        if Success then
        begin
          W3ButtonConnection.Caption := 'Connect';
          W3ButtonStartNode.Enabled := False;
        end else
        begin
          W3ButtonConnection.Caption := 'Disconnect';
          W3ButtonStartNode.Enabled := True;
        end;
      end )
  end else
  begin
    URL := 'ws://' + W3EditBoxIpAddress.Text + ':' + W3EditBoxIpPort.Text;
    try
      if W3CheckBoxTcp.Checked then
        FSocket.BinaryMessageDataType := wdtArrayBuffer;
      FSocket.Connect(URL, ['openlcb.websocket'], @WebSocketConnectCallback);

    {    procedure (Socket: TW3WebSocket; Success: boolean)
        begin
          if Success then
          begin
            W3ButtonConnection.Caption := 'Disconnect';
            W3ButtonStartNode.Enabled := True;
         //    W3ButtonStartNodeClick(W3ButtonStartNode);
          end else
          begin
            W3ButtonConnection.Caption := 'Connect';
            W3ButtonStartNode.Enabled := False;
          end;
        end )   }
    except
      ShowMessage('Except');
    end;
  end;
end;

procedure TTabSettingsForm.SendMessage(Sender: TObject; LccMessage: TLccMessage);
var
  i: Integer;
begin
  MessageList.Text := LccMessage.ConvertToGridConnectStr(#10, W3CheckBoxDetailedLog.Checked);
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
  ControllerManager.NodeManager.ProcessMessage(LccMessage);
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

procedure TTabSettingsForm.WebSocketConnectCallback(Socket: TW3WebSocket; Success: Boolean);
begin
ShowMessage('connect');

  if Success then
  begin
    W3ButtonConnection.Caption := 'Disconnect';
    W3ButtonStartNode.Enabled := True;
 //   W3ButtonStartNodeClick(W3ButtonStartNode);
  end else
  begin
    W3ButtonConnection.Caption := 'Connect';
    W3ButtonStartNode.Enabled := False;
  end;
end;


initialization
  Forms.RegisterForm({$I %FILE%}, TTabSettingsForm);
end.
