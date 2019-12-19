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
  System.Memory,
  System.Memory.Views,
  System.Memory.Buffer,
  System.Memory.Allocation,
  System.Streams,
  Storage,
  lcc_node_manager,
  lcc_node,
  lcc_node_messages,
  lcc_node_messages_can_assembler_disassembler,
  lcc_protocol_memory_configurationdefinitioninfo,
  lcc_defines;

type
  TTabSettingsForm = class(TW3Form)
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

    procedure SendMessage(Sender: TObject; LccMessage: TLccMessage);
    procedure ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
  public
    CanNodeManager: TLccCanNodeManager;
  end;

implementation

{ TTabSettingsForm }

procedure TTabSettingsForm.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  CanNodeManager := TLccCanNodeManager.Create(nil);
  CanNodeManager.OnLccMessageSend := @SendMessage;
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
  //    ShowMessage('Socket Open');
      FConnected := True;
    end;

  FSocket.OnClosed := procedure (Sender: TW3WebSocket)
    begin
   //   ShowMessage('Socket Closed');
      FConnected := False;
    end;

  FSocket.OnError := procedure (Sender: TW3WebSocket)
    begin
      ShowMessage('Socket Error');
    end;

  FSocket.OnMessage := procedure (Sender: TW3WebSocket; Message: TWebSocketMessageData)
    begin
 //      ShowMessage(Message.mdText);
       LccMessage := TLccMessage.Create;
       try
         LccMessage.LoadByGridConnectStr(Message.mdText);
         ReceiveMessage(Self, LccMessage);
   //      W3Listbox1.AddItem('R:' + Message.mdText);

     //    W3Listbox1.AddItem(LccMessage.ConvertToGridConnectStr(''));
  //      W3Listbox1.AddItem(LccMessage.ConvertToLccTcpString);
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
  i: Integer;
begin
  if CanNodeManager.Nodes.Count = 0 then
  begin
    CanNode := CanNodeManager.AddNode as TLccCanNode;
    CanNode.ProtocolSupportedProtocols.CDI := True;
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

    // Setup the CDI
    {$IFDEF DWSCRIPT}
      CanNode.ProtocolConfigurationDefinitionInfo.AStream.Position := 0;
      var BinaryByte: TBinaryData;
      var BinaryByteArray: TDynamicByteArray;
      BinaryByte := TBinaryData.Create(1);
      BinaryByteArray := BinaryByte.ToBytes;
      for i := 0 to Length(CDI_XML) - 1 do
      begin
        BinaryByteArray[0] := Ord(CDI_XML[i]);
        CanNode.ProtocolConfigurationDefinitionInfo.AStream.Write(BinaryByteArray);
      end;
    {$ELSE}
      CanNode.ProtocolConfigurationDefinitionInfo.AStream.Clear;
      {$IFDEF LCC_MOBILE}     // Delphi only
        for i := 0 to Length(CDI_XML) - 1 do
          CanNode.ProtocolConfigurationDefinitionInfo.AStream.Write(Ord(CDI_XML[i]), 1);
      {$ELSE}
        for i := 1 to Length(CDI_XML) do
        {$IFDEF FPC}
          CanNode.ProtocolConfigurationDefinitionInfo.AStream.WriteByte(Ord(CDI_XML[i]));
        {$ELSE}
          CanNode.ProtocolConfigurationDefinitionInfo.AStream.Write( Ord(CDI_XML[i]), 1);
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    CanNode.ProtocolConfigurationDefinitionInfo.Valid := True;

    // Setup the SNIP by extracting information from the CDI
    CanNode.ProtocolSimpleNodeInfo.LoadFromXmlString(CDI_XML);

    CanNode.Login(NULL_NODE_ID); // Create our own ID

    lcc_node_messages_can_assembler_disassembler.Max_Allowed_Datagrams := 1; // HACK ALLERT: Allow OpenLCB Python Scripts to run

    W3ButtonStartNode.Caption := 'Stop Node';
  end else
  begin
    W3ButtonStartNode.Caption := 'Start Node';
    CanNodeManager.Clear;
  end;
end;

procedure TTabSettingsForm.W3ButtonConnectionClick(Sender: TObject);
var
  URL: string;

begin
  if FSocket.Connected or FConnected then
  begin
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
 //   URL := "wss://echo.websocket.org";
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
begin
  WriteLn('SendMessage: ' + LccMessage.ConvertToGridConnectStr(''));
  FSocket.Write(LccMessage.ConvertToGridConnectStr(''));
end;

procedure TTabSettingsForm.ReceiveMessage(Sender: TObject; LccMessage: TLccMessage);
begin
  WriteLn('ReceiveMessage: ' + LccMessage.ConvertToGridConnectStr(''));
  CanNodeManager.ProcessMessage(LccMessage);
end;

initialization
  Forms.RegisterForm({$I %FILE%}, TTabSettingsForm);
end.
