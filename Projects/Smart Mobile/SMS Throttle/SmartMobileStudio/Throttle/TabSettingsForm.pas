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
  lcc_message,
  lcc_nodemanager,
  Storage;

type
  TTabSettingsForm = class(TW3Form)
    procedure W3Button1Click(Sender: TObject);
  private
    {$I 'TabSettingsForm:intf'}
    FSocket: TW3WebSocket;
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  end;

implementation

{ TTabSettingsForm }

procedure TTabSettingsForm.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
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
    end;

  FSocket.OnClosed := procedure (Sender: TW3WebSocket)
    begin
  //    ShowMessage('Socket Closed');
    end;

  FSocket.OnError := procedure (Sender: TW3WebSocket)
    begin
  //    ShowMessage('Socket Error');
    end;

  FSocket.OnMessage := procedure (Sender: TW3WebSocket; Message: TWebSocketMessageData)
    begin
 //      ShowMessage(Message.mdText);
       LccMessage := TLccMessage.Create;
       try
         LccMessage.LoadByGridConnectStr(Message.mdText);
         W3Listbox1.AddItem(LccMessage.ConvertToGridConnectStr(';'));
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



procedure TTabSettingsForm.W3Button1Click(Sender: TObject);
var
  URL: string;

begin
  if FSocket.Connected then
  begin
    FSocket.Disconnect(procedure (Socket: TW3WebSocket; Success: boolean)
      begin
        if Success then
        begin
 //         ShowMessage('Success');
          W3ButtonConnection.Caption := 'Open';
        end else
        begin
 //         ShowMessage('Failure');
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

initialization
  Forms.RegisterForm({$I %FILE%}, TTabSettingsForm);
end.
