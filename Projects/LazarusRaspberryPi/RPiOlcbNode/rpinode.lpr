program rpinode;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  crt, lcc_nodemanager, lcc_app_common_settings, lcc_ethernetclient, lcc_messages
  ;

const
 // SETTINGS_PATH = '/home/pi/Documents/LccLib/Projects/LazarusRaspberryPi/RPiOlcbNode/settings.ini';
  SETTINGS_PATH = './settings.ini';
  CDI_PATH      = './example_cdi.xml';

type

  { TOlcbNodeApplication }

  TOlcbNodeApplication = class(TCustomApplication)
  private
    FConnected: Boolean;
    FEthernet: TLccEthernetClient;
    FFailedConnecting: Boolean;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
    FTriedConnecting: Boolean;
  protected
    procedure DoRun; override;
    procedure EthernetConnectChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    function LogIntoOlcbNetwork: Boolean;

    property FailedConnecting: Boolean read FFailedConnecting write FFailedConnecting;
    property TriedConnecting: Boolean read FTriedConnecting write FTriedConnecting;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property Connected: Boolean read FConnected write FConnected;
    property Ethernet: TLccEthernetClient read FEthernet write FEthernet;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
  end;

{ TOlcbNode }

procedure TOlcbNodeApplication.DoRun;
var
  ErrorMsg: String;
  Running: Boolean;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  LccSettings.FilePath := SETTINGS_PATH;
  LccSettings.LoadFromFile;
  NodeManager.CAN := True;

  Ethernet.GridConnect := True;

  if LogInToOlcbNetwork then
  begin
    Running := True;
    NodeManager.Enabled := True;
    WriteLn('Enabled');
    while Running do
    begin
      CheckSynchronize();  // Pump the timers
      if KeyPressed then
        Running := ReadKey <> 'q';
    end;
    NodeManager.Enabled := False;
    Ethernet.CloseConnection(nil);
    NodeManager.HardwareConnection := nil;
    Ethernet.NodeManager := nil;
    FreeAndNil(FLccSettings);
    FreeAndNil(FEthernet);
    FreeAndNil(FNodeManager);
    WriteLn('Exiting');
  end;

  // stop program loop
  Terminate;
end;

procedure TOlcbNodeApplication.EthernetConnectChange(Sender: TObject; EthernetRec: TLccEthernetRec);
begin
  case EthernetRec.ConnectionState of
    ccsClientConnecting :
      begin
        WriteLn('Connecting');
        TriedConnecting := True;
      end;
    ccsClientConnected :
      begin
        WriteLn('Connected');
        Connected := True;
      end;
    ccsClientDisconnecting :
      begin
        WriteLn('DisConnecting');
      end;
    ccsClientDisconnected :
      begin
        WriteLn('DisConnected');
        FailedConnecting := True;
      end;
  end;
end;

function TOlcbNodeApplication.LogIntoOlcbNetwork: Boolean;
var
  WaitCount: Integer;
begin
  Result := True;
  while not Connected and Result do
  begin
    TriedConnecting := False;
    FailedConnecting := False;
    Ethernet.OpenConnectionWithLccSettings;
    while not TriedConnecting do
    begin
      if KeyPressed then Result := ReadKey <> 'q';
      CheckSynchronize();  // Pump the timers
    end;
    while not FailedConnecting and not Connected do
    begin
      if KeyPressed then Result := ReadKey <> 'q';
      CheckSynchronize();  // Pump the timers
    end;
    if FailedConnecting then
    begin
      WaitCount := 0;
      while (WaitCount < 50) and Result do
      begin
        if KeyPressed then Result := ReadKey <> 'q';
        Sleep(100);
        Inc(WaitCount);
      end;
    end;
  end;
end;

constructor TOlcbNodeApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  LccSettings := TLccSettings.Create(nil);
  NodeManager := TLccNodeManager.Create(nil);
  Ethernet := TLccEthernetClient.Create(nil);
  NodeManager.CreateRootNode;
  NodeManager.LccSettings := LccSettings;
  NodeManager.HardwareConnection := Ethernet;
  NodeManager.RootNode.CDI.LoadFromXml(CDI_PATH);
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Count := 10;
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Enable := True;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Count := 10;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Enable := True;
  Ethernet.LccSettings := LccSettings;
  Ethernet.OnConnectionStateChange := @EthernetConnectChange;
  Ethernet.NodeManager := NodeManager;
end;

destructor TOlcbNodeApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TOlcbNodeApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TOlcbNodeApplication;
begin
  Application:=TOlcbNodeApplication.Create(nil);
  Application.Title:='OpenLcb Node';
  Application.Run;
  Application.Free;
end.

