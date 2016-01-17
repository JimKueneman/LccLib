program rpinode;

{$mode objfpc}{$H+}

{$IFDEF darwin}
  {$DEFINE UseCThreads}
{$ENDIF}
{$IFDEF linux}
  {$DEFINE UseCThreads}
{$ENDIF}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  crt, lcc_nodemanager, lcc_app_common_settings, lcc_ethernetclient, lcc_ethenetserver,
  lcc_messages, lcc_raspberrypi
  ;

const
  LF = #10+#13;
const

  CDI_FILE            = 'example_cdi.xml';
  SETTINGS_FILE       = 'settings.ini';
  CONFIGURATION_FILE  = 'config.dat';

type

  { TOlcbNodeApplication }

  TOlcbNodeApplication = class(TCustomApplication)
  private
    FConnected: Boolean;
    FEthernet: TLccEthernetClient;
    FEthernetServer: TLccEthernetServer;
    FFailedConnecting: Boolean;
    FIsServer: Boolean;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
    FPiUart: TRaspberryPiUart;
    FTriedConnecting: Boolean;
  protected
    procedure DoRun; override;
    procedure EthernetConnectChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    function CreateOlcbServer: Boolean;
    function LogIntoOlcbNetwork: Boolean;

    property FailedConnecting: Boolean read FFailedConnecting write FFailedConnecting;
    property TriedConnecting: Boolean read FTriedConnecting write FTriedConnecting;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property Connected: Boolean read FConnected write FConnected;
    property Ethernet: TLccEthernetClient read FEthernet write FEthernet;
    property EthernetServer: TLccEthernetServer read FEthernetServer write FEthernetServer;
    property IsServer: Boolean read FIsServer write FIsServer;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property PiUart: TRaspberryPiUart read FPiUart write FPiUart;
  end;

{ TOlcbNode }

procedure TOlcbNodeApplication.DoRun;
var
  ErrorMsg: String;
  Running: Boolean;
  TxBuffer, RxBuffer: TPiUartBuffer;
  RxCount, i: Integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h s', 'help server');
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

  if HasOption('s', 'server') then
    IsServer := True;

  WriteLn('Press "Q" to quit');

  { add your program here }

  ErrorMsg:=GetAppConfigDir(False);
  if not DirectoryExists(GetAppConfigDir(False)) then
    mkdir(GetAppConfigDir(False));
  LccSettings.FilePath := GetAppConfigDir(False) + SETTINGS_FILE;
  if not FileExists(GetAppConfigDir(False) + SETTINGS_FILE) then
    LccSettings.SaveToFile
  else
    LccSettings.LoadFromFile;
  NodeManager.RootNode.Configuration.FilePath := GetAppConfigDir(False) + CONFIGURATION_FILE;
  if FileExists(GetAppConfigDir(False) + CONFIGURATION_FILE) then
   NodeManager.RootNode.Configuration.LoadFromFile;


  if IsServer then
    NodeManager.HardwareConnection := EthernetServer
  else
    NodeManager.HardwareConnection := Ethernet;

  NodeManager.CAN := True;

  Ethernet.GridConnect := True;
  EthernetServer.Gridconnect := True;

  if PiUart.OpenUart('/dev/' + GetRaspberryPiUartPortNames) then
  begin
    TxBuffer[0] := 0;
    if IsServer then
    begin
      if CreateOlcbServer then
      begin
        Running := True;
        NodeManager.Enabled := True;
        WriteLn('Node started');
        while Running do
        begin
          while not PiUart.Write(@TxBuffer, 1) do
            Delay(10);
          Inc(TxBuffer[0]);
          PiUart.Read(@RxBuffer, 1);
          Delay(2);
          CheckSynchronize();  // Pump the timers
          if KeyPressed then
            Running := ReadKey <> 'q';
        end;
      end;
    end else
    begin
      if LogInToOlcbNetwork then
      begin
        Running := True;
        NodeManager.Enabled := True;
        WriteLn('Node started');
        while Running do
        begin
          PiUart.Write(@TxBuffer, 1);
          Inc(TxBuffer[0]);
          PiUart.Read(@RxBuffer, 1);
          CheckSynchronize();  // Pump the timers
          if KeyPressed then
            Running := ReadKey <> 'q';
        end;
      end;
    end;
  end else
    WriteLn('Can''t open the Serial Port: ' + GetRaspberryPiUartPortNames);

  WriteLn('Exiting');
  PiUart.CloseUart;
  NodeManager.Enabled := False;
  Ethernet.CloseConnection(nil);
  EthernetServer.CloseConnection(nil);
  NodeManager.HardwareConnection := nil;
  Ethernet.NodeManager := nil;
  EthernetServer.NodeManager := nil;
  FreeAndNil(FLccSettings);
  FreeAndNil(FEthernet);
  FreeAndNil(FEthernetServer);
  FreeAndNil(FNodeManager);

  // stop program loop
  Terminate;
  WriteLn('Done');
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
        WriteLn('IP = ' + EthernetRec.ClientIP + ' ' + IntToStr(EthernetRec.ClientPort));
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
    ccsListenerConnecting :
      begin
        WriteLn('Connecting Listener');
        TriedConnecting := True;
      end;
    ccsListenerConnected :
      begin
        WriteLn('Connected Listener');
        WriteLn('IP = ' + EthernetRec.ListenerIP + ' ' + IntToStr(EthernetRec.ListenerPort));
        Connected := True;
      end;
    ccsListenerDisconnecting :
      begin

      end;
    ccsListenerDisconnected :
      begin

      end;
    ccsListenerClientConnecting :
      begin

      end;
    ccsListenerClientConnected :
      begin
        WriteLn('New Client');
        WriteLn('IP = ' + EthernetRec.ListenerIP + ' ' + IntToStr(EthernetRec.ListenerPort));
        WriteLn('IP = ' + EthernetRec.ClientIP + ' ' + IntToStr(EthernetRec.ClientPort));
        Connected := True;
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
  WriteLn(' ');
  WriteLn(' ');
  WriteLn('HINT Configuration files are located here:');
  WriteLn(GetAppConfigDir(False));
  WriteLn(' ');
  StopOnException:=True;
  LccSettings := TLccSettings.Create(nil);
  NodeManager := TLccNodeManager.Create(nil);
  Ethernet := TLccEthernetClient.Create(nil);
  EthernetServer := TLccEthernetServer.Create(nil);
  NodeManager.CreateRootNode;
  NodeManager.LccSettings := LccSettings;
  NodeManager.RootNode.CDI.LoadFromXml(GetAppConfigDir(False) + CDI_FILE);
  if FileExists(GetAppConfigDir(False) + CDI_FILE) then
    WriteLn('Found External CDI file: ' + CDI_FILE);
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Count := 10;
  NodeManager.RootNode.EventsConsumed.AutoGenerate.Enable := True;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Count := 10;
  NodeManager.RootNode.EventsProduced.AutoGenerate.Enable := True;
  Ethernet.LccSettings := LccSettings;
  Ethernet.OnConnectionStateChange := @EthernetConnectChange;
  Ethernet.NodeManager := NodeManager;
  EthernetServer.LccSettings := LccSettings;
  EthernetServer.OnConnectionStateChange := @EthernetConnectChange;
  EthernetServer.NodeManager := NodeManager;
  PiUart := TRaspberryPiUart.Create;
  PiUart.Speed:= pus_9600Hz;
end;

function TOlcbNodeApplication.CreateOlcbServer: Boolean;
var
  WaitCount: Integer;
begin
  Result := True;
  while not Connected and Result do
  begin
    TriedConnecting := False;
    FailedConnecting := False;
    EthernetServer.OpenConnectionWithLccSettings;
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

destructor TOlcbNodeApplication.Destroy;
begin
  FreeAndNil(FPiUart);
  inherited Destroy;
end;

procedure TOlcbNodeApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  writeln('-s   : starts rpinode as a ethernet server');
end;

var
  Application: TOlcbNodeApplication;
begin
  Application:=TOlcbNodeApplication.Create(nil);
  Application.Title:='OpenLcb Node';
  Application.Run;
  Application.Free;
end.

