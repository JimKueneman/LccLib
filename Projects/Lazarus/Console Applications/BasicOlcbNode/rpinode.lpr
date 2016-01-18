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
  lcc_messages, lcc_can_message_assembler_disassembler,
  {$IFDEF CPUARM}
  lcc_raspberrypi,
  {$ENDIF}
  lcc_utilities;

const
  SETTINGS_FILE       = 'settings.ini';
  CONFIGURATION_FILE  = 'config.dat';

type

  { TOlcbNodeApplication }

  TOlcbNodeApplication = class(TCustomApplication)
  private
    FConnected: Boolean;
    FEthernetClient: TLccEthernetClient;
    FEthernetServer: TLccEthernetServer;
    FFailedConnecting: Boolean;
    FIsServer: Boolean;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
    {$IFDEF CPUARM}FPiUart: TRaspberryPiUart;{$ENDIF}
    FTriedConnecting: Boolean;
  protected
    procedure DoRun; override;
    function CreateOlcbServer: Boolean;
    function LogIntoOlcbNetwork: Boolean;
    procedure OnAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnEthernetConnectChange(Sender: TObject; EthernetRec: TLccEthernetRec);
    procedure OnNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);


    property FailedConnecting: Boolean read FFailedConnecting write FFailedConnecting;
    property TriedConnecting: Boolean read FTriedConnecting write FTriedConnecting;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property Connected: Boolean read FConnected write FConnected;
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property EthernetServer: TLccEthernetServer read FEthernetServer write FEthernetServer;
    property IsServer: Boolean read FIsServer write FIsServer;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    {$IFDEF CPUARM}property PiUart: TRaspberryPiUart read FPiUart write FPiUart;{$ENDIF}
  end;

{ TOlcbNode }

procedure TOlcbNodeApplication.DoRun;
var
  ErrorMsg, CustomNodeID: String;
  Running: Boolean;
  {$IFDEF CPUARM}
  TxBuffer, RxBuffer: TPiUartBuffer;
  {$ENDIF}
begin
  CustomNodeID := '';

  try
  // Make sure the configuration file path exists
  if not DirectoryExists(GetAppConfigDir(False)) then
    ForceDirectories(GetAppConfigDir(False));
  except
    WriteLn('Can''t create configuration directory');
    Terminate;
    Exit;
  end;

  // quick check parameters
  ErrorMsg:=CheckOptions('h s c p C i N d', 'help server consumers producers cdi id ndi datagram');
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

  ErrorMsg := GetAppConfigDir(False);
  if HasOption('C', 'cdi') then
  begin
    if not FileExists(GetAppConfigDir(False) + GetOptionValue('C', 'cdi')) then
    begin
      WriteLn('Error loading CDI file: ' + GetAppConfigDir(False) + GetOptionValue('C', 'cdi'));
      Terminate;
      Exit;
    end;
    NodeManager.RootNode.CDI.LoadFromXml(GetAppConfigDir(False) + GetOptionValue('C', 'cdi'));
    NodeManager.RootNode.SimpleNodeInfo.LoadFromXml(GetAppConfigDir(False) + GetOptionValue('C', 'cdi'));
    WriteLn('Node CDI file: ' + GetAppConfigDir(False) + GetOptionValue('C', 'cdi'))
  end;

  if HasOption('d', 'datagram') then
  begin
    try
      Max_Allowed_Datagrams := StrToInt(GetOptionValue('d', 'datagram'));
    except
      WriteLn('Invalid number of datagram buffers defined');
      Terminate;
      Exit;
    end;
  end;

  if HasOption('c', 'consumers') then
  begin
    try
      NodeManager.RootNode.EventsConsumed.AutoGenerate.Count := StrToInt(GetOptionValue('c', 'consumers'));
      NodeManager.RootNode.EventsConsumed.AutoGenerate.Enable := True;
      StrToInt(GetOptionValue('c', 'consumers'));
    except
      WriteLn('Invalid value for parameter of Consumer Events');
      Terminate;
      Exit;
    end;
    WriteLn('Consumer Events = ' + GetOptionValue('c', 'consumers'))
  end;

  if HasOption('p', 'producers') then
  begin
    try
      NodeManager.RootNode.EventsProduced.AutoGenerate.Count := StrToInt(GetOptionValue('p', 'producers'));
      NodeManager.RootNode.EventsProduced.AutoGenerate.Enable := True;
    except
      WriteLn('Invalid value for parameter of Producer Events');
      Terminate;
      Exit;
    end;
    WriteLn('Producer Events = ' + GetOptionValue('p', 'producers'))
  end;

  if HasOption('i', 'id') then
  begin
    if ValidateNodeIDAsHexString(GetOptionValue('i', 'id')) then
      CustomNodeID := (GetOptionValue('i', 'id'))
    else begin
      WriteLn('Invalid NodeID: ' + GetOptionValue('i', 'id'));
      Terminate;
      Exit;
    end;
  end;

  if HasOption('N', 'ndi') then
  begin
    if not FileExists(GetAppConfigDir(False) + GetOptionValue('N', 'ndi')) then
    begin
      WriteLn('Error loading Node Definition File');
      WriteLn(GetAppConfigDir(False) + GetOptionValue('N', 'ndi'));
      Terminate;
      Exit;
    end;
    try
      NodeManager.RootNode.NDI.FilePath := GetAppConfigDir(False) + GetOptionValue('N', 'ndi');
      NodeManager.RootNode.NDI.LoadFromXML;
      WriteLn('Node Definition file: ' + GetAppConfigDir(False) + GetOptionValue('N', 'ndi'))
    except
      WriteLn('Error loading Node Definition File');
      WriteLn(GetAppConfigDir(False) + GetOptionValue('N', 'ndi'));
      Terminate;
      Exit;
    end;
  end;

  WriteLn('Press "q" to quit');

  { add your program here }

  // Point the Settings object to the configuration file path
  LccSettings.FilePath := GetAppConfigDir(False) + SETTINGS_FILE;
  // Create a Settings file if it does not exist or load it if it does exist
  if not FileExists(GetAppConfigDir(False) + SETTINGS_FILE) then
    LccSettings.SaveToFile
  else
    LccSettings.LoadFromFile;

  // See if there is a custom Node ID to use
  if CustomNodeID <> '' then    // This was validated earlier
  begin
    LccSettings.General.NodeID := CustomNodeID;
    LccSettings.SaveToFile;
  end;

  // Point the Configuration object to the configuration file path
  // IS THIS OBE WITH THE NDI?
  NodeManager.RootNode.Configuration.FilePath := GetAppConfigDir(False) + CONFIGURATION_FILE;
  // If a configuration file exists load it
  if FileExists(GetAppConfigDir(False) + CONFIGURATION_FILE) then
    NodeManager.RootNode.Configuration.LoadFromFile;


  if IsServer then
    NodeManager.HardwareConnection := EthernetServer
  else
    NodeManager.HardwareConnection := EthernetClient;

  NodeManager.CAN := True;

  EthernetClient.GridConnect := True;
  EthernetServer.Gridconnect := True;

  {$IFDEF CPUARM}if PiUart.OpenUart('/dev/' + GetRaspberryPiUartPortNames) then
  begin {$ENDIF}
    {$IFDEF CPUARM}TxBuffer[0] := 0;{$ENDIF}
    if IsServer then
    begin
      if CreateOlcbServer then
      begin
        Running := True;
        NodeManager.Enabled := True;
        WriteLn('Node started');
        while Running do
        begin
          {$IFDEF CPUARM}
          while not PiUart.Write(@TxBuffer, 1) do
            Delay(10);
          Inc(TxBuffer[0]);
          PiUart.Read(@RxBuffer, 1);
          Delay(2);
          {$ENDIF}
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
          {$IFDEF CPUARM}
          PiUart.Write(@TxBuffer, 1);
          Inc(TxBuffer[0]);
          PiUart.Read(@RxBuffer, 1);
          {$ENDIF}
          CheckSynchronize();  // Pump the timers
          if KeyPressed then
            Running := ReadKey <> 'q';
        end;
      end;
    end;
  {$IFDEF CPUARM}end else
    WriteLn('Can''t open the Serial Port: ' + GetRaspberryPiUartPortNames);{$ENDIF}

  WriteLn('Exiting');
  {$IFDEF CPUARM}PiUart.CloseUart;{$ENDIF}
  NodeManager.Enabled := False;
  EthernetClient.CloseConnection(nil);
  EthernetServer.CloseConnection(nil);
  NodeManager.HardwareConnection := nil;
  EthernetClient.NodeManager := nil;
  EthernetServer.NodeManager := nil;
  FreeAndNil(FLccSettings);
  FreeAndNil(FEthernetClient);
  FreeAndNil(FEthernetServer);
  FreeAndNil(FNodeManager);

  // stop program loop
  Terminate;
  WriteLn('Done');
end;

procedure TOlcbNodeApplication.OnEthernetConnectChange(Sender: TObject; EthernetRec: TLccEthernetRec);
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

procedure TOlcbNodeApplication.OnNodeIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  WriteLn('NodeID: ' + LccSourceNode.NodeIDStr);
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
    EthernetClient.OpenConnectionWithLccSettings;
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

procedure TOlcbNodeApplication.OnAliasIDChanged(Sender: TObject; LccSourceNode: TLccNode);
begin
  WriteLn('AliasID: ' + LccSourceNode.AliasIDStr);
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
  EthernetClient := TLccEthernetClient.Create(nil);
  EthernetServer := TLccEthernetServer.Create(nil);
  NodeManager.CreateRootNode;
  NodeManager.LccSettings := LccSettings;
  NodeManager.OnAliasIDChanged := @OnAliasIDChanged;
  NodeManager.OnNodeIDChanged := @OnNodeIDChanged;
  EthernetClient.LccSettings := LccSettings;
  EthernetClient.OnConnectionStateChange := @OnEthernetConnectChange;
  EthernetClient.NodeManager := NodeManager;
  EthernetServer.LccSettings := LccSettings;
  EthernetServer.OnConnectionStateChange := @OnEthernetConnectChange;
  EthernetServer.NodeManager := NodeManager;
  {$IFDEF CPUARM}
  PiUart := TRaspberryPiUart.Create;
  PiUart.Speed:= pus_9600Hz;
  {$ENDIF}
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
  {$IFDEF CPUARM}FreeAndNil(FPiUart);{$ENDIF}
  inherited Destroy;
end;

procedure TOlcbNodeApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  writeln('-s   : starts rpinode as a ethernet server [-s]');
  writeln('-p   : number of producer events [-p 6]');
  writeln('-c   : number of consumer events [-c 6]');
  writeln('-C   : filename of the CDI file [-C MyCdi.xml]');
  writeln('-i   : nodeID for node [-i 0x203456123456]');
  writeln('-N   : filename of the node definition file [-n nodedefinition.xml]');
  writeln('-d   : define the number of datagram buffers available, use 1 to run against Olcb python test suite [-d 1]');
end;

var
  Application: TOlcbNodeApplication;
begin
  Application:=TOlcbNodeApplication.Create(nil);
  Application.Title:='OpenLcb Node';
  Application.Run;
  Application.Free;
end.

