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
  lcc_messages, lcc_can_message_assembler_disassembler, lcc_sdn_utilities,
  {$IFDEF CPUARM}
  lcc_raspberrypi,
  {$ENDIF}
  {$IFDEF FPC}
  laz2_XMLWrite,
  {$ELSE}
  Xml.XMLDoc,
  Xml.xmldom,
  Xml.XMLIntf,
  {$ENDIF}
  lcc_utilities, lcc_xmlutilities, lcc_defines;

const
  SETTINGS_FILE       = 'settings.ini';
  CONFIGURATION_FILE  = 'config.dat';

type

  { TOlcbNodeApplication }

  TOlcbNodeApplication = class(TCustomApplication)
  private
    FAliasAllocated: Boolean;
    FConnected: Boolean;
    FEthernetClient: TLccEthernetClient;
    FEthernetServer: TLccEthernetServer;
    FFailedConnecting: Boolean;
    FIsServer: Boolean;
    FLccMessage: TLccMessage;
    FLccSettings: TLccSettings;
    FNodeManager: TLccNodeManager;
    {$IFDEF CPUARM}FPiSpi: TRaspberryPiSpi;{$ENDIF}
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

    property AliasAllocated: Boolean read FAliasAllocated write FAliasAllocated;
    property Connected: Boolean read FConnected write FConnected;
    property EthernetClient: TLccEthernetClient read FEthernetClient write FEthernetClient;
    property EthernetServer: TLccEthernetServer read FEthernetServer write FEthernetServer;
    property IsServer: Boolean read FIsServer write FIsServer;
    property LccSettings: TLccSettings read FLccSettings write FLccSettings;
    property LccMessage: TLccMessage read FLccMessage write FLccMessage;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    {$IFDEF CPUARM}property PiSpi: TRaspberryPiSpi read FPiSpi write FPiSpi;{$ENDIF}
  end;

{ TOlcbNode }

procedure TOlcbNodeApplication.DoRun;
var
  ErrorMsg, CustomNodeID: String;
  Running: Boolean;
  Key: Char;
  {$IFDEF CPUARM}
  TxBuffer, RxBuffer: TPiSpiBuffer;
  IoPortA, IoPortB: Byte;
  i: Integer;
  Action: TLccBinaryAction;
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
  ErrorMsg:=CheckOptions('h s C i t f d -H', 'help server cdi id templatefile configurationfile datagram hub');
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

  if not Assigned(NodeManager.RootNode.SdnController) then
    NodeManager.RootNode.SdnController := TLccSdnController.Create(nil);
  if HasOption('t', 'template') then
  begin
    if not FileExists(GetAppConfigDir(False) + GetOptionValue('t', 'template')) then
    begin
      WriteLn('Error loading software defined Template File');
      WriteLn(GetAppConfigDir(False) + GetOptionValue('t', 'template'));
      Terminate;
      Exit;
    end;
    try
      // The XML will be loaded after the NodeID is generated
      NodeManager.RootNode.SdnController.FilePathTemplate := GetAppConfigDir(False) + GetOptionValue('t', 'template');
      WriteLn('Node Template file: ' + GetAppConfigDir(False) + GetOptionValue('t', 'template'))
    except
      WriteLn('Error loading Node Template File');
      WriteLn(GetAppConfigDir(False) + GetOptionValue('t', 'template'));
      Terminate;
      Exit;
    end;
  end;

  if HasOption('f', 'configurationfile') then
  begin
    try
      // The XML will be loaded after the NodeID is generated
      if not Assigned(NodeManager.RootNode.SdnController) then
        NodeManager.RootNode.SdnController := TLccSdnController.Create(nil);
      NodeManager.RootNode.SdnController.FilePath := GetAppConfigDir(False) + GetOptionValue('f', 'template');
      WriteLn('Node Definition file: ' + GetAppConfigDir(False) + GetOptionValue('f', 'template'))
    except
      WriteLn('Error loading Node Definition File');
      WriteLn(GetAppConfigDir(False) + GetOptionValue('f', 'template'));
      Terminate;
      Exit;
    end;
  end;

  if HasOption('H', 'hub') then
    EthernetServer.Hub := True
  else
    EthernetServer.Hub := False;

  if HasOption('T', 'tcp') then
  begin
    EthernetServer.Gridconnect := False;
    EthernetClient.Gridconnect := False;
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

  {$IFDEF CPUARM}
  IoPortA := 0;
  IoPortB := 0;
  {$ENDIF}

  {$IFDEF CPUARM}if PiSpi.OpenSpi(SPI_DRIVER_PATH_CS0) then
  begin {$ENDIF}
    {$IFDEF CPUARM}TxBuffer[0] := 0;{$ENDIF}
    if IsServer then
    begin
      if CreateOlcbServer then
      begin
        Running := True;
        NodeManager.Enabled := True;
        WriteLn('Node started');
        WriteLn('Generating NodeID/Alias');
        while not AliasAllocated do
          CheckSynchronize();  // Pump the timers  ;
        WriteLn('Input Action Count: ' + IntToStr(NodeManager.RootNode.SdnController.InputActionCount));
        WriteLn('Output Action Count: ' + IntToStr(NodeManager.RootNode.SdnController.OutputActionCount));
        {$IFDEF CPUARM}
        TxBuffer[0] := MCP23S17_WRITE;
        TxBuffer[1] := MCP23S17_IOCON1;
        TxBuffer[2] := %00001000;
        PiSpi.Transfer(@TxBuffer, @RxBuffer, 2 + 1);
        TxBuffer[0] := MCP23S17_WRITE;
        TxBuffer[1] := MCP23S17_GPPUA;  // Pull ups
        TxBuffer[2] := $FF;
        PiSpi.Transfer(@TxBuffer, @RxBuffer, 2 + 1);
        TxBuffer[0] := MCP23S17_WRITE;
        TxBuffer[1] := MCP23S17_GPPUB;  // Pull ups
        TxBuffer[2] := $FF;
        PiSpi.Transfer(@TxBuffer, @RxBuffer, 2 + 1);
        TxBuffer[0] := MCP23S17_WRITE;
        TxBuffer[1] := MCP23S17_OLATA;  // Outputs Low
        TxBuffer[2] := $00;
        PiSpi.Transfer(@TxBuffer, @RxBuffer, 2 + 1);
        {$ENDIF}
        while Running do
        begin
          {$IFDEF CPUARM}
          TxBuffer[0] := MCP23S17_READ or (MCP23S17_ADDRESS_0 shl 1);
          TxBuffer[1] := MCP23S17_GPIOA;
          TxBuffer[2] := 0;  // Dummy
          TxBuffer[3] := 0;  // Dummy
          PiSpi.Transfer(@TxBuffer, @RxBuffer, 2+2);  // Command + 2 Bytes reply

          if RxBuffer[2] <> IoPortA then
          begin
            IoPortA := RxBuffer[2];
            WriteLn('IoPortA: ' + IntToStr(IoPortA));
          end;
          if RxBuffer[3] <> IoPortB then
          begin
            IoPortB := RxBuffer[3];
            WriteLn('IoPortB: ' + IntToStr(IoPortB));
          end;


          for i := 0 to 15 do
          begin

            if i < 8 then
              Action := NodeManager.RootNode.SdnController.PinUpdate(i, ((IoPortA shr i) and $01) <> 0)
            else
              Action := NodeManager.RootNode.SdnController.PinUpdate(i, ((IoPortB shr (i-8)) and $01) <> 0);

            if Assigned(Action) then
            begin
              if Action.ActionType = lat_Input then
              begin
                if Action.Producer and Action.IsDirty then
                begin
                  WriteLn('Input Action change detected sending PCER, pin ' + IntToStr(i));
                  if Action.EventState = evs_Valid then
                    LccMessage.LoadPCER(NodeManager.RootNode.NodeID, NodeManager.RootNode.AliasID, @Action.FEventIDHi)
                  else
                    LccMessage.LoadPCER(NodeManager.RootNode.NodeID, NodeManager.RootNode.AliasID, @Action.FEventIDLo);
                  NodeManager.SendLccMessage(LccMessage);
                  Action.IsDirty := False;
                end;
              end else
              if Action.ActionType = lat_Output then
              begin
                if Action.Consumer and Action.Logic.Calculate then
                begin
                  if Action.EventState = evs_Valid then
                  begin
                    WriteLn('Output Action change detected sending PCER, pin ' + IntToStr(i));
                    LccMessage.LoadPCER(NodeManager.RootNode.NodeID, NodeManager.RootNode.AliasID, @Action.FEventIDHi);
                    NodeManager.SendLccMessage(LccMessage);
                  end else
                  if Action.EventState = evs_Invalid then
                  begin
                    WriteLn('Output Action change detected sending PCER, pin ' + IntToStr(i));
                    LccMessage.LoadPCER(NodeManager.RootNode.NodeID, NodeManager.RootNode.AliasID, @Action.FEventIDLo);
                    NodeManager.SendLccMessage(LccMessage);
                  end;
                  Action.IsDirty := False;
                end;
              end;
            end;
          end;
          {$ENDIF}
          CheckSynchronize();  // Pump the timers
          if KeyPressed then
          begin
            Key := ReadKey;
            Running := Key <> 'q';

       {     case Key of
              '0' : IoPortB := IoPortB or $01;
              '1' : IoPortB := IoPortB and not $01;
              '2' : IoPortB := IoPortB or $02;
              '3' : IoPortB := IoPortB and not $02;
            end;
         }

          end;
        end;
      end;

    end else
    begin
      if LogInToOlcbNetwork then
      begin
        Running := True;
        NodeManager.Enabled := True;
        WriteLn('Node started');
         {$IFDEF CPUARM}

        {$ENDIF}
        while Running do
        begin
          {$IFDEF CPUARM}
          TxBuffer[0] := MCP23S17_READ or (MCP23S17_ADDRESS_0 shl 1);
          TxBuffer[1] := MCP23S17_GPIOA;
          PiSpi.Transfer(@TxBuffer, @RxBuffer, 2+2);  // Command + 2 Bytes reply
          if RxBuffer[2] <> IoPortA then
          begin
            IoPortA := RxBuffer[2];
            WriteLn('IoPortA: ' + IntToStr(IoPortA));
          end;
          if RxBuffer[3] <> IoPortB then
          begin
            IoPortB := RxBuffer[3];
            WriteLn('IoPortB: ' + IntToStr(IoPortB));
          end;
          {$ENDIF}
          CheckSynchronize();  // Pump the timers
          if KeyPressed then
            Running := ReadKey <> 'q';
        end;
      end;
    end;
  {$IFDEF CPUARM}end else
    WriteLn('Can''t open the Spi Port');{$ENDIF}

  WriteLn('Exiting');
  {$IFDEF CPUARM}PiSpi.CloseSpi;{$ENDIF}
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
  FreeAndNil(FLccMessage);

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

  NodeManager.RootNode.SdnController.NodeID := LccSourceNode.NodeID;
  if FileExists(NodeManager.RootNode.SdnController.FilePath) then
  begin
    NodeManager.RootNode.SdnController.XMLParse(NodeManager.RootNode.SdnController.FilePath);
  end else
  if FileExists(NodeManager.RootNode.SdnController.FilePathTemplate) then
  begin
    NodeManager.RootNode.SdnController.XMLParse(NodeManager.RootNode.SdnController.FilePathTemplate);
    NodeManager.RootNode.SdnController.AutoAssignEventIDs;
    NodeManager.RootNode.SdnController.AutoAssignLogicEvents;
    NodeManager.RootNode.SdnController.XMLExport(NodeManager.RootNode.SdnController.FilePath);
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
  AliasAllocated := True;
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
  LccMessage := TLccMessage.Create;
  {$IFDEF CPUARM}
  PiSpi := TRaspberryPiSpi.Create;
  PiSpi.Mode := psm_ClkIdleLo_DataRising;
  PiSpi.Speed := pss_976kHz;
  PiSpi.Bits := psb_8;
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
    while not TriedConnecting and Result do
    begin
      if KeyPressed then Result := ReadKey <> 'q';
      CheckSynchronize();  // Pump the timers
    end;
    while not FailedConnecting and not Connected and Result do
    begin
      if KeyPressed then Result := ReadKey <> 'q';
      CheckSynchronize();  // Pump the timers
    end;
    if FailedConnecting and Result then
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
  {$IFDEF CPUARM}FreeAndNil(FPiSpi);{$ENDIF}
  inherited Destroy;
end;

procedure TOlcbNodeApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  writeln('-s   : starts rpinode as a ethernet server [-s]');
  writeln('-C   : filename of the CDI file [-C MyCdi.xml]');
  writeln('-i   : nodeID for node [-i 0x203456123456]');
  writeln('-t   : filename of the node template file [-n nodetemplate.xml]');
  writeln('-f   : filename of the node definition file that is created from the template file [-n nodedefinition.xml]');
  writeln('-d   : define the number of datagram buffers available, use 1 to run against Olcb python test suite [-d 1]');
  writeln('-H   : If the node is a server (-s) this switch enables the node to be a hub to relay messages to other connections');
  writeln('-T   : Use pure LCC TCP protocol instead of CAN Gridconnect over TCP');
end;

var
  Application: TOlcbNodeApplication;
begin
  Application:=TOlcbNodeApplication.Create(nil);
  Application.Title:='OpenLcb Node';
  Application.Run;
  Application.Free;
end.

