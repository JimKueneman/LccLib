unit lcc_app_common_settings;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ActnList, ExtCtrls, Buttons,
  lcc_utilities,
  {$ENDIF}
  {$IFNDEF FPC}
  Types,
  System.SyncObjs,
  {$ENDIF}
  IniFiles, lcc_defines;

 // Classes, SysUtils, FileUtil, IniFiles;

const
  STR_INI_COMPORT_SECTION = 'ComPort';
  STR_INI_COMPORT_PORT_KEY = 'Port';
  STR_INI_COMPORT_BAUD_KEY = 'Baud';
  STR_INI_COMPORT_BITS_KEY = 'DataBits';
  STR_INI_COMPORT_PARITY_KEY = 'Parity';
  STR_INI_COMPORT_STOPBITS_KEY = 'StopBits';
  STR_INI_COMPORT_FLOWCONTROL_KEY = 'FlowControl';
  STR_INI_COMPORT_AUTOCONNECT = 'AutoConnect';

  STR_INT_GENERAL_SECTION = 'General';
  STR_INI_ALIASID = 'AliasID';
  STR_INI_NODEID = 'NodeID';
  STR_INI_SENDPACKETDELAY = 'SendDelay';
  STR_INI_AUTOSCAN = 'AutoScan';
  STR_INI_JMRI_FORMAT = 'JMRI_Format';
  STR_INI_LOGGING     = 'Logging';
  STR_INI_DETAILED_LOGGING = 'DetailedLogging';
  STR_INI_PIPELINE = 'SchedulerPipelineSize';
  STR_INI_CV_BLOCK_READ = 'CvBlockRead';
  STR_FUNCTION_ACCESS = 'FunctionAccessType';

  STR_INI_THROTTLE_SECTION = 'Throttle';
  STR_INI_THROTTLE_AUTOLOADFDI = 'AutoLoadFDI';
  STR_INI_THROTTLE_AUTOSCROLLDELTA = 'AutoScrollDelta';


  STR_INI_ETHERNET_SECTION = 'Ethernet';
  STR_INI_ETHERNET_LOCAL_LISTENER_IP = 'LocalListenerIP';
  STR_INI_ETHERNET_REMOTE_LISTENER_IP = 'RemoteListenerIP';
  STR_INI_ETHERNET_LOCAL_CLIENT_IP = 'LocalclientIP';
  STR_INI_ETHERNET_LOCAL_LISTENER_PORT = 'LocalListenerPort';
  STR_INI_ETHERNET_REMOTE_LISTENER_PORT = 'RemotePort';
  STR_INI_ETHERNET_LOCAL_CLIENT_PORT = 'LocalClientPort';
  STR_INI_ETHERNET_GRIDCONNECT = 'GridConnect';


type
  TComPortParity = (
    cpp_None,
    cpp_Even,
    cpp_Odd,
    cpp_Mark,
    cpp_Space
  );
const
  HI_PARTITY_TYPE = 4;

type
  TComPortFlowControl = (
    cpf_None,
    cpf_CTS_RTS,            // Hardware with CTS/RTS
    cpf_DTR_DSR,            // Hardware with DTR/DSR
    cpf_XON_XOFF            // Software
  );

type
  TComPortDataBits = (
    cpdb_8_Bits,                // 8 Data bits
    cpdb_9_Bits                 // 9 Data bits
  );

  TComPortStopBits = (
    cpsb_1_StopBit,                // 1 Stop bit
    cpsb_1_5_StopBit,              // 1.5 Stop bit
    cpsb_2_StopBit                 // 2 Stop bits
  );

const
  HI_FLOWCONTROL_TYPE = 3;

type

  TLccSettings = class;

  { TLccSettingBase }

  TLccSettingBase = class(TPersistent)
  private
    FOwner: TLccSettings;
  protected
    property Owner: TLccSettings read FOwner write FOwner;
  public
    constructor Create(AnOwner: TLccSettings); virtual;
  end;

  { TComPortSettings }

  TComPortSettings = class(TLccSettingBase)
  private
    FAutoConnectAtBoot: Boolean;
    FBaudRate: DWord;
    FDataBits: TComPortDataBits;
    FFlowControl: TComPortFlowControl;
    FParity: TComPortParity;
    FPort: string;
    FStopBits: TComPortStopBits;
  public
    constructor Create(AnOwner: TLccSettings); override;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
  published
    property AutoConnectAtBoot: Boolean read FAutoConnectAtBoot write FAutoConnectAtBoot;
    property BaudRate: DWord read FBaudRate write FBaudRate;
    property DataBits: TComPortDataBits read FDataBits write FDataBits;
    property Parity: TComPortParity read FParity write FParity;
    property Port: string read FPort write FPort;
    property StopBits: TComPortStopBits read FStopBits write FStopBits;
    property FlowControl: TComPortFlowControl read FFlowControl write FFlowControl;
  end;

  { TLoggingSettings }

  TLoggingSettings = class(TLccSettingBase)
  private
    FEnabled: Boolean;
    FDetailed: Boolean;
    FJMRIFormat: Boolean;
  public
    constructor Create(AnOwner: TLccSettings); override;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
  published
    property Detailed: Boolean read FDetailed write FDetailed;
    property Enabled: Boolean read FEnabled write FEnabled;
    property JMRIFormat: Boolean read FJMRIFormat write FJMRIFormat;
  end;

  { TGeneralSettings }

  TGeneralSettings = class(TLccSettingBase)
  private
    FAliasID: string;
    FCVBlockRead: Word;                                                         // For Train Configuration eventually
    FNodeID: string;
    FSchedulerPipelineSize: Integer;
    procedure SetAliasID(AValue: string);                                       // Set from a separate thread possibly
  public
    constructor Create(AnOwner: TLccSettings); override;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    function AliasIDAsVal: Word;
    function NodeIDAsVal: QWord;
    procedure NodeIDAsTNodeID(var ANodeID: TNodeID);
    property CVBlockRead: Word read FCVBlockRead write FCVBlockRead;
  published
    property AliasID: string read FAliasID write SetAliasID;
    property NodeID: string read FNodeID write FNodeID;
    property SchedulerPipelineSize: Integer read FSchedulerPipelineSize write FSchedulerPipelineSize;
  end;

  { TThrottleSettings }

  TThrottleSettings = class(TLccSettingBase)
  private
    FAutoLoadFDI: Boolean;
    FAutoScrollDelta: Integer;
  public
    constructor Create(AnOwner: TLccSettings); override;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
  published
    property AutoLoadFDI: Boolean read FAutoLoadFDI write FAutoLoadFDI;
    property AutoScrollDelta: Integer read FAutoScrollDelta write FAutoScrollDelta;
  end;

  { TEthernetSettings }

  TEthernetSettings = class(TLccSettingBase)
  private
    FGridConnect: Boolean;
    FLocalClientIP: string;
    FLocalClientPort: Integer;
    FLocalListenerIP: string;
    FLocalListenerPort: Integer;
    FRemoteListenerIP: string;
    FRemoteListenerPort: Integer;
  public
    constructor Create(AnOwner: TLccSettings); override;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
  published
    property GridConnect: Boolean read FGridConnect write FGridConnect;
    property LocalClientIP: string read FLocalClientIP write FLocalClientIP;
    property RemoteListenerIP: string read FRemoteListenerIP write FRemoteListenerIP;
    property LocalListenerIP: string read FLocalListenerIP write FLocalListenerIP;
    property LocalClientPort: Integer read FLocalClientPort write FLocalClientPort;
    property RemoteListenerPort: Integer read FRemoteListenerPort write FRemoteListenerPort;
    property LocalListenerPort: Integer read FLocalListenerPort write FLocalListenerPort;
  end;

  // Settings that all Lcc Applications will have in common

  { TLccSettings }

  TLccSettings = class(TComponent)
  private
    FComPort: TComPortSettings;
    FEthernet: TEthernetSettings;
    FFilePath: string;
    FGeneral: TGeneralSettings;
    FLock: TCriticalSection;
    FLogging: TLoggingSettings;
    FThrottle: TThrottleSettings;
  protected
    property Lock: TCriticalSection read FLock write FLock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile;       // Set FilePath as the Target File
    procedure SaveToFile;
  published
    property ComPort: TComPortSettings read FComPort write FComPort;
    property Ethernet: TEthernetSettings read FEthernet write FEthernet;
    property FilePath: string read FFilePath write FFilePath;
    property General: TGeneralSettings read FGeneral write FGeneral;
    property Logging: TLoggingSettings read FLogging write FLogging;
    property Throttle: TThrottleSettings read FThrottle write FThrottle;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF FPC}
 // {$I TLccSettings.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccSettings]);
end;

{ TLccSettingBase }

constructor TLccSettingBase.Create(AnOwner: TLccSettings);
begin
  inherited Create;
  FOwner := AnOwner;
end;


{ TLoggingSettings }

constructor TLoggingSettings.Create(AnOwner: TLccSettings);
begin
  inherited Create(AnOwner);
  FJMRIFormat := False;
  FEnabled := False;
  FDetailed := False;
end;

procedure TLoggingSettings.LoadFromFile(IniFile: TIniFile);
begin
  FJMRIFormat := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_JMRI_FORMAT, False);
  FEnabled := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_LOGGING, False);
  FDetailed := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_DETAILED_LOGGING, False);
end;

procedure TLoggingSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_JMRI_FORMAT, FJMRIFormat);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_LOGGING, FEnabled);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_DETAILED_LOGGING, FDetailed);
end;

{ TEthernetSettings }

constructor TEthernetSettings.Create(AnOwner: TLccSettings);
begin
  inherited Create(AnOwner);
  FLocalClientIP := '127.0.0.1';
  FLocalListenerIP := '127.0.0.1';
  FRemoteListenerIP := '127.0.0.1';
  FLocalClientPort := 12022;
  FRemoteListenerPort := 12021;
  FLocalListenerPort := 12021;
end;

procedure TEthernetSettings.LoadFromFile(IniFile: TIniFile);
begin
  FGridConnect := IniFile.ReadBool(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_GRIDCONNECT, False);
  FLocalClientIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_CLIENT_IP, '127.0.0.1');
  FLocalListenerIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_LISTENER_IP, '127.0.0.1');
  FRemoteListenerIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_LISTENER_IP, '127.0.0.1');
  FLocalClientPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_CLIENT_PORT, 12022);
  FRemoteListenerPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_LISTENER_PORT, 12021);
  FLocalListenerPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_LISTENER_PORT, 12021);
end;

procedure TEthernetSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteBool(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_GRIDCONNECT, FGridConnect);
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_CLIENT_IP, FLocalClientIP);
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_LISTENER_IP, FLocalListenerIP);
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_LISTENER_IP, FRemoteListenerIP);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_CLIENT_PORT, FLocalClientPort);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_LISTENER_PORT, FRemoteListenerPort);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_LISTENER_PORT, FLocalListenerPort);
end;

{ TThrottleSettings }

constructor TThrottleSettings.Create(AnOwner: TLccSettings);
begin
  inherited Create(AnOwner);
  FAutoLoadFDI := True;
  FAutoScrollDelta := 2;
end;

procedure TThrottleSettings.LoadFromFile(IniFile: TIniFile);
begin
  AutoLoadFDI := IniFile.ReadBool(STR_INI_THROTTLE_SECTION, STR_INI_THROTTLE_AUTOLOADFDI, True);
  AutoScrollDelta := IniFile.ReadInteger(STR_INI_THROTTLE_SECTION, STR_INI_THROTTLE_AUTOSCROLLDELTA, 2);
end;

procedure TThrottleSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteBool(STR_INI_THROTTLE_SECTION, STR_INI_THROTTLE_AUTOLOADFDI, FAutoLoadFDI);
  IniFile.WriteInteger(STR_INI_THROTTLE_SECTION, STR_INI_THROTTLE_AUTOSCROLLDELTA, FAutoScrollDelta);
end;

{ TGeneralSettings }

procedure TGeneralSettings.SetAliasID(AValue: string);
begin
  if FAliasID=AValue then Exit;
  Owner.Lock.Enter;
  FAliasID:=AValue;
  Owner.Lock.Leave;
end;

constructor TGeneralSettings.Create(AnOwner: TLccSettings);
begin
  inherited Create(AnOwner);
  FAliasID := '0x000';
  FNodeID := '0x000000000000';
  FSchedulerPipelineSize := 5;
end;

procedure TGeneralSettings.LoadFromFile(IniFile: TIniFile);
begin
  AliasID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, '0x000');
  NodeID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, '0x000000000000');
  FSchedulerPipelineSize := IniFile.ReadInteger(STR_INT_GENERAL_SECTION, STR_INI_PIPELINE, 1);
end;

procedure TGeneralSettings.NodeIDAsTNodeID(var ANodeID: TNodeID);
var
  Temp: QWord;
begin
  NodeID := Trim(NodeID);
  Temp := StrToQWord(NodeID);
  ANodeID[0] :=  Temp and $0000000000FFFFFF;
  ANodeID[1] := (Temp and $0000FFFFFF000000) shr 24;
end;

function TGeneralSettings.NodeIDAsVal: QWord;
begin
  Result := StrToQWord(NodeID)
end;

procedure TGeneralSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, FAliasID);
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, FNodeID);
  IniFile.WriteInteger(STR_INT_GENERAL_SECTION, STR_INI_PIPELINE, FSchedulerPipelineSize);
end;

function TGeneralSettings.AliasIDAsVal: Word;
begin
  Result := StrToInt(AliasID);
end;

{ TComPortSettings }

constructor TComPortSettings.Create(AnOwner: TLccSettings);
begin
  inherited Create(AnOwner);
  {$IFDEF cpuarm}
  FBaudRate := 333333;
  {$ELSE}
  FBaudRate := 333333;
  {$ENDIF}
  FDataBits := cpdb_8_Bits;
  FParity := cpp_None;
  FPort := 'COM1';
  FStopBits := cpsb_1_StopBit;
  FFlowControl := cpf_None;
end;

procedure TComPortSettings.LoadFromFile(IniFile: TIniFile);
var
  Value: QWord;
begin
  Port := IniFile.ReadString(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PORT_KEY, 'COM2');
  {$IFDEF cpuarm}
  BaudRate := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, 333333);
  {$ELSE}
  BaudRate := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, 333333);
  {$ENDIF}
  DataBits := TComPortDataBits(IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BITS_KEY, Integer( cpdb_8_Bits)));
  StopBits := TComPortStopBits(IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_STOPBITS_KEY, Integer(cpsb_1_StopBit)));
  Value := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PARITY_KEY, 0);
  if Value > HI_PARTITY_TYPE then
    Value := 0;
  Parity := TComPortParity( Value);

  Value := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_FLOWCONTROL_KEY, 0);
  if Value > HI_FLOWCONTROL_TYPE then
    Value := 0;
  FlowControl := TComPortFlowControl( Value);
  AutoConnectAtBoot := IniFile.ReadBool(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_AUTOCONNECT, False);
end;

procedure TComPortSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PORT_KEY, Port);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, BaudRate);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BITS_KEY, Integer( DataBits));
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_STOPBITS_KEY, Integer( StopBits));
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PARITY_KEY, Integer( Parity));
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_FLOWCONTROL_KEY, Integer( FlowControl));
  IniFile.WriteBool(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_AUTOCONNECT, FAutoConnectAtBoot);
end;


{ TLccSettings }

constructor TLccSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FComPort := TComPortSettings.Create(Self);
  FGeneral := TGeneralSettings.Create(Self);
  FThrottle := TThrottleSettings.Create(Self);
  FEthernet := TEthernetSettings.Create(Self);
  FLogging := TLoggingSettings.Create(Self);
end;

destructor TLccSettings.Destroy;
begin
  FreeAndNil(FComPort);
  FreeAndNil(FGeneral);
  FreeAndNil(FThrottle);
  FreeAndNil(FEthernet);
  FreeAndNil(FLogging);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TLccSettings.LoadFromFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FilePath);
  try
   ComPort.LoadFromFile(IniFile);
   General.LoadFromFile(IniFile);
   Throttle.LoadFromFile(IniFile);
   Ethernet.LoadFromFile(IniFile);
   Logging.LoadFromFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TLccSettings.SaveToFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FilePath);
  try
    ComPort.SaveToFile(IniFile);
    General.SaveToFile(IniFile);
    Throttle.SaveToFile(IniFile);
    Ethernet.SaveToFile(IniFile);
    Logging.SaveToFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

initialization
  RegisterClass(TLccSettings);

finalization

end.

