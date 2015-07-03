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
  STR_INI_ETHERNET_LOCAL_IP = 'LocalIP';
  STR_INI_ETHERNET_REMOTE_IP = 'RemoteIP';
  STR_INI_ETHERNET_LOCAL_PORT = 'LocalPort';
  STR_INI_ETHERNET_REMOTE_PORT = 'RemotePort';


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
    function NodeIDAsVal: DWord;
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
    FRemotePort: Integer;
    FLocalPort: Integer;
    FLocalIP: string;
    FRemoteIP: string;
  public
    constructor Create(AnOwner: TLccSettings); override;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
  published
    property LocalIP: string read FLocalIP write FLocalIP;
    property RemoteIP: string read FRemoteIP write FRemoteIP;
    property LocalPort: Integer read FLocalPort write FLocalPort;            // Storage if the connection is a listener
    property RemotePort: Integer read FRemotePort write FRemotePort;         // Storage if the connection is a client
  end;

  // Settings that all Lcc Applications will have in common

  { TLccSettings }

  TLccSettings = class(TComponent)
  private
    FComPort: TComPortSettings;
    FEthernet: TEthernetSettings;
    FGeneral: TGeneralSettings;
    FLock: TCriticalSection;
    FLogging: TLoggingSettings;
    FThrottle: TThrottleSettings;
  protected
    property Lock: TCriticalSection read FLock write FLock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
  published
    property ComPort: TComPortSettings read FComPort write FComPort;
    property Ethernet: TEthernetSettings read FEthernet write FEthernet;
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
  inherited;
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
  LocalIP := '127.0.0.1';
  RemoteIP := '127.0.0.1';
  LocalPort := 12021;
  RemotePort := 12022;
end;

procedure TEthernetSettings.LoadFromFile(IniFile: TIniFile);
begin
  LocalIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_IP, '127.0.0.1');
  RemoteIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_IP, '127.0.0.1');
  LocalPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_PORT, 12022);
  RemotePort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_PORT, 12021);
end;

procedure TEthernetSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_IP, FLocalIP);
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_IP, FRemoteIP);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_PORT, FRemotePort);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_PORT, FLocalPort);
end;

{ TThrottleSettings }

constructor TThrottleSettings.Create(AnOwner: TLccSettings);
begin
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

function TGeneralSettings.NodeIDAsVal: DWord;
begin
  Result := StrToInt(NodeID)
end;

{ TComPortSettings }

constructor TComPortSettings.Create(AnOwner: TLccSettings);
begin
  FBaudRate := 333333;
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
  BaudRate := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, 333333);
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

procedure TLccSettings.LoadFromFile(FileName: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
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

procedure TLccSettings.SaveToFile(FileName: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
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

end.

