unit lcc_common_classes;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
  {$ELSE}
    System.Generics.Collections,
  {$ENDIF}
  lcc_node_messages,
  lcc_threaded_circulararray,
  lcc_ethernet_tcp,
  lcc_threaded_stringlist,
  lcc_alias_server,
  lcc_defines;

type
  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FGridConnect: Boolean;
    FMsgStringList: TStringList;
    FOutgoingCircularArray: TThreadedCirularArray;
    FOutgoingGridConnect: TThreadStringList;
    FSleepCount: Integer;
    FTcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine;
    FUseSynchronize: Boolean;
    FWorkerMsg: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;

    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
  public
    {$IFDEF FPC}
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    {$ELSE}
    constructor Create(CreateSuspended: Boolean); reintroduce;
    {$ENDIF}
    destructor Destroy; override;

    procedure UpdateAliasServer(LccMessage: TLccMessage);

    property GridConnect: Boolean read FGridConnect write FGridConnect;    // Ethernet Only
    property MsgStringList: TStringList read FMsgStringList write FMsgStringList;
    property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
    property OutgoingCircularArray: TThreadedCirularArray read FOutgoingCircularArray write FOutgoingCircularArray;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    property SleepCount: Integer read FSleepCount write FSleepCount;
    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
    property TcpDecodeStateMachine: TOPStackcoreTcpDecodeStateMachine read FTcpDecodeStateMachine write FTcpDecodeStateMachine;
  end;


  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent)
  protected
    FIncomingCircularArray: TThreadedCirularArray;
    FIncomingGridConnect: TThreadStringList;
  public
    property IncomingGridConnect: TThreadStringList read FIncomingGridConnect;
    property IncomingCircularArray: TThreadedCirularArray read FIncomingCircularArray;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual; abstract;
  end;

implementation

{ TLccHardwareConnectionManager }

constructor TLccHardwareConnectionManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIncomingGridConnect := TThreadStringList.Create;
  FIncomingCircularArray := TThreadedCirularArray.Create;
end;

destructor TLccHardwareConnectionManager.Destroy;
begin
  FreeAndNil(FIncomingCircularArray);
  FreeAndNil(FIncomingGridConnect);
  inherited Destroy;
end;

{ TLccConnectionThread }

{$IFDEF FPC}
constructor TLccConnectionThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
{$ELSE}
constructor TLccConnectionThread.Create(CreateSuspended: Boolean);
{$ENDIF}
begin
  inherited Create(CreateSuspended {$IFDEF FPC}, StackSize{$ENDIF});
  FWorkerMsg := TLccMessage.Create;
  FMsgStringList := TStringList.Create;
  FOutgoingCircularArray := TThreadedCirularArray.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
  OutgoingGridConnect.Delimiter := #10;
  FTcpDecodeStateMachine := TOPStackcoreTcpDecodeStateMachine.Create;
end;

destructor TLccConnectionThread.Destroy;
begin
  FreeAndNil(FWorkerMsg);
  FreeAndNil(FMsgStringList);
  FreeAndNil(FOutgoingCircularArray);
  FreeAndNil(FOutgoingGridConnect);
  FreeAndNil(FTcpDecodeStateMachine);
  inherited Destroy;
end;

function TLccConnectionThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TLccConnectionThread.UpdateAliasServer(LccMessage: TLccMessage);
begin
  case LccMessage.CAN.MTI of
    MTI_CAN_AMR : AliasServer.RemoveMapping(LccMessage.CAN.SourceAlias);
    MTI_CAN_AMD : AliasServer.ForceMapping(LccMessage.SourceID, LccMessage.CAN.SourceAlias)
  end;
end;

end.

