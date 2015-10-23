unit lcc_common_classes;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  contnrs,
  {$ELSE}
  System.Generics.Collections,
  {$ENDIF}
   lcc_messages, lcc_can_message_assembler_disassembler, lcc_threadedcirculararray, lcc_threaded_stringlist;

type
   { TLccEthernetBaseThread }

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FGridConnect: Boolean;
    FMsgAssembler: TLccMessageAssembler;
    FMsgDisAssembler: TLccMessageDisAssembler;
    FMsgStringList: TStringList;
    FOutgoingCircularArray: TThreadedCirularArray;
    FOutgoingGridConnect: TThreadStringList;
    FSleepCount: Integer;
    FWorkerMsg: TLccMessage;
    function GetIsTerminated: Boolean;
  protected
    FRunning: Boolean;
  public
    {$IFDEF FPC}
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    {$ELSE}
    constructor Create(CreateSuspended: Boolean); reintroduce;
    {$ENDIF}
    destructor Destroy; override;
    property GridConnect: Boolean read FGridConnect write FGridConnect;    // Ethernet Only
    property MsgAssembler: TLccMessageAssembler read FMsgAssembler write FMsgAssembler;
    property MsgDisAssembler: TLccMessageDisAssembler read FMsgDisAssembler write FMsgDisAssembler;
    property MsgStringList: TStringList read FMsgStringList write FMsgStringList;
    property OutgoingGridConnect: TThreadStringList read FOutgoingGridConnect write FOutgoingGridConnect;
    property OutgoingCircularArray: TThreadedCirularArray read FOutgoingCircularArray write FOutgoingCircularArray;
    property Running: Boolean read FRunning write FRunning;
    property IsTerminated: Boolean read GetIsTerminated;
    property SleepCount: Integer read FSleepCount write FSleepCount;
    property WorkerMsg: TLccMessage read FWorkerMsg write FWorkerMsg;
  end;


  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent)
  public
    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
    procedure SendMessageRawGridConnect(GridConnectStr: String); virtual; abstract;
    {$IFDEF FPC}
    procedure FillWaitingMessageList(WaitingMessageList: TObjectList); virtual; abstract;
    {$ELSE}
    procedure FillWaitingMessageList(WaitingMessageList: TObjectList<TLccMessage>); virtual; abstract;
    {$ENDIF}
  end;

implementation

{ TLccConnectionThread }

{$IFDEF FPC}
constructor TLccConnectionThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
{$ELSE}
constructor TLccConnectionThread.Create(CreateSuspended: Boolean);
{$ENDIF}
begin
  inherited Create(CreateSuspended {$IFDEF FPC}, StackSize{$ENDIF});
  FMsgAssembler := TLccMessageAssembler.Create;
  FMsgDisAssembler := TLccMessageDisAssembler.Create;
  FWorkerMsg := TLccMessage.Create;
  FMsgStringList := TStringList.Create;
  FOutgoingCircularArray := TThreadedCirularArray.Create;
  FOutgoingGridConnect := TThreadStringList.Create;
end;

destructor TLccConnectionThread.Destroy;
begin
  FreeAndNil(FMsgAssembler);
  FreeAndNIl(FMsgDisAssembler);
  FreeAndNil(FWorkerMsg);
  FreeAndNil(FMsgStringList);
  FreeAndNil(FOutgoingCircularArray);
  FreeAndNil(FOutgoingGridConnect);
  inherited Destroy;
end;

function TLccConnectionThread.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

end.

