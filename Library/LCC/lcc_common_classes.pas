unit lcc_common_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lcc_messages, contnrs;

type
   { TLccEthernetBaseThread }

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FGridConnect: Boolean;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); virtual;
    property GridConnect: Boolean read FGridConnect write FGridConnect;    // Ethernet Only
  end;


  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent)
  public
    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
    procedure FillWaitingMessageList(WaitingMessageList: TObjectList); virtual; abstract;
  end;

implementation

{ TLccConnectionThread }

constructor TLccConnectionThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
end;

end.

