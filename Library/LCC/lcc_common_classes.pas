unit lcc_common_classes;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  contnrs,
  {$ELSE}
  System.Generics.Collections,
  {$ENDIF}
   lcc_messages;

type
   { TLccEthernetBaseThread }

  { TLccConnectionThread }

  TLccConnectionThread = class(TThread)
  private
    FGridConnect: Boolean;
  public
    property GridConnect: Boolean read FGridConnect write FGridConnect;    // Ethernet Only
  end;


  { TLccHardwareConnectionManager }

  TLccHardwareConnectionManager = class(TComponent)
  public
    procedure SendMessage(AMessage: TLccMessage); virtual; abstract;
    {$IFDEF FPC}
    procedure FillWaitingMessageList(WaitingMessageList: TObjectList); virtual; abstract;
    {$ELSE}
    procedure FillWaitingMessageList(WaitingMessageList: TObjectList<TLccMessage>); virtual; abstract;
    {$ENDIF}
  end;

implementation

{ TLccConnectionThread }


end.

