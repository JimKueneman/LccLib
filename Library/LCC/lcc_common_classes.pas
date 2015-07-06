unit lcc_common_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lcc_messages;

type
   { TLccEthernetBaseThread }

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
  end;

implementation

end.

