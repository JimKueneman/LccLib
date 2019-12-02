unit lcc_utilities;

interface

uses 
  System.Types,
  System.Types.Convert,
  System.Time,
  System.Streams,
  System.Reader,
  System.Writer,
  System.Device.Storage,
  SmartCL.Device.Storage,
  SmartCL.Application,
  SmartCL.Components,
  SmartCL.System,
  lcc_defines;

  function EqualNodeID(NodeID1: TNodeID; NodeID2: TNodeID; IncludeNullNode: Boolean): Boolean;
  function EqualEventID(EventID1, EventID2: TEventID): Boolean;
  procedure NodeIDToEventID(NodeID: TNodeID; LowBytes: Word; var EventID: TEventID);
  function NullNodeID(ANodeID: TNodeID): Boolean;
  procedure StringToNullArray(AString: String; var ANullArray: array of Byte; var iIndex: Integer);
  function _Lo(Data: DWORD): Byte;
  function _Hi(Data: DWORD): Byte;
  function _Higher(Data: DWORD): Byte;
  function _Highest(Data: DWORD): Byte;
//  function _Highest1(Data: QWord): Byte;
//  function _Highest2(Data: QWord): Byte;

{$IFDEF DWSCRIPT}
  function Lo(Data: Word): Byte;
  function Hi(Data: Word): Byte;
{$ENDIF}

implementation

function EqualNodeID(NodeID1: TNodeID; NodeID2: TNodeID; IncludeNullNode: Boolean): Boolean;
begin
  if IncludeNullNode then
    Result := (NodeID1[0] = NodeID2[0]) and (NodeID1[1] = NodeID2[1])
  else
    Result := not NullNodeID(NodeID1) and not NullNodeID(NodeID2) and (NodeID1[0] = NodeID2[0]) and (NodeID1[1] = NodeID2[1])
end;

function EqualEventID(EventID1, EventID2: TEventID): Boolean;
begin
  Result := (EventID1[0] = EventID2[0]) and
            (EventID1[1] = EventID2[1]) and
            (EventID1[2] = EventID2[2]) and
            (EventID1[3] = EventID2[3]) and
            (EventID1[4] = EventID2[4]) and
            (EventID1[5] = EventID2[5]) and
            (EventID1[6] = EventID2[6]) and
            (EventID1[7] = EventID2[7])
end;

procedure NodeIDToEventID(NodeID: TNodeID; LowBytes: Word; var EventID: TEventID);
begin
  EventID[0]     := _Higher( NodeID[1]); // But these all need the 48 Bit Full ID in the Byte Fields
  EventID[1] := _Hi(     NodeID[1]);
  EventID[2] := _Lo(     NodeID[1]);
  EventID[3] := _Higher( NodeID[0]);
  EventID[4] := _Hi(     NodeID[0]);
  EventID[5] := _Lo(     NodeID[0]);
  EventID[6] := _Hi(LowBytes);
  EventID[7] := _Lo(LowBytes);
end;

procedure StringToNullArray(AString: String; var ANullArray: array of Byte; var iIndex: Integer);
var
  Len, i: Integer;
begin
  Len := Length(AString);
  if Len > 0 then
  begin
    {$IFDEF FPC}
      for i := 1 to Len do
    {$ELSE}
      {$IFDEF LCC_MOBILE}
        for i := 0 to Len - 1 do
      {$ELSE}
        for i := 1 to Len do
      {$ENDIF}
    {$ENDIF}
    begin
      ANullArray[iIndex] := Ord( AString[i]);
      Inc(iIndex);
    end;
  end;
  ANullArray[iIndex] := 0;
  Inc(iIndex);
end;

function NullNodeID(ANodeID: TNodeID): Boolean;
begin
  Result := (ANodeID[0] = 0) and (ANodeID[1] = 0)
end;

function _Lo(Data: DWORD): Byte;
begin
  Result := Byte(Data) and $000000FF;
end;

function _Hi(Data: DWORD): Byte;
begin
  Result := Byte((Data shr 8) and $000000FF);
end;

function _Higher(Data: DWORD): Byte;
begin
  Result := Byte((Data shr 16) and $000000FF);
end;

function _Highest(Data: DWORD): Byte;
begin
  Result := Byte((Data shr 24) and $000000FF);
end;

{
function _Highest1(Data: QWord): Byte;
begin
  Result := Byte((Data shr 32) and $00000000000000FF);
end;

function _Highest2(Data: QWord): Byte;
begin
  Result := Byte((Data shr 40) and $00000000000000FF);
end;
}

{$IFDEF DWSCRIPT}
function Lo(Data: Word): Byte;
begin
  Result := Byte(Data) and $00FF;
end;

function Hi(Data: Word): Byte;
begin
  Result := Byte((Data shr 8) and $00FF);
end;
{$ENDIF}

end.
