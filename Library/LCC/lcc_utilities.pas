unit lcc_utilities;

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF LCC_WINDOWS}
   Windows,
  {$ELSE}
    {$IFDEF FPC}
      {$IFNDEF FPC_CONSOLE_APP}
      LclIntf,
      {$ENDIF}
      baseUnix, sockets,
    {$ELSE}
    strutils, Posix.NetinetIn, Posix.ArpaInet, Posix.SysSocket, Posix.Errno, Posix.Unistd,
    {$ENDIF}
  {$ENDIF}
  Types, lcc_defines, lcc_compiler_types;

  function GetTickCount : DWORD;
  function _Lo(Data: DWORD): Byte;
  function _Hi(Data: DWORD): Byte;
  function _Higher(Data: DWORD): Byte;
  function _Highest(Data: DWORD): Byte;
  function _Highest1(Data: QWord): Byte;
  function _Highest2(Data: QWord): Byte;
  function MTI2String(MTI: Word): String;
  function EqualNodeID(NodeID1: TNodeID; NodeID2: TNodeID; IncludeNullNode: Boolean): Boolean;
  function EqualEventID(EventID1, EventID2: TEventID): Boolean;
  procedure NodeIDToEventID(NodeID: TNodeID; LowBytes: Word; var EventID: TEventID);
  function NullNodeID(ANodeID: TNodeID): Boolean;
  procedure StringToNullArray(AString: String; var ANullArray: array of Byte; var iIndex: Integer);
  function NullArrayToString(var ANullArray: array of Byte): String;
  function EventIDToString(EventID: TEventID; InsertDots: Boolean): String;
  function ExtractDataBytesAsInt(DataArray: array of Byte; StartByteIndex, EndByteIndex: Integer): QWord;
  function ValidateNodeIDAsHexString(NodeID: string): Boolean;
  function ValidateNodeID(NodeID: TNodeID): Boolean;
  function StrToNodeID(NodeID: string): TNodeID;
  function StrToEventID(Event: string): TEventID;
  {$IFNDEF LCC_WINDOWS}
  function ResolveUnixIp: String;
  {$ENDIF}

{$IFDEF FPC}
type
  TCriticalSection = class
  protected
    Lock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;
{$ENDIF}

implementation


function GetTickCount : DWORD;
 {On Windows, this is number of milliseconds since Windows was
   started. On non-Windows platforms, LCL returns number of
   milliseconds since Dec. 30, 1899, wrapped by size of DWORD.
   This value can overflow LongInt variable when checks turned on,
   so "wrap" value here so it fits within LongInt.
  Also, since same thing could happen with Windows that has been
   running for at least approx. 25 days, override it too.}
{$IFNDEF LCC_WINDOWS}
var
  Hour, Minute, Second, MilliSecond: Word;
{$ENDIF}
begin
{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
    {$IFDEF LCC_WINDOWS}
      Result := Windows.GetTickCount mod High(LongInt);
    {$ELSE}
      Result := LclIntf.GetTickCount mod High(LongInt);
    {$ENDIF}
  {$ELSE}
    {$IFDEF LCC_WINDOWS}
      {$IFDEF LCC_WIN64}
      Result := GetTickCount64;
      {$ELSE}
      Result := GetTickCount;
      {$ENDIF}
    {$ELSE}
    DecodeTime(Now, Hour, Minute, Second, MilliSecond);
    Result := (((Hour*3600) + (Minute*60) + Second) * 1000) + MilliSecond;
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  Result := TThread.GetTickCount;
{$ENDIF}
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

function _Highest1(Data: QWord): Byte;
begin
  Result := Byte((Data shr 32) and $00000000000000FF);
end;

function _Highest2(Data: QWord): Byte;
begin
  Result := Byte((Data shr 40) and $00000000000000FF);
end;

function MTI2String(MTI: Word): String;
begin
  case MTI of
  {  MTI_CID0 : Result := 'Check ID 0';
    MTI_CID1 : Result := 'Check ID 1';
    MTI_CID2 : Result := 'Check ID 2';
    MTI_CID3 : Result := 'Check ID 3';
    MTI_CID4 : Result := 'Check ID 4';
    MTI_CID5 : Result := 'Check ID 5';
    MTI_CID6 : Result := 'Check ID 6';

    MTI_RID : Result := 'Reserve ID [RID]';
    MTI_AMD : Result := 'Alias Map Definition [AMD]';
    MTI_AME : Result := 'Alias Map Enquiry [AME]';
    MTI_AMR : Result := 'Alias Map Reset [AMR]'; }

    MTI_INITIALIZATION_COMPLETE : Result := 'Initialization Complete';
    MTI_VERIFY_NODE_ID_NUMBER_DEST : Result := 'Verify Node ID with Destination Address';
    MTI_VERIFY_NODE_ID_NUMBER      : Result := 'Verify Node ID Global';
    MTI_VERIFIED_NODE_ID_NUMBER    : Result := 'Verified Node ID';
    MTI_OPTIONAL_INTERACTION_REJECTED : Result := 'Optional Interaction Rejected';
    MTI_TERMINATE_DUE_TO_ERROR        : Result := 'Terminate Due to Error';

    MTI_PROTOCOL_SUPPORT_INQUIRY  : Result := 'Protocol Support Inquiry';
    MTI_PROTOCOL_SUPPORT_REPLY    : Result := 'Protocol Support Reply';

    MTI_CONSUMER_IDENTIFY              : Result := 'Consumer Identify';
    MTI_CONSUMER_IDENTIFY_RANGE        : Result := 'Consumer Identify Range';
    MTI_CONSUMER_IDENTIFIED_UNKNOWN    : Result := 'Consumer Identified Unknown';
    MTI_CONSUMER_IDENTIFIED_SET        : Result := 'Consumer Identified Valid';
    MTI_CONSUMER_IDENTIFIED_CLEAR      : Result := 'Consumer Identified Clear';
    MTI_CONSUMER_IDENTIFIED_RESERVED   : Result := 'Consumer Identified Reserved';
    MTI_PRODUCER_IDENDIFY              : Result := 'Producer Identify';
    MTI_PRODUCER_IDENTIFY_RANGE        : Result := 'Producer Identify Range';
    MTI_PRODUCER_IDENTIFIED_UNKNOWN    : Result := 'Producer Identified Unknown';
    MTI_PRODUCER_IDENTIFIED_SET        : Result := 'Producer Identified Valid';
    MTI_PRODUCER_IDENTIFIED_CLEAR      : Result := 'Producer Identified Clear';
    MTI_PRODUCER_IDENTIFIED_RESERVED   : Result := 'Producer Identified Reserved';
    MTI_EVENTS_IDENTIFY_DEST           : Result := 'Events Identify with Destination Address';
    MTI_EVENTS_IDENTIFY                : Result := 'Events Identify Global';
    MTI_EVENT_LEARN                    : Result := 'Event Learn';
    MTI_PC_EVENT_REPORT                : Result := 'Producer/Consumer Event Report [PCER] ';

    MTI_SIMPLE_NODE_INFO_REQUEST       : Result := 'Simple Node Info Request [SNIP]';
    MTI_SIMPLE_NODE_INFO_REPLY         : Result := 'Simple Node Info Reply [SNIP]';

    MTI_SIMPLE_TRAIN_INFO_REQUEST       : Result := 'Simple Train Node Info Request [STNIP]';
    MTI_SIMPLE_TRAIN_INFO_REPLY         : Result := 'Simple Train Node Info Reply [STNIP]';

    MTI_DATAGRAM                       : Result := 'Datagram';
    MTI_DATAGRAM_OK_REPLY              : begin
                                           Result := 'Datagram Reply OK';
                                  {         if LocalHelper.DataCount > 2 then
                                           begin
                                             if LocalHelper.Data[2] and DATAGRAM_OK_ACK_REPLY_PENDING = DATAGRAM_OK_ACK_REPLY_PENDING then
                                             begin
                                               if LocalHelper.Data[2] and $7F = 0 then
                                                 Result := Result + ' - Reply Is Pending - Maximum wait time = Infinity'
                                               else
                                                 Result := Result + ' - Reply Is Pending - Maximum wait time = ' + IntToStr( Round( Power(2, LocalHelper.Data[2] and $7F))) + ' seconds'
                                             end else
                                               Result := Result + ' - Reply Is Not Pending'
                                           end else
                                             Result := Result + ' - Does not include Extended Flags';   }
                                         end;
    MTI_DATAGRAM_REJECTED_REPLY        : Result := 'Datagram Rejected Reply';

    MTI_TRACTION_PROTOCOL              : Result := 'Traction Protocol';
    MTI_TRACTION_REPLY                 : Result := 'Traction Reply';

    MTI_STREAM_INIT_REQUEST            : Result := 'Stream Request';
    MTI_STREAM_INIT_REPLY              : Result := 'Stream Init Reply';
    MTI_STREAM_SEND                    : Result := 'Stream Send';
    MTI_STREAM_PROCEED                 : Result := 'Stream Proceed';
    MTI_STREAM_COMPLETE                : Result := 'Stream Complete';
   else
    Result := 'Unknown MTI';
  end;
end;

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

function NullNodeID(ANodeID: TNodeID): Boolean;
begin
  Result := (ANodeID[0] = 0) and (ANodeID[1] = 0)
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

function NullArrayToString(var ANullArray: array of Byte): String;
var
  i: Integer;
begin
  Result := '';
  i := 0;
  while ANullArray[i] <> 0 do
  begin
    Result := Result + Chr( ANullArray[i]);
    Inc(i);
  end;
end;

function EventIDToString(EventID: TEventID; InsertDots: Boolean): String;
var
  i: Integer;
begin
  Result := '';
  if InsertDots then
  begin
    for i := 0 to MAX_EVENT_LEN - 1 do
    begin
      if i < MAX_EVENT_LEN - 1 then
        Result := Result + IntToHex(EventID[i], 2) + '.'
      else
        Result := Result + IntToHex(EventID[i], 2);
    end;
  end else
  begin
    for i := 0 to MAX_EVENT_LEN - 1 do
      Result := Result + IntToHex(EventID[i], 2);
  end;
end;

function ExtractDataBytesAsInt(DataArray: array of Byte; StartByteIndex, EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( DataArray[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

function ValidateNodeIDAsHexString(NodeID: string): Boolean;
begin
  Result := ValidateNodeID(StrToNodeID(NodeID));
end;

function ValidateNodeID(NodeID: TNodeID): Boolean;
begin
  // Upper byte must be 0
  Result := ((NodeID[0] and $FF000000) = 0) and  ((NodeID[1] and $FF000000) = 0)
end;

function StrToNodeID(NodeID: string): TNodeID;
var
  Temp: QWord;
  ANodeID: TNodeID;
begin
  NodeID := Trim(NodeID);
  {$IFDEF FPC}
  Temp := StrToQWord(NodeID);
  {$ELSE}
  Temp := StrToInt64(NodeID);
  {$ENDIF}
  Result[0] :=  Temp and $0000000000FFFFFF;
  Result[1] := (Temp and $FFFFFFFFFF000000) shr 24;  // allow the upper nibble to be part of it to catch incorrect node id values
end;


function StrToEventID(Event: string): TEventID;
var
  i: Integer;
begin
  if Length(Event) = 16 then
  begin
    i := 0;
    {$IFDEF LCC_MOBILE}
    Result[0] := StrToInt('0x' + Event[0] + Event[1]);
    Result[1] := StrToInt('0x' + Event[2] + Event[3]);
    Result[2] := StrToInt('0x' + Event[4] + Event[5]);
    Result[3] := StrToInt('0x' + Event[6] + Event[7]);
    Result[4] := StrToInt('0x' + Event[8] + Event[9]);
    Result[5] := StrToInt('0x' + Event[10] + Event[11]);
    Result[6] := StrToInt('0x' + Event[12] + Event[13]);
    Result[7] := StrToInt('0x' + Event[14] + Event[15]);
    {$ELSE}
    Result[0] := StrToInt('0x' + Event[1] + Event[2]);
    Result[1] := StrToInt('0x' + Event[3] + Event[4]);
    Result[2] := StrToInt('0x' + Event[5] + Event[6]);
    Result[3] := StrToInt('0x' + Event[7] + Event[8]);
    Result[4] := StrToInt('0x' + Event[9] + Event[10]);
    Result[5] := StrToInt('0x' + Event[11] + Event[12]);
    Result[6] := StrToInt('0x' + Event[13] + Event[14]);
    Result[7] := StrToInt('0x' + Event[15] + Event[16]);
    {$ENDIF}
  end else
    FillChar(Result, 0, SizeOf(TEventID))
end;

{$IFNDEF LCC_WINDOWS}
  {$IFDEF FPC}

  function ResolveUnixIp: String;
  const
    CN_GDNS_ADDR = '127.0.0.1';
    CN_GDNS_PORT = 53;
  var
    sock: longint;
    err: longint;
    HostAddr: TSockAddr;
    l: Integer;
    UnixAddr: TInetSockAddr;

  begin
    err := 0;

    sock := fpsocket(AF_INET, SOCK_DGRAM, 0);
    assert(sock <> -1);

    UnixAddr.sin_family := AF_INET;
    UnixAddr.sin_port := htons(CN_GDNS_PORT);
    UnixAddr.sin_addr := StrToHostAddr(CN_GDNS_ADDR);

    if (fpConnect(sock, @UnixAddr, SizeOf(UnixAddr)) = 0) then
    begin
      try
        l := SizeOf(HostAddr);
        if (fpgetsockname(sock, @HostAddr, @l) = 0) then
        begin
          Result := NetAddrToStr(HostAddr.sin_addr);
        end
        else
        begin
          err:=socketError;
        end;
      finally
        if (fpclose(sock) <> 0) then
        begin
          err := socketError;
        end;
      end;
    end else
    begin
      err:=socketError;
    end;

    if (err <> 0) then
    begin
      // report error
    end;
  end;
  {$ELSE}
  type
    TArray4Int = array[1..4] of byte;
    PArray4Int = ^TArray4Int;

  function StrToHostAddr(IP : String): in_addr ;

    Var
      Dummy: String;
      I, j, k: Longint;
      Temp: TArray4Int;
 //     Temp: in_addr;
    begin
      Result.s_addr := 0;              //:=NoAddress;
      For I := 1 to 4 do
        begin
          If I < 4 Then
            begin
              J := Pos('.', String( IP));
              If J = 0 then
                exit;
              Dummy := Copy(String( IP) , 1, J-1);
              Delete(IP, 1, J);
            end
           else
             Dummy:=IP;
          k := StrToInt(string( Dummy));
          Temp[i] := k;
      //    PArray4Int(temp.s_addr)^[i] := k;      // Crashes Delphi
       end;
       Result.s_addr := ntohl(UInt32( Temp));
   //    Result.s_addr := ntohl(Temp.s_addr);     // Crashes Delphi
    end;

    function NetAddrToStr (Entry : in_addr) : String;
    Var
      Dummy: String;
      i, j: Longint;
    begin
      NetAddrToStr := '';
      j := entry.s_addr;
      For i := 1 to 4 do
       begin
  //       Dummy := IntToStr( PArray4Int(j)^[i]);        // Crashes Delphi
         Dummy := IntToStr( Entry.s_addr and $000000FF);
         Entry.s_addr  :=  Entry.s_addr  shr 8;
         NetAddrToStr := Result + Dummy;
         If i < 4 Then
           NetAddrToStr := Result + '.';
       end;
    end;

  function ResolveUnixIp: String;
  const
    CN_GDNS_ADDR = '127.0.0.1';
    CN_GDNS_PORT = 53;
  var
    sock: longint;
    err: longint;
    HostAddr: SockAddr;
    l: Cardinal;
    UnixAddr: sockaddr_in;
    i: Integer;

  begin
    err := 0;

    sock := socket(AF_INET, SOCK_DGRAM, 0);
    assert(sock <> -1);

    UnixAddr.sin_family := AF_INET;
    UnixAddr.sin_port := htons(CN_GDNS_PORT);
    UnixAddr.sin_addr := StrToHostAddr(CN_GDNS_ADDR);

    if (Connect(sock, psockaddr(@UnixAddr)^, SizeOf(UnixAddr)) = 0) then
    begin
      try
        l := SizeOf(HostAddr);
        if (getsockname(sock, HostAddr, l) = 0) then
          Result := NetAddrToStr(psockaddr_in( @HostAddr).sin_addr)
        else
        begin
          err:=errno;
        end;
      finally
        if (__close(sock) <> 0) then
        begin
          err := errno;
        end;
      end;
    end else
    begin
      err:=errno;
    end;

    if (err <> 0) then
    begin
      // report error
    end;
  end;
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
{ TCriticalSection }

constructor TCriticalSection.Create;
begin
  System.InitCriticalSection(Lock);
end;

destructor TCriticalSection.Destroy;
begin
  DoneCriticalsection(Lock);
end;

procedure TCriticalSection.Enter;
begin
  System.EnterCriticalsection(Lock);
end;

procedure TCriticalSection.Leave;
begin
  System.LeaveCriticalsection(Lock);
end;
{$ENDIF}

end.



