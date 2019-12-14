unit lcc_utilities;

interface

{$i lcc_compilers.inc}

uses
 {$IFDEF DWSCRIPT}
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
  {$ELSE}
    Classes,
    SysUtils,
    {$IFDEF LCC_WINDOWS}
    Windows,
    {$ELSE}
      {$IFDEF FPC}
        {$IFNDEF FPC_CONSOLE_APP}
        LclIntf,
        {$ENDIF}
        {$IFDEF ULTIBO}
        console,
        {$ELSE}
        baseUnix,
        {$ENDIF}
        sockets,
      {$ELSE}
      strutils, Posix.NetinetIn, Posix.ArpaInet, Posix.SysSocket, Posix.Errno, Posix.Unistd,
      {$ENDIF}
    {$ENDIF}

    {$IFNDEF ULTIBO}
    blcksock,
    {$ELSE}
    Winsock2,
    {$ENDIF}
  {$ENDIF}
  lcc_defines;

  function EqualNodeID(NodeID1: TNodeID; NodeID2: TNodeID; IncludeNullNode: Boolean): Boolean;
  function EqualEventID(EventID1, EventID2: TEventID): Boolean;
  procedure NodeIDToEventID(NodeID: TNodeID; LowBytes: Word; var EventID: TEventID);
  function NullNodeID(ANodeID: TNodeID): Boolean;
  procedure StringToNullArray(AString: String; var ANullArray: array of Byte; var iIndex: Integer);
  function EventIDToString(EventID: TEventID; InsertDots: Boolean): String;
  function NodeIDToString(NodeID: TNodeID; InsertDots: Boolean): String;
  procedure NodeIDStringToNodeID(ANodeIDStr: String; var ANodeID: TNodeID);
  function StrToNodeID(NodeID: string): TNodeID;
  function StrToEventID(Event: string): TEventID;
  function _Lo(Data: DWORD): Byte;
  function _Hi(Data: DWORD): Byte;
  function _Higher(Data: DWORD): Byte;
  function _Highest(Data: DWORD): Byte;
  {$IFNDEF DW_SCRIPT}
  function _Highest1(Data: QWord): Byte;
  function _Highest2(Data: QWord): Byte;
  {$IFDEF LCC_WINDOWS}
  function ResolveWindowsIp(Socket: TBlockSocket): string;
  {$ELSE}
    {$IFDEF ULTIBO}
      function ResolveUltiboIp: String;
      function GetNetworkConnected:Boolean;
      procedure WaitForNetworkConnection(PrintToConsole: Boolean);
    {$ELSE}
      function ResolveUnixIp: String;
    {$ENDIF}
  {$ENDIF}

  {$ENDIF}

{$IFDEF DWSCRIPT}
  function Lo(Data: Word): Byte;
  function Hi(Data: Word): Byte;
{$ENDIF}

implementation

{$IFDEF LCC_WINDOWS}
  function ResolveWindowsIp(Socket: TBlockSocket): String;
  var
    IpStrings: TStringList;
    LocalName: String;
    i: Integer;
    LocalSocket: TTCPBlockSocket;
  begin
    Result := '';
    LocalSocket := nil;
    if not Assigned(Socket) then
    begin
      LocalSocket := TTCPBlockSocket.Create;
      Socket := LocalSocket;
    end;
    LocalName := Socket.LocalName;
    IpStrings := TStringList.Create;
    try
       Socket.ResolveNameToIP(LocalName, IpStrings) ;  // '192.168.0.8';
       for i := 0 to IpStrings.Count - 1 do
         Result := IpStrings[i];
    finally
      IpStrings.Free;
      if Assigned(LocalSocket) then
        LocalSocket.Free;
    end;
  end;
{$ELSE}
  {$IFNDEF ULTIBO}
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
  {$ELSE} // Is ULTIBO
    function ResolveUltiboIp: String;
    var
      Winsock2TCPClient:TWinsock2TCPClient;
    begin
     {Create TCP Client}
     Winsock2TCPClient:=TWinsock2TCPClient.Create;
     {Get the Address}
     Result:=Winsock2TCPClient.LocalAddress;
     {Destroy the Client}
     Winsock2TCPClient.Free;
    end;

  {$ENDIF}
{$ENDIF}

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

function NodeIDToString(NodeID: TNodeID; InsertDots: Boolean): String;
var
  i: Integer;
begin
  Result := '';
  if InsertDots then
  begin
    for i := MAX_NODEID_LEN - 1 downto 0 do
    begin
      if i > 0 then
      begin
        if i < MAX_NODEID_LEN div 2 then
          Result := Result + IntToHex(((NodeID[0] shr (i*8)) and $0000FF), 2) + '.'
        else
          Result := Result + IntToHex(((NodeID[1] shr ((i-3)*8)) and $0000FF), 2) + '.'
      end else
      begin
         if i < MAX_NODEID_LEN div 2 then
          Result := Result + IntToHex(((NodeID[0] shr (i*8)) and $0000FF), 2)
        else
          Result := Result + IntToHex(((NodeID[1] shr ((i-3)*8)) and $0000FF), 2)
      end;
    end;
  end else
  begin
    for i := MAX_NODEID_LEN - 1 downto 0 do
    begin
      if i < MAX_NODEID_LEN div 2 then
        Result := Result + IntToHex(((NodeID[0] shr (i*8)) and $0000FF), 2)
      else
        Result := Result + IntToHex(((NodeID[1] shr ((i-3)*8)) and $0000FF), 2)
    end
  end;
end;

procedure NodeIDStringToNodeID(ANodeIDStr: String; var ANodeID: TNodeID);
var
  TempStr: String;
  TempNodeID: QWord;
begin
  ANodeIDStr := Trim( String( ANodeIDStr));
  TempStr := StringReplace(String( ANodeIDStr), '0x', '', [rfReplaceAll, rfIgnoreCase]);
  TempStr := StringReplace(String( TempStr), '$', '', [rfReplaceAll, rfIgnoreCase]);
  try
    TempNodeID := StrToInt64('$' + String( TempStr));
    ANodeID[0] := DWord( TempNodeID and $0000000000FFFFFF);
    ANodeID[1] := DWord( (TempNodeID shr 24) and $0000000000FFFFFF);
  except
    ANodeID[0] := 0;
    ANodeID[1]  := 0;
  end;
end;

function StrToEventID(Event: string): TEventID;
var
  TempEvent: string;
  TempChar: Char;
  i: Integer;
begin
  Event := Trim(Event);
  if Length(Event) = 23 then
  begin
    TempEvent := '';
    {$IFDEF LCC_MOBILE}
    for i := 0 to 22 do
    {$ELSE}
    for i := 1 to 23 do
    {$ENDIF}
    begin
      TempChar := Event[i];
      if TempChar <> '.' then
        TempEvent := TempEvent + TempChar;
    end;
    Event := TempEvent;
  end;

  if Length(Event) = 16 then
  begin
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

function StrToNodeID(NodeID: string): TNodeID;
var
  Temp: QWord;
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

{$IFNDEF DW_SCRIPT}
function _Highest1(Data: QWord): Byte;
begin
  Result := Byte((Data shr 32) and $00000000000000FF);
end;

function _Highest2(Data: QWord): Byte;
begin
  Result := Byte((Data shr 40) and $00000000000000FF);
end;
{$ENDIF}

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
