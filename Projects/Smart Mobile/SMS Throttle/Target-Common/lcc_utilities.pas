unit lcc_utilities;

interface

{$IFDEF DWSCRIPT}
{$ELSE}
{$i lcc_compilers.inc}
{$ENDIF}

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
  System.Memory.Buffer,
  System.Memory,
  {$ELSE}
    Classes,
    SysUtils,
    {$IFDEF LCC_WINDOWS}
    Windows,
    {$ELSE}
      {$IFDEF FPC}
        {$IFDEF ULTIBO}console,{$ELSE}baseUnix,{$ENDIF}
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

  function FormatStrToInt(AStr: string): string;
  function EqualNodeID(NodeID1: TNodeID; NodeID2: TNodeID; IncludeNullNode: Boolean): Boolean;
  function EqualNode(NodeID1: TNodeID; Node1AliasID: Word; NodeID2: TNodeID; Node2AliasID: Word; NodeID_OR_Alias: Boolean): Boolean;
  function EqualEventID(EventID1, EventID2: TEventID): Boolean;
  procedure NodeIDToEventID(NodeID: TNodeID; LowBytes: Word; var EventID: TEventID);
  function NullNodeID(ANodeID: TNodeID): Boolean;
  procedure StringToNullArray(AString: String; var ANullArray: TLccDynamicByteArray; var iIndex: Integer);
  function EventIDToString(EventID: TEventID; InsertDots: Boolean): String;
  function NodeIDToString(NodeID: TNodeID; InsertDots: Boolean): String;
  {$IFNDEF DWSCRIPT}
  procedure NodeIDStringToNodeID(ANodeIDStr: String; var ANodeID: TNodeID);
  function StrToNodeID(NodeID: string): TNodeID;
  {$ENDIF}
  function StrToEventID(Event: string): TEventID;
  function _Lo(Data: DWORD): Byte;
  function _Hi(Data: DWORD): Byte;
  function _Higher(Data: DWORD): Byte;
  function _Highest(Data: DWORD): Byte;
  {$IFNDEF DWSCRIPT}
    function _Highest1(Data: QWord): Byte;
    function _Highest2(Data: QWord): Byte;
    {$IFDEF LCC_WINDOWS}
      function ResolveWindowsIp(Socket: TBlockSocket): string; overload;
      function ResolveWindowsIp: string; overload;
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
  procedure FreeAndNil(AnObject: TObject);
  {$ENDIF}

  function StreamReadByte(AStream: TStream): Byte;
  procedure StreamWriteByte(AStream: TStream; AByte: Byte);

  function GridConnectToDetailedGridConnect(MessageString: string): string;


implementation

uses
  lcc_node_messages;

{$IFDEF DWSCRIPT}
var
  FBinaryData: TBinaryData;
  FOneByteArray: TLccDynamicByteArray;
{$ENDIF}


{$IFNDEF DWSCRIPT}

  {$IFDEF LCC_WINDOWS}
    function ResolveWindowsIp: string;
    var
      Socket: TTCPBlockSocket;
    begin
      Socket := TTCPBlockSocket.Create;          // Created in context of the thread
      Socket.Family := SF_IP4;                  // IP4
      Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
      Socket.HeartbeatRate := 0;
      Socket.SetTimeout(0);
      if Socket.LastError <> 0 then
        Result := 'Unknown'
      else
        Result := ResolveWindowsIp(Socket);
      try
      finally
        Socket.Free;
      end;
    end;


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

      begin
        err := 0;

        sock := socket(AF_INET, SOCK_DGRAM, 0);
        assert(sock <> -1);

        FillChar(UnixAddr, SizeOf(UnixAddr), #0);

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
{$ENDIF}

function FormatStrToInt(AStr: string): string;
begin
//  {$IFDEF IOS32}
  Result := '0x' + AStr;

 // {$ELSE}
 // Result := '$' + AStr;
 // {$ENDIF}

end;

function EqualNodeID(NodeID1: TNodeID; NodeID2: TNodeID; IncludeNullNode: Boolean): Boolean;
begin
  if IncludeNullNode then
    Result := (NodeID1[0] = NodeID2[0]) and (NodeID1[1] = NodeID2[1])
  else
    Result := not NullNodeID(NodeID1) and not NullNodeID(NodeID2) and (NodeID1[0] = NodeID2[0]) and (NodeID1[1] = NodeID2[1])
end;

function EqualNode(NodeID1: TNodeID; Node1AliasID: Word; NodeID2: TNodeID; Node2AliasID: Word; NodeID_OR_Alias: Boolean): Boolean;
begin
  if NodeID_OR_Alias then
    Result := (NodeID1[0] = NodeID2[0]) and (NodeID1[1] = NodeID2[1]) or (Node1AliasID = Node2AliasID)
  else begin
    Assert((Node1AliasID = 0) and (Node2AliasID > 0), 'Node1 AliasID is zero and the other is valid, should not be possible with GridConnect all Aliases should be valid');
    Assert((Node1AliasID > 0) and (Node2AliasID = 0),  'Node2 AliasID is zero and the other is valid, should not be possible with GridConnect all Aliases should be valid');
    Result := (NodeID1[0] = NodeID2[0]) and (NodeID1[1] = NodeID2[1]) and (Node1AliasID = Node2AliasID)
  end;
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
  EventID[0] := _Higher( NodeID[1]); // But these all need the 48 Bit Full ID in the Byte Fields
  EventID[1] := _Hi(     NodeID[1]);
  EventID[2] := _Lo(     NodeID[1]);
  EventID[3] := _Higher( NodeID[0]);
  EventID[4] := _Hi(     NodeID[0]);
  EventID[5] := _Lo(     NodeID[0]);
  EventID[6] := _Hi(LowBytes);
  EventID[7] := _Lo(LowBytes);
end;

procedure StringToNullArray(AString: String; var ANullArray: TLccDynamicByteArray; var iIndex: Integer);
var
  Len, i: Integer;
begin
  Len := Length(AString);
  if Len > 0 then
  begin
    {$IFDEF FPC}
      for i := 1 to Len do
    {$ELSE}
      {$IFDEF LCC_MOBILE}for i := 0 to Len - 1 do{$ELSE}for i := 1 to Len do{$ENDIF}
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

{$IFNDEF DWSCRIPT}
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
{$ENDIF}

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
  begin
    for i := 0 to MAX_EVENT_LEN - 1 do
      Result[i] := 0
  end
end;

{$IFNDEF DWSCRIPT}
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
{$ENDIF}

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

{$IFNDEF DWSCRIPT}
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

procedure FreeAndNil(AnObject: TObject);
begin
  AnObject.Free;
end;
{$ENDIF}

function MTI_ToString(MTI: DWord): string;

begin
  case MTI of
    MTI_CAN_CID0 : Result := 'Check ID 0';
    MTI_CAN_CID1 : Result := 'Check ID 1';
    MTI_CAN_CID2 : Result := 'Check ID 2';
    MTI_CAN_CID3 : Result := 'Check ID 3';
    MTI_CAN_CID4 : Result := 'Check ID 4';
    MTI_CAN_CID5 : Result := 'Check ID 5';
    MTI_CAN_CID6 : Result := 'Check ID 6';

    MTI_CAN_RID : Result := 'Reserve ID [RID]';
    MTI_CAN_AMD : Result := 'Alias Map Definition [AMD]';
    MTI_CAN_AME : Result := 'Alias Map Enquiry [AME]';
    MTI_CAN_AMR : Result := 'Alias Map Reset [AMR]';

    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY : begin
                                           Result := 'Datagram Single Frame:';

                                         end;
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START : begin
                                           Result := 'Datagram Start Frame:';

                                         end;
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME : Result := 'Datagram Frame';
    MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END : Result := 'Datagram End Frame';

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

    MTI_TRACTION_SIMPLE_TRAIN_INFO_REQUEST       : Result := 'Simple Train Node Info Request [STNIP]';
    MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY         : Result := 'Simple Train Node Info Reply [STNIP]';

    MTI_DATAGRAM                       : Result := 'Datagram';
    MTI_DATAGRAM_OK_REPLY              : Result := 'Datagram Reply OK';
    MTI_DATAGRAM_REJECTED_REPLY        : Result := 'Datagram Rejected Reply';

    MTI_TRACTION_REQUEST               : Result := 'Traction Protocol';
    MTI_TRACTION_REPLY                 : Result := 'Traction Reply';
    MTI_STREAM_INIT_REQUEST            : Result := 'Stream Init Request';
    MTI_STREAM_INIT_REPLY              : Result := 'Stream Init Reply';
    MTI_CAN_FRAME_TYPE_CAN_STREAM_SEND     : Result := 'Stream Send - CAN Frame';
    MTI_STREAM_PROCEED                 : Result := 'Stream Proceed';
    MTI_STREAM_COMPLETE                : Result := 'Stream Complete';
   else
    Result := 'Unknown MTI';
  end;
end;

function IsPrintableChar(C: Char): Boolean;
begin
  Result := ((Ord( C) >= 32) and (Ord( C) <= 126)) { or ((Ord( C) >= 128) and (Ord( C) <= 255))}
end;

function RawHelperDataToStr(Message: TLccMessage; ASCII: Boolean): string;
var
  j, iStart: Integer;
begin
  Result := '';
  iStart := 0;
  Result := Result + ' [';
  for j := iStart to Message.DataCount - 1 do                     // Skip the Address
  begin
    if ASCII then
    begin
      if IsPrintableChar( Chr( Message.DataArray[j])) then
        Result := Result + Chr( Message.DataArray[j])
      else
        Result := Result + '.';
    end else
    begin
      Result := Result + IntToHex(Message.DataArray[j], 2);
      if j < Message.DataCount then
        Result := Result + '.'
    end;
  end;
  Result := Result + ']';
end;

function StreamReadByte(AStream: TStream): Byte;
begin
  Result := 0;
  {$IFDEF DWSCRIPT}
  FOneByteArray := AStream.Read(1);
  Result := FOneByteArray[0];
  {$ELSE}
  AStream.Read(Result, 1);
  {$ENDIF}
end;

procedure StreamWriteByte(AStream: TStream; AByte: Byte);
begin
  {$IFDEF FPC}
  AStream.WriteByte(AByte);
  {$ELSE}
    {$IFDEF DWSCRIPT}
     FOneByteArray[0] := AByte;
     AStream.Write(FOneByteArray);
    {$ELSE}
    AStream.Write(AByte, 1)
    {$ENDIF}
  {$ENDIF}
end;

function GridConnectToDetailedGridConnect(MessageString: string): string;
var
  j, S_Len: Integer;
//  f: single;
//  Half: Word;
  Message: TLccMessage;

//  MultiFrame: TMultiFrameBuffer;
//  LocalHelper: TLccMessageHelper;
begin
//  LocalHelper := TLccMessageHelper.Create;

  Message := TLccMessage.Create;
  try
    if Message.LoadByGridConnectStr(MessageString) then
    begin
      Result := MessageString;
      S_Len := Length(Result);
      for j := 0 to (28-S_Len) do
        Result := Result + ' ' ;

      if Message.HasDestination then
        Result := Result + '0x' + IntToHex( Message.CAN.SourceAlias, 4) + ' -> ' + '0x' + IntToHex( Message.CAN.DestAlias, 4)
      else
        Result := Result + '0x' + IntToHex( Message.CAN.SourceAlias, 4);

      if Message.MTI = MTI_DATAGRAM then
        Result := Result + RawHelperDataToStr(Message, True) + ' MTI: ' + MTI_ToString(Message.MTI)
      else
        Result := Result + '   MTI: ' + MTI_ToString(Message.MTI) + ' - ';

      if Message.MTI = MTI_STREAM_SEND then
      begin
        case Message.MTI of
          MTI_STREAM_INIT_REQUEST            : Result := Result + ' Suggested Bufer Size: ' + IntToStr((Message.DataArray[2] shl 8) or Message.DataArray[3]) + ' Flags: 0x' + IntToHex(Message.DataArray[4], 2) + ' Additional Flags: 0x' + IntToHex(Message.DataArray[5], 2) + ' Source Stream ID: ' + IntToStr(Message.DataArray[6]);
          MTI_STREAM_INIT_REPLY              : Result := Result + ' Negotiated Bufer Size: ' + IntToStr((Message.DataArray[2] shl 8) or Message.DataArray[3]) + ' Flags: 0x' + IntToHex(Message.DataArray[4], 2) + ' Additional Flags: 0x' + IntToHex(Message.DataArray[5], 2) + ' Source Stream ID: ' + IntToStr(Message.DataArray[6]) + ' Destination Stream ID: ' + IntToStr(Message.DataArray[7]);
          MTI_STREAM_SEND                    : begin end;
          MTI_STREAM_PROCEED                 : Result := Result + ' Source Stream ID: ' + IntToStr(Message.DataArray[2]) + ' Destination Stream ID: ' + IntToStr(Message.DataArray[3]) + ' Flags: 0x' + IntToHex(Message.DataArray[4], 2) + ' Additional Flags: 0x' + IntToHex(Message.DataArray[5], 2);
          MTI_STREAM_COMPLETE                : Result := Result + ' Source Stream ID: ' + IntToStr(Message.DataArray[2]) + ' Destination Stream ID: ' + IntToStr(Message.DataArray[3]) + ' Flags: 0x' + IntToHex(Message.DataArray[4], 2) + ' Additional Flags: 0x' + IntToHex(Message.DataArray[5], 2);
        end
      end;

      if Message.MTI = MTI_OPTIONAL_INTERACTION_REJECTED then
      begin
      end;

      // SNII/SNIP
      if Message.MTI = MTI_SIMPLE_NODE_INFO_REPLY then
        Result := Result + RawHelperDataToStr(Message, True);

      // STNIP
      if Message.MTI = MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY then
        Result := Result + RawHelperDataToStr(Message, True);

      // Events
      if (Message.MTI = MTI_PRODUCER_IDENDIFY) or (Message.MTI = MTI_PRODUCER_IDENTIFIED_SET) or (Message.MTI = MTI_PRODUCER_IDENTIFIED_CLEAR) or
        (Message.MTI = MTI_PRODUCER_IDENTIFIED_UNKNOWN) or (Message.MTI = MTI_CONSUMER_IDENTIFY) or (Message.MTI = MTI_CONSUMER_IDENTIFIED_SET) or
        (Message.MTI = MTI_CONSUMER_IDENTIFIED_CLEAR) or (Message.MTI = MTI_CONSUMER_IDENTIFIED_UNKNOWN) or (Message.MTI = MTI_PC_EVENT_REPORT)
      then begin
          Result := Result + 'EventID: ' + EventIDToString(Message.ExtractDataBytesAsEventID(0), False);
      end;

    (*
      // Traction Protocol
      if Message.MTI = MTI_TRACTION_PROTOCOL then
      begin
        MultiFrame := MultiFrames.ProcessFrame(Message);
        if Assigned(MultiFrame) then
        begin
          case MultiFrame.DataArray[0] of
              TRACTION_SPEED_DIR :
                begin
                  Result := Result + ' LCC Speed/Dir Operation; Speed = ';
                  f := HalfToFloat( (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2]);
                  if f = 0 then
                  begin
                    if DWord( f) and $80000000 = $80000000 then
                      Result := Result + '-0.0'
                    else
                      Result := Result + '+0.0'
                  end else
                    Result := Result + IntToStr( round(f));
                end;
              TRACTION_FUNCTION : Result := Result + ' LCC Traction Operation - Function Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 3), 6) + '], Value: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(4, 5), 2) + ']';
              TRACTION_E_STOP : Result := Result + ' LCC Traction Emergency Stop';
              TRACTION_QUERY_SPEED : Result := Result + ' Query Speeds';
              TRACTION_QUERY_FUNCTION : Result := Result + ' Query Function - Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ' [0x' + IntToHex( MultiFrame.ExtractDataBytesAsInt(1, 3), 6) + ']';
              TRACTION_CONTROLLER_CONFIG :
                begin;
                  case MultiFrame.DataArray[1] of
                    TRACTION_CONTROLLER_CONFIG_ASSIGN :
                      begin
                        if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                          Result := Result + ' Controller Config Assign - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                        else
                          Result := Result + ' Controller Config Assign - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                      end;
                    TRACTION_CONTROLLER_CONFIG_RELEASE :
                      begin
                        if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                          Result := Result + ' Controller Config Release - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                        else
                          Result := Result + ' Controller Config Release - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                      end;
                    TRACTION_CONTROLLER_CONFIG_QUERY :
                      begin
                        Result := Result + ' Controller Config Query';
                      end;
                    TRACTION_CONTROLLER_CONFIG_NOTIFY :
                      begin
                        if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                          Result := Result + ' Controller Config Notify - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' [Alias: ' + MultiFrame.ExtractDataBytesAsHex(9, 10) + ']'
                        else
                          Result := Result + ' Controller Config Notify - Flags: ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Controller ID ' + MultiFrame.ExtractDataBytesAsHex(3, 8) + ' Alias not included'
                      end
                  end
                end;
              TRACTION_CONSIST :
                begin
                  case MultiFrame.DataArray[1] of
                    TRACTION_CONSIST_ATTACH : Result := Result + 'Consist Config Attach';
                    TRACTION_CONSIST_DETACH : Result := Result + 'Consist Config Detach';
                    TRACTION_CONSIST_QUERY : Result := Result + 'Consit Config Query';
                  end
                end;
              TRACTION_MANAGE :
                begin
                  case MultiFrame.DataArray[1] of
                      TRACTION_MANAGE_RESERVE : Result := Result + 'Traction Management Reserve';
                      TRACTION_MANAGE_RELEASE : Result := Result + 'Traction Management Release'
                  end
                end
          else
            Result := Result + 'Unknown Traction Operation';
          end;

          FreeAndNil(MultiFrame);
        end;
      end;

      // Traction Protocol Reply
      if LocalHelper.MTI = MTI_TRACTION_REPLY then
      begin
        MultiFrame := MultiFrames.ProcessFrame(LocalHelper);
        if Assigned(MultiFrame) then
        begin
          case MultiFrame.DataArray[0] of
              TRACTION_QUERY_SPEED :
                begin
                  Result := Result + 'Query Speed Reply : Set Speed = ';
                    Half := (MultiFrame.DataArray[1] shl 8) or MultiFrame.DataArray[2];
                    if Half = $FFFF then
                    begin
                      Result := Result + 'NaN'
                    end else
                    begin
                      f := HalfToFloat( Half);
                      if f = 0 then
                      begin
                        if DWord( f) and $80000000 = $80000000 then
                          Result := Result + '-0.0'
                        else
                          Result := Result + '+0.0'
                      end else
                        Result := Result + IntToStr( round(f));
                    end;

                    Result := Result + ': Status = ' + MultiFrame.ExtractDataBytesAsHex(3, 3);

                    Result := Result + ': Commanded Speed = ';
                    Half := (MultiFrame.DataArray[4] shl 8) or MultiFrame.DataArray[5];
                    if Half = $FFFF then
                    begin
                      Result := Result + 'NaN'
                    end else
                    begin
                      f := HalfToFloat( Half);
                      if f = 0 then
                      begin
                        if DWord( f) and $80000000 = $80000000 then
                          Result := Result + '-0.0'
                        else
                          Result := Result + '+0.0'
                      end else
                        Result := Result + IntToStr( round(f));
                    end;

                    Result := Result + ': Actual Speed = ';
                    Half := (MultiFrame.DataArray[6] shl 8) or MultiFrame.DataArray[7];
                    if Half = $FFFF then
                    begin
                      Result := Result + 'NaN'
                    end else
                    begin
                      f := HalfToFloat( Half);
                      if f = 0 then
                      begin
                        if DWord( f) and $80000000 = $80000000 then
                          Result := Result + '-0.0'
                        else
                          Result := Result + '+0.0'
                      end else
                        Result := Result + IntToStr( round(f));
                    end
                end;
              TRACTION_QUERY_FUNCTION : Result := Result + 'Query Function Reply - Address: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(1, 3)) + ', Value: ' + IntToStr( MultiFrame.ExtractDataBytesAsInt(4, 5));
              TRACTION_CONTROLLER_CONFIG :
                begin;
                  case MultiFrame.DataArray[1] of
                    TRACTION_CONTROLLER_CONFIG_ASSIGN :
                      begin
                        Result := Result + 'Controller Config Assign Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2)
                      end;
                    TRACTION_CONTROLLER_CONFIG_QUERY :
                      begin
                        if MultiFrame.ExtractDataBytesAsInt(2, 2) and TRACTION_FLAGS_ALIAS_INCLUDED <> 0 then
                          Result := Result + 'Controller Config Query Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Result = ' + MultiFrame.ExtractDataBytesAsHex(3, 3) + ' Active Controller = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(4, 9), 12) + ' Alias = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(10, 11), 4)
                        else
                          Result := Result + 'Controller Config Query Reply - Flags = ' + MultiFrame.ExtractDataBytesAsHex(2, 2) + ' Result = ' + MultiFrame.ExtractDataBytesAsHex(3, 3) + ' Active Controller = 0x' + IntToHex(MultiFrame.ExtractDataBytesAsInt(4, 9), 12);
                      end;
                    TRACTION_CONTROLLER_CONFIG_NOTIFY :
                      begin
                        Result := Result + 'Controller Config Notify Reply - Result = ' + MultiFrame.ExtractDataBytesAsHex(2, 2)
                      end;
                  end
                end;
              TRACTION_CONSIST :
                begin
                  case MultiFrame.DataArray[1] of
                    TRACTION_CONSIST_ATTACH : Result := Result + 'Consist Config Attach Reply';
                    TRACTION_CONSIST_DETACH : Result := Result + 'Consist Config Detach Reply';
                    TRACTION_CONSIST_QUERY : Result := Result + 'Consit Config Query Reply';
                  end
                end;
              TRACTION_MANAGE :
                begin
                  case MultiFrame.DataArray[1] of
                      TRACTION_MANAGE_RESERVE : Result := Result +  'Manage: Reserve' + 'Result = ' + MultiFrame.ExtractDataBytesAsHex(2, 2);
                  end
                end
          else
            Result := Result + 'Unknown Traction Reply Operation';
          end;

          FreeAndNil(MultiFrame);
        end;
      end;

      *)
    end;
  finally
    Message.Free
  end;
end;

{$IFNDEF DWSCRIPT}
  {$IFNDEF LCC_MOBILE}
  function GridConnectToJMRI(GridStr: AnsiString): AnsiString;
  var
    NPos: integer;
    Header: PAnsiChar;
    i: Integer;
  begin
    Result := GridStr;
    NPos := Pos('N', string( GridStr));
    GridStr[NPos] := #0;
    Header := @GridStr[3];
    Result := '[' + Header + ']';
    Header := @GridStr[NPos];
    Inc(Header);
    if Header^ <> ';' then
      Result := Result + ' ';
    while Header^ <> ';' do
    begin
      Result := Result + Header^;
      Inc(Header);
      if Header^ = ';' then
        Break;
      Result := Result + Header^ + ' ';
      Inc(Header);
    end;
    Result := AnsiString( Trim(string( Result)));
    for i := 0 to (40 - Length(Result)) do
      Result := Result + ' ';  // Pad them all to the same length
  end;
  {$ENDIF}
{$ENDIF}

{$IFDEF ULTIBO}
function GetNetworkConnected:Boolean;
var
 Address:String;
begin
 {}
 Result:=False;

 {Get Address}
 Address := ResolveUltiboIp;

 {Check Address}
 if Address = '' then Exit;
 if Address = '0.0.0.0' then Exit;
 if Address = '255.255.255.255' then Exit;
 if Copy(Address,1,Length('192.168.100.')) = '192.168.100.' then Exit;

 Result:=True;
end;

procedure WaitForNetworkConnection(PrintToConsole: Boolean);
begin
 if PrintToConsole then
   ConsoleWriteLn('Looking for the Raspberry Pi''s IP Address');
 while GetNetworkConnected = False do
    Sleep(1000);
 if PrintToConsole then
   ConsoleWriteLn('IP Address found: ' + ResolveUltiboIp);
end;
{$ENDIF}

initialization

{$IFDEF DWSCRIPT}
// Allocate a byte that is of TByteArray to use in stream operations in a similar manner as Lazarus in decendants
FBinaryData := TBinaryData.Create(1);
FOneByteArray := FBinaryData.ToBytes;
{$ENDIF}


finalization

end.
