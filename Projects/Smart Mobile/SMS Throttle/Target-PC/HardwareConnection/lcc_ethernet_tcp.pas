unit lcc_ethernet_tcp;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

 uses
  Classes, SysUtils,
  {$IFNDEF FPC}
  Types,
  {$ENDIF}
  lcc_defines;

 const
  STATE_FLAGS_BYTE_1 = 0;
  STATE_FLAGS_BYTE_2 = 1;
  STATE_SIZE_BYTE_1  = 2;
  STATE_SIZE_BYTE_2  = 3;
  STATE_SIZE_BYTE_3  = 4;
  STATE_GATEWAY_BYTE_1 = 5;
  STATE_GATEWAY_BYTE_2 = 6;
  STATE_GATEWAY_BYTE_3 = 7;
  STATE_GATEWAY_BYTE_4 = 8;
  STATE_GATEWAY_BYTE_5 = 9;
  STATE_GATEWAY_BYTE_6 = 10;
  STATE_CAPTURE_TIME_BYTE_1 = 11;
  STATE_CAPTURE_TIME_BYTE_2 = 12;
  STATE_CAPTURE_TIME_BYTE_3 = 13;
  STATE_CAPTURE_TIME_BYTE_4 = 14;
  STATE_CAPTURE_TIME_BYTE_5 = 15;
  STATE_CAPTURE_TIME_BYTE_6 = 16;
  STATE_NEXT_MESSAGE_BYTE   = 17;

type

  { TOPStackcoreTcpDecodeStateMachine }

  TOPStackcoreTcpDecodeStateMachine = class
  private
    FDecodeTcpMessage: TLccDynamicByteArray;
    FiMessage: Word;
    FMessageFlags: Word;
    FMessageSize: DWord;
    FTcpReceiveState: Word;
  protected
    property TcpReceiveState: Word read FTcpReceiveState write FTcpReceiveState;
    property MessageFlags: Word read FMessageFlags write FMessageFlags;
    property DecodeTcpMessage: TLccDynamicByteArray read FDecodeTcpMessage write FDecodeTcpMessage;
    property MessageSize: DWord read FMessageSize write FMessageSize;
    property iMessage: Word read FiMessage write FiMessage;
  public
    constructor Create;
    function OPStackcoreTcp_DecodeMachine(NextByte: Byte; var TcpMessage: TLccDynamicByteArray): Boolean;
    procedure Reset;
  end;

implementation


{ TOPStackcoreTcpDecodeStateMachine }

constructor TOPStackcoreTcpDecodeStateMachine.Create;
begin
  inherited Create;
  Reset;
end;

function TOPStackcoreTcpDecodeStateMachine.OPStackcoreTcp_DecodeMachine(NextByte: Byte; var TcpMessage: TLccDynamicByteArray): Boolean;
begin
  Result := False;
  case TcpReceiveState of
    STATE_FLAGS_BYTE_1 :
      begin
        SetLength(FDecodeTcpMessage, MAX_HEADER_ONLY_LEN);
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_FLAGS_BYTE_2  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_SIZE_BYTE_1  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_SIZE_BYTE_2  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_SIZE_BYTE_3  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_GATEWAY_BYTE_1  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_GATEWAY_BYTE_2  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_GATEWAY_BYTE_3  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_GATEWAY_BYTE_4  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_GATEWAY_BYTE_5  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_GATEWAY_BYTE_6  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_CAPTURE_TIME_BYTE_1  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_CAPTURE_TIME_BYTE_2  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_CAPTURE_TIME_BYTE_3  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_CAPTURE_TIME_BYTE_4  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_CAPTURE_TIME_BYTE_5  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);
        Exit;
      end;
    STATE_CAPTURE_TIME_BYTE_6  :
      begin
        DecodeTcpMessage[TcpReceiveState] := NextByte;
        Inc(FTcpReceiveState);

        MessageFlags := (DecodeTcpMessage[0] shl 8) or DecodeTcpMessage[1];
        if MessageFlags and OPSTACK_TCP_FLAG_CHAINING = OPSTACK_TCP_FLAG_CHAINING then
          TcpReceiveState := STATE_FLAGS_BYTE_1  // More headers
        else begin
          MessageSize := (DecodeTcpMessage[2] shl 16) or (DecodeTcpMessage[3] shl 8) or DecodeTcpMessage[4];
          MessageSize := MessageSize - MAX_HEADER_CONTRIBUTION_TO_SIZE_FIELD_LEN;
          SetLength(FDecodeTcpMessage, MessageSize + MAX_HEADER_ONLY_LEN);
          iMessage := TcpReceiveState;
        end;

        Exit;
      end;
    STATE_NEXT_MESSAGE_BYTE :
      begin
        DecodeTcpMessage[iMessage] := NextByte;
        Inc(FiMessage);
        if iMessage >= Length(DecodeTcpMessage) then
        begin
          TcpMessage := DecodeTcpMessage;             // All done
          TcpReceiveState := STATE_FLAGS_BYTE_1;      // Next Message
          Result := True;
        end;
      end;
  end

end;

procedure TOPStackcoreTcpDecodeStateMachine.Reset;
begin
  FDecodeTcpMessage := nil;
  FiMessage := 0;
  FMessageFlags := 0;
  FMessageSize := 0;
  FTcpReceiveState := 0;
end;


end.

