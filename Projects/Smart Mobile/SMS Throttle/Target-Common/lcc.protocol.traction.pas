unit lcc.protocol.traction;

interface

uses
{$IFDEF DWSCRIPT}
  System.Types,
  System.Types.Convert,
  System.Time,
  System.Streams,
  System.Reader,
  System.Writer,
  System.Lists,
  System.Device.Storage,
  SmartCL.Device.Storage,
  SmartCL.Application,
  SmartCL.Components,
  SmartCL.System,
{$ELSE}
  Classes,
  SysUtils,
{$ENDIF}
  lcc.protocol.base,
  lcc_defines,
  lcc_messages,
  lcc_utilities;

type

  { TTraction }

  TTraction = class(TNodeProtocolBase)
  private
    FLegacySpeedSteps: Byte;
    FLegacyTechnology: Byte;
    FLegacyTrainID: Word;
  //JDK      FLinkedNode: TLccNode;                 // depends on the Node: Throttle Node = Linked Train Node, Train Node = Linked Throttle Node
  //JDK       FScratchNode: TLccNode;
  //JDK    FSpeed: THalfFloat;
  // JDK    FSpeedActual: THalfFloat;
  // JDK    FSpeedCommanded: THalfFloat;
    procedure SetFunctions(Index: DWord; AValue: Word);
    function GetFunctions(Index: DWord): Word;
  protected
    FunctionArray: array of Word;
    procedure GrowArray(NewSize: DWord);
  public
  //JDK    property Speed: THalfFloat read FSpeed;
  //JDK    property SpeedActual: THalfFloat read FSpeedActual;
  //JDK    property SpeedCommanded: THalfFloat read FSpeedCommanded;
    property Functions[Index: DWord]: Word read GetFunctions;
  //JDK      property LinkedNode: TLccNode read FLinkedNode write FLinkedNode;
    property LegacyTechnology: Byte read FLegacyTechnology write FLegacyTechnology;
    property LegacyTrainID: Word read FLegacyTrainID write FLegacyTrainID;
    property LegacySpeedSteps: Byte read FLegacySpeedSteps write FLegacySpeedSteps;
    //JDK       property ScratchNode: TLccNode read FScratchNode write FScratchNode;

    function IsLinked: Boolean;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

implementation

{ TTraction }

procedure TTraction.SetFunctions(Index: DWord; AValue: Word);
begin
  GrowArray(Index + 1);
  FunctionArray[Index] := AValue
end;

function TTraction.GetFunctions(Index: DWord): Word;
begin
  GrowArray(Index + 1);
  Result := FunctionArray[Index];
end;

procedure TTraction.GrowArray(NewSize: DWord);
var
  OldSize, i: DWord;
begin
  OldSize := Length(FunctionArray);
  if NewSize > OldSize then
  begin
    {$IFDEF DWSCRIPT}
    var BinaryData: TBinaryData;
    BinaryData := TBinaryData.Create(TMarshal.AllocMem(NewSize).Segment);
    FunctionArray := BinaryData.ToBytes;
    {$ELSE}
      SetLength(FunctionArray, NewSize);
    {$ENDIF}
    i := OldSize;
    while i < NewSize do
    begin
      FunctionArray[i] := 0;
      Inc(i)
    end
  end;
end;

function TTraction.IsLinked: Boolean;
begin
  //JDK    Result := Assigned(LinkedNode)
end;

function TTraction.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[0] of
    TRACTION_QUERY_SPEED :
        begin
 //JDK         FSpeed := LccMessage.ExtractDataBytesAsInt(1, 2);
// JDK          FSpeedCommanded := LccMessage.ExtractDataBytesAsInt(4, 5);
 //JDK         FSpeedActual := LccMessage.ExtractDataBytesAsInt(6, 7); ;
        end;
    TRACTION_QUERY_FUNCTION :
        begin
          SetFunctions(LccMessage.ExtractDataBytesAsInt(1, 3), LccMessage.ExtractDataBytesAsInt(4,5))
        end;
    TRACTION_CONTROLLER_CONFIG :
        begin
          case LccMessage.DataArrayIndexer[1] of
            TRACTION_CONTROLLER_CONFIG_ASSIGN :
                begin

                end;
            TRACTION_CONTROLLER_CONFIG_QUERY :
                begin

                end;
            TRACTION_CONTROLLER_CONFIG_NOTIFY :
                begin

                end;
          end;
        end;
    TRACTION_CONSIST :
        begin

        end;
    TRACTION_MANAGE :
        begin

        end;
  end;
end;

end.

