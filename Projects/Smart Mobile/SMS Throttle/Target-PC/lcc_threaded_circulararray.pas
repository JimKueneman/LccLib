unit lcc_threaded_circulararray;

interface

uses
  Classes,
  {$IFDEF FPC}
  syncobjs,
  {$ELSE}
  System.SyncObjs,
  {$ENDIF}
  SysUtils,
  lcc_defines;

type

  TCircularArray = array[0..512] of Byte;

  { TThreadedCirularArray }

  TThreadedCirularArray = class
  private
    FCircularArray: TCircularArray;
    FCount: Word;
    FHead: Word;
    FLock: TCriticalSection;
    FTail: Word;
  protected
    property Lock: TCriticalSection read FLock write FLock;
    property CircularArray: TCircularArray read FCircularArray write FCircularArray;
    property Head: Word read FHead write FHead;
    property Tail: Word read FTail write FTail;
  public
    property Count: Word read FCount;
    constructor Create;
    destructor Destroy; override;
    procedure LockArray;
    procedure UnLockArray;
    procedure AddChunk(AChunk: TLccDynamicByteArray);
    procedure PullArray(var AChunk: TLccDynamicByteArray);
  end;

implementation

{ TThreadedCirularArray }

procedure TThreadedCirularArray.AddChunk(AChunk: TLccDynamicByteArray);
var
  i: Integer;
begin
  LockArray;
  try
     // Do we have room?
     if Length(FCircularArray) - Count > Length(AChunk) then
     begin
       for i := 0 to Length(AChunk) - 1 do
       begin
         FCircularArray[Tail] := AChunk[i];
         Inc(FTail);
         Inc(FCount);
         if Tail >= Length(FCircularArray) then
           Tail := 0;
       end
     end;
    finally
    UnLockArray;
  end;
end;

constructor TThreadedCirularArray.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TThreadedCirularArray.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TThreadedCirularArray.LockArray;
begin
  FLock.Enter;
end;

procedure TThreadedCirularArray.PullArray(var AChunk: TLccDynamicByteArray);
var
  i: Integer;
  LocalCount: Word;
begin
  LockArray;
  try
     if Count > 0 then
     begin
       LocalCount := Count;
       SetLength(AChunk, Count);
       for i := 0 to LocalCount - 1 do
       begin
         AChunk[i] := CircularArray[Head];
         Inc(FHead);
         Dec(FCount);
         if Head >= Length(FCircularArray) then
           Head := 0;
       end
     end;
    finally
    UnLockArray;
  end;
end;

procedure TThreadedCirularArray.UnLockArray;
begin
  FLock.Leave;
end;

end.

