unit Storage;

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
  SmartCL.Storage.Local,
  System.Memory,
  System.Memory.Views,
  System.Memory.Buffer,
  System.Memory.Allocation;

type
  TNodeInfo = record
    NodeID: array[0..1] of LongWord = [0, 0];
  end;

procedure InitializeStorage;

var
  NodeInfo: TNodeInfo;

var
  W3Storage: TW3LocalStorage;

implementation

procedure InitializeStorage;
var
  s: string;
  Data: TBinaryData;
  i: Integer;
begin
  W3Storage := TW3LocalStorage.Create;
  if W3Storage.GetKeyExists('NodeID') then
  begin
    s := Trim(W3Storage.GetKeyStr('NodeID', ''));
    if s <> '' then
    begin

      Data := TBinaryData.Create(TUnManaged.AllocMemA(6));
      for i := 0 to 7 do
      begin
        Data.Bytes[i] := StrToInt(s[i+1]);
      end
    end else
    begin

      W3Storage.SetKeyStr('NodeID', '123456789000');
    end
  end;
end;

end.
