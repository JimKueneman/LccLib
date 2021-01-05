unit lcc_protocol_simplenodeinfo;

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
  System.Memory.Buffer,
  System.Memory,
{$ELSE}
  Classes,
  SysUtils,
{$ENDIF}
  lcc_protocol_base,
  lcc_defines,
  lcc_node_messages,
  lcc_utilities;

type

  { TProtocolSimpleNodeInfo }

  TProtocolSimpleNodeInfo = class(TNodeProtocolBase)
  public
    function PackedFormat(StreamManufacturerInfo, StreamConfiguration: TStream): TLccDynamicByteArray;
  end;

implementation

{ TProtocolSimpleNodeInfo }

function TProtocolSimpleNodeInfo.PackedFormat(StreamManufacturerInfo, StreamConfiguration: TStream): TLccDynamicByteArray;
var
  Len, i: Integer;
  AByte: Byte;
begin

  Result := nil;

  Len := 1; // Version number
  StreamManufacturerInfo.Position := ADDRESS_MFG_NAME;
  while StreamReadByte(StreamManufacturerInfo) <> Ord(#0) do
   Inc(Len);
  StreamManufacturerInfo.Position := ADDRESS_MODEL_NAME;
  while StreamReadByte(StreamManufacturerInfo) <> Ord(#0) do
   Inc(Len);
  StreamManufacturerInfo.Position := ADDRESS_HARDWARE_VERSION;
  while StreamReadByte(StreamManufacturerInfo) <> Ord(#0) do
   Inc(Len);
  StreamManufacturerInfo.Position := ADDRESS_SOFTWARE_VERSION;
  while StreamReadByte(StreamManufacturerInfo) <> Ord(#0) do
   Inc(Len);
  Inc(Len); // Version Number
  StreamConfiguration.Position := ADDRESS_USER_NAME;
  while StreamReadByte(StreamConfiguration) <> Ord(#0) do
   Inc(Len);
  StreamConfiguration.Position := ADDRESS_USER_DESCRIPTION;
  while StreamReadByte(StreamConfiguration) <> Ord(#0) do
   Inc(Len);
  Inc(Len, 6);  // six NULLs for 6 strings

  {$IFDEF DWSCRIPT}
  var BinaryData: TBinaryData;
  BinaryData := TBinaryData.Create(Len);
  Result := BinaryData.ToBytes;
  {$ELSE}
  SetLength(Result, Len);
  {$ENDIF}

  i := 0;
  Result[i] := 1; // Version number
  Inc(i);

  StreamManufacturerInfo.Position := ADDRESS_MFG_NAME;
  AByte := StreamReadByte(StreamManufacturerInfo);
  while AByte <> Ord(#0) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamManufacturerInfo);
    Inc(i);
  end;
  Result[i] := Ord(#0);  // null terminate string
  Inc(i);

  StreamManufacturerInfo.Position := ADDRESS_MODEL_NAME;
  AByte := StreamReadByte(StreamManufacturerInfo);
  while AByte <> Ord(#0) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamManufacturerInfo);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
  Inc(i);

  StreamManufacturerInfo.Position := ADDRESS_HARDWARE_VERSION;
  AByte := StreamReadByte(StreamManufacturerInfo);
  while AByte <> Ord( Ord(#0)) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamManufacturerInfo);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
  Inc(i);

  StreamManufacturerInfo.Position := ADDRESS_SOFTWARE_VERSION;
  AByte := StreamReadByte(StreamManufacturerInfo);
  while AByte <> Ord( Ord(#0)) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamManufacturerInfo);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
  Inc(i);

  Result[i] := 1; // Version number
  Inc(i);

  StreamConfiguration.Position := ADDRESS_USER_NAME;
  AByte := StreamReadByte(StreamConfiguration);
  while AByte <> Ord( Ord(#0)) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamConfiguration);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
  Inc(i);

  StreamConfiguration.Position := ADDRESS_USER_DESCRIPTION;
  AByte := StreamReadByte(StreamConfiguration);
  while AByte <> Ord( Ord(#0)) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamConfiguration);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
end;

end.

