unit lcc_protocol_memory_configuration;
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
  lcc_protocol_base,
  lcc_node_messages;

type

{ TProtocolMemoryConfiguration }

TProtocolMemoryConfiguration = class(TStreamBasedProtocol)
private
  FAutoSaveOnWrite: Boolean;
  FFilePath: String;
protected
public
  property AutoSaveOnWrite: Boolean read FAutoSaveOnWrite write FAutoSaveOnWrite;
  property FilePath: String read FFilePath write FFilePath;

  constructor Create(ASendMessageFunc: TLccSendMessageFunc; AnAddressSpace: Byte; IsStringBasedStream: Boolean); override;
  destructor Destroy; override;

  procedure WriteRequest(LccMessage: TLccMessage); override;
  function ReadAsString(Address: DWord): String;
  procedure LoadFromFile;
end;


implementation

{ TProtocolMemoryConfiguration }

constructor TProtocolMemoryConfiguration.Create(ASendMessageFunc: TLccSendMessageFunc; AnAddressSpace: Byte; IsStringBasedStream: Boolean);
begin
 inherited Create(ASendMessageFunc, AnAddressSpace, IsStringBasedStream);
 AutoSaveOnWrite := False;
end;

destructor TProtocolMemoryConfiguration.Destroy;
begin
 inherited Destroy;
end;

procedure TProtocolMemoryConfiguration.LoadFromFile;
begin
 if FileExists(String( FilePath)) then
   AStream.LoadFromFile(String( FilePath))
end;

function TProtocolMemoryConfiguration.ReadAsString(Address: DWord): String;
var
 i: DWord;
 C: Char;
 Done: Boolean;
begin
 Result := '';
 if AStream.Size > Address then
 begin
   AStream.Position := Address;
   i := 0;
   Done := False;
   while (i + Address < DWord( AStream.Size)) and not Done do
   begin
     {$IFDEF FPC}
     C := Chr(AStream.ReadByte);
     {$ELSE}
     AStream.Read(C, 1);
     {$ENDIF}
     if C <> #0 then
       Result := Result + C
     else
       Done := True;
     Inc(i)
   end;
 end;
end;

procedure TProtocolMemoryConfiguration.WriteRequest(LccMessage: TLccMessage);
var
 i: Integer;
 iStart : Integer;
 WriteCount,Address: DWord;
 {$IFNDEF FPC}
 AByte: Byte;
 {$ENDIF}
begin
 // Assumption is this is a datagram message
 if LccMessage.DataArrayIndexer[1] and $03 = 0 then
   iStart := 7
 else
   iStart := 6;
 WriteCount := LccMessage.DataCount - iStart;
 Address := LccMessage.ExtractDataBytesAsInt(2, 5);
 if Address + WriteCount > DWord( AStream.Size) then
   AStream.Size := Int64( Address) + Int64(WriteCount);
 AStream.Position := Address;
 for i := iStart to LccMessage.DataCount - 1 do
 begin
   {$IFDEF FPC}
    AStream.WriteByte(LccMessage.DataArrayIndexer[i]);
   {$ELSE}
   AByte := LccMessage.DataArrayIndexer[i];
   AStream.Write(AByte, 1);
   {$ENDIF}
 end;
 if AutoSaveOnWrite then
 begin
   Assert(FilePath = '', 'Configuration filename not set with AutoSaveOnWrite enabled');
   if not FileExists(String( FilePath)) then
   begin
     if not DirectoryExists(ExtractFilePath(FilePath)) then
       if ForceDirectories(ExtractFileDir(FilePath)) then
         AStream.SaveToFile(String( FilePath))
   end else
     AStream.SaveToFile(String( FilePath))
 end;
end;

end.

