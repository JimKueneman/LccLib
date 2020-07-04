//
// Reads and Writes the Configuration Memory from/to a datagram that the Configuration Definition Info (CDI) maps out
//

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
  lcc_node_messages,
  lcc_defines;

type

{ TProtocolMemoryConfiguration }

TProtocolMemoryConfiguration = class(TNodeProtocolBase)
private
  (*
  {$IFNDEF DWSCRIPT}
  FAutoSaveOnWrite: Boolean;
  FAutosaveFilePath: String;
  {$ENDIF}   *)
protected
public
  constructor Create(ASendMessageFunc: TOnMessageEvent); override;
  destructor Destroy; override;

  procedure DatagramWriteRequest(LccMessage: TLccMessage; AStream: TStream); override;

(*  {$IFNDEF DWSCRIPT}
  procedure LoadFromFile;
  {$ENDIF}   *)
end;


implementation

{ TProtocolMemoryConfiguration }

constructor TProtocolMemoryConfiguration.Create(ASendMessageFunc: TOnMessageEvent );
begin
 inherited Create(ASendMessageFunc );
// AutoSaveOnWrite := False;
end;


destructor TProtocolMemoryConfiguration.Destroy;
begin
 inherited Destroy;
end;

(*
{$IFNDEF DWSCRIPT}
procedure TProtocolMemoryConfiguration.LoadFromFile;
begin
 if FileExists(String( AutosaveFilePath)) then
   AStream.LoadFromFile(String( AutosaveFilePath))
end;
{$ENDIF}
*)

procedure TProtocolMemoryConfiguration.DatagramWriteRequest(LccMessage: TLccMessage;
  AStream: TStream);
begin
  inherited DatagramWriteRequest(LccMessage, AStream);

  (*
  {$IFNDEF DWSCRIPT}
  if AutoSaveOnWrite then
  begin
    Assert(AutosaveFilePath = '', 'Configuration filename not set with AutoSaveOnWrite enabled');
    if not FileExists(String( AutosaveFilePath)) then
    begin
      if not DirectoryExists(ExtractFilePath(AutosaveFilePath)) then
        if ForceDirectories(ExtractFileDir(AutosaveFilePath)) then
          AStream.SaveToFile(String( AutosaveFilePath))
    end else
      AStream.SaveToFile(String( AutosaveFilePath))
  end;
  {$ENDIF}    *)
end;

end.

