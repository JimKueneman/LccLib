unit lcc_protocol_acdi;

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
  lcc_defines,
  lcc_node_messages;

type

{ TACDIMfg }

TACDIMfg = class(TStreamBasedProtocol)
public
  procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage); override;
end;

{ TACDIUser }

TACDIUser = class(TACDIMfg)
public
  procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage); override;
  procedure WriteRequest(LccMessage: TLccMessage); override;
end;


implementation

{ TACDIMfg }

procedure TACDIMfg.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
var
  i, Offset: Integer;
  ReadCount: Integer;
  Address: DWord;
  FlatArray: array[0..ACDI_MFG_SIZE - 1] of Byte;
  //JDK  SNIP: TSimpleNodeInfo;
begin
  // Assumption is this is a datagram message
  ReadCount := LccMessage.ExtractDataBytesAsInt(7, 7);
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArrayIndexer[0];
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArrayIndexer[1] or $10;   // Make it a reply
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];          // Copy the address
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArrayIndexer[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArrayIndexer[5];
  OutMessage.DataArrayIndexer[6] := LccMessage.DataArrayIndexer[6];

  FlatArray[0] := 0;
  {$IFDEF DWSCRIPT}
  for i := 0 to ACDI_MFG_SIZE - 1 do
    FlatArray[i] := 0;
  {$ELSE}
  FillChar(FlatArray, ACDI_MFG_SIZE, #0);
  {$ENDIF}
   //JDK
 { SNIP := (Owner as TLccOwnedNode).SimpleNodeInfo;
  FlatArray[0] := SNIP.Version;
  Offset := ACDI_MFG_OFFSET_MANUFACTURER;
  StringToNullArray(SNIP.Manufacturer, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_MODEL;
  StringToNullArray(SNIP.Model, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_HARDWARE_VERSION;
  StringToNullArray(SNIP.HardwareVersion, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_SOFTWARE_VERSION;
  StringToNullArray(SNIP.SoftwareVersion, FlatArray, Offset);
 }
  OutMessage.DataCount := ReadCount + 7;
  for i := 0 to ReadCount - 1 do
    OutMessage.DataArrayIndexer[i + 7] := FlatArray[Address + DWord( i)];
  OutMessage.UserValid := True;
end;

{ TACDIUser }

procedure TACDIUser.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
var
  i, Offset: Integer;
  ReadCount: Integer;
  Address: DWord;
  FlatArray: array[0..ACDI_USER_SIZE - 1] of Byte;
  //JDK  SNIP: TSimpleNodeInfo;
begin
  FlatArray[0] := 0;
  // Assumption is this is a datagram message
  ReadCount := LccMessage.ExtractDataBytesAsInt(7, 7);
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArrayIndexer[0];
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArrayIndexer[1] or $10;   // Make it a reply
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];          // Copy the address
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArrayIndexer[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArrayIndexer[5];
  OutMessage.DataArrayIndexer[6] := LccMessage.DataArrayIndexer[6];

  FillChar(FlatArray, ACDI_USER_SIZE, #0);

  //JDK
 { SNIP := (Owner as TLccOwnedNode).SimpleNodeInfo;
  FlatArray[0] := SNIP.UserVersion;
  Offset := ACDI_USER_OFFSET_NAME;
  StringToNullArray(SNIP.UserName, FlatArray, Offset);
  Offset := ACDI_USER_OFFSET_DESCRIPTION;
  StringToNullArray(SNIP.UserDescription, FlatArray, Offset);
  }
  OutMessage.DataCount := ReadCount + 7;
  for i := 0 to ReadCount - 1 do
    OutMessage.DataArrayIndexer[i + 7] := FlatArray[Address + DWord(i)];
  OutMessage.UserValid := True;
end;

procedure TACDIUser.WriteRequest(LccMessage: TLccMessage);
var
  //JDK  Configuration: TConfiguration;
  Address: DWord;
begin
  // We should never allow the Version to be written too so never write to 0 offset
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  if Address > 0 then
  begin
//JDK    Configuration := (Owner as TLccOwnedNode).Configuration;
 //JDK   Configuration.WriteRequest(LccMessage);
  end;
end;

end.

