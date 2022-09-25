unit lcc_train_server;

interface

{$IFNDEF DWSCRIPT}
{$I lcc_compilers.inc}
{$ENDIF}

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
  {$IFNDEF ULTIBO}
    {$IFDEF FPC}
      {$IFNDEF FPC_CONSOLE_APP}
        ExtCtrls,
      {$ENDIF}
    {$ELSE}
      System.Types,
      FMX.Types,
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF DWSCRIPT}
    {$IFDEF FPC}
    contnrs,
    {$ELSE}
      System.Generics.Collections,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
  lcc_alias_server;

type

  { TLccTrainObject }

  TLccTrainObject = class
  private
    FNodeAlias: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;

  end;

  { TLccTrainServer }

  TLccTrainServer = class
  private
    {$IFDEF DELPHI}
    FTrainList: TObjectList<TLccTrainObject>;
    {$ELSE}
    FTrainList: TObjectList;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function AddTrainObject(NewNodeID: TNodeID; NewAlias: Word): TLccTrainObject;
    function FindTrainObject(TestNodeID: TNodeID; TestAlias: Word): TLccTrainObject;

    {$IFDEF DELPHI}
    property TrainList: TObjectList<TLccTrainServerObject> read FTrainList write FTrainList;
    {$ELSE}
    property TrainList: TObjectList read FTrainList write FTrainList;
    {$ENDIF}
  end;

implementation

{ TLccTrainServer }

constructor TLccTrainServer.Create;
begin
  {$IFDEF DELPHI}
  FTrainList := TObjectList<TLccTrainObject>.Create(True);
  {$ELSE}
  FTrainList := TObjectList.Create(True);
  {$ENDIF}
end;

destructor TLccTrainServer.Destroy;
begin
  FreeAndNil(FTrainList);
  inherited Destroy;
end;

function TLccTrainServer.AddTrainObject(NewNodeID: TNodeID; NewAlias: Word): TLccTrainObject;
begin
  Result := TLccTrainObject.Create;
  Result.NodeAlias := NewAlias;
  Result.NodeID := NewNodeID;
end;

function TLccTrainServer.FindTrainObject(TestNodeID: TNodeID; TestAlias: Word): TLccTrainObject;
var
  i: Integer;
  TrainObject: TLccTrainObject;
begin
  Result := nil;
  for i := 0 to TrainList.Count - 1 do
  begin
    TrainObject := TrainList.Items[i] as TLccTrainObject;
    if EqualNode(TestNodeID, TestAlias, TrainObject.NodeID, TrainObject.NodeAlias, True) then
    begin
      Result := TrainObject;
      Break
    end;
  end;
end;

end.

