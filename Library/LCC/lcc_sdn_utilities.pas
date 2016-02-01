unit lcc_sdn_utilities;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, lcc_xmlutilities,
  {$IFDEF FPC}
  contnrs,
  {$ELSE}
  System.Generics.Collections,
  {$ENDIF}
  lcc_defines, lcc_utilities;

const
  VALID_DICTIONARY: array[0..4] of string = ('occupied', 'straight', 'hi', 'on', 'valid');
  INVALID_DICTIONARY: array[0..4] of string = ('unoccupied', 'diverging', 'lo', 'off', 'invalid');

type

  TSupportsEventType = (set_None, set_LoEventID, set_HiEventID);
  TPCMap = array of TEventID;
  TLogicalActionType = (lat_Undefined, lat_Input, lat_Output);

  TLccBinaryAction = class;  //forwared

  { TLccLogicAction }

  TLccLogicAction = class
  private
    FEventIDHiLinked: TEventID;
    FEventIDLoLinked: TEventID;
    FEventStateLinked: TEventState;
    FLogicTrueStateName: string;
    FLinkedName: string;
    FLogicTrueState: TEventState;
  public
    property LinkedName: string read FLinkedName write FLinkedName;
    property EventIDLoLinked: TEventID read FEventIDLoLinked write FEventIDLoLinked;
    property EventIDHiLinked: TEventID read FEventIDHiLinked write FEventIDHiLinked;
    property EventStateLinked: TEventState read FEventStateLinked write FEventStateLinked;
    property LogicTrueStateName: string read FLogicTrueStateName write FLogicTrueStateName;
    property LogicTrueState: TEventState read FLogicTrueState write FLogicTrueState;

    function EqualAction(AnAction: TLccBinaryAction): Boolean;
  end;

  { TLccLogic }

  TLccLogic = class
  private
    FActions: TObjectList{$IFNDEF FPC}<TLccLogicAction>{$ENDIF};
    FOwner: TLccBinaryAction;
    function GetAction(Index: Integer): TLccLogicAction;
  public
    constructor Create(AnOwnerAction: TLccBinaryAction);
    destructor Destroy; override;
    function Calculate: Boolean;
    property Actions: TObjectList{$IFNDEF FPC}<TLccLogicAction>{$ENDIF} read FActions write FActions;
    property Action[Index: Integer]: TLccLogicAction read GetAction;
    property Owner: TLccBinaryAction read FOwner;
  end;

  { TLccBinaryAction }

  TLccBinaryAction = class
  private
    FActionType: TLogicalActionType;
    FConsumer: Boolean;
    FEventState: TEventState;
    FIoPin: Integer;
    FIsDirty: Boolean;
    FLogic: TLccLogic;
    FName: string;
    FProducer: Boolean;
  public
    FEventIDHi: TEventID;
    FEventIDLo: TEventID;
    constructor Create(AnActionType: TLogicalActionType);
    destructor Destroy; override;

    property ActionType: TLogicalActionType read FActionType;
    property Name: string read FName write FName;
    property EventIDLo: TEventID read FEventIDLo write FEventIDLo;
    property EventIDHi: TEventID read FEventIDHi write FEventIDHi;
    property EventState: TEventState read FEventState write FEventState;
    property Producer: Boolean read FProducer write FProducer;
    property Consumer: Boolean read FConsumer write FConsumer;
    property IoPin: Integer read FIoPin write FIoPin;
    property Logic: TLccLogic read FLogic write FLogic;
    property IsDirty: Boolean read FIsDirty write FIsDirty;
  end;

  { TLccActionGroup }

  TLccActionGroup = class
  private
    FActions: TObjectList{$IFNDEF FPC}<TLccBinaryAction>{$ENDIF};
    FLccClass: string;
    function GetAction(Index: Integer): TLccBinaryAction;
  public
    constructor Create;
    destructor Destroy; override;
    property Actions: TObjectList{$IFNDEF FPC}<TLccBinaryAction>{$ENDIF} read FActions write FActions;
    property LccClass: string read FLccClass write FLccClass;
    property Action[Index: Integer]: TLccBinaryAction read GetAction;
  end;

  { TLccObject }

  TLccObject = class
  private
    FLccClass: string;
    FDescription: string;
    FInputActionGroups: TObjectList{$IFNDEF FPC}<TLccActionGroup>{$ENDIF};
    FName: string;
    FOutputActionGroups: TObjectList{$IFNDEF FPC}<TLccActionGroup>{$ENDIF};
    function GetInputActionGroup(Index: Integer): TLccActionGroup;
    function GetOutputActionGroup(Index: Integer): TLccActionGroup;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property LccClass: string read FLccClass write FLccClass;
    property InputActionGroups: TObjectList{$IFNDEF FPC}<TLccActionGroup>{$ENDIF} read FInputActionGroups write FInputActionGroups;
    property OutputActionGroups: TObjectList{$IFNDEF FPC}<TLccActionGroup>{$ENDIF} read FOutputActionGroups write FOutputActionGroups;
    property InputActionGroup[Index: Integer]: TLccActionGroup read GetInputActionGroup;
    property OutputActionGroup[Index: Integer]: TLccActionGroup read GetOutputActionGroup;
  end;

  { TLccSdnController }

  TLccSdnController = class(TComponent)
  private
    FConsumerMap: TPCMap;
    FExternals: TStringList;
    FFilePath: string;
    FFilePathTemplate: string;
    FAvailableIoInput: Integer;
    FAvailableIoOutput: Integer;
    FActions: TObjectList{$IFNDEF FPC}<TLccBinaryAction>{$ENDIF};
    FLccObjects: TObjectList{$IFNDEF FPC}<TLccObject>{$ENDIF};
    FNodeID: TNodeID;
    FProducerIdMap: TPCMap;
    FVersion: string;
    FXmlDocument: LccXmlDocument;
    function GetActionItem(Index: Integer): TLccBinaryAction;
    function GetInputActionCount: Integer;
    function GetObjectItem(Index: Integer): TLccObject;
    function GetOuptputActionCount: Integer;
  protected
    procedure AppendToPCMap(var PCMap: TPCMap; Event: TEventID);
    procedure InternalParse;
    procedure InternalExport(XmlDoc: LccXmlDocument);
    function EventStateToAttribString(EventState: TEventState): string;
    function AttribStringToEventState(EventState: string): TEventState;
    function InValidDictionary(Value: string): Boolean;
    function InInvalidDictionary(Value: string): Boolean;

    function FindActionByName(ActionName: string): TLccBinaryAction;
    function FindActionByIoPin(IoPin: Integer): TLccBinaryAction;

    procedure LogicActionsUpdate(AnAction: TLccBinaryAction);

  public
    property AvailableIoInputs: Integer read FAvailableIoInput;
    property AvailableIoOutput: Integer read FAvailableIoOutput;
    property ConsumerIdMap: TPCMap read FConsumerMap;
    property Externals: TStringList read FExternals write FExternals;
    property FilePathTemplate: string read FFilePathTemplate write FFilePathTemplate;
    property FilePath: string read FFilePath write FFilePath;
    property Actions: TObjectList{$IFNDEF FPC}<TLccBinaryAction>{$ENDIF} read FActions write FActions;
    property ActionItem[Index: Integer]: TLccBinaryAction read GetActionItem;
    property LccObjects: TObjectList{$IFNDEF FPC}<TLccObject>{$ENDIF} read FLccObjects write FLccObjects;
    property InputActionCount: Integer read GetInputActionCount;
    property OutputActionCount: Integer read GetOuptputActionCount;
    property NodeID: TNodeID read FNodeID write FNodeID;
    property ObjectItem[Index: Integer]: TLccObject read GetObjectItem;
    property ProducerIdMap: TPCMap read FProducerIdMap;
    property Version: string read FVersion;
    property XmlDocument: LccXmlDocument read FXmlDocument;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AutoAssignEventIDs;
    procedure AutoAssignLogicEvents;
    procedure Clear;
    function PinUpdate(IoPin: Integer; IoPinState: Boolean): TLccBinaryAction;
    function SupportsProduced(var Event: TEventID; var Action: TLccBinaryAction): TSupportsEventType;
    function SupportsConsumed(var Event: TEventID; var Action: TLccBinaryAction): TSupportsEventType;
    function XMLParse(AFilePath: string): Boolean;
    procedure XMLExport(ExportFilePath: string);
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFNDEF FPC_CONSOLE_APP}
  {$IFDEF FPC}
  //{$I TLccSdnController.lrs}
  {$ENDIF}
  RegisterComponents('LCC',[TLccSdnController]);
  {$ENDIF}
end;

{ TLccLogicAction }

function TLccLogicAction.EqualAction(AnAction: TLccBinaryAction): Boolean;
begin
  Result := EqualEventID(AnAction.EventIDLo, EventIDLoLinked) and EqualEventID(AnAction.EventIDHi, EventIDHiLinked)
end;

{ TLccLogic }

constructor TLccLogic.Create(AnOwnerAction: TLccBinaryAction);
begin
  inherited Create;
  FOwner := AnOwnerAction;
  FActions := TObjectList{$IFNDEF FPC}<TLccLogicAction>{$ENDIF}.Create;
end;

destructor TLccLogic.Destroy;
begin
  {$IFDEF FPC}
  FreeAndNil(FActions);
  {$ELSE}
  FActions.DisposeOf;
  FActions := nil;
  {$ENDIF}
  inherited Destroy;
end;

function TLccLogic.Calculate: Boolean;
var
  i: Integer;
  LogicAction: TLccLogicAction;
  EvaluatedResult: Boolean;
begin
  Result := False;
  EvaluatedResult := True;
  i := 0;
  while EvaluatedResult and (i < Actions.Count) do
  begin
    LogicAction := Action[i];
    EvaluatedResult := LogicAction.LogicTrueState = LogicAction.EventStateLinked;
    Inc(i);
  end;

  if EvaluatedResult and (Owner.EventState <> evs_Valid) then
  begin
    Owner.EventState  := evs_Valid;
    Owner.IsDirty := True;
    Result := True
  end else
  if not EvaluatedResult and (Owner.EventState <> evs_InValid) then
  begin
    Owner.EventState  := evs_InValid;
    Owner.IsDirty := True;
    Result := True
  end
end;

function TLccLogic.GetAction(Index: Integer): TLccLogicAction;
begin
   {$IFDEF FPC}
  Result := Actions[Index] as TLccLogicAction;
  {$ELSE}
  Result := Actions[Index];
  {$ENDIF}
end;

{ TLccBinaryAction }

constructor TLccBinaryAction.Create(AnActionType: TLogicalActionType);
begin
  inherited Create;
  FActionType := AnActionType;
  FLogic := TLccLogic.Create(Self);
end;

destructor TLccBinaryAction.Destroy;
begin
   {$IFDEF FPC}
  FreeAndNil(FLogic);
  {$ELSE}
  FLogic.DisposeOf;
  FLogic := nil;
  {$ENDIF}
  inherited Destroy;
end;

{ TLccActionGroup }

constructor TLccActionGroup.Create;
begin
  inherited;
  FActions := TObjectList{$IFNDEF FPC}<TLccBinaryAction>{$ENDIF}.Create;
end;

destructor TLccActionGroup.Destroy;
begin
  {$IFDEF FPC}
  FreeAndNil(FActions);
  {$ELSE}
  FActions.DisposeOf;
  FActions := nil;
  {$ENDIF}
  inherited Destroy;
end;

function TLccActionGroup.GetAction(Index: Integer): TLccBinaryAction;
begin
  {$IFDEF FPC}
  Result := Actions[Index] as TLccBinaryAction;
  {$ELSE}
  Result := Actions[Index];
  {$ENDIF}
end;

{ TLccObject }

constructor TLccObject.Create;
begin
  inherited;
  FInputActionGroups := TObjectList{$IFNDEF FPC}<TLccActionGroup>{$ENDIF}.Create;
  FOutputActionGroups := TObjectList{$IFNDEF FPC}<TLccActionGroup>{$ENDIF}.Create;
end;

destructor TLccObject.Destroy;
begin
  {$IFDEF FPC}
  FreeAndNil(FInputActionGroups);
  FreeAndNil(FOutputActionGroups);
  {$ELSE}
  FInputActionGroups.DisposeOf;
  FOutputActionGroups.DisposeOf;
  FInputActionGroups := nil;
  FOutputActionGroups := nil;
  {$ENDIF}
  inherited Destroy;
end;

function TLccObject.GetInputActionGroup(Index: Integer): TLccActionGroup;
begin
   {$IFDEF FPC}
   Result := InputActionGroups[Index] as TLccActionGroup;
   {$ELSE}
   Result := InputActionGroups[Index];
  {$ENDIF}
end;

function TLccObject.GetOutputActionGroup(Index: Integer): TLccActionGroup;
begin
  {$IFDEF FPC}
  Result := OutputActionGroups[Index] as TLccActionGroup;
  {$ELSE}
  Result := OutputActionGroups[Index];
  {$ENDIF}
end;

{ TLccSdnController }

procedure TLccSdnController.AppendToPCMap(var PCMap: TPCMap; Event: TEventID);
begin
  SetLength(PCMap, Length(PCMap) + 1);
  PCMap[Length(PCMap)-1] := Event;
end;

procedure TLccSdnController.AutoAssignEventIDs;
var
  ActionGroup: TLccActionGroup;
  Action: TLccBinaryAction;
  LccObject: TLccObject;
  iObject, iActionGroup, iAction, iIoPin, EventOffset: Integer;
begin
  iIoPin := 0;
  EventOffset := 0;
  SetLength(FProducerIdMap, 0);
  SetLength(FConsumerMap, 0);

  for iObject:= 0 to LccObjects.Count - 1 do
  begin
    LccObject := LccObjects[iObject] {$IFDEF FPC}as TLccObject{$ENDIF};

    // SDN expects the auto generated number to sart with inputs then move to outputs
    for iActionGroup := 0 to LccObject.InputActionGroups.Count - 1 do
    begin
      ActionGroup := LccObject.InputActionGroups[iActionGroup] {$IFDEF FPC}as TLccActionGroup{$ENDIF};
      for iAction := 0 to ActionGroup.Actions.Count - 1 do
      begin
        if iIoPin < AvailableIoInputs then
        begin
          Action := ActionGroup.Actions[iAction] {$IFDEF FPC}as TLccBinaryAction{$ENDIF};
          NodeIDToEventID(NodeID, EventOffset, Action.FEventIDLo);
          NodeIDToEventID(NodeID, EventOffset+1, Action.FEventIDHi);
          Action.EventState := evs_Unknown;
          Action.IoPin := iIoPin;
          Action.Producer := True;
          AppendToPCMap(FProducerIdMap, Action.FEventIDLo);
          AppendToPCMap(FProducerIdMap, Action.FEventIDHi);
          Inc(iIoPin);
          Inc(EventOffset, 2);
        end else
          raise exception.Create('Not enough I/O Pins to implement this xml file');
      end;
    end;

    for iActionGroup := 0 to LccObject.OutputActionGroups.Count - 1 do
    begin
      ActionGroup := LccObject.OutputActionGroups[iActionGroup] {$IFDEF FPC}as TLccActionGroup{$ENDIF};
      for iAction := 0 to ActionGroup.Actions.Count - 1 do
      begin
        if iIoPin < AvailableIoInputs + AvailableIoOutput then
        begin
          Action := ActionGroup.Actions[iAction] {$IFDEF FPC}as TLccBinaryAction{$ENDIF};
          NodeIDToEventID(NodeID, EventOffset, Action.FEventIDLo);
          NodeIDToEventID(NodeID, EventOffset+1, Action.FEventIDHi);
          Action.EventState := evs_Unknown;
          Action.IoPin := iIoPin;
          Action.Consumer := True;
          AppendToPCMap(FConsumerMap, Action.FEventIDLo);
          AppendToPCMap(FConsumerMap, Action.FEventIDHi);
          Inc(iIoPin);
          Inc(EventOffset, 2);
        end else
          raise exception.Create('Not enough I/O Pins to implement this xml file');
      end;
    end;
  end;
end;

procedure TLccSdnController.AutoAssignLogicEvents;
var
  iAction, iLogic: Integer;
  LogicAction: TLccLogicAction;
  ActionLink: TLccBinaryAction;
begin
  for iAction := 0 to Actions.Count - 1 do
    for iLogic := 0 to ActionItem[iAction].Logic.Actions.Count - 1 do
    begin
      LogicAction := ActionItem[iAction].Logic.Action[iLogic];
      ActionLink := FindActionByName(LogicAction.LinkedName);
      if Assigned(ActionLink) then
      begin
        LogicAction.FEventIDLoLinked := ActionLink.EventIDLo;
        LogicAction.FEventIDHiLinked := ActionLink.EventIDHi;
      end
    end;
   // TODO: Deal with External Assignments
end;

procedure TLccSdnController.Clear;
begin
  XmlFreeDocument(FXmlDocument);
end;

constructor TLccSdnController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExternals := TStringList.Create;
  FLccObjects := TObjectList{$IFNDEF FPC}<TLccObject>{$ENDIF}.Create;
  FActions := TObjectList{$IFNDEF FPC}<TLccBinaryAction>{$ENDIF}.Create;
  Actions.OwnsObjects := False;
end;

destructor TLccSdnController.Destroy;
begin
  FreeAndNil(FExternals);
  {$IFDEF FPC}
  FreeAndNil(FLccObjects);
  FreeAndNil(FActions);
  {$ELSE}
  LccObjects.DisposeOf;
  LccObjects := nil;
  Actions.DisposeOf;
  Actions := nil;
  {$ENDIF}
  inherited Destroy;
end;

function TLccSdnController.EventStateToAttribString(EventState: TEventState): string;
begin
  case EventState of
    evs_Valid : Result := 'valid';
    evs_InValid : Result := 'invalid';
    evs_Unknown : Result := 'unknown'
  end;
end;

procedure TLccSdnController.XMLExport(ExportFilePath: string);
var
  XmlDoc: LccXmlDocument;
begin
  XmlDoc := XmlCreateEmptyDocument;
  InternalExport(XmlDoc);
  XmlWriteToFile(ExportFilePath, XmlDoc);
  XmlFreeDocument(XmlDoc);
end;

function TLccSdnController.FindActionByName(ActionName: string): TLccBinaryAction;
var
  iAction: Integer;
begin
  Result := nil;
  for iAction := 0 to Actions.Count - 1 do
  begin
    if ActionItem[iAction].Name = ActionName then
    begin
      Result := ActionItem[iAction];
      Break;
    end;
  end;
end;

function TLccSdnController.FindActionByIoPin(IoPin: Integer): TLccBinaryAction;
var
  iAction: Integer;
begin
  Result := nil;
  for iAction := 0 to Actions.Count - 1 do
  begin
    if ActionItem[iAction].IoPin = IoPin then
    begin
      Result := ActionItem[iAction];
      Break;
    end;
  end;
end;

procedure TLccSdnController.LogicActionsUpdate(AnAction: TLccBinaryAction);
var
  i, j: Integer;
  LocalAction: TLccBinaryAction;
  LogicAction: TLccLogicAction;
begin
  for i := 0 to Actions.Count - 1 do
  begin
    LocalAction := ActionItem[i];
    if LocalAction.Consumer then
    begin
      for j := 0 to LocalAction.Logic.Actions.Count - 1 do
      begin
        LogicAction := LocalAction.Logic.Action[j];
        if LogicAction.EqualAction(AnAction) then
        begin
          if AnAction.EventState <> LocalAction.EventState then
          begin
            LogicAction.EventStateLinked := AnAction.EventState;
            LocalAction.FIsDirty := True;
          end;
        end;
      end;
    end;
  end;
end;

function TLccSdnController.GetActionItem(Index: Integer): TLccBinaryAction;
begin
  {$IFDEF FPC}
  Result := Actions[Index] as TLccBinaryAction;
  {$ELSE}
  Result := Actions[Index];
  {$ENDIF}
end;

function TLccSdnController.GetInputActionCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Actions.Count - 1 do
  begin
    if ActionItem[i].ActionType = lat_Input then
      Inc(Result);
  end;
end;

function TLccSdnController.GetObjectItem(Index: Integer): TLccObject;
begin
  {$IFDEF FPC}
  Result := LccObjects[Index] as TLccObject;
  {$ELSE}
  Result := LccObjects[Index];
  {$ENDIF}
end;

function TLccSdnController.GetOuptputActionCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Actions.Count - 1 do
  begin
    if ActionItem[i].ActionType = lat_Output then
      Inc(Result);
  end;
end;

function TLccSdnController.InInvalidDictionary(Value: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := 0;
  while (i < Length(INVALID_DICTIONARY)) and not Result do
  begin
    Result := Value = INVALID_DICTIONARY[i];
    Inc(i);
  end;
end;

function TLccSdnController.PinUpdate(IoPin: Integer; IoPinState: Boolean): TLccBinaryAction;
var
  Action: TLccBinaryAction;
begin
  Result := FindActionByIoPin(IoPin);
  if Assigned(Result) then
  begin
    if Result.ActionType = lat_Input then
    begin
      case Result.EventState of
        evs_InValid :
          begin
            if IoPinState then
            begin
              Result.FIsDirty := True;
              Result.EventState := evs_Valid;
              LogicActionsUpdate(Result);
            end;
          end;
        evs_Valid :
          begin
            if not IoPinState then
            begin
              Result.FIsDirty := True;
              Result.EventState := evs_InValid;
              LogicActionsUpdate(Result);
            end;
          end;
        evs_Unknown :
          begin
            Result.FIsDirty := True;
            if IoPinState then
              Result.EventState := evs_Valid
            else
              Result.EventState := evs_InValid;
            LogicActionsUpdate(Result);
          end;
      end;
    end
  end;
end;

procedure TLccSdnController.InternalExport(XmlDoc: LccXmlDocument);
var
  RootNode, ChildNode, ExternalsNode, ObjectNode, ActionGroupNode, InputsNode, ActionNode, OutputsNode, LogicNode: LccXmlNode;
  i, iObjects, iActionGroups, iActions, iLogicActions: Integer;
begin
  RootNode := XmlCreateRootNode(XmlDoc, 'sdn', '');

  ChildNode := XmlCreateChildNode(XmlDoc, RootNode, 'ioboard', '');
  XmlAttributeForce(XmlDoc, ChildNode, 'inputs', IntToStr(AvailableIoInputs));
  XmlAttributeForce(XmlDoc, ChildNode, 'outputs', IntToStr(AvailableIoOutput));

  ExternalsNode := XmlCreateChildNode(XmlDoc, RootNode, 'externals', '');
  for i := 0 to Externals.Count - 1 do
    XmlCreateChildNode(XmlDoc, ExternalsNode, 'name', Externals[i]);

  for iObjects := 0 to LccObjects.Count - 1 do
  begin
    ObjectNode := XmlCreateChildNode(XmlDoc, RootNode, 'object', '');
    XmlAttributeForce(XmlDoc, ObjectNode, 'class', (ObjectItem[iObjects].LccClass));
    ChildNode := XmlCreateChildNode(XmlDoc, ObjectNode, 'name', ObjectItem[iObjects].Name);
    ChildNode := XmlCreateChildNode(XmlDoc, ObjectNode, 'description', ObjectItem[iObjects].Description);

    InputsNode := XmlCreateChildNode(XmlDoc, ObjectNode, 'inputs', '');
    for iActionGroups := 0 to ObjectItem[iObjects].InputActionGroups.Count - 1 do
    begin
      ActionGroupNode := XmlCreateChildNode(XmlDoc, InputsNode, 'actiongroup', '');
      XmlAttributeForce(XmlDoc, ActionGroupNode, 'class', (ObjectItem[iObjects].InputActionGroup[iActionGroups].LccClass));
      for iActions := 0 to ObjectItem[iObjects].InputActionGroup[iActionGroups].Actions.Count - 1 do
      begin
        ActionNode := XmlCreateChildNode(XmlDoc, ActionGroupNode, 'action', '');
        XmlAttributeForce(XmlDoc, ActionNode, 'eventidlo', StringReplace( EventIDToString(ObjectItem[iObjects].InputActionGroup[iActionGroups].Action[iActions].EventIDLo, True), NodeIDToString(NodeID, True), '{$NODEID}', [rfReplaceAll, rfIgnoreCase]));
        XmlAttributeForce(XmlDoc, ActionNode, 'eventidhi', StringReplace( EventIDToString(ObjectItem[iObjects].InputActionGroup[iActionGroups].Action[iActions].EventIDHi, True), NodeIDToString(NodeID, True), '{$NODEID}', [rfReplaceAll, rfIgnoreCase]));
        XmlAttributeForce(XmlDoc, ActionNode, 'eventstate', EventStateToAttribString(ObjectItem[iObjects].InputActionGroup[iActionGroups].Action[iActions].EventState));
        XmlAttributeForce(XmlDoc, ActionNode, 'iopin', IntToStr(ObjectItem[iObjects].InputActionGroup[iActionGroups].Action[iActions].IoPin));
        XmlCreateChildNode(XmlDoc, ActionNode, 'name', ObjectItem[iObjects].InputActionGroup[iActionGroups].Action[iActions].Name);
      end;
    end;

    OutputsNode := XmlCreateChildNode(XmlDoc, ObjectNode, 'outputs', '');
    for iActionGroups := 0 to ObjectItem[iObjects].OutputActionGroups.Count - 1 do
    begin
      ActionGroupNode := XmlCreateChildNode(XmlDoc, OutputsNode, 'actiongroup', '');
      XmlAttributeForce(XmlDoc, ActionGroupNode, 'class', (ObjectItem[iObjects].OutputActionGroup[iActionGroups].LccClass));
      for iActions := 0 to ObjectItem[iObjects].OutputActionGroup[iActionGroups].Actions.Count - 1 do
      begin
        ActionNode := XmlCreateChildNode(XmlDoc, ActionGroupNode, 'action', '');
        XmlAttributeForce(XmlDoc, ActionNode, 'eventidlo', StringReplace( EventIDToString(ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].EventIDLo, True), NodeIDToString(NodeID, True), '{$NODEID}', [rfReplaceAll, rfIgnoreCase]));
        XmlAttributeForce(XmlDoc, ActionNode, 'eventidhi', StringReplace( EventIDToString(ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].EventIDHi, True), NodeIDToString(NodeID, True), '{$NODEID}', [rfReplaceAll, rfIgnoreCase]));
        XmlAttributeForce(XmlDoc, ActionNode, 'eventstate', EventStateToAttribString(ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].EventState));
        XmlAttributeForce(XmlDoc, ActionNode, 'iopin', IntToStr(ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].IoPin));
        ChildNode := XmlCreateChildNode(XmlDoc, ActionNode, 'name', ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].Name);
        LogicNode := XmlCreateChildNode(XmlDoc, ActionNode, 'logic', '');
        for iLogicActions := 0 to ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].Logic.Actions.Count - 1 do
        begin
          ChildNode := XmlCreateChildNode(XmlDoc, LogicNode, 'action', '');
          // Don't save the state of the linked event as we don't know what it will be on reboot
          XmlAttributeForce(XmlDoc, ChildNode, 'eventidlo', StringReplace( EventIDToString(ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].Logic.Action[iLogicActions].EventIDLoLinked, True), NodeIDToString(NodeID, True), '{$NODEID}', [rfReplaceAll, rfIgnoreCase]));
          XmlAttributeForce(XmlDoc, ChildNode, 'eventidhi', StringReplace( EventIDToString(ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].Logic.Action[iLogicActions].EventIDHiLinked, True), NodeIDToString(NodeID, True), '{$NODEID}', [rfReplaceAll, rfIgnoreCase]));
          XmlAttributeForce(XmlDoc, ChildNode, 'truestate', ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].Logic.Action[iLogicActions].LogicTrueStateName);
          XmlCreateChildNode(XmlDoc, ChildNode, 'name', ObjectItem[iObjects].OutputActionGroup[iActionGroups].Action[iActions].Logic.Action[iLogicActions].LinkedName);
        end;
      end;
    end;
  end;
end;

procedure TLccSdnController.InternalParse;
var
  RootNode, ObjectChildNode, ExternalsNode, ObjectNode, ActionGroupNode, ActionNode, LogicNode, LogicActionNode, ChildNode: LccXmlNode;
  Attrib: string;
  LccObject: TLccObject;
  LccAction: TLccBinaryAction;
  LccActionGroup: TLccActionGroup;
  LccLogicAction: TLccLogicAction;
begin
  Externals.Clear;
  FAvailableIoInput := 0;
  FAvailableIoOutput := 0;
  LccObjects.Clear;

  RootNode := XmlFindRootNode(XmlDocument, 'sdn');
  if Assigned(RootNode) then
  begin
    FVersion := XmlAttributeRead(RootNode, 'version');

    // Load information about the IO board
    ObjectChildNode := XmlFindChildNode(RootNode, 'ioboard');
    if Assigned(ObjectChildNode) then
    begin
       FAvailableIoInput := StrToInt(XmlAttributeRead(ObjectChildNode, 'inputs'));
       FAvailableIoOutput := StrToInt(XmlAttributeRead(ObjectChildNode, 'outputs'));
    end;

    // Load the External Event Names we need for this SDN
    ExternalsNode := XmlFindChildNode(RootNode, 'externals');
    if Assigned(ExternalsNode) then
    begin
      ObjectChildNode := XmlFirstChild(ExternalsNode);
      while Assigned(ObjectChildNode) do
      begin
        if XmlNodeName(ObjectChildNode) = 'name' then
          Externals.Add(XmlNodeTextContent(ObjectChildNode));
        ObjectChildNode := XmlNextSiblingNode(ObjectChildNode);
      end;
    end;

    // Load the input action information
    ObjectNode := XmlFirstChild(RootNode);
    while Assigned(ObjectNode) do
    begin
      if XmlNodeName(ObjectNode) = 'object' then
      begin
        LccObject := TLccObject.Create;
        LccObject.LccClass := XmlAttributeRead(ObjectNode, 'class');
        LccObjects.Add(LccObject);
        ObjectChildNode := XmlFirstChild(ObjectNode);
        while Assigned(ObjectChildNode) do
        begin
          if XmlNodeName(ObjectChildNode) = 'name' then
            LccObject.Name := XmlNodeTextContent(ObjectChildNode)
          else
          if XmlNodeName(ObjectChildNode) = 'description' then
            LccObject.Description := XmlNodeTextContent(ObjectChildNode)
          else
          if XmlNodeName(ObjectChildNode) = 'inputs' then
          begin
            ActionGroupNode := XmlFirstChild(ObjectChildNode);
            while Assigned(ActionGroupNode) do
            begin
              LccActionGroup := TLccActionGroup.Create;
              LccActionGroup.LccClass := XmlAttributeRead(ActionGroupNode, 'class');
              LccObject.InputActionGroups.Add(LccActionGroup);
              ActionNode := XmlFirstChild(ActionGroupNode);
              while Assigned(ActionNode) do
              begin
                LccAction := TLccBinaryAction.Create(lat_Input);
                LccActionGroup.Actions.Add(LccAction);
                Actions.Add(LccAction);
                LccAction.Producer := True;
                Attrib := XmlAttributeRead(ActionNode, 'eventidlo');
                if Attrib <> '' then
                  LccAction.EventIDLo := StrToEventID(StringReplace(Attrib, '{$NODEID}', NodeIDToString(NodeID, True), [rfReplaceAll, rfIgnoreCase]));
                Attrib := XmlAttributeRead(ActionNode, 'eventidhi');
                if Attrib <> '' then
                  LccAction.EventIDHi := StrToEventID(StringReplace(Attrib, '{$NODEID}', NodeIDToString(NodeID, True), [rfReplaceAll, rfIgnoreCase]));
                Attrib := XmlAttributeRead(ActionNode, 'eventstate');
                LccAction.EventState := AttribStringToEventState(Attrib);
                Attrib := XmlAttributeRead(ActionNode, 'iopin');
                if Attrib <> '' then
                  LccAction.IoPin := StrToInt(Attrib);
                ChildNode := XmlFindChildNode(ActionNode, 'name');
                if Assigned(ChildNode) then
                  LccAction.Name := XmlNodeTextContent(ChildNode);
                ActionNode := XmlNextSiblingNode(ActionNode);
              end;
              ActionGroupNode := XmlNextSiblingNode(ActionGroupNode);
            end;
          end else
          if XmlNodeName(ObjectChildNode) = 'outputs' then
          begin
            ActionGroupNode := XmlFirstChild(ObjectChildNode);
            while Assigned(ActionGroupNode) do
            begin
              LccActionGroup := TLccActionGroup.Create;
              LccActionGroup.LccClass := XmlAttributeRead(ActionGroupNode, 'class');
              LccObject.OutputActionGroups.Add(LccActionGroup);
              ActionNode := XmlFirstChild(ActionGroupNode);
              while Assigned(ActionNode) do
              begin
                LccAction := TLccBinaryAction.Create(lat_Output);
                LccActionGroup.Actions.Add(LccAction);
                Actions.Add(LccAction);
                LccAction.Consumer := True;
                Attrib := XmlAttributeRead(ActionNode, 'eventidlo');
                if Attrib <> '' then
                  LccAction.EventIDLo := StrToEventID(StringReplace(Attrib, '{$NODEID}', NodeIDToString(NodeID, True), [rfReplaceAll, rfIgnoreCase]));
                Attrib := XmlAttributeRead(ActionNode, 'eventidhi');
                if Attrib <> '' then
                  LccAction.EventIDHi := StrToEventID(StringReplace(Attrib, '{$NODEID}', NodeIDToString(NodeID, True), [rfReplaceAll, rfIgnoreCase]));
                Attrib := XmlAttributeRead(ActionNode, 'state');
                LccAction.EventState := AttribStringToEventState(Attrib);
                Attrib := XmlAttributeRead(ActionNode, 'iopin');
                if Attrib <> '' then
                  LccAction.IoPin := StrToInt(Attrib);

                ChildNode := XmlFindChildNode(ActionNode, 'name');
                if Assigned(ChildNode) then
                  LccAction.Name := XmlNodeTextContent(ChildNode);
                LogicNode := XmlFindChildNode(ActionNode, 'logic');
                if Assigned(LogicNode) then
                begin
                  LogicActionNode := XmlFindChildNode(LogicNode, 'action');
                  while Assigned(LogicActionNode) do
                  begin
                    LccLogicAction := TLccLogicAction.Create;
                    LccAction.Logic.Actions.Add(LccLogicAction);
                    ChildNode := XmlFindChildNode(LogicActionNode, 'name');
                    if Assigned(ChildNode) then
                      LccLogicAction.LinkedName := XmlNodeTextContent(ChildNode);
                    // Don't save or restore the state of the linked action, don't know what it will be
                    Attrib := XmlAttributeRead(LogicActionNode, 'eventidlo');
                    if Attrib <> '' then
                      LccLogicAction.EventIDLoLinked := StrToEventID(StringReplace(Attrib, '{$NODEID}', NodeIDToString(NodeID, True), [rfReplaceAll, rfIgnoreCase]));
                    Attrib := XmlAttributeRead(LogicActionNode, 'eventidhi');
                    if Attrib <> '' then
                      LccLogicAction.EventIDHiLinked := StrToEventID(StringReplace(Attrib, '{$NODEID}', NodeIDToString(NodeID, True), [rfReplaceAll, rfIgnoreCase]));
                    Attrib := XmlAttributeRead(LogicActionNode, 'truestate');
                    LccLogicAction.LogicTrueStateName := Attrib;
                    LccLogicAction.LogicTrueState := AttribStringToEventState(Attrib);
                    LogicActionNode := XmlNextSiblingNode(LogicActionNode);
                  end;
                end;
                ActionNode := XmlNextSiblingNode(ActionNode);
              end;
              ActionGroupNode := XmlNextSiblingNode(ActionGroupNode);
            end;
          end;
          ObjectChildNode := XmlNextSiblingNode(ObjectChildNode);
        end;
      end;
      ObjectNode := XmlNextSiblingNode(ObjectNode);
    end;
  end;
end;

function TLccSdnController.InValidDictionary(Value: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := 0;
  while (i < Length(VALID_DICTIONARY)) and not Result do
  begin
    Result := Value = VALID_DICTIONARY[i];
    Inc(i);
  end;
end;

function TLccSdnController.XMLParse(AFilePath: string): Boolean;
begin
  Result := False;
  try
    if FileExists(AFilePath) then
    begin
      FXmlDocument := XmlLoadFromFile(AFilePath);
      if Assigned(XmlDocument) then
        InternalParse;
    end;
  except
    Result := False
  end;
end;

function TLccSdnController.SupportsConsumed(var Event: TEventID; var Action: TLccBinaryAction): TSupportsEventType;
var
  i: Integer;
  LocalAction: TLccBinaryAction;
begin
  Result := set_None;
  Action := nil;
  for i := 0 to Actions.Count - 1 do
  begin
    LocalAction := ActionItem[i];
    if LocalAction.ActionType = lat_Output then
    begin
      if EqualEventID(LocalAction.FEventIDLo, Event) then
      begin
        Action := LocalAction;
        Result := set_LoEventID;
        Exit;
      end;
      if EqualEventID(LocalAction.FEventIDHi, Event) then
      begin
        Action := LocalAction;
        Result := set_HiEventID;
        Exit;
      end;
    end;
  end;
end;

function TLccSdnController.SupportsProduced(var Event: TEventID; var Action: TLccBinaryAction): TSupportsEventType;
var
  i: Integer;
  LocalAction: TLccBinaryAction;
begin
  Result := set_None;
  Action := nil;
  for i := 0 to Actions.Count - 1 do
  begin
    LocalAction := ActionItem[i];
    if LocalAction.ActionType = lat_Input then
    begin
      if EqualEventID(LocalAction.FEventIDLo, Event) then
      begin
        Action := LocalAction;
        Result := set_LoEventID;
        Exit;
      end;
      if EqualEventID(LocalAction.FEventIDHi, Event) then
      begin
        Action := LocalAction;
        Result := set_HiEventID;
        Exit;
      end;
    end;
  end;
end;

function TLccSdnController.AttribStringToEventState(EventState: string): TEventState;
begin
  if InValidDictionary(EventState) then
    Result := evs_Valid
  else
   if InInValidDictionary(EventState) then
    Result := evs_InValid
  else
    Result := evs_Unknown;
end;

end.

