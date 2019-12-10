unit lcc_cdi_parser;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, DOM, XMLRead, LResources, Buttons,
  {$ELSE}
  {$ENDIF}
  Types,
  lcc_nodemanager,
  lcc_message,
  lcc_defines;

const
  CV_BUTTON_WIDTH = 80;


type
  TConfigMemState = (ocs_Current, ocs_Unknown);
  TConfigDataDirection = (cdd_Read, cdd_Write);
  TParserSerializer = (ps_InsertRead, ps_InsertWrite, ps_RemoveRead, ps_RemoveWrite);

  TParserNotificationEvent = procedure(Sender: TObject; Notify: TParserSerializer) of object;

  TLccCdiParser = class;

  // Holds the Property/Value pairs for a Map type Configuration
  { TMapRelation }

  TMapRelation = class
  private
    FProp: string;
    FValue: string;
  public
    constructor Create( AValue, AProperty: string);
    property Value: string read FValue write FValue;
    property Prop: string read FProp write FProp;
  end;

  // Holds TMapRelations for a Map type Configuration
  { TMap }

  TMap = class(TList)
  private
    FList: TList;
    function GetRelation(Index: Integer): TMapRelation;
    procedure SetRelation(Index: Integer; AValue: TMapRelation);
  protected
    property List: TList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Relation: TMapRelation);
    procedure AddRelation( AValue, AProperty: string);
    procedure ClearList;
    function FindMapByValue(AValue: string): TMapRelation;
    function FindMapByProperty(AProperty: string): TMapRelation;
    property Relations[Index: Integer]: TMapRelation read GetRelation write SetRelation;
  end;

  { TConfigData }

  TConfigData = class
  private
    FDataBit: Byte;
    FDataDirection: TConfigDataDirection;
    FDataEvent: TNodeID;
    FDataInteger: Integer;
    FDataString: ansistring;
  public
    property DataDirection: TConfigDataDirection read FDataDirection write FDataDirection;
    property DataString: ansistring read FDataString write FDataString;
    property DataInteger: Integer read FDataInteger write FDataInteger;
    property DataEvent: TNodeID read FDataEvent write FDataEvent;
    property DataBit: Byte read FDataBit write FDataBit;
  end;

  // Contains information that defines what the configuration info for an element
  { TConfigInfo }

  TConfigInfo = class
  private
    FConfigData: TConfigData;
    FMemAddress: DWord;                                                         // Address of the Configurtion memory space (as extracted from the XML file)
    FMemSize: DWord;                                                            // Size of the Configuration Memory space (as extracted from the XML file)
    FDataType: TLccConfigDataType;                                             // Defines the Configuration Memory space type (cdt_String, cdt_Int, cdt_EventID, cdt_Bit)
    FMapList: TMap;                                                             // List of the possible values/user names if the Configuration Memory space is a Map.  This should be used as index of the comboxbox may not be right in all cases
    FOnMemChangeState: TNotifyEvent;                                            // Called when MemState changes
    FMemState: TConfigMemState;                                                 // Tracks the state of the Configruation Memory with the UI for the Element (unknown, saved, unsaved, etc)
    procedure SetMemState(AValue: TConfigMemState);
  public
    constructor Create(MemOffset, MemSize: DWord; ADataType: TLccConfigDataType);
    destructor Destroy; override;
    property ConfigData: TConfigData read FConfigData write FConfigData;
    property MemAddress: DWord read FMemAddress write FMemAddress;
    property MemSize: DWord read FMemSize write FMemSize;
    property MemState: TConfigMemState read FMemState write SetMemState;
    property DataType: TLccConfigDataType read FDataType write FDataType;
    property MapList: TMap read FMapList write FMapList;
    property OnMemChangeState: TNotifyEvent read FOnMemChangeState write FOnMemChangeState;
  end;

  // UI control to edit Integer or bit type Configuration meory
  { TLccSpinEdit }

  TLccSpinEdit = class(TSpinEdit)
  private
    FCompareCVSpeedButton: TSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageIndexRead: Integer;
    FImageIndexStateCurrent: Integer;
    FImageIndexStateUnknown: Integer;
    FImageIndexWrite: Integer;
    FImageList16x16: TImageList;
    FReadCVSpeedButton: TSpeedButton;
    FStateImage: TImage;
    FWriteCVSpeedButton: TSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property ImageIndexStateCurrent: Integer read FImageIndexStateCurrent write FImageIndexStateCurrent;
    property ImageIndexStateUnknown: Integer read FImageIndexStateUnknown write FImageIndexStateUnknown;
    property ImageIndexRead: Integer read FImageIndexRead write FImageIndexRead;
    property ImageIndexWrite: Integer read FImageIndexWrite write FImageIndexWrite;
    property StateImage: TImage read FStateImage write FStateImage;
  end;

  // UI control to edit String type Configuration meory
  { TLccEdit }

  TLccEdit = class(TEdit)
  private
    FCompareCVSpeedButton: TSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageIndexRead: Integer;
    FImageIndexStateCurrent: Integer;
    FImageIndexStateUnknown: Integer;
    FImageIndexWrite: Integer;
    FImageList16x16: TImageList;
    FReadCVSpeedButton: TSpeedButton;
    FStateImage: TImage;
    FWriteCVSpeedButton: TSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property ImageIndexStateCurrent: Integer read FImageIndexStateCurrent write FImageIndexStateCurrent;
    property ImageIndexStateUnknown: Integer read FImageIndexStateUnknown write FImageIndexStateUnknown;
    property ImageIndexRead: Integer read FImageIndexRead write FImageIndexRead;
    property ImageIndexWrite: Integer read FImageIndexWrite write FImageIndexWrite;
    property StateImage: TImage read FStateImage write FStateImage;
  end;

  // UI control to edit Max type Configuration meory
  { TLccComboBox }

  TLccComboBox = class(TComboBox)
  private
    FCompareCVSpeedButton: TSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageIndexRead: Integer;
    FImageIndexStateCurrent: Integer;
    FImageIndexStateUnknown: Integer;
    FImageIndexWrite: Integer;
    FImageList16x16: TImageList;
    FReadCVSpeedButton: TSpeedButton;
    FStateImage: TImage;
    FWriteCVSpeedButton: TSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property ImageIndexStateCurrent: Integer read FImageIndexStateCurrent write FImageIndexStateCurrent;
    property ImageIndexStateUnknown: Integer read FImageIndexStateUnknown write FImageIndexStateUnknown;
    property ImageIndexRead: Integer read FImageIndexRead write FImageIndexRead;
    property ImageIndexWrite: Integer read FImageIndexWrite write FImageIndexWrite;
    property StateImage: TImage read FStateImage write FStateImage;
  end;

  { TLccCdiParserSerializer }

  TLccCdiParserSerializer = class
  private
    FConfigInfo: TConfigInfo;
    FConfigInfoList: TList;
    FOnNotification: TParserNotificationEvent;
    FOwnerParser: TLccCdiParser;
    FWorkerMessage: TLccMessage;
  protected
    procedure DoNotification(Notify: TParserSerializer); virtual;
    function PackCVReads(CV_Start: DWord): Byte;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRead(AConfigInfo: TConfigInfo);    // THIS SHOULD BE ONLY ONE ADD SO THIS STAYS SERIALIZED
    procedure AddWrite(AConfigInfo: TConfigInfo);
    procedure Clear;
    procedure SendNext;
    procedure Remove(AConfigInfo: TConfigInfo);

    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ConfigInfoList: TList read FConfigInfoList write FConfigInfoList;
    property OwnerParser: TLccCdiParser read FOwnerParser write FOwnerParser;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property OnNotification: TParserNotificationEvent read FOnNotification write FOnNotification;
  end;

  { TLccCdiParser }

  TLccCdiParser = class(TLccCdiParserBase)
  private
    FAutoReadOnTabChange: Boolean;
    FButtonReadPage: TButton;
    FButtonStop: TButton;
    FButtonWritePage: TButton;
    FCVBlockRead: Word;
    FImageIndexRead: Integer;
    FImageIndexStateCurrent: Integer;
    FImageIndexStateUnknown: Integer;
    FImageIndexWrite: Integer;
    FImageList16x16: TImageList;
    FMarkedToStop: Boolean;
    FMarkedToStopIsStopping: Boolean;
    FNodeManager: TLccNodeManager;
    FLccNode: TLccNode;
    FOnAfterReadPage: TNotifyEvent;
    FOnAfterWritePage: TNotifyEvent;
    FOnBuildInterfaceComplete: TNotifyEvent;
    FOnClearInterface: TNotifyEvent;
    FPallet: TPanel;
    FPalletButtons: TPanel;
    FPrintMemOffset: Boolean;
    FSerializer: TLccCdiParserSerializer;
    FShowReadBtn: Boolean;
    FShowWriteBtn: Boolean;
    FStatusPanel: TStatusPanel;
    FSuppressNameAndDescription: Boolean;
    FWorkerMessage: TLccMessage;
    function GetCVBlockRead: Word;
    procedure SetCVBlockRead(AValue: Word);
  protected
    function AddTab(PageControl: TPageControl; ACaption: string): TScrollBox;
    procedure AddLabel(ParentControl: TScrollBox; ACaption: string; var ControlOffset: Integer; ControlMargin, Indent: Integer; Bold: Boolean);
    procedure AddSpinEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean; ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
    procedure AddEdit(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean; ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
    procedure AddComboBoxList(ParentControl: TScrollBox; Element: TDOMNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean; ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
    procedure AddPalletButtons(ParentControl: TPanel);
    procedure AddSpeedButtonGlyph(SpeedButton: TSpeedButton; ImageListIndex: Integer);
    procedure DoAfterReadPage(Sender: TObject); virtual;
    procedure DoAfterWritePage(Sender: TObject); virtual;
    procedure DoButtonReadPageClick(Sender: TObject); virtual;
    procedure DoButtonWritePageClick(Sender: TObject); virtual;
    procedure DoButtonStopClick(Sender: TObject); virtual;
    procedure DoSpeedButtonCompareClick(Sender: TObject); virtual;
    procedure DoSpeedButtonReadClick(Sender: TObject); virtual;
    procedure DoSpeedButtonWriteClick(Sender: TObject); virtual;
    procedure DoClearInterface; virtual;
    procedure DoBuildInterfaceComplete; virtual;
    function ExtractElementItem(Element: TDOMNode; Item: string; var ItemStr: string): Boolean;
    function ExtractElementAttribute(Element: TDOMNode; AttributeName: string; var AttributeStr: string): Boolean;
    function FindControlByAddress(Address: DWord): TControl;
    function FindActivePage: TTabSheet;
    function IsMemorySpace(Segment: TDOMNode; MemorySpace: Byte): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnSpinEditChange(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
    procedure OnComboBoxChange(Sender: TObject);
    procedure OnPageControlChange(Sender: TObject);
    procedure OnSerializerNotification(Sender: TObject; Notify: TParserSerializer);
    procedure ProcessElementForUI(ParentControl: TScrollBox; Element: TDOMNode; var MemOffset: DWord; var ControlOffset: Integer; Indent: Integer; DoSuppressNameAndDescription: Boolean; DoPrintMemOffset: Boolean; ShowReadBtn, ShowWriteBtn: Boolean);
    function SpeedButtonToConfigInfo(Sender: TObject): TConfigInfo;
    procedure UpdateMemOffsetJump(Element: TDOMNode; var MemOffset: DWord);
    function UpdateMemOffsetSize(Element: TDOMNode): DWord;

    property Pallet: TPanel read FPallet;
    property PalletButtons: TPanel read FPalletButtons write FPalletButtons;
    property ButtonReadPage: TButton read FButtonReadPage write FButtonReadPage;
    property ButtonWritePage: TButton read FButtonWritePage write FButtonWritePage;
    property ButtonStop: TButton read FButtonStop write FButtonStop;
    property MarkedToStop: Boolean read FMarkedToStop write FMarkedToStop;
    property MarkedToStopIsStopping: Boolean read FMarkedToStopIsStopping write FMarkedToStopIsStopping;
    property Serializer: TLccCdiParserSerializer read FSerializer write FSerializer;
    property StatusPanel: TStatusPanel read FStatusPanel write FStatusPanel;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Build_CDI_Interface(AnLccNode: TLccNode; ParentControl: TPanel; CDI: TXMLDocument): TPageControl;
    procedure Clear_CDI_Interface(ClearLccNode: Boolean);
    procedure DoConfigMemReadReply(ANode: TObject); override;
    procedure DoConfigMemWriteReply(ANode: TObject); override;
    procedure NotifyLccNodeDestroy(LccNode: TObject); override;
    procedure SetNodeManager(ANodeManager: TObject); override;

    property LccNode: TLccNode read FLccNode;
  published
    property AutoReadOnTabChange: Boolean read FAutoReadOnTabChange write FAutoReadOnTabChange;
    property CVBlockRead: Word read GetCVBlockRead write SetCVBlockRead;
    property ImageList16x16: TImageList read FImageList16x16 write FImageList16x16;
    property ImageIndexStateCurrent: Integer read FImageIndexStateCurrent write FImageIndexStateCurrent;
    property ImageIndexStateUnknown: Integer read FImageIndexStateUnknown write FImageIndexStateUnknown;
    property ImageIndexRead: Integer read FImageIndexRead write FImageIndexRead;
    property ImageIndexWrite: Integer read FImageIndexWrite write FImageIndexWrite;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property PrintMemOffset: Boolean read FPrintMemOffset write FPrintMemOffset;
    property ShowReadBtn: Boolean read FShowReadBtn write FShowReadBtn;
    property ShowWriteBtn: Boolean read FShowWriteBtn write FShowWriteBtn;
    property SuppressNameAndDescription: Boolean read FSuppressNameAndDescription write FSuppressNameAndDescription;
    property OnAfterWritePage: TNotifyEvent read FOnAfterWritePage write FOnAfterWritePage;
    property OnAfterReadPage: TNotifyEvent read FOnAfterReadPage write FOnAfterReadPage;
    property OnClearInterface: TNotifyEvent read FOnClearInterface write FOnClearInterface;
    property OnBuildInterfaceComplete: TNotifyEvent read FOnBuildInterfaceComplete write FOnBuildInterfaceComplete;
  end;

procedure Register;

implementation

type
  TLccNodeManagerHack = class(TLccNodeManager)
  end;

procedure Register;
begin
  {$I TLccCdiParser.lrs}
  RegisterComponents('LCC',[TLccCdiParser]);
end;

{ TLccCdiParserSerializer }

procedure TLccCdiParserSerializer.DoNotification(Notify: TParserSerializer);
begin
  if Assigned(OnNotification) then
    OnNotification(Self, Notify);
end;

function TLccCdiParserSerializer.PackCVReads(CV_Start: DWord): Byte;
var
  Next: TConfigInfo;
  CV_Next: DWord;
  i: Integer;
  Found: Boolean;
begin
  Result := 1;
  CV_Next := CV_Start + 1;
  repeat
    Found := False;
    i := 0; // Index 0 is what is being used at the base address
    while (i < ConfigInfoList.Count) and not Found do
    begin
      Next := TConfigInfo( ConfigInfoList[i]);
      if Next.MemAddress = CV_Next then
      begin
        Inc(Result);
        Inc(CV_Next);
        Found := True;
      end else
        Inc(i);
    end;

    if Found then
      ConfigInfoList.Delete(i);            // We don't own these, they belong to the Controls

  until not Found or (Result = OwnerParser.CVBlockRead);
end;

constructor TLccCdiParserSerializer.Create;
begin
  inherited Create;
  FConfigInfo := nil;
  FConfigInfoList := TList.Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TLccCdiParserSerializer.Destroy;
begin
  FreeAndNil(FConfigInfoList);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

procedure TLccCdiParserSerializer.AddRead(AConfigInfo: TConfigInfo);
begin
  if Assigned(AConfigInfo) then
  begin
    AConfigInfo.ConfigData.DataDirection := cdd_Read;
    ConfigInfoList.Add(AConfigInfo);
    DoNotification(ps_InsertRead);
  end;
end;

procedure TLccCdiParserSerializer.AddWrite(AConfigInfo: TConfigInfo);
begin
  if Assigned(AConfigInfo) then
  begin
    AConfigInfo.ConfigData.DataDirection := cdd_Write;
    ConfigInfoList.Add(AConfigInfo);
    DoNotification(ps_InsertWrite);
  end;
end;

procedure TLccCdiParserSerializer.Clear;
var
  i: Integer;
begin
  for i := 0 to ConfigInfoList.Count - 1 do
  begin
    if TConfigInfo(ConfigInfoList[i]).ConfigData.DataDirection = cdd_Read then
      DoNotification(ps_RemoveRead)
    else
      DoNotification(ps_RemoveWrite);
  end;
  ConfigInfoList.Clear;
  ConfigInfo := nil;
end;

procedure TLccCdiParserSerializer.SendNext;
var
  SequencialCVReads: Byte;
begin
  if (ConfigInfoList.Count > 0) and not Assigned(ConfigInfo) and Assigned(OwnerParser.NodeManager.RootNode) then
  begin
    ConfigInfo := TConfigInfo( ConfigInfoList[0]);
    ConfigInfoList.Delete(0);
    if Assigned(OwnerParser) then
    begin
      if ConfigInfo.ConfigData.DataDirection = cdd_Read then
      begin    // Reply will return on OwnerParser.DoConfigMemReadReply(...);
        if (ConfigInfo.MemAddress and $FF000000 = $FF000000) and (OwnerParser.CVBlockRead > 1) then
        begin
           SequencialCVReads := PackCVReads(ConfigInfo.MemAddress);
           OwnerParser.LccNode.ConfigurationMem.Initialize(ConfigInfo.MemAddress, MSI_CONFIG, SequencialCVReads, ConfigInfo.DataType);
           WorkerMessage.LoadConfigMemRead(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, ConfigInfo.MemAddress, SequencialCVReads);
           TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
        end else
        begin
          OwnerParser.LccNode.ConfigurationMem.Initialize(ConfigInfo.MemAddress, MSI_CONFIG, ConfigInfo.MemSize, ConfigInfo.DataType);
          WorkerMessage.LoadConfigMemRead(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, ConfigInfo.MemAddress, ConfigInfo.MemSize);
          TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
        end;
      end else
      if ConfigInfo.ConfigData.DataDirection = cdd_Write then
      begin    // Reply MIGHT return on OwnerParser.DoConfigMemWriteReply(...);  Nodes are NOT required to send this
        case ConfigInfo.DataType of
          cdt_Int :
            begin
              OwnerParser.LccNode.ConfigurationMem.Initialize(ConfigInfo.MemAddress, MSI_CONFIG, ConfigInfo.MemSize, ConfigInfo.DataType);
              WorkerMessage.LoadConfigMemWriteInteger(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, ConfigInfo.MemAddress, ConfigInfo.MemSize, ConfigInfo.ConfigData.DataInteger);
              TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
            end;
          cdt_String :
            begin
              OwnerParser.LccNode.ConfigurationMem.Initialize(ConfigInfo.MemAddress, MSI_CONFIG, ConfigInfo.MemSize, ConfigInfo.DataType);
              WorkerMessage.LoadConfigMemWriteString(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, ConfigInfo.MemAddress, ConfigInfo.ConfigData.DataString);
              TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
            end;
        end;
      end;
    end;
  end;
end;

procedure TLccCdiParserSerializer.Remove(AConfigInfo: TConfigInfo);
begin
  if AConfigInfo = ConfigInfo then
  begin
    ConfigInfo := nil;
    if AConfigInfo.ConfigData.DataDirection = cdd_Read then
      DoNotification(ps_RemoveRead)
    else
      DoNotification(ps_RemoveWrite);
    SendNext;
  end;
end;


{ TMapRelation }

constructor TMapRelation.Create(AValue, AProperty: string);
begin
  inherited Create;
  Value := AValue;
  Prop := AProperty
end;

{ TMap }

function TMap.GetRelation(Index: Integer): TMapRelation;
begin
  Result := TMapRelation( List[Index])
end;


procedure TMap.SetRelation(Index: Integer; AValue: TMapRelation);
begin
  List[Index] := AValue
end;

constructor TMap.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMap.Destroy;
begin
  ClearList;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMap.Add(Relation: TMapRelation);
begin
  List.Add(Relation);
end;

procedure TMap.AddRelation(AValue, AProperty: string);
begin
  List.Add( TMapRelation.Create( AValue, AProperty));
end;

procedure TMap.ClearList;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    Clear;
  end;
end;

function TMap.FindMapByValue(AValue: string): TMapRelation;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  AValue := LowerCase( AValue);
  while not Assigned(Result) and (i < List.Count) do
  begin
    if LowerCase( Relations[i].Value) = AValue then
      Result := Relations[i];
    Inc(i)
  end;
end;

function TMap.FindMapByProperty(AProperty: string): TMapRelation;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  AProperty := LowerCase( AProperty);
  while not Assigned(Result) and (i < List.Count) do
  begin
    if LowerCase( Relations[i].Prop) = AProperty then
      Result := Relations[i];
    Inc(i)
  end;
end;

{ TLccComboBox }

procedure TLccComboBox.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TBitmap;
begin
  case ConfigInfo.MemState of
    ocs_Current :
      begin
        ImageIndex := ImageIndexStateCurrent;
        StateImage.Picture.Clear;
      end;
    ocs_Unknown :
      begin
        ImageIndex := ImageIndexStateUnknown;
        Bitmap := TBitmap.Create;
        Bitmap.Width := ImageList16x16.Width;
        Bitmap.Height := ImageList16x16.Height;
        ImageList16x16.GetBitmap(ImageIndex, Bitmap);
        StateImage.Picture.Graphic := Bitmap;
        Bitmap.Free;
      end;
  end;
end;

constructor TLccComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
  ReadCVSpeedButton := TSpeedButton.Create(Self);
  WriteCVSpeedButton := TSpeedButton.Create(Self);
  CompareCVSpeedButton := TSpeedButton.Create(Self);
  StateImage := TImage.Create(Self);
  FImageList16x16 := nil;
end;

destructor TLccComboBox.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TLccEdit }

procedure TLccEdit.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TBitmap;
begin
  case ConfigInfo.MemState of
    ocs_Current :
      begin
        ImageIndex := ImageIndexStateCurrent;
        StateImage.Picture.Clear;
      end;
    ocs_Unknown :
      begin
        ImageIndex := ImageIndexStateUnknown;
        Bitmap := TBitmap.Create;
        Bitmap.Width := ImageList16x16.Width;
        Bitmap.Height := ImageList16x16.Height;
        ImageList16x16.GetBitmap(ImageIndex, Bitmap);
        StateImage.Picture.Graphic := Bitmap;
        Bitmap.Free;
      end;
  end;
end;

constructor TLccEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
  ReadCVSpeedButton := TSpeedButton.Create(Self);
  WriteCVSpeedButton := TSpeedButton.Create(Self);
  CompareCVSpeedButton := TSpeedButton.Create(Self);
  StateImage := TImage.Create(Self);
  ImageList16x16 := nil;
end;

destructor TLccEdit.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TLccSpinEdit }

procedure TLccSpinEdit.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TBitmap;
begin
  case ConfigInfo.MemState of
    ocs_Current :
      begin
        ImageIndex := ImageIndexStateCurrent;
        StateImage.Picture.Clear;
      end;
    ocs_Unknown :
      begin
        ImageIndex := ImageIndexStateUnknown;
        Bitmap := TBitmap.Create;
        Bitmap.Width := ImageList16x16.Width;
        Bitmap.Height := ImageList16x16.Height;
        ImageList16x16.GetBitmap(ImageIndex, Bitmap);
        StateImage.Picture.Graphic := Bitmap;
        Bitmap.Free;
      end;
  end;
end;

constructor TLccSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
  ReadCVSpeedButton := TSpeedButton.Create(Self);
  WriteCVSpeedButton := TSpeedButton.Create(Self);
  CompareCVSpeedButton := TSpeedButton.Create(Self);
  StateImage := TImage.Create(Self);
  ImageList16x16 := nil;
end;

destructor TLccSpinEdit.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TLccCdiParser }

procedure TLccCdiParser.AddSpeedButtonGlyph(SpeedButton: TSpeedButton; ImageListIndex: Integer);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := ImageList16x16.Width;
  Bitmap.Height := ImageList16x16.Height;
  ImageList16x16.GetBitmap(ImageListIndex, Bitmap);
  SpeedButton.Glyph.Assign(Bitmap);
  Bitmap.Free;
end;

procedure TLccCdiParser.DoAfterReadPage(Sender: TObject);
begin
  if Assigned(OnAfterReadPage) then
    OnAfterReadPage(Self)
end;

procedure TLccCdiParser.DoAfterWritePage(Sender: TObject);
begin
  if Assigned(OnAfterWritePage) then
    OnAfterWritePage(Self)
end;

procedure TLccCdiParser.DoClearInterface;
begin
  if Assigned(OnClearInterface) then
    OnClearInterface(Self);
end;

procedure TLccCdiParser.DoConfigMemReadReply(ANode: TObject);


    procedure LoadConfigMemToControl(TempNode: TLccOwnedNode; Control: TComponent; DataOffset: Integer);
    var
      TempConfigInfo: TConfigInfo;
    begin
      if Assigned(Control) then
      begin
        TempConfigInfo := nil;
        if Control is TLccSpinEdit then
        begin
          if TempNode.ConfigurationMem.DataType = cdt_Int then
          begin
            if TempNode.ConfigurationMem.Address and $FF000000 = $FF000000 then
              TLccSpinEdit(Control).Value := TempNode.ConfigurationMem.DataRaw[DataOffset]
            else
              TLccSpinEdit(Control).Value := TempNode.ConfigurationMem.DataTypeInteger;
            TLccSpinEdit(Control).OnChange(TLccSpinEdit(Control));
            TempConfigInfo := TLccSpinEdit(Control).ConfigInfo;
            TempConfigInfo.MemState := ocs_Current;
            Serializer.Remove(TempConfigInfo);
          end;
        end else
        if Control is TLccEdit then
        begin
          if TempNode.ConfigurationMem.DataType = cdt_String then
          begin
            TLccEdit(Control).Text := TempNode.ConfigurationMem.DataTypeString;
            TLccEdit(Control).OnChange(TLccEdit(Control));
            TempConfigInfo := TLccEdit(Control).ConfigInfo;
            TempConfigInfo.MemState := ocs_Current;
            Serializer.Remove(TempConfigInfo);
          end;
        end else
        if Control is TLccComboBox then
        begin
          if TempNode.ConfigurationMem.DataType = cdt_Int then
          begin
            TLccComboBox(Control).ItemIndex := TempNode.ConfigurationMem.DataTypeInteger;
            TLccComboBox(Control).OnChange(TLccComboBox(Control));
            TempConfigInfo := TLccComboBox(Control).ConfigInfo;
            TempConfigInfo.MemState := ocs_Current;
            Serializer.Remove(TempConfigInfo);
          end;
        end;
      end;
    end;

var
  TempNode: TLccOwnedNode;

  i: Integer;

begin
  Assert(not (ANode is TLccOwnedNode), 'Not a TLccNode');

  TempNode := TLccOwnedNode( ANode);
  if TempNode = LccNode then       // Make sure the passed node is the same one we called on for the config memory
  begin
    if TempNode.ConfigurationMem.Address and $FF000000 = $FF000000 then
    begin
      for i := TempNode.ConfigurationMem.DataCount - 1 downto 0 do             // Do this backward to ensure we don't delete the initial ConfigMem and force a new block to be read
        LoadConfigMemToControl(TempNode, FindControlByAddress(TempNode.ConfigurationMem.Address + i), i)
    end else
      LoadConfigMemToControl(TempNode, FindControlByAddress(TempNode.ConfigurationMem.Address), 0)
  end;
end;

procedure TLccCdiParser.DoConfigMemWriteReply(ANode: TObject);
var
  Control: TControl;
  TempNode: TLccOwnedNode;
  TempConfigInfo: TConfigInfo;
begin
  Assert(not (ANode is TLccOwnedNode), 'Not a TLccNode');

  TempNode := TLccOwnedNode( ANode);
  if TempNode = LccNode then       // Make sure the passed node is the same one we called on for the config memory
  begin
    TempConfigInfo := nil;
    Control := FindControlByAddress(TempNode.ConfigurationMem.Address);
    if Assigned(Control) then
    begin
      if Control is TLccSpinEdit then
      begin
        if TempNode.ConfigurationMem.DataType = cdt_Int then
        begin
          TLccSpinEdit(Control).ConfigInfo.MemState := ocs_Current;
          TempConfigInfo := TLccSpinEdit(Control).ConfigInfo;
        end;
      end else
      if Control is TLccEdit then
      begin
        if TempNode.ConfigurationMem.DataType = cdt_String then
        begin
          TLccEdit(Control).ConfigInfo.MemState := ocs_Current;
          TempConfigInfo := TLccEdit(Control).ConfigInfo;
        end;
      end else
      if Control is TLccComboBox then
      begin
        if TempNode.ConfigurationMem.DataType = cdt_Int then
        begin
          TLccComboBox(Control).ConfigInfo.MemState := ocs_Current;
          TempConfigInfo := TLccComboBox(Control).ConfigInfo;
        end;
      end;
    end;
    Serializer.Remove(TempConfigInfo);
  end;
end;

procedure TLccCdiParser.DoSpeedButtonCompareClick(Sender: TObject);
var
  ConfigInfo: TConfigInfo;
begin
  if Assigned(FLccNode) then
  begin
    ConfigInfo := SpeedButtonToConfigInfo(Sender);
    if Assigned(ConfigInfo) then
    begin
      // Reply will return on DoConfigMemReply(...);
    end;
  end;
end;

procedure TLccCdiParser.DoSpeedButtonReadClick(Sender: TObject);
begin
  if Assigned(FLccNode) and Assigned(NodeManager) then
  begin
    Serializer.AddRead(SpeedButtonToConfigInfo(Sender));
    Serializer.SendNext;
  end;
end;

procedure TLccCdiParser.DoSpeedButtonWriteClick(Sender: TObject);
begin
  if Assigned(FLccNode) and Assigned(NodeManager) then
  begin
    Serializer.AddWrite(SpeedButtonToConfigInfo(Sender));
    Serializer.SendNext;
  end;
end;

procedure TLccCdiParser.DoBuildInterfaceComplete;
begin
  if Assigned(OnBuildInterfaceComplete) then
    OnBuildInterfaceComplete(Self);
end;

procedure TLccCdiParser.DoButtonReadPageClick(Sender: TObject);

  procedure RunSheet(Component: TComponent);
  var
    i: Integer;
  begin
    for i := 0 to Component.ComponentCount - 1 do
      RunSheet(Component.Components[i]);

    if Component is TLccSpinEdit then
      Serializer.AddRead(TLccSpinEdit( Component).ConfigInfo)
    else
    if Component is TLccEdit then
      Serializer.AddRead(TLccEdit( Component).ConfigInfo)
    else
    if Component is TLccComboBox then
      Serializer.AddRead(TLccComboBox( Component).ConfigInfo)
  end;

var
  TabSheet: TTabSheet;
begin
  TabSheet := FindActivePage;
  if Assigned(TabSheet) then
    RunSheet(TabSheet);
  Serializer.SendNext;
end;

procedure TLccCdiParser.DoButtonStopClick(Sender: TObject);
begin
  MarkedToStop := True;
end;

procedure TLccCdiParser.DoButtonWritePageClick(Sender: TObject);
  procedure RunSheet(Component: TComponent);
  var
    i: Integer;
  begin
    for i := 0 to Component.ComponentCount - 1 do
      RunSheet(Component.Components[i]);

    if Component is TLccSpinEdit then
      Serializer.AddWrite(TLccSpinEdit( Component).ConfigInfo)
    else
    if Component is TLccEdit then
      Serializer.AddWrite(TLccEdit( Component).ConfigInfo)
    else
    if Component is TLccComboBox then
      Serializer.AddWrite(TLccComboBox( Component).ConfigInfo)
  end;

var
  TabSheet: TTabSheet;
begin
  TabSheet := FindActivePage;
  if Assigned(TabSheet) then
    RunSheet(TabSheet);
  Serializer.SendNext;
end;

function TLccCdiParser.ExtractElementItem(Element: TDOMNode; Item: string; var ItemStr: string): Boolean;
var
  Node: TDOMNode;
begin
  Result := False;
  ItemStr := '';
  Node := Element.FindNode(Item);
  if Assigned(Node) then
  begin
    Result := True;
    ItemStr := Node.TextContent;
  end;
end;

function TLccCdiParser.ExtractElementAttribute(Element: TDOMNode; AttributeName: string; var AttributeStr: string): Boolean;
var
  Node: TDOMNode;
begin
  Result := False;
  AttributeStr := '';
  if Element.HasAttributes then
  begin
    Node := Element.Attributes.GetNamedItem(AttributeName);
    if Assigned(Node) then
    begin
      Result := True;
      AttributeStr := Node.TextContent;
    end;
  end;
end;

function TLccCdiParser.FindControlByAddress(Address: DWord): TControl;

  function SearchComponents(AComponent: TComponent): TComponent;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to AComponent.ComponentCount - 1 do
    begin
      Result := SearchComponents(AComponent.Components[i]);
      if Assigned(Result) then
        Break
    end;

    if not Assigned(Result) then
    begin
      if AComponent is TLccSpinEdit then
      begin
        if TLccSpinEdit( AComponent).ConfigInfo.MemAddress = Address then
          Result := AComponent
      end else
      if AComponent is TLccEdit then
      begin
        if TLccEdit( AComponent).ConfigInfo.MemAddress = Address then
          Result := AComponent
      end else
      if AComponent is TLccComboBox then
      begin
        if TLccComboBox( AComponent).ConfigInfo.MemAddress = Address then
          Result := AComponent
      end
    end
  end;

begin
  if Assigned(Pallet) then
    Result := SearchComponents(Pallet) as TControl;
end;

function TLccCdiParser.GetCVBlockRead: Word;
begin
  Result := FCVBlockRead;
end;

function TLccCdiParser.FindActivePage: TTabSheet;
var
  i: Integer;
  PageControl: TPageControl;
begin
  if Assigned(Pallet) then
  begin
    for i := 0 to Pallet.ComponentCount - 1 do
    begin
      if Pallet.Components[i] is TPageControl then
      begin
        PageControl := TPageControl( Pallet.Components[i]);
        Result := PageControl.ActivePage;
      end;
    end;
  end;
end;

function TLccCdiParser.IsMemorySpace(Segment: TDOMNode; MemorySpace: Byte): Boolean;
var
  AttributeStr: string;
begin
  Result := False;
  AttributeStr := '';
  if Segment.HasAttributes then
  begin
    if ExtractElementAttribute(Segment, 'space', AttributeStr) then
      Result :=  AttributeStr = IntToStr(MemorySpace);
  end;
end;

procedure TLccCdiParser.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TLccNodeManager then
  begin
     case Operation of
       opInsert : TLccNodeManager(AComponent).CdiParser := Self;
       opRemove : TLccNodeManager(AComponent).CdiParser := nil;
     end;
  end;
end;

procedure TLccCdiParser.UpdateMemOffsetJump(Element: TDOMNode; var MemOffset: DWord);
var
  i: Integer;
begin
  if Element.HasAttributes then
  begin
    for i := 0 to Element.Attributes.Length - 1 do
    begin
      if LowerCase( Element.Attributes.Item[i].NodeName) = 'origin' then
        MemOffset := StrToInt64( Element.Attributes.Item[i].TextContent)
      else
      if LowerCase( Element.Attributes.Item[i].NodeName) = 'offset' then
        MemOffset := MemOffset + StrToInt64( Element.Attributes.Item[i].TextContent);
    end;
  end;
end;

function TLccCdiParser.UpdateMemOffsetSize(Element: TDOMNode): DWord;
var
  OffsetModified: Boolean;
  TempStr: string;
begin
  OffsetModified := False;
  TempStr := '';
  if Element.HasAttributes then
  begin
    if Element.HasAttributes then
    begin
      if ExtractElementAttribute(Element, 'size', TempStr) then
      begin
        Result := StrToInt64( TempStr);
        if Element.NodeName = 'bit' then
        begin
          Result := (Result div 8);
          if Result mod 8 <> 0 then
            Inc(Result);
        end;
        OffsetModified := True;
      end
    end;
  end;
  if not OffsetModified then
  begin
    if LowerCase( Element.NodeName) = 'int' then
      Result := 1
    else
    if LowerCase( Element.NodeName) = 'eventid' then
      Result := 8
    else
  end;
end;

function TLccCdiParser.AddTab(PageControl: TPageControl; ACaption: string): TScrollBox;
var
  Tab: TTabSheet;
begin
  Tab := PageControl.AddTabSheet;
  Tab.Caption := ACaption;
  Result := TScrollBox.Create(Tab);
  Result.Align := alClient;
  Result.BorderSpacing.Around := 8;
  Result.VertScrollBar.Tracking := True;
  Result.HorzScrollBar.Tracking := True;
  Result.Parent := Tab;
end;

procedure TLccCdiParser.AddLabel(ParentControl: TScrollBox; ACaption: string;
  var ControlOffset: Integer; ControlMargin, Indent: Integer; Bold: Boolean);
var
  ALabel: TLabel;
begin
  ALabel := TLabel.Create(ParentControl);
  ALabel.Caption := ACaption;
  ALabel.Top := ControlOffset;
  ALabel.Left := Indent;
  if Bold then
    ALabel.Font.Style := [fsBold];
  ALabel.Parent := ParentControl;
  ControlOffset := ControlOffset + ALabel.Height + ControlMargin;
end;

procedure TLccCdiParser.AddPalletButtons(ParentControl: TPanel);
const
  BUTTON_MARGIN = 4;
  BUTTON_WIDTH = 120;
  BUTTON_HEIGHT = 22;
  STATUS_HEIGHT = 20;
var
  X, Y: Integer;
  StatusBar: TStatusBar;
begin
  PalletButtons := TPanel.Create(ParentControl);
  PalletButtons.Caption := '';
  PalletButtons.Align := alBottom;
  PalletButtons.Height := STATUS_HEIGHT + BUTTON_HEIGHT + BUTTON_HEIGHT + BUTTON_MARGIN * 4;
  PalletButtons.BevelOuter := bvNone;
  PalletButtons.Parent := ParentControl;

  StatusBar := TStatusBar.Create(PalletButtons);
  StatusPanel := StatusBar.Panels.Add;
  StatusPanel.Width := 200;
  StatusBar.Parent := PalletButtons;

  ButtonWritePage := TButton.Create(PalletButtons);
  ButtonWritePage.Width := BUTTON_WIDTH;
  ButtonWritePage.Height := BUTTON_HEIGHT;
  ButtonWritePage.Caption := 'Write Page';
  ButtonWritePage.Anchors := [akRight, akTop];
  X := PalletButtons.Width - BUTTON_MARGIN - BUTTON_WIDTH;
  ButtonWritePage.Left := X;
  Y := BUTTON_MARGIN;
  ButtonWritePage.Top := Y;
  ButtonWritePage.OnClick := @DoButtonWritePageClick;
  ButtonWritePage.Parent := PalletButtons;

  ButtonReadPage := TButton.Create(PalletButtons);
  ButtonReadPage.Width := BUTTON_WIDTH;
  ButtonReadPage.Height := BUTTON_HEIGHT;
  ButtonReadPage.Caption := 'Read Page';
  ButtonReadPage.Anchors := [akRight, akTop];
  ButtonReadPage.Left := X - BUTTON_MARGIN - BUTTON_WIDTH;
  ButtonReadPage.Top := Y;
  ButtonReadPage.OnClick := @DoButtonReadPageClick;
  ButtonReadPage.Parent := PalletButtons;

  ButtonStop := TButton.Create(PalletButtons);
  ButtonStop.Width := BUTTON_WIDTH;
  ButtonStop.Height := BUTTON_HEIGHT;
  ButtonStop.Caption := 'Stop Action';
  ButtonStop.Anchors := [akRight, akTop];
  X := PalletButtons.Width - BUTTON_MARGIN - BUTTON_WIDTH;
  ButtonStop.Left := X;
  Y := BUTTON_MARGIN + BUTTON_MARGIN + BUTTON_HEIGHT;
  ButtonStop.Top := Y;
  ButtonStop.OnClick := @DoButtonStopClick;
  ButtonStop.Enabled := False;
  ButtonStop.Parent := PalletButtons;
end;

procedure TLccCdiParser.AddSpinEdit(ParentControl: TScrollBox;
  Element: TDOMNode; var ControlOffset: Integer; ControlMargin,
  Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean;
  ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
var
  ASpinEdit: TLccSpinEdit;
  TempStr: string;
  ButtonLeft: Integer;
begin
  TempStr := '';

  // Debug Printing
  if DoPrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Create the SpinEdit
  ASpinEdit := TLccSpinEdit.Create(ParentControl);
  ASpinEdit.Width := 120;
  ASpinEdit.MaxValue := MaxInt;

  // Extract special modifiers
  if ExtractElementItem(Element, 'min', TempStr) then
    ASpinEdit.MinValue := StrToInt(TempStr);
  if ExtractElementItem(Element, 'max', TempStr) then
    ASpinEdit.MaxValue := StrToInt(TempStr);
  if ExtractElementItem(Element, 'default', TempStr) then
    ASpinEdit.Value := StrToInt(TempStr);
  ASpinEdit.Text := '';

  // Look for descripive names and descriptions to print
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  Inc(Indent, 8);

  // Create the ConfigInfo Struture
  if ElementType = 'int' then
    ASpinEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
  else
  if ElementType = 'bit' then
  begin
    ASpinEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
    ASpinEdit.MaxValue := 1;
    ASpinEdit.MinValue := 0;
  end;
  ASpinEdit.ConfigInfo.OnMemChangeState := @ASpinEdit.OnDrawImageState;

  // Create the Control Window
  ASpinEdit.StateImage.Left := Indent;
  ASpinEdit.StateImage.Top := ControlOffset;
  ASpinEdit.StateImage.Width := 20; // ImageList16x16.Width;
  ASpinEdit.StateImage.Height := 20; //ImageList16x16.Height;
  ASpinEdit.ImageList16x16 := ImageList16x16;
  ASpinEdit.ImageIndexStateCurrent := ImageIndexStateCurrent;
  ASpinEdit.ImageIndexStateUnknown := ImageIndexStateUnknown;
  ASpinEdit.ImageIndexRead := ImageIndexRead;
  ASpinEdit.ImageIndexWrite := ImageIndexWrite;
  ASpinEdit.OnDrawImageState(ASpinEdit.ConfigInfo);
  ASpinEdit.StateImage.Parent := ParentControl;

  ASpinEdit.Top := ControlOffset;
  ASpinEdit.Left := ASpinEdit.StateImage.Left + ASpinEdit.StateImage.Width + 8;
  ASpinEdit.Parent := ParentControl;
  ASpinEdit.OnChange := @OnSpinEditChange;

  ButtonLeft := ASpinEdit.Left + ASpinEdit.Width + 4;
  if ShowReadBtn then
  begin
    ASpinEdit.ReadCVSpeedButton.Visible := True;
    ASpinEdit.ReadCVSpeedButton.Left := ButtonLeft;
    ASpinEdit.ReadCVSpeedButton.Top := ASpinEdit.Top;
    ASpinEdit.ReadCVSpeedButton.Height := ASpinEdit.Height;
    ASpinEdit.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
    ASpinEdit.ReadCVSpeedButton.Caption := 'Read';
    ASpinEdit.ReadCVSpeedButton.OnClick := @DoSpeedButtonReadClick;
    AddSpeedButtonGlyph(ASpinEdit.ReadCVSpeedButton, ImageIndexRead);
    ASpinEdit.ReadCVSpeedButton.Parent := ParentControl;
    ButtonLeft := ButtonLeft + ASpinEdit.ReadCVSpeedButton.Width;
  end else
   ASpinEdit.ReadCVSpeedButton.Visible := False;

  if ShowWriteBtn then
  begin
    ASpinEdit.WriteCVSpeedButton.Visible := True;
    ASpinEdit.WriteCVSpeedButton.Left := ButtonLeft;
    ASpinEdit.WriteCVSpeedButton.Top := ASpinEdit.Top;
    ASpinEdit.WriteCVSpeedButton.Height := ASpinEdit.Height;
    ASpinEdit.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
    ASpinEdit.WriteCVSpeedButton.Caption := 'Write';
    ASpinEdit.WriteCVSpeedButton.OnClick := @DoSpeedButtonWriteClick;
    AddSpeedButtonGlyph(ASpinEdit.WriteCVSpeedButton, ImageIndexWrite);
    ASpinEdit.WriteCVSpeedButton.Parent := ParentControl;
    ButtonLeft := ButtonLeft + ASpinEdit.WriteCVSpeedButton.Width;
  end else
    ASpinEdit.WriteCVSpeedButton.Visible := False;

{  ASpinEdit.CompareCVSpeedButton.Left := ASpinEdit.WriteCVSpeedButton.Left + ASpinEdit.WriteCVSpeedButton.Width + 4;
  ASpinEdit.CompareCVSpeedButton.Top := ASpinEdit.Top;
  ASpinEdit.CompareCVSpeedButton.Height := ASpinEdit.Height;
  ASpinEdit.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
  ASpinEdit.CompareCVSpeedButton.Caption := 'Compare';
  ASpinEdit.CompareCVSpeedButton.OnClick := @DoSpeedButtonCompareClick;
  ASpinEdit.CompareCVSpeedButton.Parent := ParentControl;    }

  // Update the Control Offsets
  ControlOffset := ControlOffset + ASpinEdit.Height + ControlMargin;
end;

procedure TLccCdiParser.AddEdit(ParentControl: TScrollBox; Element: TDOMNode;
  var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset,
  MemSize: DWord; DoPrintMemOffset: Boolean; ElementType: string; ShowReadBtn,
  ShowWriteBtn: Boolean);
var
  AnEdit: TLccEdit;
  TempStr: string;
  i, ButtonLeft, ButtonWidthTotal: Integer;
  Size: TSize;
begin
  TempStr := '';

  // Debug Printing
  if DoPrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Create the Edit
  AnEdit := TLccEdit.Create(ParentControl);
  AnEdit.Left := Indent + ImageList16x16.Width + 8;    // Need valid to deal with setting the width

  // Look for descripive names and descriptions to print
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);

  // Create the ConfigInfo Struture
  if ElementType = 'eventid' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_EventID)
  else
  if ElementType = 'int' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
  else
  if ElementType = 'string' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_String)
  else
  if ElementType = 'bit' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
  AnEdit.ConfigInfo.OnMemChangeState := @AnEdit.OnDrawImageState;

  // Calculate the Width of the control needed
  for i := 0 to MemSize - 1 do
    TempStr := TempStr + 'Y';
  Size := Application.MainForm.Canvas.TextExtent(TempStr);
  if ElementType = 'eventid' then
    AnEdit.Width := Round( Size.cx * 3.2)
  else
    AnEdit.Width := Round( Size.cx * 1.2);

  ButtonWidthTotal := 0;
  if ShowReadBtn then
    ButtonWidthTotal := ButtonWidthTotal + CV_BUTTON_WIDTH + 4;
  if ShowWriteBtn then
    ButtonWidthTotal := ButtonWidthTotal + CV_BUTTON_WIDTH + 4;

  if AnEdit.Left + AnEdit.Width + ButtonWidthTotal + 32 > ParentControl.Parent.Width then      // The ScrollWindow can be wider than the view
    AnEdit.Width := ParentControl.Parent.Width - ButtonWidthTotal + 32;

  // Create the Control Window
  AnEdit.StateImage.Left := Indent;
  AnEdit.StateImage.Top := ControlOffset;
  AnEdit.StateImage.Width := ImageList16x16.Width;
  AnEdit.StateImage.Height := ImageList16x16.Height;
  AnEdit.ImageList16x16 := ImageList16x16;
  AnEdit.ImageIndexStateCurrent := ImageIndexStateCurrent;
  AnEdit.ImageIndexStateUnknown := ImageIndexStateUnknown;
  AnEdit.ImageIndexRead := ImageIndexRead;
  AnEdit.ImageIndexWrite := ImageIndexWrite;
  AnEdit.OnDrawImageState(AnEdit.ConfigInfo);
 // AnEdit.Anchors := [akLeft, akRight, akTop];
  AnEdit.StateImage.Parent := ParentControl;

  AnEdit.Top := ControlOffset;
  AnEdit.Parent := ParentControl;
  AnEdit.OnChange := @OnEditChange;

  ButtonLeft := AnEdit.Left + AnEdit.Width + 4;
  if ShowReadBtn then
  begin
    AnEdit.ReadCVSpeedButton.Visible := True;
    AnEdit.ReadCVSpeedButton.Left := ButtonLeft;
    AnEdit.ReadCVSpeedButton.Top := AnEdit.Top;
    AnEdit.ReadCVSpeedButton.Height := AnEdit.Height;
    AnEdit.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AnEdit.ReadCVSpeedButton.Caption := 'Read';
    AnEdit.ReadCVSpeedButton.OnClick := @DoSpeedButtonReadClick;
    AddSpeedButtonGlyph(AnEdit.ReadCVSpeedButton, ImageIndexRead);
  //  AnEdit.ReadCVSpeedButton.Anchors := [akRight, akTop];
    AnEdit.ReadCVSpeedButton.Parent := ParentControl;
    ButtonLeft := ButtonLeft + AnEdit.ReadCVSpeedButton.Width + 4;
  end else
    AnEdit.ReadCVSpeedButton.Visible := True;

  if ShowWriteBtn then
  begin
    AnEdit.WriteCVSpeedButton.Visible := True;
    AnEdit.WriteCVSpeedButton.Left := ButtonLeft;
    AnEdit.WriteCVSpeedButton.Top := AnEdit.Top;
    AnEdit.WriteCVSpeedButton.Height := AnEdit.Height;
    AnEdit.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AnEdit.WriteCVSpeedButton.Caption := 'Write';
    AnEdit.WriteCVSpeedButton.OnClick := @DoSpeedButtonWriteClick;
    AddSpeedButtonGlyph(AnEdit.WriteCVSpeedButton, ImageIndexWrite);
   // AnEdit.WriteCVSpeedButton.Anchors := [akRight, akTop];
    AnEdit.WriteCVSpeedButton.Parent := ParentControl;
    ButtonLeft := ButtonLeft + AnEdit.WriteCVSpeedButton.Width + 4;
  end else
    AnEdit.WriteCVSpeedButton.Visible := False;

{  AnEdit.CompareCVSpeedButton.Left := AnEdit.WriteCVSpeedButton.Left + AnEdit.WriteCVSpeedButton.Width + 4;
  AnEdit.CompareCVSpeedButton.Top := AnEdit.Top;
  AnEdit.CompareCVSpeedButton.Height := AnEdit.Height;
  AnEdit.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
  AnEdit.CompareCVSpeedButton.Caption := 'Compare';
  AnEdit.CompareCVSpeedButton.OnClick := @DoSpeedButtonCompareClick;
  AnEdit.CompareCVSpeedButton.Parent := ParentControl; }

  // Update the Control Offsets
  ControlOffset := ControlOffset + AnEdit.Height + ControlMargin;
end;

procedure TLccCdiParser.AddComboBoxList(ParentControl: TScrollBox;
  Element: TDOMNode; var ControlOffset: Integer; ControlMargin,
  Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean;
  ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
var
  AComboBoxList: TLccComboBox;
  TempStr, LongestStr, ValueStr, PropertyStr: string;
  MapNode, ChildNode: TDOMNode;
  DoIndent: Boolean;
  Size: TSize;
  ButtonLeft: Integer;
begin
  TempStr := '';

  // Debug Printing
  if DoPrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Find the map for the element
  MapNode := Element.FindNode('map');

  // A ComboBox is only used for a map element
  if MapNode <> nil then
  begin
    // Create the ComboBox
    AComboBoxList := TLccComboBox.Create(ParentControl);
    AComboBoxList.AutoSize := True;
    AComboBoxList.Style := csDropDownList;

    // Look for descripive names and descriptions to print
    if ExtractElementItem(Element, 'name', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
    if ExtractElementItem(Element, 'description', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
    Inc(Indent, 8);

    // The map can have a name and description too, look for them and print
    DoIndent := False;
    if ExtractElementItem(MapNode, 'name', TempStr) then
    begin
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
      DoIndent := True;
    end;
    if ExtractElementItem(MapNode, 'description', TempStr) then
    begin
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
      DoIndent := True
    end;

    // If there were map descriptions then indent the following deeper than the descriptions
    if DoIndent then
      Inc(Indent, 8);

    // Create the ConfigInfo Struture
    if ElementType = 'eventid' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_EventID)
    else
    if ElementType = 'int' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
    else
    if ElementType = 'string' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_String)
    else
    if ElementType = 'bit' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
    AComboBoxList.ConfigInfo.OnMemChangeState := @AComboBoxList.OnDrawImageState;

    // Run the children of the map looking for its relations
    LongestStr := '';
    ChildNode := MapNode.FirstChild;
    while ChildNode <> nil do
    begin
      if LowerCase( ChildNode.NodeName) = 'relation' then
      begin
        // Found a relation
        PropertyStr := '';
        ValueStr := '';
        // Look for the value
        if ExtractElementItem(ChildNode, 'value', ValueStr) then
        begin
          // Found the value add it to the Listbox
          AComboBoxList.Items.Add(ValueStr);
          // Track the longest string so the control width can be set later
          if Length(ValueStr) > Length(LongestStr) then
            LongestStr := ValueStr;
        end;
        PropertyStr := '';
        // Look for the property
        ExtractElementItem(ChildNode, 'property', PropertyStr);
        // Create a list of relations for later use in the ComboBox
        AComboBoxList.ConfigInfo.MapList.AddRelation(ValueStr, PropertyStr);
      end;
      ChildNode := ChildNode.NextSibling;
    end;

    // Deselect any relation so it is clear it is not a valid value yet (need to read the config memory to select the correct one)
    AComboBoxList.ItemIndex := -1;

    // Calculate the correct size to display all the text
    Size := Application.MainForm.Canvas.TextExtent(LongestStr);
    AComboBoxList.Width := Round( Size.cx + 50);

    // Create the Control Window
    AComboBoxList.StateImage.Left := Indent;
    AComboBoxList.StateImage.Top := ControlOffset;
    AComboBoxList.StateImage.Width := ImageList16x16.Width;
    AComboBoxList.StateImage.Height := ImageList16x16.Height;
    AComboBoxList.ImageList16x16 := ImageList16x16;
    AComboBoxList.ImageIndexStateCurrent := ImageIndexStateCurrent;
    AComboBoxList.ImageIndexStateUnknown := ImageIndexStateUnknown;
    AComboBoxList.ImageIndexRead := ImageIndexRead;
    AComboBoxList.ImageIndexWrite := ImageIndexWrite;
    AComboBoxList.OnDrawImageState(AComboBoxList.ConfigInfo);
    AComboBoxList.StateImage.Parent := ParentControl;

    AComboBoxList.Top := ControlOffset;
    AComboBoxList.Left := AComboBoxList.StateImage.Left + AComboBoxList.StateImage.Width + 8;
    AComboBoxList.Parent := ParentControl;
    AComboBoxList.OnChange := @OnComboBoxChange;

    ButtonLeft := AComboBoxList.Left + AComboBoxList.Width + 4;
    if ShowReadBtn then
    begin
      AComboBoxList.ReadCVSpeedButton.Visible := True;
      AComboBoxList.ReadCVSpeedButton.Left := ButtonLeft;
      AComboBoxList.ReadCVSpeedButton.Top := AComboBoxList.Top;
      AComboBoxList.ReadCVSpeedButton.Height := AComboBoxList.Height;
      AComboBoxList.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
      AComboBoxList.ReadCVSpeedButton.Caption := 'Read';
      AComboBoxList.ReadCVSpeedButton.OnClick := @DoSpeedButtonReadClick;
      AddSpeedButtonGlyph(AComboBoxList.ReadCVSpeedButton, ImageIndexRead);
      AComboBoxList.ReadCVSpeedButton.Parent := ParentControl;
      ButtonLeft := ButtonLeft + AComboBoxList.ReadCVSpeedButton.Width + 4;
    end else
      AComboBoxList.ReadCVSpeedButton.Visible := False;

    if ShowWriteBtn then
    begin
      AComboBoxList.WriteCVSpeedButton.Visible := True;
      AComboBoxList.WriteCVSpeedButton.Left := ButtonLeft;
      AComboBoxList.WriteCVSpeedButton.Top := AComboBoxList.Top;
      AComboBoxList.WriteCVSpeedButton.Height := AComboBoxList.Height;
      AComboBoxList.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
      AComboBoxList.WriteCVSpeedButton.Caption := 'Write';
      AComboBoxList.WriteCVSpeedButton.OnClick := @DoSpeedButtonWriteClick;
      AddSpeedButtonGlyph(AComboBoxList.WriteCVSpeedButton, ImageIndexWrite);
      AComboBoxList.WriteCVSpeedButton.Parent := ParentControl;
      ButtonLeft := ButtonLeft + AComboBoxList.WriteCVSpeedButton.Width + 4;
    end else
      AComboBoxList.WriteCVSpeedButton.Visible := False;

 {   AComboBoxList.CompareCVSpeedButton.Left := AComboBoxList.WriteCVSpeedButton.Left + AComboBoxList.WriteCVSpeedButton.Width + 4;
    AComboBoxList.CompareCVSpeedButton.Top := AComboBoxList.Top;
    AComboBoxList.CompareCVSpeedButton.Height := AComboBoxList.Height;
    AComboBoxList.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AComboBoxList.CompareCVSpeedButton.Caption := 'Compare';
    AComboBoxList.CompareCVSpeedButton.OnClick := @DoSpeedButtonCompareClick;
    AComboBoxList.CompareCVSpeedButton.Parent := ParentControl;       }

    // Update the Control Offsets
    ControlOffset := ControlOffset + AComboBoxList.Height + ControlMargin;
  end;
end;

procedure TLccCdiParser.ProcessElementForUI(ParentControl: TScrollBox;
  Element: TDOMNode; var MemOffset: DWord; var ControlOffset: Integer;
  Indent: Integer; DoSuppressNameAndDescription: Boolean;
  DoPrintMemOffset: Boolean; ShowReadBtn, ShowWriteBtn: Boolean);
var
  Group_Child, Map_Child: TDOMNode;
  TempStr: string;
  ReplicationCount, i: Integer;
  MemSize: DWord;
begin
 if Element <> nil then
 begin
   TempStr := '';

   // Test for a Group segment
   if LowerCase( Element.NodeName) = 'group' then
   begin
     // If it is a group then run into the group
     Inc(Indent, 8);

     // Group may override the Offset
     UpdateMemOffsetJump(Element, MemOffset);

     // Look for descripive names and descriptions to print
     if ExtractElementItem(Element, 'name', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, True);
     if ExtractElementItem(Element, 'description', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent + 4, False);

     // Look for replications
     if ExtractElementAttribute(Element, 'replication', TempStr) then
       ReplicationCount := StrToInt(TempStr)
     else
       ReplicationCount := 1;
     ExtractElementItem(Element, 'repname', TempStr);   // Is only one repeated name allowed?  Appears to be with the XML tool.

     // Run through the replicated group (if there was no replication then this is set to 1)
     for i := 1 to ReplicationCount do
     begin
       // TempStr contains the repeated name so print it with the iteration number
       if TempStr <> '' then
         AddLabel(ParentControl, TempStr + ' ' + IntToStr(i), ControlOffset, 2, Indent + 8, False);

       // Run through each of the children of the group calling this method to process them
       Group_Child := Element.FirstChild;
       while Group_Child <> nil do
       begin
         ProcessElementForUI(ParentControl, Group_Child, MemOffset, ControlOffset, Indent + 16, True, DoPrintMemOffset, ShowReadBtn, ShowWriteBtn);
         Group_Child := Group_Child.NextSibling;
       end;
     end;
   end else
   begin
     // It is not a group
     if (LowerCase(Element.NodeName) = 'name') or (LowerCase(Element.NodeName) = 'description') then
     begin
       // It is a descriptive block so print it
       if not DoSuppressNameAndDescription then
         AddLabel(ParentControl, Element.TextContent, ControlOffset, 2, Indent, False);
     end else
     if LowerCase(Element.NodeName) = 'int' then
     begin
       // It is an Integer which may have a memory modifier as well as a size
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);

       // If it has a map then create a ComboListBox to handle it else use a Spin Edit
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddSpinEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, Element.NodeName, ShowReadBtn, ShowWriteBtn)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, Element.NodeName, ShowReadBtn, ShowWriteBtn);

       // Update the Control Offset
       Inc(MemOffset, MemSize);
     end else
     if LowerCase(Element.NodeName) = 'bit' then
     begin
       // It is an Bit which may have a memory modifier as well as a size
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);

       // Think a bit MUST have a map, not sure what the alternative would look like
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddSpinEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, Element.NodeName, ShowReadBtn, ShowWriteBtn)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, Element.NodeName, ShowReadBtn, ShowWriteBtn);

       // Update the Control Offset
       Inc(MemOffset, MemSize);
     end else
     if (LowerCase(Element.NodeName) = 'string') or (LowerCase(Element.NodeName) = 'eventid') then
     begin
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);
       Map_Child := Element.FindNode('map');
       if Map_Child = nil then
         AddEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, Element.NodeName, ShowReadBtn, ShowWriteBtn)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, Element.NodeName, ShowReadBtn, ShowWriteBtn);

       // Update the Control Offset
       Inc(MemOffset, MemSize);
     end else
   end;
 end;
end;

procedure TLccCdiParser.SetCVBlockRead(AValue: Word);
begin
  if AValue < 1 then
    AValue := 1;
  FCVBlockRead := AValue;
end;

procedure TLccCdiParser.SetNodeManager(ANodeManager: TObject);
begin
  NodeManager := ANodeManager as TLccNodeManager;
end;

function TLccCdiParser.SpeedButtonToConfigInfo(Sender: TObject): TConfigInfo;
begin
  if TSpeedButton(Sender).Owner is TLccSpinEdit then
    Result := TLccSpinEdit( TSpeedButton(Sender).Owner).ConfigInfo
  else
  if TSpeedButton(Sender).Owner is TLccEdit then
    Result := TLccEdit( TSpeedButton(Sender).Owner).ConfigInfo
  else
  if TSpeedButton(Sender).Owner is TLccComboBox then
    Result := TLccComboBox( TSpeedButton(Sender).Owner).ConfigInfo
  else
   Result := nil;
end;

procedure TLccCdiParser.OnSpinEditChange(Sender: TObject);
begin
  (Sender as TLccSpinEdit).ConfigInfo.MemState := ocs_Unknown;
  (Sender as TLccSpinEdit).ConfigInfo.ConfigData.DataInteger := (Sender as TLccSpinEdit).Value;
end;

procedure TLccCdiParser.OnEditChange(Sender: TObject);
begin
  (Sender as TLccEdit).ConfigInfo.MemState := ocs_Unknown;
  (Sender as TLccEdit).ConfigInfo.ConfigData.DataString := (Sender as TLccEdit).Text;
end;

procedure TLccCdiParser.OnComboBoxChange(Sender: TObject);
begin
 (Sender as TLccComboBox).ConfigInfo.MemState := ocs_Unknown;
 (Sender as TLccComboBox).ConfigInfo.ConfigData.DataInteger := (Sender as TLccComboBox).ItemIndex;
end;

procedure TLccCdiParser.OnPageControlChange(Sender: TObject);
begin
  Serializer.Clear;
  if AutoReadOnTabChange then
    DoButtonReadPageClick(Sender);
end;

procedure TLccCdiParser.OnSerializerNotification(Sender: TObject; Notify: TParserSerializer);
begin
  if Assigned(Pallet) then
  begin

    if MarkedToStop and not MarkedToStopIsStopping then
    begin
      MarkedToStopIsStopping := True;
      Serializer.Clear;
    end;

    if Serializer.ConfigInfoList.Count > 0 then
    begin
      ButtonReadPage.Enabled := False;
      ButtonWritePage.Enabled := False;
      ButtonStop.Enabled := True;
    end else
    begin
      ButtonReadPage.Enabled := True;
      ButtonWritePage.Enabled := True;
      ButtonStop.Enabled := False;
      MarkedToStop := False;
      MarkedToStopIsStopping := False;
      if Notify = ps_RemoveRead then
        DoAfterReadPage(Self)
      else
        DoAfterWritePage(Self);
    end;
    StatusPanel.Text := 'Remaining: ' + IntToStr(Serializer.ConfigInfoList.Count);
  end;
end;

constructor TLccCdiParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImageList16x16 := nil;
  FShowReadBtn := True;
  FShowWriteBtn := True;
  FSuppressNameAndDescription := False;
  FPrintMemOffset := False;
  FWorkerMessage := TLccMessage.Create;
  FSerializer := TLccCdiParserSerializer.Create;
  Serializer.OwnerParser := Self;
  CVBlockRead := 1;
end;

destructor TLccCdiParser.Destroy;
begin
  if Assigned(NodeManager) then
    NodeManager.CdiParser := nil;
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FSerializer);
  inherited Destroy;
end;

function TLccCdiParser.Build_CDI_Interface(AnLccNode: TLccNode; ParentControl: TPanel; CDI: TXMLDocument): TPageControl;
const
  IDENTIFICATION_INDENT = 16;
var
  CDI_Root, Cdi_Child, Identification_Root, Identification_Child, Segment_Root, Segment_Child, Map_Child, Relation_Child: TDOMNode;
  ScrollBox: TScrollBox;
  MemOffset: DWord;
  ControlOffset: Integer;
  ItemStr: string;
begin
  Clear_CDI_Interface(False);
  ItemStr := '';
  FLccNode := AnLccNode;
  FPallet := ParentControl;
  Pallet.Caption := 'Building GUI';
  AddPalletButtons(ParentControl);
  Serializer.OnNotification := @OnSerializerNotification;
  Result := TPageControl.Create(ParentControl);
  Result.Align := alClient;
  Result.Parent := ParentControl;
  ErrorCode := 0;
  CDI_Root := CDI.FindNode('cdi');
  if Assigned(CDI_Root) then
  begin

    // Handle the Identification block
    Identification_Root := CDI_Root.FindNode('identification');
    if Assigned(Identification_Root) then
    begin
      ControlOffset := 0;

      // Add a tab to place the Identification information on
      ScrollBox := AddTab(Result, 'Identification');

      // Space on the Top
      AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);

      // Handle the manufacturer
      AddLabel(ScrollBox, 'Manufacturer: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('manufacturer');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ' ', ControlOffset, 2, 0, False);

      // Handle the model number
      AddLabel(ScrollBox, 'Model: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('model');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      // Handle the Hardware Version
      AddLabel(ScrollBox, 'Hardware Version: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('hardwareVersion');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      // Handle the Software Version
      AddLabel(ScrollBox, 'Software Version: ', ControlOffset, 2, IDENTIFICATION_INDENT, True);
      Identification_Child := Identification_Root.FindNode('softwareVersion');
      if Assigned(Identification_Child) then
        AddLabel(ScrollBox, ItemStr + Identification_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 4, False)
      else
        AddLabel(ScrollBox, ItemStr, ControlOffset, 2, 0, False);

      // Handle any map blocks that contain descriptive information
      Inc(ControlOffset, 8);
      Identification_Child := Identification_Root.FirstChild;
      while Assigned(Identification_Child) do
      begin
        if LowerCase( Identification_Child.NodeName) = 'map' then
        begin
          Map_Child := Identification_Child.FirstChild;
          while Assigned(Map_Child) do
          begin
            if LowerCase( Map_Child.NodeName) = 'relation' then
            begin
              Relation_Child := Map_Child.FirstChild;
              while Assigned(Relation_Child) do
              begin
                if (LowerCase( Relation_Child.NodeName) = 'value') then
                  AddLabel(ScrollBox, Relation_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 16, False)
                else
                if (LowerCase( Relation_Child.NodeName) = 'property') then
                   AddLabel(ScrollBox, Relation_Child.TextContent, ControlOffset, 2, IDENTIFICATION_INDENT + 8, False);
                Relation_Child := Relation_Child.NextSibling;
              end;
              Map_Child := Map_Child.NextSibling;
            end
          end;
        end;
        Identification_Child := Identification_Child.NextSibling;
      end;
      AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);    // Space on the Bottom
      ScrollBox.AutoScroll := True;
    end;

    // Handled the Segment blocks
    ControlOffset := 0;
    Cdi_Child := CDI_Root.FirstChild;
    while Assigned(Cdi_Child) do
    begin
      if LowerCase(Cdi_Child.NodeName) = 'segment' then
      begin
        Segment_Root := Cdi_Child;
        // First Find the Config Memory Segment
        if IsMemorySpace(Segment_Root, 253) then
        begin
          ControlOffset := 0;
          MemOffset := 0;
          AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);    // Space on the Top

          // Add a new Tabsheet for this Segment using it Name Element as the tab title
          if ExtractElementItem(Segment_Root, 'name', ItemStr) then
            ScrollBox := AddTab(Result, ItemStr)
          else
            ScrollBox := AddTab(Result, '[Unnamed]');

          // Select it to create the window so the size of the Scrollbox is correct
          // Set it back to a simple tab so it builds faster
          Result.ActivePageIndex := Result.PageCount - 1;
          Result.ActivePageIndex := 0;

          // Add the description of this segment as the first line of the Tab Page
          if ExtractElementItem(Segment_Root, 'description', ItemStr) then
            AddLabel(ScrollBox, ItemStr, ControlOffset, 4, 4, False);

          // Time to build the UI for this segment
          UpdateMemOffsetJump(Segment_Root, MemOffset);      // Segment may override the Offset

          // Run all children of the Segment
          Segment_Child := Segment_Root.FirstChild;
          while Segment_Child <> nil do
          begin
            if (LowerCase(Segment_Child.NodeName) <> 'name') and (LowerCase(Segment_Child.NodeName) <> 'description') then
              ProcessElementForUI(ScrollBox, Segment_Child, MemOffset, ControlOffset, 4, SuppressNameAndDescription, PrintMemOffset, ShowReadBtn, ShowWriteBtn);
            Segment_Child := Segment_Child.NextSibling;
          end;

          // Space on the bottom
          AddLabel(ScrollBox, ' ', ControlOffset, 4, 0, False);
        end;
      end;
      Cdi_Child := Cdi_Child.NextSibling;
    end;

    // Allow the controls to be built so Change event are not fired the first time a tab is selected
    Result.ActivePageIndex := Result.PageCount - 1;
    Result.ActivePageIndex := 0;


    // If we anchor it earlier then any control that make the client widow wider "drags" the edits boxes to the right.
          for ControlOffset := 0 to ScrollBox.ControlCount - 1 do
          begin
            if ScrollBox.Controls[ControlOffset] is TLccEdit then
            begin
            //  (ScrollBox.Controls[ControlOffset] as TLccEdit).Visible := False;
            //  (ScrollBox.Controls[ControlOffset] as TLccEdit).Anchors := [akRight, akLeft, akTop];
           //   (ScrollBox.Controls[ControlOffset] as TLccEdit).Visible := True;
         //     (ScrollBox.Controls[ControlOffset] as TLccEdit).Width := 50;
            end;
          end;
  end else
    ErrorCode := 1;   // No CDI Element
  ParentControl.Caption := '';
  Result.OnChange := @OnPageControlChange;
  OnPageControlChange(Result);
  DoBuildInterfaceComplete;
end;

procedure TLccCdiParser.Clear_CDI_Interface(ClearLccNode: Boolean);
var
  i: Integer;
begin
  Serializer.Clear;
  DoClearInterface;
  if Assigned(Pallet) then
  begin
    for i := Pallet.ControlCount - 1 downto 0 do
      Pallet.Controls[i].Free;
  end;
  StatusPanel := nil;;
  ButtonStop := nil;
  ButtonWritePage := nil;
  ButtonReadPage := nil;
  FPallet := nil;
  if ClearLccNode then
    FLccNode := nil;
end;

procedure TLccCdiParser.NotifyLccNodeDestroy(LccNode: TObject);
begin
  if LccNode = FLccNode then
    Clear_CDI_Interface(True);
end;


{ TConfigInfo }

procedure TConfigInfo.SetMemState(AValue: TConfigMemState);
begin
  if AValue <> FMemState then
  begin
    FMemState:=AValue;
    if Assigned(OnMemChangeState) then
      OnMemChangeState(Self);
  end;
end;

constructor TConfigInfo.Create(MemOffset, MemSize: DWord;
  ADataType: TLccConfigDataType);
begin
  inherited Create;
  FMemState := ocs_Unknown;
  FMemAddress := MemOffset;
  FMemSize := MemSize;
  FDataType := ADataType;
  MapList := TMap.Create;
  FConfigData := TConfigData.Create;
end;

destructor TConfigInfo.Destroy;
begin
   FreeAndNil( FMapList);
   FreeAndNil(FConfigData);
  inherited Destroy;
end;

initialization
  RegisterClass(TLccCdiParser);

finalization

end.



