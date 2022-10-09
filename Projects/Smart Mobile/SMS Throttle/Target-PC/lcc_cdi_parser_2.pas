unit lcc_cdi_parser_2;

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
      FMX.Layouts,
      FMX.TabControl,
      FMX.StdCtrls,
      System.Generics.Collections,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
  lcc_xmlutilities;

type

  {$IFDEF FPC}
    TLccCdiContainter = TPanel;
    TLccTabControl = TPageControl;
    TLccSpeedButton = TSpeedButton;
    TLccLabel = TLabel;
  {$ELSE}
    TLccCdiContainer = TLayout;
    TLccTabControl = TTabControl;
    TLccSpeedButton = TSpeedButton;
    TLccLabel = TLabel;
  {$ENDIF}

  TLccCdiParser = class
  private
    FButtonWritePage: TLccSpeedButton;
    FButtonReadWriteStop: TLccSpeedButton;
    FButtonReadPage: TLccSpeedButton;
    FStatusLabel: TLccLabel;
  public
    property ButtonWritePage: TLccSpeedButton read FButtonWritePage write FButtonWritePage;
    property ButtonReadPage: TLccSpeedButton read FButtonReadPage write FButtonReadPage;
    property ButtonReadWriteStop: TLccSpeedButton read FButtonReadWriteStop write FButtonReadWriteStop;

    property StatusLabel: TLccLabel read FStatusLabel write FStatusLabel;


    procedure Render_CDI_Interface({AnLccNode: TLccNode;} CdiContainer: TLccCdiContainer; CDI: LccXmlDocument);
    procedure Clear_CDI_Interface;
  end;

implementation



{ TLccCdiParser }
procedure TLccCdiParser.Render_CDI_Interface(CdiContainer: TLccCdiContainer; CDI: LccXmlDocument);
const
  IDENTIFICATION_INDENT = 16;
var
  CDI_Root,
  Cdi_Child,
  Identification_Root,
  Identification_Child,
  Segment_Root,
  Segment_Child,
  Map_Child,
  Relation_Child: LccXmlNode;
  ScrollBox: TScrollBox;
  MemOffset: DWord;
  ControlOffset: Integer;
  ItemStr: string;

  ButtonContainer: TLccCdiContainer;
begin
  Clear_CDI_Interface;
  ItemStr := '';

  ButtonContainer := TLccCdiContainer.Create(CdiContainer);
  ButtonContainer.Align := TAlignLayout.Bottom;
  ButtonContainer.Height := 88;
  ButtonContainer.Parent := CdiContainer;

  StatusLabel := TLccLabel.Create(ButtonContainer);
  StatusLabel.Align := TAlignLayout.Bottom;
  StatusLabel.Height := 20;
  StatusLabel.Parent := ButtonContainer;
  StatusLabel.Text := 'Status: Building';

  ButtonReadWriteStop := TLccSpeedButton.Create(ButtonContainer);
  ButtonReadWriteStop.Text := 'Stop Page';
  ButtonReadWriteStop.Align := TAlignLayout.Bottom;
 // ButtonReadPage.Height := 44;
 // ButtonReadPage.OnClick := @DoButtonWritePageClick;
  ButtonReadWriteStop.Parent := ButtonContainer;

  ButtonReadPage := TLccSpeedButton.Create(ButtonContainer);
  ButtonReadPage.Text := 'Read Page';
  ButtonReadPage.Align := TAlignLayout.Bottom;
 // ButtonReadPage.Height := 44;
 // ButtonReadPage.OnClick := @DoButtonWritePageClick;
  ButtonReadPage.Parent := ButtonContainer;

  ButtonWritePage := TLccSpeedButton.Create(ButtonContainer);
  ButtonWritePage.Text := 'Write Page';
  ButtonWritePage.Align := TAlignLayout.Bottom;
 // ButtonWritePage.Height := 44;
 // ButtonWritePage.OnClick := @DoButtonWritePageClick;
  ButtonWritePage.Parent := ButtonContainer;

  {
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

  }
end;


procedure TLccCdiParser.Clear_CDI_Interface;
var
  i: Integer;
begin
 { Serializer.Clear;
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
//  if ClearLccNode then
 //   FLccNode := nil;  }

end;

end.
