unit UnitMainUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, StrUtils;

const
  MAX_MFG_CHARS = 64;
  MAX_USER_NAME_CHARS = 20;
  MAX_USER_DESC_CHARS = 40;

type
  TCDIStringInfo = record
    Offset: Word;   // Offset in the Byte Array where the string starts
    Length: Word;   // Length of the CDI String
  end;
  PCDIStringInfo = ^TCDIStringInfo;

const
  MAP_DELIMITER_COUNT = 4;
  MAP_DELIMITER: array[0..MAP_DELIMITER_COUNT-1] of AnsiString = (
    '<manufacturer>',                                                           // LOWERCASE
    '<model>',
    '<hardwareversion>',
    '<softwareversion>'
  );

MAP_DELIMITER_TAIL: array[0..MAP_DELIMITER_COUNT-1] of AnsiString = (
    '</manufacturer>',                                                          // LOWERCASE
    '</model>',
    '</hardwareversion>',
    '</softwareversion>'
  );

type
    TCDIMap = array[0..MAP_DELIMITER_COUNT-1] of TCDIStringInfo;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelMfgInfoVer: TLabel;
    LabelUserInfoVersion: TLabel;
    EditMfg: TEdit;
    EditName: TEdit;
    EditHWVersion: TEdit;
    EditSWVersion: TEdit;
    EditUserDefinedName: TEdit;
    EditUserDefinedDesc: TEdit;
    ButtonGenerateCode: TButton;
    ButtonDecompile: TButton;
    EditMfgInfoVer: TEdit;
    EditUserInfoVer: TEdit;
    GroupBox1: TGroupBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    LabelMfgLen: TLabel;
    LabelNodeNameLen: TLabel;
    LabelHardwareLen: TLabel;
    LabelSoftwareLen: TLabel;
    LabelUserName: TLabel;
    LabelUserDesc: TLabel;
    Label13: TLabel;
    LabelTotalCount: TLabel;
    Label15: TLabel;
    LabelMaxDesc: TLabel;
    LabelUserMaxChar: TLabel;
    LabelMaxMfg: TLabel;
    Label19: TLabel;
    LabelMfgSubTotal: TLabel;
    Label21: TLabel;
    CheckBoxACDI: TCheckBox;
    EditXMLFile: TEdit;
    Button1: TButton;
    Label7: TLabel;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    MemoCDI: TMemo;
    Label8: TLabel;
    Label9: TLabel;
    EditCDIArray: TEdit;
    EditCDIMap: TEdit;
    Label10: TLabel;
    EditMaxCDIArray: TEdit;
    CheckBoxVirtualNodeCDI: TCheckBox;
    Label11: TLabel;
    LabelMaxMfgStrLen: TLabel;
    procedure EditMfgChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure EditHWVersionChange(Sender: TObject);
    procedure EditSWVersionChange(Sender: TObject);
    procedure EditUserDefinedNameChange(Sender: TObject);
    procedure EditUserDefinedDescChange(Sender: TObject);
    procedure ButtonGenerateCodeClick(Sender: TObject);
    procedure ButtonDecompileClick(Sender: TObject);
    procedure EditMfgInfoVerKeyPress(Sender: TObject; var Key: Char);
    procedure EditMfgInfoVerExit(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function ACDILen: Integer;
    function PackString(Str: string; EndComma, IsVersion: Boolean): string;
    function DecompileString(Str: string): string;
    function DecompileVersionString(Str: string): string;
    procedure UpdateUI;
    function IdentifyMapItem(Delimiter, DelimiterTail: AnsiString; TestStr: AnsiString; MapInfo: PCDIStringInfo; ByteTotal: Integer): Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.EditMfgChange(Sender: TObject);
begin
  LabelMfgLen.Caption := IntToStr(Length(EditMfg.Text)+1);
  LabelTotalCount.Caption := IntToStr(ACDILen);
  UpdateUI
end;

procedure TForm1.EditNameChange(Sender: TObject);
begin
  LabelNodeNameLen.Caption := IntToStr(Length(EditName.Text)+1);
  LabelTotalCount.Caption := IntToStr(ACDILen);
  UpdateUI
end;

procedure TForm1.EditHWVersionChange(Sender: TObject);
begin
  LabelHardwareLen.Caption := IntToStr(Length(EditHWVersion.Text)+1);
  LabelTotalCount.Caption := IntToStr(ACDILen);
  UpdateUI
end;

procedure TForm1.EditSWVersionChange(Sender: TObject);
begin
  LabelSoftwareLen.Caption := IntToStr(Length(EditSWVersion.Text)+1);
  LabelTotalCount.Caption := IntToStr(ACDILen);
  UpdateUI
end;

procedure TForm1.EditUserDefinedNameChange(Sender: TObject);
begin
  LabelUserName.Caption := IntToStr(Length(EditUserDefinedName.Text)+1);
  LabelTotalCount.Caption := IntToStr(ACDILen);
  UpdateUI
end;

procedure TForm1.EditUserDefinedDescChange(Sender: TObject);
begin
  LabelUserDesc.Caption := IntToStr(Length(EditUserDefinedDesc.Text));
  LabelTotalCount.Caption := IntToStr(ACDILen);
  UpdateUI
end;

procedure TForm1.ButtonGenerateCodeClick(Sender: TObject);
var
  s1: string;
begin       {
  RichEdit1.Lines.BeginUpdate;
  try
    s1 := '';
    RichEdit1.Lines.Clear;
    s1 := '';
    s1 := 'const';
    RichEdit1.Lines.Add(s1);
    if CheckBoxACDI.Checked then
    begin
      RichEdit1.Lines.Add('  MAX_ACDI_ARRAY_VNODE = ' + IntToStr(ACDILen)+';');
      s1 := '  ACDI_VNODE_STRINGS: array[0..MAX_ACDI_ARRAY_VNODE - 1] of byte = (';
    end else
    begin
       RichEdit1.Lines.Add('  MAX_ACDI_ARRAY = ' + IntToStr(ACDILen)+';');
      s1 := '  ACDI_NODE_STRINGS: array[0..MAX_ACDI_ARRAY - 1] of byte = (';
    end;
    RichEdit1.Lines.Add(s1);
    RichEdit1.Lines.Add(PackString(EditMfgInfoVer.Text, True, True));
    RichEdit1.Lines.Add(PackString(EditMfg.Text, True, False));
    RichEdit1.Lines.Add(PackString(EditName.Text, True, False));
    RichEdit1.Lines.Add(PackString(EditHWVersion.Text, True, False));
    RichEdit1.Lines.Add(PackString(EditSWVersion.Text, True, False));
    RichEdit1.Lines.Add(PackString(EditUserInfoVer.Text, True, True));
    RichEdit1.Lines.Add(PackString(EditUserDefinedName.Text, True, False));
    RichEdit1.Lines.Add(PackString(EditUserDefinedDesc.Text, False, False));
    s1 := '    );';
    RichEdit1.Lines.Add(s1)
  finally
    RichEdit1.Lines.EndUpdate
  end;   }
end;

function TForm1.ACDILen: Integer;
  begin
  Result := Length(EditMfg.Text) + Length(EditName.Text) + Length(EditHWVersion.Text) +
              Length(EditSWVersion.Text) + Length(EditUserDefinedName.Text) + Length(EditUserDefinedDesc.Text) + 8 // For the #0 and Version IDs

end;

function TForm1.PackString(Str: string; EndComma, IsVersion: Boolean): string;
var
  i: Integer;
begin
  Result := '';
  Str := Trim(Str);
  for i := 1 to Length(Str) do
    Result := Result + '$' + IntToHex( Ord(Str[i]), 2) + ',';
  if IsVersion then
  begin
    Result := '      '+Result + '  // Version = ' + Str
  end else
  begin
    if EndComma then
      Result := '      '+Result + '$' + IntToHex( Ord(#0), 2) + ',  // ' + Str
    else
      Result := '      '+Result + '$' + IntToHex( Ord(#0), 2)+'  // ' + Str
  end
end;

procedure TForm1.ButtonDecompileClick(Sender: TObject);
var
  Index: Integer;
begin
 { if Pos('const', LowerCase(RichEdit1.Lines[0])) = 0 then
    Index := 2
  else
    Index := 3;
  EditMfgInfoVer.Text := DecompileVersionString(RichEdit1.Lines[Index]);
  Inc(Index);
  EditMfg.Text := DecompileString(RichEdit1.Lines[Index]);
  Inc(Index);
  EditName.Text := DecompileString(RichEdit1.Lines[Index]);
  Inc(Index);
  EditHWVersion.Text := DecompileString(RichEdit1.Lines[Index]);
  Inc(Index);
  EditSWVersion.Text := DecompileString(RichEdit1.Lines[Index]);
  Inc(Index);
  EditUserInfoVer.Text := DecompileVersionString(RichEdit1.Lines[Index]);
  Inc(Index);
  EditUserDefinedName.Text := DecompileString(RichEdit1.Lines[Index]);
  Inc(Index);
  EditUserDefinedDesc.Text := DecompileString(RichEdit1.Lines[Index]);     }
end;

function TForm1.DecompileVersionString(Str: string): string;
var
  i: Integer;
  Temp: string;
begin
  Str := Trim(Str);
  Result := '';
  Temp := '';
  i := 1;
  if Str <> '' then
  begin
    while i < Length(Str) do
    begin
      if Str[i] = ',' then
      begin
        Result := Result + Char( StrToInt(Temp));
        Temp := '';
      end else
      begin
        if (Str[i] >= '0') and (Str[i] <= '9') then
          Temp := Temp + Str[i];
      end;
      Inc(i)
    end;
  end
end;

function TForm1.DecompileString(Str: string): string;
var
  i: Integer;
  Temp: string;
begin
  Str := Trim(Str);
  Result := '';
  Temp := '';
  i := 1;
  if Str <> '' then
  begin
    while (Str[i] <> #0) and (i < Length(Str)) do
    begin
      if Str[i] = ',' then
      begin
        Result := Result + Char( StrToInt(Temp));
        Temp := '';
      end else
      begin
        if (Str[i] >= '0') and (Str[i] <= '9') then
          Temp := Temp + Str[i];
      end;
      Inc(i)
    end;
  end
end;

procedure TForm1.EditMfgInfoVerKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key < '0') and (Key > '9') then
  begin
    Beep;
    Key := #0;
  end;
end;

procedure TForm1.EditMfgInfoVerExit(Sender: TObject);
begin
  try
    if StrToInt(EditMfgInfoVer.Text) > 255 then
      EditMfgInfoVer.SelectAll;
  except
  end;
end;

procedure TForm1.UpdateUI;
begin
  LabelMfgSubTotal.Caption := IntToStr(Length(EditMfg.Text)+1+Length(EditName.Text)+1+Length(EditHWVersion.Text)+1+Length(EditSWVersion.Text)+1);
  if (StrToInt(LabelMfgSubTotal.Caption) > MAX_MFG_CHARS) then
  begin
    LabelMaxMfg.Font.Color := clRed;
    LabelMaxMfg.Font.Style := [fsBold];
  end else
  begin
    LabelMaxMfg.Font.Color := clWindowText;
    LabelMaxMfg.Font.Style := []
  end;

  if (StrToInt(LabelUserName.Caption) > MAX_USER_NAME_CHARS) then
  begin
    LabelUserMaxChar.Font.Color := clRed;
    LabelUserMaxChar.Font.Style := [fsBold];
  end else
  begin
    LabelUserMaxChar.Font.Color := clWindowText;
    LabelUserMaxChar.Font.Style := []
  end;

  if (StrToInt(LabelUserDesc.Caption) > MAX_USER_DESC_CHARS) then
  begin
    LabelMaxDesc.Font.Color := clRed;
    LabelMaxDesc.Font.Style := [fsBold];
  end else
  begin
    LabelMaxDesc.Font.Color := clWindowText;
    LabelMaxDesc.Font.Style := []
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    EditXMLFile.Text := OpenDialog1.FileName;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  XML: TFileStream;
  i, j, AsciiArrayCount, Offset, Index, ExtraLines: Integer;
  Strings, ASCII: TStringList;
  ASCII_Line: AnsiString;
  Map: TCDIMap;
  ArrayIdentifier, MaxArrayIdentifier, MapIdentifier: AnsiString;
begin
  MemoCDI.Lines.BeginUpdate;
  try
    MemoCDI.Clear;
    XML := TFileStream.Create(EditXMLFile.Text, fmOpenRead or fmShareDenyWrite);
    try
      // Read in the XML file from disk
      SetLength(ASCII_Line, XML.Size);
      XML.ReadBuffer(ASCII_Line[1], XML.Size);
      ASCII_Line := Trim(ASCII_Line);
      Strings := TStringList.Create;
      ASCII := TStringList.Create;
      try
        AsciiArrayCount := 0;
        Strings.Text := ASCII_Line;
        ExtraLines := -1;

        // Strip off any white space, (space, tabs, etc), extra lines, garbage after </cdi>, etc
        for i := 0 to Strings.Count - 1 do
        begin
          Strings[i] := Trim(Strings[i]);
          Index := Pos('</cdi>', LowerCase(Strings[i]));
          if Index > 0 then
          begin
            Inc(Index, 5);
            ASCII_Line := Strings[i];
            SetLength(ASCII_Line, Index);
            Strings[i] := ASCII_Line;
            ExtraLines := Strings.Count - 1 - i;
            Strings[i] := Strings[i] + #0;
          end;
        end;
        for i := 0 to ExtraLines - 1 do
          Strings.Delete(Strings.Count-1);


        // Create the ASCII array for the XML characters
        for i := 0 to Strings.Count - 1 do
        begin
          if Length(Strings[i]) > 0 then
          begin
            ASCII_Line := '';
            for j := 1 to Length(Strings[i]) do
            begin
              ASCII_Line := ASCII_Line + '$' + IntToHex( Ord(Strings[i][j]), 2) + ', ';
              Inc(AsciiArrayCount)
            end;
            // Strip off the ',' on the last line
            if i = Strings.Count - 1 then
              SetLength(ASCII_Line, Length(ASCII_Line) - 2);

            ASCII_Line := '    ' + ASCII_Line + '   // ' + Strings[i];
            ASCII.Add(ASCII_Line);
          end
        end;

        // Build the mPascal Code
        if CheckBoxVirtualNodeCDI.Checked then
        begin
          ArrayIdentifier := EditCDIArray.Text + '_VNODE';
          MaxArrayIdentifier := EditMaxCDIArray.Text + '_VNODE';
          MapIdentifier := EditCDIMap.Text + '_VNODE';
        end else
        begin
          ArrayIdentifier := EditCDIArray.Text;
          MaxArrayIdentifier := EditMaxCDIArray.Text;
          MapIdentifier := EditCDIMap.Text;
        end;

        ASCII.Insert(0, 'const');
        ASCII.Insert(1, '  ' + MaxArrayIdentifier + ' = ' + IntToStr(AsciiArrayCount) + ';' );
        ASCII.Insert(2, '  ' + ArrayIdentifier + ': array[0..' + MaxArrayIdentifier + '-1] of byte = (' );

        // Create a lookup Map of the CDI constant data is within the strings (Manufacturer Name, Manufacturer Model, Manufacturer Hardware Version, Manufacturer Sofware Version, etc)
        for j := 0 to MAP_DELIMITER_COUNT - 1 do
        begin
          Offset := 0;
          i := 0;
          while (i < Strings.Count) do
          begin
            IdentifyMapItem(MAP_DELIMITER[j], MAP_DELIMITER_TAIL[j], Strings[i], @Map[j], Offset);
            Offset := Offset + Length(Strings[i]);
            Inc(i);
          end;
        end;
        ASCII.Add('  );');
        ASCII.Add('');
        ASCII.Add('type');
        ASCII.Add('  TCDIStringInfo = record');
        ASCII.Add('    Offset: Word;   // Offset in the Byte Array where the string starts');
        ASCII.Add('    Length: Word;   // Length of the CDI String');
        ASCII.Add('  end;');
        ASCII.Add('  PCDIStringInfo = ^TCDIStringInfo;');
        ASCII.Add('');
        ASCII.Add('type');
        ASCII.Add('  TCDIStringMap = record');
        ASCII.Add('    Manufacturer         : TCDIStringInfo;');
        ASCII.Add('    ManufacturerModel    : TCDIStringInfo;');
        ASCII.Add('    ManufacturerHWVersion: TCDIStringInfo;');
        ASCII.Add('    ManufacturerSWVersion: TCDIStringInfo;');
        ASCII.Add('  end;');
        ASCII.Add('');

        ASCII.Add('const');
        ASCII.Add('  ' + MapIdentifier + ': TCDIStringMap = (' );
        ASCII.Add('    (' + IntToStr(Map[0].Offset) + ', ' + IntToStr(Map[0].Length) + '),    // Manufacturer - [String Offset, Length] into the CDI Memory Space ($FF)');
        ASCII.Add('    (' + IntToStr(Map[1].Offset) + ', ' + IntToStr(Map[1].Length) + '),    // ManufacturerModel - [String Offset, Length] into the CDI Memory Space ($FF)');
        ASCII.Add('    (' + IntToStr(Map[2].Offset) + ', ' + IntToStr(Map[2].Length) + '),    // ManufacturerHWVersion - [String Offset, Length] into the CDI Memory Space ($FF)');
        ASCII.Add('    (' + IntToStr(Map[3].Offset) + ', ' + IntToStr(Map[3].Length) + ')     // ManufacturerSWVersion - [String Offset, Length] into the CDI Memory Space ($FF)');
        ASCII.Add('  );');

        for i := 0 to ASCII.Count - 1 do
          MemoCDI.Lines.Add(ASCII[i]);

        i := Map[0].Length + Map[1].Length + Map[2].Length + Map[3].Length;
        LabelMaxMfgStrLen.Caption := IntToStr(i);
        if i > 64 then
        begin
          LabelMaxMfgStrLen.Font.Color := clRed;
          LabelMaxMfgStrLen.Font.Style := [fsBold];
        end else
        begin
          LabelMaxMfgStrLen.Font.Color := clWindowText;
          LabelMaxMfgStrLen.Font.Style := [];
        end;

      finally
        ASCII.Free;
        Strings.Free
      end;
    finally
      XML.Free
    end
  finally
    MemoCDI.Lines.EndUpdate
  end
end;

function TForm1.IdentifyMapItem(Delimiter, DelimiterTail, TestStr: AnsiString; MapInfo: PCDIStringInfo; ByteTotal: Integer): Boolean;
var
  PositionHead, PositionTail, LocalMapStartPos: Integer;
begin
  Result := False;
  TestStr := LowerCase(TestStr);
  PositionHead := Pos(Delimiter, TestStr);
  if PositionHead > 0 then
  begin
    LocalMapStartPos := PositionHead + Length(Delimiter);
    MapInfo.Offset := ByteTotal + LocalMapStartPos - 1;
    PositionTail := PosEx(DelimiterTail, TestStr, LocalMapStartPos);
    if PositionTail > 0 then
    begin
      MapInfo.Length := PositionTail - LocalMapStartPos;
      Result := True
    end
  end
end;

end.
