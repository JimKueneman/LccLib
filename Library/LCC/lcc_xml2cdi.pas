unit lcc_Xml2Cdi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function TXml2CdiConvert(XmlFilePath: string; PackedCdi: TStringList): Boolean;

implementation

function TXml2CdiConvert(XmlFilePath: string; PackedCdi: TStringList): Boolean;
var
  XmlFile: TStringList;
  ASCII_Line: string;
  i, j, AsciiArrayCount: Integer;
begin
  Result := False;
  if FileExists(XmlFilePath) then
  begin
    XmlFile := TStringList.Create;
    XmlFile.LoadFromFile(XmlFilePath);

    AsciiArrayCount := 0;
    // Create the ASCII array for the XML characters
    for i := 0 to XmlFile.Count - 1 do
    begin
      if Length(XmlFile[i]) > 0 then
      begin
        ASCII_Line := '';
        for j := 1 to Length(XmlFile[i]) do
        begin
          ASCII_Line := ASCII_Line + '$' + IntToHex( Ord(XmlFile[i][j]), 2) + ', ';
          Inc(AsciiArrayCount)
        end;
        // Strip off the ',' on the last line
        if i = XmlFile.Count - 1 then
          SetLength(ASCII_Line, Length(ASCII_Line) - 2);

        ASCII_Line := '    ' + ASCII_Line + '   // ' + XmlFile[i];
        PackedCdi.Add(ASCII_Line);
      end
    end;
  end;
end;

end.

