unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  AFile: TFileStream;
  i: Integer;
  S: string;
begin
  if OpenDialog1.Execute then
  begin
    AFile := TFileStream.Create(OpenDialog1.FileName, fmOpenReadWrite);
    try
      for i := 0 to AFile.Size - 1 do
        S := S + Char( AFile.ReadByte);
      S := StringReplace(S, ';:', ';' + #13 + ':', [rfReplaceAll]);
      AFile.Size := 0;
      AFile.WriteAnsiString(S);
    finally
      AFile.Free;
    end;
  end;
end;

end.

