unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, lcc_cdi_parser, laz2_DOM, laz2_XMLRead;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonOpenCDI: TButton;
    ImageList: TImageList;
    LccCdiParser1: TLccCdiParser;
    OpenDialog: TOpenDialog;
    PanelCDI: TPanel;
    procedure ButtonOpenCDIClick(Sender: TObject);
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

procedure TForm1.ButtonOpenCDIClick(Sender: TObject);
var
  Doc: TXMLDocument;
  FileStream: TFileStream;
begin
  Doc := nil;
  FileStream := nil;
  OpenDialog.Filter := 'CDI Files|*.xml;*.XML';
  if OpenDialog.Execute then
  begin
    try
      LccCdiParser1.Clear_CDI_Interface(True);
      FileStream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
      ReadXMLFile(Doc, FileStream);
      LccCdiParser1.Build_CDI_Interface(nil, PanelCDI, Doc);
    finally
      FreeAndNil(FileStream);
      FreeAndNil(Doc);
    end;
  end;
end;

end.

