unit form_logging;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormNetworkLogging }

  TFormNetworkLogging = class(TForm)
    Button1: TButton;
    MemoLogging1: TMemo;
    MemoLogging2: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure MemoLogging1Change(Sender: TObject);
  private

  public

  end;

var
  FormNetworkLogging: TFormNetworkLogging;

implementation

{$R *.lfm}

{ TFormNetworkLogging }

procedure TFormNetworkLogging.MemoLogging1Change(Sender: TObject);
begin

end;

procedure TFormNetworkLogging.Button1Click(Sender: TObject);
begin
  MemoLogging1.Clear;
  MemoLogging2.Clear;
end;

end.

