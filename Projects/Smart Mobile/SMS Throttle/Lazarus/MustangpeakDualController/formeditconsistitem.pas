unit FormEditConsistItem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormConsistEditor }

  TFormConsistEditor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBoxReverse: TCheckBox;
    CheckBoxLinkF0: TCheckBox;
    CheckBoxLinkFn: TCheckBox;
    CheckBoxHidden: TCheckBox;
    LabelTrain: TLabel;
    LabelTrainID: TLabel;
  private

  public

  end;

var
  FormConsistEditor: TFormConsistEditor;

implementation

{$R *.lfm}

end.

