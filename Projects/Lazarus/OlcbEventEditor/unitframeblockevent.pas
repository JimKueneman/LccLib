unit unitFrameBlockEvent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Spin, virtuallistview;

type

  { TFrameBlockName }

  TFrameBlockName = class(TFrame)
    ButtonCreateBlocks: TButton;
    CheckBoxMultipleBlocks: TCheckBox;
    EditBlockName: TEdit;
    EditBlockDescription: TEdit;
    LabelMultiple: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelName: TLabel;
    Label2: TLabel;
    SpinEditStartBlock: TSpinEdit;
    SpinEditEndBlock: TSpinEdit;
    procedure ButtonCreateBlocksClick(Sender: TObject);
    procedure CheckBoxMultipleBlocksChange(Sender: TObject);
  private
    FVirtualListviewActions: TVirtualListview;
    FVirtualListviewResponses: TVirtualListview;

  public
    property VirtualListviewActions: TVirtualListview read FVirtualListviewActions write FVirtualListviewActions;
    property VirtualListviewResponses: TVirtualListview read FVirtualListviewResponses write FVirtualListviewResponses;
  end;

implementation

{$R *.lfm}

{ TFrameBlockName }

procedure TFrameBlockName.ButtonCreateBlocksClick(Sender: TObject);
var
  i: Integer;
  s: string;
  Item, BlockItem: TVirtualListviewItem;
begin
  VirtualListviewActions.BeginUpdate;
  try
    if CheckBoxMultipleBlocks.Checked then
    begin
      for i := SpinEditStartBlock.Value to SpinEditEndBlock.Value do
      begin
         s := StringReplace(EditBlockName.Caption, '?', IntToStr(i), [rfReplaceAll]);
         Item := VirtualListviewActions.Items.Add(s, -1);
         Item.Captions.Add('Block Enter Action');
         Item.Captions.Add('Results initiated: 0');
         Item := VirtualListviewActions.Items.Add(s);
         Item.Captions.Add('Block Leave Action');
         Item.Captions.Add('Results initiated: 0');
      end;
    end else
    begin
      BlockItem := VirtualListviewActions.Items.Add('New Block', -1);
      BlockItem.Captions.Add('Block Description');
      BlockItem.Expandable := True;
      BlockItem.Expanded := True;
      BlockItem.ImageIndex := 0;
      BlockItem.StateImageIndex := 0;

      Item := BlockItem.ChildItems.Add('New Block: EnterBlock', -1);
      Item.Captions.Add('Block Enter Action');
      Item.Captions.Add('Results initiated: 0');
      Item.ImageIndex := 0;
      Item.StateImageIndex := 0;

      Item := BlockItem.ChildItems.Add('New Block: LeaveBlock', -1);
      Item.Captions.Add('Block Leave Action');
      Item.Captions.Add('Results initiated: 0');
      Item.ImageIndex := 0;
      Item.StateImageIndex := 0;
    end;
  finally
    VirtualListviewActions.EndUpdate;
  end;
end;

procedure TFrameBlockName.CheckBoxMultipleBlocksChange(Sender: TObject);
begin
  SpinEditEndBlock.Enabled := CheckBoxMultipleBlocks.Checked;
  SpinEditStartBlock.Enabled := CheckBoxMultipleBlocks.Checked;
end;

end.

