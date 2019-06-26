unit unitFrameItemEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, virtuallistview;

type

  { TFrameItemEditor }

  TFrameItemEditor = class(TFrame)
    ButtonApply: TButton;
    EditDescription: TEdit;
    EditName: TEdit;
    LabelLayoutActionDescription: TLabel;
    LabelName: TLabel;
    PanelActionInfo: TPanel;
    procedure ButtonApplyClick(Sender: TObject);
    procedure EditDescriptionChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
  private
    FDisplayedItem: TVirtualListviewItem;
    FVirtualListviewActions: TVirtualListview;
    FVirtualListviewResponses: TVirtualListview;
    procedure SetDisplayedItem(AValue: TVirtualListviewItem);

  public
    property DisplayedItem: TVirtualListviewItem read FDisplayedItem write SetDisplayedItem;
    property VirtualListviewActions: TVirtualListview read FVirtualListviewActions write FVirtualListviewActions;
    property VirtualListviewResponses: TVirtualListview read FVirtualListviewResponses write FVirtualListviewResponses;

  end;

implementation

{$R *.lfm}

{ TFrameItemEditor }

procedure TFrameItemEditor.ButtonApplyClick(Sender: TObject);
begin
  if Assigned(DisplayedItem) then
  begin
    DisplayedItem.OwnerListview.BeginUpdate;
    try
      DisplayedItem.Captions[0] := EditName.Text;
      DisplayedItem.Captions[1] := EditDescription.Text;
      ButtonApply.Enabled := False;
    finally
      DisplayedItem.OwnerListview.EndUpdate;
    end;
  end;
end;

procedure TFrameItemEditor.EditDescriptionChange(Sender: TObject);
begin
  if Assigned(DisplayedItem) then
  begin
    ButtonApply.Enabled := True;
  end;
end;

procedure TFrameItemEditor.EditNameChange(Sender: TObject);
begin
  if Assigned(DisplayedItem) then
  begin
    ButtonApply.Enabled := True;
  end;
end;

procedure TFrameItemEditor.SetDisplayedItem(AValue: TVirtualListviewItem);
begin
  if FDisplayedItem = AValue then Exit;
  FDisplayedItem := AValue;

  if Assigned(DisplayedItem) then
  begin
    EditName.Text := DisplayedItem.Captions[0];
    EditDescription.Text := DisplayedItem.Captions[1];
  end;
  ButtonApply.Enabled := False;
end;

end.

