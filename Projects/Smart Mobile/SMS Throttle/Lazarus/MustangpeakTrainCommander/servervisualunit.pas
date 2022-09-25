unit servervisualunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  lcc_alias_server, lcc_train_server, lcc_utilities;

type

  { TFormServerInfo }

  TFormServerInfo = class(TForm)
    PanelAliasMappingMain: TPanel;
    PanelTrainObjectMain: TPanel;
    PanelAliasMappingHeader: TPanel;
    PanelTrainObjectHeader: TPanel;
    SplitterMain: TSplitter;
    TreeViewAliasMaps: TTreeView;
    TreeViewTrains: TTreeView;
  private

  public
    procedure AddAliasMapp(AMap: TLccAliasMapping);
    procedure RemoveAliasMap(AMap: TLccAliasMapping);
  end;

var
  FormServerInfo: TFormServerInfo;

implementation

{$R *.lfm}

{ TFormServerInfo }

procedure TFormServerInfo.AddAliasMapp(AMap: TLccAliasMapping);
var
  TreeNode: TTreeNode;
  CaptionStr: String;
begin
  TreeViewAliasMaps.Items.BeginUpdate;
  try
    CaptionStr := NodeIDToString(AMap.NodeID, False) + '; ' + NodeAliasToString(AMap.NodeAlias);

    TreeNode := TreeViewAliasMaps.Items.FindNodeWithText( CaptionStr);
    if not Assigned(TreeNode) then
    begin
      TreeNode := TreeViewAliasMaps.Items.Add(nil, CaptionStr);
     end;
  finally
    TreeViewAliasMaps.Items.EndUpdate;
  end;
end;

procedure TFormServerInfo.RemoveAliasMap(AMap: TLccAliasMapping);
var
  TreeNode: TTreeNode;
  CaptionStr: String;
begin
  TreeViewAliasMaps.Items.BeginUpdate;
  try
    CaptionStr := NodeIDToString(AMap.NodeID, False) + '; ' + NodeAliasToString(AMap.NodeAlias);

    TreeNode := TreeViewAliasMaps.Items.FindNodeWithText( CaptionStr);
    if Assigned(TreeNode) then
      TreeViewAliasMaps.Items.Delete(TreeNode);
  finally
    TreeViewAliasMaps.Items.EndUpdate;
  end;
end;

end.

