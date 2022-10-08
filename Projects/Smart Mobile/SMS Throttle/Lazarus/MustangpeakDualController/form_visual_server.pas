unit form_visual_server;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  lcc_alias_server, lcc_train_server, lcc_utilities;

type

  { TFormServerInfo }

  TFormServerInfo = class(TForm)
    PanelAliasMappingHeader: TPanel;
    PanelAliasMappingMain: TPanel;
    PanelTrainObjectHeader: TPanel;
    PanelTrainObjectMain: TPanel;
    SplitterMain: TSplitter;
    TreeViewAliasMaps: TTreeView;
    TreeViewTrains: TTreeView;
  private

  public
    procedure AddAliasMap(AMap: TLccAliasMapping);
    procedure RemoveAliasMap(AMap: TLccAliasMapping);

    procedure AddTrainObject(ATrain: TLccTrainObject);
    procedure RemoveTrainObject(ATrain: TLccTrainObject);
  end;

var
  FormServerInfo: TFormServerInfo;

implementation

{$R *.lfm}

{ TFormServerInfo }

procedure TFormServerInfo.AddAliasMap(AMap: TLccAliasMapping);
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

procedure TFormServerInfo.AddTrainObject(ATrain: TLccTrainObject);
var
  TreeNode: TTreeNode;
  CaptionStr: String;
begin
  TreeViewTrains.Items.BeginUpdate;
  try
    CaptionStr := NodeIDToString(ATrain.NodeID, False) + '; ' + NodeAliasToString(ATrain.NodeAlias);

    TreeNode := TreeViewTrains.Items.FindNodeWithText( CaptionStr);
    if not Assigned(TreeNode) then
    begin
      TreeNode := TreeViewTrains.Items.Add(nil, CaptionStr);
     end;
  finally
    TreeViewTrains.Items.EndUpdate;
  end;
end;

procedure TFormServerInfo.RemoveTrainObject(ATrain: TLccTrainObject);
var
  TreeNode: TTreeNode;
  CaptionStr: String;
begin
  TreeViewTrains.Items.BeginUpdate;
  try
    CaptionStr := NodeIDToString(ATrain.NodeID, False) + '; ' + NodeAliasToString(ATrain.NodeAlias);

    TreeNode := TreeViewTrains.Items.FindNodeWithText( CaptionStr);
    if Assigned(TreeNode) then
      TreeViewTrains.Items.Delete(TreeNode);
  finally
    TreeViewTrains.Items.EndUpdate;
  end;
end;

end.

