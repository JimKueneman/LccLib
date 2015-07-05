unit frame_lcc_logging;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, Menus, LCLType, StdCtrls, ActnList, SynEditKeyCmds, SynEdit,
  SynEditMarkupHighAll, LResources, lcc_defines;

type

  { TFrameLccLogging }

  TFrameLccLogging = class(TFrame)
    ActionListMessageLog: TActionList;
    ActionLogClear: TAction;
    ActionLogCopy: TAction;
    ActionLogCut: TAction;
    ActionLogPaste: TAction;
    ActionLogPause: TAction;
    ActionLogSelectAll: TAction;
    CheckBoxDetailedLogging: TCheckBox;
    CheckBoxJMRIFormat: TCheckBox;
    MenuItemClear: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemSeparator0: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    PopupMenuSynEdit: TPopupMenu;
    SpeedButtonClear: TSpeedButton;
    SpeedButtonPause: TSpeedButton;
    SynEdit: TSynEdit;
    procedure ActionLogClearExecute(Sender: TObject);
    procedure ActionLogCopyExecute(Sender: TObject);
    procedure ActionLogCutExecute(Sender: TObject);
    procedure ActionLogPasteExecute(Sender: TObject);
    procedure ActionLogPauseExecute(Sender: TObject);
    procedure ActionLogSelectAllExecute(Sender: TObject);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FPaused: Boolean;
    { private declarations }
  protected
    procedure Loaded; override;
  public
    { public declarations }

    property Paused: Boolean read FPaused write FPaused;
    constructor Create(TheOwner: TComponent); override;
    procedure AddLine(ALine: LccString);
  end;

implementation

{$R *.lfm}

{ TFrameLccLogging }

procedure TFrameLccLogging.ActionLogClearExecute(Sender: TObject);
begin
  SynEdit.ClearAll;
end;

procedure TFrameLccLogging.ActionLogCopyExecute(Sender: TObject);
begin
  SynEdit.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
end;

procedure TFrameLccLogging.ActionLogCutExecute(Sender: TObject);
begin
  SynEdit.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
end;

procedure TFrameLccLogging.ActionLogPasteExecute(Sender: TObject);
begin
  SynEdit.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
end;

procedure TFrameLccLogging.ActionLogPauseExecute(Sender: TObject);
begin
  Paused := not Paused;
end;

procedure TFrameLccLogging.ActionLogSelectAllExecute(Sender: TObject);
begin
  SynEdit.SelectAll;
end;

constructor TFrameLccLogging.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // Warning SysEdit make not be created here at design time!!!
  FPaused := False;
end;

procedure TFrameLccLogging.AddLine(ALine: LccString);
begin
  SynEdit.Lines.BeginUpdate;
  try
    SynEdit.Lines.Add(ALine);
  finally
    SynEdit.Lines.EndUpdate;
  end;
end;

procedure TFrameLccLogging.Loaded;
var
 Markup: TSynEditMarkupHighlightAllCaret;
begin
  inherited Loaded;
 { Markup := SynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret] as TSynEditMarkupHighlightAllCaret;
  Markup.MarkupInfo.FrameColor := clSkyBlue;
  Markup.MarkupInfo.Background := clSkyBlue;
  Markup.WaitTime := 500;
  Markup.Trim := True;
  Markup.FullWord := False;
  Markup.IgnoreKeywords := False;  }
end;

procedure TFrameLccLogging.SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Windows/Linux/OSX already handled by SynEdit using the Windows Shortcuts
  {$IFDEF darwin}
  if (Shift = [ssMeta]) then
  begin
    case Key of
    VK_C: SynEdit.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
    VK_V: SynEdit.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
    VK_X: SynEdit.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
    end;
  end;
  {$ENDIF}
end;



end.

