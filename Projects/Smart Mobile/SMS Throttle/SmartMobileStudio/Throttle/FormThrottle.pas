unit FormThrottle;

interface

uses 
  System.Types,
  System.Types.Convert,
  System.Objects,
  System.Time,
  SmartCL.System,
  SmartCL.Time,
  SmartCL.Graphics,
  SmartCL.Components,
  SmartCL.FileUtils,
  SmartCL.Forms,
  SmartCL.Fonts,
  SmartCL.Theme,
  SmartCL.Borders,
  SmartCL.Application,
  SmartCL.Controls,
  Storage;

type
  TFormThrottle = class(TW3Form)
  private
    {$I 'FormThrottle:intf'}
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  end;

implementation

{ TFormThrottle }

procedure TFormThrottle.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
end;

procedure TFormThrottle.InitializeObject;
begin
  inherited;
  {$I 'FormThrottle:impl'}
end;
 
procedure TFormThrottle.Resize;
begin
  inherited;
  InitializeStorage;
  // Not working....InitializeForm.
  W3TabControl1.Width := ClientWidth;
  W3TabControl1.Height := ClientHeight;

   W3Label1.Caption := IntToStr(ClientWidth);
end;
 
initialization
  Forms.RegisterForm({$I %FILE%}, TFormThrottle);
end.
