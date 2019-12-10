unit TabMainForm;

interface

uses 
  System.Types,
  System.Types.Convert,
  System.Objects,
  System.Time,
  System.IOUtils,
  System.Device.Storage,
  SmartCL.System,
  SmartCL.Time,
  SmartCL.Graphics,
  SmartCL.Components,
  SmartCL.FileUtils,
  SmartCL.Device.Storage,
  SmartCL.Forms,
  SmartCL.Fonts,
  SmartCL.Theme,
  SmartCL.Borders,
  SmartCL.Application,
  SmartCL.Controls,
  SmartCL.Layout,
  Storage;

type
  TTabMainForm = class(TW3Form)
  private
    {$I 'TabMainForm:intf'}
 //   FOptionsLayout: TLayout;
 //   FLayout: TLayout;
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  end;

implementation

{ TTabMainForm }

procedure TTabMainForm.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
end;

procedure TTabMainForm.InitializeObject;
begin
  inherited;
  {$I 'TabMainForm:impl'}

{  FOptionsLayout := Layout.Client(Layout.Margins(3).Spacing(3), [
    Layout.Top([
      Layout.Left(Layout.Spacing(3), [W3LabelIPAddress]),
      Layout.Client(Layout.Margins(3), [W3EditBoxIpAddress])
    ]),
    Layout.Top([
      Layout.Left(Layout.Spacing(3), [W3LabelIpPort]),
      Layout.Client(Layout.Margins(3), [W3EditBoxIpPort])
    ]),
    Layout.Top(W3CheckBoxTcp)

  ]);

  FLayout := Layout.Client(Layout.Margins(3).Spacing(3), [
    Layout.Top(W3LabelHeader),
    Layout.Client(W3PanelSettings),
    Layout.Bottom(W3ButtonConnection),
    Layout.Bottom(W3TabControlNav)
  ]);

  }

end;


 
procedure TTabMainForm.Resize;
begin
  inherited;
//  FLayout.Resize(Self);
//  FOptionsLayout.Resize(W3PanelSettings);
  W3Panel1.Width := Width;
  W3Panel1.Height := Height;
end;


initialization
  Forms.RegisterForm({$I %FILE%}, TTabMainForm);
end.
