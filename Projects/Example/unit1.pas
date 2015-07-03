unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, lcc_cdi_parser, frame_lcc_logging,
  lcc_ethenetserver, lcc_app_common_settings, frame_lcc_settings,
  lcc_ethernetclient, lcc_nodemanager, lcc_comport;

type

  { TForm1 }

  TForm1 = class(TForm)
    FrameLccSettings: TFrameLccSettings;
    LccComPort: TLccComPort;
    LccEthernetClient: TLccEthernetClient;
    LccNodeManager: TLccNodeManager;
    LccSettings: TLccSettings;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

