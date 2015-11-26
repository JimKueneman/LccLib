program RPiSync;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, crt, ftpsend, httpsend
  { you can add units after this };

const
  PI_APP_PATH    = '/home/pi/Documents/LccLib/Projects/LazarusRaspberryPi/Ethernet_Hub_Console/rpican';
  PI_HEX_PATH    = '/home/pi/Documents/LccLib/Projects/LazarusRaspberryPi/Ethernet_Hub_Console/rpican.hex';
  PI_MICRO_URL   = 'http://192.168.0.200:8000/Documents/Mustangpeak-Engineering/Firmware/MikroPascal/Projects/dsPIC33EPxxxGP502/EthernetCanHub/EthernetCanHub.hex';
  PI_FTP_FOLDER  = '/public_ftp/RPiCan';

type

  { TPiCanSync }

  TPiCanSync = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure UploadFiles;
    function CopyOverPiCanMicroCode: Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TPiCanSync }

procedure TPiCanSync.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  if CopyOverPiCanMicroCode then
    UploadFiles;


  // stop program loop
  Terminate;
end;

procedure TPiCanSync.UploadFiles;
var
  FtpClient: TFTPSend;
  CurrentDir: string;
  i: Integer;
begin
  FtpClient := TFTPSend.Create;
  FtpClient.TargetHost := 'ftp.mustangpeak.net';
  FtpClient.TargetPort := cFtpProtocol;
  FtpClient.UserName := 'jimdk@mustangpeak.net';
  FtpClient.Password := 'Vernster2!';
  if FtpClient.Login then
  begin
    WriteLn('Logged in: ' + FtpClient.TargetHost);
    FtpClient.DirectFile := True;
    CurrentDir := FtpClient.GetCurrentDir;
    WriteLn('Current Dir: ' + CurrentDir);
    if CurrentDir <> PI_FTP_FOLDER then
    begin
      FtpClient.ChangeWorkingDir(PI_FTP_FOLDER);
      CurrentDir := FtpClient.GetCurrentDir;
      WriteLn('Current Dir: ' + CurrentDir);
    end;
    if CurrentDir = PI_FTP_FOLDER then
    begin
      WriteLn('Uploading RPi Executible');
      FtpClient.DirectFileName := PI_APP_PATH;
      if FtpClient.StoreFile(ExtractFileName(PI_APP_PATH), False) then
        WriteLn('Success')
      else
        WriteLn('Failure');

      WriteLn('Uploading dsPIC Hex');
      FtpClient.DirectFileName := PI_HEX_PATH;
      if FtpClient.StoreFile(ExtractFileName(PI_HEX_PATH), False) then
        WriteLn('Success')
      else
        WriteLn('Failure');
    end;
    FtpClient.Logout;
  end;
  FtpClient.Free;
end;

function TPiCanSync.CopyOverPiCanMicroCode: Boolean;
var
  MicroHex: TFileStream;
begin
  Result := False;
  MicroHex := TFileStream.Create(PI_HEX_PATH, fmOpenWrite or fmCreate);
  try
    WriteLn('Getting the mikroPascal update from the iMac');
    if HttpGetBinary(PI_MICRO_URL, MicroHex) then
    begin
      Result := True;
      WriteLn('Success')
    end
    else
      WriteLn('Failure: is the Python Web Server running on the iMac?  [python -m SimpleHTTPServer]');
  finally
    MicroHex.Free;
  end;
end;

constructor TPiCanSync.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TPiCanSync.Destroy;
begin
  inherited Destroy;
end;

procedure TPiCanSync.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TPiCanSync;
begin
  Application := TPiCanSync.Create(nil);
  Application.Title := 'PiCanSync';
  Application.Run;
  Application.Free;
end.

