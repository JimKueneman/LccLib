unit file_utilities;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  {$IFDEF DARWIN}
  CFBase, CFBundle, CFURL, CFString,
  {$ENDIF}
  {$IFNDEF FPC}
    FMX.Forms,
  {$ENDIF}
  Classes, SysUtils;

const
  PATH_OSX_RESOURCES = 'Contents/Resources/';
  PATH_OSX_EXECUTABLE = 'Contents/MacOS/';
  PATH_UNIX_APPLICATION = '/usr/share/';    // Typical place to store the application foldler
  PATH_UNIX_SETTINGS = '/home/{user}/.config/{executable_name}';  // GetAppConfigDir  does this for us but this is what it returns
  PATH_LINUX_DEV = '/dev/';
  PATH_OSX_DEV = 'dev/';

function GetApplicationPath: string;    // Returns the path to the executible (except for Linix were it returns the root folder of the Application folder) ending in the path delimiter
function GetSettingsPath: string;

implementation

function GetSettingsPath: string;
{$IFDEF DARWIN}
//var
//  pathRef: CFURLRef;
//  pathCFStr: CFStringRef;
//  pathStr: shortstring;
{$ENDIF}
begin
  // Under OSX we get the path of the executable
  {$IFDEF FPC}
    {$IFDEF DARWIN}
    Result := GetApplicationPath + PATH_OSX_RESOURCES;
    {$ENDIF}
      // Under Windows we get the path of the executable
    {$IFDEF Windows}
    Result := GetApplicationPath;
    {$ENDIF}
    {$IFDEF Linux}
    Result := GetAppConfigDir(False);
    {$ENDIF}
  {$ELSE}
     Result := 'TODO...';



    {$IFDEF DARWIN}
  //  Result := GetApplicationPath + PATH_OSX_RESOURCES;
    {$ENDIF}
    {$IFDEF Linux}
  //  Result := GetAppConfigDir(False);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LCC_WINDOWS}
    Result := GetApplicationPath;
  {$ENDIF}
end;


function GetApplicationPath: string;
{$IFDEF FPC}
  {$IFDEF DARWIN}
  var
    pathRef: CFURLRef;
    pathCFStr: CFStringRef;
    pathStr: shortstring;
  {$ENDIF}
{$ELSE}
var
  LModuleName: string;
{$ENDIF}
begin
  {$IFDEF FPC}
    // Under OSX we get the path of the executable
    {$IFDEF DARWIN}
    pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
    pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
    CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
    CFRelease(pathRef);
    CFRelease(pathCFStr);
    Result := pathStr + '/';
    {$ENDIF}
      // Under Windows we get the path of the executable
    {$IFDEF Windows}
    Result := ExtractFilePath(Application.ExeName);
    {$ENDIF}
    {$IFDEF Linux}
    Result := PATH_UNIX_APPLICATION;    // Linux is typically hardcoded to a path
    {$ENDIF}
  {$ELSE}
     LModuleName := GetModuleName(MainInstance);
     // UNC issue in Vista.
     if Pos('\\?\', LModuleName) = 1 then
       Delete(LModuleName, 1, 4);
     Result := ExtractFilePath(LModuleName);
  {$ENDIF}

end;

end.

