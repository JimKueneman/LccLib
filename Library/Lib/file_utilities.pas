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
  {$IFDEF FPC}
    Forms,
  {$ELSE}
    FMX.Forms,
    System.IOUtils,
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

Function ValidateIP(IP4: string): Boolean; // Coding by Dave Sonsalla

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
    {$IFDEF LCC_MOBILE}
      Result := TPath.GetDocumentsPath;
      // Mobile devices have 0 indexed strings
      if Result[Length(Result)-1] <> '/' then
        Result := Result + '/';
    {$ELSE}
      Result := '';
      {$IFDEF LCC_OSX}
        Result := GetApplicationPath;
        Result := ExtractFileDir(Result);
        Result := ExtractFileDir(Result);
        Result := ExtractFileDir(Result);
        Result := Result + '/' + PATH_OSX_RESOURCES;
      {$ELSE}
        Result := GetApplicationPath;
      {$ENDIF}
      if Result[Length(Result)] <> '/' then
        Result := Result + '/';
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

Function ValidateIP(IP4: string): Boolean; // Coding by Dave Sonsalla
Var
  Octet : String;
  Dots, I : Integer;
Begin
  IP4 := IP4+'.'; //add a dot. We use a dot to trigger the Octet check, so need the last one
  Dots := 0;
  Octet := '0';
  {$IFDEF LCC_MOBILE}
  For I := 0 To Length(IP4) - 1 Do
  {$ELSE}
  For I := 1 To Length(IP4) Do
  {$ENDIF}
  Begin
    If IP4[I] in ['0'..'9','.'] Then
    Begin
      If IP4[I] = '.' Then //found a dot so inc dots and check octet value
      Begin
        Inc(Dots);
        If (length(Octet) =1) Or (StrToInt(Octet) > 255) Then Dots := 5; //Either there's no number or it's higher than 255 so push dots out of range
        Octet := '0'; // Reset to check the next octet
      End // End of IP4[I] is a dot
      Else // Else IP4[I] is not a dot so
      Octet := Octet + IP4[I]; // Add the next character to the octet
    End // End of IP4[I] is not a dot
    Else // Else IP4[I] Is not in CheckSet so
      Dots := 5; // Push dots out of range
  End;
  Result := (Dots = 4) // The only way that Dots will equal 4 is if we passed all the tests
end;

end.

