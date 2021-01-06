unit lcc_ethernet_http;


{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF FPC}
  {$IFNDEF FPC_CONSOLE_APP}
  //  {$DEFINE LOGGING}
  {$ENDIF}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP} LResources, Forms, Controls, Graphics, Dialogs, {$ENDIF}
  {$ELSE}
  FMX.Forms, Types, System.Generics.Collections,
  {$ENDIF}
  {$IFDEF LOGGING}
  frame_lcc_logging,
  lcc_detailed_logging,
  {$ENDIF}

  {$IFDEF ULTIBO}
  lcc_threaded_stringlist,
  Winsock2,
  Console,
  {$ELSE}
  blcksock,
  synsock,
  Synautil,
  {$ENDIF}
  lcc_defines,
  lcc_node_manager,
  lcc_node_messages,
  lcc_ethernet_client,
  lcc_app_common_settings,
  lcc_node_messages_can_assembler_disassembler,
  lcc_common_classes,
  lcc_ethernet_common,
  lcc_ethernet_server;

type

   { TLccHTTPServerThread }

  TLccHTTPServerThread = class(TLccEthernetServerThread)
    procedure Execute; override;
  end;

  { TLccHTTPListener }

  TLccHTTPListener = class(TLccEthernetListener)
  protected
    function CreateThreadObject: TLccEthernetServerThread; override;
  end;


  { TLccHTTPServer }

  TLccHTTPServer = class(TLccEthernetServer)
  protected
    function CreateListenerObject(AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener; override;
    function IsLccLink: Boolean; override;
  end;

implementation

{ TLccHTTPServer }

function TLccHTTPServer.CreateListenerObject(
  AConnectionInfo: TLccEthernetConnectionInfo): TLccEthernetListener;
begin
  Result := TLccHTTPListener.Create(True, Self, AConnectionInfo);
end;

function TLccHTTPServer.IsLccLink: Boolean;
begin
  Result := False
end;

{ TLccHTTPServerThread }

// Good Example of HTTP
// https://wiki.freepascal.org/Light_Web_Server
procedure TLccHTTPServerThread.Execute;
const
  BASE_PATH = '/Users/JimKueneman/Documents/LccLib/Projects/Smart Mobile/SMS Throttle/SmartMobileStudio/Throttle/www';
  INDEX_PATH = '/INDEX.HTML';
  MANIFEST_PATH = '/APP.MANIFEST';
  CSS_PATH = '/RES/APP.CSS';
  SHIM_PATH = '/LIB/MUTATION.OBSERVER.SHIM.JS';
  POLYFILL_PATH = '/LIB/POLYFILL.CUSTOM.EVENTS.JS';
  MAIN_PATH = '/MAIN.JS';
  JSON_PATH = '/WEBAPP.JSON';

var
  RxStr, FilePath: String;
  InHeader, OutHeader: TStringList;
  Header, ContentType: string;
  MemStream: TFileStream;
begin
  FRunning := True;

  HandleSendConnectionNotification(lcsConnecting);
  Socket := TTCPBlockSocket.Create;          // Created in context of the thread
  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
  Socket.HeartbeatRate := ConnectionInfo.HeartbeatRate;
  Socket.SetTimeout(0);
  Socket.Socket := ListenerSocketHandle;    // Read back the handle
  if Socket.LastError <> 0 then
  begin
    HandleErrorAndDisconnect;
    Socket.CloseSocket;
    Socket.Free;
    Socket := nil;
    FRunning := False
  end else
  begin
    ConnectionInfo.ClientIP := Socket.GetRemoteSinIP;
    ConnectionInfo.ClientPort := Socket.GetRemoteSinPort;
    ConnectionInfo.ListenerIP := Socket.GetLocalSinIP;
    ConnectionInfo.ListenerPort := Socket.GetLocalSinPort;
    if Socket.LastError <> 0 then
    begin
      HandleErrorAndDisconnect;
      Socket.CloseSocket;
      Socket.Free;
      Socket := nil;
      FRunning := False
    end else
    begin
      HandleSendConnectionNotification(lcsConnected);
      try
        InHeader := TStringList.Create;
        OutHeader := TStringList.Create;
        try
          while not IsTerminated and (ConnectionInfo.ConnectionState = lcsConnected) do
          begin
            RxStr := Socket.RecvString(1000);

            case Socket.LastError of
              0 :
                begin
                  InHeader.Add(RxStr);
                  if RxStr = '' then
                  begin
                    Header := UpperCase( InHeader[0]);
                    if Pos('GET', Header) = 1 then
                    begin // Is a GET

                      FilePath := '';

                      if Pos(' ' + MANIFEST_PATH + ' ', Header) = 4 then
                      begin
                        FilePath := BASE_PATH + MANIFEST_PATH;
                        ContentType := 'Content-type: text/plain';
                      end else
                      if Pos(' ' + CSS_PATH + ' ', Header) = 4 then
                      begin
                        FilePath := BASE_PATH + CSS_PATH;
                        ContentType := 'Content-type: Text/css';
                      end else
                      if Pos(' ' + SHIM_PATH + ' ', Header) = 4 then
                      begin
                        FilePath := BASE_PATH + SHIM_PATH;
                        ContentType := 'Content-type: text/javascript';
                      end else
                      if Pos(' ' + POLYFILL_PATH + ' ', Header) = 4 then
                      begin
                        FilePath := BASE_PATH + POLYFILL_PATH;
                        ContentType := 'Content-type: text/javascript';
                      end else
                      if Pos(' ' + MAIN_PATH + ' ', Header) = 4 then
                      begin
                        FilePath := BASE_PATH + MAIN_PATH;
                         ContentType := 'Content-type: text/javascript';
                      end else
                      if Pos(' ' + JSON_PATH + ' ', Header) = 4 then
                      begin
                        FilePath := BASE_PATH + JSON_PATH;
                        ContentType := 'Content-type: application/json';
                      end else
                      if (Pos(' ' + '/' + ' ', Header) = 4) or (Pos(' ' + INDEX_PATH + ' ', Header) = 4) then
                      begin
                        FilePath := BASE_PATH + INDEX_PATH;
                        ContentType := 'Content-type: Text/Html';
                      end;

                      if FileExists(FilePath) then
                      begin
                        MemStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
                        try
                          MemStream.Position := 0;
                          Socket.SendString('HTTP/1.0 200' + CRLF);
                          Socket.SendString(ContentType + CRLF);
                          Socket.SendString('Content-length: ' + IntToStr(MemStream.Size) + CRLF);
                    //      Socket.SendString('Connection: close' + CRLF);
                          Socket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
                          Socket.SendString('Server: Mustangpeak LccLib' + CRLF);
                          Socket.SendString('' + CRLF);
                          Socket.SendStreamRaw(MemStream);
                          Socket.SendString(CRLF);
                        finally
                          MemStream.Free;
                          InHeader.Clear;
                        end;
                      end else
                      begin
                        // probably should return a fail or something here
                        beep;
                      end;
                    end;
                  end;
                end;
              WSAETIMEDOUT :
                begin
           //       HandleErrorAndDisconnect;
                end;
              WSAECONNRESET   :
                begin
                  HandleErrorAndDisconnect;
                end
            else
              HandleErrorAndDisconnect
            end;

        end;
        finally
          FreeAndNil(InHeader);
          FreeAndNil(OutHeader);
          HandleSendConnectionNotification(lcsDisconnecting);
          Socket.CloseSocket;
          Socket.Free;
          Socket := nil;
        end;
      finally
        HandleSendConnectionNotification(lcsDisconnected);
        Owner.ConnectionThreads.Remove(Self);
        FRunning := False;
      end;
    end;
  end;
end;

{ TLccHTTPListener }

function TLccHTTPListener.CreateThreadObject: TLccEthernetServerThread;
begin
  Result := TLccHTTPServerThread.Create(True, Owner, ConnectionInfo);
end;


end.

