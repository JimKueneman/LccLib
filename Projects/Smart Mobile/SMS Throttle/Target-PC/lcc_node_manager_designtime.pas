unit lcc_node_manager_designtime;

interface

uses
  Classes,
  SysUtils,
  contnrs,
  ExtCtrls,
  lcc_node_manager;

type
  { TLccNodeManagerDesignTime }

  TLccNodeManagerDesignTime = class(TLccNodeManager)
  private
    FHardwareConnection: TLccHardwareConnectionManager;
  protected
    procedure DoLccMessageSend(Message: TLccMessage); override;
    procedure DoLccMessageReceive(Message: TLccMessage); override;
  public

  published
    property HardwareConnection: TLccHardwareConnectionManager read FHardwareConnection write FHardwareConnection;
  end;


  { TLccCanNodeManager }

  TLccCanNodeManager = class(TLccNodeManager)
  private
    FHardwareConnection: TLccHardwareConnectionManager;
  protected
    procedure DoLccMessageSend(Message: TLccMessage); override;
    procedure DoLccMessageReceive(Message: TLccMessage); override;
  public

  published
    property HardwareConnection: TLccHardwareConnectionManager read FHardwareConnection write FHardwareConnection;
  end;

implementation

{ TLccCanNodeManager }

procedure TLccCanNodeManager.DoLccMessageSend(Message: TLccMessage);
begin
  inherited DoLccMessageReceive(Message);
  if Assigned(HardwareConnection) then
    HardwareConnection.SendMessage(Message);
end;

procedure TLccCanNodeManager.DoLccMessageReceive(Message: TLccMessage);
begin
  inherited DoLccMessageReceive(Message);
  if Assigned(HardwareConnection) then
    HardwareConnection.SendMessage(Message);
end;


{ TLccNodeManagerDesignTime }

procedure TLccNodeManagerDesignTime.DoLccMessageReceive(Message: TLccMessage);
begin
  inherited DoLccMessageReceive(Message);
  if Assigned(HardwareConnection) then
    HardwareConnection.SendMessage(Message);
end;

procedure TLccNodeManagerDesignTime.DoLccMessageSend(Message: TLccMessage);
begin
  inherited DoLccMessageReceive(Message);
  if Assigned(HardwareConnection) then
    HardwareConnection.SendMessage(Message);
end;


initialization
   RegisterClass(TLccNodeManager);

finalization

end.

