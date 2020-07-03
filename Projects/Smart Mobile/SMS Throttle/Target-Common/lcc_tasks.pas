unit lcc_tasks;

interface

uses
  Classes, SysUtils,
  lcc_node,
  lcc_node_manager,
  lcc_node_messages,
  lcc_defines;

type

  { TLccTask }

  TLccTask = class(TObject)
  private
    FCompleted: Boolean;
    FInternalTime: Integer;
    FLccNode: TLccNode;
    FMaxTimeToTimeout: Integer;
    FOnSendMessage: TOnMessageEvent;
    FState: Integer;
  public
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
    property State: Integer read FState write FState;
    property Completed: Boolean read FCompleted write FCompleted;
    property InternalTime: Integer read FInternalTime;
    property MaxTimeToTimeout: Integer read FMaxTimeToTimeout write FMaxTimeToTimeout;
    property LccNode: TLccNode read FLccNode;

    constructor Create(ALccNode: TLccNode; AnOnSendMessage: TOnMessageEvent);
    destructor Destroy; override;

    procedure ProcessTask(LccMessage: TLccMessage); virtual;
  end;

  { TTractionTrainManageReservationTask }
  // This task is launched when a Manage->ReserveTrain received.  It handles the
  // reservation and release before it ends.
  TTractionTrainManageReservationTask = class(TLccTask)
  public
    procedure ProcessTask(LccMessage: TLccMessage); override;
  end;

  { TTractionSearchRequestTask }

  TTractionSearchRequestTask = class(TLccTask)
  public
    procedure ProcessTask(LccMessage: TLccMessage); override;
  end;

  { TLccTaskManager }

  TLccTaskManager = class(TObject)
  private
    FOnSendMessage: TOnMessageEvent;
    FTaskList: TList;

  protected
    property TaskList: TList read FTaskList write FTaskList;
    property OnSendMessage: TOnMessageEvent read FOnSendMessage write FOnSendMessage;
  public
    constructor Create(AnOnSendMessage: TOnMessageEvent);
    destructor Destroy; override;

    procedure AddTask(ATask: TLccTask);
    procedure ProcessTasks(LccMessage: TLccMessage);
  end;

implementation

{ TTractionSearchRequestTask }

procedure TTractionSearchRequestTask.ProcessTask(LccMessage: TLccMessage);
  begin
  case State of
    0 :
      begin

      end;
    1 :
      begin

      end;
    2 :
      begin
        Completed := True;
      end;
  end;
end;

{ TTractionTrainManageReservationTask }

procedure TTractionTrainManageReservationTask.ProcessTask(LccMessage: TLccMessage);
begin
  inherited ProcessTask(LccMessage);
  case State of
    0 :
      begin

      end;
    1 :
      begin

      end;
    2 :
      begin
        Completed := True;
      end;
  end;
end;

{ TLccTask }

constructor TLccTask.Create(ALccNode: TLccNode; AnOnSendMessage: TOnMessageEvent);
begin
  inherited Create;
  FOnSendMessage := AnOnSendMessage;
  FLccNode := ALccNode;
end;

destructor TLccTask.Destroy;
begin
  inherited Destroy;
end;

procedure TLccTask.ProcessTask(LccMessage: TLccMessage);
begin
  Inc(FMaxTimeToTimeout);
  Inc(FInternalTime);
  if (MaxTimeToTimeout > InternalTime) then
    Completed := True;
end;

{ TLccTaskManager }

constructor TLccTaskManager.Create(AnOnSendMessage: TOnMessageEvent);
begin
  inherited Create;
  FTaskList := TList.Create;
  FOnSendMessage := AnOnSendMessage;
end;

procedure TLccTaskManager.AddTask(ATask: TLccTask);
begin
   TaskList.Add(ATask);
end;

destructor TLccTaskManager.Destroy;
begin
  FreeAndNil(FTAskList);
  inherited Destroy;
end;

procedure TLccTaskManager.ProcessTasks(LccMessage: TLccMessage);
var
  i: Integer;
  Task: TLccTask;
begin
  for i := 0 to TaskList.Count - 1 do
  begin
    Task := TLccTask(TaskList[i]);
    Task.ProcessTask(LccMessage);
    if Task.Completed then
    begin
      TaskList.Remove(Task);
      FreeAndNil(Task);
    end;
  end;
end;

end.

