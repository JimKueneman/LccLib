unit Mustangpeak.FMX.Animations;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Ani, System.Rtti;

type
  TFloatKinematicAnimation = class(TCustomPropertyAnimation)
  private
    FStartFloat: Single;
    FStopMaxValue: Single;
    FStartFromCurrent: Boolean;
    FInitialVelocity: Single;
    FAcceleration: Single;
    FInternalAcceleration: Single;
    procedure SetInitialVelocity(const Value: Single);
  protected
    property InternalAcceleration: Single read FInternalAcceleration write FInternalAcceleration;
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Acceleration: Single read FAcceleration write FAcceleration;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Loop default False;
    property InitialVelocity: Single read FInitialVelocity write SetInitialVelocity;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: Single read FStartFloat write FStartFloat stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopMaxValue: Single read FStopMaxValue write FStopMaxValue stored True nodefault;
    property Trigger;
    property TriggerInverse;
  end;

implementation

{ TFloatKinematicAnimation }

constructor TFloatKinematicAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartFloat := 0;
  FStopMaxValue := 0;
  FAcceleration := 9.8;
  FInitialVelocity := 0;
end;

procedure TFloatKinematicAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
  StopValue: Single;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        StartValue := P.GetValue(FInstance).AsExtended;
    end;
  end;
  InternalAcceleration := 2*(StopMaxValue-(InitialVelocity*Duration)/(Duration*Duration));
end;

procedure TFloatKinematicAnimation.ProcessAnimation;
var
  T: TRttiType;
  P: TRttiProperty;
  s: Single;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      s := StartValue + (InitialVelocity*CurrentTime+(0.5*InternalAcceleration*CurrentTime*CurrentTime));
      if s > StopMaxValue then
        s := StopMaxValue;
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        P.SetValue(FInstance, s);
    end;
  end;
end;

procedure TFloatKinematicAnimation.SetInitialVelocity(const Value: Single);
begin
  FInitialVelocity := Value;
  if FInitialVelocity = 0 then
    FInitialVelocity := 100;
end;

end.
