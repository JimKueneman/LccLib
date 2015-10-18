unit Unit1;
 
{Demo application for GPIO on Raspberry Pi}
{Inspired by the Python input/output demo application by Gareth Halfacree}
{written for the Raspberry Pi User Guide, ISBN 978-1-118-46446-5}
 
{$mode objfpc}{$H+}
 
interface
 
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Unix, lcc_raspberrypi_gpio;
 
type
 
  { TForm1 }
 
  TForm1 = class(TForm)
    GPIO25In: TButton;
    SpeedButton: TButton;
    LogMemo: TMemo;
    GPIO23switch: TToggleBox;
    Timer1: TTimer;
    GPIO18Pwm: TToggleBox;
    Direction: TToggleBox;
    procedure DirectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GPIO18PwmChange(Sender: TObject);
    procedure GPIO25InClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure GPIO23switchChange(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;
 
const
     INPUT = 0;
     OUTPUT = 1;
     PWM_OUTPUT = 2;
     LOW = False;
     HIGH = True;
     PUD_OFF = 0;
     PUD_DOWN = 1;
     PUD_UP = 2;
 // Convert Raspberry Pi P1 pins (Px) to GPIO port
     P3 = 0;
     P5 = 1;
     P7 = 4;
     P8 = 14;
     P10 = 15;
     P11 = 17;
     P12 = 18;
     P13 = 21;
     P15 = 22;
     P16 = 23;
     P18 = 24;
     P19 = 10;
     P21 = 9;
     P22 = 25;
     P23 = 11;
     P24 = 8;
     P26 = 7;
 
var
  Form1: TForm1;
  GPIO_Driver: TIoDriver;
  GpF: TIoPort;
  PWM :Boolean;
  i, d : integer;
  Pin,Pout,Ppwm : byte;
 
implementation
 
{$R *.lfm}
 
{ TForm1 }
 
procedure TForm1.FormActivate(Sender: TObject);
begin
  if not GPIO_Driver.MapIo then
  begin
     LogMemo.Lines.Add('Error mapping gpio registry');
     Exit;
  end
  else
  begin
    GpF := GpIo_Driver.CreatePort(GPIO_BASE, CLOCK_BASE, GPIO_PWM);
    if not Assigned(GpF) then
      Exit;
  end;
  Timer1.Enabled:= True;
  Timer1.Interval:= 25;     //25 ms controll interval
  Pin := P22;
  Pout := P16;
  Ppwm := P12;
  i:=1;
  GpF.SetPinMode(Pout,OUTPUT);
  GpF.SetPinMode(Pin,INPUT);
  GpF.SetPullMode(Pin,PUD_Up);    // Input PullUp High level
end;

procedure TForm1.GPIO25InClick(Sender: TObject);
begin
  if not Assigned(GpF) then Exit;

  If GpF.GetBit(Pin) then LogMemo.Lines.Add('In: '+IntToStr(1))
                     else LogMemo.Lines.Add('In: '+IntToStr(0));
end;

procedure TForm1.GPIO18PwmChange(Sender: TObject);
begin
  if not Assigned(GpF) then Exit;

  if GPIO18Pwm.Checked then
  begin
    GpF.SetPinMode(Ppwm,PWM_OUTPUT);
    PWM := True;                          //PWM on
  end
  else
  begin
    GpF.SetPinMode(Ppwm,INPUT);
    PWM := False;                          //PWM off
  end;
end;

procedure TForm1.DirectionChange(Sender: TObject);
begin
  if Direction.Checked then d:=10 else d:=-10;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GpIo_Driver := TIoDriver.Create;
end;

 
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GpF.SetPinMode(Ppwm,INPUT);
  GpF.ClearBit(Pout);
  GpIo_Driver.UnmapIoRegisrty(GpF);
  if Assigned(GPIO_Driver) then
    GPIO_Driver.Free;
end;
 
procedure TForm1.GPIO23switchChange(Sender: TObject);

Begin
  if not Assigned(GpF) then Exit;

  Timer1.Enabled := False;
  if GPIO23switch.Checked then
  begin
    GpF.SetBit(Pout); //Turn LED on
  end
  else
  begin
    GpF.ClearBit(Pout); //Turn LED off
  end;
  Timer1.Enabled := True;
end;

procedure TForm1.SpeedButtonClick(Sender: TObject);
var
  i,p: longint;
  k,v: comp;
  ido:TDateTime;
begin
  if not Assigned(GpF) then Exit;

  ido:= Time;
  k:= TimeStampToMSecs(DateTimeToTimeStamp(ido));
  LogMemo.Lines.Add('Start: '+TimeToStr(ido));
  p:=10000000 ;
  For i:=1 to p  do Begin
    GpF.SetBit(P16);
    GpF.ClearBit(P16);
  end;
  ido:= Time;
  v:= TimeStampToMSecs(DateTimeToTimeStamp(ido));
  LogMemo.Lines.Add('Stop: '+TimeToStr(ido)+' Frequency: '+
                                IntToStr(p div (Int64(v-k)))+' kHz');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not Assigned(GpF) then Exit;

  If PWM then Begin
    If (d > 0) and (i+d < 1024) then begin
          i:=i+d;
          GpF.PwmWrite(Ppwm,i);
    end ;
    If (d < 0) and (i+d > -1) then begin
          i:=i+d;
          GpF.PwmWrite(Ppwm,i);
    end;
  end;
end;

 
end.
