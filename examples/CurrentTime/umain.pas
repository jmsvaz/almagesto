unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, almBase, almDateTime;

type

  { TfmMain }

  TfmMain = class(TForm)
    btTimer: TButton;
    edDST: TFloatSpinEdit;
    edStandardTime: TLabeledEdit;
    edLocalMeanTime: TLabeledEdit;
    edTAI: TLabeledEdit;
    edTCB: TLabeledEdit;
    edTCG: TLabeledEdit;
    edTDB: TLabeledEdit;
    edTimeZone: TFloatSpinEdit;
    edLongitude: TFloatSpinEdit;
    edTT: TLabeledEdit;
    edUT0: TLabeledEdit;
    edUT1: TLabeledEdit;
    edUT2: TLabeledEdit;
    edUTC: TLabeledEdit;
    lbDST: TLabel;
    lbTimeZone: TLabel;
    lbLongitude: TLabel;
    Timer: TTimer;
    procedure btTimerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerStartTimer(Sender: TObject);
    procedure TimerStopTimer(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ts: TTimeScales;
    StandardTime: TDateTime;
    procedure DisplayTimeScales;
  end; 

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  ts:= TTimeScales.Create;
  edTimeZone.Value:= -3;   // Brazil/Sao Paulo timezone
  edDST.Value:= 1;   // We're at Daylight Saving Time on Brazil 2010 Summer!
end;

procedure TfmMain.btTimerClick(Sender: TObject);
begin
  Timer.Enabled:= not Timer.Enabled;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  ts.Free;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  Caption:= Application.Title;
  Timer.Enabled:= True;
end;

procedure TfmMain.TimerStartTimer(Sender: TObject);
begin
  btTimer.Caption:= 'Pause';
end;

procedure TfmMain.TimerStopTimer(Sender: TObject);
begin
  btTimer.Caption:= 'Continue';
end;

procedure TfmMain.TimerTimer(Sender: TObject);
begin
  StandardTime:= Now;
  ts.UTC:= DateTimeToJulianDate(StandardTimeToUTC(StandardTime,edTimeZone.Value,edDST.Value));
  DisplayTimeScales;
end;

procedure TfmMain.DisplayTimeScales;
  function JDFmt(jd: TJulianDate): string;
  var
    floatfmt, dtfmt: string;
  begin
    floatfmt:= '#,##0.00000000';
    dtfmt:= 'yyyy/mm/dd hh:nn:ss.zzz';
    Result:= FormatDateTime(dtfmt,JulianDateToDateTime(jd)) +
             ' -> JD ' + FormatFloat(floatfmt,jd);
  end;
begin
  edStandardTime.Text:= JDFmt(DateTimeToJulianDate(StandardTime));
  edLocalMeanTime.Text:= JDFmt(UniversalTimeToLocalMeanTime(ts.UT1,edLongitude.Value));
  edUTC.Text:= JDFmt(ts.UTC);
  edTAI.Text:= JDFmt(ts.TAI);
  edTT.Text:=  JDFmt(ts.TT);
  edTDB.Text:= JDFmt(ts.TDB);
  edTCG.Text:= JDFmt(ts.TCG);
  edTCB.Text:= JDFmt(ts.TCB);
  edUT1.Text:= JDFmt(ts.UT1);
  edUT2.Text:= JDFmt(ts.UT2);
  edUT0.Text:= JDFmt(ts.UT0);
end;



end.

