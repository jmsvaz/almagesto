unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, almDateTime;

type

  { TfmMain }

  TfmMain = class(TForm)
    btCompute: TButton;
    deUTC: TDateEdit;
    edTime: TEdit;
    edTAI: TLabeledEdit;
    edTT: TLabeledEdit;
    edTDB: TLabeledEdit;
    edUT1: TLabeledEdit;
    edUT2: TLabeledEdit;
    edUT0: TLabeledEdit;
    edTCG: TLabeledEdit;
    edTCB: TLabeledEdit;
    lbUTC: TLabel;
    pnTimeScales: TPanel;
    procedure btComputeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ts: TTimeScales;
    procedure DisplayTimeScales;
  end; 

var
  fmMain: TfmMain;

implementation

uses almBase;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
var
  dt: TDateTime;
begin
  ts:= TTimeScales.Create;
  dt:= Now;
  deUTC.Date:= dt;
  edTime.Text:= FormatDateTime('hh:nn:ss.zzz',dt);
end;

procedure TfmMain.btComputeClick(Sender: TObject);
begin
  ts.UTC:= DateTimeToJulianDate(deUTC.Date + StrToTime(edTime.Text));
  DisplayTimeScales;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  ts.Free;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  Caption:= Application.Title;

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
