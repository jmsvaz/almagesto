unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, TAGraph, TASeries, TAChartAxis, TALegendPanel, almDateTime;

type

  { TfmMain }

  TfmMain = class(TForm)
    btDraw: TButton;
    btSave: TButton;
    Chart: TChart;
    cbUTC: TCheckBox;
    cbUT1: TCheckBox;
    cbUT0: TCheckBox;
    cbUT2: TCheckBox;
    cbTAI: TCheckBox;
    cbTT: TCheckBox;
    cbTCG: TCheckBox;
    cbTCB: TCheckBox;
    cbTDB: TCheckBox;
    ChartLegendPanel1: TChartLegendPanel;
    deStart: TDateEdit;
    deEnd: TDateEdit;
    gbTimeScales: TGroupBox;
    lbEndDate: TLabel;
    lbStartDate: TLabel;
    SaveDialog: TSaveDialog;
    procedure btDrawClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure cbTAIChange(Sender: TObject);
    procedure cbTCBChange(Sender: TObject);
    procedure cbTCGChange(Sender: TObject);
    procedure cbTDBChange(Sender: TObject);
    procedure cbTTChange(Sender: TObject);
    procedure cbUT0Change(Sender: TObject);
    procedure cbUT1Change(Sender: TObject);
    procedure cbUT2Change(Sender: TObject);
    procedure cbUTCChange(Sender: TObject);
    procedure ChartAxisList1MarkToText(var AText: String; AMark: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveDialogTypeChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    UTC,UT1,TAI,TT,TCG,TCB,TDB,UT0,UT2: TLineSeries;
    TS: TTimeScales;
    procedure Draw;
  end; 

var
  fmMain: TfmMain;

implementation

uses almBase, almConsts;

{$R *.lfm}


{ TfmMain }

procedure TfmMain.btDrawClick(Sender: TObject);
begin
  Draw;
end;

procedure TfmMain.btSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    case SaveDialog.FilterIndex of
      1: // bmp: TBitmap
        Chart.SaveToFile(TBitmap, SaveDialog.FileName);
      2: // xpm: TPixmap
        Chart.SaveToFile(TPixmap, SaveDialog.FileName);
      3: // png: TPortableNetworkGraphic
        Chart.SaveToFile(TPortableNetworkGraphic, SaveDialog.FileName);
      4: // jpg: TJpegImage
        Chart.SaveToFile(TJpegImage, SaveDialog.FileName);
      5: // tif: TTiffImage
        Chart.SaveToFile(TTiffImage, SaveDialog.FileName);
    end;
end;

procedure TfmMain.cbTAIChange(Sender: TObject);
begin
  if cbTAI.Checked then
    Chart.AddSeries(TAI)
  else
    Chart.DeleteSeries(TAI);
end;

procedure TfmMain.cbTCBChange(Sender: TObject);
begin
  if cbTCB.Checked then
    Chart.AddSeries(TCB)
  else
    Chart.DeleteSeries(TCB);
end;

procedure TfmMain.cbTCGChange(Sender: TObject);
begin
  if cbTCG.Checked then
    Chart.AddSeries(TCG)
  else
    Chart.DeleteSeries(TCG);
end;

procedure TfmMain.cbTDBChange(Sender: TObject);
begin
  if cbTDB.Checked then
    Chart.AddSeries(TDB)
  else
    Chart.DeleteSeries(TDB);
end;

procedure TfmMain.cbTTChange(Sender: TObject);
begin
  if cbTT.Checked then
    Chart.AddSeries(TT)
  else
    Chart.DeleteSeries(TT);
end;

procedure TfmMain.cbUT0Change(Sender: TObject);
begin
  if cbUT0.Checked then
    Chart.AddSeries(UT0)
  else
    Chart.DeleteSeries(UT0);
end;

procedure TfmMain.cbUT1Change(Sender: TObject);
begin
  if cbUT1.Checked then
    Chart.AddSeries(UT1)
  else
    Chart.DeleteSeries(UT1);
end;

procedure TfmMain.cbUT2Change(Sender: TObject);
begin
  if cbUT2.Checked then
    Chart.AddSeries(UT2)
  else
    Chart.DeleteSeries(UT2);
end;

procedure TfmMain.cbUTCChange(Sender: TObject);
begin
  if cbUTC.Checked then
    Chart.AddSeries(UTC)
  else
    Chart.DeleteSeries(UTC);
end;

procedure TfmMain.ChartAxisList1MarkToText(var AText: String; AMark: Double);
begin
  AText:= FormatDateTime('yyyy.mm.dd',AMark)
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  // workaround until bug Id 17373 is resolved: http://bugs.freepascal.org/view.php?id=17373
  Chart.BottomAxis.OnMarkToText:= @ChartAxisList1MarkToText; // should config at runtime

  Caption:= Application.Title;
  ts:= TTimeScales.Create;

  UTC := TLineSeries.Create(Chart);
  UTC.Title := 'UTC';
  UTC.SeriesColor := clRed;

  TAI := TLineSeries.Create(Chart);
  TAI.Title := 'TAI';
  TAI.SeriesColor := clGreen;

  TT := TLineSeries.Create(Chart);
  TT.Title := 'TT';
  TT.SeriesColor := clBlue;

  TCG := TLineSeries.Create(Chart);
  TCG.Title := 'TCG';
  TCG.SeriesColor := clYellow;

  TCB := TLineSeries.Create(Chart);
  TCB.Title := 'TCB';
  TCB.SeriesColor := clFuchsia;

  TDB := TLineSeries.Create(Chart);
  TDB.Title := 'TDB';
  TDB.SeriesColor := clOlive;

  UT1 := TLineSeries.Create(Chart);
  UT1.Title := 'UT1';
  UT1.SeriesColor := clPurple;

  UT0 := TLineSeries.Create(Chart);
  UT0.Title := 'UT0';
  UT0.SeriesColor := clSilver;

  UT2 := TLineSeries.Create(Chart);
  UT2.Title := 'UT2';
  UT2.SeriesColor := clSilver;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  deStart.Date:= EncodeDate(1950,1,1);
  deEnd.Date:= EncodeDate(2020,1,1);

  cbUTC.Checked:= True;
  cbTAI.Checked:= True;
  cbTT.Checked:= True;
  cbTCG.Checked:= True;
  cbTCB.Checked:= True;
  cbTDB.Checked:= True;
  cbUT1.Checked:= True;
  cbUT0.Checked:= True;
  cbUT2.Checked:= True;

  Draw;
end;

procedure TfmMain.SaveDialogTypeChange(Sender: TObject);
begin
  case SaveDialog.FilterIndex of
    1: // bmp: TBitmap
      SaveDialog.DefaultExt:= '.bmp';
    2: // xpm: TPixmap
      SaveDialog.DefaultExt:= '.xpm';
    3: // png: TPortableNetworkGraphic
      SaveDialog.DefaultExt:= '.png';
    4: // jpg: TJpegImage
      SaveDialog.DefaultExt:= '.jpg';
    5: // tif: TTiffImage
      SaveDialog.DefaultExt:= '.tif';
  end;
end;

procedure TfmMain.Draw;
var
  i,Days: Integer;
  d1,d2: TDateTime;
  offset: TJulianDate;
begin
  TAI.Clear;
  UTC.Clear;
  TT.Clear;
  TCG.Clear;
  TCB.Clear;
  TDB.Clear;
  UT1.Clear;
  UT2.Clear;
  UT0.Clear;

  d1:= deStart.Date;
  d2:= deEnd.Date;
  ts.UTC:= DateTimeToJulianDate(d1);
  Days:= Trunc(d2-d1);

  for i:= 1 to Days do
    begin
       offset:= ts.TAI;
       d1:= JulianDateToDateTime(ts.UTC);
       TAI.AddXY(d1, (ts.TAI-offset)*SecondsPerDay);
       UTC.AddXY(d1, (ts.UTC-offset)*SecondsPerDay);
       TT.AddXY(d1 , (ts.TT-offset)*SecondsPerDay);
       TCG.AddXY(d1, (ts.TCG-offset)*SecondsPerDay);
       TCB.AddXY(d1, (ts.TCB-offset)*SecondsPerDay);
       TDB.AddXY(d1, (ts.TDB-offset)*SecondsPerDay);
       UT1.AddXY(d1, (ts.UT1-offset)*SecondsPerDay);
       UT2.AddXY(d1, (ts.UT2-offset)*SecondsPerDay);
       UT0.AddXY(d1, (ts.UT0-offset)*SecondsPerDay);

       ts.UTC:= ts.UTC + 1;
    end;
end;

end.
