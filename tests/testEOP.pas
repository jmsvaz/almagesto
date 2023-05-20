unit testEOP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, almEOP;

type

  TTestEOPDownload= class(TTestCase)
  protected
    fEOPDownload: TEOPDownload;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDownloadEOPC04;
  end;

  { TTestEOP }

  TTestEOP = class(TTestCase)
  protected
    fEOP: TEOP;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEOPData;
    procedure TestMinDate;
  end;

implementation

uses almBase;

{ TTestEOP }

procedure TTestEOP.SetUp;
begin
  fEOP:= TEOP.Create(GetCurrentDir);
end;

procedure TTestEOP.TearDown;
begin
  FreeAndNil(fEOP);
end;

procedure TTestEOP.TestEOPData;
var
  MJD: TMJD;
  DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double;
  Computed, Expected: Double;
begin
  // Test values from some line of 2023-05-17 file (https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now)
  //  # YR  MM  DD  HH       MJD        x(")        y(")  UT1-UTC(s)       dX(")      dY(")       xrt(")      yrt(")      LOD(s)        x Er        y Er  UT1-UTC Er      dX Er       dY Er       xrt Er      yrt Er      LOD Er
  //  1999  10   4   0  51455.00    0.007840    0.381709   0.4672324    0.000191    0.000171    0.000988    0.000813   0.0008735    0.000088    0.000072   0.0000300    0.000104    0.000103    0.000233    0.000271   0.0000232

  fEOP.AutoDownload:= True;

  MJD:= 51455.00;
  fEOP.GetEOP(MJD, DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt);

  Expected:= 0.4672324;
  Computed:= DUT1;
  AssertEquals('DUT1', Expected,Computed,1e-7);

  Expected:= 0.007840;
  Computed:= Xp;
  AssertEquals('Xp',Expected,Computed,1e-7);

  Expected:= 0.381709;
  Computed:= Yp;
  AssertEquals('Yp',Expected,Computed,1e-7);

  Expected:= 0.0008735;
  Computed:= LOD;
  AssertEquals('LOD',Expected,Computed,1e-7);

  Expected:= 0.000191;
  Computed:= dX;
  AssertEquals('dX',Expected,Computed,1e-7);

  Expected:= 0.000171;
  Computed:= dY;
  AssertEquals('dY',Expected,Computed,1e-7);

  Expected:= 0.000988;
  Computed:= xrt;
  AssertEquals('xrt',Expected,Computed,1e-7);

  Expected:= 0.000813;
  Computed:= yrt;
  AssertEquals('yrt',Expected,Computed,1e-7);
end;

procedure TTestEOP.TestMinDate;
var
  Computed, Expected: TMJD;
begin
  fEOP.Download;

  Expected:= 37665.00;
  Computed:= fEOP.MinDate;
  AssertEquals('MinDate',Expected,Computed,1e-7);
end;

procedure TTestEOPDownload.TestDownloadEOPC04;
var
  DownloadedFile: string;
begin
  DownloadedFile:= fEOPDownload.DownloadEOPC04;
  Assert(FileExists(DownloadedFile));
end;

procedure TTestEOPDownload.SetUp;
begin
  fEOPDownload:= TEOPDownload.Create(GetCurrentDir);
end;

procedure TTestEOPDownload.TearDown;
begin
  fEOPDownload.Free;
end;

initialization

  RegisterTest(TTestEOPDownload);
  RegisterTest(TTestEOP);
end.

