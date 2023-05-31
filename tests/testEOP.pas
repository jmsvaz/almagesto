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
    procedure TestMatchValue;
    procedure TestInterpolatedValue;
    procedure TestInterpolatedValueatBegining;
    procedure TestMinDate;
    procedure TestInsideTolerance;
    procedure TestAfterTolerance;
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

procedure TTestEOP.TestMatchValue;
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

procedure TTestEOP.TestInterpolatedValue;
var
  MJD: TMJD;
  DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double;
  Computed, Expected: Double;
begin
{ Test values interpolated in LibreOffice Calc from 2023-05-17 file (https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now)
  # YR  MM  DD  HH       MJD        x(")        y(")           UT1-UTC(s)       dX(")           dY(")           xrt(")           yrt(")         LOD(s)
  1999  10   3   0  51454,00	0,0071390	0,3809570	0,4680178	0,0001570	0,0001540	0,0009860	0,0012470	0,0006884
  1999  10   4   0  51455,00	0,0078400	0,3817090	0,4672324	0,0001910	0,0001710	0,0009880	0,0008130	0,0008735
  1999  10   5   0  51456,00	0,0084590	0,3821910	0,4662616	0,0001670	0,0001240	0,0007840	0,0005380	0,0010529
  1999  10   6   0  51457,00	0,0091830	0,3826600	0,4651447	0,0000480	-0,0000120	0,0008030	0,0005930	0,0011824
interpolated value: 51455,50	0,0081481	0,3819677	0,4667677	0,0001886	0,0001571	0,0008849	0,0006449	0,0009667
}

  fEOP.AutoDownload:= True;

  MJD:= 51455.5;
  fEOP.GetEOP(MJD, DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt);

  Expected:= 0.4667677;
  Computed:= DUT1;
  AssertEquals('DUT1', Expected,Computed,1e-7);

  Expected:= 0.0081481;
  Computed:= Xp;
  AssertEquals('Xp',Expected,Computed,1e-7);

  Expected:= 0.3819677;
  Computed:= Yp;
  AssertEquals('Yp',Expected,Computed,1e-7);

  Expected:= 0.0009667;
  Computed:= LOD;
  AssertEquals('LOD',Expected,Computed,1e-7);

  Expected:= 0.0001886;
  Computed:= dX;
  AssertEquals('dX',Expected,Computed,1e-7);

  Expected:= 0.0001571;
  Computed:= dY;
  AssertEquals('dY',Expected,Computed,1e-7);

  Expected:= 0.0008849;
  Computed:= xrt;
  AssertEquals('xrt',Expected,Computed,1e-7);

  Expected:= 0.0006449;
  Computed:= yrt;
  AssertEquals('yrt',Expected,Computed,1e-7);

end;

procedure TTestEOP.TestInterpolatedValueatBegining;
var
  MJD: TMJD;
  DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double;
  Computed, Expected: Double;
begin
{ Test values interpolated in LibreOffice Calc from 2023-05-17 file (https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now)
       MJD        x(")        y(")           UT1-UTC(s)       dX(")           dY(")           xrt(")           yrt(")         LOD(s)
       37665,00	-0,0127000	0,2130000	0,0326338	0,0000000	0,0000000	0,0000000	0,0000000	0,0017230
       37666,00	-0,0159000	0,2141000	0,0320547	0,0000000	0,0000000	0,0000000	0,0000000	0,0016690
       37667,00	-0,0190000	0,2152000	0,0315526	0,0000000	0,0000000	0,0000000	0,0000000	0,0015820
       37668,00	-0,0219990	0,2163010	0,0311435	0,0000000	0,0000000	0,0000000	0,0000000	0,0014960

interpolated value: 37665,50	-0,0143124	0,2135501	0,0323356	0,0000000	0,0000000	0,0000000	0,0000000	0,0017023
}

  fEOP.AutoDownload:= True;

  MJD:= 37665.5;
  fEOP.GetEOP(MJD, DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt);

  Expected:= 0.0323356;
  Computed:= DUT1;
  AssertEquals('DUT1', Expected,Computed,1e-7);

  Expected:= -0.0143124;
  Computed:= Xp;
  AssertEquals('Xp',Expected,Computed,1e-7);

  Expected:= 0.2135501;
  Computed:= Yp;
  AssertEquals('Yp',Expected,Computed,1e-7);

  Expected:= 0.0017023;
  Computed:= LOD;
  AssertEquals('LOD',Expected,Computed,1e-7);

  Expected:= 0.0000000;
  Computed:= dX;
  AssertEquals('dX',Expected,Computed,1e-7);

  Expected:= 0.0000000;
  Computed:= dY;
  AssertEquals('dY',Expected,Computed,1e-7);

  Expected:= 0.0000000;
  Computed:= xrt;
  AssertEquals('xrt',Expected,Computed,1e-7);

  Expected:= 0.0000000;
  Computed:= yrt;
  AssertEquals('yrt',Expected,Computed,1e-7);
end;

procedure TTestEOP.TestInsideTolerance;
var
  Computed, Expected: Double;
  DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double;
  MJD: TMJD;
begin
  fEOP.Download;

  MJD:= fEOP.MaxDate;
  fEOP.GetEOP(MJD, DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt);
  Expected:= DUT1;

  MJD:= MJD + fEOP.Tolerance/2;
  fEOP.GetEOP(MJD, DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt);
  Computed:= DUT1;

  AssertEquals('DUT1 inside tolerance',Expected,Computed,1e-7);
end;

procedure TTestEOP.TestAfterTolerance;
var
  Computed, Expected: Double;
  DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double;
  MJD: TMJD;
begin
  fEOP.Download;
  MJD:= fEOP.MaxDate + 2*fEOP.Tolerance;
  fEOP.GetEOP(MJD, DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt);

  Expected:= 0;
  Computed:= DUT1;
  AssertEquals('DUT1 after tolerance',Expected,Computed,1e-7);
end;

procedure TTestEOP.TestMinDate;
var
  Computed, Expected: TMJD;
begin
  // MinDate from https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now
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

