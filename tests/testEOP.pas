unit testEOP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, almEOP;

type

  { TTestEOPDownload }

  TTestEOPDownload= class(TTestCase)
  protected
    fEOPDownload: TEOPDownload;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDownloadEOPC04;
    procedure TestDownloadEOP20C04;
    procedure TestDownloadEOP14C04;
    procedure TestDownloadEOPC01;
    procedure TestDownloadEOPFinals2000A;
  end;


  { TTestEOPData }

  TTestEOPData = class(TTestCase)
  protected
    fEOPDownload: TEOPDownload;
    fEOP: TEOPData;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEOPC04;
    procedure TestEOP20C04;
    procedure TestEOP14C04;
    procedure TestEOPC01;
    procedure TestEOPFinals2000A;

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
    procedure TestAfterMaxDate;
  end;

implementation

uses almBase;

{ TTestEOPData }

procedure TTestEOPData.SetUp;
begin
  fEOPDownload:= TEOPDownload.Create(GetCurrentDir);
end;

procedure TTestEOPData.TearDown;
begin
  FreeAndNil(fEOPDownload);
  if Assigned(fEOP) then
    FreeAndNil(fEOP);
end;

procedure TTestEOPData.TestEOPC04;
var
  FileName: string;
  MJD: TMJD;
  id: Integer;
  EOPItem: TEOPItem;
  Computed, Expected: Double;
begin
  FileName:= fEOPDownload.DownloadEOPC04;
  if FileExists(FileName) then
    begin
      fEOP:= TEOPReader.ReadEOPC04File(FileName);
      try
        // Test values from some line of 2023-05-17 file (https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now)
        //  # YR  MM  DD  HH       MJD        x(")        y(")  UT1-UTC(s)       dX(")      dY(")       xrt(")      yrt(")      LOD(s)        x Er        y Er  UT1-UTC Er      dX Er       dY Er       xrt Er      yrt Er      LOD Er
        //  1999  10   4   0  51455.00    0.007840    0.381709   0.4672324    0.000191    0.000171    0.000988    0.000813   0.0008735    0.000088    0.000072   0.0000300    0.000104    0.000103    0.000233    0.000271   0.0000232
        MJD:= 51455.00;
        if fEOP.Find(MJD, id) then
          begin
            EOPItem:= fEOP[id];

            Expected:= 0.4672324;
            Computed:= EOPItem.DUT1;
            AssertEquals('DUT1', Expected,Computed,1e-7);

            Expected:= 0.007840;
            Computed:= EOPItem.Xp;
            AssertEquals('Xp',Expected,Computed,1e-7);

            Expected:= 0.381709;
            Computed:= EOPItem.Yp;
            AssertEquals('Yp',Expected,Computed,1e-7);

            Expected:= 0.0008735;
            Computed:= EOPItem.LOD;
            AssertEquals('LOD',Expected,Computed,1e-7);

            Expected:= 0.000191;
            Computed:= EOPItem.dX;
            AssertEquals('dX',Expected,Computed,1e-7);

            Expected:= 0.000171;
            Computed:= EOPItem.dY;
            AssertEquals('dY',Expected,Computed,1e-7);

            Expected:= 0.000988;
            Computed:= EOPItem.xrt;
            AssertEquals('xrt',Expected,Computed,1e-7);

            Expected:= 0.000813;
            Computed:= EOPItem.yrt;
            AssertEquals('yrt',Expected,Computed,1e-7);
          end
        else
          Fail('eopc04: MJD not found.');
      except
        Fail('eopc04: File not loaded.');
      end;
    end
  else
    Fail('eopc04: File not downloaded.');
end;

procedure TTestEOPData.TestEOP20C04;
var
  FileName: string;
  MJD: TMJD;
  id: Integer;
  EOPItem: TEOPItem;
  Computed, Expected: Double;
begin
  FileName:= fEOPDownload.DownloadEOP20C04;
  if FileExists(FileName) then
    begin
      fEOP:= TEOPReader.ReadEOP20C04File(FileName);
      try
        // Test values from some line of 2023-05-17 file (https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now)
        //  # YR  MM  DD  HH       MJD        x(")        y(")  UT1-UTC(s)       dX(")      dY(")       xrt(")      yrt(")      LOD(s)        x Er        y Er  UT1-UTC Er      dX Er       dY Er       xrt Er      yrt Er      LOD Er
        //  1999  10   4   0  51455.00    0.007840    0.381709   0.4672324    0.000191    0.000171    0.000988    0.000813   0.0008735    0.000088    0.000072   0.0000300    0.000104    0.000103    0.000233    0.000271   0.0000232
        MJD:= 51455.00;
        if fEOP.Find(MJD, id) then
          begin
            EOPItem:= fEOP[id];

            Expected:= 0.4672324;
            Computed:= EOPItem.DUT1;
            AssertEquals('DUT1', Expected,Computed,1e-7);

            Expected:= 0.007840;
            Computed:= EOPItem.Xp;
            AssertEquals('Xp',Expected,Computed,1e-7);

            Expected:= 0.381709;
            Computed:= EOPItem.Yp;
            AssertEquals('Yp',Expected,Computed,1e-7);

            Expected:= 0.0008735;
            Computed:= EOPItem.LOD;
            AssertEquals('LOD',Expected,Computed,1e-7);

            Expected:= 0.000191;
            Computed:= EOPItem.dX;
            AssertEquals('dX',Expected,Computed,1e-7);

            Expected:= 0.000171;
            Computed:= EOPItem.dY;
            AssertEquals('dY',Expected,Computed,1e-7);

            Expected:= 0.000988;
            Computed:= EOPItem.xrt;
            AssertEquals('xrt',Expected,Computed,1e-7);

            Expected:= 0.000813;
            Computed:= EOPItem.yrt;
            AssertEquals('yrt',Expected,Computed,1e-7);
          end
        else
          Fail('eop20c04: MJD not found.');
      except
        Fail('eop20c04: File not loaded.');
      end;
    end
  else
    Fail('eop20c04: File not downloaded.');
end;

procedure TTestEOPData.TestEOP14C04;
var
  FileName: string;
  MJD: TMJD;
  id: Integer;
  EOPItem: TEOPItem;
  Computed, Expected: Double;
begin
  FileName:= fEOPDownload.DownloadEOP14C04;
  if FileExists(FileName) then
    begin
      fEOP:= TEOPReader.ReadEOP14C04File(FileName);
      try
        // Test values from some line of 2023-06-04 file (https://hpiers.obspm.fr/iers/eop/eopc04_14/eopc04_IAU2000.62-now)
        //        Date      MJD      x          y        UT1-UTC       LOD         dX        dY        x Err     y Err   UT1-UTC Err  LOD Err     dX Err       dY Err
        //                           "          "           s           s          "         "           "          "          s         s            "           "
        //       (0h UTC)
        //  1999  10   4  51455   0.007864   0.381773   0.4671864   0.0008596   0.000147   0.000049   0.000124   0.000091  0.0000052  0.0000312    0.000053    0.000043
        MJD:= 51455;
        if fEOP.Find(MJD, id) then
          begin
            EOPItem:= fEOP[id];

            Expected:= 0.4671864;
            Computed:= EOPItem.DUT1;
            AssertEquals('DUT1', Expected,Computed,1e-7);

            Expected:= 0.007864;
            Computed:= EOPItem.Xp;
            AssertEquals('Xp',Expected,Computed,1e-7);

            Expected:= 0.381773;
            Computed:= EOPItem.Yp;
            AssertEquals('Yp',Expected,Computed,1e-7);

            Expected:= 0.0008596;
            Computed:= EOPItem.LOD;
            AssertEquals('LOD',Expected,Computed,1e-7);

            Expected:= 0.000147;
            Computed:= EOPItem.dX;
            AssertEquals('dX',Expected,Computed,1e-7);

            Expected:= 0.000049;
            Computed:= EOPItem.dY;
            AssertEquals('dY',Expected,Computed,1e-7);

            Expected:= 0.0;
            Computed:= EOPItem.xrt;
            AssertEquals('xrt',Expected,Computed,1e-7);

            Expected:= 0.0;
            Computed:= EOPItem.yrt;
            AssertEquals('yrt',Expected,Computed,1e-7);
          end
        else
          Fail('eop14c04: MJD not found.');
      except
        Fail('eop14c04: File not loaded.');
      end;
    end
  else
    Fail('eop14c04: File not downloaded.');
end;

procedure TTestEOPData.TestEOPC01;
var
  FileName: string;
  MJD: TMJD;
  id: Integer;
  EOPItem: TEOPItem;
  Computed, Expected: Double;
begin
  FileName:= fEOPDownload.DownloadEOPC01;
  if FileExists(FileName) then
    begin
      fEOP:= TEOPReader.ReadEOPC01File(FileName);
      try
        // Test values from some line of 2023-06-04 file (https://hpiers.obspm.fr/iers/eop/eopc01/eopc01.iau2000.1846-now)
        //#  MJD         PM-X      PM-Y       UT1-TAI       DX           DY          X-ERR     Y-ERR      UT1-ERR    DX  -ERR   DY  -ERR        RMS      CORR      CORR      CORR      CORR     IND1     IND2    IND3     XRT       YRT        LOD         DXRT       DYRT       XRT-ERR   YRT-ERR    LOD-ERR     DXRT  -ERR  DYRT  -ERR
        //#              SECONDS   SECONDS    SECONDS       SECONDS      SECONDS     SECONDS   SECONDS    SECONDS    SECONDS    SECONDS         DELAY    X-Y       X-U       Y-U       DX-DY                              SECONDS   SECONDS    SECONDS     SECONDS    SECONDS    SECONDS   SECONDS    SECONDS     SECONDS     SECONDS
        //#              OF ARC    OF ARC     OF TIME       OF ARC       OF ARC      OF ARC    OF ARC     OF TIME    OF ARC     OF ARC          PSEC                                                                      OF ARC    OF ARC     OF TIME     OF ARC     OF ARC     OF ARC    OF ARC     OF TIME     OF ARC      OF ARC
        //#                                                                                                                                                                                                               PER DAY   PER DAY    PER DAY     PER DAY    PER DAY    PER DAY   PER DAY    PER DAY     PER DAY     PER DAY
        //#  (days)         (")       (")        (s)          (")          (")         (")       (")       (s)        (")        (")            (ps)                        (     unitless       )                        ("/d)     ("/d)      (s)         ("/d)      ("/d)      ("/d)     ("/d)      (s)         ("/d)       ("/d)
        //   51452.690  0.007315  0.379788 -31.5308335      0.000028    -0.000104    0.000124  0.000091   0.0000070  0.000054   0.000044          0.     0.000     0.000     0.000     0.000         0      0      0      0.000000  0.000000   0.000894      0          0        0.000000  0.000000   0.000031      0           0

        MJD:= 51452.690;
        if fEOP.Find(MJD, id) then
          begin
            EOPItem:= fEOP[id];

            Expected:= -31.5308335;
            Computed:= EOPItem.DUT1;
            AssertEquals('DUT1', Expected,Computed,1e-8);

            Expected:= 0.007315;
            Computed:= EOPItem.Xp;
            AssertEquals('Xp',Expected,Computed,1e-7);

            Expected:= 0.379788;
            Computed:= EOPItem.Yp;
            AssertEquals('Yp',Expected,Computed,1e-7);

            Expected:= 0.000894;
            Computed:= EOPItem.LOD;
            AssertEquals('LOD',Expected,Computed,1e-7);

            Expected:= 0.000028;
            Computed:= EOPItem.dX;
            AssertEquals('dX',Expected,Computed,1e-7);

            Expected:= -0.000104;
            Computed:= EOPItem.dY;
            AssertEquals('dY',Expected,Computed,1e-7);

            Expected:= 0.000000;
            Computed:= EOPItem.xrt;
            AssertEquals('xrt',Expected,Computed,1e-7);

            Expected:= 0.000000;
            Computed:= EOPItem.yrt;
            AssertEquals('yrt',Expected,Computed,1e-7);
          end
        else
          Fail('eopc01: MJD not found.');
      except
        Fail('eopc01: File not loaded.');
      end;
    end
  else
    Fail('eopc01: File not downloaded.');
end;

procedure TTestEOPData.TestEOPFinals2000A;
var
  FileName: string;
  MJD: TMJD;
  id: Integer;
  EOPItem: TEOPItem;
  Computed, Expected: Double;
begin
  //           MJD          PM-x               PM-y                UT1-UTC              LOD                   dX                 dY
  //                      sec. of arc        sec. of arc          sec. of time         msec. of time        msec. of arc       msec. of arc
  //   9910 4 51455.00 I  0.007845 0.000092  0.381653 0.000097  I 0.4672008 0.0000115  0.8692 0.0074  I     0.055    0.125     0.088    0.298   .007850   .381800   .4672270     1.891     0.284

  FileName:= fEOPDownload.DownloadEOPFinals2000A;
  if FileExists(FileName) then
    begin
      fEOP:= TEOPReader.ReadEOPFinals2000AFile(FileName);
      if Assigned(fEOP) then
        begin
          MJD:= 51455.00;
          if fEOP.Find(MJD, id) then
            begin
              EOPItem:= fEOP[id];

              Expected:= 0.4672008;
              Computed:= EOPItem.DUT1;
              AssertEquals('DUT1', Expected,Computed,1e-8);

              Expected:= 0.007845;
              Computed:= EOPItem.Xp;
              AssertEquals('Xp',Expected,Computed,1e-7);

              Expected:= 0.381653;
              Computed:= EOPItem.Yp;
              AssertEquals('Yp',Expected,Computed,1e-7);

              Expected:= 0.0008692;
              Computed:= EOPItem.LOD;
              AssertEquals('LOD',Expected,Computed,1e-8);

              Expected:= 0.000055;
              Computed:= EOPItem.dX;
              AssertEquals('dX',Expected,Computed,1e-7);

              Expected:= 0.000088;
              Computed:= EOPItem.dY;
              AssertEquals('dY',Expected,Computed,1e-7);

              Expected:= 0.0;
              Computed:= EOPItem.xrt;
              AssertEquals('xrt',Expected,Computed,1e-7);

              Expected:= 0.0;
              Computed:= EOPItem.yrt;
              AssertEquals('yrt',Expected,Computed,1e-7);
            end
          else
            Fail('Finals2000A: MJD not found.');
        end
      else
        Fail('Finals2000A: File not loaded.');
    end
  else
    Fail('Finals2000A: File not downloaded.');
end;

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

procedure TTestEOP.TestAfterMaxDate;
var
  Computed, Expected: Double;
  DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double;
  MJD: TMJD;
begin
  fEOP.Download;
  MJD:= fEOP.MaxDate + 1;
  fEOP.GetEOP(MJD, DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt);

  Expected:= 0;
  Computed:= DUT1;
  AssertEquals('DUT1 after tolerance',Expected,Computed,1e-7);
end;

procedure TTestEOP.TestMinDate;
var
  Computed, Expected: TMJD;
begin
  // MinDate from https://hpiers.obspm.fr/iers/eop/eopc01/eopc01.iau2000.1846-now
  fEOP.Download;

  Expected:= -4703.268;
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

procedure TTestEOPDownload.TestDownloadEOP20C04;
var
  DownloadedFile: string;
begin
  DownloadedFile:= fEOPDownload.DownloadEOP20C04;
  Assert(FileExists(DownloadedFile));
end;

procedure TTestEOPDownload.TestDownloadEOP14C04;
var
  DownloadedFile: string;
begin
  DownloadedFile:= fEOPDownload.DownloadEOP14C04;
  Assert(FileExists(DownloadedFile));
end;

procedure TTestEOPDownload.TestDownloadEOPC01;
var
  DownloadedFile: string;
begin
  DownloadedFile:= fEOPDownload.DownloadEOPC01;
  Assert(FileExists(DownloadedFile));
end;

procedure TTestEOPDownload.TestDownloadEOPFinals2000A;
var
  DownloadedFile: string;
begin
  DownloadedFile:= fEOPDownload.DownloadEOPFinals2000A;
  Assert(FileExists(DownloadedFile));
end;

procedure TTestEOPDownload.SetUp;
begin
  fEOPDownload:= TEOPDownload.Create(GetCurrentDir);
end;

procedure TTestEOPDownload.TearDown;
begin
  FreeAndNil(fEOPDownload);
end;

initialization

  RegisterTest(TTestEOPDownload);
  RegisterTest(TTestEOPData);
  RegisterTest(TTestEOP);
end.

