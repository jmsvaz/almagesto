unit testEarthOrientation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestEarthOrientation }

  TTestEarthOrientation= class(TTestCase)
  published
    procedure TestPrecession_IAU2006;
    procedure TestFundamentalArgumentL_IERS2003;
    procedure TestFundamentalArgumentLP_IERS2003;
    procedure TestFundamentalArgumentF_IERS2003;
    procedure TestFundamentalArgumentD_IERS2003;
    procedure TestFundamentalArgumentOM_IERS2003;
    procedure TestFundamentalArgumentMe_IERS2003;
    procedure TestFundamentalArgumentVe_IERS2003;
    procedure TestFundamentalArgumentEa_IERS2003;
    procedure TestFundamentalArgumentMa_IERS2003;
    procedure TestFundamentalArgumentJu_IERS2003;
    procedure TestFundamentalArgumentSa_IERS2003;
    procedure TestFundamentalArgumentUr_IERS2003;
    procedure TestFundamentalArgumentNe_IERS2003;
    procedure TestFundamentalArgumentPA_IERS2003;
    procedure TestNutation_IAU1980;
    procedure TestNutation_IAU2000A_IERS;
    procedure TestNutation_IAU2000A_SOFA;
    procedure TestNutation_IAU2000B;
    procedure TestERA_IAU2000;
    procedure TestGMST_IAU1982;
    procedure TestGMST_IAU2000;
    procedure TestGMST_IAU2006;
    procedure TestEquationOfEquinoxes_IAU1994;
    procedure TestEquationOfEquinoxes_IAU2000;
    procedure TestEquationOfEquinoxes_IAU2000A;
    procedure TestEquationOfEquinoxes_IAU2000B;
  end;

implementation

uses almBase, almEarth;

procedure TTestEarthOrientation.TestPrecession_IAU2006;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedEps0, ExpectedEpsA,ExpectedPsiA,ExpectedChiA,ExpectedOmegaA: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 52541.0;
  ExpectedEps0:= 0.4090926006005828715;
  ExpectedEpsA:= 0.4090864054922431688;
  ExpectedPsiA:= 0.6664369630191613431e-3;
  ExpectedChiA:= 0.1387703379530915364e-5;
  ExpectedOmegaA:= 0.4090925973783255982;

  PrecessionIAU2006(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('Eps0',ExpectedEps0,ComputedEps0,1e-15);
  AssertEquals('EpsA',ExpectedEpsA,ComputedEpsA,1e-15);
  AssertEquals('PsiA',ExpectedPsiA,ComputedPsiA,1e-15);
  AssertEquals('ChiA',ExpectedChiA,ComputedChiA,1e-15);
  AssertEquals('OmegaA',ExpectedOmegaA,ComputedOmegaA,1e-15);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentL_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 5.132369751108684150;

  faL_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument L from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentLP_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 6.226797973505507345;

  faLP_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument LP from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentF_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 0.2597711366745499518;

  faF_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument F from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentD_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 1.946709205396925672;

  faD_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument D from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentOM_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= -5.973618440951302183;

  faOM_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument OM from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentMe_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 5.417338184297289661;

  faMe_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument Me from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentVe_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 3.424900460533758000;

  faVe_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument Ve from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentEa_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 1.744713738913081846;

  faEa_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument Ea from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentMa_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 3.275506840277781492;

  faMa_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument Ma from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentJu_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 5.275711665202481138;

  faJu_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument Ju from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentSa_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 5.371574539440827046;

  faSa_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument Sa from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentUr_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 5.180636450180413523;

  faUr_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument Ur from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentNe_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 2.079343830860413523;

  faNe_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument Ne from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentPA_IERS2003;
var
  TDB: TJulianDate;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 0.8*JulianDaysPerCentury + J2000;
  ExpectedFA:= 0.1950884762240000000e-1;

  faPA_IERS2003(TDB,ComputedFA);

  AssertEquals('Fundamental Argument PA from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestNutation_IAU1980;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi, ExpectedDeltaEps: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9643658353226563966e-5;
  ExpectedDeltaEps:= 0.4060051006879713322e-4;

  NutationIAU1980(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
  AssertEquals('DeltaEps',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
end;

procedure TTestEarthOrientation.TestNutation_IAU2000A_IERS;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi, ExpectedDeltaEps: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9630909107115518431e-5;
  ExpectedDeltaEps:= 0.4063239174001678710e-4;

  NutationIAU2000A_IERS(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
  AssertEquals('DeltaEps',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
end;

procedure TTestEarthOrientation.TestNutation_IAU2000A_SOFA;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi, ExpectedDeltaEps: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9630909107115518431e-5;
  ExpectedDeltaEps:= 0.4063239174001678710e-4;

  NutationIAU2000A(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
  AssertEquals('DeltaEps',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
end;

procedure TTestEarthOrientation.TestNutation_IAU2000B;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi, ExpectedDeltaEps: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9632552291148362783e-5;
  ExpectedDeltaEps:=  0.4063197106621159367e-4;

  NutationIAU2000B(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
  AssertEquals('DeltaEps',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
end;

procedure TTestEarthOrientation.TestERA_IAU2000;
var
  UT1: TJulianDate;
  ComputedERA: Double;
  ExpectedERA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  UT1:= 2400000.5 + 54388.0;
  ExpectedERA:= 0.4022837240028158102;

  EarthRotationAngleIAU2000(UT1,ComputedERA);

  AssertEquals('ERA_2000',ExpectedERA,ComputedERA,1e-12);
end;

procedure TTestEarthOrientation.TestGMST_IAU1982;
var
  UT1: TJulianDate;
  ComputedGMST: Double;
  ExpectedGMST: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  UT1:= 2400000.5 + 53736.0;
  ExpectedGMST:= 1.754174981860675096;

  GreenwichMeanSiderealTimeIAU1982(UT1,ComputedGMST);

  AssertEquals('GMST_82',ExpectedGMST,ComputedGMST,1e-12);
end;

procedure TTestEarthOrientation.TestGMST_IAU2000;
var
  UT1,TT: TJulianDate;
  ComputedGMST: Double;
  ExpectedGMST: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  UT1:= 2400000.5 + 53736.0;
  TT:= 2400000.5 + 53736.0;
  ExpectedGMST:= 1.754174972210740592;

  GreenwichMeanSiderealTimeIAU2000(UT1,TT,ComputedGMST);

  AssertEquals('GMST_2000',ExpectedGMST,ComputedGMST,1e-12);
end;

procedure TTestEarthOrientation.TestGMST_IAU2006;
var
  UT1,TT: TJulianDate;
  ComputedGMST: Double;
  ExpectedGMST: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  UT1:= 2400000.5 + 53736.0;
  TT:= 2400000.5 + 53736.0;
  ExpectedGMST:= 1.754174971870091203;

  GreenwichMeanSiderealTimeIAU2006(UT1,TT,ComputedGMST);

  AssertEquals('GMST_2006',ExpectedGMST,ComputedGMST,1e-12);
end;

procedure TTestEarthOrientation.TestEquationOfEquinoxes_IAU1994;
var
  TDB: TJulianDate;
  ComputedEqEq: Double;
  ExpectedEqEq: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 41234.0;
  ExpectedEqEq:= 0.5357758254609256894e-4;

  EquationOfEquinoxes_IAU1994(TDB, ComputedEqEq);

  AssertEquals('EqEq_1994',ExpectedEqEq,ComputedEqEq,1e-17);
end;

procedure TTestEarthOrientation.TestEquationOfEquinoxes_IAU2000;
var
  TDB: TJulianDate;
  DeltaPsi, EpsA: Double;
  ComputedEqEq: Double;
  ExpectedEqEq: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 53736.0;

  EpsA:=  0.4090789763356509900;
  DeltaPsi:= -0.9630909107115582393e-5;

  ExpectedEqEq:= -0.8834193235367965479e-5;

  EquationOfEquinoxes_IAU2000(TDB, DeltaPsi, EpsA, ComputedEqEq);

  AssertEquals('EqEq_2000',ExpectedEqEq,ComputedEqEq,1e-18);

end;

procedure TTestEarthOrientation.TestEquationOfEquinoxes_IAU2000A;
var
  TDB: TJulianDate;
  ComputedEqEq: Double;
  ExpectedEqEq: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedEqEq:= -0.8834192459222588227e-5;

  EquationOfEquinoxes_IAU2000A(TDB, ComputedEqEq);

  AssertEquals('EqEq_2000A',ExpectedEqEq,ComputedEqEq,1e-18);
end;

procedure TTestEarthOrientation.TestEquationOfEquinoxes_IAU2000B;
var
  TDB: TJulianDate;
  ComputedEqEq: Double;
  ExpectedEqEq: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedEqEq:= -0.8835700060003032831e-5;

  EquationOfEquinoxes_IAU2000B(TDB, ComputedEqEq);

  AssertEquals('EqEq_2000B',ExpectedEqEq,ComputedEqEq,1e-18);
end;


initialization

  RegisterTest(TTestEarthOrientation);
end.

