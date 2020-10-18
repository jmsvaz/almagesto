unit testEarthOrientation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
                                           // Eps0, EpsA,PsiA,ChiA,OmegaA
  { TTestEarthOrientation }

  TTestEarthOrientation= class(TTestCase)
  published
    procedure TestJ2000ObliquityIAU1980;
    procedure TestJ2000ObliquityIAU2006;
    procedure TestMeanObliquityIAU1980;
    procedure TestMeanObliquityIAU2000;
    procedure TestMeanObliquityIAU2006;
    procedure TestPrecessionEps0_IAU1976;
    procedure TestPrecessionEpsA_IAU1976;
    procedure TestPrecessionPsiA_IAU1976;
    procedure TestPrecessionChiA_IAU1976;
    procedure TestPrecessionOmegaA_IAU1976;
    procedure TestPrecessionEps0_IAU2000;
    procedure TestPrecessionEpsA_IAU2000;
    procedure TestPrecessionPsiA_IAU2000;
    procedure TestPrecessionChiA_IAU2000;
    procedure TestPrecessionOmegaA_IAU2000;
    procedure TestPrecessionEps0_IAU2006;
    procedure TestPrecessionEpsA_IAU2006;
    procedure TestPrecessionPsiA_IAU2006;
    procedure TestPrecessionChiA_IAU2006;
    procedure TestPrecessionOmegaA_IAU2006;
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
    procedure TestNutationDeltaPsi_IAU1980;
    procedure TestNutationDeltaEps_IAU1980;
    procedure TestNutationDeltaPsi_IAU2000A_IERS;
    procedure TestNutationDeltaEps_IAU2000A_IERS;
    procedure TestNutationDeltaPsi_IAU2000A;
    procedure TestNutationDeltaEps_IAU2000A;
    procedure TestNutationDeltaPsi_IAU2000B;
    procedure TestNutationDeltaEps_IAU2000B;
    procedure TestERA_IAU2000;
    procedure TestGMST_IAU1982;
    procedure TestGMST_IAU2000;
    procedure TestGMST_IAU2006;
    procedure TestEquationOfEquinoxes_IAU1994;
    procedure TestEquationOfEquinoxes_IAU2000;
    procedure TestEquationOfEquinoxesCT_IAU2000;
    procedure TestEquationOfEquinoxes_IAU2000A;
    procedure TestEquationOfEquinoxes_IAU2000B;
    procedure TestEquationOfEquinoxes_IAU2006A;
  end;

implementation

uses almBase, almEarth;

procedure TTestEarthOrientation.TestJ2000ObliquityIAU1980;
var
  ComputedEps0: Double;
  ExpectedEps0: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  ExpectedEps0:= 0.409092804222329; //84381.448*RadiansPerArcSecond;

  ComputedEps0:= ObliquityJ2000IAU1980;

  AssertEquals('J2000 Obliquity IAU1980',ExpectedEps0,ComputedEps0,1e-14);
end;

procedure TTestEarthOrientation.TestJ2000ObliquityIAU2006;
var
  ComputedEps0: Double;
  ExpectedEps0: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  ExpectedEps0:= 0.4090926006005828715;

  ComputedEps0:= ObliquityJ2000IAU2006;

  AssertEquals('J2000 Obliquity IAU2006',ExpectedEps0,ComputedEps0,1e-14);
end;

procedure TTestEarthOrientation.TestMeanObliquityIAU1980;
var
  TDB: TJulianDate;
  ComputedEpsA: Double;
  ExpectedEpsA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 54388.0;
  ExpectedEpsA:= 0.4090751347643816218;

  ComputedEpsA:= MeanObliquityIAU1980(TDB);

  AssertEquals('EpsA IAU1980',ExpectedEpsA,ComputedEpsA,1e-14);
end;

procedure TTestEarthOrientation.TestMeanObliquityIAU2000;
var
  TDB: TJulianDate;
  ComputedEpsA: Double;
  ExpectedEpsA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedEpsA:= 0.4090791789404229916;

  ComputedEpsA:= MeanObliquityIAU2000(TDB);

  AssertEquals('EpsA IAU2000',ExpectedEpsA,ComputedEpsA,1e-12);
end;

procedure TTestEarthOrientation.TestMeanObliquityIAU2006;
var
  TDB: TJulianDate;
  ComputedEpsA: Double;
  ExpectedEpsA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 54388.0;
  ExpectedEpsA:= 0.4090749229387258204;

  ComputedEpsA:= MeanObliquityIAU2006(TDB);

  AssertEquals('EpsA IAU2006',ExpectedEpsA,ComputedEpsA,1e-14);
end;

procedure TTestEarthOrientation.TestPrecessionEps0_IAU1976;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedEps0: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 52541.0;
  ExpectedEps0:= 0.409092804222329; //84381.448*RadiansPerArcSecond;

  PrecessionIAU1976(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('Eps0 IAU1976',ExpectedEps0,ComputedEps0,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionEpsA_IAU1976;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedEpsA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 54388.0;
  ExpectedEpsA:= 0.4090751347643816218;

  PrecessionIAU1976(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('EpsA IAU1976',ExpectedEpsA,ComputedEpsA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionPsiA_IAU1976;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedPsiA: Double;
begin
  // There are no test values

  TDB:= 2400000.5 + 52541.0;
  ExpectedPsiA:= 0;

  PrecessionIAU1976(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('PsiA IAU1976 (no test value)',ExpectedPsiA,ComputedPsiA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionChiA_IAU1976;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedChiA: Double;
begin
  // There are no test values

  TDB:= 2400000.5 + 52541.0;
  ExpectedChiA:= 0;

  PrecessionIAU1976(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('ChiA IAU1976 (no test value)',ExpectedChiA,ComputedChiA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionOmegaA_IAU1976;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedOmegaA: Double;
begin
  // There are no test values

  TDB:= 2400000.5 + 52541.0;
  ExpectedOmegaA:= 0;

  PrecessionIAU1976(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('OmegaA IAU1976 (no test value)',ExpectedOmegaA,ComputedOmegaA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionEps0_IAU2000;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedEps0: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 52541.0;
  ExpectedEps0:= 0.409092804222329; //84381.448*RadiansPerArcSecond;

  PrecessionIAU2000(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('Eps0 IAU2000',ExpectedEps0,ComputedEps0,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionEpsA_IAU2000;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedEpsA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedEpsA:= 0.4090791789404229916;

  PrecessionIAU2000(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('EpsA IAU2000',ExpectedEpsA,ComputedEpsA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionPsiA_IAU2000;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedPsiA: Double;
begin
  // There are no test values

  TDB:= 2400000.5 + 52541.0;
  ExpectedPsiA:= 0;

  PrecessionIAU2000(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('PsiA IAU2000 (no test value)',ExpectedPsiA,ComputedPsiA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionChiA_IAU2000;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedChiA: Double;
begin
  // There are no test values

  TDB:= 2400000.5 + 52541.0;
  ExpectedChiA:= 0;

  PrecessionIAU2000(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('ChiA IAU2000 (no test value)',ExpectedChiA,ComputedChiA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionOmegaA_IAU2000;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedOmegaA: Double;
begin
  // There are no test values

  TDB:= 2400000.5 + 52541.0;
  ExpectedOmegaA:= 0;

  PrecessionIAU2000(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('OmegaA IAU2000 (no test value)',ExpectedOmegaA,ComputedOmegaA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionEps0_IAU2006;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedEps0: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 52541.0;
  ExpectedEps0:= 0.4090926006005828715;

  PrecessionIAU2006(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('Eps0 IAU2006',ExpectedEps0,ComputedEps0,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionEpsA_IAU2006;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedEpsA: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 52541.0;
  ExpectedEpsA:= 0.4090864054922431688;

  PrecessionIAU2006(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('EpsA IAU2006',ExpectedEpsA,ComputedEpsA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionPsiA_IAU2006;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedPsiA: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 52541.0;
  ExpectedPsiA:= 0.6664369630191613431e-3;

  PrecessionIAU2006(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('PsiA IAU2006',ExpectedPsiA,ComputedPsiA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionChiA_IAU2006;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedChiA: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 52541.0;
  ExpectedChiA:= 0.1387703379530915364e-5;

  PrecessionIAU2006(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('ChiA IAU2006',ExpectedChiA,ComputedChiA,1e-15);
end;

procedure TTestEarthOrientation.TestPrecessionOmegaA_IAU2006;
var
  TDB: TJulianDate;
  ComputedEps0, ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA: Double;
  ExpectedOmegaA: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 52541.0;
  ExpectedOmegaA:= 0.4090925973783255982;

  PrecessionIAU2006(TDB,ComputedEps0,ComputedEpsA,ComputedPsiA,ComputedChiA,ComputedOmegaA);

  AssertEquals('OmegaA IAU2006',ExpectedOmegaA,ComputedOmegaA,1e-15);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentL_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 5.132369751108684150;

  faL_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument L from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentLP_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 6.226797973505507345;

  faLP_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument LP from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentF_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 0.2597711366745499518;

  faF_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument F from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentD_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 1.946709205396925672;

  faD_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument D from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentOM_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= -5.973618440951302183;

  faOM_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument OM from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentMe_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 5.417338184297289661;

  faMe_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument Me from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentVe_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 3.424900460533758000;

  faVe_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument Ve from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentEa_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 1.744713738913081846;

  faEa_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument Ea from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentMa_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 3.275506840277781492;

  faMa_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument Ma from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentJu_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 5.275711665202481138;

  faJu_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument Ju from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentSa_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 5.371574539440827046;

  faSa_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument Sa from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentUr_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 5.180636450180413523;

  faUr_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument Ur from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentNe_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 2.079343830860413523;

  faNe_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument Ne from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestFundamentalArgumentPA_IERS2003;
var
  TDB_C: Double;
  ComputedFA: Double;
  ExpectedFA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB_C:= 0.8;
  ExpectedFA:= 0.1950884762240000000e-1;

  faPA_IERS2003(TDB_C,ComputedFA);

  AssertEquals('Fundamental Argument PA from IERS2003',ExpectedFA,ComputedFA,1e-12);
end;

procedure TTestEarthOrientation.TestNutationDeltaPsi_IAU1980;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9643658353226563966e-5;

  NutationIAU1980(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi IAU1980',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
end;

procedure TTestEarthOrientation.TestNutationDeltaEps_IAU1980;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaEps: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaEps:= 0.4060051006879713322e-4;

  NutationIAU1980(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaEps IAU1980',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
end;

procedure TTestEarthOrientation.TestNutationDeltaPsi_IAU2000A_IERS;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi: Double;
begin
  // There are no test values

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9630909107115518431e-5;

  NutationIAU2000A_IERS2003(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi IERS IAU2000A',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
end;

procedure TTestEarthOrientation.TestNutationDeltaEps_IAU2000A_IERS;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaEps: Double;
begin
  // There are no test values

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaEps:= 0.4063239174001678710e-4;

  NutationIAU2000A_IERS2003(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaEps IERS IAU2000A',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
end;

procedure TTestEarthOrientation.TestNutationDeltaPsi_IAU2000A;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9630909107115518431e-5;

  NutationIAU2000A(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi IAU2000A',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
end;

procedure TTestEarthOrientation.TestNutationDeltaEps_IAU2000A;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaEps: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaEps:= 0.4063239174001678710e-4;

  NutationIAU2000A(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaEps IAU2000A',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
end;

procedure TTestEarthOrientation.TestNutationDeltaPsi_IAU2000B;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9632552291148362783e-5;

  NutationIAU2000B(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi IAU2000B',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
end;

procedure TTestEarthOrientation.TestNutationDeltaEps_IAU2000B;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaEps: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaEps:=  0.4063197106621159367e-4;

  NutationIAU2000B(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaEps IAU2000B',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
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

  AssertEquals('ERA IAU2000',ExpectedERA,ComputedERA,1e-12);
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

  AssertEquals('GMST IAU1982',ExpectedGMST,ComputedGMST,1e-12);
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

  AssertEquals('GMST IAU2000',ExpectedGMST,ComputedGMST,1e-12);
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

  AssertEquals('GMST IAU2006',ExpectedGMST,ComputedGMST,1e-12);
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

  AssertEquals('EqEq IAU1994',ExpectedEqEq,ComputedEqEq,1e-17);
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

  AssertEquals('EqEq IAU2000',ExpectedEqEq,ComputedEqEq,1e-18);

end;

procedure TTestEarthOrientation.TestEquationOfEquinoxesCT_IAU2000;
var
  TDB: TJulianDate;
  ComputedCT: Double;
  ExpectedCT: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 53736.0;

  ExpectedCT:= 0.2046085004885125264e-8;

  EquationOfEquinoxesCT_IAU2000(TDB, ComputedCT);

  AssertEquals('EqEq CT IAU2000',ExpectedCT,ComputedCT,1e-20);
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

  AssertEquals('EqEq IAU2000A',ExpectedEqEq,ComputedEqEq,1e-18);
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

  AssertEquals('EqEq IAU2000B',ExpectedEqEq,ComputedEqEq,1e-18);
end;

procedure TTestEarthOrientation.TestEquationOfEquinoxes_IAU2006A;
var
  TDB: TJulianDate;
  ComputedEqEq: Double;
  ExpectedEqEq: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedEqEq:= -0.8834195072043790156e-5;

  EquationOfEquinoxes_IAU2006A(TDB, ComputedEqEq);

  AssertEquals('EqEq IAU2006A',ExpectedEqEq,ComputedEqEq,1e-18);
end;


initialization

  RegisterTest(TTestEarthOrientation);
end.

