unit testEarthOrientation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestEarthOrientation }

  TTestEarthOrientation= class(TTestCase)
  published
    procedure TestPrecessionIAU2006;
    procedure TestNutationIAU1980;
    procedure TestNutationIAU2000A_IERS;
    procedure TestNutationIAU2000A_SOFA;
    procedure TestNutationIAU2000B;
    procedure TestERAIAU2000;
  end;

implementation

uses almBase, almEarth;

procedure TTestEarthOrientation.TestPrecessionIAU2006;
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

procedure TTestEarthOrientation.TestNutationIAU1980;
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

procedure TTestEarthOrientation.TestNutationIAU2000A_IERS;
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

procedure TTestEarthOrientation.TestNutationIAU2000A_SOFA;
var
  TDB: TJulianDate;
  ComputedDeltaPsi, ComputedDeltaEps: Double;
  ExpectedDeltaPsi, ExpectedDeltaEps: Double;
begin
  // Test values from IAU SOFA C version 2012-03-01 Release

  TDB:= 2400000.5 + 53736.0;
  ExpectedDeltaPsi:= -0.9630909107115518431e-5;
  ExpectedDeltaEps:= 0.4063239174001678710e-4;

  NutationIAU2000A_SOFA(TDB,ComputedDeltaPsi,ComputedDeltaEps);

  AssertEquals('DeltaPsi',ExpectedDeltaPsi,ComputedDeltaPsi,1e-13);
  AssertEquals('DeltaEps',ExpectedDeltaEps,ComputedDeltaEps,1e-13);
end;

procedure TTestEarthOrientation.TestNutationIAU2000B;
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

procedure TTestEarthOrientation.TestERAIAU2000;
var
  UT1: TJulianDate;
  ComputedERA: Double;
  ExpectedERA: Double;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release

  UT1:= 2400000.5 + 54388.0;
  ExpectedERA:= 0.4022837240028158102;

  EarthRotationAngleIAU2000(UT1,ComputedERA);

  AssertEquals('ERA',ExpectedERA,ComputedERA,1e-12);
end;


initialization

  RegisterTest(TTestEarthOrientation);
end.

