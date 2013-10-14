unit testEarthOrientation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TTestEarthOrientation= class(TTestCase)
  published
    procedure TestPrecessionIAU2006;
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



initialization

  RegisterTest(TTestEarthOrientation);
end.

