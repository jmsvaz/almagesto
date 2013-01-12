unit testLocalReferenceSystems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,almLocalReferenceSystems;

type

  { TTestEllipsoid }

  TTestEllipsoid= class(TTestCase)
  published
    procedure TestWGS60;
    procedure TestIAU64;
    procedure TestWGS66;
    procedure TestSA69;
    procedure TestGRS67;
    procedure TestWGS72;
    procedure TestIAU75;
    procedure TestIAU76;
    procedure TestGRS80;
    procedure TestMERIT83;
    procedure TestWGS84;
    procedure TestIERS1989;
    procedure TestIERS2003;
    procedure TestDefaultEllipsoidValue;
  end;

{ TTestGeodeticTransform }

  TTestGeodeticTransform = class(TTestCase)
  private
    a,f: Double;
  protected
    procedure SetUp; override;
  published
    procedure TestZeroLatLong;
    procedure TestNorthPole;
    procedure TestSomeCoordinate;
    procedure TestTEarthEllipsoidCalling;
    procedure TestTEarthEllipsoidDefaultCalling;
  end;

implementation

uses almBase;

{ TTestEllipsoid }

procedure TTestEllipsoid.TestWGS60;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeWGS60,a,f);
  AssertEquals(6378165,a,0);
  AssertEquals(1/298.3,f,0);
end;

procedure TTestEllipsoid.TestIAU64;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeIAU64,a,f);
  AssertEquals(6378160,a,0);
  AssertEquals(1/298.25,f,0)
end;

procedure TTestEllipsoid.TestWGS66;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeWGS66,a,f);
  AssertEquals(6378145,a,0);
  AssertEquals(1/298.25,f,0);
end;

procedure TTestEllipsoid.TestSA69;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeSA69,a,f);
  AssertEquals(6378160,a,0);
  AssertEquals(1/298.25,f,0);
end;

procedure TTestEllipsoid.TestGRS67;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeGRS67,a,f);
  AssertEquals(6378160,a,0);
  AssertEquals(1/298.2471674273,f,0);
end;

procedure TTestEllipsoid.TestWGS72;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeWGS72,a,f);
  AssertEquals(6378135,a,0);
  AssertEquals(1/298.26,f,0);
end;

procedure TTestEllipsoid.TestIAU75;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeIAU75,a,f);
  AssertEquals(6378140,a,0);
  AssertEquals(1/298.256,f,0);
end;

procedure TTestEllipsoid.TestIAU76;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeIAU76,a,f);
  AssertEquals(6378140,a,0);
  AssertEquals(1/298.257,f,0);
end;

procedure TTestEllipsoid.TestGRS80;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeGRS80,a,f);
  AssertEquals(6378137,a,0);
  AssertEquals(1/298.257222101,f,0);
end;

procedure TTestEllipsoid.TestMERIT83;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeMERIT83,a,f);
  AssertEquals(6378137,a,0);
  AssertEquals(1/298.257,f,0);
end;

procedure TTestEllipsoid.TestWGS84;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeWGS84,a,f);
  AssertEquals(6378137,a,0);
  AssertEquals(1/298.257223563,f,0);
end;

procedure TTestEllipsoid.TestIERS1989;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeIERS1989,a,f);
  AssertEquals(6378136,a,0);
  AssertEquals(1/298.257,f,0);
end;

procedure TTestEllipsoid.TestIERS2003;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeIERS2003,a,f);
  AssertEquals(6378136.6,a,0);
  AssertEquals(1/298.25642,f,0);
end;

procedure TTestEllipsoid.TestDefaultEllipsoidValue;
var
  a1,f1, a2,f2: Double;
begin
  GetEarthEllipsoid(DefaultEarthEllipsoid,a1,f1);
  GetEarthEllipsoid(a2,f2);
  AssertEquals(a1,a2,0);
  AssertEquals(f1,f2,0);
end;

{ TTestGeodeticTransform }

procedure TTestGeodeticTransform.SetUp;
begin
  inherited SetUp;
  // using default ellipsoid
  GetEarthEllipsoid(DefaultEarthEllipsoid,a,f);
end;

procedure TTestGeodeticTransform.TestZeroLatLong;
var
  output: TPosition;
begin
  // at Lat=Long=Height=0, we have x=a and y=z=0
  output:= GeodeticToGeocentric(0,0,0,a,f);
  AssertEquals(a,output.X,0);
  AssertEquals(0,output.Y,0);
  AssertEquals(0,output.Z,0);
end;

procedure TTestGeodeticTransform.TestNorthPole;
var
  output: TPosition;
begin
  // at North pole (Lat=90º,Long=0º,Height=0, we have x=0,y=0 and z= a.(1-f)
  output:= GeodeticToGeocentric(Pi/2,0,0,a,f);
  // as sin(pi/2) doesn't return ZERO, we have to use 1e-9 as an error delta
  AssertEquals(0,output.X,1e-9);
  AssertEquals(0,output.Y,1e-9);
  AssertEquals(a*(1-f),output.Z,1e-9);
end;

procedure TTestGeodeticTransform.TestSomeCoordinate;
var
  output: TPosition;
  Lat,Long,Height,f_a,f_f,error: Double;
begin
  // using SOFA v20120301_a "t_gd2gc" testing values
  Lat:=-0.5;
  Long:=3.1;
  Height:=2500;
  f_a:=6378136.0;
  f_f:=0.0033528;
  error:= 1e-9;
  // call function
  output:= GeodeticToGeocentric(Lat,Long,Height,f_a,f_f);
  AssertEquals(-5598999.6665116328,output.X,error);
  AssertEquals(233011.63514630572,output.Y,error);
  AssertEquals(-3040909.0517314132,output.Z,error);
end;

procedure TTestGeodeticTransform.TestTEarthEllipsoidCalling;
var
  actual, expected: TPosition;
begin
  actual:= GeodeticToGeocentric(0,0,0,a,f);
  expected:= GeodeticToGeocentric(0,0,0,DefaultEarthEllipsoid);
  AssertEquals(expected.X,actual.X,0);
  AssertEquals(expected.Y,actual.Y,0);
  AssertEquals(expected.Z,actual.Z,0);
end;

procedure TTestGeodeticTransform.TestTEarthEllipsoidDefaultCalling;
var
  actual, expected: TPosition;
begin
  actual:= GeodeticToGeocentric(0,0,0,DefaultEarthEllipsoid);
  expected:= GeodeticToGeocentric(0,0,0);
  AssertEquals(expected.X,actual.X,0);
  AssertEquals(expected.Y,actual.Y,0);
  AssertEquals(expected.Z,actual.Z,0);
end;

initialization
  RegisterTest(TTestEllipsoid);
  RegisterTest(TTestGeodeticTransform);

end.

