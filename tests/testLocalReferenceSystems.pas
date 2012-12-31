unit testLocalReferenceSystems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestEllipsoid }

  TTestEllipsoid= class(TTestCase)
  published
    procedure TestWGS60;
    procedure TestWGS66;
    procedure TestGRS67;
    procedure TestWGS72;
    procedure TestGRS80;
    procedure TestMERIT83;
    procedure TestWGS84;
    procedure TestIERS1989;
    procedure TestIERS2003;
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
  end;

implementation

uses almBase, almLocalReferenceSystems;

{ TTestEllipsoid }

procedure TTestEllipsoid.TestWGS60;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeWGS60,a,f);
  AssertEquals(6378165,a,0);
  AssertEquals(1/298.3,f,0);
end;

procedure TTestEllipsoid.TestWGS66;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeWGS66,a,f);
  AssertEquals(6378145,a,0);
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

{ TTestGeodeticTransform }

procedure TTestGeodeticTransform.SetUp;
begin
  inherited SetUp;
  // using WGS84 ellipsoid
  a:= 6378137;
  f:= 1/298.257223563;
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
  // at North pole (Lat=90º,Long=Height=0, we have x=y=0 and z= a.(1-f)
  output:= GeodeticToGeocentric(Pi/2,0,0,a,f);
  // as we compute sin(pi/2), we have to use 1e-9 as error
  AssertEquals(0,output.X,1e-9);
  AssertEquals(0,output.Y,1e-9);
  AssertEquals(a*(1-f),output.Z,1e-9);
end;

initialization
  RegisterTest(TTestEllipsoid);
  RegisterTest(TTestGeodeticTransform);

end.

