unit testLocalReferenceSystems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestEllipsoid }

  TTestEllipsoid= class(TTestCase)
  published
    procedure TestWGS84;
  end;

{ TTestGeodetic }

  TTestGeodetic= class(TTestCase)
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

procedure TTestEllipsoid.TestWGS84;
var
  a,f: Double;
begin
  GetEarthEllipsoid(eeWGS84,a,f);
  AssertEquals(6378137,a,0);
  AssertEquals(1/298.257223563,f,0);
end;

{ TTestGeodetic }

procedure TTestGeodetic.SetUp;
begin
  inherited SetUp;
  // using WGS84 ellipsoid
  a:= 6378137;
  f:= 1/298.257223563;
end;

procedure TTestGeodetic.TestZeroLatLong;
var
  output: TPosition;
begin
  // at Lat=Long=Height=0, we have x=a and y=z=0
  output:= GeodeticToGeocentric(0,0,0,a,f);
  AssertEquals(a,output.X,0);
  AssertEquals(0,output.Y,0);
  AssertEquals(0,output.Z,0);
end;

procedure TTestGeodetic.TestNorthPole;
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
  RegisterTest(TTestGeodetic);

end.

