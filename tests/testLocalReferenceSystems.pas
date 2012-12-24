unit testLocalReferenceSystems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

{ TTestGeodetic }

  TTestGeodetic= class(TTestCase)
  published
    procedure TestZeroLatLong;
  end;

implementation

uses almBase, almLocalReferenceSystems;

{ TTestGeodetic }

procedure TTestGeodetic.TestZeroLatLong;
var
  output: TPosition;
begin

end;

initialization
//  RegisterTest(TTestNativeTime);
  RegisterTest(TTestGeodetic);

end.

