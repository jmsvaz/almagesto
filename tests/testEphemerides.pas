unit testEphemerides;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestEPM08 }

  TTestEPM08= class(TTestCase)
  published
    procedure TestSunPosVel;
  end;

implementation

uses almEPMModel;

{ TTestEPM08 }

procedure TTestEPM08.TestSunPosVel;
var
  EPM08: TEPM08Model;
begin
  EPM08:= TEPM08Model.Create;
  try
    EPM08.SetTextPath(IncludeTrailingPathDelimiter(ParamStr(0)) + 'Data/EPM/DATA/008');
  finally
    EPM08.Free;
  end;

end;

initialization
  RegisterTest(TTestEPM08);

{

2451544.50 (TDB), EPM2008
Planets and Moon positions and velocities (AU, AU/day)
The numbering convention for 'PL_NUM' and 'CENTR_NUM':
                1 = MERCURY           8 = NEPTUNE
                2 = VENUS             9 = PLUTO
                3 = EARTH            10 = MOON
                4 = MARS             11 = SUN
                5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
                6 = SATURN           13 = EARTH-MOON BARYCENTER
                7 = URANUS

                Coordinates regard to  12
                1
                       -0.147867856165        -0.400628890603        -0.198914276961
                        0.021174245678        -0.005515502504        -0.005141098900
                2
                       -0.725769994855        -0.039668109791         0.027901450100
                        0.000518907042        -0.018515095630        -0.008362180731
                3
                       -0.175664427755         0.886198886248         0.384434608194
                       -0.017228571561        -0.002766250471        -0.001199380220
                4
                        1.383221288027        -0.008149842848        -0.041040073583
                        0.000753301367         0.013807159722         0.006312746508
                5
                        3.996320252937         2.730993212416         1.073274379210
                       -0.004558099208         0.005878007823         0.002630568481
                6
                        6.401414941385         6.170249311455         2.273028135864
                       -0.004285745445         0.003522771484         0.001639335139
                7
                       14.423379497080       -12.510142427262        -5.683128039056
                        0.002683753549         0.002455011824         0.001037270128
                8
                       16.803619692902       -22.983580667876        -9.825654895255
                        0.002584744055         0.001661542114         0.000615729419
                9
                       -9.884008347505       -27.980958360457        -5.753982664628
                        0.003034076134        -0.001134500048        -0.001268194489
                10
                       -0.177787788433         0.884618218353         0.384015643027
                       -0.016904689919        -0.003189750140        -0.001384021742
                11
                       -0.007139777986        -0.002644057234        -0.000921478148
                        0.000005374267        -0.000006761941        -0.000003034374
                12
                        0.000000000000         0.000000000000         0.000000000000
                        0.000000000000         0.000000000000         0.000000000000
                13
                       -0.175690227828         0.886179680209         0.384429517522
                       -0.017224636210        -0.002771396239        -0.001201623722
}

end.

