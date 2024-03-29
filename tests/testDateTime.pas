unit testDateTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestNativeTime }

  TTestNativeTime= class(TTestCase)
  published
    procedure TestJulianDateConversion;
    procedure TestTLeapSecondOnTDateTime;
  end;

  { TTestTimeConversion }

  TTestTimeConversion= class(TTestCase)
  private
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestStandardTimeConversion;
    procedure TestLocalMeanTimeConversion;
    procedure TestJulianDateConversion;
    procedure TestJulianEpochToJulianDDate;
    procedure TestJulianDateToJulianEpoch;
    procedure TestBesselianEpochToJulianDDate;
    procedure TestJulianDateToBesselianEpoch;
  end;

  { TTestTimeDeltas }

  TTestTimeDeltas = class(TTestCase)
  published
    procedure Test1977Origins;
    procedure TestDeltaTAI;
    procedure TestDeltaTCG;
    procedure TestDeltaTCB; 
    procedure TestDeltaAT1;
    procedure TestDeltaAT2;
    procedure TestDeltaAT3;
  end;

  { TTestTimeScalesClass }
  TTestTimeScalesClass = class(TTestCase)
  published
    procedure TestSOFAValues;
  end;




implementation

uses almBase, almDateTime, almUnits;

procedure TTestTimeConversion.TestStandardTimeConversion;
var
  StandardTime, UTC, Expected: TJulianDate;
begin
  StandardTime:= Now;
  Expected:= StandardTime - (1 - 3)/24;
  UTC:= StandardTimeToUTC(StandardTime, -3, 1);
  AssertEquals(Expected,UTC,0);
end;

procedure TTestTimeConversion.TestLocalMeanTimeConversion;
var
  LocalMeanTime, UT, Expected: TJulianDate;
begin
  LocalMeanTime:= Now;
  Expected:= LocalMeanTime - (-75)/360;
  UT:= LocalMeanTimeToUniversalTime(LocalMeanTime, -75);
  AssertEquals(Expected,UT,0);
end;

procedure TTestTimeConversion.TestJulianDateConversion;
var
  LocalTime: TDateTime;
  JD, Expected: TJulianDate;
begin
  // at 2000-01-01 midnight, JD is 2451544.5
  LocalTime:= EncodeDate(2000,1,1);
  Expected:= 2451544.5;
  JD:= DateTimeToJulianDate(LocalTime);
  AssertEquals(Expected,JD,0);
end;

procedure TTestTimeConversion.TestJulianEpochToJulianDDate;
var
  JulianEpoch, Computed, Expected: TJulianDate;
begin
  // This function tests Julian Epoch conversion from JulianEpochToJulianDate
  // Test values from IAU SOFA C version 2020-07-21 Release
  JulianEpoch:= 1996.8;
  Expected:= 2400000.5 + 50375.7;
  Computed:= JulianEpochToJulianDate(JulianEpoch);
  AssertEquals(Expected,Computed,1e-9);
end;

procedure TTestTimeConversion.TestJulianDateToJulianEpoch;
var
  JD, Computed, Expected: TJulianDate;
begin
  // This function tests Julian Epoch conversion from JulianDateToJulianEpoch
  // Test values from IAU SOFA C version 2020-07-21 Release
  JD:= 2451545 -7392.5;
  Expected:= 1979.760438056125941;
  Computed:= JulianDateToJulianEpoch(JD);
  AssertEquals(Expected,Computed,1e-12);
end;

procedure TTestTimeConversion.TestBesselianEpochToJulianDDate;
var
  BesselianEpoch, Computed, Expected: TJulianDate;
begin
  // This function tests Besselian Epoch conversion from BesselianEpochToJulianDate
  // Test values from IAU SOFA C version 2020-07-21 Release
  BesselianEpoch:= 1957.3;
  Expected:= 2400000.5 + 35948.1915101513;
  Computed:= BesselianEpochToJulianDate(BesselianEpoch);
  AssertEquals(Expected,Computed,1e-9);
end;

procedure TTestTimeConversion.TestJulianDateToBesselianEpoch;
var
  JD, Computed, Expected: TJulianDate;
begin
  // This function tests Besselian Epoch conversion from JulianDateToBesselianEpoch
  // Test values from IAU SOFA C version 2020-07-21 Release
  JD:= 2415019.8135 + 30103.18648;
  Expected:= 1982.418424159278580;
  Computed:= JulianDateToBesselianEpoch(JD);
  AssertEquals(Expected,Computed,1e-12);

end;


procedure TTestTimeConversion.SetUp;
begin

end; 

procedure TTestTimeConversion.TearDown;
begin

end; 

{ TTestTimeDeltas }

procedure TTestTimeDeltas.Test1977Origins;
begin
  // at 1977 Jan 1.0 TAI: TT, TCG and TCB are sincronized and are called T0
  AssertEquals(0,DeltaTCG(T0),1e-9);
  AssertEquals(6.55E-5,DeltaTCB(T0),1e-9);
  AssertEquals(-6.55E-5,DeltaTDB_FB2001(T0,0),1e-6);
end;

procedure TTestTimeDeltas.TestDeltaTAI;
begin
  // DeltaTAI is always this fixed value
  AssertEquals(32.184,DeltaTAI,0);
end;

procedure TTestTimeDeltas.TestDeltaTCG;
var
  JD, TTFrac, TCGFrac, Computed, Expected: TJulianDate;
begin
  // This function tests DeltaTCG = TCG - TT (seconds)
  // Test values from IAU SOFA C version 2010-12-01 Release
  JD:= 2453750.5;
  TTFrac:=  0.892482639;
  TCGFrac:= 0.8924900312508587113;
  Expected:= Convert((TCGFrac - TTFrac),cDays,cSeconds);
  Computed:= DeltaTCG(JD + TTFrac);
  AssertEquals(Expected,Computed,1e-9);
end;

procedure TTestTimeDeltas.TestDeltaTCB;
var
  JD, TCBFrac, TDBFrac, Computed, Expected: TJulianDate;
begin
  // This function tests DeltaTCB = TCB - TDB (seconds)
  // Test values from IAU SOFA C version 2010-12-01 Release
  JD:= 2453750.5;
  TCBFrac:= 0.893019599;
  TDBFrac:= 0.8928551362746343397;
  Expected:= Convert((TCBFrac - TDBFrac),cDays,cSeconds);
  Computed:= DeltaTCB(JD + TCBFrac);
  AssertEquals(Expected,Computed,1e-9);
end;

procedure TTestTimeDeltas.TestDeltaAT1;
var
  JD, Computed, Expected: TJulianDate;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release
  JD:= DateTimeToJulianDate(Encodedate(2003, 6, 1));
  Expected:= (32.0);
  Computed:= DeltaAT(JD);
  AssertEquals(Expected,Computed,0);
end;

procedure TTestTimeDeltas.TestDeltaAT2;
var
  JD, Computed, Expected: TJulianDate;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release
  JD:= DateTimeToJulianDate(Encodedate(2008, 1, 17));
  Expected:= (33.0);
  Computed:= DeltaAT(JD);
  AssertEquals(Expected,Computed,0);
end;

procedure TTestTimeDeltas.TestDeltaAT3;
var
  JD, Computed, Expected: TJulianDate;
begin
  // Test values from IAU SOFA C version 2020-07-21 Release
  JD:= DateTimeToJulianDate(Encodedate(2017, 9, 1));
  Expected:= (37.0);
  Computed:= DeltaAT(JD);
  AssertEquals(Expected,Computed,0);
end;


{ TTestNativeTime }

procedure TTestNativeTime.TestJulianDateConversion;
var
  LocalTime: TDateTime;
  JD, Expected: TJulianDate;
begin
  // at January 1, 4713 BC Greenwich noon, JD is 0
  LocalTime:= ComposeDateTime(EncodeDate(-4712,1,1),EncodeTime(12,0,0,0)); // <- not working!! Only Year > 0
  Expected:= 0;
  JD:= DateTimeToJulianDate(LocalTime);
  AssertEquals(Expected,JD,0);
end;

procedure TTestNativeTime.TestTLeapSecondOnTDateTime;
var
  dt1, dt2: TDateTime;
begin
  // we had a Leap Second on 2008-12-31, so that we have the UTC time 23:59:60
  dt1:= ComposeDateTime(EncodeDate(2008,12,31),EncodeTime(23,59,59,0));
  dt2:= ComposeDateTime(EncodeDate(2008,12,31),EncodeTime(23,59,60,0));
  AssertTrue(dt1 - dt2 <> 0);
  dt1:= ComposeDateTime(EncodeDate(2009,1,1),EncodeTime(0,0,0,0));
  AssertTrue(dt1 - dt2 <> 0);
end;

{ TTestTimeScalesClass }

procedure TTestTimeScalesClass.TestSOFAValues;
var
  dt: TDateTime;
  ts: TTimeScales;
begin
  {
  at the SOFA Time Scales and Calendar Tools 2010-08-27 cookbook, we have the
  following test case:
  input
    Lat: 19:28:52.5
    Long: -155:55:59.6
    H: 0 m
    UTC: 2006-01-15 21:24:37.5
    dut: 0.3341 s
  output
    UTC: 2006/01/15 21:24:37.500000
    UT1: 2006/01/15 21:24:37.834100
    TAI: 2006/01/15 21:25:10.500000
    TT : 2006/01/15 21:25:42.684000
    TCG: 2006/01/15 21:25:43.322690
    TDB: 2006/01/15 21:25:42.683799
    TCB: 2006/01/15 21:25:56.893378
  }
  //TODO: use another Time conversion routine to update millisecond resolution
  dt:= ComposeDateTime(EncodeDate(2006,1,15),EncodeTime(21,24,35,500));
  ts:= TTimeScales.Create(DateTimeToJulianDate(dt),0.3341);
  // testing UT1
  dt:= ComposeDateTime(EncodeDate(2006,1,15),EncodeTime(21,24,35,834));
  AssertEquals(dt,JulianDateToDateTime(ts.UT1),1e-9);
  // testing TAI
  dt:= ComposeDateTime(EncodeDate(2006,1,15),EncodeTime(21,25,10,500));
  AssertEquals(dt,JulianDateToDateTime(ts.TAI),1e-9);
end;

initialization
//  RegisterTest(TTestNativeTime);
  RegisterTest(TTestTimeConversion);
  RegisterTest(TTestTimeDeltas);
//  RegisterTest(TTestTimeScalesClass);

end.

