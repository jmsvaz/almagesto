unit testCalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestFixedDateConversion }

  TTestFixedDateConversion= class(TTestCase)
  published
    procedure TestJulianDateToFixedDate;
    procedure TestFixedDateToJulianDate;
    procedure TestRataDieToFixedDate;
    procedure TestFixedDateToRataDie;
    procedure TestDateTimeToFixedDate;
    procedure TestFixedDateToDateTime;
  end;

implementation

uses almCalendar;

{ TTestFixedDateConversion }

procedure TTestFixedDateConversion.TestJulianDateToFixedDate;
var
  FD,Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193;
  FD:= JulianDateToFixedDate(1507231.5);
  AssertEquals(Expected,FD,0);
  Expected:= 507850;
  FD:= JulianDateToFixedDate(2229274.5);
  AssertEquals(Expected,FD,0);
  FixedDateEpochType:= fdeJulianDate;
end;

procedure TTestFixedDateConversion.TestFixedDateToJulianDate;
var
  JD,Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 1507231.5;
  JD:= FixedDateToJulianDate(-214193);
  AssertEquals(Expected,JD,0);
  Expected:= 2229274.5;
  JD:= FixedDateToJulianDate(507850);
  AssertEquals(Expected,JD,0);
  FixedDateEpochType:= fdeJulianDate;
end;

procedure TTestFixedDateConversion.TestRataDieToFixedDate;
var
  FD,Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193;
  FD:= RataDieToFixedDate(Expected);
  AssertEquals(Expected,FD,0);
  FixedDateEpochType:= fdeJulianDate;
  Expected:= 507850;
  FD:= RataDieToFixedDate(2229274.5);
  AssertEquals(Expected,FD,0);
end;

procedure TTestFixedDateConversion.TestFixedDateToRataDie;
var
  RD,Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 1507231.5;
  RD:= FixedDateToRataDie(Expected);
  AssertEquals(Expected,RD,0);
  FixedDateEpochType:= fdeJulianDate;
  Expected:= 2229274.5;
  RD:= FixedDateToRataDie(507850);
  AssertEquals(Expected,RD,0);

end;

procedure TTestFixedDateConversion.TestDateTimeToFixedDate;
var
  FD,Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193;
  FD:= DateTimeToFixedDate(Expected);
  AssertEquals(Expected,FD,0);
  FixedDateEpochType:= fdeJulianDate;
  Expected:= 507850;
  FD:= DateTimeToFixedDate(2229274.5);
  AssertEquals(Expected,FD,0);
end;

procedure TTestFixedDateConversion.TestFixedDateToDateTime;
var
  DT,Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 1507231.5;
  DT:= FixedDateToDateTime(Expected);
  AssertEquals(Expected,DT,0);
end;

initialization
  RegisterTest(TTestFixedDateConversion);

end.

