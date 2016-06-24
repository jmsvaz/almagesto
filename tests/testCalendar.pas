unit testCalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, almCalendar;

type

  { TTestFixedDateJulianDateConversion }

  TTestFixedDateJulianDateConversion= class(TTestCase)
  protected
    OldFixedDateEpochType: TFixedDateEpochType;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestJulianDateToFixedDateWhenFixedDateIsRataDie;
    procedure TestNegativeJulianDateToFixedDateWhenFixedDateIsRataDie;
    procedure TestJulianDateToNegativeFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToJulianDateWhenFixedDateIsRataDie;
    procedure TestNegativeFixedDateToJulianDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToNegativeJulianDateWhenFixedDateIsRataDie;
    procedure TestJulianDateToFixedDateWhenFixedDateisJulianDate;
    procedure TestFixedDateToJulianDateWhenFixedDateisJulianDate;
  end;

  { TTestFixedDateRataDieConversion }

  TTestFixedDateRataDieConversion= class(TTestCase)
  protected
    OldFixedDateEpochType: TFixedDateEpochType;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRataDieToFixedDateWhenFixedDateisJulianDate;
    procedure TestNegativeRataDieToFixedDateWhenFixedDateisJulianDate;
    procedure TestRataDieToNegativeFixedDateWhenFixedDateisJulianDate;
    procedure TestFixedDateToRataDieWhenFixedDateisJulianDate;
    procedure TestNegativeFixedDateToRataDieWhenFixedDateisJulianDate;
    procedure TestFixedDateToNegativeRataDieWhenFixedDateisJulianDate;
    procedure TestRataDieToFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToRataDieWhenFixedDateisRataDie;
  end;

  { TTestFixedDateDateTimeConversion }

  TTestFixedDateDateTimeConversion= class(TTestCase)
  protected
    OldFixedDateEpochType: TFixedDateEpochType;
    procedure SetUp; override;
    procedure TearDown; override;
  published
  end;

  { TTestJulianCalendar }

  TTestJulianCalendar= class(TTestCase)
  protected
    OldFixedDateEpochType: TFixedDateEpochType;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDateToNegativeFixedDateWhenFixedDateIsRataDie;
    procedure TestNegativeFixedDateToDateWhenFixedDateIsRataDie;
    procedure TestNegativeYearToFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToNegativeYearWhenFixedDateIsRataDie;
    procedure TestPositiveYearToFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToPositiveYearWhenFixedDateIsRataDie;
    procedure TestJulianCalendarEpochToFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToJulianCalendarEpochWhenFixedDateIsRataDie;
  end;

implementation

{ TTestJulianCalendar }

procedure TTestJulianCalendar.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestJulianCalendar.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestJulianCalendar.TestDateToNegativeFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193;
  AssertEquals(Expected,JulianCalendarToFixedDate(-586,7,30),0);
end;

procedure TTestJulianCalendar.TestNegativeFixedDateToDateWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToJulianCalendar(-214193,Year,Month,Day);
  AssertTrue((Year=-586) and (Month=7) and (Day=30));
end;

procedure TTestJulianCalendar.TestNegativeYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -61387;
  AssertEquals(Expected,JulianCalendarToFixedDate(-168,12,8),0);
end;

procedure TTestJulianCalendar.TestFixedDateToNegativeYearWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToJulianCalendar(-61387,Year,Month,Day);
  AssertTrue((Year=-168) and (Month=12) and (Day=8));
end;

procedure TTestJulianCalendar.TestPositiveYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 601716;
  AssertEquals(Expected,JulianCalendarToFixedDate(1648,5,31),0);
end;

procedure TTestJulianCalendar.TestFixedDateToPositiveYearWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToJulianCalendar(601716,Year,Month,Day);
  AssertTrue((Year=1648) and (Month=5) and (Day=31));
end;

procedure TTestJulianCalendar.TestJulianCalendarEpochToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -1;
  AssertEquals(Expected,JulianCalendarToFixedDate(1,1,1),0);
end;

procedure TTestJulianCalendar.TestFixedDateToJulianCalendarEpochWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToJulianCalendar(-1,Year,Month,Day);
  AssertTrue((Year=1) and (Month=1) and (Day=1));
end;

{ TTestFixedDateDateTimeConversion }

procedure TTestFixedDateDateTimeConversion.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestFixedDateDateTimeConversion.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;


{ TTestFixedDateRataDieConversion }

procedure TTestFixedDateRataDieConversion.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestFixedDateRataDieConversion.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestFixedDateRataDieConversion.TestRataDieToFixedDateWhenFixedDateisJulianDate;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeJulianDate;
  Expected:= 2229274.5;
  AssertEquals(Expected,RataDieToFixedDate(507850),0);
end;

procedure TTestFixedDateRataDieConversion.TestNegativeRataDieToFixedDateWhenFixedDateisJulianDate;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeJulianDate;
  Expected:= 1507231.5;
  AssertEquals(Expected,RataDieToFixedDate(-214193),0);
end;

procedure TTestFixedDateRataDieConversion.TestRataDieToNegativeFixedDateWhenFixedDateisJulianDate;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeJulianDate;
  Expected:= -37;
  AssertEquals(Expected,RataDieToFixedDate(-1721461.5),0);
end;

procedure TTestFixedDateRataDieConversion.TestRataDieToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 214193;
  AssertEquals(Expected,RataDieToFixedDate(Expected),0);
end;

procedure TTestFixedDateRataDieConversion.TestFixedDateToRataDieWhenFixedDateisRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 214193;
  AssertEquals(Expected,FixedDateToRataDie(Expected),0);
end;

procedure TTestFixedDateRataDieConversion.TestFixedDateToRataDieWhenFixedDateisJulianDate;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeJulianDate;
  Expected:= 507850;
  AssertEquals(Expected,FixedDateToRataDie(2229274.5),0);
end;

procedure TTestFixedDateRataDieConversion.TestNegativeFixedDateToRataDieWhenFixedDateisJulianDate;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeJulianDate;
  Expected:= -1721461.5;
  AssertEquals(Expected,FixedDateToRataDie(-37),0);
end;

procedure TTestFixedDateRataDieConversion.TestFixedDateToNegativeRataDieWhenFixedDateisJulianDate;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeJulianDate;
  Expected:= -214193;
  AssertEquals(Expected,FixedDateToRataDie(1507231.5),0);
end;

{ TTestFixedDateJulianDateConversion }

procedure TTestFixedDateJulianDateConversion.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestFixedDateJulianDateConversion.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestFixedDateJulianDateConversion.TestJulianDateToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 507850;
  AssertEquals(Expected,JulianDateToFixedDate(2229274.5),0);
end;

procedure TTestFixedDateJulianDateConversion.TestNegativeJulianDateToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -1721461.5;
  AssertEquals(Expected,JulianDateToFixedDate(-37),0);
end;

procedure TTestFixedDateJulianDateConversion.TestJulianDateToNegativeFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193;
  AssertEquals(Expected,JulianDateToFixedDate(1507231.5),0);
end;

procedure TTestFixedDateJulianDateConversion.TestJulianDateToFixedDateWhenFixedDateisJulianDate;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeJulianDate;
  Expected:= 214193;
  AssertEquals(Expected,JulianDateToFixedDate(Expected),0);
end;

procedure TTestFixedDateJulianDateConversion.TestFixedDateToJulianDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 2229274.5;
  AssertEquals(Expected,FixedDateToJulianDate(507850),0);
end;

procedure TTestFixedDateJulianDateConversion.TestNegativeFixedDateToJulianDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 1507231.5;
  AssertEquals(Expected,FixedDateToJulianDate(-214193),0);
end;

procedure TTestFixedDateJulianDateConversion.TestFixedDateToNegativeJulianDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -37;
  AssertEquals(Expected,FixedDateToJulianDate(-1721461.5),0);
end;

procedure TTestFixedDateJulianDateConversion.TestFixedDateToJulianDateWhenFixedDateisJulianDate;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeJulianDate;
  Expected:= 2229274.5;
  AssertEquals(Expected,FixedDateToJulianDate(Expected),0);
end;


initialization
  RegisterTest(TTestFixedDateJulianDateConversion);
  RegisterTest(TTestFixedDateRataDieConversion);
  RegisterTest(TTestFixedDateDateTimeConversion);
  RegisterTest(TTestJulianCalendar);

end.

