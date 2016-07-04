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
    procedure TestDateTimeToFixedDateWhenFixedDateIsRataDie;
    procedure TestNegativeDateTimeToFixedDateWhenFixedDateIsRataDie;
    procedure TestDateTimeToNegativeFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToDateTimeWhenFixedDateIsRataDie;
    procedure TestNegativeFixedDateToDateTimeWhenFixedDateIsRataDie;
    procedure TestFixedDateToNegativeDateTimeWhenFixedDateIsRataDie;
    procedure TestDateTimeToFixedDateWhenFixedDateisDateTime;
    procedure TestFixedDateToDateTimeWhenFixedDateisDateTime;
  end;

  { TTestWeekDay }

  TTestWeekDay= class(TTestCase)
  protected
    OldFixedDateEpochType: TFixedDateEpochType;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NegativeFixedDateWhenFixedDateIsRataDie;
    procedure PositiveFixedDateWhenFixedDateIsRataDie;
    procedure PositiveFixedDateWhenFixedDateIsJulianDate;
    procedure PositiveFixedDateWhenFixedDateIsDateTime;
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

  { TTestGregorianCalendar }

  TTestGregorianCalendar= class(TTestCase)
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
    procedure TestGregorianCalendarEpochToFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToGregorianCalendarEpochWhenFixedDateIsRataDie;
  end;

  { TTestEgyptianCalendar }

  TTestEgyptianCalendar= class(TTestCase)
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
    procedure TestCalendarEpochToFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToCalendarEpochWhenFixedDateIsRataDie;
  end;

  { TTestArmenianCalendar }

  TTestArmenianCalendar= class(TTestCase)
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
    procedure TestCalendarEpochToFixedDateWhenFixedDateIsRataDie;
    procedure TestFixedDateToCalendarEpochWhenFixedDateIsRataDie;
  end;



  { TTestMayanCalendar }

  TTestMayanCalendar = class(TTestCase)
  protected
    OldFixedDateEpochType: TFixedDateEpochType;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLongCountToNegativeFixedDateWhenFixedDateIsRataDie;
    procedure TestLongCountToPositiveFixedDateWhenFixedDateIsRataDie;
    procedure TestNegativeFixedDateToLongCountWhenFixedDateIsRataDie;
    procedure TestPositiveFixedDateToLongCountWhenFixedDateIsRataDie;
    procedure TestNegativeFixedDateToHaabWhenFixedDateIsRataDie;
    procedure TestPositiveFixedDateToHaabWhenFixedDateIsRataDie;
    procedure TestNegativeFixedDateToTzolkinWhenFixedDateIsRataDie;
    procedure TestPositiveFixedDateToTzolkinWhenFixedDateIsRataDie;
  end;

  { TTestAztecCalendar }

  TTestAztecCalendar = class(TTestCase)
  protected
    OldFixedDateEpochType: TFixedDateEpochType;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNegativeFixedDateToXihuitlWhenFixedDateIsRataDie;
    procedure TestPositiveFixedDateToXihuitlWhenFixedDateIsRataDie;
    procedure TestNegativeFixedDateToTonalpohualliWhenFixedDateIsRataDie;
    procedure TestPositiveFixedDateToTonalpohualliWhenFixedDateIsRataDie;
  end;


implementation

{ TTestArmenianCalendar }

procedure TTestArmenianCalendar.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestArmenianCalendar.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestArmenianCalendar.TestDateToNegativeFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193 + 6/24;
  AssertEquals(Expected,ArmenianCalendarToFixedDate(-1138,4,10),0);
end;

procedure TTestArmenianCalendar.TestNegativeFixedDateToDateWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToArmenianCalendar(-214193 + 6/24,Year,Month,Day);
  AssertTrue((Year=-1138) and (Month=4) and (Day=10));
end;

procedure TTestArmenianCalendar.TestNegativeYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 49217 + 6/24;
  AssertEquals(Expected,ArmenianCalendarToFixedDate(-417,12,15),0);
end;

procedure TTestArmenianCalendar.TestFixedDateToNegativeYearWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToArmenianCalendar(49217 + 6/24,Year,Month,Day);
  AssertTrue((Year=-417) and (Month=12) and (Day=15));
end;

procedure TTestArmenianCalendar.TestPositiveYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 601716 + 6/24;
  AssertEquals(Expected,ArmenianCalendarToFixedDate(1097,8,24),0);
end;

procedure TTestArmenianCalendar.TestFixedDateToPositiveYearWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToArmenianCalendar(601716 + 6/24,Year,Month,Day);
  AssertTrue((Year=1097) and (Month=8) and (Day=24));
end;

procedure TTestArmenianCalendar.TestCalendarEpochToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= ArmenianCalendarEpoch;
  AssertEquals(Expected,ArmenianCalendarToFixedDate(1,1,1),0);
end;

procedure TTestArmenianCalendar.TestFixedDateToCalendarEpochWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToArmenianCalendar(ArmenianCalendarEpoch,Year,Month,Day);
  AssertTrue((Year=1) and (Month=1) and (Day=1));
end;

{ TTestEgyptianCalendar }

procedure TTestEgyptianCalendar.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestEgyptianCalendar.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestEgyptianCalendar.TestDateToNegativeFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193 + 6/24;
  AssertEquals(Expected,EgyptianCalendarToFixedDate(161,7,15),0);
end;

procedure TTestEgyptianCalendar.TestNegativeFixedDateToDateWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToEgyptianCalendar(-214193 + 6/24,Year,Month,Day);
  AssertTrue((Year=161) and (Month=7) and (Day=15));
end;

procedure TTestEgyptianCalendar.TestNegativeYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -330415 + 6/24;
  AssertEquals(Expected,EgyptianCalendarToFixedDate(-157,2,13),0);
end;

procedure TTestEgyptianCalendar.TestFixedDateToNegativeYearWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToEgyptianCalendar(-330415 + 6/24,Year,Month,Day);
  AssertTrue((Year=-157) and (Month=2) and (Day=13));
end;

procedure TTestEgyptianCalendar.TestPositiveYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 601716 + 6/24;
  AssertEquals(Expected,EgyptianCalendarToFixedDate(2396,11,29),0);
end;

procedure TTestEgyptianCalendar.TestFixedDateToPositiveYearWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToEgyptianCalendar(601716 + 6/24,Year,Month,Day);
  AssertTrue((Year=2396) and (Month=11) and (Day=29));
end;

procedure TTestEgyptianCalendar.TestCalendarEpochToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= EgyptianCalendarEpoch;
  AssertEquals(Expected,EgyptianCalendarToFixedDate(1,1,1),0);
end;

procedure TTestEgyptianCalendar.TestFixedDateToCalendarEpochWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToEgyptianCalendar(EgyptianCalendarEpoch,Year,Month,Day);
  AssertTrue((Year=1) and (Month=1) and (Day=1));
end;

{ TTestAztecCalendar }

procedure TTestAztecCalendar.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestAztecCalendar.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestAztecCalendar.TestNegativeFixedDateToXihuitlWhenFixedDateIsRataDie;
var
  Day, Month: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToAztecXihuitl(-214193,Day, Month);
  AssertTrue((Month=2) and (Day=6));
end;

procedure TTestAztecCalendar.TestPositiveFixedDateToXihuitlWhenFixedDateIsRataDie;
var
  Day, Month: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToAztecXihuitl(601716,Day, Month);
  AssertTrue((Month=8) and (Day=20));
end;

procedure TTestAztecCalendar.TestNegativeFixedDateToTonalpohualliWhenFixedDateIsRataDie;
var
  Number, Name: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToAztecTonalpohualli(-214193,Number, Name);
  AssertTrue((Number=5) and (Name=9));
end;

procedure TTestAztecCalendar.TestPositiveFixedDateToTonalpohualliWhenFixedDateIsRataDie;
var
  Number, Name: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToAztecTonalpohualli(601716,Number, Name);
  AssertTrue((Number=8) and (Name=18));
end;

{ TTestWeekDay }

procedure TTestWeekDay.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestWeekDay.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestWeekDay.NegativeFixedDateWhenFixedDateIsRataDie;
var
  Expected: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 0;
  AssertEquals(Expected,DayOfWeekFromFixed(-214193));
end;

procedure TTestWeekDay.PositiveFixedDateWhenFixedDateIsRataDie;
const
  Wednesday = 3;
begin
  FixedDateEpochType:= fdeRataDie;
  AssertEquals(Wednesday,DayOfWeekFromFixed(601716));
end;

procedure TTestWeekDay.PositiveFixedDateWhenFixedDateIsJulianDate;
const
  Wednesday = 3;
begin
  FixedDateEpochType:= fdeJulianDate;
  AssertEquals(Wednesday,DayOfWeekFromFixed(2323140.5));
end;

procedure TTestWeekDay.PositiveFixedDateWhenFixedDateIsDateTime;
const
  Saturday = 6;
begin
  FixedDateEpochType:= fdeDateTime;
  AssertEquals(Saturday,DayOfWeekFromFixed(EncodeDate(2016,6,25)));
end;

{ TTestMayanCalendar }

procedure TTestMayanCalendar.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestMayanCalendar.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestMayanCalendar.TestLongCountToNegativeFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193;
  AssertEquals(Expected,MayanLongCountToFixedDate(6,8,3,13,9),0);
end;

procedure TTestMayanCalendar.TestLongCountToPositiveFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 601716;
  AssertEquals(Expected,MayanLongCountToFixedDate(12,1,10,2,18),0);
end;

procedure TTestMayanCalendar.TestNegativeFixedDateToLongCountWhenFixedDateIsRataDie;
var
  Baktun, Katun, Tun, Uinal, Kin: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToMayanLongCount(-214193,Baktun, Katun, Tun, Uinal, Kin);
  AssertTrue((Baktun=6) and (Katun=8) and (Tun=3) and (Uinal=13) and (Kin=9));
end;

procedure TTestMayanCalendar.TestPositiveFixedDateToLongCountWhenFixedDateIsRataDie;
var
  Baktun, Katun, Tun, Uinal, Kin: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToMayanLongCount(601716,Baktun, Katun, Tun, Uinal, Kin);
  AssertTrue((Baktun=12) and (Katun=1) and (Tun=10) and (Uinal=2) and (Kin=18));
end;

procedure TTestMayanCalendar.TestNegativeFixedDateToHaabWhenFixedDateIsRataDie;
var
  Day, Month: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToMayanHaab(-214193,Day, Month);
  AssertTrue((Month=11) and (Day=12));
end;

procedure TTestMayanCalendar.TestPositiveFixedDateToHaabWhenFixedDateIsRataDie;
var
  Day, Month: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToMayanHaab(601716,Day, Month);
  AssertTrue((Month=18) and (Day=6));
end;

procedure TTestMayanCalendar.TestNegativeFixedDateToTzolkinWhenFixedDateIsRataDie;
var
  Number, Name: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToMayanTzolkin(-214193,Number, Name);
  AssertTrue((Number=5) and (Name=9));
end;

procedure TTestMayanCalendar.TestPositiveFixedDateToTzolkinWhenFixedDateIsRataDie;
var
  Number, Name: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToMayanTzolkin(601716,Number, Name);
  AssertTrue((Number=8) and (Name=18));
end;

{ TTestGregorianCalendar }

procedure TTestGregorianCalendar.SetUp;
begin
  inherited SetUp;
  OldFixedDateEpochType:= FixedDateEpochType;
end;

procedure TTestGregorianCalendar.TearDown;
begin
  FixedDateEpochType:= OldFixedDateEpochType;
  inherited TearDown;
end;

procedure TTestGregorianCalendar.TestDateToNegativeFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193;
  AssertEquals(Expected,GregorianCalendarToFixedDate(-586,7,24),0);
end;

procedure TTestGregorianCalendar.TestNegativeFixedDateToDateWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToGregorianCalendar(-214193,Year,Month,Day);
  AssertTrue((Year=-586) and (Month=7) and (Day=24));
end;

procedure TTestGregorianCalendar.TestNegativeYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -61387;
  AssertEquals(Expected,GregorianCalendarToFixedDate(-168,12,5),0);
end;

procedure TTestGregorianCalendar.TestFixedDateToNegativeYearWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToGregorianCalendar(-61387,Year,Month,Day);
  AssertTrue((Year=-168) and (Month=12) and (Day=5));
end;

procedure TTestGregorianCalendar.TestPositiveYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 601716;
  AssertEquals(Expected,GregorianCalendarToFixedDate(1648,6,10),0);
end;

procedure TTestGregorianCalendar.TestFixedDateToPositiveYearWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToGregorianCalendar(601716,Year,Month,Day);
  AssertTrue((Year=1648) and (Month=6) and (Day=10));
end;

procedure TTestGregorianCalendar.TestGregorianCalendarEpochToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= GregorianCalendarEpoch;
  AssertEquals(Expected,GregorianCalendarToFixedDate(1,1,1),0);
end;

procedure TTestGregorianCalendar.TestFixedDateToGregorianCalendarEpochWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToGregorianCalendar(GregorianCalendarEpoch,Year,Month,Day);
  AssertTrue((Year=1) and (Month=1) and (Day=1));
end;

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
  AssertEquals('Year-Month-Day Nomenclature',Expected,JulianCalendarToFixedDate(-586,7,30),0);
  AssertEquals('Roman Nomenclature',Expected,JulianCalendarToFixedDate(-586,8,1,3,False),0);
end;

procedure TTestJulianCalendar.TestNegativeFixedDateToDateWhenFixedDateIsRataDie;
var
  Year,Month,Day,Event,Count: Integer;
  Leap: Boolean;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToJulianCalendar(-214193,Year,Month,Day);
  AssertTrue('Year-Month-Day Nomenclature',(Year=-586) and (Month=7) and (Day=30));
  FixedDateToJulianCalendar(-214193,Year,Month,Event,Count,Leap);
  AssertTrue('Roman Nomenclature',(Year=-586) and (Month=8) and (Event=1) and (Count=3) and (Leap=False));
end;

procedure TTestJulianCalendar.TestNegativeYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -61387;
  AssertEquals('Year-Month-Day Nomenclature',Expected,JulianCalendarToFixedDate(-168,12,8),0);
  AssertEquals('Roman Nomenclature',Expected,JulianCalendarToFixedDate(-168,12,3,6,False),0);
end;

procedure TTestJulianCalendar.TestFixedDateToNegativeYearWhenFixedDateIsRataDie;
var
  Year,Month,Day,Event,Count: Integer;
  Leap: Boolean;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToJulianCalendar(-61387,Year,Month,Day);
  AssertTrue('Year-Month-Day Nomenclature',(Year=-168) and (Month=12) and (Day=8));
  FixedDateToJulianCalendar(-61387,Year,Month,Event,Count,Leap);
  AssertTrue('Roman Nomenclature',(Year=-168) and (Month=12) and (Event=3) and (Count=6) and (Leap=False));
end;

procedure TTestJulianCalendar.TestPositiveYearToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 601716;
  AssertEquals('Year-Month-Day Nomenclature',Expected,JulianCalendarToFixedDate(1648,5,31),0);
  AssertEquals('Roman Nomenclature',Expected,JulianCalendarToFixedDate(1648,6,1,2,False),0);
end;

procedure TTestJulianCalendar.TestFixedDateToPositiveYearWhenFixedDateIsRataDie;
var
  Year,Month,Day,Event,Count: Integer;
  Leap: Boolean;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToJulianCalendar(601716,Year,Month,Day);
  AssertTrue('Year-Month-Day Nomenclature',(Year=1648) and (Month=5) and (Day=31));
  FixedDateToJulianCalendar(601716,Year,Month,Event,Count,Leap);
  AssertTrue('Roman Nomenclature',(Year=1648) and (Month=6) and (Event=1) and (Count=2) and (Leap=False));
end;

procedure TTestJulianCalendar.TestJulianCalendarEpochToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= JulianCalendarEpoch;
  AssertEquals(Expected,JulianCalendarToFixedDate(1,1,1),0);
end;

procedure TTestJulianCalendar.TestFixedDateToJulianCalendarEpochWhenFixedDateIsRataDie;
var
  Year,Month,Day: Integer;
begin
  FixedDateEpochType:= fdeRataDie;
  FixedDateToJulianCalendar(JulianCalendarEpoch,Year,Month,Day);
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

procedure TTestFixedDateDateTimeConversion.TestDateTimeToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 708842;
  AssertEquals(Expected,DateTimeToFixedDate(15248),0);
end;

procedure TTestFixedDateDateTimeConversion.TestNegativeDateTimeToFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 544676;
  AssertEquals(Expected,DateTimeToFixedDate(-148918),0);
end;

procedure TTestFixedDateDateTimeConversion.TestDateTimeToNegativeFixedDateWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -214193;
  AssertEquals(Expected,DateTimeToFixedDate(-907787),0);
end;

procedure TTestFixedDateDateTimeConversion.TestFixedDateToDateTimeWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= 15248;
  AssertEquals(Expected,FixedDateToDateTime(708842),0);
end;

procedure TTestFixedDateDateTimeConversion.TestNegativeFixedDateToDateTimeWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -907787;
  AssertEquals(Expected,FixedDateToDateTime(-214193),0);
end;

procedure TTestFixedDateDateTimeConversion.TestFixedDateToNegativeDateTimeWhenFixedDateIsRataDie;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeRataDie;
  Expected:= -148918;
  AssertEquals(Expected,FixedDateToDateTime(544676),0);
end;

procedure TTestFixedDateDateTimeConversion.TestDateTimeToFixedDateWhenFixedDateisDateTime;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeDateTime;
  Expected:= 214193;
  AssertEquals(Expected,DateTimeToFixedDate(Expected),0);
end;

procedure TTestFixedDateDateTimeConversion.TestFixedDateToDateTimeWhenFixedDateisDateTime;
var
  Expected: Extended;
begin
  FixedDateEpochType:= fdeDateTime;
  Expected:= 214193;
  AssertEquals(Expected,FixedDateToDateTime(Expected),0);
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
  RegisterTest(TTestWeekDay);
  RegisterTest(TTestFixedDateJulianDateConversion);
  RegisterTest(TTestFixedDateRataDieConversion);
  RegisterTest(TTestFixedDateDateTimeConversion);
  RegisterTest(TTestJulianCalendar);
  RegisterTest(TTestGregorianCalendar);
  RegisterTest(TTestEgyptianCalendar);
  RegisterTest(TTestArmenianCalendar);
  RegisterTest(TTestMayanCalendar);
  RegisterTest(TTestAztecCalendar);

end.

