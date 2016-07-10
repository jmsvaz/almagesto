{
    almCalendar is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2011,2016 João Marcelo S. Vaz

    Almagesto is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Almagesto is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

//  This unit has calendar manipulation routines.
unit almCalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFixedDate = Extended;

// Fixed Date calendars (Julian Date, Rata Die and TDateTime) functions
function FixedDateToJulianDate(FixedDate: TFixedDate): TFixedDate;
function JulianDateToFixedDate(JulianDate: TFixedDate): TFixedDate;
function FixedDateToRataDie(FixedDate: TFixedDate): TFixedDate;
function RataDieToFixedDate(RataDie: TFixedDate): TFixedDate;
function FixedDateToDateTime(FixedDate: TFixedDate): TFixedDate;
function DateTimeToFixedDate(DateTime: TFixedDate): TFixedDate;

// Days of the week functions (Sunday is 0 and Saturday is 6)
function DayOfWeekFromFixed(FixedDate: TFixedDate): Integer;
function KDayOnOrBefore(k: Integer; FixedDate: TFixedDate): TFixedDate;
function KDayOnOrAfter(k: Integer; FixedDate: TFixedDate): TFixedDate;
function KDayNearest(k: Integer; FixedDate: TFixedDate): TFixedDate;
function KDayBefore(k: Integer; FixedDate: TFixedDate): TFixedDate;
function KDayAfter(k: Integer; FixedDate: TFixedDate): TFixedDate;

// Julian Calendar functions
procedure FixedDateToJulianCalendar(FixedDate: TFixedDate; out Year,Month,Day: Integer); overload;
function JulianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate; overload;
procedure FixedDateToJulianCalendar(FixedDate: TFixedDate; out Year, Month, Event, Count: Integer; out Leap: Boolean); overload;
function JulianCalendarToFixedDate(Year, Month, Event, Count: Integer; Leap: Boolean): TFixedDate; overload;
function JulianLeapYear(Year: Integer): Boolean;
procedure FixedDateToRomanCalendar(FixedDate: TFixedDate; out Year, Month, Event, Count: Integer; out Leap: Boolean); overload;
function RomanCalendarToFixedDate(Year, Month, Event, Count: Integer; Leap: Boolean): TFixedDate; overload;
function RomanLeapYear(Year: Integer): Boolean;

// Gregorian Calendar functions
procedure FixedDateToGregorianCalendar(FixedDate: TFixedDate; out Year,Month,Day: Integer);
function GregorianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;
function GregorianLeapYear(Year: Integer): Boolean;
function NthKDay(n,k: Integer; Year,Month,Day: Integer): TFixedDate;
function FirstKDay(k: Integer; Year,Month,Day: Integer): TFixedDate;
function LastKDay(k: Integer; Year,Month,Day: Integer): TFixedDate;

// ISO Calendar functions
procedure FixedDateToISOCalendar(FixedDate: TFixedDate; out Year,Week,Day: Integer);
function ISOCalendarToFixedDate(Year, Week, Day: Integer): TFixedDate;


// Egyptian Calendar functions
procedure FixedDateToEgyptianCalendar(FixedDate: TFixedDate; out Year,Month,Day: Integer);
function EgyptianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;

// Armenian Calendar functions
procedure FixedDateToArmenianCalendar(FixedDate: TFixedDate; out Year,Month,Day: Integer);
function ArmenianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;

// Zoroastrian Calendar functions
procedure FixedDateToZoroastrianCalendar(FixedDate: TFixedDate; out Year,Month,Day: Integer);
function ZoroastrianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;

// Coptic Calendar functions
procedure FixedDateToCopticCalendar(FixedDate: TFixedDate; out Year,Month,Day: Integer);
function CopticCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;
function CopticLeapYear(Year: Integer): Boolean;

// Ethiopic Calendar functions
procedure FixedDateToEthiopicCalendar(FixedDate: TFixedDate; out Year,Month,Day: Integer);
function EthiopicCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;

// Mayan Calendar functions
type
  TMayanCorrelation = (mcGoodmanMartinezThompson,mcSpinden);

function MayanLongCountToFixedDate(Baktun, Katun, Tun, Uinal, Kin: Integer; MayanCorrelation: TMayanCorrelation = mcGoodmanMartinezThompson): TFixedDate;
procedure FixedDateToMayanLongCount(FixedDate: TFixedDate; out Baktun, Katun, Tun, Uinal, Kin: Integer; MayanCorrelation: TMayanCorrelation = mcGoodmanMartinezThompson);
procedure FixedDateToMayanHaab(FixedDate: TFixedDate; out Day, Month: Integer; MayanCorrelation: TMayanCorrelation = mcGoodmanMartinezThompson);
procedure FixedDateToMayanTzolkin(FixedDate: TFixedDate; out Number, Name: Integer; MayanCorrelation: TMayanCorrelation = mcGoodmanMartinezThompson);

// Aztec Calendar functions
procedure FixedDateToAztecXihuitl(FixedDate: TFixedDate; out Day, Month: Integer);
procedure FixedDateToAztecTonalpohualli(FixedDate: TFixedDate; out Number, Name: Integer);


type
  TFixedDateEpochType = (fdeJulianDate, fdeRataDie, fdeDateTime);

function FixedDateEpoch(FixedDateEpochType: TFixedDateEpochType): TFixedDate;
function JulianDateEpoch: TFixedDate;
function RataDieEpoch: TFixedDate;
function DateTimeEpoch: TFixedDate;
function JulianCalendarEpoch: TFixedDate;
function GregorianCalendarEpoch: TFixedDate;
function MayanLongCountEpoch(MayanCorrelation: TMayanCorrelation = mcGoodmanMartinezThompson): TFixedDate;
function MayanHaabEpoch(MayanCorrelation: TMayanCorrelation = mcGoodmanMartinezThompson): TFixedDate;
function MayanTzolkinEpoch(MayanCorrelation: TMayanCorrelation = mcGoodmanMartinezThompson): TFixedDate;
function AztecXihuitlEpoch: TFixedDate;
function AztecTonalpohualliEpoch: TFixedDate;
function EgyptianCalendarEpoch: TFixedDate;
function ArmenianCalendarEpoch: TFixedDate;
function ZoroastrianCalendarEpoch: TFixedDate;
function CopticCalendarEpoch: TFixedDate;
function EthiopicCalendarEpoch: TFixedDate;

var
  FixedDateEpochType: TFixedDateEpochType = fdeJulianDate;

implementation

uses Math;

(******************************************************************************)
(*                             helper routines                                *)
(*                                                                            *)

function CalMod(x, y: Extended): Extended; overload;
begin
  Result:= x - y*Floor(x/y);
end;

function CalMod(x, y: Integer): Integer; overload;
begin
  Result:= x - y*Floor(x/y);
end;

function CalAMod(x, y: Extended): Extended; overload;
begin
  Result:= y + CalMod(x, -y);
end;

function CalAMod(x, y: Integer): Integer; overload;
begin
  Result:= y + CalMod(x, -y);
end;
(******************************************************************************)

(******************************************************************************)
(*                         Calendar Epoch routines                            *)
(*                                                                            *)

const
{ Julian Date Epoch is:
  JulianDate: 0
  RataDie: -1721424.5
  Julian Calendar: Noon, January 1, 4713 BCE (-4712)
  Gregorian Calendar: Noon, November 24, -4713
}
  JulianDateEpochInRataDie = -1721424.5;

{ Rata Die Epoch is:
  RataDie: 0
  JulianDate: 1721424.5
  Julian Calendar:
  Gregorian Calendar: Midnight, December 31, 0
}
  RataDieEpochInRataDie = 0;

{ DateTime Epoch is
  RataDie: 693594
  Gregorian Calendar: Midnight, December 30, 1899
}
  DateTimeEpochInRataDie = 693594;

{ Julian Calendar Epoch is:
  RataDie: -1
  Julian Calendar: Midnight, January 1, 1 CE
  Gregorian Calendar: Midnight, December 30, 0
}
  JulianCalendarEpochInRataDie = -1;

{ Roman Calendar epoch is the Julian Calendar counted from the founding of Rome
}
  RomanCalendarEpochInYears = -752; // 753 BCE (Julian Calendar)

{ Gregorian Calendar Epoch is:
  RataDie: 1
  Julian Calendar: Midnight, January 3, 1 CE
  Gregorian Calendar: Midnight, January 1, 1 CE
}
  GregorianCalendarEpochInRataDie = 1;

{ Mayan Long Count Calendar Epoch depends on the precise correlation between the
  Western calendars and the Long Count calendar. The generally accepted
  correlation constant is the Modified Thompson 2, "Goodman–Martinez–Thompson",
  or GMT correlation of JD 584282.5.
}
  Mayan_GoodmanMartinezThompsonInRataDie = -1137142;    //   06/sep/3114 BCE (Julian calendar)
  Mayan_SpindenInJulianDate              = 489383.5;    //   11/nov/3374 BCE (Julian calendar)

{ The precise correlation between Aztec dates and Rata Die is based on the recorded
  Aztec dates of the fall of Mexico City to Hernan Cortes in August 13, 1521 (Julian)
}
  AztecCorrelationInRataDie = 555403;

{ Egyptian Calendar Epoch is:
  RataDie: -272787 + 6h
  Julian Calendar: Sunrise, February 26, 747 BCE
  Gregorian Calendar: Sunrise, February 18, -746
}
  EgyptianCalendarEpochInRataDie = -272786.75;

{ Armenian Calendar Epoch is:
  RataDie: 201443 + 6h
  Julian Calendar: Sunrise, February 26, 747 BCE
  Gregorian Calendar: Sunrise, February 18, -746
}
  ArmenianCalendarEpochInRataDie = 201443.25;

{ Zoroastrian Calendar Epoch is:
  RataDie: 230638 + 6h
  Julian Calendar: Sunrise, June 16, 632 CE
  Gregorian Calendar: Sunrise, June 19, 632
}
  ZoroastrianCalendarEpochInRataDie = 230638.25;

{ Coptic Calendar Epoch is:
  RataDie: 103605 - 6h
  Julian Calendar: Sunset before August 29, 284 CE
  Gregorian Calendar: Sunset before August 29, 284
}
  CopticCalendarEpochInRataDie = 103604.75;

{ Ethiopic Calendar Epoch is:
  RataDie: 2796 - 6h
  Julian Calendar: Sunset before August 29, 8 CE
  Gregorian Calendar: Sunset before August 27, 8
}
  EthiopicCalendarEpochInRataDie = 2795.75;

{ Islamic Calendar Epoch is:
  RataDie: 227015 - 6h
  Julian Calendar: Sunset before July 16, 622 CE
  Gregorian Calendar: Sunset before July 19, 622
}
  IslamicCalendarEpochInRataDie = 227014.75;

{ Hebrew Calendar Epoch is:
  RataDie: -1373427 - 6h
  Julian Calendar: Sunset before October 7, 3761 BCE
  Gregorian Calendar: Sunset before September 7, -3760
}
  HebrewCalendarEpochInRataDie = 1373426.75;

{ Hindu Calendar Epoch is:
  RataDie: -1132959 + 6h
  Julian Calendar: Sunrise after February 18, 3102 BCE
  Gregorian Calendar: Sunrise after January 23, -3101
}
  HinduCalendarEpochInRataDie = 1132959.25;

function FixedDateEpoch(FixedDateEpochType: TFixedDateEpochType): TFixedDate;
begin
  case FixedDateEpochType of
    fdeJulianDate: Result:= JulianDateEpochInRataDie;
    fdeRataDie: Result:= RataDieEpochInRataDie;
    fdeDateTime: Result:= DateTimeEpochInRataDie;
  end;
end;

function JulianDateEpoch: TFixedDate;
begin
  Result:= JulianDateEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function RataDieEpoch: TFixedDate;
begin
  Result:= RataDieEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function DateTimeEpoch: TFixedDate;
begin
  Result:= DateTimeEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function JulianCalendarEpoch: TFixedDate;
begin
  Result:= JulianCalendarEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function GregorianCalendarEpoch: TFixedDate;
begin
  Result:= GregorianCalendarEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function MayanLongCountEpoch(MayanCorrelation: TMayanCorrelation): TFixedDate;
begin
  case MayanCorrelation of
    mcGoodmanMartinezThompson:
      Result:= Mayan_GoodmanMartinezThompsonInRataDie  - FixedDateEpoch(FixedDateEpochType);
    mcSpinden:
      Result:= JulianDateToFixedDate(Mayan_SpindenInJulianDate);
  end;
end;

function MayanHaabEpoch(MayanCorrelation: TMayanCorrelation): TFixedDate;
begin
  // on starting epoch of Mayan Long Count it was '8 Cumku' (8-18) and there are 348 days to '8 Cumku'
  Result:= MayanLongCountEpoch(MayanCorrelation) - 348;
end;

function MayanTzolkinEpoch(MayanCorrelation: TMayanCorrelation): TFixedDate;
begin
  // on starting epoch of Mayan Long Count it was '4 Ahau' (4-20) and there are 160 days to '4 Ahau'
  Result:= MayanLongCountEpoch(MayanCorrelation) - 160;
end;

function AztecXihuitlEpoch: TFixedDate;
begin
  // on the fall of Mexico City it was '2 Xocotlhuetzi' (2-11) and there are 201 days to '2 Xocotlhuetzi'
  Result:= AztecCorrelationInRataDie - FixedDateEpoch(FixedDateEpochType);
  Result:= Result - 201;
end;

function AztecTonalpohualliEpoch: TFixedDate;
begin
  // on the fall of Mexico City it was '1 Coatl' (1-5) and there are 104 days to '1 Coatl'
  Result:= AztecCorrelationInRataDie - FixedDateEpoch(FixedDateEpochType);
  Result:= Result - 104;
end;

function EgyptianCalendarEpoch: TFixedDate;
begin
  Result:= EgyptianCalendarEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function ArmenianCalendarEpoch: TFixedDate;
begin
  Result:= ArmenianCalendarEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function ZoroastrianCalendarEpoch: TFixedDate;
begin
  Result:= ZoroastrianCalendarEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function CopticCalendarEpoch: TFixedDate;
begin
  Result:= CopticCalendarEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

function EthiopicCalendarEpoch: TFixedDate;
begin
  Result:= EthiopicCalendarEpochInRataDie - FixedDateEpoch(FixedDateEpochType);
end;

(******************************************************************************)



(******************************************************************************)
(*                          Fixed Date functions                              *)
(*                   Julian Date, Rata Die and TDateTime                      *)

function FixedDateToJulianDate(FixedDate: TFixedDate): TFixedDate;
begin
  Result:= FixedDate - JulianDateEpoch;
end;

function JulianDateToFixedDate(JulianDate: TFixedDate): TFixedDate;
begin
  Result:= JulianDate + JulianDateEpoch;
end;

function FixedDateToRataDie(FixedDate: TFixedDate): TFixedDate;
begin
  Result:= FixedDate - RataDieEpoch;
end;

function RataDieToFixedDate(RataDie: TFixedDate): TFixedDate;
begin
  Result:= RataDie + RataDieEpoch;
end;

function FixedDateToDateTime(FixedDate: TFixedDate): TFixedDate;
begin
  Result:= FixedDate - DateTimeEpoch;
end;

function DateTimeToFixedDate(DateTime: TFixedDate): TFixedDate;
begin
  Result:= DateTime + DateTimeEpoch;
end;

(******************************************************************************)




(******************************************************************************)
(*                          Weekdays functions                                *)
(*                                                                            *)

function DayOfWeekFromFixed(FixedDate: TFixedDate): Integer;
var
  day: Integer;
const
  Sunday = 0;
begin
  // Rata Die Epoch is Sunday
  day:= Trunc(FixedDate - RataDieEpoch - Sunday);
  Result:= CalMod(day,7);
end;

function KDayOnOrBefore(k: Integer; FixedDate: TFixedDate): TFixedDate;
begin
  Result:= FixedDate - DayOfWeekFromFixed(FixedDate - k);
end;

function KDayOnOrAfter(k: Integer; FixedDate: TFixedDate): TFixedDate;
begin
  Result:= KDayOnOrBefore(k, FixedDate + 6);
end;

function KDayNearest(k: Integer; FixedDate: TFixedDate): TFixedDate;
begin
  Result:= KDayOnOrBefore(k, FixedDate + 3);
end;

function KDayBefore(k: Integer; FixedDate: TFixedDate): TFixedDate;
begin
  Result:= KDayOnOrBefore(k, FixedDate - 1);
end;

function KDayAfter(k: Integer; FixedDate: TFixedDate): TFixedDate;
begin
  Result:= KDayOnOrBefore(k, FixedDate + 7);
end;


(******************************************************************************)




(******************************************************************************)
(*                       Julian Calendar functions                            *)
(*                                                                            *)

{Fixed Date of Julian Calendar starting epoch (at preceding midnight)
  Julian: 01/jan/01 CE - Gregorian: 30/dec/01 BCE
}
function JulianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate; overload;
var
  c: Integer;
begin
  c:= 0;
  if Month > 2 then
    if JulianLeapYear(Year) then
      c:= -1
    else
      c:= -2;
  Result:= 365*(Year - 1) + Floor((Year - 1)/4) + Floor((367*Month - 362)/12) + Day + c - 1;
  Result:= Result + JulianCalendarEpoch;
end;

procedure FixedDateToJulianCalendar(FixedDate: TFixedDate; out Year, Month,
  Day: Integer);
var
  c: Integer;
begin
  Year:= Floor((4*Floor(FixedDate - JulianCalendarEpoch) + 1464)/1461);
  c:= 0;
  if (FixedDate - JulianCalendarToFixedDate(Year,3,1)) >= 0 then
    if JulianLeapYear(Year) then
      c:= 1
    else
      c:= 2;
  Month:= Floor((12*(Floor(FixedDate - JulianCalendarToFixedDate(Year,1,1)) + c) + 373)/367);
  Day:=  Floor(FixedDate - JulianCalendarToFixedDate(Year,Month,1)) + 1;
end;

function IdesOfMonth(Month: Integer): Integer;
begin
  case Month of
    3,5,7,10: // march, may, july, october
      Result:= 15;
  else
      Result:= 13;
  end;
end;

function NonesOfMonth(Month: Integer): Integer;
begin
  Result:= IdesOfMonth(Month) - 8;
end;



procedure FixedDateToJulianCalendar(FixedDate: TFixedDate; out Year, Month,
  Event, Count: Integer; out Leap: Boolean);
var
  Day: Integer;
begin
  FixedDateToJulianCalendar(FixedDate, Year, Month, Day);

  if Day = 1 then
    begin
      Event:= 1; // Kalendae
      Count:= 1;
      Leap:= False;
    end
  else
    if Day <= NonesOfMonth(Month) then
      begin
        Event:= 2; // Nonae
        Count:= NonesOfMonth(Month) - Day + 1;
        Leap:= False;
      end
    else
      if Day <= IdesOfMonth(Month) then
        begin
          Event:= 3; // Idus
          Count:= IdesOfMonth(Month) - Day + 1;
          Leap:= False;
        end
      else
        if ((Month <> 2 ) or not JulianLeapYear(Year)) then
          begin
            Month:= CalAMod((Month + 1), 12);
            if (Month = 1) then
              if (Year <> -1) then
                Year:= Year + 1
              else
                Year:= 1;
            Event:= 1; // Kalendae
            Count:= Floor(JulianCalendarToFixedDate(Year, Month, 1, 1, False) - FixedDate) + 1;
            Leap:= False;
          end
        else
          if Day < 25 then
            begin
              Month:= 3; // March
              Event:= 1; // Kalendae
              Count:= 30 - Day;
              Leap:= False;
            end
          else
            begin
              Month:= 3; // March
              Event:= 1; // Kalendae
              Count:= 31 - Day;
              Leap:= (Day = 25);
            end;
end;

function JulianCalendarToFixedDate(Year, Month, Event, Count: Integer;
  Leap: Boolean): TFixedDate;
begin
  Result:= 0;
  case Event of
    1:  // Kalendae
      Result:= JulianCalendarToFixedDate(Year, Month, 1);
    2:  // Nonae
      Result:= JulianCalendarToFixedDate(Year, Month, NonesOfMonth(Month));
    3:  // Idus
      Result:= JulianCalendarToFixedDate(Year, Month, IdesOfMonth(Month));
  end;
  Result:= Result - Count;
  if not (JulianLeapYear(Year) and (Month = 3) and (Event = 1) and (Count >= 6) and (Count <= 16)) then
    Result:= Result + 1;
  if Leap then
    Result:= Result + 1;
end;

function JulianLeapYear(Year: Integer): Boolean;
// verify if the Julian Year is Leap year
// this function doesn't use de BCE notation, i.e,
//     years are  ...  -2,    -1,     0,     1,    2,    3  ...
//     that means ... 3 BCE, 2 BCE, 1 BCE, 1 CE, 2 CE, 3 CE ...
// and doesn't use the historical leap year before 9 CE
// tha would be 45, 42, 39, 36, 33, 30, 27, 24, 21, 18, 15, 12, 9
begin
  Result:= (CalMod(Year,4) = 0);
end;

procedure FixedDateToRomanCalendar(FixedDate: TFixedDate; out Year, Month,
  Event, Count: Integer; out Leap: Boolean);
begin
  FixedDateToJulianCalendar(FixedDate, Year, Month, Event, Count, Leap);
  Year:= Year - RomanCalendarEpochInYears;
end;

function RomanCalendarToFixedDate(Year, Month, Event, Count: Integer;
  Leap: Boolean): TFixedDate;
begin
  Result:= JulianCalendarToFixedDate(Year + RomanCalendarEpochInYears, Month, Event, Count, Leap);
end;

function RomanLeapYear(Year: Integer): Boolean;
begin
  Result:= JulianLeapYear(Year + RomanCalendarEpochInYears);
end;

(******************************************************************************)

(******************************************************************************)
(*                    Gregorian Calendar functions                            *)
(*                                                                            *)

{Fixed of Gregorian Calendar starting epoch (at preceding midnight)
  Gregorian: 01/jan/01 CE - Julian: 03/jan/01 CE
}
function GregorianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;
var
  c: Integer;
begin
  c:= 0;
  if Month > 2 then
    if GregorianLeapYear(Year) then
      c:= -1
    else
      c:= -2;
  Result:= 365*(Year - 1) + Floor((Year - 1)/4) - Floor((Year - 1)/100) +
           Floor((Year - 1)/400) + Floor((367*Month - 362)/12) + Day + c - 1;
  Result:= Result + GregorianCalendarEpoch;
end;

function FixedDateToGregorianYear(FixedDate: TFixedDate): Integer;
var
  d0, n400, d1, n100, d2, n4, d3, n1: Integer;
begin
  d0:= Floor(FixedDate - GregorianCalendarEpoch);
  n400:= Floor(d0/146097);
  d1:= CalMod(d0,146097);
  n100:= Floor(d1/36524);
  d2:= CalMod(d1,36524);
  n4:= Floor(d2/1461);
  d3:= CalMod(d2,1461);
  n1:= Floor(d3/365);
  Result:= 400*n400 + 100*n100 + 4*n4 + n1;
  if (n100 <> 4) and (n1 <> 4) then
    Inc(Result);
end;

procedure FixedDateToGregorianCalendar(FixedDate: TFixedDate; out Year, Month,
  Day: Integer);
var
  c: Integer;
begin
  Year:= FixedDateToGregorianYear(FixedDate);
  c:= 0;
  if (FixedDate - GregorianCalendarToFixedDate(Year,3,1)) >= 0 then
    if GregorianLeapYear(Year) then
      c:= 1
    else
      c:= 2;
  Month:= Floor((12*(Floor(FixedDate - GregorianCalendarToFixedDate(Year,1,1)) + c) + 373)/367);
  Day:= Floor(FixedDate - GregorianCalendarToFixedDate(Year,Month,1)) + 1;
end;

function GregorianLeapYear(Year: Integer): Boolean;
begin
  Result:= ( (CalMod(Year,4) = 0) and ( (CalMod(Year,100) <> 0) or (CalMod(Year,400) = 0) ) )
end;

function NthKDay(n, k: Integer; Year, Month, Day: Integer): TFixedDate;
begin
  if n > 0 then
    Result:= 7*n + KDayBefore(k,GregorianCalendarToFixedDate(Year,Month,Day))
  else
    if n < 0 then
      Result:= 7*n + KDayAfter(k,GregorianCalendarToFixedDate(Year,Month,Day))
    else
      raise Exception.Create('Invalid n day');
end;

function FirstKDay(k: Integer; Year, Month, Day: Integer): TFixedDate;
begin
  Result:= NthKDay(1,k,Year,Month,Day);
end;

function LastKDay(k: Integer; Year, Month, Day: Integer): TFixedDate;
begin
  Result:= NthKDay(-1,k,Year,Month,Day);
end;

(******************************************************************************)

(******************************************************************************)
(*                    Gregorian Calendar functions                            *)
(*                                                                            *)

procedure FixedDateToISOCalendar(FixedDate: TFixedDate; out Year, Week,
  Day: Integer);
var
  approx: Integer;
begin
  Year:= FixedDateToGregorianYear(FixedDate);
  if FixedDate >= ISOCalendarToFixedDate(Year+1,1,1) then
    Inc(Year);
  Week:= 1 + Floor((FixedDate - ISOCalendarToFixedDate(Year,1,1))/7);
  Day:= CalAMod(Trunc(FixedDate - RataDieEpoch),7)
end;

function ISOCalendarToFixedDate(Year, Week, Day: Integer): TFixedDate;
const
  Sunday = 0;
begin
  Result:= NthKDay(Week,Sunday,Year-1,12,28) + Day;
end;

(******************************************************************************)


(******************************************************************************)
(*                      Egyptian Calendar functions                           *)
(*                                                                            *)

procedure FixedDateToEgyptianCalendar(FixedDate: TFixedDate; out Year, Month,
  Day: Integer);
var
  Days: Integer;
begin
  Days:= Floor(FixedDate - EgyptianCalendarEpoch);
  Year:= Floor(Days/365) + 1;
  Month:= Floor(CalMod(Days,365)/30) + 1;
  Day:= Days - 365*(Year - 1) - 30*(Month - 1) + 1;
end;

function EgyptianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;
begin
  Result:= EgyptianCalendarEpoch + 365*(Year - 1) + 30*(Month - 1) + Day - 1;
end;

(******************************************************************************)


(******************************************************************************)
(*                      Armenian Calendar functions                           *)
(*                                                                            *)


procedure FixedDateToArmenianCalendar(FixedDate: TFixedDate; out Year, Month,
  Day: Integer);
begin
  FixedDateToEgyptianCalendar(FixedDate + EgyptianCalendarEpoch - ArmenianCalendarEpoch,Year,Month,Day);
end;

function ArmenianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;
begin
  Result:= ArmenianCalendarEpoch + EgyptianCalendarToFixedDate(Year,Month,Day) - EgyptianCalendarEpoch;
end;

(******************************************************************************)


(******************************************************************************)
(*                    Zoroastrian Calendar functions                          *)
(*                                                                            *)


procedure FixedDateToZoroastrianCalendar(FixedDate: TFixedDate; out Year,
  Month, Day: Integer);
begin
  FixedDateToEgyptianCalendar(FixedDate + EgyptianCalendarEpoch - ZoroastrianCalendarEpoch,Year,Month,Day);
end;

function ZoroastrianCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;
begin
  Result:= ZoroastrianCalendarEpoch + EgyptianCalendarToFixedDate(Year,Month,Day) - EgyptianCalendarEpoch;
end;

(******************************************************************************)

(******************************************************************************)
(*                       Coptic Calendar functions                            *)
(*                                                                            *)

procedure FixedDateToCopticCalendar(FixedDate: TFixedDate; out Year, Month,
  Day: Integer);
begin
  Year:= Floor((4*Floor(FixedDate - CopticCalendarEpoch) + 1463)/1461);
  Month:= Floor(Floor(FixedDate - CopticCalendarToFixedDate(Year,1,1))/30) + 1;
  Day:=  Floor(FixedDate - CopticCalendarToFixedDate(Year,Month,1)) + 1;
end;

function CopticCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;
begin
  Result:= CopticCalendarEpoch -1 + 365*(Year - 1) + Floor(Year/4) + 30*(Month - 1) + Day;
end;

function CopticLeapYear(Year: Integer): Boolean;
begin
  Result:= (CalMod(Year,4) = 3);
end;

(******************************************************************************)

(******************************************************************************)
(*                      Ethiopic Calendar functions                           *)
(*                                                                            *)

procedure FixedDateToEthiopicCalendar(FixedDate: TFixedDate; out Year, Month,
  Day: Integer);
begin
  FixedDateToCopticCalendar(FixedDate + CopticCalendarEpoch - EthiopicCalendarEpoch,Year,Month,Day);

end;

function EthiopicCalendarToFixedDate(Year, Month, Day: Integer): TFixedDate;
begin
  Result:= EthiopicCalendarEpoch + CopticCalendarToFixedDate(Year,Month,Day) - CopticCalendarEpoch;

end;

(******************************************************************************)


(******************************************************************************)
(*                        Mayan Calendar functions                            *)
(*                                                                            *)

function MayanLongCountToFixedDate(Baktun, Katun, Tun, Uinal, Kin: Integer;
  MayanCorrelation: TMayanCorrelation): TFixedDate;
begin
  Result:= (((Baktun*20 + Katun)*20 + Tun)*18 + Uinal)*20 + Kin;
  Result:= Result + MayanLongCountEpoch(MayanCorrelation);
end;

procedure FixedDateToMayanLongCount(FixedDate: TFixedDate; out Baktun, Katun,
  Tun, Uinal, Kin: Integer; MayanCorrelation: TMayanCorrelation);
var
  Days: Integer;
begin
  Days:= Floor(FixedDate - MayanLongCountEpoch(MayanCorrelation));
  Baktun:= Floor(Days/144000);
  Days:= CalMod(Days,144000);
  Katun:= Floor(Days/7200);
  Days:= CalMod(Days,7200);
  Tun:= Floor(Days/360);
  Days:= CalMod(Days,360);
  Uinal:= Floor(Days/20);
  Kin:= CalMod(Days,20);
end;

procedure FixedDateToMayanHaab(FixedDate: TFixedDate; out Day, Month: Integer;
  MayanCorrelation: TMayanCorrelation);
var
  Count: Integer;
begin
  Count:= CalMod(Floor(FixedDate - MayanHaabEpoch(MayanCorrelation)),365);
  Month:= 1 + Floor(Count/20);
  Day:= CalMod(Count,20);
end;

procedure FixedDateToMayanTzolkin(FixedDate: TFixedDate; out Number,
  Name: Integer; MayanCorrelation: TMayanCorrelation);
var
  Count: Integer;
begin
  Count:= Floor(FixedDate - MayanTzolkinEpoch(MayanCorrelation));
  Name:= CalAMod(Count,20);
  Number:= CalAMod(Count,13);
end;

(******************************************************************************)


(******************************************************************************)
(*                        Aztec Calendar functions                            *)
(*                                                                            *)

procedure FixedDateToAztecXihuitl(FixedDate: TFixedDate; out Day, Month: Integer
  );
var
  Count: Integer;
begin
  Count:= CalMod(Floor(FixedDate - AztecXihuitlEpoch),365);
  Month:= 1 + Floor(Count/20);
  Day:= CalMod(Count,20) + 1;
end;

procedure FixedDateToAztecTonalpohualli(FixedDate: TFixedDate; out Number,
  Name: Integer);
var
  Count: Integer;
begin
  Count:= Floor(FixedDate - AztecTonalpohualliEpoch) + 1;
  Name:= CalAMod(Count,20);
  Number:= CalAMod(Count,13);
end;

(******************************************************************************)



end.

