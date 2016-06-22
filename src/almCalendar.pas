{
    almCalendar is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2011 João Marcelo S. Vaz

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

function FixedDateToJulianDate(FixedDate: Extended): Extended;
function JulianDateToFixedDate(JulianDate: Extended): Extended;
function FixedDateToRataDie(FixedDate: Extended): Extended;
function RataDieToFixedDate(RataDie: Extended): Extended;
function FixedDateToDateTime(FixedDate: Extended): Extended;
function DateTimeToFixedDate(DateTime: Extended): Extended;

// Julian Calendar functions
procedure JulianDateToJulianCalendar(JulianDate: Extended; var Year,Month,Day: Integer); overload;
function JulianCalendarToJulianDate(Year, Month, Day: Integer): Extended; overload;
function JulianLeapYear(Year: Integer): Boolean;

// Gregorian Calendar functions
procedure JulianDateToGregorianCalendar(JulianDate: Extended; var Year,Month,Day: Integer);
function GregorianCalendarToJulianDate(Year, Month, Day: Integer): Extended;
function GregorianLeapYear(Year: Integer): Boolean;

type
  TFixedDateEpochType = (fdeJulianDate, fdeRataDie, fdeDateTime);

function FixedDateEpoch(FixedDateEpochType: TFixedDateEpochType): Extended;
function JulianDateEpoch: Extended;
function RataDieEpoch: Extended;
function DateTimeEpoch: Extended;

var
  FixedDateEpochType: TFixedDateEpochType = fdeJulianDate;

implementation

uses Math;

// helper routines

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

function FixedDateEpoch(FixedDateEpochType: TFixedDateEpochType): Extended;
begin
  case FixedDateEpochType of
    fdeJulianDate: Result:= 1721424.5;
    fdeRataDie: Result:= 0;
    fdeDateTime: Result:= 693594.5;
  end;
end;

// JulianDate Epoch is Noon, January 1, 4713 BCE (Julian)
function JulianDateEpoch: Extended;
begin
  Result:= -1721424.5 - FixedDateEpoch(FixedDateEpochType);
end;

// RataDie Epoch is Midnight, January 1, 1 (Gregorian)
function RataDieEpoch: Extended;
begin
  Result:= 0 - FixedDateEpoch(FixedDateEpochType);
end;

// DateTime Epoch is Midnight, January 1, 1 (Gregorian)
function DateTimeEpoch: Extended;
begin
  Result:= 693594.5 - FixedDateEpoch(FixedDateEpochType);
end;

function FixedDateToJulianDate(FixedDate: Extended): Extended;
begin
  Result:= FixedDate - JulianDateEpoch;
end;

function JulianDateToFixedDate(JulianDate: Extended): Extended;
begin
  Result:= JulianDate + JulianDateEpoch;
end;

function FixedDateToRataDie(FixedDate: Extended): Extended;
begin
  Result:= FixedDate - RataDieEpoch;
end;

function RataDieToFixedDate(RataDie: Extended): Extended;
begin
  Result:= RataDie + RataDieEpoch;
end;

function FixedDateToDateTime(FixedDate: Extended): Extended;
begin
  Result:= FixedDate - DateTimeEpoch;
end;

function DateTimeToFixedDate(DateTime: Extended): Extended;
begin
  Result:= DateTime + DateTimeEpoch;
end;




(******************************************************************************)
(*                       Julian Calendar functions                            *)
(*                                                                            *)

{Julian Date of Julian Calendar starting epoch (at preceding midnight)
  Julian: 01/jan/01 CE - Gregorian: 30/dec/01 BCE
}
const JulianCalendarEpoch = 1721423.5;

function JulianCalendarToJulianDate(Year, Month, Day: Integer): Extended; overload;
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

procedure JulianDateToJulianCalendar(JulianDate: Extended; var Year, Month, Day: Integer); overload;
var
  c: Integer;
begin
  Year:= Floor((4*Floor(JulianDate - JulianCalendarEpoch) + 1464)/1461);
  c:= 0;
  if (JulianDate - JulianCalendarToJulianDate(Year,3,1)) >= 0 then
    if JulianLeapYear(Year) then
      c:= 1
    else
      c:= 2;
  Month:= Floor((12*(Floor(JulianDate - JulianCalendarToJulianDate(Year,1,1)) + c) + 373)/367);
  Day:=  Floor(JulianDate - JulianCalendarToJulianDate(Year,Month,1)) + 1;
end;

function JulianLeapYear(Year: Integer): Boolean;
// verify if the Julian Year is Leap year
// this function doesn't use de BCE notation, i.e,
//     years are  ...  -2,    -1,     0,     1,    2,    3  ...
//     that means ... 3 BCE, 2 BCE, 1 BCE, 1 CE, 2 CE, 3 CE ...
// and before 9 CE, doesn't use the historical leap year
begin
  Result:= (CalMod(Year,4) = 0);
//    Result:= (-Year in [45, 42, 39, 36, 33, 30, 27, 24, 21, 18, 15, 12, 9]) or (Year = 8);  // -Year = Year BC
end;
(******************************************************************************)

(******************************************************************************)
(*                    Gregorian Calendar functions                            *)
(*                                                                            *)

{Julian Date of Gregorian Calendar starting epoch (at preceding midnight)
  Gregorian: 01/jan/01 CE - Julian: 03/jan/01 CE
}
const GregorianCalendarEpoch = 1721425.5;

function GregorianCalendarToJulianDate(Year, Month, Day: Integer): Extended;
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

function JulianDateToGregorianYear(JulianDate: Extended): Integer;
var
  d0, n400, d1, n100, d2, n4, d3, n1: Integer;
begin
  d0:= Floor(JulianDate - GregorianCalendarEpoch);
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

procedure JulianDateToGregorianCalendar(JulianDate: Extended; var Year, Month, Day: Integer);
var
  c: Integer;
begin
  Year:= JulianDateToGregorianYear(JulianDate);
  c:= 0;
  if (JulianDate - GregorianCalendarToJulianDate(Year,3,1)) >= 0 then
    if GregorianLeapYear(Year) then
      c:= 1
    else
      c:= 2;
  Month:= Floor((12*(Floor(JulianDate - GregorianCalendarToJulianDate(Year,1,1)) + c) + 373)/367);
  Day:= Floor(JulianDate - GregorianCalendarToJulianDate(Year,Month,1)) + 1;
end;

function GregorianLeapYear(Year: Integer): Boolean;
begin
  Result:= ( (CalMod(Year,4) = 0) and ( (CalMod(Year,100) <> 0) or (CalMod(Year,400) = 0) ) )
end;

(******************************************************************************)

end.

