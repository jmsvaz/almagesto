{
    almUnist is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2023 João Marcelo S. Vaz

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

//  This unit has units conversion routines.

unit almUnits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function Convert(aValue: Double; FromUnit: Double; ToUnit: Double): Double;

const

  // angles
  cRadians = 2*Pi;
  cDegrees = 360;
  cArcMinutes = 60*cDegrees;
  cArcSeconds = 60*cArcMinutes;
  cMilliArcSeconds = 1000*cArcSeconds;
  cMicroArcSeconds = 1000*cMilliArcSeconds;

  // time
  cDays = 1;
  cHours = 24*cDays;
  cMinutes = 60*cHours;
  cSeconds = 60*cMinutes;
  cMilliSeconds = 1000*cSeconds;
  cMicroSeconds = 1000*cMilliSeconds;


implementation

function Convert(aValue: Double; FromUnit: Double; ToUnit: Double): Double;
begin
  Result:= AValue*ToUnit/FromUnit;
end;

end.

