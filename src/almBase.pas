{
    almBase is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2011, 2020 João Marcelo S. Vaz

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

//  This unit has the basic classes, types and routines.
unit almBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  {Julian Date (JD) is the interval of time in days and fractions of a day since
   January 1, 4713 BC Greenwich noon, Julian proleptic calendar. In precise
   work, the timescale, e.g., Terrestrial Time (TT) or Universal Time (UT),
   should be specified. }
  TJulianDate = Double;

  {Modified Julian Date (MJD) is equivalent to the Julian Date minus 2400000.5
   and is used where it is convenient to employ a day beginning at midnight.}
  TMJD = TJulianDate;

  TTriplet = record
    X: Double;
    Y: Double;
    Z: Double;
  end;

  TPosition = TTriplet;
  TVelocity = TTriplet;
  TAcceleration = TTriplet;

  TBodyState = record
    Pos: TPosition;
    Vel: TVelocity;
    Accel: TAcceleration
  end;

  TSolarSystemBody = (stSun,       //< Sun
                      plMercury,   //< Planets
                      plVenus,
                      plEarth,
                      plMars,
                      plJupiter,
                      plSaturn,
                      plUranus,
                      plNeptune,
                      dpCeres,     //< Dwarf planets
                      dpPluto,
                      dpHaumea,
                      dpMakemake,
                      dpEris,
                      pmMoon,     // Planet moons
                      pmPhobos,
                      pmDeimos);

  { TEphemeridesModel }

  TEphemeridesModel = class
    protected
      fName: string;
    public
      function HasBody(ABody: TSolarSystemBody): Boolean; virtual; abstract;
      function GetState(ABody: TSolarSystemBody; TDB: Double): TBodyState; virtual;
               abstract;
      property Name: string read fName;
    end;

const

  ssbMinStar = stSun;
  ssbMaxStar = stSun;
  ssbMinPlanet = plMercury;
  ssbMaxPlanet = plNeptune;
  ssbMinDwarfPlanet = dpCeres;
  ssbMaxDwarfPlanet = dpEris;
  ssbMinEarthMoon = pmMoon;
  ssbMaxEarthMoon = pmMoon;
  ssbMinMarsMoon = pmPhobos;
  ssbMaxMarsMoon = pmDeimos;


function fmod(X, Range: Double): Double;
function fmod(X, Max, Min: Double): Double;

{$I consts.inc}

implementation

function fmod(X, Range: Double): Double;
begin
  Result:= fmod(X, Range, 0);
end;

function fmod(X, Max, Min: Double): Double;
var
  Range: Double;
begin
  if Max < Min then
    begin
      Range:= Min;
      Min:= Max;
      Max:= Range;
    end;
  Range:= Max - Min;
  if Range = 0 then
    Result:= 0
  else
    begin
      Result:= Range*Frac(X/range);
      while Result < Min do
        Result:= Result + Range;
    end;
end;

end.

