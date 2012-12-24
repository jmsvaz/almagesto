{
    almBase is part of Almagesto, a Free Pascal astronomical library.
    This contains the basic classes, types and routines.

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

  TSolarSystemBody = (stSun,       // Sun
                      plMercury,   // Planets
                      plVenus,
                      plEarth,
                      plMars,
                      plJupiter,
                      plSaturn,
                      plUranus,
                      plNeptune,
                      dpCeres,     // Dwarf planets
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


  //  Julian Date of epoch J2000.0 (2000 January 1, 12h)
    J2000  = 2451545.0;
  //  Julian Date of epoch J1900.0 (1900 January 1, 12h)
    J1900  = 2415020.0;
  //  Julian Date of epoch B1950.0 (1950 January 1, 12h)
    B1950  = 2433282.42345905;
  //  Julian Date of epoch B1900.0 (1900 January 1, 12h)
    B1900  = 2415020.31352;
  //  Julian Date of epoch B1850.0 (1850 January 1, 12h)
    B1850  = 2396758.20358095;

  // Julian Date on 2000 January 1, 0h
    JulianDate2000 = 2451544.5;

  //number of radians in one revolution
    RadiansPerRev = 2*Pi;
  //number of degrees in one revolution
    DegreesPerRev = 360;
  //number of hours in one revolution
    HoursPerRev   = 24;

  //number of arcminutes in one degree
    ArcMinutesPerDegree         = 60;
  //number of arcseconds in one arcminute
    ArcSecondsPerArcMinute      = 60;
  //number of milliseconds in one second
    MilliArcSecondsPerArcSecond = 1000;
  //number of arcseconds in one degress
    ArcSecondsPerDegree         = ArcSecondsPerArcMinute * ArcMinutesPerDegree;
  //number of milliseconds in one day
    MilliArcSecondsPerDegree    = MilliArcSecondsPerArcSecond * ArcSecondsPerDegree;

  //number of hours in one day
    HoursPerDay            = HoursPerRev;
  //number of minutes in one hour
    MinutesPerHour         = 60;
  //number of seconds in one minute
    SecondsPerMinute       = 60;
  //number of milliseconds in one second
    MilliSecondsPerSecond = 1000;
  //number of microseconds in one second
    MicroSecondsPerSecond = 1000*MilliSecondsPerSecond;
  //number of minutes in one day
    MinutesPerDay          = MinutesPerHour * HoursPerDay;
  //number of seconds in one hour
    SecondsPerHour         = SecondsPerMinute * MinutesPerHour;
  //number of seconds in one day
    SecondsPerDay          = SecondsPerMinute * MinutesPerDay;
  //number of milliseconds in one day
    MilliSecondsPerDay     = MilliSecondsPerSecond * SecondsPerDay;

  //number of hours in one degree
    HoursPerDegree         = HoursPerDay/DegreesPerRev;
  //number of radians in one degree
    RadiansPerDegree       = RadiansPerRev/DegreesPerRev;
  //number of radians in one hour
    RadiansPerHour         = RadiansPerRev/HoursPerDay;
  //number of radians in one arcsecond
    RadiansPerArcSecond    = RadiansPerDegree/ArcSecondsPerDegree;

{$I consts.inc}

implementation

end.
