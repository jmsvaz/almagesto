{
    almLocalReferenceSystems is part of Almagesto, a Free Pascal astronomical library.
    This file contains localreference systems conversion routines.

    Copyright (C) 2012 João Marcelo S. Vaz

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

unit almLocalReferenceSystems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, almBase;

//  GeodeticToGeocentric converts a Geodetic position wrt to a reference ellipsoid
//       to a cartesian geocenric positon
function GeodeticToGeocentric(Latitude, Longitude, Height, a, f: Double): TPosition;

implementation

function GeodeticToGeocentric(Latitude, Longitude, Height, a, f: Double
  ): TPosition;
var
  DF2, C, S, ach, ash, sinLon, cosLon, cosLat, sinLat: Extended;
begin

  // Compute factors at the observer's longitude
  SinCos(Longitude,sinLon, cosLon);
  // Compute factors at the observer's latitude
  SinCos(Latitude ,cosLat, sinLat);

  // Compute parameters relating to geodetic to geocentric conversion.
  DF2:= Sqr(1.0 - f);
  C:= 1.0/Sqrt(Sqr(cosLat) + DF2*Sqr(sinLat));
  S:= DF2 * C;
  ach:= a * C + Height;
  ash:= a * S + Height;

  // Compute position vector components in meters.
  Result.X:= ach * cosLat * cosLon;
  Result.Y:= ach * cosLat * sinLon;
  Result.Z:= ash * sinLat;

end;

end.

