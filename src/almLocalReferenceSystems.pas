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

type

  // historical earth ellipsoids
  TEarthEllipsoid = (eeWGS60,eeWGS66,eeGRS67,eeWGS72,eeGRS80,eeMERIT83,eeWGS84,eeIERS1989,eeIERS2003);


// GetEarthEllipsoid returns historical earth ellipsoid parameters (a and f)
procedure GetEarthEllipsoid(Ellipsoid: TEarthEllipsoid; out a,f: Double);

//  GeodeticToGeocentric converts a Geodetic position wrt to a reference ellipsoid
//       to a cartesian geocenric positon
function GeodeticToGeocentric(Latitude, Longitude, Height, a, f: Double): TPosition;

implementation

const
  EarthEllipsoid: array[TEarthEllipsoid,0..1] of Double =
    ((6378165  , 1/298.3         ),
     (6378145  , 1/298.25        ),
     (6378160  , 1/298.2471674273),
     (6378135  , 1/298.26        ),
     (6378137  , 1/298.257222101 ),
     (6378137  , 1/298.257       ),
     (6378137  , 1/298.257223563 ),
     (6378136  , 1/298.257       ),
     (6378136.6, 1/298.25642     ));


procedure GetEarthEllipsoid(Ellipsoid: TEarthEllipsoid; out a, f: Double);
begin
  a:= EarthEllipsoid[Ellipsoid,0];
  f:= EarthEllipsoid[Ellipsoid,1];
end;

function GeodeticToGeocentric(Latitude, Longitude, Height, a, f: Double
  ): TPosition;
var
  DF2, C, S, ach, ash, sinLon, cosLon, cosLat, sinLat: Extended;
begin

  // Compute factors at the observer's longitude
  SinCos(Longitude,sinLon, cosLon);
  // Compute factors at the observer's latitude
  SinCos(Latitude ,sinLat, cosLat);

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

