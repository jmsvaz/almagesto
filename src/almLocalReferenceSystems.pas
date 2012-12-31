{
    almLocalReferenceSystems is part of Almagesto, a Free Pascal astronomical library.

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

{almLocalReferenceSystems contains local reference systems transformation routines.
}
unit almLocalReferenceSystems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, almBase;

type

  { Earth reference ellipsoids.

    Reference ellipsoid is a mathematically-defined surface that approximates the geoid,
    the truer figure of the Earth, or other planetary body. Because of their relative
    simplicity, reference ellipsoids are used as a preferred surface on which geodetic
    network computations are performed and point coordinates such as latitude, longitude,
    and elevation are defined.
    An ellipsoid of revolution is uniquely defined by two numbers. Geodesists, by
    convention, use the semimajor axis and flattening. The size is represented by
    the radius at the equator — the semimajor axis of the cross-sectional ellipse and
    designated by the letter a. The shape of the ellipsoid is given by the flattening f,
    which indicates how much the ellipsoid departs from spherical.
  }
  TEarthEllipsoid = (
    eeWGS60,     //< World Geodetic System 1960
    eeWGS66,     //< World Geodetic System 1966
    eeGRS67,     //< Geodetic Reference System 1967
    eeWGS72,     //< World Geodetic System 1972
    eeGRS80,     //< Geodetic Reference System 1980
    eeMERIT83,   //< MERIT 1983
    eeWGS84,     //< World Geodetic System 1984
    eeIERS1989,  //< IERS Conventions 1989
    eeIERS2003); //< IERS Conventions 2003


{ GetEarthEllipsoid returns Earth reference ellipsoids parameters (Earth equatorial radius
  and flattening).
     @param(Ellipsoid is the @link(TEarthEllipsoid) kind that you need the parameters)
     @returns(a is a Double number with the Earth equatorial radius in meters)
     @returns(f is a Double number with the Earth flattening value (a-b)/a)

  sources:
  @unorderedList(
    @item(Explanatory Supplement to the Astronomical Almanac, P. Kenneth Seidelmann
          (ed), University Science Books (1992), p220, Table 4.242.1)
    @item(IERS Conventions (2003), Chapter 1, p12)
  )
  @seealso(TEarthEllipsoid)
}
procedure GetEarthEllipsoid(Ellipsoid: TEarthEllipsoid; out a,f: Double);

{  GeodeticToGeocentric transform geodetic coordinates to geocentric for a reference
   ellipsoid of specified form.
   @param(Latitude is the geodetic latititude in radians)
   @param(Longitude is the geodeticlongitude measured eastward around the Earth in radians)
   @param(Height is the height above ellipsoid in meters or in the same unit as @link(a))
   @param(a is the Earth equatorial radius in meters or in the same unit as @link(Height))
   @param(f is the Earth flattening value (a-b)/a)
   @returns(a @link(TPosition) value with the geocentric coordinate vector in the same unit
            as @link(a) and @link(Height))
}
function GeodeticToGeocentric(Latitude, Longitude, Height, a, f: Double): TPosition;

implementation

const
  { values from Earth reference ellipsoids
  sources:
  @unorderedList(
    @item(Explanatory Supplement to the Astronomical Almanac, P. Kenneth Seidelmann
         (ed), University Science Books (1992), p220, Table 4.242.1)
    @item(IERS Conventions (2003), Chapter 1, p12)
  }
  EarthEllipsoid: array[TEarthEllipsoid,0..1] of Double =
    ((6378165  , 1/298.3         ),   // eeWGS60
     (6378145  , 1/298.25        ),   // eeWGS66
     (6378160  , 1/298.2471674273),   // eeGRS67
     (6378135  , 1/298.26        ),   // eeWGS72
     (6378137  , 1/298.257222101 ),   // eeGRS80
     (6378137  , 1/298.257       ),   // eeMERIT83
     (6378137  , 1/298.257223563 ),   // eeWGS84
     (6378136  , 1/298.257       ),   // eeIERS1989
     (6378136.6, 1/298.25642     ));  // eeIERS2003


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

