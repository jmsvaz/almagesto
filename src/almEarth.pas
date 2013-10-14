{
    almEarth is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2013 João Marcelo S. Vaz

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

//  This unit has Earth orientation routines.
unit almEarth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, almBase;

procedure PrecessionIAU2006(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);



implementation

uses Math;

// Eps0: Obliquity of ecliptic at epoch
// EpsA: Obliquity of the ecliptic at date = Mean Obliquity
// PsiA: Precession in longitude, referred to the ecliptic of epoch
// OmegaA: Precession in obliquity, referred to the ecliptic of epoch
// ChiA: Planetary precession along the Equator
// DeltaPsi: Nutation in longitude
// DeltaEps: Nutation in obliquity

procedure PrecessionIAU1976(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);
//  reference: Lieske, J., et al. (1977). Astron. & Astrophys. 58, 1-16. (IAU 1976 Precession Model)
//             Lieske, J. (1979). Astron. & Astrophys. 73, 282-284.
//
//  This routine forms the three Euler angles which implement general
//  precession between J2000.0 and the date, using the IAU 1976 Precession model
//  (as for the FK5 catalog)
//  P = Rz(ChiA).Rx(-OmegaA).Rz(-PsiA).Rx(Eps0)
//  result = compute Precession Angles (PsiA, ChiA, OmegaA) (in radians)
//  uses: TDB
var
  t: Extended;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

//  Precession angles (Lieske et al. 1977)
  Eps0  := 84381.448; // obliquity of ecliptic at J2000.0 (in arcseconds)
  EpsA  := Eps0 + (-   46.8150 + (- 0.00059 + (  0.001813)*t)*t)*t;
  PsiA  :=        (  5038.7784 + (- 1.07259 + (- 0.001147)*t)*t)*t;
  ChiA  :=        (    10.5526 + (- 2.38064 + (- 0.001125)*t)*t)*t;
  OmegaA:= Eps0 + (              (  0.05127 + (- 0.007726)*t)*t)*t;

//  change to radians
  Eps0  := Eps0*RadiansPerArcSecond;
  EpsA  := EpsA*RadiansPerArcSecond;
  PsiA  := PsiA*RadiansPerArcSecond;
  ChiA  := ChiA*RadiansPerArcSecond;
  OmegaA:= OmegaA*RadiansPerArcSecond;
end;

procedure PrecessionIAU2000(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);
//  reference: McCarthy & Petit, IERS Conventions (2003), p. 45, IERS Technical Note 32, November 2003
//  P = Rz(ChiA).Rx(-OmegaA).Rz(-PsiA).Rx(Eps0)
//  result = compute Precession Angles (PsiA, ChiA, OmegaA) (in radians)
//  uses: TDB
var
  t: Extended;
const
// IAU 2000 precession corrections
// reference: McCarthy & Petit, IERS Conventions (2003), p. 43, IERS Technical Note 32, November 2003
  dPsiA = -0.29965; // correction for the precession rate of the equator in longitude (in arcseconds/century)
  dOmegaA = -0.02524; // correction for the precession rate of the equator in obliquity (in arcseconds/century)
begin
//  Precession angles (Lieske et al. 1977), but using TT instead of TDB (IERS Conventions 2003)
  PrecessionIAU1976(TDB, Eps0, EpsA,PsiA,ChiA,OmegaA);

//  Apply IAU 2000 precession corrections.
  t:= (TDB - J2000)/JulianDaysPerCentury;
  PsiA  := PsiA  + (dPsiA)*t*RadiansPerArcSecond;
  EpsA  := EpsA  + (dOmegaA)*t*RadiansPerArcSecond;
  OmegaA:= OmegaA  + (dOmegaA)*t*RadiansPerArcSecond;
end;

procedure PrecessionIAU2006(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);
//  reference: Capitaine et al, Astron. Astrophys. 412, 567-586 (2003)
//  P = Rz(ChiA).Rx(-OmegaA).Rz(-PsiA).Rx(Eps0)
//  result = compute Precession Angles (PsiA, ChiA, OmegaA) (in radians)
//  uses: TT
var
  t: Extended;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

//  Precession angles (Capitaine et al. 2003)
  Eps0  := 84381.406; // obliquity of ecliptic at J2000.0 (in arcseconds)
  EpsA  := Eps0 + (-   46.836769 + (- 0.0001831 + (  0.00200340 + (- 0.000000576 - 0.0000000434*t)*t)*t)*t)*t;
  PsiA  :=        (  5038.481507 + (- 1.0790069 + (- 0.00114045 + (  0.000132851 - 0.0000000951*t)*t)*t)*t)*t;
  ChiA  :=        (    10.556403 + (- 2.3814292 + (- 0.00121197 + (  0.000170663 - 0.0000000560*t)*t)*t)*t)*t;
  OmegaA:= Eps0 + (-    0.025754 + (  0.0512623 + (- 0.00772503 + (- 0.000000467 + 0.0000003337*t)*t)*t)*t)*t;

//  change to radians
  Eps0  := Eps0*RadiansPerArcSecond;
  EpsA  := EpsA*RadiansPerArcSecond;
  PsiA  := PsiA*RadiansPerArcSecond;
  ChiA  := ChiA*RadiansPerArcSecond;
  OmegaA:= OmegaA*RadiansPerArcSecond;
end;

procedure NutationIAU1980(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
//  REFERENCE:  Seidelmann, P.K. (1982) Celestial Mechanics 27, 79-106 (IAU 1980 Theory of Nutation)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the two Nutation angles in longitude and obliquity, with
//  respect to the equinox and ecliptic of date, using the IAU 1980 Nutation model
//  N = Rx(-(EpsA + DeltaEps)).Rz(-DeltaPsi).Rx(EpsA)
//  result = Nutation Angles (DeltaPsi, DeltaEps) (in arcsecs)
//  uses: TDB
const
  NE0 = 106;
  NutationCoeffs_80: array[1..NE0,1..9] of Integer =
//              Multiple of        Longitude     Obliquity
//          L   L'  F   D Omega    Ai      Ai'     Bi   Bi'
     (     (0,  0,  0,  0,  1, -171996, -1742,  92025,  89),
           (0,  0,  0,  0,  2,    2062,     2,   -895,   5),
          (-2,  0,  2,  0,  1,      46,     0,    -24,   0),
           (2,  0, -2,  0,  0,      11,     0,      0,   0),
          (-2,  0,  2,  0,  2,      -3,     0,      1,   0),
           (1, -1,  0, -1,  0,      -3,     0,      0,   0),
           (0, -2,  2, -2,  1,      -2,     0,      1,   0),
           (2,  0, -2,  0,  1,       1,     0,      0,   0),
           (0,  0,  2, -2,  2,  -13187,   -16,   5736, -31),
           (0,  1,  0,  0,  0,    1426,   -34,     54,  -1),
           (0,  1,  2, -2,  2,    -517,    12,    224,  -6),
           (0, -1,  2, -2,  2,     217,    -5,    -95,   3),
           (0,  0,  2, -2,  1,     129,     1,    -70,   0),
           (2,  0,  0, -2,  0,      48,     0,      1,   0),
           (0,  0,  2, -2,  0,     -22,     0,      0,   0),
           (0,  2,  0,  0,  0,      17,    -1,      0,   0),
           (0,  1,  0,  0,  1,     -15,     0,      9,   0),
           (0,  2,  2, -2,  2,     -16,     1,      7,   0),
           (0, -1,  0,  0,  1,     -12,     0,      6,   0),
          (-2,  0,  0,  2,  1,      -6,     0,      3,   0),
           (0, -1,  2, -2,  1,      -5,     0,      3,   0),
           (2,  0,  0, -2,  1,       4,     0,     -2,   0),
           (0,  1,  2, -2,  1,       4,     0,     -2,   0),
           (1,  0,  0, -1,  0,      -4,     0,      0,   0),
           (2,  1,  0, -2,  0,       1,     0,      0,   0),
           (0,  0, -2,  2,  1,       1,     0,      0,   0),
           (0,  1, -2,  2,  0,      -1,     0,      0,   0),
           (0,  1,  0,  0,  2,       1,     0,      0,   0),
          (-1,  0,  0,  1,  1,       1,     0,      0,   0),
           (0,  1,  2, -2,  0,      -1,     0,      0,   0),
           (0,  0,  2,  0,  2,   -2274,    -2,    977,  -5),
           (1,  0,  0,  0,  0,     712,     1,     -7,   0),
           (0,  0,  2,  0,  1,    -386,    -4,    200,   0),
           (1,  0,  2,  0,  2,    -301,     0,    129,  -1),
           (1,  0,  0, -2,  0,    -158,     0,     -1,   0),
          (-1,  0,  2,  0,  2,     123,     0,    -53,   0),
           (0,  0,  0,  2,  0,      63,     0,     -2,   0),
           (1,  0,  0,  0,  1,      63,     1,    -33,   0),
          (-1,  0,  0,  0,  1,     -58,    -1,     32,   0),
          (-1,  0,  2,  2,  2,     -59,     0,     26,   0),
           (1,  0,  2,  0,  1,     -51,     0,     27,   0),
           (0,  0,  2,  2,  2,     -38,     0,     16,   0),
           (2,  0,  0,  0,  0,      29,     0,     -1,   0),
           (1,  0,  2, -2,  2,      29,     0,    -12,   0),
           (2,  0,  2,  0,  2,     -31,     0,     13,   0),
           (0,  0,  2,  0,  0,      26,     0,     -1,   0),
          (-1,  0,  2,  0,  1,      21,     0,    -10,   0),
          (-1,  0,  0,  2,  1,      16,     0,     -8,   0),
           (1,  0,  0, -2,  1,     -13,     0,      7,   0),
          (-1,  0,  2,  2,  1,     -10,     0,      5,   0),
           (1,  1,  0, -2,  0,      -7,     0,      0,   0),
           (0,  1,  2,  0,  2,       7,     0,     -3,   0),
           (0, -1,  2,  0,  2,      -7,     0,      3,   0),
           (1,  0,  2,  2,  2,      -8,     0,      3,   0),
           (1,  0,  0,  2,  0,       6,     0,      0,   0),
           (2,  0,  2, -2,  2,       6,     0,     -3,   0),
           (0,  0,  0,  2,  1,      -6,     0,      3,   0),
           (0,  0,  2,  2,  1,      -7,     0,      3,   0),
           (1,  0,  2, -2,  1,       6,     0,     -3,   0),
           (0,  0,  0, -2,  1,      -5,     0,      3,   0),
           (1, -1,  0,  0,  0,       5,     0,      0,   0),
           (2,  0,  2,  0,  1,      -5,     0,      3,   0),
           (0,  1,  0, -2,  0,      -4,     0,      0,   0),
           (1,  0, -2,  0,  0,       4,     0,      0,   0),
           (0,  0,  0,  1,  0,      -4,     0,      0,   0),
           (1,  1,  0,  0,  0,      -3,     0,      0,   0),
           (1,  0,  2,  0,  0,       3,     0,      0,   0),
           (1, -1,  2,  0,  2,      -3,     0,      1,   0),
          (-1, -1,  2,  2,  2,      -3,     0,      1,   0),
          (-2,  0,  0,  0,  1,      -2,     0,      1,   0),
           (3,  0,  2,  0,  2,      -3,     0,      1,   0),
           (0, -1,  2,  2,  2,      -3,     0,      1,   0),
           (1,  1,  2,  0,  2,       2,     0,     -1,   0),
          (-1,  0,  2, -2,  1,      -2,     0,      1,   0),
           (2,  0,  0,  0,  1,       2,     0,     -1,   0),
           (1,  0,  0,  0,  2,      -2,     0,      1,   0),
           (3,  0,  0,  0,  0,       2,     0,      0,   0),
           (0,  0,  2,  1,  2,       2,     0,     -1,   0),
          (-1,  0,  0,  0,  2,       1,     0,     -1,   0),
           (1,  0,  0, -4,  0,      -1,     0,      0,   0),
          (-2,  0,  2,  2,  2,       1,     0,     -1,   0),
          (-1,  0,  2,  4,  2,      -2,     0,      1,   0),
           (2,  0,  0, -4,  0,      -1,     0,      0,   0),
           (1,  1,  2, -2,  2,       1,     0,     -1,   0),
           (1,  0,  2,  2,  1,      -1,     0,      1,   0),
          (-2,  0,  2,  4,  2,      -1,     0,      1,   0),
          (-1,  0,  4,  0,  2,       1,     0,      0,   0),
           (1, -1,  0, -2,  0,       1,     0,      0,   0),
           (2,  0,  2, -2,  1,       1,     0,     -1,   0),
           (2,  0,  2,  2,  2,      -1,     0,      0,   0),
           (1,  0,  0,  2,  1,      -1,     0,      0,   0),
           (0,  0,  4, -2,  2,       1,     0,      0,   0),
           (3,  0,  2, -2,  2,       1,     0,      0,   0),
           (1,  0,  2, -2,  0,      -1,     0,      0,   0),
           (0,  1,  2,  0,  1,       1,     0,      0,   0),
          (-1, -1,  0,  2,  1,       1,     0,      0,   0),
           (0,  0, -2,  0,  1,      -1,     0,      0,   0),
           (0,  0,  2, -1,  2,      -1,     0,      0,   0),
           (0,  1,  0,  2,  0,      -1,     0,      0,   0),
           (1,  0, -2, -2,  0,      -1,     0,      0,   0),
           (0, -1,  2,  0,  1,      -1,     0,      0,   0),
           (1,  1,  0, -2,  1,      -1,     0,      0,   0),
           (1,  0, -2,  2,  0,      -1,     0,      0,   0),
           (2,  0,  0,  2,  0,       1,     0,      0,   0),
           (0,  0,  2,  4,  2,      -1,     0,      0,   0),
           (0,  1,  0,  1,  0,       1,     0,      0,   0) );
var
  t: Extended;
  Argument, sinArg, cosArg: Extended;
  FundamentalArguments: array [1..5] of Extended;
  j, i: Integer;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

//    l = mean anomaly of the Moon
  FundamentalArguments[1]:=  2.3555483935439407 + t*(8328.691422883896  + t*(1.517951635553957e-4   + 3.1028075591010306e-7 * t));
//    l' = mean anomaly of the Sun
  FundamentalArguments[2]:=  6.240035939326023  + t*(628.3019560241842  + t*(-2.7973749400020225e-6 - 5.817764173314431e-8 * t));
//    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node
  FundamentalArguments[3]:= 1.6279019339719611 + t*(8433.466158318453  + t*(-6.427174970469119e-5  + 5.332950492204896e-8 * t));
//    D = mean elongation of the Moon from the Sun
  FundamentalArguments[4]:= 5.198469513579922  + t*(7771.377146170642  + t*(-3.340851076525812e-5  + 9.211459941081184e-8 * t));
//    OM = mean longitude of the Moon's ascending node
  FundamentalArguments[5]:= 2.1824386243609943 + t*(-33.75704593375351 + t*(3.614285992671591e-5   + 3.878509448876288e-8 * t));
// put in 2Pi range
  for i:= 1 to 5 do
    FundamentalArguments[i]:= fmod(FundamentalArguments[i],RadiansPerRev);

  //  Change time argument from centuries to millennia.
  t:= t/10;

//  Initialize nutation components.
  DeltaPsi:= 0;
  DeltaEps:= 0;
  // Argument = Soma(Nj.Fj)
  // DelPsi = Soma[(Ai + Ai'.T).sin(Argument)]
  // DelEps = Soma[(Bi + Bi'.T).cos(Argument)]
  //  Sum the nutation terms, ending with the biggest
  for i:= NE0 downto 1 do
    begin
      //   Form argument for current term
      Argument:= 0;
      for j:= 1 to 5 do
        Argument:= Argument + NutationCoeffs_80[i,j] * FundamentalArguments[j];
      // Accumulate current nutation term
      SinCos(Argument,sinArg,cosArg);
      DeltaPsi:= DeltaPsi + (NutationCoeffs_80[i,6] + NutationCoeffs_80[i,7]*t)*sinArg;
      DeltaEps:= DeltaEps + (NutationCoeffs_80[i,8] + NutationCoeffs_80[i,9]*t)*cosArg;
    end;
//    change to arcsecs
  DeltaPsi:= DeltaPsi/10000;
  DeltaEps:= DeltaEps/10000;
//    change to radians
  DeltaPsi:= DeltaPsi*RadiansPerArcSecond;
  DeltaEps:= DeltaEps*RadiansPerArcSecond;
end;

procedure NutationIAU2000B(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
//  REFERENCE:   (IAU 2000B Theory of Nutation Model)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the two Nutation angles in longitude and obliquity, with
//  respect to the equinox and ecliptic of date, using the IAU 2000B Theory of Nutation Model
//  N = Rx(-(EpsA + DeltaEps)).Rz(-DeltaPsi).Rx(EpsA)
//  result = Nutation Angles (DeltaPsi, DeltaEps) (in arcsecs)
//  uses: TT
const
//  Number of terms in the luni-solar nutation model
  NLS = 77;

//  Coefficients for fundamental arguments
  NALS: array[1..NLS,1..5] of Integer =
//              L     L'    F     D     Om
          (    (0,    0,    0,    0,    1),
               (0,    0,    2,   -2,    2),
               (0,    0,    2,    0,    2),
               (0,    0,    0,    0,    2),
               (0,    1,    0,    0,    0),
               (0,    1,    2,   -2,    2),
               (1,    0,    0,    0,    0),
               (0,    0,    2,    0,    1),
               (1,    0,    2,    0,    2),
               (0,   -1,    2,   -2,    2),
               (0,    0,    2,   -2,    1),
              (-1,    0,    2,    0,    2),
              (-1,    0,    0,    2,    0),
               (1,    0,    0,    0,    1),
              (-1,    0,    0,    0,    1),
              (-1,    0,    2,    2,    2),
               (1,    0,    2,    0,    1),
              (-2,    0,    2,    0,    1),
               (0,    0,    0,    2,    0),
               (0,    0,    2,    2,    2),
               (0,   -2,    2,   -2,    2),
              (-2,    0,    0,    2,    0),
               (2,    0,    2,    0,    2),
               (1,    0,    2,   -2,    2),
              (-1,    0,    2,    0,    1),
               (2,    0,    0,    0,    0),
               (0,    0,    2,    0,    0),
               (0,    1,    0,    0,    1),
              (-1,    0,    0,    2,    1),
               (0,    2,    2,   -2,    2),
               (0,    0,   -2,    2,    0),
               (1,    0,    0,   -2,    1),
               (0,   -1,    0,    0,    1),
              (-1,    0,    2,    2,    1),
               (0,    2,    0,    0,    0),
               (1,    0,    2,    2,    2),
              (-2,    0,    2,    0,    0),
               (0,    1,    2,    0,    2),
               (0,    0,    2,    2,    1),
               (0,   -1,    2,    0,    2),
               (0,    0,    0,    2,    1),
               (1,    0,    2,   -2,    1),
               (2,    0,    2,   -2,    2),
              (-2,    0,    0,    2,    1),
               (2,    0,    2,    0,    1),
               (0,   -1,    2,   -2,    1),
               (0,    0,    0,   -2,    1),
              (-1,   -1,    0,    2,    0),
               (2,    0,    0,   -2,    1),
               (1,    0,    0,    2,    0),
               (0,    1,    2,   -2,    1),
               (1,   -1,    0,    0,    0),
              (-2,    0,    2,    0,    2),
               (3,    0,    2,    0,    2),
               (0,   -1,    0,    2,    0),
               (1,   -1,    2,    0,    2),
               (0,    0,    0,    1,    0),
              (-1,   -1,    2,    2,    2),
              (-1,    0,    2,    0,    0),
               (0,   -1,    2,    2,    2),
              (-2,    0,    0,    0,    1),
               (1,    1,    2,    0,    2),
               (2,    0,    0,    0,    1),
              (-1,    1,    0,    1,    0),
               (1,    1,    0,    0,    0),
               (1,    0,    2,    0,    0),
              (-1,    0,    2,   -2,    1),
               (1,    0,    0,    0,    2),
              (-1,    0,    0,    1,    0),
               (0,    0,    2,    1,    2),
              (-1,    0,    2,    4,    2),
              (-1,    1,    0,    1,    1),
               (0,   -2,    2,   -2,    1),
               (1,    0,    2,    2,    1),
              (-2,    0,    2,    2,    2),
              (-1,    0,    0,    0,    2),
               (1,    1,    2,   -2,    2));

//  Luni-Solar nutation coefficients, unit 1e-7 arcseconds
    CLS: array[1..NLS,1..6] of Extended =
//                  longitude      |       obliquity
//          sin,     t*sin,    cos |   cos,   t*cos,   sin
    (( -172064161, -174666,  33386, 92052331,  9086, 15377),
     (  -13170906,   -1675, -13696,  5730336, -3015, -4587),
     (   -2276413,    -234,   2796,   978459,  -485,  1374),
     (    2074554,     207,   -698,  -897492,   470,  -291),
     (    1475877,   -3633,  11817,    73871,  -184, -1924),
     (    -516821,    1226,   -524,   224386,  -677,  -174),
     (     711159,      73,   -872,    -6750,     0,   358),
     (    -387298,    -367,    380,   200728,    18,   318),
     (    -301461,     -36,    816,   129025,   -63,   367),
     (     215829,    -494,    111,   -95929,   299,   132),
     (     128227,     137,    181,   -68982,    -9,    39),
     (     123457,      11,     19,   -53311,    32,    -4),
     (     156994,      10,   -168,    -1235,     0,    82),
     (      63110,      63,     27,   -33228,     0,    -9),
     (     -57976,     -63,   -189,    31429,     0,   -75),
     (     -59641,     -11,    149,    25543,   -11,    66),
     (     -51613,     -42,    129,    26366,     0,    78),
     (      45893,      50,     31,   -24236,   -10,    20),
     (      63384,      11,   -150,    -1220,     0,    29),
     (     -38571,      -1,    158,    16452,   -11,    68),
     (      32481,       0,      0,   -13870,     0,     0),
     (     -47722,       0,    -18,      477,     0,   -25),
     (     -31046,      -1,    131,    13238,   -11,    59),
     (      28593,       0,     -1,   -12338,    10,    -3),
     (      20441,      21,     10,   -10758,     0,    -3),
     (      29243,       0,    -74,     -609,     0,    13),
     (      25887,       0,    -66,     -550,     0,    11),
     (     -14053,     -25,     79,     8551,    -2,   -45),
     (      15164,      10,     11,    -8001,     0,    -1),
     (     -15794,      72,    -16,     6850,   -42,    -5),
     (      21783,       0,     13,     -167,     0,    13),
     (     -12873,     -10,    -37,     6953,     0,   -14),
     (     -12654,      11,     63,     6415,     0,    26),
     (     -10204,       0,     25,     5222,     0,    15),
     (      16707,     -85,    -10,      168,    -1,    10),
     (      -7691,       0,     44,     3268,     0,    19),
     (     -11024,       0,    -14,      104,     0,     2),
     (       7566,     -21,    -11,    -3250,     0,    -5),
     (      -6637,     -11,     25,     3353,     0,    14),
     (      -7141,      21,      8,     3070,     0,     4),
     (      -6302,     -11,      2,     3272,     0,     4),
     (       5800,      10,      2,    -3045,     0,    -1),
     (       6443,       0,     -7,    -2768,     0,    -4),
     (      -5774,     -11,    -15,     3041,     0,    -5),
     (      -5350,       0,     21,     2695,     0,    12),
     (      -4752,     -11,     -3,     2719,     0,    -3),
     (      -4940,     -11,    -21,     2720,     0,    -9),
     (       7350,       0,     -8,      -51,     0,     4),
     (       4065,       0,      6,    -2206,     0,     1),
     (       6579,       0,    -24,     -199,     0,     2),
     (       3579,       0,      5,    -1900,     0,     1),
     (       4725,       0,     -6,      -41,     0,     3),
     (      -3075,       0,     -2,     1313,     0,    -1),
     (      -2904,       0,     15,     1233,     0,     7),
     (       4348,       0,    -10,      -81,     0,     2),
     (      -2878,       0,      8,     1232,     0,     4),
     (      -4230,       0,      5,      -20,     0,    -2),
     (      -2819,       0,      7,     1207,     0,     3),
     (      -4056,       0,      5,       40,     0,    -2),
     (      -2647,       0,     11,     1129,     0,     5),
     (      -2294,       0,    -10,     1266,     0,    -4),
     (       2481,       0,     -7,    -1062,     0,    -3),
     (       2179,       0,     -2,    -1129,     0,    -2),
     (       3276,       0,      1,       -9,     0,     0),
     (      -3389,       0,      5,       35,     0,    -2),
     (       3339,       0,    -13,     -107,     0,     1),
     (      -1987,       0,     -6,     1073,     0,    -2),
     (      -1981,       0,      0,      854,     0,     0),
     (       4026,       0,   -353,     -553,     0,  -139),
     (       1660,       0,     -5,     -710,     0,    -2),
     (      -1521,       0,      9,      647,     0,     4),
     (       1314,       0,      0,     -700,     0,     0),
     (      -1283,       0,      0,      672,     0,     0),
     (      -1331,       0,      8,      663,     0,     4),
     (       1383,       0,     -2,     -594,     0,    -2),
     (       1405,       0,      4,     -610,     0,     2),
     (       1290,       0,      0,     -556,     0,     0));


var
  t: Extended;
  FundamentalArguments: array [1..5] of Extended;
  Argument, sinArg, cosArg: Extended;
  j, i: Integer;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

  // Fundamental (Delaunay) arguments from Simon et al. (1994)
  //    l = mean anomaly of the Moon (in arcseconds)
  FundamentalArguments[1]:= 134.96340251*ArcSecondsPerDegree + 1717915923.217800*t;
  //    l' = mean anomaly of the Sun (in arcseconds)
  FundamentalArguments[2]:= 357.52910918*ArcSecondsPerDegree + 129596581.048100*t;
  //    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (in arcseconds)
  FundamentalArguments[3]:= 93.27209062*ArcSecondsPerDegree + 1739527262.847800*t;
  //    D = mean elongation of the Moon from the Sun (in arcseconds)
  FundamentalArguments[4]:= 297.85019547*ArcSecondsPerDegree + 1602961601.209000*t;
  //    OM = mean longitude of the Moon's ascending node (in arcseconds)
  FundamentalArguments[5]:= 125.04455501*ArcSecondsPerDegree - 6962890.543100*t;
  // change Delaunay arguments to radians
  for i:= 1 to 5 do
    FundamentalArguments[i]:= RadiansPerArcSecond*FundamentalArguments[i];

//  Initialize nutation components.
  DeltaPsi:= 0;
  DeltaEps:= 0;
  //  Sum the luni-solar nutation terms, ending with the biggest.
  for i:= NLS downto 1 do
    begin
      //   Form argument for current term.
      Argument:= 0;
      for j:= 1 to 5 do
        Argument:= Argument + NALS[i,j] * FundamentalArguments[j];
      //   Accumulate current nutation term.
      SinCos(Argument,sinArg,cosArg);
      DeltaPsi:= DeltaPsi + (CLS[i,1] + CLS[i,2]*t)*sinArg + CLS[i,3]* cosArg;
      DeltaEps:= DeltaEps + (CLS[i,4] + CLS[i,5]*t)*cosArg + CLS[i,6]* sinArg;
    end;
//    change to arcsecs
  DeltaPsi:= DeltaPsi/1e7;
  DeltaEps:= DeltaEps/1e7;

//  Fixed offset to correct for missing terms in truncated series (planetary nutation)
  DeltaPsi:= DeltaPsi - 0.135/MilliArcSecondsPerArcSecond;
  DeltaEps:= DeltaEps + 0.388/MilliArcSecondsPerArcSecond;

//    change to radians
  DeltaPsi:= DeltaPsi*RadiansPerArcSecond;
  DeltaEps:= DeltaEps*RadiansPerArcSecond;
end;



end.

