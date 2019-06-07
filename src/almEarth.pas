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

procedure NutationIAU1980(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
procedure NutationIAU2000B(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
procedure NutationIAU2000A_IERS(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
procedure NutationIAU2000A_SOFA(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);



implementation

uses Math;

{$I nutation.inc}

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
  // Argument = Summation(Nj.Fj)
  // DelPsi = Summation[(Ai + Ai'.T).sin(Argument)]
  // DelEps = Summation[(Bi + Bi'.T).cos(Argument)]
  //  Sum the nutation terms, ending with the biggest
  for i:= High(NutationIAU1980_Coeffs) downto Low(NutationIAU1980_Coeffs) do
    begin
      //   Form argument for current term
      Argument:= 0;
      for j:= 1 to 5 do
        Argument:= Argument + NutationIAU1980_Coeffs[i,j] * FundamentalArguments[j];
      // Accumulate current nutation term
      SinCos(Argument,sinArg,cosArg);
      DeltaPsi:= DeltaPsi +
                 (NutationIAU1980_Coeffs[i,6] +
                  NutationIAU1980_Coeffs[i,7]*t)*sinArg;
      DeltaEps:= DeltaEps +
                 (NutationIAU1980_Coeffs[i,8] +
                  NutationIAU1980_Coeffs[i,9]*t)*cosArg;
    end;
//    change to arcsecs
  DeltaPsi:= DeltaPsi*1e-4;
  DeltaEps:= DeltaEps*1e-4;
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
  for i:= High(NutationIAU2000B_Coeffs) downto Low(NutationIAU2000B_Coeffs) do
    begin
      //   Form argument for current term.
      Argument:= 0;
      for j:= 1 to 5 do
        Argument:= Argument + NutationIAU2000B_Coeffs[i,j] * FundamentalArguments[j];
      SinCos(Argument,sinArg,cosArg);
      //   Accumulate current nutation term.
      DeltaPsi:= DeltaPsi +
                 (NutationIAU2000B_Coeffs[i,6] +
                  NutationIAU2000B_Coeffs[i,7]*t)*sinArg +
                  NutationIAU2000B_Coeffs[i,8]* cosArg;
      DeltaEps:= DeltaEps +
                 (NutationIAU2000B_Coeffs[i,9] +
                  NutationIAU2000B_Coeffs[i,10]*t)*cosArg +
                  NutationIAU2000B_Coeffs[i,11]* sinArg;
    end;
//    change to arcsecs
  DeltaPsi:= DeltaPsi*1e-7;
  DeltaEps:= DeltaEps*1e-7;

//  Fixed offset to correct for missing terms in truncated series (planetary nutation)
  DeltaPsi:= DeltaPsi - 0.135/MilliArcSecondsPerArcSecond;
  DeltaEps:= DeltaEps + 0.388/MilliArcSecondsPerArcSecond;

//    change to radians
  DeltaPsi:= DeltaPsi*RadiansPerArcSecond;
  DeltaEps:= DeltaEps*RadiansPerArcSecond;
end;

procedure NutationIAU2000A_IERS(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
//  REFERENCE:  IAU 2000A Theory of Nutation Model
//              IERS Conventions (2003)
//  This routine computes the two Nutation angles in longitude and obliquity, with
//  respect to the equinox and ecliptic of date, using the IAU 2000A Theory of Nutation Model
//  N = Rx(-(EpsA + DeltaEps)).Rz(-DeltaPsi).Rx(EpsA)
//  result = Nutation Angles (DeltaPsi, DeltaEps) (in arcsecs)
//  uses: TDB
var
  t: Extended;
  FundamentalArguments: array [1..14] of Extended;
  Argument, sinArg, cosArg: Extended;
  dPsiLS, dEpsLS, dPsiPL, dEpsPL: Double;
  j, i: Integer;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

   // Fundamental (Delaunay) arguments from Simon et al. (1994)
  //    l = mean anomaly of the Moon (in arcseconds)
  FundamentalArguments[1]:= 134.96340251*ArcSecondsPerDegree + (1717915923.217800 +
                             (31.879200 + (0.05163500 - 0.0002447000*t)*t)*t)*t;
  //    l' = mean anomaly of the Sun (in arcseconds)
  FundamentalArguments[2]:= 357.52910918*ArcSecondsPerDegree + (129596581.048100 +
                             (-0.553200 + (0.00013600 - 0.0000114900*t)*t)*t)*t;
  //    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (in arcseconds)
  FundamentalArguments[3]:= 93.27209062*ArcSecondsPerDegree + (1739527262.847800 +
                             (-12.751200 + (-0.00103700 + 0.0000041700*t)*t)*t)*t;
  //    D = mean elongation of the Moon from the Sun (in arcseconds)
  FundamentalArguments[4]:= 297.85019547*ArcSecondsPerDegree + (1602961601.209000 +
                             (-6.370600 + (0.00659300 - 0.0000316900*t)*t)*t)*t;
  //    OM = mean longitude of the Moon's ascending node (in arcseconds)
  FundamentalArguments[5]:= 125.04455501*ArcSecondsPerDegree + (-6962890.543100 +
                             (7.472200 + (0.00770200 - 0.0000593900*t)*t)*t)*t;
  // change Delaunay arguments to radians
  for i:= 1 to 5 do
    FundamentalArguments[i]:= RadiansPerArcSecond*FundamentalArguments[i];

  // Planetary longitudes, Mercury through Neptune (Souchay et al. 1999).
  //    lMe = mean longitude of Mercury
  FundamentalArguments[6]:= 4.402608842 + 2608.7903141574 * t;
  //    lVe = mean longitude of Venus
  FundamentalArguments[7]:= 3.176146697 + 1021.3285546211 * t;
  //    lE = mean longitude of Earth
  FundamentalArguments[8]:= 1.753470314 + 628.3075849991 * t;
  //    lMa = mean longitude of Mars
  FundamentalArguments[9]:= 6.203480913 + 334.0612426700 * t;
  //    lJu = mean longitude of Jupiter
  FundamentalArguments[10]:= 0.599546497 + 52.9690962641 * t;
  //    lSa = mean longitude of Saturn
  FundamentalArguments[11]:= 0.874016757 + 21.3299104960 * t;
  //    lUr = mean longitude of Uranus
  FundamentalArguments[12]:= 5.481293872 + 7.4781598567 * t;
  //    lNe = mean longitude of Neptune
  FundamentalArguments[13]:= 5.311886287 + 3.8133035638 * t;
  //    Pa = general precession on longitude
  FundamentalArguments[14]:= (0.024381750 + 0.00000538691 * t) * t;

//  Initialize Luni-Solar nutation components
  dPsiLS:= 0;
  dEpsLS:= 0;
//  Sum the luni-solar nutation terms, ending with the biggest.
 for i:= High(NutationIAU2000A_LSCoeffs) downto Low(NutationIAU2000A_LSCoeffs) do
   begin
     //   Form argument for current term.
     Argument:= 0;
     for j:= 1 to 5 do
       Argument:= Argument + NutationIAU2000A_LSCoeffs[i,j] * FundamentalArguments[j];
     SinCos(Argument,sinArg,cosArg);
     //   Accumulate current nutation term.
     dPsiLS:= dPsiLS +
              (NutationIAU2000A_LSCoeffs[i,6] + NutationIAU2000A_LSCoeffs[i,7]*t)*sinArg +
              (NutationIAU2000A_LSCoeffs[i,8] + NutationIAU2000A_LSCoeffs[i,9]*t)*cosArg;
     dEpsLS:= dEpsLS +
              (NutationIAU2000A_LSCoeffs[i,10] + NutationIAU2000A_LSCoeffs[i,11]*t)*cosArg +
              (NutationIAU2000A_LSCoeffs[i,12] + NutationIAU2000A_LSCoeffs[i,13]*t)*sinArg;
   end;

//  Initialize Planetary nutation components
  dPsiPL:= 0;
  dEpsPL:= 0;
//  Sum the planetary nutation terms, ending with the biggest.
 for i:= High(NutationIAU2000A_PLCoeffs) downto Low(NutationIAU2000A_PLCoeffs) do
   begin
     //   Form argument for current term.
     Argument:= 0;
     for j:= 1 to 14 do
       Argument:= Argument + NutationIAU2000A_PLCoeffs[i,j] * FundamentalArguments[j];
     SinCos(Argument,sinArg,cosArg);
     //   Accumulate current nutation term.
     dPsiPL:= dPsiPL +
              NutationIAU2000A_PLCoeffs[i,15]*sinArg +
              NutationIAU2000A_PLCoeffs[i,16]*cosArg;
     dEpsPL:= dEpsPL +
              NutationIAU2000A_PLCoeffs[i,17]*cosArg +
              NutationIAU2000A_PLCoeffs[i,18]*sinArg;
   end;

//  Add Luni-Solar and Planetary components
  DeltaPsi:= dPsiLS + dPsiPL;
  DeltaEps:= dEpsLS + dEpsPL;

//    change to arcsecs
  DeltaPsi:= DeltaPsi*1e-7;
  DeltaEps:= DeltaEps*1e-7;

//    change to radians
  DeltaPsi:= DeltaPsi*RadiansPerArcSecond;
  DeltaEps:= DeltaEps*RadiansPerArcSecond;
end;

procedure NutationIAU2000A_SOFA(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
//  REFERENCE:  IAU 2000A Theory of Nutation Model
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the two Nutation angles in longitude and obliquity, with
//  respect to the equinox and ecliptic of date, using the IAU 2000A Theory of Nutation Model
//  N = Rx(-(EpsA + DeltaEps)).Rz(-DeltaPsi).Rx(EpsA)
//  result = Nutation Angles (DeltaPsi, DeltaEps) (in arcsecs)
//  uses: TDB
var
  t: Double;
  el, elp, f, d, om, lMe, lVe, lEa, lMa, lJu, lSa, lUr, lNe, Pa: Double;
  Argument, sinArg, cosArg: Double;
  dPsiLS, dEpsLS, dPsiPL, dEpsPL: Double;
  i: Integer;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

   // Fundamental (Delaunay) arguments (arcseconds converted to radians)
  //    l = mean anomaly of the Moon (IERS 2003)
  el:=  fmod(RadiansPerArcSecond*(485868.249036 + (1717915923.2178 +
                             (31.8792 + (0.051635 - 0.00024470*t)*t)*t)*t),RadiansPerRev);
  //    l' = mean anomaly of the Sun (MHB2000)
  elp:= fmod(RadiansPerArcSecond*(1287104.79305 + (129596581.0481 +
                             (-0.5532 + (0.000136 - 0.00001149*t)*t)*t)*t),RadiansPerRev);
  //    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (IERS 2003)
  f:= fmod(RadiansPerArcSecond*(335779.526232 + (1739527262.8478 +
                             (-12.7512 + (-0.001037 + 0.00000417*t)*t)*t)*t),RadiansPerRev);
  //    D = mean elongation of the Moon from the Sun (MHB2000)
  d:= fmod(RadiansPerArcSecond*(1072260.70369 + (1602961601.2090 +
                             (-6.3706 + (0.006593 - 0.00003169*t)*t)*t)*t),RadiansPerRev);
  //    OM = mean longitude of the Moon's ascending node (IERS 2003)
  om:= fmod(RadiansPerArcSecond*(450160.398036 + (-6962890.5431 +
                             (7.4722 + (0.007702 - 0.00005939*t)*t)*t)*t),RadiansPerRev);

//  Initialize Luni-Solar nutation components
  dPsiLS:= 0;
  dEpsLS:= 0;
//  Sum the luni-solar nutation terms, ending with the biggest.
 for i:= High(NutationIAU2000A_LSCoeffsSOFA) downto Low(NutationIAU2000A_LSCoeffsSOFA) do
   begin
     //   Form argument for current term.
       Argument:= NutationIAU2000A_LSCoeffsSOFA[i,1] * el  +
                  NutationIAU2000A_LSCoeffsSOFA[i,2] * elp +
                  NutationIAU2000A_LSCoeffsSOFA[i,3] * f   +
                  NutationIAU2000A_LSCoeffsSOFA[i,4] * d   +
                  NutationIAU2000A_LSCoeffsSOFA[i,5] * om;
     SinCos(fmod(Argument,RadiansPerRev),sinArg,cosArg);
     //   Accumulate current nutation term.
     dPsiLS:= dPsiLS +
              (NutationIAU2000A_LSCoeffsSOFA[i,6] + NutationIAU2000A_LSCoeffsSOFA[i,7]*t)*sinArg +
              NutationIAU2000A_LSCoeffsSOFA[i,8]*cosArg;
     dEpsLS:= dEpsLS +
              (NutationIAU2000A_LSCoeffsSOFA[i,9] + NutationIAU2000A_LSCoeffsSOFA[i,10]*t)*cosArg +
              NutationIAU2000A_LSCoeffsSOFA[i,11]*sinArg;
   end;

 // Fundamental (Delaunay) arguments
//    l = mean anomaly of the Moon (MHB2000)
  el:= fmod(2.35555598 + 8328.6914269551*t,RadiansPerRev);
//    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (in arcseconds) (MHB2000)
  f:= fmod(1.627905234 + 8433.466158131*t,RadiansPerRev);
//    D = mean elongation of the Moon from the Sun (MHB2000)
  d:= fmod(5.198466741 + 7771.3771468121*t,RadiansPerRev);
//    OM = mean longitude of the Moon's ascending node (MHB2000)
  om:= fmod(2.18243920 - 33757045*t,RadiansPerRev);

 //    lMe = mean longitude of Mercury (IERS 2003)
 lMe:= fmod(4.402608842 + 2608.7903141574 * t,RadiansPerRev);
 //    lVe = mean longitude of Venus (IERS 2003)
 lVe:= fmod(3.176146697 + 1021.3285546211 * t,RadiansPerRev);
 //    lE = mean longitude of Earth (IERS 2003)
 lEa:= fmod(1.753470314 + 628.3075849991 * t,RadiansPerRev);
 //    lMa = mean longitude of Mars (IERS 2003)
 lMa:= fmod(6.203480913 + 334.0612426700 * t,RadiansPerRev);
 //    lJu = mean longitude of Jupiter (IERS 2003)
 lJu:= fmod(0.599546497 + 52.9690962641 * t,RadiansPerRev);
 //    lSa = mean longitude of Saturn (IERS 2003)
 lSa:= fmod(0.874016757 + 21.3299104960 * t,RadiansPerRev);
 //    lUr = mean longitude of Uranus (IERS 2003)
 lUr:= fmod(5.481293872 + 7.4781598567 * t,RadiansPerRev);
 //    lNe = mean longitude of Neptune (MHB2000)
 lNe:= fmod(5.321159000 + 3.8127774000*t,RadiansPerRev);
 //    Pa = general precession on longitude
 Pa:= (0.024381750 + 0.00000538691 * t) * t;

//  Initialize Planetary nutation components
  dPsiPL:= 0;
  dEpsPL:= 0;
//  Sum the planetary nutation terms, ending with the biggest.
 for i:= High(NutationIAU2000A_PLCoeffsSOFA) downto Low(NutationIAU2000A_PLCoeffsSOFA) do
   begin
     //   Form argument for current term.
     Argument:= NutationIAU2000A_PLCoeffsSOFA[i,1] * el   +
                NutationIAU2000A_PLCoeffsSOFA[i,2] * f    +
                NutationIAU2000A_PLCoeffsSOFA[i,3] * d    +
                NutationIAU2000A_PLCoeffsSOFA[i,4] * om   +
                NutationIAU2000A_PLCoeffsSOFA[i,5] * lMe  +
                NutationIAU2000A_PLCoeffsSOFA[i,6] * lVe  +
                NutationIAU2000A_PLCoeffsSOFA[i,7] * lEa  +
                NutationIAU2000A_PLCoeffsSOFA[i,8] * lMa  +
                NutationIAU2000A_PLCoeffsSOFA[i,9] * lJu +
                NutationIAU2000A_PLCoeffsSOFA[i,10] * lSa +
                NutationIAU2000A_PLCoeffsSOFA[i,11] * lUr +
                NutationIAU2000A_PLCoeffsSOFA[i,12] * lNe +
                NutationIAU2000A_PLCoeffsSOFA[i,13] * Pa;
     SinCos(fmod(Argument,RadiansPerRev),sinArg,cosArg);
     //   Accumulate current nutation term.
     dPsiPL:= dPsiPL +
              NutationIAU2000A_PLCoeffsSOFA[i,14]*sinArg +
              NutationIAU2000A_PLCoeffsSOFA[i,15]*cosArg;
     dEpsPL:= dEpsPL +
              NutationIAU2000A_PLCoeffsSOFA[i,16]*sinArg +
              NutationIAU2000A_PLCoeffsSOFA[i,17]*cosArg;
   end;

//  Add Luni-Solar and Planetary components
  DeltaPsi:= dPsiLS + dPsiPL;
  DeltaEps:= dEpsLS + dEpsPL;

//    change to arcsecs
  DeltaPsi:= DeltaPsi*1e-7;
  DeltaEps:= DeltaEps*1e-7;

//    change to radians
  DeltaPsi:= DeltaPsi*RadiansPerArcSecond;
  DeltaEps:= DeltaEps*RadiansPerArcSecond;
end;

end.

