{
    almEarth is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2013, 2019 João Marcelo S. Vaz

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

procedure PrecessionIAU1976(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);
procedure PrecessionIAU2000(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);
procedure PrecessionIAU2006(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);

procedure faL_IERS2003(TDB_C: Double; out L: Double);
procedure faLP_IERS2003(TDB_C: Double; out LP: Double);
procedure faF_IERS2003(TDB_C: Double; out F: Double);
procedure faD_IERS2003(TDB_C: Double; out D: Double);
procedure faOM_IERS2003(TDB_C: Double; out OM: Double);
procedure faMe_IERS2003(TDB_C: Double; out Me: Double);
procedure faVe_IERS2003(TDB_C: Double; out Ve: Double);
procedure faEa_IERS2003(TDB_C: Double; out Ea: Double);
procedure faMa_IERS2003(TDB_C: Double; out Ma: Double);
procedure faJu_IERS2003(TDB_C: Double; out Ju: Double);
procedure faSa_IERS2003(TDB_C: Double; out Sa: Double);
procedure faUr_IERS2003(TDB_C: Double; out Ur: Double);
procedure faNe_IERS2003(TDB_C: Double; out Ne: Double);
procedure faPA_IERS2003(TDB_C: Double; out Pa: Double);

procedure NutationIAU1980(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
procedure NutationIAU2000A_IERS2003(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
procedure NutationIAU2000A(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
procedure NutationIAU2000B(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);

procedure EarthRotationAngleIAU2000(UT1: TJulianDate; out ERA: Double);
procedure GreenwichMeanSiderealTimeIAU1982(UT1: TJulianDate; out GMST: Double);
procedure GreenwichMeanSiderealTimeIAU2000(UT1: TJulianDate; TDB: TJulianDate; out GMST: Double);
procedure GreenwichMeanSiderealTimeIAU2006(UT1: TJulianDate; TDB: TJulianDate; out GMST: Double);

procedure EquationOfEquinoxes(DeltaPsi, EpsA, CT: Double; out EqEq: Double);
procedure EquationOfEquinoxesCT_IAU1994(TDB: TJulianDate; out CT: Double); overload;
procedure EquationOfEquinoxes_IAU1994(TDB: TJulianDate; out EqEq: Double); overload;
procedure EquationOfEquinoxes_IAU2000(TDB: TJulianDate; DeltaPsi, EpsA: Double; out EqEq: Double); overload;
procedure EquationOfEquinoxesCT_IAU2000(TDB: TJulianDate; out CT: Double); overload;
procedure EquationOfEquinoxes_IAU2000A(TDB: TJulianDate; out EqEq: Double); overload;
procedure EquationOfEquinoxes_IAU2000B(TDB: TJulianDate; out EqEq: Double); overload;
procedure EquationOfEquinoxes_IAU2006A(TDB: TJulianDate; out EqEq: Double); overload;

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
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

//  Precession angles (Lieske et al. 1977) with IAU 2000 precession corrections
  Eps0  := 84381.448; // obliquity of ecliptic at J2000.0 (in arcseconds)
  EpsA  := Eps0 + (-   46.84024 + (- 0.00059 + (  0.001813)*t)*t)*t;
  PsiA  :=        (  5038.47875 + (- 1.07259 + (- 0.001147)*t)*t)*t;
  ChiA  :=        (    10.5526  + (- 2.38064 + (- 0.001125)*t)*t)*t;
  OmegaA:= Eps0 + (-    0.02524 + (  0.05127 + (- 0.007726)*t)*t)*t;

//  change to radians
  Eps0  := Eps0*RadiansPerArcSecond;
  EpsA  := EpsA*RadiansPerArcSecond;
  PsiA  := PsiA*RadiansPerArcSecond;
  ChiA  := ChiA*RadiansPerArcSecond;
  OmegaA:= OmegaA*RadiansPerArcSecond;
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

procedure faL_IERS2003(TDB_C: Double; out L: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//              Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation: mean anomaly of the Moon (l)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  L: radians
begin
  L:= RadiansPerArcSecond*(485868.249036 + (1717915923.2178 + (31.8792 + (0.051635 - 0.00024470*TDB_C)*TDB_C)*TDB_C)*TDB_C);
  L:= fmod(L,RadiansPerRev);
end;

procedure faLP_IERS2003(TDB_C: Double; out LP: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean anomaly of the Sun (lp)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  LP: radians
begin
  LP:= RadiansPerArcSecond*(1287104.793048 + (129596581.0481 + (-0.5532 + (0.000136 - 0.00001149*TDB_C)*TDB_C)*TDB_C)*TDB_C);
  LP:= fmod(LP,RadiansPerRev);
end;

procedure faF_IERS2003(TDB_C: Double; out F: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean argument of the latitude of the Moon (f)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  F: radians
begin
  F:= RadiansPerArcSecond*(335779.526232 + (1739527262.8478 + (-12.7512 + (-0.001037 + 0.00000417*TDB_C)*TDB_C)*TDB_C)*TDB_C);
  F:= fmod(F,RadiansPerRev);
end;

procedure faD_IERS2003(TDB_C: Double; out D: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean elongation of the Moon from the Sun (d)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  D: radians
begin
  D:= RadiansPerArcSecond*(1072260.703692 + (1602961601.2090 + (-6.3706 + (0.006593 - 0.00003169*TDB_C)*TDB_C)*TDB_C)*TDB_C);
  D:= fmod(D,RadiansPerRev);
end;

procedure faOM_IERS2003(TDB_C: Double; out OM: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of the Moon’s ascending node (om)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  OM: radians
begin
  OM:= RadiansPerArcSecond*(450160.398036 + (-6962890.5431 + (7.4722 + (0.007702 - 0.00005939*TDB_C)*TDB_C)*TDB_C)*TDB_C);
  OM:= fmod(OM,RadiansPerRev);
end;

procedure faMe_IERS2003(TDB_C: Double; out Me: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//              Souchay, J., Loysel, B., Kinoshita, H., Folgueira, M. 1999, Astron.Astrophys.Supp.Ser. 135, 111
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of Mercury (Me)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Me: radians
begin
  Me:= 4.402608842 + 2608.7903141574 * TDB_C;
  Me:= fmod(Me,RadiansPerRev);
end;

procedure faVe_IERS2003(TDB_C: Double; out Ve: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//              Souchay, J., Loysel, B., Kinoshita, H., Folgueira, M. 1999, Astron.Astrophys.Supp.Ser. 135, 111
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of Venus (Ve)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Ve: radians
begin
  Ve:= 3.176146697 + 1021.3285546211 * TDB_C;
  Ve:= fmod(Ve,RadiansPerRev);
end;

procedure faEa_IERS2003(TDB_C: Double; out Ea: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//              Souchay, J., Loysel, B., Kinoshita, H., Folgueira, M. 1999, Astron.Astrophys.Supp.Ser. 135, 111
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of Earth (Ea)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Ea: radians
begin
  Ea:= 1.753470314 + 628.3075849991 * TDB_C;
  Ea:= fmod(Ea,RadiansPerRev);
end;

procedure faMa_IERS2003(TDB_C: Double; out Ma: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//              Souchay, J., Loysel, B., Kinoshita, H., Folgueira, M. 1999, Astron.Astrophys.Supp.Ser. 135, 111
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of Mars (Ma)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Ma: radians
begin
  Ma:= 6.203480913 + 334.0612426700 * TDB_C;
  Ma:= fmod(Ma,RadiansPerRev);
end;

procedure faJu_IERS2003(TDB_C: Double; out Ju: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//              Souchay, J., Loysel, B., Kinoshita, H., Folgueira, M. 1999, Astron.Astrophys.Supp.Ser. 135, 111
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of Jupiter (Ju)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Ju: radians
begin
  Ju:= 0.599546497 + 52.9690962641 * TDB_C;
  Ju:= fmod(Ju,RadiansPerRev);
end;

procedure faSa_IERS2003(TDB_C: Double; out Sa: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//              Souchay, J., Loysel, B., Kinoshita, H., Folgueira, M. 1999, Astron.Astrophys.Supp.Ser. 135, 111
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of Saturn (Sa)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Sa: radians
begin
  Sa:= 0.874016757 + 21.3299104960 * TDB_C;
  Sa:= fmod(Sa,RadiansPerRev);
end;

procedure faUr_IERS2003(TDB_C: Double; out Ur: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//              Souchay, J., Loysel, B., Kinoshita, H., Folgueira, M. 1999, Astron.Astrophys.Supp.Ser. 135, 111
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of Uranus (Ur)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Ur: radians
begin
  Ur:= 5.481293872 + 7.4781598567 * TDB_C;
  Ur:= fmod(Ur,RadiansPerRev);
end;

procedure faNe_IERS2003(TDB_C: Double; out Ne: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: mean longitude of Neptune (Ne)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Ne: radians
begin
  Ne:= 5.311886287 + 3.8133035638*TDB_C;
  Ne:= fmod(Ne,RadiansPerRev);
end;

procedure faPA_IERS2003(TDB_C: Double; out Pa: Double);
//  REFERENCE:  IAU 2000 Theory of Nutation Model
//		Simon, J.−L., Bretagnon, P., Chapront, J., Chapront−Touze, M., Francou, G., Laskar, J. 1994, Astron.Astrophys. 282, 663−683
//		McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003), IERS Technical Note No. 32, BKG (2004)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the Fundamental arguments for nutation etc: general accumulated precession in longitude (Pa)
//  TDB_C = TDB, Julian centuries since J2000.0
//  result =  Pa: radians
begin
  Pa:= (0.024381750 + 0.00000538691 * TDB_C) * TDB_C;
  Pa:= fmod(Pa,RadiansPerRev);
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

procedure NutationIAU2000A_IERS2003(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
//  REFERENCE:  IAU 2000A Theory of Nutation Model
//              IERS Conventions (2003)
//  This routine computes the two Nutation angles in longitude and obliquity, with
//  respect to the equinox and ecliptic of date, using the IAU 2000A Theory of Nutation Model
//  N = Rx(-(EpsA + DeltaEps)).Rz(-DeltaPsi).Rx(EpsA)
//  result = Nutation Angles (DeltaPsi, DeltaEps) (in arcsecs)
//  uses: TDB
var
  t: Double;
  FundamentalArguments: array [1..14] of Double;
  Argument, sinArg, cosArg: Double;
  dPsiLS, dEpsLS, dPsiPL, dEpsPL: Double;
  j, i: Integer;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

  // Fundamental (Delaunay) arguments (arcseconds converted to radians)
  //    l = mean anomaly of the Moon (IERS 2003)
  faL_IERS2003(t,FundamentalArguments[1]);
  //    l' = mean anomaly of the Sun (IERS 2003)
  faLP_IERS2003(t,FundamentalArguments[2]);
  //    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (IERS 2003)
  faF_IERS2003(t,FundamentalArguments[3]);
  //    D = mean elongation of the Moon from the Sun (IERS 2003)
  faD_IERS2003(t,FundamentalArguments[4]);
  //    OM = mean longitude of the Moon's ascending node (IERS 2003)
  faOM_IERS2003(t,FundamentalArguments[5]);

  // Planetary longitudes, Mercury through Neptune (Souchay et al. 1999).
  //    lMe = mean longitude of Mercury (IERS 2003)
  faMe_IERS2003(t,FundamentalArguments[6]);
  //    lVe = mean longitude of Venus (IERS 2003)
  faVe_IERS2003(t,FundamentalArguments[7]);
  //    lE = mean longitude of Earth (IERS 2003)
  faEa_IERS2003(t,FundamentalArguments[8]);
  //    lMa = mean longitude of Mars (IERS 2003)
  faMa_IERS2003(t,FundamentalArguments[9]);
  //    lJu = mean longitude of Jupiter (IERS 2003)
  faJu_IERS2003(t,FundamentalArguments[10]);
  //    lSa = mean longitude of Saturn (IERS 2003)
  faSa_IERS2003(t,FundamentalArguments[11]);
  //    lUr = t longitude of Uranus (IERS 2003)
  faUr_IERS2003(t,FundamentalArguments[12]);
  //    lNe = mean longitude of Neptune (MHB2000)
  faNe_IERS2003(t,FundamentalArguments[13]);
  //    Pa = general precession on longitude
  faPa_IERS2003(t,FundamentalArguments[14]);

//  Initialize Luni-Solar nutation components
  dPsiLS:= 0;
  dEpsLS:= 0;
//  Sum the luni-solar nutation terms, ending with the biggest.
 for i:= High(NutationIAU2000A_LSCoeffsIERS2003) downto Low(NutationIAU2000A_LSCoeffsIERS2003) do
   begin
     //   Form argument for current term.
     Argument:= 0;
     for j:= 1 to 5 do
       Argument:= Argument + NutationIAU2000A_LSCoeffsIERS2003[i,j] * FundamentalArguments[j];
     SinCos(Argument,sinArg,cosArg);
     //   Accumulate current nutation term.
     dPsiLS:= dPsiLS +
              (NutationIAU2000A_LSCoeffsIERS2003[i,6] + NutationIAU2000A_LSCoeffsIERS2003[i,7]*t)*sinArg +
              (NutationIAU2000A_LSCoeffsIERS2003[i,8] + NutationIAU2000A_LSCoeffsIERS2003[i,9]*t)*cosArg;
     dEpsLS:= dEpsLS +
              (NutationIAU2000A_LSCoeffsIERS2003[i,10] + NutationIAU2000A_LSCoeffsIERS2003[i,11]*t)*cosArg +
              (NutationIAU2000A_LSCoeffsIERS2003[i,12] + NutationIAU2000A_LSCoeffsIERS2003[i,13]*t)*sinArg;
   end;

//  Initialize Planetary nutation components
  dPsiPL:= 0;
  dEpsPL:= 0;
//  Sum the planetary nutation terms, ending with the biggest.
 for i:= High(NutationIAU2000A_PLCoeffsIERS2003) downto Low(NutationIAU2000A_PLCoeffsIERS2003) do
   begin
     //   Form argument for current term.
     Argument:= 0;
     for j:= 1 to 14 do
       Argument:= Argument + NutationIAU2000A_PLCoeffsIERS2003[i,j] * FundamentalArguments[j];
     SinCos(Argument,sinArg,cosArg);
     //   Accumulate current nutation term.
     dPsiPL:= dPsiPL +
              NutationIAU2000A_PLCoeffsIERS2003[i,15]*sinArg +
              NutationIAU2000A_PLCoeffsIERS2003[i,16]*cosArg;
     dEpsPL:= dEpsPL +
              NutationIAU2000A_PLCoeffsIERS2003[i,17]*cosArg +
              NutationIAU2000A_PLCoeffsIERS2003[i,18]*sinArg;
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

procedure NutationIAU2000A(TDB: TJulianDate; out DeltaPsi, DeltaEps: Double);
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
  faL_IERS2003(t,el);
  //    l' = mean anomaly of the Sun (MHB2000)
  elp:= fmod(RadiansPerArcSecond*(1287104.79305 + (129596581.0481 +
                             (-0.5532 + (0.000136 - 0.00001149*t)*t)*t)*t),RadiansPerRev);
  //    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (IERS 2003)
  faF_IERS2003(t,f);
  //    D = mean elongation of the Moon from the Sun (MHB2000)
  d:= fmod(RadiansPerArcSecond*(1072260.70369 + (1602961601.2090 +
                             (-6.3706 + (0.006593 - 0.00003169*t)*t)*t)*t),RadiansPerRev);
  //    OM = mean longitude of the Moon's ascending node (IERS 2003)
  faOM_IERS2003(t,om);

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
 // Convert from 0.1 microarcsec units to radians
 dPsiLS:= dPsiLS*RadiansPerArcSecond/1e7;
 dEpsLS:= dEpsLS*RadiansPerArcSecond/1e7;

 // Fundamental (Delaunay) arguments
//    l = mean anomaly of the Moon (MHB2000)
  el:= fmod(2.35555598 + 8328.6914269554*t,RadiansPerRev);
//    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (in arcseconds) (MHB2000)
  f:= fmod(1.627905234 + 8433.466158131*t,RadiansPerRev);
//    D = mean elongation of the Moon from the Sun (MHB2000)
  d:= fmod(5.198466741 + 7771.3771468121*t,RadiansPerRev);
//    OM = mean longitude of the Moon's ascending node (MHB2000)
  om:= fmod(2.18243920 - 33.757045*t,RadiansPerRev);

 //    lMe = mean longitude of Mercury (IERS 2003)
 faMe_IERS2003(t,lMe);
 //    lVe = mean longitude of Venus (IERS 2003)
 faVe_IERS2003(t,lVe);
 //    lE = mean longitude of Earth (IERS 2003)
 faEa_IERS2003(t,lEa);
 //    lMa = mean longitude of Mars (IERS 2003)
 faMa_IERS2003(t,lMa);
 //    lJu = mean longitude of Jupiter (IERS 2003)
 faJu_IERS2003(t,lJu);
 //    lSa = mean longitude of Saturn (IERS 2003)
 faSa_IERS2003(t,lSa);
 //    lUr = t longitude of Uranus (IERS 2003)
 faUr_IERS2003(t,lUr);
 //    lNe = mean longitude of Neptune (MHB2000)
 lNe:= fmod(5.321159000 + 3.8127774000*t,RadiansPerRev);
 //    Pa = general precession on longitude
 faPa_IERS2003(t,Pa);

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
 // Convert from 0.1 microarcsec units to radians
 dPsiPL:= dPsiPL*RadiansPerArcSecond/1e7;
 dEpsPL:= dEpsPL*RadiansPerArcSecond/1e7;

//  Add Luni-Solar and Planetary components
  DeltaPsi:= dPsiLS + dPsiPL;
  DeltaEps:= dEpsLS + dEpsPL;
end;

procedure EarthRotationAngleIAU2000(UT1: TJulianDate; out ERA: Double);
//  REFERENCE:  Capitaine N., Guinot B. and McCarthy D.D, Astronomy & Astrophysics, 355, 398-405 (2000)
//              IERS Conventions (2010). Gérard Petit and Brian Luzum (eds.). (IERS Technical Note 36)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  result = Earth Rotation Angle (ERA) (in radians)
//  uses: UT1
var
  f,t: Double;
begin
 t:= UT1 - J2000;
 //Fractional part of T (days)
 f:=  fmod(t, 1.0);
 // compute Earth Rotation Angle
 ERA:= RadiansPerRev*(f + 0.7790572732640 + 0.00273781191135448 * t);
 // put in range (2Pi)
 ERA:= fmod(ERA,RadiansPerRev);
end;

procedure GreenwichMeanSiderealTimeIAU1982(UT1: TJulianDate; out GMST: Double);
//  result = Greenwich Mean Sidereal Time (IAU 1982 Model): radians
//  reference: GMST: Aoki et al., Astron. Astrophys. 105, 359-361 (1982)
//             International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  uses: UT1, fmod
//  used by:
var
  t, F: Double;
begin
  t:= (UT1 - J2000)/JulianDaysPerCentury;
  // fractional part of JD(UT1)
  F:= SecondsPerDay * Frac(UT1);

  // compute Greenwich Mean Sidereal Time (in seconds)
  // The first constant has to be adjusted by 12 hours because the UT1 is supplied as a Julian date, which begins at noon.           */
  GMST:= (24110.54841 - SecondsPerDay/2 + (8640184.812866 + (0.093104 - 6.2e-6*t)*t)*t) + F;
  // change to radians
  GMST:= GMST*RadiansPerRev/SecondsPerDay;
  // put in range (2Pi)
  GMST:= fmod(GMST,RadiansPerRev);
end;

procedure GreenwichMeanSiderealTimeIAU2000(UT1: TJulianDate; TDB: TJulianDate;
  out GMST: Double);
//  result = Greenwich Mean Sidereal Time (IAU 2000 Model): radians
//  references: ERA: Capitaine N., Guinot B. and McCarthy D.D, Astronomy & Astrophysics, 355, 398-405 (2000)
//              GMST: Capitaine et al, Astron. Astrophys. 406, 1135-1149 (2003)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  uses: UT1, TDB, fmod
var
  t, ERA: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;
  // compute IAU2000 Earth Rotation Angle
  EarthRotationAngleIAU2000(UT1,ERA);
  // compute IAU2000 Greenwich Mean Sidereal Time (in arcseconds)
  GMST:= ERA + (0.014506 + (4612.15739966 + (1.39667721 + (- 0.00009344 + (0.00001882)*T)*T)*T)*T)*RadiansPerArcSecond;
   // put in range (2Pi)
  GMST:= fmod(GMST,RadiansPerRev);
end;

procedure GreenwichMeanSiderealTimeIAU2006(UT1: TJulianDate; TDB: TJulianDate;
  out GMST: Double);
//  result = Greenwich Mean Sidereal Time (IAU 2006 Model): radians
//  references: ERA: Capitaine N., Guinot B. and McCarthy D.D, Astronomy & Astrophysics, 355, 398-405 (2000)
//              GMST: Capitaine et al, Astron. Astrophys. 412, 567-586 (2003)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  uses: UT1, TDB, fmod
var
  t, ERA: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;
  // compute IAU2000 Earth Rotation Angle
  EarthRotationAngleIAU2000(UT1,ERA);
  // compute IAU2000 Greenwich Mean Sidereal Time (in radians)
  GMST:= ERA + (0.014506 + (4612.156534 + (1.3915817 + (- 0.00000044 + (- 0.000029956 - 0.0000000368*T)*T)*T)*T)*T)*RadiansPerArcSecond;
   // put in range (2Pi)
  GMST:= fmod(GMST,RadiansPerRev);
end;

procedure EquationOfEquinoxes(DeltaPsi, EpsA, CT: Double; out EqEq: Double);
begin
  EqEq:= DeltaPsi*Cos(EpsA) + CT;
end;

procedure EquationOfEquinoxesCT_IAU1994(TDB: TJulianDate; out CT: Double);
//  reference: GAST: Capitaine N. & Gontier A. M., Astronomy & Astrophysics, 275, 645-650 (1993)
//             Eq Eq: IAU Resolution C7, Recommendation 3 (1994)
//             International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//             "NOVAS: Naval Observatory Vector Astrometry Subroutines" : NOVAS-C Version 2.0.1 (10 Dec 99)
//  result = Equation Of Equinoxes, IAU 1994 model: radians
//           GAST = GMST + Equation of the Equinoxes
var
  t, Omega: Double;
begin
 t:= (TDB - J2000)/JulianDaysPerCentury;
 Omega:= 2.1824386243609943 + t*(-33.75704593375351 + t*(3.614285992671591e-5   + 3.878509448876288e-8 * t));
 // compute Complementary Terms (in arcseconds)
 CT:= 0.00264*sin(Omega) + 0.000063*sin(Omega+Omega);
 // change to Radians
 CT:= CT*RadiansPerArcSecond;
end;

procedure EquationOfEquinoxes_IAU1994(TDB: TJulianDate; out EqEq: Double);
var
  Eps0, EpsA,PsiA,ChiA,OmegaA, DeltaPsi, DeltaEps, CT: Double;
begin
  PrecessionIAU1976(TDB, Eps0, EpsA,PsiA,ChiA,OmegaA);
  NutationIAU1980(TDB, DeltaPsi, DeltaEps);
  EquationOfEquinoxesCT_IAU1994(TDB, CT);

  EquationOfEquinoxes(DeltaPsi, EpsA, CT, EqEq);
end;

procedure EquationOfEquinoxesCT_IAU2000(TDB: TJulianDate; out CT: Double);
//  references: GMST: Capitaine et al, Astron. Astrophys. 406, 1135-149 (2003)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  result = Equation Of Equinoxes (IAU 2000 Model): radians
const
// number of coefficients for t^0
    NE0 = 33;
// number of coefficients for t^1
    NE1 = 1;

//  Argument coefficients for t^0
    KE0: array[1..NE0,1..14] of Integer =
//       l,  l', F,  D, Om, LMe,LVe,LE, LMa,LJu,LSa, LU, LN, pA
     (  (0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  2,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  0,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  1,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  1,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (1,  0,  0,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  1,  2, -2,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  1,  2, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  4, -4,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  1, -1,  1,  0, -8, 12,  0,  0,  0,  0,  0,  0),
        (0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  2,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (1,  0,  2,  0,  3,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (1,  0,  2,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  2, -2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  1, -2,  2, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  1, -2,  2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  0,  0,  0,  0,  8,-13,  0,  0,  0,  0,  0, -1),
        (0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (2,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (1,  0,  0, -2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  1,  2, -2,  2,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (1,  0,  0, -2, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  4, -2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (0,  0,  2, -2,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (1,  0, -2,  0, -3,  0,  0,  0,  0,  0,  0,  0,  0,  0),
        (1,  0, -2,  0, -1,  0,  0,  0,  0,  0,  0,  0,  0,  0));

//  Argument coefficients for t^1
    KE1: array[1..NE1,1..14] of Integer =
//       l,  l', F,  D, Om, LMe,LVe,LE, LMa,LJu,LSa, LU, LN, pA
     (  (0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0));

//  Sine and cosine coefficients for t^0
    SE0: array[1..NE0,1..2] of Extended =
//             sin                cos
     (  ( +2640.96e-6,          -0.39e-6),
        (   +63.52e-6,          -0.02e-6),
        (   +11.75e-6,          +0.01e-6),
        (   +11.21e-6,          +0.01e-6),
        (    -4.55e-6,          +0.00e-6),
        (    +2.02e-6,          +0.00e-6),
        (    +1.98e-6,          +0.00e-6),
        (    -1.72e-6,          +0.00e-6),
        (    -1.41e-6,          -0.01e-6),
        (    -1.26e-6,          -0.01e-6),
        (    -0.63e-6,          +0.00e-6),
        (    -0.63e-6,          +0.00e-6),
        (    +0.46e-6,          +0.00e-6),
        (    +0.45e-6,          +0.00e-6),
        (    +0.36e-6,          +0.00e-6),
        (    -0.24e-6,          -0.12e-6),
        (    +0.32e-6,          +0.00e-6),
        (    +0.28e-6,          +0.00e-6),
        (    +0.27e-6,          +0.00e-6),
        (    +0.26e-6,          +0.00e-6),
        (    -0.21e-6,          +0.00e-6),
        (    +0.19e-6,          +0.00e-6),
        (    +0.18e-6,          +0.00e-6),
        (    -0.10e-6,          +0.05e-6),
        (    +0.15e-6,          +0.00e-6),
        (    -0.14e-6,          +0.00e-6),
        (    +0.14e-6,          +0.00e-6),
        (    -0.14e-6,          +0.00e-6),
        (    +0.14e-6,          +0.00e-6),
        (    +0.13e-6,          +0.00e-6),
        (    -0.11e-6,          +0.00e-6),
        (    +0.11e-6,          +0.00e-6),
        (    +0.11e-6,          +0.00e-6));

//  Sine and cosine coefficients for t^1
    SE1: array[1..NE1,1..2] of Extended =
//             sin                cos
     (  (    -0.87e-6,          +0.00e-6));
var
  t: Double;
  S0, S1: Double;
  Argument, sinArg, cosArg: Double;
  FundamentalArguments: array [1..14] of Double;
  i, j: Integer;
begin
 // compute complementary terms
 t:= (TDB - J2000)/JulianDaysPerCentury;

  //    l = mean anomaly of the Moon (in arcseconds)
 faL_IERS2003(t,FundamentalArguments[1]);
 //    l' = mean anomaly of the Sun (in arcseconds)
 faLP_IERS2003(t,FundamentalArguments[2]);
 //    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (in arcseconds)
 faF_IERS2003(t,FundamentalArguments[3]);
 //    D = mean elongation of the Moon from the Sun (in arcseconds)
 faD_IERS2003(t,FundamentalArguments[4]);
 //    OM = mean longitude of the Moon's ascending node (in arcseconds)
 faOM_IERS2003(t,FundamentalArguments[5]);

 //    lMe = mean longitude of Mercury
 faMe_IERS2003(t,FundamentalArguments[6]);
 //    lVe = mean longitude of Venus
 faVe_IERS2003(t,FundamentalArguments[7]);
 //    lE = mean longitude of Earth
 faEa_IERS2003(t,FundamentalArguments[8]);
 //    lMa = mean longitude of Mars
 faMa_IERS2003(t,FundamentalArguments[9]);
 //    lJu = mean longitude of Jupiter
 faJu_IERS2003(t,FundamentalArguments[10]);
 //    lSa = mean longitude of Saturn
 faSa_IERS2003(t,FundamentalArguments[11]);
 //    lUr = mean longitude of Uranus
 faUr_IERS2003(t,FundamentalArguments[12]);
 //    lNe = mean longitude of Neptune
 faNe_IERS2003(t,FundamentalArguments[13]);
 //    Pa = general precession on longitude
 faPa_IERS2003(t,FundamentalArguments[14]);

//  Evaluate the EE complementary terms.
 // Argument = Soma(Nj*Fj)
 // S0 = Soma[SEs0i*sin(Argument) + SEc0*cos(Argument)]
 // Argument = Soma(Nj*Fj)
 // S1 = Soma[SEs1i*sin(Argument) + SEc1*cos(Argument)]*T
 S0:= 0;
 S1:= 0;
 for i:= NE0 downto 1 do
   begin
     Argument:= 0;
     for j:= 1 to 14 do
       Argument:= Argument + KE0[i,j] * FundamentalArguments[j];
     SinCos(Argument,sinArg,cosArg);
     S0:= S0 + (SE0[i,1]*sinArg + SE0[i,2]*cosArg);
   end;
 for i:= NE1 downto 1 do
   begin
     //   Form argument for current term
     Argument:= 0;
     for j:= 1 to 14 do
       Argument:= Argument + KE1[i,j] * FundamentalArguments[j];
     SinCos(Argument,sinArg,cosArg);
     S1:= S1 + (SE1[i,1]*sinArg + SE1[i,2]*cosArg);
   end;
 // CT in arcseconds
 CT:= S0 + S1*t;
 // change to Radians
 CT:= CT*RadiansPerArcSecond;
end;

procedure EquationOfEquinoxes_IAU2000(TDB: TJulianDate; DeltaPsi, EpsA: Double;
  out EqEq: Double);
//  references: GMST: Capitaine et al, Astron. Astrophys. 406, 1135-149 (2003)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  result = Equation Of Equinoxes (IAU 2000 Model): radians

var
  CT: Double;
begin
 EquationOfEquinoxesCT_IAU2000(TDB, CT);

 EquationOfEquinoxes(DeltaPsi, EpsA, CT, EqEq);
end;

procedure EquationOfEquinoxes_IAU2000A(TDB: TJulianDate; out EqEq: Double);
var
  Eps0, EpsA,PsiA,ChiA,OmegaA, DeltaPsi, DeltaEps: Double;
begin
  PrecessionIAU2000(TDB, Eps0, EpsA,PsiA,ChiA,OmegaA);
  NutationIAU2000A(TDB, DeltaPsi, DeltaEps);

  EquationOfEquinoxes_IAU2000(TDB, DeltaPsi, EpsA, EqEq);
end;

procedure EquationOfEquinoxes_IAU2000B(TDB: TJulianDate; out EqEq: Double);
var
  Eps0, EpsA,PsiA,ChiA,OmegaA, DeltaPsi, DeltaEps: Double;
begin
  PrecessionIAU2000(TDB, Eps0, EpsA,PsiA,ChiA,OmegaA);
  NutationIAU2000B(TDB, DeltaPsi, DeltaEps);

  EquationOfEquinoxes_IAU2000(TDB, DeltaPsi, EpsA, EqEq);
end;

procedure EquationOfEquinoxes_IAU2006A(TDB: TJulianDate; out EqEq: Double);
var
  Eps0, EpsA,PsiA,ChiA,OmegaA, DeltaPsi, DeltaEps: Double;
begin
  PrecessionIAU2006(TDB, Eps0, EpsA,PsiA,ChiA,OmegaA);
  NutationIAU2000A(TDB, DeltaPsi, DeltaEps);

  EquationOfEquinoxes_IAU2000(TDB, DeltaPsi, EpsA, EqEq);
end;






end.

