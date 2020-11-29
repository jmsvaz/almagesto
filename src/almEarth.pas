{
    almEarth is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2013, 2019, 2020 João Marcelo S. Vaz

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

{  This unit has Earth orientation routines.

The transformation to be used to relate the International Terrestrial Reference System (ITRS)
to the Geocentric Celestial Reference System (GCRS) at the date t of the observation can be written as:

[GCRS] = Q(t).R(t).W(t) [ITRS], where

Q(t) = the transformation matrix arising from the motion of the celestial pole in the celestial reference system
R(t) - the transformation matrix arising from the rotation of the Earth around the axis associated with the pole
W(t) = the transformation matrix arising from polar motion

According to IAU 2006 Resolution B2, the system at date t as realized from the ITRS by applying the
transformation W(t) in both procedures is the “Terrestrial Intermediate Reference System” (TIRS). It
uses the CIP as its z-axis and the TIO as its x-axis.

The CIO based procedure realizes an intermediate celestial reference system at date t that uses the CIP
as its z-axis and the CIO as its x-axis. According to IAU 2006 Resolution B2, it is called the “Celestial
Intermediate Reference System” (CIRS). It uses the “Earth Rotation Angle” in the transformation matrix
R(t), and the two coordinates of the CIP in the GCRS (Capitaine, 1990) in the transformation matrix
Q(t).

The classical procedure realizes an intermediate celestial reference system at date t that uses the CIP as
its z-axis and the equinox as its x-axis. It is called the “true equinox and equator of date system”. It uses
apparent Greenwich Sidereal Time (GST) in the transformation matrix R(t) and the classical precession
and nutation parameters in the transformation matrix Q(t).

Each of the transformation matrix components W(t), R(t) and Q(t) is a series of rotations about the
axes 1, 2 and 3 of the coordinate frame. In the following, R1 , R2 and R3 denote rotation matrices
with positive angle about the axes 1, 2 and 3.

W(t) = R3(−s')·R2(xp)·R1(yp),

xp and yp being the “polar coordinates” of the Celestial Intermediate Pole (CIP) in the ITRS and s' being a
quantity, named “TIO locator”, which provides the position of the TIO on the equator of the CIP corresponding
to the kinematical definition of the “non-rotating” origin (NRO) in the ITRS when the CIP is moving with
respect to the ITRS due to polar motion.

The standard pole coordinates to be used for the parameters xp and yp ,
if not estimated from the observations, are those published by the IERS
with additional components to account for the effects of ocean tides and
for nutation terms with periods less than two days.
(xp, yp) = (x,y)IERS + (∆x, ∆y)oceantides + (∆x, ∆y)libration

R(t) = R3(−ERA), CIO based

where ERA is the Earth Rotation Angle between the CIO and the TIO at date t on the equator of the CIP,
which provides a rigorous definition of the sidereal rotation of the Earth.

Q(t) = R3(−E)·R2(−d)·R3(E)·R3(s), CIO based

E and d being such that the coordinates of the CIP in the GCRS are:
X = sin d cos E,
Y = sin d sin E,
Z = cos d,

and s being a quantity, named “CIO locator”, which provides the position of the CIO on the equator of the
CIP corresponding to the kinematical definition of the NRO in the GCRS when the CIP is moving with respect
to the GCRS, between the reference epoch and the date t due to precession and nutation.

R(t) = R3(−GST), equinox based

wher GST is the Apparent Greenwich Sidereal Time, i.e. the angle between the
equinox and the TIO, to represent the Earth’s angle of rotation, instead of the ERA.

Q(t) = B.P.N

B = R3(-ẟα0).R2(-ẟψb.sin(ε0)).R1(ẟεb).
P(t) = R1(−ε0)·R3(ψA)·R1(ωA)·R3(−χA) or P(t) = R3(ζA)·R2(-θA)·R3(ZA)
N(t) = R1(-εA).R3(Δψ).R1(εA+Δε)


X = X̄ + ξ0 − ẟα0.Ȳ
Y = Ȳ + η0 + ẟα0.X̄

X̄ = sin(ω).sin(ψ)
Ȳ = −sin(ε).cos(ω) + cos(ε0).sin(ω).cos(ψ)

ω = ωA + ∆ε1
ψ = ψA + ∆ψ1

∆ψ1.sin(ωA) = ∆ψ.sin(εA).cos(χA) − ∆ε.sin(χA)
∆ε1 = ∆ψ.sin(εA).sin(χA) + ∆ε.cos(χA)

δX = δψ.sin(εA) + (ψA.cos(ε0) − χA).δε
δY = δε − (ψA.cos(ε0) − χA).δψ.sin(εA)



sources: McCarthy, IERS Conventions (1996), IERS Technical Note 21, 1996
         McCarthy & Petit, IERS Conventions (2003), IERS Technical Note 32, 2003
         Luzum & Petit, IERS Conventions (2010), IERS Technical Note 36, 2010
}

unit almEarth;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, almBase;

type
  TPrecessionNutationModel = (pnNewcomb, pnIAU1948, pnIAU1976, pnIAU1980, pnIAU1982, pnIAU1994, pnIERS1996, pnIAU2000A, pnIAU2000B, pnIAU2006A, pnIAU2006B);

  { TEarthOrientation }

  TEarthOrientation = class
    private
      fCT: Double;
      fChiA: Double;
      fDeltaEps: Double;
      fDeltaPsi: Double;
      fEps0: Double;
      fEpsA: Double;
      fGAST: Double;
      fGMST: Double;
      fOmegaA: Double;
      fPsiA: Double;
      fTrueObliquity: Double;
      FTDB: TJulianDate;
      FUT1: TJulianDate;
      fPrecessionNutationModel: TPrecessionNutationModel;
      procedure SetPrecessionNutationModel(AValue: TPrecessionNutationModel);
      procedure SetTDB(AValue: TJulianDate);
      procedure SetUT1(AValue: TJulianDate);

    public
      constructor Create;
      constructor Create(aUT1: TJulianDate; aTDB: TJulianDate);
      procedure Update(aUT1: TJulianDate; aTDB: TJulianDate);
      property UT1: TJulianDate read FUT1 write SetUT1;
      property TDB: TJulianDate read FTDB write SetTDB;
      property PrecessionNutationModel: TPrecessionNutationModel read fPrecessionNutationModel write SetPrecessionNutationModel;

      //** Obliquity of the ecliptic at epoch (in radians)
      property Eps0: Double read fEps0;
      //** Precession in longitude, referred to the ecliptic of epoch (in radians)
      property PsiA: Double read fPsiA;
      //** Precession in obliquity, referred to the ecliptic of epoch (in radians)
      property OmegaA: Double read fOmegaA;
      //** Planetary precession along the Equator (in radians)
      property ChiA: Double read fChiA;
      //** Nutation in longitude (in radians)
      property DeltaPsi: Double read fDeltaPsi;
      //** Nutation in obliquity (in radians)
      property DeltaEpsilon: Double read fDeltaEps;
      //** Mean obliquity of the ecliptic (in radians)
      property EpsA: Double read fEpsA;
      //** Mean obliquity of the ecliptic (in radians)
      property MeanObliquity: Double read fEpsA;
      //** True obliquity of the ecliptic (in radians)
      property TrueObliquity: Double read fTrueObliquity;
      //** Greenwich Mean Sidereal Time (in radians)
      property GreenwichMeanSiderealTime: Double read fGMST;
      //** Greenwich Apparent Sidereal Time (in radians)
      property GreenwichAparentSiderealTime: Double read fGAST;


  end;

function ObliquityJ2000IAU1980: Double;
function ObliquityJ2000IAU2006: Double;
function MeanObliquityIAU1980(TDB: TJulianDate): Double;
function MeanObliquityIAU2000(TDB: TJulianDate): Double;
function MeanObliquityIAU2006(TDB: TJulianDate): Double;

function FrameBiasInLongitudeIAU2000: Double;
function FrameBiasInObiquityIAU2000: Double;
function FrameBiasICRSRAIAU2000: Double;
function FrameBiasInXIAU2000: Double;
function FrameBiasInYIAU2000: Double;


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

function GreenwichSiderealTime(GMST, EqEq: Double): Double; overload;
function GreenwichSiderealTime(GMST, DeltaPsi, EpsA, CT: Double): Double; overload;
function GreenwichSiderealTime_IAU1994(UT1: TJulianDate; TDB: TJulianDate): Double;
function GreenwichSiderealTime_IAU2000A(UT1: TJulianDate; TDB: TJulianDate): Double;
function GreenwichSiderealTime_IAU2000B(UT1: TJulianDate; TDB: TJulianDate): Double;
function GreenwichSiderealTime_IAU2006A(UT1: TJulianDate; TDB: TJulianDate): Double;

function TIOLocatorSpIAU2000(TDB: TJulianDate): Double;

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

function ObliquityJ2000IAU1980: Double;
// reference: Capitaine et al, Astron. Astrophys. 412, 567-586 (2003)
//            Lieske, J., et al. (1977). Astron. & Astrophys. 58, 1-16. (IAU 1976 Precession Model)
//            Seidelmann, P.K. (1982) Celestial Mechanics 27, 79-106 (IAU 1980 Theory of Nutation)
// result = obliquity of ecliptic at J2000.0 (Eps0): radians
begin
  Result:= 84381.448;     // = 23°26'21".448 in arcseconds
  Result:= Result*RadiansPerArcSecond;
end;

function MeanObliquityIAU1980(TDB: TJulianDate): Double;
// reference: Capitaine et al, Astron. Astrophys. 412, 567-586 (2003)
//            Lieske, J., et al. (1977). Astron. & Astrophys. 58, 1-16. (IAU 1976 Precession Model)
//            Seidelmann, P.K. (1982) Celestial Mechanics 27, 79-106 (IAU 1980 Theory of Nutation)
// result = obliquity of ecliptic (EpsA): radians
var
  t: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;
  Result:= (-   46.8150 + (- 0.00059 + (  0.001813)*t)*t)*t;
  Result:= ObliquityJ2000IAU1980 + Result*RadiansPerArcSecond;
end;

function MeanObliquityIAU2000(TDB: TJulianDate): Double;
// reference: Capitaine et al, Astron. Astrophys. 412, 567-586 (2003)
// result = obliquity of ecliptic (EpsA): radians
var
  t: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;
  // IAU 1976 obliquity with precession-rate part of the IAU 2000 precession-nutation models
  Result:= (-   46.84024 + (- 0.00059 + (  0.001813)*t)*t)*t;
  Result:= ObliquityJ2000IAU1980 + Result*RadiansPerArcSecond;
end;

function ObliquityJ2000IAU2006: Double;
// reference: Capitaine et al, Astron. Astrophys. 412, 567-586 (2003)
// result = obliquity of ecliptic at J2000.0 (Eps0): radians
begin
  Result:= 84381.406;     // = 23°26'21".406 in arcseconds
  Result:= Result*RadiansPerArcSecond;
end;

function MeanObliquityIAU2006(TDB: TJulianDate): Double;
// reference: Capitaine et al, Astron. Astrophys. 412, 567-586 (2003)
// result = obliquity of ecliptic (EpsA): radians
var
  t: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;
  Result:= (-   46.836769 + (- 0.0001831 + (  0.00200340 + (- 0.000000576 - 0.0000000434*t)*t)*t)*t)*t;
  Result:= ObliquityJ2000IAU2006 + Result*RadiansPerArcSecond;
end;

function FrameBiasInLongitudeIAU2000: Double;
//  reference: McCarthy & Petit, IERS Conventions (2003), p. 45, IERS Technical Note 32, November 2003
//             International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
// result = frame bias in longitude (dPsiB): radians
begin
 Result:= -0.041775;     // in arcseconds
 Result:= Result*RadiansPerArcSecond;
end;

function FrameBiasInObiquityIAU2000: Double;
//  reference: McCarthy & Petit, IERS Conventions (2003), p. 45, IERS Technical Note 32, November 2003
//             International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
// result = frame bias in obliquity (dEpsB): radians
begin
 Result:= -0.0068192;     // in arcseconds
 Result:= Result*RadiansPerArcSecond;
end;

function FrameBiasICRSRAIAU2000: Double;
//  reference: McCarthy & Petit, IERS Conventions (2003), p. 45, IERS Technical Note 32, November 2003
//             International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
// result = ICRS RA of the J2000.0 equinox (dRA0): radians
begin
 Result:= -0.0146;     // in arcseconds
 Result:= Result*RadiansPerArcSecond;
end;

function FrameBiasInXIAU2000: Double;
//  reference: McCarthy & Petit, IERS Conventions (2003), p. 45, IERS Technical Note 32, November 2003
// result = frame bias in X (Xi0): radians
begin
 Result:= -0.0166170;     // in arcseconds
 Result:= Result*RadiansPerArcSecond;
end;

function FrameBiasInYIAU2000: Double;
//  reference: McCarthy & Petit, IERS Conventions (2003), p. 45, IERS Technical Note 32, November 2003
// result = frame bias in Y (Eta0): radians
begin
 Result:= -0.0068192;     // in arcseconds
 Result:= Result*RadiansPerArcSecond;
end;

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
  t: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

//  Precession angles (Lieske et al. 1977)
  Eps0  := ObliquityJ2000IAU1980;
  EpsA  := MeanObliquityIAU1980(TDB);
  PsiA  :=        (  5038.7784 + (- 1.07259 + (- 0.001147)*t)*t)*t*RadiansPerArcSecond;;
  ChiA  :=        (    10.5526 + (- 2.38064 + (- 0.001125)*t)*t)*t*RadiansPerArcSecond;;
  OmegaA:= Eps0 + (              (  0.05127 + (- 0.007726)*t)*t)*t*RadiansPerArcSecond;;
end;

procedure PrecessionIAU2000(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);
//  reference: McCarthy & Petit, IERS Conventions (2003), p. 45, IERS Technical Note 32, November 2003
//  P = Rz(ChiA).Rx(-OmegaA).Rz(-PsiA).Rx(Eps0)
//  result = compute Precession Angles (PsiA, ChiA, OmegaA) (in radians)
//  uses: TDB
var
  t: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

//  Precession angles (Lieske et al. 1977) with IAU 2000 precession corrections
  Eps0  := ObliquityJ2000IAU1980;
  EpsA  := MeanObliquityIAU2000(TDB);
  PsiA  :=        (  5038.47875 + (- 1.07259 + (- 0.001147)*t)*t)*t*RadiansPerArcSecond;
  ChiA  :=        (    10.5526  + (- 2.38064 + (- 0.001125)*t)*t)*t*RadiansPerArcSecond;
  OmegaA:= Eps0 + (-    0.02524 + (  0.05127 + (- 0.007726)*t)*t)*t*RadiansPerArcSecond;
end;

procedure PrecessionIAU2006(TDB: TJulianDate; out Eps0, EpsA,PsiA,ChiA,OmegaA: Double);
//  reference: Capitaine et al, Astron. Astrophys. 412, 567-586 (2003)
//  P = Rz(ChiA).Rx(-OmegaA).Rz(-PsiA).Rx(Eps0)
//  result = compute Precession Angles (PsiA, ChiA, OmegaA) (in radians)
//  uses: TT
var
  t: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

//  Precession angles (Capitaine et al. 2003)
  Eps0  := ObliquityJ2000IAU2006;
  EpsA  := MeanObliquityIAU2006(TDB);
  PsiA  :=        (  5038.481507 + (- 1.0790069 + (- 0.00114045 + (  0.000132851 - 0.0000000951*t)*t)*t)*t)*t*RadiansPerArcSecond;
  ChiA  :=        (    10.556403 + (- 2.3814292 + (- 0.00121197 + (  0.000170663 - 0.0000000560*t)*t)*t)*t)*t*RadiansPerArcSecond;
  OmegaA:= Eps0 + (-    0.025754 + (  0.0512623 + (- 0.00772503 + (- 0.000000467 + 0.0000003337*t)*t)*t)*t)*t*RadiansPerArcSecond;
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
  t: Double;
  Argument, sinArg, cosArg: Double;
  FundamentalArguments: array [1..5] of Double;
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
//  REFERENCE:  McCarthy & Luzum, Cel.Mech.Dyn.Astron., 85, 37-49 (2003) (IAU 2000B Theory of Nutation Model)
//              International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  This routine computes the two Nutation angles in longitude and obliquity, with
//  respect to the equinox and ecliptic of date, using the IAU 2000B Theory of Nutation Model
//  N = Rx(-(EpsA + DeltaEps)).Rz(-DeltaPsi).Rx(EpsA)
//  result = Nutation Angles (DeltaPsi, DeltaEps) (in arcsecs)
//  uses: TT
var
  t: Double;
  FundamentalArguments: array [1..5] of Double;
  Argument, sinArg, cosArg: Double;
  j, i: Integer;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;

  // Fundamental (Delaunay) arguments from Simon et al. (1994)
  //    l = mean anomaly of the Moon (in arcseconds)
  FundamentalArguments[1]:= 485868.249036 + 1717915923.2178*t;
  //    l' = mean anomaly of the Sun (in arcseconds)
  FundamentalArguments[2]:= 1287104.79305 + 129596581.0481*t;
  //    F = L - OM = mean longitude of the Moon - mean longitude of the Moon's ascending node (in arcseconds)
  FundamentalArguments[3]:= 335779.526232 + 1739527262.8478*t;
  //    D = mean elongation of the Moon from the Sun (in arcseconds)
  FundamentalArguments[4]:= 1072260.70369 + 1602961601.2090*t;
  //    OM = mean longitude of the Moon's ascending node (in arcseconds)
  FundamentalArguments[5]:= 450160.398036 - 6962890.5431*t;
  // change Delaunay arguments to radians
  for i:= 1 to 5 do
    FundamentalArguments[i]:= fmod(RadiansPerArcSecond*FundamentalArguments[i],RadiansPerRev);

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
//    Convert from 0.1 microarcsec units to radians
  DeltaPsi:= DeltaPsi*RadiansPerArcSecond/1e7;
  DeltaEps:= DeltaEps*RadiansPerArcSecond/1e7;

//  Fixed offset to correct for missing terms in truncated series (planetary nutation)
  DeltaPsi:= DeltaPsi - 0.135*RadiansPerArcSecond/MilliArcSecondsPerArcSecond;
  DeltaEps:= DeltaEps + 0.388*RadiansPerArcSecond/MilliArcSecondsPerArcSecond;
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
    KE0: array[1..NE0,1..8] of Integer =
//       l,  l', F,  D, Om, LVe,LE, pA
     (  (0,  0,  0,  0,  1,  0,  0,  0),
        (0,  0,  0,  0,  2,  0,  0,  0),
        (0,  0,  2, -2,  3,  0,  0,  0),
        (0,  0,  2, -2,  1,  0,  0,  0),
        (0,  0,  2, -2,  2,  0,  0,  0),
        (0,  0,  2,  0,  3,  0,  0,  0),
        (0,  0,  2,  0,  1,  0,  0,  0),
        (0,  0,  0,  0,  3,  0,  0,  0),
        (0,  1,  0,  0,  1,  0,  0,  0),
        (0,  1,  0,  0, -1,  0,  0,  0),
      // 11-20
        (1,  0,  0,  0, -1,  0,  0,  0),
        (1,  0,  0,  0,  1,  0,  0,  0),
        (0,  1,  2, -2,  3,  0,  0,  0),
        (0,  1,  2, -2,  1,  0,  0,  0),
        (0,  0,  4, -4,  4,  0,  0,  0),
        (0,  0,  1, -1,  1, -8, 12,  0),
        (0,  0,  2,  0,  0,  0,  0,  0),
        (0,  0,  2,  0,  2,  0,  0,  0),
        (1,  0,  2,  0,  3,  0,  0,  0),
        (1,  0,  2,  0,  1,  0,  0,  0),
     // 21-30
        (0,  0,  2, -2,  0,  0,  0,  0),
        (0,  1, -2,  2, -3,  0,  0,  0),
        (0,  1, -2,  2, -1,  0,  0,  0),
        (0,  0,  0,  0,  0,  8,-13, -1),
        (0,  0,  0,  2,  0,  0,  0,  0),
        (2,  0, -2,  0, -1,  0,  0,  0),
        (1,  0,  0, -2,  1,  0,  0,  0),
        (0,  1,  2, -2,  2,  0,  0,  0),
        (1,  0,  0, -2, -1,  0,  0,  0),
        (0,  0,  4, -2,  4,  0,  0,  0),
     // 31-33
        (0,  0,  2, -2,  4,  0,  0,  0),
        (1,  0, -2,  0, -3,  0,  0,  0),
        (1,  0, -2,  0, -1,  0,  0,  0));

//  Argument coefficients for t^1
    KE1: array[1..NE1,1..8] of Integer =
//       l,  l', F,  D, Om, LVe,LE, pA
     (  (0,  0,  0,  0,  1,  0,  0,  0));

//  Sine and cosine coefficients for t^0
    SE0: array[1..NE0,1..2] of Double =
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
    SE1: array[1..NE1,1..2] of Double =
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

 //    lVe = mean longitude of Venus
 faVe_IERS2003(t,FundamentalArguments[6]);
 //    lE = mean longitude of Earth
 faEa_IERS2003(t,FundamentalArguments[7]);
  //    Pa = general precession on longitude
 faPa_IERS2003(t,FundamentalArguments[8]);

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
     for j:= 1 to 8 do
       Argument:= Argument + KE0[i,j] * FundamentalArguments[j];
     SinCos(Argument,sinArg,cosArg);
     S0:= S0 + (SE0[i,1]*sinArg + SE0[i,2]*cosArg);
   end;
 for i:= NE1 downto 1 do
   begin
     //   Form argument for current term
     Argument:= 0;
     for j:= 1 to 8 do
       Argument:= Argument + KE1[i,j] * FundamentalArguments[j];
     SinCos(Argument,sinArg,cosArg);
     S1:= S1 + (SE1[i,1]*sinArg + SE1[i,2]*cosArg);
   end;
 // CT in arcseconds
 CT:= S0 + S1*t;
 // change to Radians
 CT:= CT*RadiansPerArcSecond;
end;

procedure EquationOfEquinoxes_IAU1994(TDB: TJulianDate; out EqEq: Double);
var
  EpsA, DeltaPsi, DeltaEps, CT: Double;
begin
  EpsA:= MeanObliquityIAU1980(TDB);
  NutationIAU1980(TDB, DeltaPsi, DeltaEps);
  EquationOfEquinoxesCT_IAU1994(TDB, CT);

  EquationOfEquinoxes(DeltaPsi, EpsA, CT, EqEq);
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
  EpsA, DeltaPsi, DeltaEps, CT: Double;
begin
  EpsA:= MeanObliquityIAU2000(TDB);
  NutationIAU2000A(TDB, DeltaPsi, DeltaEps);
  EquationOfEquinoxesCT_IAU2000(TDB, CT);

  EquationOfEquinoxes(DeltaPsi, EpsA, CT, EqEq);
end;

procedure EquationOfEquinoxes_IAU2000B(TDB: TJulianDate; out EqEq: Double);
var
  EpsA, DeltaPsi, DeltaEps, CT: Double;
begin
 EpsA:= MeanObliquityIAU2000(TDB);
 NutationIAU2000B(TDB, DeltaPsi, DeltaEps);
 EquationOfEquinoxesCT_IAU2000(TDB, CT);

  EquationOfEquinoxes(DeltaPsi, EpsA, CT, EqEq);
end;

procedure EquationOfEquinoxes_IAU2006A(TDB: TJulianDate; out EqEq: Double);
var
  EpsA, DeltaPsi, DeltaEps, CT: Double;
begin
  EpsA:= MeanObliquityIAU2006(TDB);
  NutationIAU2000A(TDB, DeltaPsi, DeltaEps);
  EquationOfEquinoxesCT_IAU2000(TDB, CT);

  EquationOfEquinoxes(DeltaPsi, EpsA, CT, EqEq);
end;

function GreenwichSiderealTime(GMST, EqEq: Double): Double;
begin
  Result:= GMST + EqEq;
  // put in range (2Pi)
  Result:= fmod(Result,RadiansPerRev);
end;

function GreenwichSiderealTime(GMST, DeltaPsi, EpsA, CT: Double): Double;
var
  EqEq: Double;
begin
  EquationOfEquinoxes(DeltaPsi, EpsA, CT, EqEq);
  Result:= GreenwichSiderealTime(GMST, EqEq);
end;

function GreenwichSiderealTime_IAU1994(UT1: TJulianDate; TDB: TJulianDate
  ): Double;
var
  GMST, EpsA, DeltaPsi, DeltaEps, CT: Double;
begin
  GreenwichMeanSiderealTimeIAU1982(UT1, GMST);
  EpsA:= MeanObliquityIAU1980(TDB);
  NutationIAU1980(TDB, DeltaPsi, DeltaEps);
  EquationOfEquinoxesCT_IAU1994(TDB, CT);

  Result:= GreenwichSiderealTime(GMST, DeltaPsi, EpsA, CT);
end;

function GreenwichSiderealTime_IAU2000A(UT1: TJulianDate; TDB: TJulianDate
  ): Double;
var
  GMST, EpsA, DeltaPsi, DeltaEps, CT: Double;
begin
  GreenwichMeanSiderealTimeIAU2000(UT1, TDB, GMST);
  EpsA:= MeanObliquityIAU2000(TDB);
  NutationIAU2000A(TDB, DeltaPsi, DeltaEps);
  EquationOfEquinoxesCT_IAU2000(TDB, CT);

  Result:= GreenwichSiderealTime(GMST, DeltaPsi, EpsA, CT);
end;

function GreenwichSiderealTime_IAU2000B(UT1: TJulianDate; TDB: TJulianDate
  ): Double;
var
  GMST, EpsA, DeltaPsi, DeltaEps, CT: Double;
begin
  GreenwichMeanSiderealTimeIAU2000(UT1, TDB, GMST);
  EpsA:= MeanObliquityIAU2000(TDB);
  NutationIAU2000B(TDB, DeltaPsi, DeltaEps);
  EquationOfEquinoxesCT_IAU2000(TDB, CT);

  Result:= GreenwichSiderealTime(GMST, DeltaPsi, EpsA, CT);
end;

function GreenwichSiderealTime_IAU2006A(UT1: TJulianDate; TDB: TJulianDate
  ): Double;
var
  GMST, EpsA, DeltaPsi, DeltaEps, CT: Double;
begin
  GreenwichMeanSiderealTimeIAU2000(UT1, TDB, GMST);
  EpsA:= MeanObliquityIAU2006(TDB);
  NutationIAU2000A(TDB, DeltaPsi, DeltaEps);
  EquationOfEquinoxesCT_IAU2000(TDB, CT);

  Result:= GreenwichSiderealTime(GMST, DeltaPsi, EpsA, CT);
end;

function TIOLocatorSpIAU2000(TDB: TJulianDate): Double;
//  reference: McCarthy & Petit, IERS Conventions (2003), p. 38, IERS Technical Note 32, November 2003
//             International Astronomical Union's SOFA (Standards of Fundamental Astronomy) software collection.
//  result = The quantity s', positioning the Terrestrial Intermediate Origin
//           on the equator of the Celestial Intermediate Pole.
var
  t: Double;
begin
  t:= (TDB - J2000)/JulianDaysPerCentury;
  //  Approximate S' (in arcseconds)
  Result:= -47e-6 * t;
  // change to Radians
  Result:= Result*RadiansPerArcSecond;
end;

{ TEarthOrientation }

procedure TEarthOrientation.SetPrecessionNutationModel(
  AValue: TPrecessionNutationModel);
begin
  if AValue in [pnNewcomb, pnIAU1948, pnIAU1976, pnIAU1980, pnIAU1982, pnIERS1996] then exit;
//    Error('Model not yet implemented.');

  if fPrecessionNutationModel <> AValue then
    begin
      fPrecessionNutationModel:= AValue;
      Update(UT1, TDB); // ??
    end;
end;

constructor TEarthOrientation.Create;
begin
  Create(0,0)
end;

constructor TEarthOrientation.Create(aUT1: TJulianDate; aTDB: TJulianDate);
begin
 PrecessionNutationModel:= pnIAU2000B;
 Update(aUT1,aTDB);
end;

procedure TEarthOrientation.Update(aUT1: TJulianDate; aTDB: TJulianDate);
begin
  TDB:= aTDB;
  UT1:= aUT1;
end;

procedure TEarthOrientation.SetTDB(AValue: TJulianDate);
var
  dummy: Double;
begin
  if TDB = AValue then exit;
  fTDB:= AValue;

  case PrecessionNutationModel of
    pnNewcomb:
     begin
    //       Precession_Newcomb;
    //       Nutation_Newcomb;
    //       fCT:= 0;
     end;
    pnIAU1948:
     begin
    //       Precession_Newcomb;
    //       Nutation_IAU1948;
    //       fCT:= 0;
     end;
    pnIAU1976:
     begin
    //       Precession_IAU1976;
    //       Nutation_IAU1948;
    //       fCT:= 0;
     end;
    pnIAU1980:
      begin
        fEps0:= ObliquityJ2000IAU1980;
        fEpsA:= MeanObliquityIAU1980(TDB);
        PrecessionIAU1976(TDB, dummy, dummy,fPsiA,fChiA,fOmegaA);
        NutationIAU1980(TDB, fDeltaPsi, fDeltaEps);
        fCT:= 0;
      end;
    pnIAU1982:
     begin
       fEps0:= ObliquityJ2000IAU1980;
       fEpsA:= MeanObliquityIAU1980(TDB);
       PrecessionIAU1976(TDB, dummy, dummy,fPsiA,fChiA,fOmegaA);
       NutationIAU1980(TDB, fDeltaPsi, fDeltaEps);
       fCT:= 0;
     end;
    pnIAU1994:
     begin
       fEps0:= ObliquityJ2000IAU1980;
       fEpsA:= MeanObliquityIAU1980(TDB);
       PrecessionIAU1976(TDB, dummy, dummy,fPsiA,fChiA,fOmegaA);
       NutationIAU1980(TDB, fDeltaPsi, fDeltaEps);
       EquationOfEquinoxesCT_IAU1994(TDB, fCT);
     end;
    pnIERS1996:
     begin
//       fEps0:= ObliquityJ2000IAU1980;
//       fEpsA:= MeanObliquityIAU1980(TDB);
//       PrecessionIAU1976(TDB, dummy, dummy,fPsiA,fChiA,fOmegaA);
//       NutationIERS1996(TDB, fDeltaPsi, fDeltaEps);
//       EquationOfEquinoxesCT_IAU1994(TDB, fCT);
     end;
    pnIAU2000A:
     begin
       fEps0:= ObliquityJ2000IAU1980;
       fEpsA:= MeanObliquityIAU2000(TDB);
       PrecessionIAU2000(TDB, dummy, dummy,fPsiA,fChiA,fOmegaA);
       NutationIAU2000A(TDB, fDeltaPsi, fDeltaEps);
       EquationOfEquinoxesCT_IAU2000(TDB, fCT);
     end;
    pnIAU2000B:
     begin
       fEps0:= ObliquityJ2000IAU1980;
       fEpsA:= MeanObliquityIAU2000(TDB);
       PrecessionIAU2000(TDB, dummy, dummy,fPsiA,fChiA,fOmegaA);
       NutationIAU2000B(TDB, fDeltaPsi, fDeltaEps);
       EquationOfEquinoxesCT_IAU2000(TDB, fCT);
     end;
    pnIAU2006A:
     begin
       fEps0:= ObliquityJ2000IAU2006;
       fEpsA:= MeanObliquityIAU2006(TDB);
       PrecessionIAU2006(TDB, dummy, dummy,fPsiA,fChiA,fOmegaA);
       NutationIAU2000A(TDB, fDeltaPsi, fDeltaEps);
       EquationOfEquinoxesCT_IAU2000(TDB, fCT);
     end;
    pnIAU2006B:
     begin
       fEps0:= ObliquityJ2000IAU2006;
       fEpsA:= MeanObliquityIAU2006(TDB);
       PrecessionIAU2006(TDB, dummy, dummy,fPsiA,fChiA,fOmegaA);
       NutationIAU2000B(TDB, fDeltaPsi, fDeltaEps);
       EquationOfEquinoxesCT_IAU2000(TDB, fCT);
     end;
  end;
  fTrueObliquity:= MeanObliquity + DeltaEpsilon;
end;

procedure TEarthOrientation.SetUT1(AValue: TJulianDate);
begin
  if UT1 = AValue then exit;
  fUT1:= AValue;

  case PrecessionNutationModel of
    pnNewcomb:
     begin
    //       fGMST:= GMST_Newcomb;
     end;
    pnIAU1948:
     begin
    //       fGMST:= GMST_Newcomb;
     end;
    pnIAU1976:
     begin
    //       fGMST:= GMST_Newcomb;
     end;
    pnIAU1980:
      begin
    //        fGMST:= GMST_Newcomb;
      end;
    pnIAU1982:
     begin
       GreenwichMeanSiderealTimeIAU1982(UT1, fGMST);
     end;
    pnIAU1994:
     begin
       GreenwichMeanSiderealTimeIAU1982(UT1, fGMST);
     end;
    pnIERS1996:
     begin
       GreenwichMeanSiderealTimeIAU1982(UT1, fGMST);
     end;
    pnIAU2000A:
     begin
       GreenwichMeanSiderealTimeIAU2000(UT1, TDB, fGMST);
     end;
    pnIAU2000B:
     begin
       GreenwichMeanSiderealTimeIAU2000(UT1, TDB, fGMST);
     end;
    pnIAU2006A:
     begin
       GreenwichMeanSiderealTimeIAU2006(UT1, TDB, fGMST);
     end;
    pnIAU2006B:
     begin
       GreenwichMeanSiderealTimeIAU2006(UT1, TDB, fGMST);
     end;
  end;
  fGAST:= GreenwichSiderealTime(GreenwichMeanSiderealTime, DeltaPsi, EpsA, fCT)
end;





end.

