{
    almDateTime is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2010,2013, 2020, 2022, 2023 João Marcelo S. Vaz

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

    Some code, where stated:
      Copyright (C) 2003 by Alan W. Irwin, licenced under GPLv2

}

//  This unit has Date and Time manipulation routines.
unit almDateTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, almBase;

type

  { TTimeValue is a representation class for a time value. It provides common
  astronomical time formats like JulianDate, Modified Julian Date, Julian Epoch,
  Besselian Epoch and th native TDateTime format.
  }

  TTimeValue = class
    private
      fJulianDate: TJulianDate;
      fJulianDateFrac: TJulianDate;
      function GetBesselianEpoch: Extended;
      function GetDateTime: TDateTime;
      function GetISO8601: string;
      function GetJD: TJulianDate;
      function GetJulianEpoch: Extended;
      function GetMJD: TMJD;
      procedure SetBesselianEpoch(AValue: Extended);
      procedure SetDateTime(AValue: TDateTime);
      procedure SetISO8601(AValue: string);
      procedure SetJD(AValue: TJulianDate);
      procedure SetJulianEpoch(AValue: Extended);
      procedure SetMJD(AValue: TMJD);
    public
      constructor Create(aJulianDate: TJulianDate = 0; aJulianDateFrac: TJulianDate = 0);
      {Julian Date is a continuous count of days starting at noon on Monday, January 1, 4713 BC,
      proleptic Julian calendar (November 24, 4714 BC, in the proleptic Gregorian calendar),
      plus the fraction of a day since the preceding noon in Universal Time.
      }
      property JD: TJulianDate read GetJD write SetJD;
      {Modified Julian Date is the Julian Date with a more recente starting point
      (0:00 November 17, 1858).
      MJD = JD − 2400000.5
      }
      property MJD: TMJD read GetMJD write SetMJD;
      property DateTime: TDateTime read GetDateTime write SetDateTime;
      {Besselian Epoch is defined at Lieske, J.H., 1979, Astron.Astrophys. 73, 282 as
      BE = 1900.0 + (JED - 2415020.31352)/365.242198781
      At the notes on Recomendation 2 of the XVIth General Assembly Grenoble, France,  1976
      the Besselian year was fixed at the length of the tropical year at B1900.0.
      }
      property BesselianEpoch: Extended read GetBesselianEpoch write SetBesselianEpoch;
      {JulianEpoch is defined at Lieske, J.H., 1979, Astron.Astrophys. 73, 282 as
      JE = 2000.0 + (JED - 2451545.0)/365.25
      }
      property JulianEpoch: Extended read GetJulianEpoch write SetJulianEpoch;
      {ISO8601 is the date and time expressed according to the international standard ISO 8601.
      }
      property ISO8601: string read GetISO8601 write SetISO8601;
      function JDAsStr(Digits: Integer = 5): String;
      function MJDAsStr(Digits: Integer = 5): String;
      function DateTimeAsStr(Digits: Integer = 5): String;
      function BesselianEpochAsStr(Digits: Integer = 1): String;
      function JulianEpochAsStr(Digits: Integer = 1): String;
    end;

  {TTimeScales is a class used to compute some astronomical time scales.
   You should set UTC to get the other time scales.

   The most important are:
     TAI is an atomic time scale and is the official timekeeping standard.
     UT1 is a solar time and is based on the Earth rotation.
     UTC is a hybrid atomic/solar time scale and is the basis of civil time.
     TT is a dynamical time and used for solar system lookup.
     TCG is a dynamical time used for calculations centered on the Earth.
     TCB is a dynamical time used for calculations centered on the Solar System Barycenter.
     TDB is a scaled form of TCB that keeps in step with TT on the average.
     UT0 and UT2 are specialist forms of universal time that take into account
        polar motion and known seasonal effects, no longer used.
  }
  TTimeScales = class
    private
      fUTC: TJulianDate;
      fDUT1: TJulianDate;
      fTDB: TJulianDate;
      fTT: TJulianDate;
      fUT1: TJulianDate;
      procedure SetUTC(const aUTC: TJulianDate);
      function GetTAI: TJulianDate;
      function GetTCB: TJulianDate;
      function GetTCG: TJulianDate;
      function GetUT0: TJulianDate;
      function GetUT2: TJulianDate;
    protected
      procedure UTCChanged;
    public
      constructor Create(aUTC: TJulianDate; aDUT1: TJulianDate); overload;
      constructor Create(aUTC: TJulianDate); overload;
      constructor Create; overload;
      procedure SetUTC(const aUTC: TJulianDate; const aDUT1: TJulianDate);
      {UTC = Coordinated Universal Time.
       It is the international standard on which civil time is based. It's is an
       atomic timescale that approximates UT1 and ticks SI seconds, in step with TAI.
       It is kept within 0.9 seconds of UT1 by the introduction of occasional intercalary
       leap seconds.}
      property UTC: TJulianDate read fUTC write SetUTC;
      {TAI = International Atomic Time.
       A widely used practical realization of Terrestrial Time (TT) with a fixed
       shift from the latter due to historical reasons. It's a continuous time
       scale, calculated by the Bureau International des Poids et Mesures (BIPM),
       using data from atomic clocks around the world in accordance with the
       definition of the SI second.}
      property TAI: TJulianDate read GetTAI;
      {TT = Terrestrial Time.
       A coordinate time whose mean rate is close to the mean rate of the proper
       time of an observer located on the rotating Earth geoid; it is related to
       Geocentric Coordinate Time (TCG) by a convencional linear transformation.}
      property TT: TJulianDate read fTT;
      {TDB = Barycentric Dynamical Time.
       A time scale originally intended to serve as an independent time argument
       of barycentric ephemerides and equations of motions. The IAU 1991 resolutions
       introduced TCB and noted that TDB was a linear transformation of TCB.
       In 2006, TDB was re-defined, fixing the rate ratio and zero point.}
      property TDB: TJulianDate read fTDB;
      {TCG = Geocentric Coordinate Time.
       The coordinate time of the GCRS based on the SI second; it is related to
       Terrestrial Time (TT) by a convencional linear transformation.}
      property TCG: TJulianDate read GetTCG;
      {TCB = Barycentric Coordinate Time.
       The coordinate time of the BCRS; it is related to Geocentric Coordinate
       Time (TCG) and Terrestrial Time (TT) by relativistic transformations
       that include secular terms.}
      property TCB: TJulianDate read GetTCB;
      {UT1 = Universal Time.
       Universal Time (UT1) is a timescale based on the rotation of the Earth.
       It's the angle of the Earth's rotation about the Celestial Intermediate
       Pole (CIP) axis defined by its conventional linear relation to the Earth
       Rotation Angle (ERA).}
      property UT1: TJulianDate read fUT1;
      {UT2 = Universal Time.
       It's is a smoothed version of UT1, filtering out periodic seasonal variations.
       It is mostly of historic interest and rarely used anymore.}
      property UT2: TJulianDate read GetUT2;
      {UT0 = Universal Time.
       It's is an observatory-specific version of UT1 in the sense that UT0 contains
       the effect of polar motion on the observed rotation of the earth. Polar
       motion is equivalent to a change in latitude and longitude of points on
       the earth's surface with respect to the earth's instantaneous rotation axis.
       Since UT1 is now determined from observations from an ensemble of obsevatories,
       often as part of VLBI interferometers, the practical use of UT0 has dwindled.}
      property UT0: TJulianDate read GetUT0;
    end;


  //     dTDB = TDB - TT   in Seconds
      function DeltaTDB_FB2001(TDB: TJulianDate; AmpCut: Double): Double;

  // Delta AT  = TAI - UTC (in seconds)
      function DeltaAT(UTC: TJulianDate): Double;
  // Delta TAI = TT - TAI  (in Seconds)
      function DeltaTAI: Double;
  // Delta T   = TT  - UT1 (in seconds)
      function DeltaT(UT1: TJulianDate): Double;
  // Delta TDB = TDB - TT  (in seconds)
      function DeltaTDB(TDB: TJulianDate): Double;
  // Delta TCG = TCG - TT  (in seconds)
      function DeltaTCG(TT: TJulianDate): Double;
  // Delta TCB = TCB - TDB (in seconds)
      function DeltaTCB(TCB: TJulianDate): Double;
  // DUT1 = UT1 - UTC (in seconds)
      function DUT1(UTC: TJulianDate): Double;
  // Delta UT2 = UT2 - UT1 (in seconds)
      function DeltaUT2(UT1: TJulianDate): Double;
  // Delta UT0 = UT0 - UT1 (in seconds)
      function DeltaUT0(UT1: TJulianDate): Double;

  //  JulianDateToMJD converts a JulianDate to MJD
  function JulianDateToMJD(JulianDate: TJulianDate): TMJD;

  //  MJDToJulianDate converts MJD to JulianDate
  function MJDToJulianDate(MJD: TMJD): TJulianDate;

  //  JulianDateToDateTime converts JulianDate to Delphi TDateTime
  function JulianDateToDateTime(JulianDate: TJulianDate):TDateTime;

  //  DateTimeToJulianDate converts Delphi TDateTime to JulianDate
  function DateTimeToJulianDate(DateTime: TDateTime):TJulianDate;

  //  JulianDateToBesselianEpoch converts a JulianDate value to a Besselian Epoch value
  //  This function is used to compute UT2
  function JulianDateToBesselianEpoch(JD: TJulianDate): Double;

  //  BesselianEpochToJulianDate converts a Besselian Epoch to a JulianDate value value
  function BesselianEpochToJulianDate(BesselianEpoch: Double): TJulianDate;

  //  JulianDateToJulieanEpoch converts a JulianDate value to a Julian Epoch value
  function JulianDateToJulianEpoch(JD: TJulianDate): Double;

  //  JulianEpochToJulianDate converts a Julian Epoch value to a JulianDate value
  function JulianEpochToJulianDate(JulianEpoch: Double): TJulianDate;

  function StandardTimeToUTC(StandardTime: TDateTime; TimeZone: Double; DayLightSavings: Double): TDateTime;
  function UTCToStandardTime(UTC: TDateTime; TimeZone: Double; DayLightSavings: Double): TDateTime;
  function LocalMeanTimeToUniversalTime(LocalMeanTime: TDateTime; Longitude: Double): TDateTime;
  function UniversalTimeToLocalMeanTime(UniversalTime: TDateTime; Longitude: Double): TDateTime;


implementation

uses Math, DateUtils;

{ TTimeValue }

constructor TTimeValue.Create(aJulianDate: TJulianDate;
  aJulianDateFrac: TJulianDate);
begin
  fJulianDate:= aJulianDate;
  fJulianDateFrac:= aJulianDateFrac;
end;

function TTimeValue.JDAsStr(Digits: Integer): String;
begin
  Result:= FloatToStrF(JD, ffFixed,0, Digits);
end;

function TTimeValue.MJDAsStr(Digits: Integer): String;
begin
 Result:= FloatToStrF(MJD, ffFixed,0, Digits);
end;

function TTimeValue.DateTimeAsStr(Digits: Integer): String;
begin
 Result:= FloatToStrF(DateTime, ffFixed,0, Digits);
end;

function TTimeValue.BesselianEpochAsStr(Digits: Integer): String;
begin
 Result:= 'B' + FloatToStrF(DateTime, ffFixed,0, Digits);
end;

function TTimeValue.JulianEpochAsStr(Digits: Integer): String;
begin
 Result:= 'J' + FloatToStrF(DateTime, ffFixed,0, Digits);
end;

function TTimeValue.GetJD: TJulianDate;
begin
  Result:= fJulianDate + fJulianDateFrac;
end;

function TTimeValue.GetJulianEpoch: Extended;
begin
  Result:= JulianDateToJulianEpoch(JD);
end;

function TTimeValue.GetDateTime: TDateTime;
begin
 Result:= JulianDateToDateTime(JD);
end;

function TTimeValue.GetISO8601: string;
begin
 Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz', DateTime);
end;

function TTimeValue.GetBesselianEpoch: Extended;
begin
  Result:= JulianDateToBesselianEpoch(JD);
end;

function TTimeValue.GetMJD: TMJD;
begin
  Result:= JulianDateToMJD(JD);
end;

procedure TTimeValue.SetBesselianEpoch(AValue: Extended);
begin
 JD:= BesselianEpochToJulianDate(AValue);
end;

procedure TTimeValue.SetDateTime(AValue: TDateTime);
begin
  JD:= DateTimeToJulianDate(AValue);
end;

procedure TTimeValue.SetISO8601(AValue: string);
begin
  DateTime:= ISO8601ToDate(AValue,True);
end;

procedure TTimeValue.SetJD(AValue: TJulianDate);
begin
 fJulianDate:= Int(AValue);
 fJulianDateFrac:= Frac(AValue);
end;

procedure TTimeValue.SetJulianEpoch(AValue: Extended);
begin
 JD:= JulianEpochToJulianDate(AValue);
end;

procedure TTimeValue.SetMJD(AValue: TMJD);
begin
  JD:= MJDToJulianDate(AValue);
end;

{
**  An approximation to TDB-TT, the difference between barycentric
**  dynamical time and terrestrial time, for an observer on the Earth.
**
**  The different time scales - proper, coordinate and realized - are
**  related to each other:
**
**            TAI             <-  physically realized
**             :
**          offset            <-  observed (nominally +32.184s)
**             :
**            TT              <-  terrestrial time
**             :
**    rate adjustment (L_G)   <-  definition of TT
**             :
**            TCG             <-  time scale for GCRS
**             :
**      "periodic" terms      <-  iauDtdb  is an implementation
**             :
**    rate adjustment (L_C)   <-  function of solar-system ephemeris
**             :
**            TCB             <-  time scale for BCRS
**             :
**    rate adjustment (-L_B)  <-  definition of TDB
**             :
**            TDB             <-  TCB scaled to track TT
**             :
**      "periodic" terms      <-  -iau_DTDB is an approximation
**             :
**            TT              <-  terrestrial time
}


{ TTimeScales }

constructor TTimeScales.Create;
begin
 fUTC:= 0;
 fDUT1:= 0;
 fTDB:= 0;
 fTT:= 0;
 fUT1:= 0;
end;

constructor TTimeScales.Create(aUTC: TJulianDate);
begin
  Create(aUTC, 0);
end;

constructor TTimeScales.Create(aUTC: TJulianDate; aDUT1: TJulianDate);
begin
  Create;
  SetUTC(aUTC,aDUT1);
end;

procedure TTimeScales.SetUTC(const aUTC: TJulianDate; const aDUT1: TJulianDate
  );
begin
  if ((fUTC=aUTC) and (fDUT1=aDUT1)) then exit;
  fUTC:= aUTC;
  fDUT1:= aDUT1;
  UTCChanged;
end;

procedure TTimeScales.SetUTC(const aUTC: TJulianDate);
begin
  SetUTC(aUTC,fDUT1);
end;

procedure TTimeScales.UTCChanged;
begin
  fUT1:= UTC + fDUT1/SecondsPerDay;
  fTT:=  TAI + DeltaTAI/SecondsPerDay;   // TODO: Use DeltaT => fTT:= UT1 - DeltaT(UT1)
  fTDB:= TT  + DeltaTDB(TT)/SecondsPerDay; // it should be DeltaTDB(TDB), but it's OK
end;

function TTimeScales.GetTAI: TJulianDate;
begin
  Result:= UTC + DeltaAT(UTC)/SecondsPerDay;
end;

function TTimeScales.GetTCB: TJulianDate;
begin
  //TODO: it should be DeltaTCB(TCB). Should make a loop to converge
  Result:= TDB + DeltaTCB(TDB)/SecondsPerDay;
end;

function TTimeScales.GetTCG: TJulianDate;
begin
  Result:= TT  + DeltaTCG(TT)/SecondsPerDay;
end;

function TTimeScales.GetUT0: TJulianDate;
begin
  Result:= UT1 + DeltaUT0(UT1)/SecondsPerDay;
end;

function TTimeScales.GetUT2: TJulianDate;
begin
  Result:= UT1 + DeltaUT2(UT1)/SecondsPerDay;
end;


function DeltaT(UT1: TJulianDate): Double;
{   Delta T = TT - UT1

  Stephenson, F.R. & Morrison, L.V., "Long-Term Fluctuations in the Earth's
    Rotation: 700 BC to AD 1990", Philosophical Transactions of the Royal
    Society of London, Ser. A, 351 (1995), 165-202.

  Stephenson, F.R., "Historical Eclipses and Earth's Rotation", Cambridge U. Press (1997).

  Meeus, J. & Simons, L. "Polynomial aproximations to Delta T, 1620-2000 AD", Journal
    of British Astronomical Assoc., 110, 6 (2000).
}
const
 dT1  = -600;
 dT2  = 1620;
 dT3  = 1690;
 dT4  = 1770;
 dT5  = 1820;
 dT6  = 1870;
 dT7  = 1900;
 dT8  = 1940;
 dT9  = 1990;
 dt10 = 2000;

 NDeltaTSteph = 45;
 DeltaTSteph: array [1..NDeltaTSteph,1..2] of Integer =
    ((-600,19063),  // Stephenson (1997; p. 508) - linear interpolation with table
     (-500,16800),  // Table for -500 through 1600, from (Stephenson 1997)
     (-450,16000),
     (-400,15300),
     (-350,14600),
     (-300,14000),
     (-250,13400),
     (-200,12800),
     (-150,12200),
     (-100,11600),
     (-50,11100),
     (0,10600),
     (50,10100),
     (100,9600),
     (150,9100),
     (200,8600),
     (250,8200),
     (300,7700),
     (350,7200),
     (400,6700),
     (450,6200),
     (500,5700),
     (550,5200),
     (600,4700),
     (650,4300),
     (700,3800),
     (750,3400),
     (800,3000),
     (850,2600),
     (900,2200),
     (950,1900),
     (1000,1600),
     (1050,1350),
     (1100,1100),
     (1150,900),
     (1200,750),
     (1250,600),
     (1300,470),
     (1350,380),
     (1400,300),
     (1450,230),
     (1500,180),
     (1550,140),
     (1600,110),
     (1620,124));
 PolyCoefMeeus: array [1..8,-1..4] of Double =
    ((3.45, 40.3, -107.0,   50,  -454,  1244),
     (2.70, 10.2,   11.3,   -1,   -16,    70),
     (2.05, 14.7,  -18.8,  -22,   173,     6),
     (1.55,  5.7,   12.7,  111,  -534, -1654),
     (1.15, -5.8,  -14.6,   27,   101,  8234),
     (0.80, 21.4,   67.0, -443,    19,  4441),
     (0.35, 36.2,   74.0,  189,  -140, -1883),
     (0.05, 60.8,   82.0, -188, -5034,     0));
var
 Y, B, dd, dT2000M, dT2000S: Double;
 i: Integer;
begin
 Y:= (2000 + (UT1 - JulianDate2000)/JulianDaysPerYear);

 if(Y < dT1) then
   //   before -600: formula by Stephenson (1997; p. 508)
   begin
     B:= (Y - 1735)*0.01;
     Result:= -20 + 35*B*B;
//      B:= (Y - 1955.0);
//      Result:= Result - 0.000091*(fTidalAcc + 26.0)*B*B;
   end
 else if (Y >= dT1) and (Y < dT2) then
     // between -500 and 1600: linear interpolation between values of table DeltaT (Stephenson 1997)
     //     adjusted for transition: before -500 (Stephenson (1997; p. 508)) and
     //                              after 1600 (table deltaT (Stephenson 1997))
   begin
     i:= 1;
     while (Y > DeltaTSteph[i+1,1]) do
       Inc(i);
     dd:= (DeltaTSteph[i+1,2] - DeltaTSteph[i,2])/(DeltaTSteph[i+1,1] - DeltaTSteph[i,1]);
     Result:= DeltaTSteph[i,2] + (Y - DeltaTSteph[i,1]) * dd;
//      B:= (Y - 1955.0);
//      Result:= Result - 0.000091*(fTidalAcc + 26.0)*B*B;
   end

 else if (Y >= dT2) and (Y < dT3) then
      //  between 1620 and 1690: formula by Meeus (2000)
   begin
     B:= 3.45 + 0.01 * (Y - 2000);
     Result:= 40.3 + B*(-107.0 + B*(50 + B*(-454 + B*1244)));
   end
 else if (Y >= dT3) and (Y < dT4) then
      //  between 1690 and 1770: formula by Meeus (2000)
   begin
     B:= 2.70 + 0.01 * (Y - 2000);
     Result:= 10.2 + B*(11.3 + B*(-1 + B*(-16 + B*70)));
   end
 else if (Y >= dT4) and (Y < dT5) then
      //  between 1770 and 1820: formula by Meeus (2000)
   begin
     B:= 2.05 + 0.01 * (Y - 2000);
     Result:= 14.7 + B*(-18.8 + B*(-22 + B*(173 + B*6)));
   end
 else if (Y >= dT5) and (Y < dT6) then
      //  between 1820 and 1870: formula by Meeus (2000)
   begin
     B:= 1.55 + 0.01 * (Y - 2000);
     Result:= 5.7 + B*(12.7 + B*(111 + B*(-534 - B*1654)));
   end
 else if (Y >= dT6) and (Y < dT7) then
      //  between 1870 and 1900: formula by Meeus (2000)
   begin
     B:= 1.15 + 0.01 * (Y - 2000);
     Result:= -5.8 + B*(-14.6 + B*(27 + B*(101 + B*8234)));
   end
 else if (Y >= dT7) and (Y < dT8) then
      //  between 1900 and 1940: formula by Meeus (2000)
   begin
     B:= 0.80 + 0.01 * (Y - 2000);
     Result:= 21.4 + B*(67.0 + B*(-443 + B*(19 + B*4441)));
   end
 else if (Y >= dT8) and (Y < dT9) then
      //  between 1940 and 1990: formula by Meeus (2000)
   begin
     B:= 0.35 + 0.01 * (Y - 2000);
     Result:= 36.2 + B*(74.0 + B*(189 + B*(-140 - B*1883)));
   end
 else if (Y >= dT9) and (Y <= dT10) then
      //  between 1990 and 2000: formula by Meeus (2000)
   begin
     B:= 0.05 + 0.01 * (Y - 2000);
     Result:= 60.8 + B*(82.0 + B*(-188 - B*5034));
   end

 else if (Y > dt10) then
   //  after 2000: formula by Stephenson (1997; p. 507)
   //     adjusted for Delta T at 2000 by Meeus
   begin
     // Delta T by Meeus at 2000
     B:= 0.05 + 0.01*(dT10 - 2000);
     dT2000M:= 60.8 + B*(82.0 + B*(-188 - B*5034));
     // Delta T by Stephenson at 2000
     B:= 0.01 * (dT10 - 1820);
     dT2000S:= -20 + 31*B*B;

     // Delta T by Stephenson minus the difference at 2000
     B:= 0.01 * (Y - 1820);
     Result:= -20 + 31*B*B - (dT2000S - dT2000M);
   end
 else
   Result:= 0;
end;

// Delta AT = TAI - UTC   in Seconds
function DeltaAT(UTC: TJulianDate): Double;
{$I dat.inc}
var
 i, this: Integer;
 More: Boolean;
 Offset: Double;
 MaxDate: TJulianDate; // Bulletin date plus 6 months (date of new bulletin)
begin
 Result:= 0;
 // DAT = (TT - UTC) - (TT - TAI)
 // DAT = DeltaT - DeltaTAI
 MaxDate:= DateTimeToJulianDate(IncMonth(StrToDate(BulletinDate, 'y/m/d', '-'),6));
 if (UTC < MinDate) then // use historical Delta T -> UTC = UT1
   Result:= DeltaT(UTC) - DeltaTAI
 else
   if (UTC >= MaxDate) then // use formula for Delta T (adjusted to last known DAT)
     begin
       // compute offset at last known date between true DAT and formula for DeltaT
       Offset:= (DeltaT(MaxDate) - DeltaTAI) - (LeapSeconds[NLeapSeconds,2] +
                (JulianDateToMJD(MaxDate) - LeapSeconds[NLeapSeconds,3])*LeapSeconds[NLeapSeconds,4]);
       Result:= (DeltaT(UTC) - DeltaTAI) - Offset;
     end
   else
   //  Find the most recent table entry
     begin
       i:= NLeapSeconds;
       this:= NLeapSeconds;
       More:= True;
       while More do
         begin
           this:= i;
           More:= ((UTC < LeapSeconds[this,1]) and (this > 1));
           Inc(i, -1);
         end;
       if (UTC >= LeapSeconds[this,1]) then
         Result:= LeapSeconds[this,2] + (JulianDateToMJD(UTC) - LeapSeconds[this,3])*LeapSeconds[this,4];
     end
end;

// DUT1 = UT1 - UTC (in seconds)
function DUT1(UTC: TJulianDate): Double;
{$I dut1.inc}
var
 i, this: Integer;
 More: Boolean;
begin
 //  Find the most recent table entry
   begin
     i:= NDUT1;
     this:= NDUT1;
     More:= True;
     while More do
       begin
         this:= i;
         More:= ((UTC < DUT1_Array[this,1]) and (this > 1));
         Inc(i, -1);
       end;
     if (UTC >= DUT1_Array[this,1]) then
       Result:= DUT1_Array[this,2];
   end
end;

// Delta TAI = TT - TAI   in Seconds
function DeltaTAI: Double;
begin
 Result:= 32.184;
end;

// dTCB = TCB - TDB   in Seconds
function DeltaTCB(TCB: TJulianDate): Double;
// IAU 2006 Resolution 3 : Re-definition of Barycentric Dynamical Time, TDB
{ REFERENCES:
    IERS Conventions (2010). Petit, G. & Luzum, B. ( IERS Technical Note n 36),
                             Frankfurt am Main: Verlag des Bundesamts für
                             Kartographie und Geodäsie, 2010
}
const
 TDBo = -6.55E-5; // TDB−TCB at JD 2443144.5 TAI - consistency with the widely used TDB - TT formula of Fairhead & Bretagnon (1990)
begin
  Result:= LB*(TCB - T0)*SecondsPerDay - TDBo;
end;

//    dTCG = TCG - TT   in Seconds
function DeltaTCG(TT: TJulianDate): Double;
//  IAU 1991 Resolution A.4 - Recommendation IV
{ REFERENCES:
    IERS Conventions (2010). Petit, G. & Luzum, B. ( IERS Technical Note n 36),
                             Frankfurt am Main: Verlag des Bundesamts für
                             Kartographie und Geodäsie, 2010
}
begin
 Result:= SecondsPerDay*(TT - T0)*LG/(1-LG);
end;

//     dTDB = TDB - TT   in Seconds
function DeltaTDB(TDB: TJulianDate): Double;
{
  REFERENCES:
    Fairhead, L. & Bretagnon, P. (1990) Astron. & Astrophys. 229, 240.
    Kaplan, G. (2005), US Naval Observatory Circular 179.
}
var
  T: TJulianDate;
begin
{
  1. Expression used in this function is a truncated form of a
  longer and more precise series given in the first reference.  The
  result is good to about 10 microseconds. The expression is given in USNO
  Circular 179, eq. 2.6.
  2. This function is a Pascal version of NOVAS C 3.0 routine 'tdb2tt'.
}
  T:= (TDB - J2000)/JulianDaysPerCentury;
  Result:= 0.001657 * sin (628.3076 * T + 6.2401) + 0.000022 * sin (575.3385  * T + 4.2970)
         + 0.000014 * sin (1256.6152 *T + 6.1969) + 0.000005 * sin (606.9777 * T + 4.0212)
         + 0.000005 * sin (52.9691 * T + 0.4444)  + 0.000002 * sin (21.3299 * T + 5.5431)
         + 0.000010 * T * sin (628.3076 * T + 4.2490);
end;

//     dTDB = TDB - TT   in Seconds
function DeltaTDB_FB2001(TDB: TJulianDate; AmpCut: Double): Double;
{
  REFERENCES:
    FB1990: Fairhead, L. & Bretagnon, P. (1990) Astron. & Astrophys. 229, 240 -
            An analytical formula for the time transformation TB-TT
    TE405: Irwin, A. W.; Fukushima, T. (1999) Astron. & Astrophys. 348, 642 -
           A numerical time ephemeris of the Earth

  This function is a Pascal version of Alan W. Irwin routine 'fb2001.f' (Copyright
  (C) 2003 by Alan W. Irwin, licenced under GPLv2)
}
const
  nFBSeries = 1697;
  FBSeries: array[1..nFBSeries, 1..5] of Double =
    ((1,0,1656.674564,6283.075849991,6.240054195),
     (2,0,22.417597,5753.384884897,4.296977429),
     (3,0,13.839792,12566.151699983,6.196904410),
     (4,0,4.770119,529.690965095,0.444401653),
     (5,0,4.677525,6069.776754553,4.021195074),
     (6,0,2.257108,213.299095438,5.543113083),
     (7,0,1.694203,-3.523118349,5.025132781),
     (8,0,1.554905,77713.771467747,5.198466627),
     (9,0,1.276839,7860.419392439,5.988822341),
     (10,0,1.193385,5223.693919802,3.649825295),
     (11,0,1.115322,3930.209696220,1.422745069),
     (12,0,0.794186,11506.769769794,2.322313126),
     (13,0,0.447061,26.298319800,3.615796498),
     (14,0,0.435218,-398.149003408,4.349339387),
     (15,0,0.600309,1577.343542448,2.678271909),
     (16,0,0.496083,6208.294251424,5.696701128),
     (17,0,0.486306,5884.926846583,0.520007197),
     (18,0,0.431746,74.781598567,2.435897982),
     (19,0,0.466231,6244.942814354,5.866398950),
     (20,0,0.375510,5507.553238667,4.103476804),
     (21,0,0.243085,-775.522611324,3.651837924),
     (22,0,0.173435,18849.227549974,6.153743448),
     (23,0,0.230729,5856.477659115,4.773852395),
     (24,0,0.203748,12036.460734888,4.333987500),
     (25,0,0.143935,-796.298006816,5.957517589),
     (26,0,0.159080,10977.078804699,1.890075240),
     (27,0,0.119370,38.133035638,4.551584711),
     (28,0,0.118971,5486.777843175,1.914547225),
     (29,0,0.116121,1059.381930189,0.873504273),
     (30,0,0.137927,11790.629088659,1.135934668),
     (31,0,0.098358,2544.314419883,0.092794018),
     (32,0,0.101868,-5573.142801806,5.984503384),
     (33,0,0.080179,206.185548437,2.095376768),
     (34,0,0.079646,4694.002954708,2.949236327),
     (35,0,0.062617,20.775395492,2.654394814),
     (36,0,0.075019,2942.463423292,4.980931885),
     (37,0,0.064397,5746.271337896,1.280308700),
     (38,0,0.063814,5760.498431898,4.167901758),
     (39,0,0.048042,2146.165416475,1.495846011),
     (40,0,0.048373,155.420399434,2.251573730),
     (41,0,0.058854,426.598190876,4.839643597),
     (42,0,0.046551,-0.980321068,0.921573539),
     (43,0,0.054139,17260.154654690,3.411091150),
     (44,0,0.042411,6275.962302991,2.869567046),
     (45,0,0.040140,-7.113547001,3.566725418),
     (46,0,0.036564,5088.628839767,3.324679049),
     (47,0,0.040766,12352.852604545,3.981496548),
     (48,0,0.036507,801.820931124,6.248866009),
     (49,0,0.036955,3154.687084896,5.071801441),
     (50,0,0.042732,632.783739313,5.720622952),
     (51,0,0.042560,161000.685737301,1.270837216),
     (52,0,0.040480,15720.838784878,2.546610087),
     (53,0,0.028244,-6286.598968340,5.069663517),
     (54,0,0.033483,6062.663207553,4.144987117),
     (55,0,0.034867,522.577418094,5.210063597),
     (56,0,0.032444,6076.890301554,0.749317441),
     (57,0,0.030215,7084.896781115,3.389610345),
     (58,0,0.029247,-71430.695617756,4.183179224),
     (59,0,0.033529,9437.762934887,2.404714239),
     (60,0,0.032423,8827.390269875,5.541473358),
     (61,0,0.027567,6279.552731642,5.040845942),
     (62,0,0.029863,12139.553509107,1.770182830),
     (63,0,0.022509,10447.387839604,1.460726249),
     (64,0,0.020937,8429.241266467,0.652303414),
     (65,0,0.020322,419.484643875,3.735407558),
     (66,0,0.024816,-1194.447010225,1.087136918),
     (67,0,0.025196,1748.016413067,2.901883301),
     (68,0,0.021691,14143.495242431,5.952658011),
     (69,0,0.017673,6812.766815086,3.186129056),
     (70,0,0.022533,6133.512652857,3.308095242),
     (71,0,0.016155,10213.285546211,1.331103163),
     (72,0,0.014751,1349.867409659,4.308933301),
     (73,0,0.015952,-220.412642439,4.005298303),
     (74,0,0.015974,-2352.866153772,6.145309444),
     (75,0,0.014223,17789.845619785,2.104551187),
     (76,0,0.017779,73.297125859,3.475972764),
     (77,0,0.013671,-536.804512095,5.971672261),
     (78,0,0.011942,8031.092263058,2.053414714),
     (79,0,0.014318,16730.463689596,3.016058105),
     (80,0,0.012471,103.092774219,1.737497547),
     (81,0,0.010962,3.590428652,2.196567739),
     (82,0,0.015078,19651.048481098,3.969480756),
     (83,0,0.010396,951.718406251,5.717799605),
     (84,0,0.011707,-4705.732307544,2.654125609),
     (85,0,0.010455,5863.591206116,1.913695554),
     (86,0,0.012420,4690.479836359,4.734090399),
     (87,0,0.011850,5643.178563677,5.489013255),
     (88,0,0.008610,3340.612426700,3.661698960),
     (89,0,0.011622,5120.601145584,4.863933069),
     (90,0,0.010825,553.569402842,0.842715011),
     (91,0,0.008666,-135.065080035,3.293406547),
     (92,0,0.009948,149.563197135,4.870690692),
     (93,0,0.009858,6309.374169791,1.061816410),
     (94,0,0.007957,316.391869657,2.464879118),
     (95,0,0.010099,283.859318865,1.942176992),
     (96,0,0.007147,-242.728603974,3.661486981),
     (97,0,0.007505,5230.807466803,4.920937684),
     (98,0,0.008323,11769.853693166,1.229391394),
     (99,0,0.007490,-6256.777530192,3.658444681),
     (100,0,0.009370,149854.400133688,0.673879008),
     (101,0,0.007117,38.027672636,5.294249518),
     (102,0,0.007857,12168.002696575,0.525733542),
     (103,0,0.006996,6206.809778716,0.836340575),
     (104,0,0.006056,955.599741609,4.194535082),
     (105,0,0.008107,13367.972631107,3.793235255),
     (106,0,0.006731,5650.292110678,5.639906826),
     (107,0,0.007291,36.648562930,0.114834057),
     (108,0,0.006366,4164.311989613,2.262084528),
     (109,0,0.006858,5216.580372801,0.642065042),
     (110,0,0.006919,6681.224853400,6.018501540),
     (111,0,0.006826,7632.943259650,3.458654112),
     (112,0,0.005308,-1592.596013633,2.500382359),
     (113,0,0.005250,-1.484472708,3.060078612),
     (114,0,0.005096,11371.704689758,2.547107802),
     (115,0,0.004841,5333.900241022,0.437078014),
     (116,0,0.005583,5966.683980335,2.246165393),
     (117,0,0.006304,11926.254413669,2.512929463),
     (118,0,0.006603,23581.258177318,5.393136849),
     (119,0,0.004648,1589.072895284,1.275847525),
     (120,0,0.005119,6438.496249426,1.486539245),
     (121,0,0.004521,4292.330832950,6.140635794),
     (122,0,0.005680,23013.539539587,4.557814650),
     (123,0,0.005488,-3.455808046,0.090675389),
     (124,0,0.004193,7234.794256242,4.869091389),
     (125,0,0.003742,7238.675591600,4.691976180),
     (126,0,0.004149,-110.206321219,3.016203583),
     (127,0,0.004553,11499.656222793,5.554997737),
     (128,0,0.004892,5436.993015240,1.475419291),
     (129,0,0.004044,4732.030627343,1.398784824),
     (130,0,0.004157,12491.370101415,5.650928227),
     (131,0,0.004349,11513.883316794,2.181745393),
     (132,0,0.003899,12528.018664345,5.823320039),
     (133,0,0.003129,6836.645252834,0.003844094),
     (134,0,0.004080,-7058.598461315,3.690360123),
     (135,0,0.003265,76.266071276,1.516662407),
     (136,0,0.002954,6283.143160294,4.447203799),
     (137,0,0.002872,28.449187468,1.158692983),
     (138,0,0.002881,735.876513532,0.349248536),
     (139,0,0.003279,5849.364112115,4.893384664),
     (140,0,0.003620,6209.778724132,1.473749211),
     (141,0,0.003074,949.175608970,5.185873116),
     (142,0,0.002775,9917.696874510,1.030026330),
     (143,0,0.002646,10973.555686350,3.918259167),
     (144,0,0.002575,25132.303399966,6.109658991),
     (145,0,0.003500,263.083923373,1.892100735),
     (146,0,0.002740,18319.536584880,4.320519046),
     (147,0,0.002464,202.253395174,4.698205955),
     (148,0,0.002409,2.542797281,5.325009315),
     (149,0,0.003354,-90955.551694869,1.942656160),
     (150,0,0.002297,6496.374945429,5.061867201),
     (151,0,0.003002,6172.869528772,2.797812689),
     (152,0,0.003202,27511.467873537,0.531673101),
     (153,0,0.002954,-6283.008539689,4.533471191),
     (154,0,0.002353,639.897286314,3.734606091),
     (155,0,0.002401,16200.772724501,2.605547086),
     (156,0,0.003053,233141.314403242,3.029029275),
     (157,0,0.003024,83286.914269554,2.355556099),
     (158,0,0.002863,17298.182327326,5.240963796),
     (159,0,0.002103,-7079.373856808,5.756537805),
     (160,0,0.002303,83996.847317739,2.013686351),
     (161,0,0.002303,18073.704938650,1.089100401),
     (162,0,0.002376,63.735898303,0.759240205),
     (163,0,0.002493,6386.168624210,0.645024108),
     (164,0,0.002365,3.932153263,6.215882895),
     (165,0,0.002169,11015.106477335,4.845297676),
     (166,0,0.002384,6243.458341645,3.809286079),
     (167,0,0.002183,1162.474704408,6.179613365),
     (168,0,0.002341,6246.427287062,4.781718572),
     (169,0,0.002199,-245.831646229,5.956152329),
     (170,0,0.001729,3894.181829542,1.264976635),
     (171,0,0.001896,-3128.388765096,4.914236405),
     (172,0,0.002024,14712.317116458,2.752035940),
     (173,0,0.002072,35.164090221,1.405202289),
     (174,0,0.001737,6290.189396992,5.280820220),
     (175,0,0.001602,14314.168113050,4.203664812),
     (176,0,0.002218,491.557929457,1.571007353),
     (177,0,0.002182,454.909366527,1.402102022),
     (178,0,0.001897,22483.848574493,4.167932534),
     (179,0,0.001824,-3738.761430108,0.545659908),
     (180,0,0.001894,1052.268383188,5.817163943),
     (181,0,0.001421,20.355319399,2.419886601),
     (182,0,0.001408,10984.192351700,2.732084456),
     (183,0,0.001847,10873.986030480,2.903477692),
     (184,0,0.001391,-8635.942003763,0.593891500),
     (185,0,0.001388,-7.046236698,1.166116834),
     (186,0,0.001810,-88860.057071360,0.487354780),
     (187,0,0.001288,-1990.745017041,3.913022879),
     (188,0,0.001297,23543.230504682,3.063805007),
     (189,0,0.001335,-266.607041722,3.995764027),
     (190,0,0.001376,10969.965257698,5.152914035),
     (191,0,0.001745,244287.600006854,3.626395211),
     (192,0,0.001649,31441.677569757,1.952049261),
     (193,0,0.001416,9225.539273283,4.996408405),
     (194,0,0.001238,4804.209275927,5.503385845),
     (195,0,0.001472,4590.910180489,4.164902188),
     (196,0,0.001169,6040.347246017,5.841719038),
     (197,0,0.001039,5540.085789459,2.769748116),
     (198,0,0.001004,-170.672870619,0.755008103),
     (199,0,0.001284,10575.406682942,5.306538208),
     (200,0,0.001276,71.812653151,4.713486491),
     (201,0,0.001321,18209.330263660,2.624866792),
     (202,0,0.001297,21228.392023546,0.382603541),
     (203,0,0.000954,6282.095528923,0.882213514),
     (204,0,0.001143,6058.731054290,1.170122072),
     (205,0,0.000979,5547.199336460,5.448376256),
     (206,0,0.000987,-6262.300454499,2.656491413),
     (207,0,0.001070,-154717.609887309,1.827624637),
     (208,0,0.000991,4701.116501708,4.386970853),
     (209,0,0.001156,-14.227094002,3.042886838),
     (210,0,0.001176,277.034993741,3.335520880),
     (211,0,0.000890,13916.019109642,5.601498302),
     (212,0,0.000884,-1551.045222648,1.088876800),
     (213,0,0.000876,5017.508371365,3.969900081),
     (214,0,0.000806,15110.466119866,5.142876722),
     (215,0,0.000773,-4136.910433516,0.022067680),
     (216,0,0.001071,175.166059800,1.844910579),
     (217,0,0.000954,-6284.056171060,0.968480906),
     (218,0,0.000737,5326.786694021,4.923812348),
     (219,0,0.000845,-433.711737877,4.749244958),
     (220,0,0.000819,8662.240323563,5.991241233),
     (221,0,0.000852,199.072001436,2.189604824),
     (222,0,0.000723,17256.631536341,6.068720788),
     (223,0,0.000940,6037.244203762,6.197428125),
     (224,0,0.000885,11712.955318231,3.280414159),
     (225,0,0.000706,12559.038152982,2.824848949),
     (226,0,0.000732,2379.164473572,2.501774703),
     (227,0,0.000764,-6127.655450557,2.236346329),
     (228,0,0.000908,131.541961686,2.521257495),
     (229,0,0.000907,35371.887265976,3.370195970),
     (230,0,0.000673,1066.495477190,3.876512579),
     (231,0,0.000814,17654.780539750,4.627122573),
     (232,0,0.000630,36.027866677,0.156368499),
     (233,0,0.000798,515.463871093,5.151962491),
     (234,0,0.000797,148.078724426,5.909225055),
     (235,0,0.000806,309.278322656,6.054059473),
     (236,0,0.000604,-39.617508346,2.839021623),
     (237,0,0.000601,412.371096874,3.984222368),
     (238,0,0.000646,11403.676995575,3.852963326),
     (239,0,0.000704,13521.751441591,2.301147201),
     (240,0,0.000603,-65147.619767764,4.140084282),
     (241,0,0.000609,10177.257679534,0.437122326),
     (242,0,0.000631,5767.611978898,4.026532479),
     (243,0,0.000576,11087.285125918,4.760293148),
     (244,0,0.000674,14945.316173554,6.270504481),
     (245,0,0.000726,5429.879468239,6.039664052),
     (246,0,0.000710,28766.924424484,5.672617793),
     (247,0,0.000647,11856.218651798,3.397130967),
     (248,0,0.000678,-5481.254918868,6.249682229),
     (249,0,0.000618,22003.914634870,2.466426204),
     (250,0,0.000660,625.670192312,5.864090309),
     (251,0,0.000737,6134.997125565,2.242666174),
     (252,0,0.000694,3496.032826134,2.668309139),
     (253,0,0.000531,6489.261398429,1.681880649),
     (254,0,0.000611,-143571.324283697,2.424979874),
     (255,0,0.000575,12043.574281889,4.216492031),
     (256,0,0.000553,12416.588502848,4.772565974),
     (257,0,0.000688,4686.889407707,6.224221623),
     (258,0,0.000495,7342.457780181,3.817297281),
     (259,0,0.000567,3634.621024518,1.649266950),
     (260,0,0.000515,18635.928454536,3.945341596),
     (261,0,0.000487,-323.505416657,4.061677171),
     (262,0,0.000662,25158.601719765,1.794057425),
     (263,0,0.000509,846.082834751,3.053875339),
     (264,0,0.000472,-12569.674818332,5.112133338),
     (265,0,0.000461,6179.983075773,0.513676005),
     (266,0,0.000641,83467.156352644,3.210727256),
     (267,0,0.000520,10344.295065386,2.445600561),
     (268,0,0.000493,18422.629359098,1.676938309),
     (269,0,0.000478,1265.567478626,5.487283235),
     (270,0,0.000472,-18.159247265,1.999707589),
     (271,0,0.000559,11190.377900137,5.783235443),
     (272,0,0.000494,9623.688276691,3.022644036),
     (273,0,0.000463,5739.157790895,1.411222703),
     (274,0,0.000432,16858.482532933,1.179256910),
     (275,0,0.000574,72140.628665941,1.758190905),
     (276,0,0.000484,17267.268201691,3.290589166),
     (277,0,0.000550,4907.302050146,0.863977703),
     (278,0,0.000399,14.977853527,2.094441910),
     (279,0,0.000491,224.344795702,0.877889029),
     (280,0,0.000432,20426.571092422,6.003826461),
     (281,0,0.000481,5749.452731634,4.309591975),
     (282,0,0.000480,5757.317038160,1.142348570),
     (283,0,0.000485,6702.560493867,0.210630224),
     (284,0,0.000426,6055.549660552,4.274476627),
     (285,0,0.000480,5959.570433334,5.031127909),
     (286,0,0.000466,12562.628581634,4.959581632),
     (287,0,0.000520,39302.096962196,4.788002894),
     (288,0,0.000458,12132.439962106,1.880105668),
     (289,0,0.000470,12029.347187887,1.405610320),
     (290,0,0.000416,-7477.522860216,1.082356312),
     (291,0,0.000449,11609.862544012,4.179993633),
     (292,0,0.000465,17253.041107690,0.353496402),
     (293,0,0.000362,-4535.059436924,1.583849521),
     (294,0,0.000383,21954.157609398,3.747382027),
     (295,0,0.000389,17.252277143,1.395753179),
     (296,0,0.000331,18052.929543158,0.566788729),
     (297,0,0.000430,13517.870106233,0.685827545),
     (298,0,0.000368,-5756.908003246,0.731367245),
     (299,0,0.000330,10557.594160824,3.710043702),
     (300,0,0.000332,20199.094959633,1.652902060),
     (301,0,0.000384,11933.367960670,5.827782462),
     (302,0,0.000387,10454.501386605,2.541182580),
     (303,0,0.000325,15671.081759407,2.178857640),
     (304,0,0.000317,138.517496871,2.255333480),
     (305,0,0.000305,9388.005909415,0.578354498),
     (306,0,0.000352,5749.861766548,3.000304847),
     (307,0,0.000311,6915.859589305,1.693597579),
     (308,0,0.000297,24072.921469776,1.997249029),
     (309,0,0.000363,-640.877607382,5.071820819),
     (310,0,0.000323,12592.450019783,1.072266473),
     (311,0,0.000341,12146.667056108,4.700649084),
     (312,0,0.000290,9779.108676125,1.812320441),
     (313,0,0.000342,6132.028180148,4.322243939),
     (314,0,0.000329,6268.848755990,3.033827585),
     (315,0,0.000374,17996.031168222,3.388710510),
     (316,0,0.000285,-533.214083444,4.687294364),
     (317,0,0.000338,6065.844601290,0.877776371),
     (318,0,0.000276,24.298513841,0.770299429),
     (319,0,0.000336,-2388.894020449,5.353796032),
     (320,0,0.000290,3097.883822726,4.075291554),
     (321,0,0.000318,709.933048185,5.941173233),
     (322,0,0.000271,13095.842665077,3.208910687),
     (323,0,0.000331,6073.708907817,4.007881088),
     (324,0,0.000292,742.990060533,2.714351006),
     (325,0,0.000362,29088.811415985,3.215976187),
     (326,0,0.000280,12359.966151546,0.710871877),
     (327,0,0.000267,10440.274292604,4.730108497),
     (328,0,0.000262,838.969287750,1.327714082),
     (329,0,0.000250,16496.361396202,0.898771760),
     (330,0,0.000325,20597.243963041,0.180044372),
     (331,0,0.000268,6148.010769956,5.152666276),
     (332,0,0.000284,5636.065016677,5.655389709),
     (333,0,0.000301,6080.822454817,2.135369862),
     (334,0,0.000294,-377.373607916,3.708784168),
     (335,0,0.000236,2118.763860378,1.733564570),
     (336,0,0.000234,5867.523359379,5.575200071),
     (337,0,0.000268,-226858.238553250,0.069433890),
     (338,0,0.000265,167283.761587292,4.369302213),
     (339,0,0.000280,28237.233459389,5.304831380),
     (340,0,0.000292,12345.739057544,4.096096337),
     (341,0,0.000223,19800.945956225,3.069328316),
     (342,0,0.000301,43232.306658416,6.205311195),
     (343,0,0.000264,18875.525869774,1.417256997),
     (344,0,0.000304,-1823.175188677,3.409035247),
     (345,0,0.000301,109.945688789,0.510922054),
     (346,0,0.000260,813.550283960,2.389438934),
     (347,0,0.000299,316428.228672795,5.384593691),
     (348,0,0.000211,5756.566278635,3.789393084),
     (349,0,0.000209,5750.203491159,1.661943386),
     (350,0,0.000216,6303.851245484,3.862953400),
     (351,0,0.000240,12489.885628707,5.683867656),
     (352,0,0.000203,1581.959348283,5.549820803),
     (353,0,0.000200,5642.198242609,1.016115785),
     (354,0,0.000197,-70.849445304,4.690702525),
     (355,0,0.000227,6287.008003255,2.911892550),
     (356,0,0.000197,533.623118358,1.048840480),
     (357,0,0.000205,-6279.485421340,1.829362732),
     (358,0,0.000209,-10988.808157535,2.636131340),
     (359,0,0.000208,-227.526189440,4.127883842),
     (360,0,0.000191,415.552490612,4.400863577),
     (361,0,0.000190,29296.615389579,4.175657086),
     (362,0,0.000264,66567.485864134,4.601101164),
     (363,0,0.000256,-3646.350377354,0.506364593),
     (364,0,0.000188,13119.721102825,2.032199000),
     (365,0,0.000185,-209.366942175,4.694756586),
     (366,0,0.000198,25934.124331089,3.832703034),
     (367,0,0.000195,4061.219215394,3.308464398),
     (368,0,0.000234,5113.487598583,1.716092001),
     (369,0,0.000188,1478.866574064,5.686928922),
     (370,0,0.000222,11823.161639450,1.942386453),
     (371,0,0.000181,10770.893256262,1.999481828),
     (372,0,0.000171,6546.159773364,1.182807995),
     (373,0,0.000169,20995.392966449,2.169080011),
     (374,0,0.000206,70.328180442,5.934076062),
     (375,0,0.000191,10660.686935042,5.405515899),
     (376,0,0.000228,33019.021112205,4.656983598),
     (377,0,0.000184,-4933.208440333,3.327516338),
     (378,0,0.000220,-135.625325010,1.765430262),
     (379,0,0.000166,23141.558382925,3.454132998),
     (380,0,0.000180,6084.003848555,0.602182111),
     (381,0,0.000190,6144.558353121,5.020464647),
     (382,0,0.000163,17782.732072784,4.960576957),
     (383,0,0.000225,16460.333529525,2.596455708),
     (384,0,0.000222,5905.702242076,3.731990323),
     (385,0,0.000204,227.476132789,5.636192701),
     (386,0,0.000159,16737.577236597,3.600686855),
     (387,0,0.000200,6805.653268085,0.868299233),
     (388,0,0.000187,11919.140866668,2.629449957),
     (389,0,0.000161,127.471796607,2.862621782),
     (390,0,0.000205,6286.666278643,1.742882331),
     (391,0,0.000189,153.778810485,4.812372643),
     (392,0,0.000168,16723.350142595,0.027860571),
     (393,0,0.000149,11720.068865232,0.659718203),
     (394,0,0.000189,5237.921013804,5.245313016),
     (395,0,0.000143,6709.674040867,4.317610605),
     (396,0,0.000146,4487.817406270,4.815302504),
     (397,0,0.000144,-664.756045130,5.381366853),
     (398,0,0.000175,5127.714692584,4.728397352),
     (399,0,0.000162,6254.626662524,1.435133076),
     (400,0,0.000187,47162.516354635,1.354371925),
     (401,0,0.000146,11080.171578918,3.369698972),
     (402,0,0.000180,-348.924420448,2.490902145),
     (403,0,0.000148,151.047669843,3.799109588),
     (404,0,0.000157,6197.248551160,1.284395337),
     (405,0,0.000167,146.594251718,0.759969109),
     (406,0,0.000133,-5331.357443741,5.409708274),
     (407,0,0.000154,95.979227218,3.366347908),
     (408,0,0.000148,-6418.140930027,3.384105853),
     (409,0,0.000128,-6525.804453965,3.803419984),
     (410,0,0.000130,11293.470674356,0.939031987),
     (411,0,0.000152,-5729.506447149,0.734109449),
     (412,0,0.000138,210.117701700,2.564216078),
     (413,0,0.000123,6066.595360816,4.517057401),
     (414,0,0.000140,18451.078546566,0.642045732),
     (415,0,0.000126,11300.584221356,3.485278510),
     (416,0,0.000119,10027.903195729,3.217431116),
     (417,0,0.000151,4274.518310832,4.404368737),
     (418,0,0.000117,6072.958148291,0.366324669),
     (419,0,0.000165,-7668.637425316,4.298212066),
     (420,0,0.000117,-6245.048177356,5.379518958),
     (421,0,0.000130,-5888.449964932,4.527681176),
     (422,0,0.000121,-543.918059096,6.109429891),
     (423,0,0.000162,9683.594581116,5.720092430),
     (424,0,0.000140,6219.339951688,0.679052066),
     (425,0,0.000118,22743.409379516,4.881123762),
     (426,0,0.000129,1692.165669502,0.351464385),
     (427,0,0.000126,5657.405657679,5.146615566),
     (428,0,0.000114,728.762966531,0.520697196),
     (429,0,0.000120,52.596639600,0.948516300),
     (430,0,0.000126,5881.403728234,5.577502478),
     (431,0,0.000114,65.220371012,3.504914846),
     (432,0,0.000158,163096.180360810,2.957128505),
     (433,0,0.000134,12341.806904281,2.598667997),
     (434,0,0.000151,16627.370915377,3.985702154),
     (435,0,0.000109,1368.660252845,0.014698352),
     (436,0,0.000131,6211.263196841,0.085079447),
     (437,0,0.000146,5792.741760812,0.708424962),
     (438,0,0.000146,-77.750543984,3.121576600),
     (439,0,0.000107,5341.013788022,0.288219117),
     (440,0,0.000138,6281.591377283,2.797425775),
     (441,0,0.000113,-6277.552925684,2.788908647),
     (442,0,0.000115,-525.758811831,5.895418948),
     (443,0,0.000138,6016.468808270,6.096188998),
     (444,0,0.000139,23539.707386333,2.028195448),
     (445,0,0.000146,-4176.041342449,4.660014159),
     (446,0,0.000107,16062.184526117,4.066519274),
     (447,0,0.000142,83783.548222301,2.936314662),
     (448,0,0.000128,9380.959672717,3.223847740),
     (449,0,0.000134,6205.325306008,1.638929505),
     (450,0,0.000101,2699.734819318,5.481620521),
     (451,0,0.000104,-568.821874027,2.205734493),
     (452,0,0.000100,23937.856389741,4.035027831),
     (453,0,0.000103,6321.103522627,2.440421099),
     (454,0,0.000100,17796.959166786,1.845872316),
     (455,0,0.000099,-6019.991926619,2.450634809),
     (456,0,0.000118,6321.208885629,2.547259985),
     (457,0,0.000138,1975.492545856,2.314608535),
     (458,0,0.000121,137.033024162,4.539108237),
     (459,0,0.000123,19402.796952817,4.538077914),
     (460,0,0.000096,5355.235881489,2.341448038),
     (461,0,0.000097,16840.670010815,5.824092825),
     (462,0,0.000119,22805.735565994,2.869025959),
     (463,0,0.000097,-5727.086565097,5.604640781),
     (464,0,0.000097,5779.683204697,1.627068604),
     (465,0,0.000096,5010.394824364,1.872493131),
     (466,0,0.000133,64471.991240625,6.056404102),
     (467,0,0.000094,6272.030149727,2.581162845),
     (468,0,0.000129,-85.827298831,2.540635083),
     (469,0,0.000131,13613.804277336,4.005732849),
     (470,0,0.000104,9814.604100291,1.959967249),
     (471,0,0.000112,16097.679950283,3.589026277),
     (472,0,0.000123,2107.034507542,1.728626455),
     (473,0,0.000091,-5327.476108383,4.377591178),
     (474,0,0.000121,36949.230808424,6.072329857),
     (475,0,0.000098,12964.300703391,6.016597025),
     (476,0,0.000088,26084.021806216,5.271879510),
     (477,0,0.000086,-117.319868220,3.321723333),
     (478,0,0.000108,-12539.853380183,3.716146547),
     (479,0,0.000097,956.289155971,1.014504256),
     (480,0,0.000096,6006.040856250,0.227714257),
     (481,0,0.000113,-7875.671863624,2.725766873),
     (482,0,0.000083,6151.533888305,3.065766231),
     (483,0,0.000109,4171.425536614,4.033365183),
     (484,0,0.000113,7330.728427345,0.656368314),
     (485,0,0.000101,6247.911759770,3.441346740),
     (486,0,0.000113,51092.726050855,2.791483063),
     (487,0,0.000090,6357.857448559,0.679091883),
     (488,0,0.000106,5621.842923210,1.815323658),
     (489,0,0.000093,6241.973868937,5.189536563),
     (490,0,0.000081,200.768922466,5.754403746),
     (491,0,0.000101,111.430161497,5.711033677),
     (492,0,0.000094,34520.309309381,0.495228011),
     (493,0,0.000079,29864.334027309,5.173509877),
     (494,0,0.000085,17157.061880472,4.358526413),
     (495,0,0.000103,909.818733055,2.812745443),
     (496,0,0.000076,203.737867882,2.525545903),
     (497,0,0.000095,394.625885059,2.901319159),
     (498,0,0.000074,22490.962121493,4.619424283),
     (499,0,0.000077,340.770892045,3.116282992),
     (500,0,0.000101,1790.642637886,1.965746074),
     (501,0,0.000076,90279.923167730,1.970905013),
     (502,0,0.000079,22476.735027492,1.160334869),
     (503,0,0.000080,6066.253636204,2.762861548),
     (504,0,0.000080,-6073.299872902,1.004184782),
     (505,0,0.000086,10021.837280099,1.498027361),
     (506,0,0.000074,1155.361157407,0.496610240),
     (507,0,0.000096,853.196381752,3.464221124),
     (508,0,0.000095,5931.259257683,5.513287193),
     (509,0,0.000088,17363.247428909,5.345239614),
     (510,0,0.000076,9924.810421511,2.438054570),
     (511,0,0.000075,12323.423096009,5.291574383),
     (512,0,0.000095,16943.762785034,0.584230713),
     (513,0,0.000092,211.814622730,1.992487951),
     (514,0,0.000092,6060.215526998,6.181639733),
     (515,0,0.000093,350.332119600,3.692973710),
     (516,0,0.000076,12721.572099417,2.771306349),
     (517,0,0.000068,4914.415597146,2.943572588),
     (518,0,0.000068,25685.872802808,0.433143149),
     (519,0,0.000067,29826.306354673,2.721471349),
     (520,0,0.000064,6165.755981771,3.638190401),
     (521,0,0.000076,217.231248701,4.049322586),
     (522,0,0.000064,6599.467719648,5.602828416),
     (523,0,0.000064,661.232926781,0.583118577),
     (524,0,0.000081,24356.780788642,1.389651453),
     (525,0,0.000079,942.062061969,5.298166475),
     (526,0,0.000066,27707.542494295,4.961338947),
     (527,0,0.000067,5983.949455722,4.599907574),
     (528,0,0.000074,-491.663292459,5.688572582),
     (529,0,0.000061,-216.480489176,4.257712487),
     (530,0,0.000062,-467.964990354,3.821672656),
     (531,0,0.000085,-174242.465964423,5.870285569),
     (532,0,0.000061,12242.646283325,6.271668196),
     (533,0,0.000075,22345.260376108,0.127482987),
     (534,0,0.000084,327574.514276408,5.981949016),
     (535,0,0.000070,156137.475983679,3.770978119),
     (536,0,0.000060,21424.466644303,3.416663097),
     (537,0,0.000059,380.127767960,3.201801268),
     (538,0,0.000069,23020.653086588,4.440892846),
     (539,0,0.000081,166573.828539107,4.711111793),
     (540,0,0.000057,26880.319813033,5.837932813),
     (541,0,0.000064,19004.647949408,6.075362619),
     (542,0,0.000079,221995.028799629,2.431665340),
     (543,0,0.000079,24279.107018214,3.516589596),
     (544,0,0.000058,1385.895276336,1.250596905),
     (545,0,0.000067,-6528.907496221,5.735845470),
     (546,0,0.000071,24065.807922776,4.286431412),
     (547,0,0.000071,26735.945262213,4.363525223),
     (548,0,0.000054,16310.979045721,4.896042166),
     (549,0,0.000053,15141.390794312,1.804548182),
     (550,0,0.000053,15664.035522709,5.658533787),
     (551,0,0.000059,24492.406113652,2.499520280),
     (552,0,0.000057,8982.810669309,4.626702867),
     (553,0,0.000052,-532.872358832,0.048857993),
     (554,0,0.000057,5842.250565114,5.020951408),
     (555,0,0.000072,4214.069015085,3.389748652),
     (556,0,0.000053,-77690.759505365,3.174788182),
     (557,0,0.000053,77736.783430129,1.005355692),
     (558,0,0.000070,490.334089179,2.640431404),
     (559,0,0.000057,1021.248894551,1.875692085),
     (560,0,0.000054,1375.773799846,3.673945628),
     (561,0,0.000070,-7576.560073574,5.396662257),
     (562,0,0.000070,55022.935747074,4.215109173),
     (563,0,0.000051,34513.263072683,5.937318081),
     (564,0,0.000052,18774.445951407,5.607999086),
     (565,0,0.000050,21548.962369292,2.844527609),
     (566,0,0.000050,-401.672121757,2.925015195),
     (567,0,0.000051,526.509571357,6.179992490),
     (568,0,0.000056,1169.588251409,2.942260988),
     (569,0,0.000050,34911.412076091,4.461326417),
     (570,0,0.000055,5219.761766539,3.568662519),
     (571,0,0.000068,180.242083090,4.007350087),
     (572,0,0.000056,4977.862273573,1.671798273),
     (573,0,0.000058,26482.170809624,3.752608035),
     (574,0,0.000062,23006.425992586,1.525651572),
     (575,0,0.000053,-2787.043023857,0.514886808),
     (576,0,0.000056,95143.132921351,2.611111995),
     (577,0,0.000065,86464.613316831,3.006591639),
     (578,0,0.000047,10241.202291167,2.467540150),
     (579,0,0.000047,5973.797527336,1.145070481),
     (580,0,0.000055,-238004.524156863,5.755254821),
     (581,0,0.000062,22380.755800274,5.230149350),
     (582,0,0.000053,-647.010833315,5.463561545),
     (583,0,0.000051,302.164775655,0.022302127),
     (584,0,0.000061,39609.654583166,5.378723605),
     (585,0,0.000049,18811.094514336,5.780111682),
     (586,0,0.000049,10239.583866011,2.240694484),
     (587,0,0.000054,11616.976091013,0.974075205),
     (588,0,0.000061,-966.970877436,3.574164554),
     (589,0,0.000064,-2139.567058334,3.685391660),
     (590,0,0.000046,29026.485229508,0.800021722),
     (591,0,0.000047,6288.598774299,2.227990651),
     (592,0,0.000053,16522.659716002,2.623443627),
     (593,0,0.000062,305281.943069182,4.787221197),
     (594,0,0.000051,453.424893819,2.798815402),
     (595,0,0.000047,5870.704753117,2.030438449),
     (596,0,0.000051,97238.627544861,1.155803603),
     (597,0,0.000061,-172146.971340914,4.414982887),
     (598,0,0.000053,222.860322994,2.049029653),
     (599,0,0.000059,40879.440504644,1.218331105),
     (600,0,0.000049,16207.886271502,3.588394524),
     (601,0,0.000051,1795.258443721,5.995111195),
     (602,0,0.000045,5227.626073065,0.849609933),
     (603,0,0.000050,12249.759830326,1.979510592),
     (604,0,0.000059,11638.311731480,4.809157673),
     (605,0,0.000047,984.600331622,1.656200129),
     (606,0,0.000043,1596.186442285,4.336325106),
     (607,0,0.000044,265.989293477,1.400631813),
     (608,0,0.000057,1045.154836188,5.811606858),
     (609,0,0.000054,6136.481598273,0.989553677),
     (610,0,0.000044,-7834.121072639,0.874812358),
     (611,0,0.000053,18216.443810661,5.686868304),
     (612,0,0.000046,11520.996863795,2.043352575),
     (613,0,0.000041,4377.611085051,0.871068571),
     (614,0,0.000051,351.816592309,3.977514617),
     (615,0,0.000050,2301.585815909,0.610804414),
     (616,0,0.000055,34115.114069275,1.064878451),
     (617,0,0.000055,6171.645688495,2.356880342),
     (618,0,0.000040,-799.821125165,4.739468304),
     (619,0,0.000040,6819.880362087,6.155030983),
     (620,0,0.000040,5209.466825801,1.066380893),
     (621,0,0.000055,9967.453899982,4.529307987),
     (622,0,0.000039,4157.198442612,6.267435242),
     (623,0,0.000044,6379.055077209,0.821833837),
     (624,0,0.000038,1685.052122502,5.992277967),
     (625,0,0.000039,28628.336226100,2.270088862),
     (626,0,0.000046,33990.618344286,0.181474159),
     (627,0,0.000037,12566.219010286,4.404053287),
     (628,0,0.000037,5860.409812379,5.206465026),
     (629,0,0.000044,1898.351217940,4.851445369),
     (630,0,0.000037,9910.583327509,4.292672255),
     (631,0,0.000038,24602.612434871,4.337489198),
     (632,0,0.000044,3104.930059424,1.114415424),
     (633,0,0.000036,5654.224263941,6.118202435),
     (634,0,0.000048,5444.106562241,4.550095512),
     (635,0,0.000049,6346.811748295,1.060064784),
     (636,0,0.000046,44809.650200863,2.689891398),
     (637,0,0.000043,6168.676743078,3.251021644),
     (638,0,0.000041,21947.111372700,1.683734557),
     (639,0,0.000038,-216.822213787,2.621316877),
     (640,0,0.000035,4797.095728926,4.573361669),
     (641,0,0.000048,11260.938123564,2.101233218),
     (642,0,0.000038,5753.452195200,2.501444650),
     (643,0,0.000038,-5753.317574594,0.196045084),
     (644,0,0.000041,305.346169393,0.062395378),
     (645,0,0.000037,-1039.026610790,0.191469985),
     (646,0,0.000036,-503.392645295,6.256082675),
     (647,0,0.000039,209.775977089,1.145728092),
     (648,0,0.000034,-5490.300961524,3.131143359),
     (649,0,0.000034,5753.336700787,4.267427502),
     (650,0,0.000034,5753.433069007,1.182056031),
     (651,0,0.000039,15265.886519300,0.713496791),
     (652,0,0.000037,32367.097656208,0.982659910),
     (653,0,0.000034,12455.945378763,3.661633793),
     (654,0,0.000039,12669.244474201,0.585657507),
     (655,0,0.000033,555.989284894,0.936661600),
     (656,0,0.000043,5755.832565452,3.760803307),
     (657,0,0.000043,5750.937204342,1.689809593),
     (658,0,0.000041,10881.099577481,2.777472423),
     (659,0,0.000039,28286.990484861,3.089852874),
     (660,0,0.000045,90394.823013051,4.437662642),
     (661,0,0.000033,4957.086878080,3.953025393),
     (662,0,0.000032,29822.783236324,1.388904868),
     (663,0,0.000032,35707.710082907,4.958963072),
     (664,0,0.000032,8858.314944321,0.168615861),
     (665,0,0.000033,1272.681025627,2.538788841),
     (666,0,0.000038,11510.701923057,5.489270518),
     (667,0,0.000038,11502.837616530,2.294892003),
     (668,0,0.000042,30666.154958433,5.821773300),
     (669,0,0.000043,58953.145443294,5.640719471),
     (670,0,0.000037,6204.362098161,5.666355829),
     (671,0,0.000043,10866.872483480,5.962622991),
     (672,0,0.000041,191.207694910,2.661359175),
     (673,0,0.000041,835.037134487,0.424622644),
     (674,0,0.000033,33794.543723529,0.357518711),
     (675,0,0.000036,239424.390253233,6.127552257),
     (676,0,0.000034,17892.938394004,5.767177381),
     (677,0,0.000042,5298.475518369,6.057471103),
     (678,0,0.000035,-149144.467085503,5.267899478),
     (679,0,0.000041,25287.723799400,1.855965771),
     (680,0,0.000037,-12566.084389680,4.576621702),
     (681,0,0.000034,6311.525037459,0.964105576),
     (682,0,0.000036,24383.079108441,5.483697045),
     (683,0,0.000035,6212.226404687,2.587984554),
     (684,0,0.000030,195.139848173,1.939897694),
     (685,0,0.000031,57375.801900846,0.306501257),
     (686,0,0.000032,15508.615123274,5.702761798),
     (687,0,0.000034,5845.431958851,3.505287923),
     (688,0,0.000034,-14919.017853755,0.656251484),
     (689,0,0.000030,10138.109516949,5.591779332),
     (690,0,0.000030,31415.379249957,6.057848965),
     (691,0,0.000029,11396.563448574,5.803147549),
     (692,0,0.000032,-371.850683608,4.819461968),
     (693,0,0.000032,5422.765921239,0.093091411),
     (694,0,0.000033,5483.254724826,0.668178340),
     (695,0,0.000028,4384.724632052,4.128185638),
     (696,0,0.000035,18202.216716659,2.754592497),
     (697,0,0.000028,10763.779709261,0.193391192),
     (698,0,0.000032,5554.312883460,5.298629222),
     (699,0,0.000030,24491.425792583,4.399975499),
     (700,0,0.000031,82937.465387549,2.905441812),
     (701,0,0.000027,35050.000274475,5.218837640),
     (702,0,0.000030,4583.796633488,1.423415677),
     (703,0,0.000032,161710.618785486,4.070627129),
     (704,0,0.000032,-3116.659412260,0.945620760),
     (705,0,0.000029,11925.274092601,0.224228428),
     (706,0,0.000027,18606.498946000,1.018568623),
     (707,0,0.000027,-3209.070465013,2.077841152),
     (708,0,0.000030,-6453.748720611,1.474002492),
     (709,0,0.000036,528.206492386,1.728662470),
     (710,0,0.000029,7856.896274090,1.629917159),
     (711,0,0.000029,11492.542675792,5.706115256),
     (712,0,0.000027,32217.200181081,4.749721762),
     (713,0,0.000028,7863.942510788,0.917886929),
     (714,0,0.000026,5024.621918366,3.850028038),
     (715,0,0.000026,37455.726495974,1.463316811),
     (716,0,0.000036,-11146.285603613,5.685821413),
     (717,0,0.000032,12074.488407524,2.980593698),
     (718,0,0.000030,12492.854574124,1.430245477),
     (719,0,0.000028,10991.305898701,3.078445565),
     (720,0,0.000034,5791.412557533,6.056129159),
     (721,0,0.000030,20452.869412222,4.120936841),
     (722,0,0.000024,12779.450795421,5.099723598),
     (723,0,0.000028,-310145.152822804,3.997070813),
     (724,0,0.000033,-71960.386582850,2.259850680),
     (725,0,0.000034,9999.986450773,5.443568196),
     (726,0,0.000025,10667.800482043,0.893976515),
     (727,0,0.000028,-65857.552815949,1.340268119),
     (728,0,0.000025,61306.011597066,1.693319974),
     (729,0,0.000025,-13341.674311307,3.917436692),
     (730,0,0.000033,6303.431169390,1.915439078),
     (731,0,0.000024,27177.851529200,4.498236762),
     (732,0,0.000027,5752.421677050,0.847468579),
     (733,0,0.000032,-4492.433212106,4.939103192),
     (734,0,0.000027,5754.348092743,4.601871871),
     (735,0,0.000025,-10419.986283508,0.151361593),
     (736,0,0.000030,48739.859897083,4.123767980),
     (737,0,0.000026,77710.248349398,0.798803853),
     (738,0,0.000026,-77717.294586096,2.968242540),
     (739,0,0.000026,29424.634232916,5.489144298),
     (740,0,0.000028,5373.257116937,1.906542014),
     (741,0,0.000028,424.447323208,2.412208742),
     (742,0,0.000023,792.774888467,0.019075383),
     (743,0,0.000023,-6549.682891713,2.026914001),
     (744,0,0.000031,76251.327770620,1.455201889),
     (745,0,0.000022,1471.753027064,0.572433836),
     (746,0,0.000027,4743.759980179,3.806773062),
     (747,0,0.000023,6592.354172647,2.488773914),
     (748,0,0.000022,22594.054895712,3.686853017),
     (749,0,0.000023,37853.875499383,0.008122258),
     (750,0,0.000028,22910.446765369,5.666518735),
     (751,0,0.000022,8584.661665901,6.033849271),
     (752,0,0.000023,6267.823378806,2.130707272),
     (753,0,0.000024,23536.116957681,0.126800054),
     (754,0,0.000022,6503.488492430,5.053935271),
     (755,0,0.000022,26087.903141574,0.423663225),
     (756,0,0.000030,50317.203439531,0.384629813),
     (757,0,0.000021,5220.512526065,1.323216762),
     (758,0,0.000021,5671.627751145,2.077315111),
     (759,0,0.000024,4708.230048709,4.975713502),
     (760,0,0.000023,-5999.216531126,3.714810368),
     (761,0,0.000026,2636.725472637,0.881015974),
     (762,0,0.000021,-2547.837538232,4.960791425),
     (763,0,0.000021,5618.319804861,1.251586284),
     (764,0,0.000021,-6043.478434754,5.877995449),
     (765,0,0.000022,2111.650313378,5.935090198),
     (766,0,0.000022,5852.545505852,1.253839846),
     (767,0,0.000021,6096.075074353,1.353707033),
     (768,0,0.000026,6057.246581581,2.047696444),
     (769,0,0.000021,33716.965065866,2.436440677),
     (770,0,0.000022,-137288.248433705,2.442700830),
     (771,0,0.000023,-440.825284878,4.775860831),
     (772,0,0.000024,28313.288804661,0.640327654),
     (773,0,0.000020,38650.173506199,0.494115536),
     (774,0,0.000020,518.645264831,1.386877741),
     (775,0,0.000026,6173.130161203,1.160716939),
     (776,0,0.000028,77376.201021662,1.097736670),
     (777,0,0.000027,62883.355139514,0.786887112),
     (778,0,0.000028,983.115858914,3.148232049),
     (779,0,0.000026,6067.329073999,4.548934661),
     (780,0,0.000020,23550.344051683,2.969478678),
     (781,0,0.000027,59414.481874748,1.678648387),
     (782,0,0.000020,5994.995155986,5.779341563),
     (783,0,0.000026,6072.224435108,0.349329481),
     (784,0,0.000020,63658.877750838,3.623253319),
     (785,0,0.000021,9498.212230635,2.273518015),
     (786,0,0.000020,5859.659052853,1.373722336),
     (787,0,0.000020,4811.322822928,5.564111462),
     (788,0,0.000019,6467.925757962,2.812620155),
     (789,0,0.000024,21393.541969858,4.400283318),
     (790,0,0.000026,508.350324092,5.228964263),
     (791,0,0.000026,6663.203617951,0.815907733),
     (792,0,0.000024,355.748745572,5.605212909),
     (793,0,0.000019,-148434.534037318,1.784506569),
     (794,0,0.000025,34596.364654652,0.983108581),
     (795,0,0.000020,65236.221293285,3.086096822),
     (796,0,0.000026,13930.196146993,3.728671542),
     (797,0,0.000024,4598.023727490,4.217924540),
     (798,0,0.000025,7647.120297001,0.421871423),
     (799,0,0.000018,24889.574795992,2.920145602),
     (800,0,0.000022,-9411.464615087,4.825954157),
     (801,0,0.000018,52175.806283148,6.236051636),
     (802,0,0.000025,78051.341913833,6.157603454),
     (803,0,0.000021,-77003.838419562,0.742918495),
     (804,0,0.000020,12526.534191637,3.766164619),
     (805,0,0.000024,80181.537466840,6.014515363),
     (806,0,0.000021,3958.126441176,4.622467607),
     (807,0,0.000024,399715.142942349,1.456955721),
     (808,0,0.000019,5220.170801453,2.547579989),
     (809,0,0.000020,-82576.981221369,0.444221297),
     (810,0,0.000020,628.851586050,3.042198102),
     (811,0,0.000024,4843.566151842,1.506734544),
     (812,0,0.000021,-1062.905048538,0.563700197),
     (813,0,0.000019,430.530344139,4.414157601),
     (814,0,0.000020,7018.952363523,0.228593887),
     (815,0,0.000017,31968.948652799,2.517947231),
     (816,0,0.000017,1258.453931626,5.751756145),
     (817,0,0.000022,494.266242443,4.086822435),
     (818,0,0.000017,36147.409877300,6.222128467),
     (819,0,0.000021,56600.279289522,0.487848453),
     (820,0,0.000021,18208.349942592,2.413090523),
     (821,0,0.000016,-6262.720530593,2.434772628),
     (822,0,0.000017,29289.501842578,0.580572874),
     (823,0,0.000019,6068.813546707,3.725843367),
     (824,0,0.000019,6070.739962400,1.174264114),
     (825,0,0.000020,12529.503137053,4.738592814),
     (826,0,0.000016,2001.443992158,5.646184567),
     (827,0,0.000017,8297.699304780,4.390170142),
     (828,0,0.000018,-1293.484223583,3.728990098),
     (829,0,0.000022,7232.251458961,0.892331957),
     (830,0,0.000021,14867.737515892,2.323569216),
     (831,0,0.000019,-5227.217038151,1.208242272),
     (832,0,0.000017,40077.619573520,4.431034440),
     (833,0,0.000016,-6279.194514633,3.957621442),
     (834,0,0.000016,529.739149204,0.591806757),
     (835,0,0.000020,-527.243284540,2.043070625),
     (836,0,0.000016,31570.799649391,4.146601707),
     (837,0,0.000020,24336.005393149,6.040670372),
     (838,0,0.000016,-529.642780985,5.747590276),
     (839,0,0.000020,5226.875313540,3.986324254),
     (840,0,0.000018,831.855740750,1.575833957),
     (841,0,0.000015,5853.296265378,5.277696357),
     (842,0,0.000015,6069.728570444,0.851314462),
     (843,0,0.000015,6069.824938663,4.049101378),
     (844,0,0.000016,7335.344233180,0.300802774),
     (845,0,0.000021,5894.610694753,5.310714442),
     (846,0,0.000018,554.069987483,0.589193379),
     (847,0,0.000019,52670.069593303,5.552164363),
     (848,0,0.000020,5678.603286329,5.145830778),
     (849,0,0.000015,11240.162728072,6.201543287),
     (850,0,0.000016,6286.957185349,3.871355485),
     (851,0,0.000016,2008.557539159,0.043902344),
     (852,0,0.000014,2648.454825473,2.087709051),
     (853,0,0.000020,532.138645649,3.169312095),
     (854,0,0.000017,2214.743087596,4.577444281),
     (855,0,0.000016,37724.753419748,2.489631986),
     (856,0,0.000018,17576.546524347,3.101103839),
     (857,0,0.000017,27278.468816441,1.498594795),
     (858,0,0.000015,30348.883772767,4.839971062),
     (859,0,0.000018,5579.442665374,1.431169737),
     (860,0,0.000019,6098.348562636,5.162335836),
     (861,0,0.000019,-69853.352075308,3.934032973),
     (862,0,0.000017,6393.282171211,5.234717215),
     (863,0,0.000014,-5732.609489404,4.642717117),
     (864,0,0.000014,1063.314083452,1.430204876),
     (865,0,0.000014,5774.160280389,0.664933293),
     (866,0,0.000013,4480.703859270,1.703516969),
     (867,0,0.000013,1179.194539040,0.747415824),
     (868,0,0.000018,-73783.561771528,5.648881672),
     (869,0,0.000013,-1325.988971911,5.806502279),
     (870,0,0.000013,6079.337982109,3.966056199),
     (871,0,0.000013,529.758275397,1.813470697),
     (872,0,0.000017,6130.543707440,5.626911182),
     (873,0,0.000017,66813.564835733,2.214701203),
     (874,0,0.000018,166754.070622198,5.566252879),
     (875,0,0.000012,6272.439184642,4.149426519),
     (876,0,0.000016,28759.810877483,5.845736898),
     (877,0,0.000017,238714.457205048,0.186119742),
     (878,0,0.000012,27832.038219283,4.701170820),
     (879,0,0.000013,-5754.365205965,2.912795924),
     (880,0,0.000012,19804.827291583,0.277907011),
     (881,0,0.000013,5717.960162245,0.213272942),
     (882,0,0.000012,6137.444806120,0.789584989),
     (883,0,0.000017,5952.456886333,5.107819032),
     (884,0,0.000014,46360.695423511,1.262830553),
     (885,0,0.000016,28230.187222691,3.675950374),
     (886,0,0.000014,30220.932239732,3.314675222),
     (887,0,0.000014,89569.990119545,5.454034421),
     (888,0,0.000014,9711.511326073,2.981414434),
     (889,0,0.000012,4068.332762395,4.574130592),
     (890,0,0.000012,-8273.820867032,4.011335853),
     (891,0,0.000013,12320.320053753,6.119122820),
     (892,0,0.000016,49515.382508407,0.128946595),
     (893,0,0.000016,6566.935168857,3.750617116),
     (894,0,0.000012,2221.856634597,0.813003023),
     (895,0,0.000011,-58864.543917773,4.097088330),
     (896,0,0.000013,17473.453750128,5.662676815),
     (897,0,0.000015,44034.127589539,3.322395062),
     (898,0,0.000015,23116.632313806,3.404636004),
     (899,0,0.000011,7122.045137742,1.080531571),
     (900,0,0.000014,42430.485727292,1.102408430),
     (901,0,0.000011,-3607.219468422,3.484803175),
     (902,0,0.000013,-6293.712515341,4.251540563),
     (903,0,0.000012,5752.404563829,5.219639865),
     (904,0,0.000011,5942.304957947,1.172189672),
     (905,0,0.000011,23123.745860807,4.088648276),
     (906,0,0.000012,5752.863620035,5.620137036),
     (907,0,0.000012,5753.906149759,6.112464351),
     (908,0,0.000015,80957.060078164,5.264923179),
     (909,0,0.000013,2854.640373910,3.598387724),
     (910,0,0.000011,21150.813365884,4.891938880),
     (911,0,0.000013,-529.623654792,0.884018986),
     (912,0,0.000015,83922.065719171,4.612300211),
     (913,0,0.000012,530.654172941,4.010698624),
     (914,0,0.000015,10550.480613823,6.248275346),
     (915,0,0.000015,94325.032709270,2.715678533),
     (916,0,0.000011,27035.740212467,5.663159895),
     (917,0,0.000011,39601.891912450,2.849080967),
     (918,0,0.000012,33163.395663024,5.719550247),
     (919,0,0.000013,-11216.284290324,3.021359854),
     (920,0,0.000010,29303.728936579,4.000317471),
     (921,0,0.000011,17370.360975910,5.081915891),
     (922,0,0.000013,5294.254143229,1.516822639),
     (923,0,0.000014,83958.714282101,4.781643468),
     (924,0,0.000011,1603.641862248,0.013838062),
     (925,0,0.000014,89220.541237541,1.220068610),
     (926,0,0.000011,29.821438149,1.394523099),
     (927,0,0.000012,16193.659177500,2.035993979),
     (928,0,0.000011,53131.406024757,4.303020978),
     (929,0,0.000011,27433.889215875,0.030613211),
     (930,0,0.000011,17686.752845566,3.912544076),
     (931,0,0.000011,-5597.964485463,4.240242706),
     (932,0,0.000014,81758.881009288,5.491852389),
     (933,0,0.000013,-11.045700264,3.387140655),
     (934,0,0.000010,4900.188503145,4.893344728),
     (935,0,0.000014,5719.444634953,0.336008800),
     (936,0,0.000013,69166.430989505,3.986724700),
     (937,0,0.000010,3641.734571519,4.569463345),
     (938,0,0.000014,5828.166483464,0.356776446),
     (939,0,0.000012,5932.743730391,5.762927211),
     (940,0,0.000012,5720.852334105,3.264101105),
     (941,0,0.000010,5908.805284331,0.263407376),
     (942,0,0.000011,-11510.292888143,2.711642605),
     (943,0,0.000013,12573.265246984,5.116247831),
     (944,0,0.000011,50290.905119731,2.742187943),
     (945,0,0.000011,5299.959991078,1.506596301),
     (946,0,0.000011,-2598.697800001,0.778189191),
     (947,0,0.000013,26709.646942413,0.212926369),
     (948,0,0.000012,25960.422650889,1.977473070),
     (949,0,0.000011,-6845.299365877,5.916622542),
     (950,0,0.000012,6061.699999706,4.954682633),
     (951,0,0.000010,46283.116765849,3.044395580),
     (952,0,0.000012,77828.671313068,4.074698716),
     (953,0,0.000010,34570.066334853,4.940463448),
     (954,0,0.000012,99030.765016814,3.439467385),
     (955,0,0.000012,11954.703601137,4.533013479),
     (956,0,0.000011,73096.640685725,5.419400410),
     (957,0,0.000010,-77184.080502653,1.468955136),
     (958,0,0.000012,6122.466952593,5.065800174),
     (959,0,0.000010,6306.954287739,5.241394098),
     (960,0,0.000011,68390.908378181,4.892742986),
     (961,0,0.000011,6356.372975850,2.119816766),
     (962,0,0.000011,38500.276031072,6.033296077),
     (963,0,0.000011,6420.108874154,1.428738560),
     (964,0,0.000010,18624.882754272,4.434296811),
     (965,0,0.000010,58177.622831970,3.301567375),
     (966,0,0.000011,85382.408893063,4.041847239),
     (967,0,0.000011,8695.848308188,2.995497137),
     (968,0,0.000011,18415.515812097,1.904982743),
     (969,0,0.000011,5261.826955440,5.877910877),
     (970,0,0.000010,17544.013973556,5.454944310),
     (971,0,0.000010,-13241.780227122,0.857937528),
     (1,1,102.156724,6283.075849991,4.249032005),
     (2,1,1.706807,12566.151699983,4.205904248),
     (3,1,0.269716,213.299095438,3.400290383),
     (4,1,0.265921,529.690965095,5.836047389),
     (5,1,0.210568,-3.523118349,6.262738116),
     (6,1,0.077996,5223.693919802,4.670344691),
     (7,1,0.054764,1577.343542448,4.534800170),
     (8,1,0.059146,26.298319800,1.083044739),
     (9,1,0.034420,-398.149003408,5.980077347),
     (10,1,0.032088,18849.227549974,4.162913471),
     (11,1,0.033595,5507.553238667,5.980162321),
     (12,1,0.029203,5856.477659115,0.623811235),
     (13,1,0.027764,155.420399434,3.745318113),
     (14,1,0.025190,5746.271337896,2.980330487),
     (15,1,0.022997,-796.298006816,1.174411864),
     (16,1,0.024976,5760.498431898,2.467913714),
     (17,1,0.021778,206.185548437,3.854785823),
     (18,1,0.017925,-775.522611324,1.092065955),
     (19,1,0.013797,426.598190876,2.699825203),
     (20,1,0.013278,6062.663207553,5.845801830),
     (21,1,0.011774,12036.460734888,2.292831923),
     (22,1,0.012871,6076.890301554,5.333425657),
     (23,1,0.012152,1059.381930189,6.222874913),
     (24,1,0.010143,4694.002954708,4.044015152),
     (25,1,0.010341,-7.113547001,5.190161176),
     (26,1,0.009357,5486.777843175,3.416081409),
     (27,1,0.010084,522.577418094,0.749319671),
     (28,1,0.008587,10977.078804699,2.777152606),
     (29,1,0.008628,6275.962302991,4.562059524),
     (30,1,0.008159,-220.412642439,5.806891625),
     (31,1,0.007746,2544.314419883,1.603197503),
     (32,1,0.007670,2146.165416475,3.000200440),
     (33,1,0.007087,74.781598567,0.443748490),
     (34,1,0.006180,-536.804512095,1.302642725),
     (35,1,0.005818,5088.628839767,4.827723531),
     (36,1,0.004945,-6286.598968340,0.268305430),
     (37,1,0.004774,1349.867409659,5.808636673),
     (38,1,0.004687,-242.728603974,5.154890570),
     (39,1,0.006089,1748.016413067,4.403765114),
     (40,1,0.005975,-1194.447010225,2.583472591),
     (41,1,0.004229,951.718406251,0.931172179),
     (42,1,0.005264,553.569402842,2.336107252),
     (43,1,0.003049,5643.178563677,1.362636367),
     (44,1,0.002974,6812.766815086,1.583011914),
     (45,1,0.003403,-2352.866153772,2.552190795),
     (46,1,0.003030,419.484643875,5.286429815),
     (47,1,0.003210,-7.046236698,1.863796539),
     (48,1,0.003058,9437.762934887,4.226420634),
     (49,1,0.002590,12352.852604545,1.991935641),
     (50,1,0.002927,5216.580372801,2.319953553),
     (51,1,0.002425,5230.807466803,3.084753336),
     (52,1,0.002656,3154.687084896,2.487447867),
     (53,1,0.002445,10447.387839604,2.347139059),
     (54,1,0.002990,4690.479836359,6.235872050),
     (55,1,0.002890,5863.591206116,0.095192438),
     (56,1,0.002498,6438.496249426,2.994779801),
     (57,1,0.001889,8031.092263058,3.569003716),
     (58,1,0.002567,801.820931124,3.425611498),
     (59,1,0.001803,-71430.695617756,2.192295974),
     (60,1,0.001783,3.932153263,5.180433689),
     (61,1,0.001694,-4705.732307544,4.641779185),
     (62,1,0.001704,-1592.596013633,3.997097652),
     (63,1,0.001736,5849.364112115,0.417556971),
     (64,1,0.001643,8429.241266467,2.180619289),
     (65,1,0.001671,38.133035638,4.164529426),
     (66,1,0.002045,7084.896781115,0.526323854),
     (67,1,0.001458,4292.330832950,1.356098141),
     (68,1,0.001437,20.355319399,3.895439360),
     (69,1,0.001738,6279.552731642,0.087484395),
     (70,1,0.001367,14143.495242431,3.987576017),
     (71,1,0.001344,7234.794256242,0.090454337),
     (72,1,0.001438,11499.656222793,0.974385788),
     (73,1,0.001257,6836.645252834,1.509069402),
     (74,1,0.001358,11513.883316794,0.495571510),
     (75,1,0.001170,103.092774219,2.838507861),
     (76,1,0.001628,7632.943259650,4.968445754),
     (77,1,0.001162,4164.311989613,3.408367809),
     (78,1,0.001092,6069.776754553,3.617975121),
     (79,1,0.001008,17789.845619785,0.286349493),
     (80,1,0.001008,639.897286314,1.610807804),
     (81,1,0.000918,10213.285546211,5.532795159),
     (82,1,0.001011,-6256.777530192,0.661826484),
     (83,1,0.000753,16730.463689596,3.905033039),
     (84,1,0.000737,11926.254413669,4.641954601),
     (85,1,0.000694,3340.612426700,2.111120606),
     (86,1,0.000701,3894.181829542,2.760823771),
     (87,1,0.000689,-135.065080035,4.768800780),
     (88,1,0.000700,13367.972631107,5.760441350),
     (89,1,0.000664,6040.347246017,1.051215840),
     (90,1,0.000654,5650.292110678,4.911401622),
     (91,1,0.000788,6681.224853400,4.699645475),
     (92,1,0.000628,5333.900241022,5.024602804),
     (93,1,0.000755,-110.206321219,4.371083892),
     (94,1,0.000628,6290.189396992,3.660487466),
     (95,1,0.000635,25132.303399966,4.121048430),
     (96,1,0.000534,5966.683980335,1.173282418),
     (97,1,0.000544,-433.711737877,0.345499370),
     (98,1,0.000517,-1990.745017041,5.414571768),
     (99,1,0.000504,5767.611978898,2.328281271),
     (100,1,0.000485,5753.384884897,1.685870696),
     (101,1,0.000463,7860.419392439,5.297686200),
     (102,1,0.000604,515.463871093,0.591998850),
     (103,1,0.000443,12168.002696575,4.830881207),
     (104,1,0.000570,199.072001436,3.899196894),
     (105,1,0.000465,10969.965257698,0.476681560),
     (106,1,0.000424,-7079.373856808,1.112243815),
     (107,1,0.000478,-6127.655450557,3.778025484),
     (108,1,0.000427,735.876513532,1.994145504),
     (109,1,0.000414,10973.555686350,5.441089088),
     (110,1,0.000512,1589.072895284,0.107127187),
     (111,1,0.000378,10984.192351700,0.915084854),
     (112,1,0.000402,11371.704689758,4.107281709),
     (113,1,0.000453,9917.696874510,1.917498398),
     (114,1,0.000394,149.563197135,2.763006087),
     (115,1,0.000371,5739.157790895,3.112111573),
     (116,1,0.000350,11790.629088659,0.440639861),
     (117,1,0.000344,412.371096874,5.676829991),
     (118,1,0.000356,6133.512652857,5.445011146),
     (119,1,0.000383,955.599741609,5.559734846),
     (120,1,0.000333,6496.374945429,0.261543845),
     (121,1,0.000340,6055.549660552,5.975534983),
     (122,1,0.000334,1066.495477190,2.335064139),
     (123,1,0.000399,11506.769769794,5.321238012),
     (124,1,0.000314,18319.536584880,2.313310261),
     (125,1,0.000424,1052.268383188,1.211888314),
     (126,1,0.000307,63.735898303,3.169807146),
     (127,1,0.000329,29.821438149,6.106912080),
     (128,1,0.000357,6309.374169791,4.223760346),
     (129,1,0.000312,-3738.761430108,2.180301114),
     (130,1,0.000268,12043.574281889,2.447513265),
     (131,1,0.000301,309.278322656,1.499775306),
     (132,1,0.000256,12491.370101415,3.662330347),
     (133,1,0.000290,625.670192312,1.272718610),
     (134,1,0.000256,5429.879468239,1.913411801),
     (135,1,0.000339,3496.032826134,4.165935118),
     (136,1,0.000283,3930.209696220,4.326206593),
     (137,1,0.000304,4686.889407707,1.612313128),
     (138,1,0.000240,12528.018664345,3.832324322),
     (139,1,0.000259,16200.772724501,3.470177067),
     (140,1,0.000238,12139.553509107,1.147977852),
     (141,1,0.000236,6172.869528772,3.776263897),
     (142,1,0.000296,-7058.598461315,0.460325820),
     (143,1,0.000306,10575.406682942,0.554750882),
     (144,1,0.000251,17298.182327326,0.834331855),
     (145,1,0.000290,4732.030627343,4.759564091),
     (146,1,0.000261,5884.926846583,0.298267928),
     (147,1,0.000249,5547.199336460,3.749356886),
     (148,1,0.000213,11712.955318231,5.415660337),
     (149,1,0.000223,4701.116501708,2.703211823),
     (150,1,0.000268,-640.877607382,0.283605176),
     (151,1,0.000209,5636.065016677,1.238406909),
     (152,1,0.000193,10177.257679534,1.943253943),
     (153,1,0.000182,6283.143160294,2.456157599),
     (154,1,0.000184,-227.526189440,5.888038582),
     (155,1,0.000182,-6283.008539689,0.241332086),
     (156,1,0.000228,-6284.056171060,2.657324756),
     (157,1,0.000166,7238.675591600,5.930629110),
     (158,1,0.000167,3097.883822726,5.570962928),
     (159,1,0.000160,-323.505416657,5.786380592),
     (160,1,0.000154,-4136.910433516,1.517853728),
     (161,1,0.000176,12029.347187887,3.139261630),
     (162,1,0.000168,12132.439962106,3.556565199),
     (163,1,0.000153,202.253395174,1.463313961),
     (164,1,0.000157,17267.268201691,1.586836555),
     (165,1,0.000142,83996.847317739,0.022670669),
     (166,1,0.000152,17260.154654690,0.708526057),
     (167,1,0.000144,6084.003848555,5.187058140),
     (168,1,0.000135,5756.566278635,1.993253045),
     (169,1,0.000134,5750.203491159,3.457179468),
     (170,1,0.000144,5326.786694021,6.066230802),
     (171,1,0.000160,11015.106477335,1.710477677),
     (172,1,0.000133,3634.621024518,2.836331278),
     (173,1,0.000134,18073.704938650,5.453106639),
     (174,1,0.000134,1162.474704408,5.326954240),
     (175,1,0.000128,5642.198242609,2.511652591),
     (176,1,0.000160,632.783739313,5.628917915),
     (177,1,0.000132,13916.019109642,0.819293763),
     (178,1,0.000122,14314.168113050,5.677408080),
     (179,1,0.000125,12359.966151546,5.251978648),
     (180,1,0.000121,5749.452731634,2.210905326),
     (181,1,0.000136,-245.831646229,1.646502367),
     (182,1,0.000120,5757.317038160,3.240865957),
     (183,1,0.000134,12146.667056108,3.059477752),
     (184,1,0.000141,17253.041107690,2.069215940),
     (185,1,0.000136,6206.809778716,1.866560322),
     (186,1,0.000129,-7477.522860216,2.781470801),
     (187,1,0.000116,5540.085789459,4.281170439),
     (188,1,0.000116,9779.108676125,3.320925269),
     (189,1,0.000129,5237.921013804,3.497678804),
     (190,1,0.000113,5959.570433334,0.983021339),
     (191,1,0.000122,6282.095528923,2.674938860),
     (192,1,0.000140,-11.045700264,4.957936982),
     (193,1,0.000108,23543.230504682,1.390112919),
     (194,1,0.000106,-12569.674818332,0.429631184),
     (195,1,0.000110,-266.607041722,5.501340161),
     (196,1,0.000115,12559.038152982,4.691456596),
     (197,1,0.000134,-2388.894020449,0.577309618),
     (198,1,0.000109,10440.274292604,6.218148184),
     (199,1,0.000102,-543.918059096,1.477971923),
     (200,1,0.000108,21228.392023546,2.237752900),
     (201,1,0.000101,-4535.059436924,3.100483450),
     (202,1,0.000099,5017.508371365,2.368226122),
     (203,1,0.000103,76.266071276,5.594294322),
     (204,1,0.000095,22483.848574493,5.061938739),
     (205,1,0.000092,-6525.804453965,5.328441170),
     (206,1,0.000090,-8635.942003763,4.819533242),
     (207,1,0.000104,949.175608970,5.674287810),
     (208,1,0.000087,6065.844601290,5.074184517),
     (209,1,0.000085,6073.708907817,6.088586475),
     (210,1,0.000084,11087.285125918,3.041319560),
     (211,1,0.000101,13517.870106233,2.196632355),
     (212,1,0.000087,24072.921469776,6.177670839),
     (213,1,0.000081,210.117701700,4.413151806),
     (214,1,0.000086,16723.350142595,1.642783244),
     (215,1,0.000079,6066.595360816,0.025375166),
     (216,1,0.000096,12345.739057544,5.904566401),
     (217,1,0.000080,9225.539273283,3.059037479),
     (218,1,0.000077,316.391869657,4.970672374),
     (219,1,0.000076,6072.958148291,4.859829129),
     (220,1,0.000077,4590.910180489,5.251224458),
     (221,1,0.000075,15110.466119866,3.209774493),
     (222,1,0.000083,10454.501386605,0.838322812),
     (223,1,0.000086,5113.487598583,3.471756201),
     (224,1,0.000083,-209.366942175,0.260535923),
     (225,1,0.000077,838.969287750,3.042670779),
     (226,1,0.000100,11933.367960670,4.056085566),
     (227,1,0.000072,17996.031168222,5.511200394),
     (228,1,0.000079,21954.157609398,4.676050907),
     (229,1,0.000069,17256.631536341,1.149101966),
     (230,1,0.000069,11403.676995575,2.643831236),
     (231,1,0.000073,11919.140866668,4.409582675),
     (232,1,0.000069,16737.577236597,1.846665352),
     (233,1,0.000066,6179.983075773,5.376466900),
     (234,1,0.000075,9623.688276691,1.385171153),
     (235,1,0.000066,-4933.208440333,5.475293903),
     (236,1,0.000066,-154717.609887309,6.119833116),
     (237,1,0.000065,18635.928454536,1.951714945),
     (238,1,0.000065,-65147.619767764,2.149147540),
     (239,1,0.000065,6489.261398429,3.553064732),
     (240,1,0.000066,2699.734819318,0.697337744),
     (241,1,0.000064,728.762966531,2.214109120),
     (242,1,0.000077,742.990060533,1.295292993),
     (243,1,0.000065,19800.945956225,4.578628378),
     (244,1,0.000081,-5729.506447149,2.217990234),
     (245,1,0.000062,-5331.357443741,0.770218476),
     (246,1,0.000069,5127.714692584,3.041135415),
     (247,1,0.000074,-1551.045222648,4.816747156),
     (248,1,0.000063,16858.482532933,2.695141918),
     (249,1,0.000058,13119.721102825,3.469684213),
     (250,1,0.000061,-5481.254918868,2.767838470),
     (251,1,0.000057,5341.013788022,4.873822728),
     (252,1,0.000062,14945.316173554,2.940633756),
     (253,1,0.000059,17782.732072784,0.385784700),
     (254,1,0.000057,13095.842665077,1.570999687),
     (255,1,0.000074,2379.164473572,6.078177476),
     (256,1,0.000059,23020.653086588,2.762084275),
     (257,1,0.000060,6209.778724132,5.836922414),
     (258,1,0.000060,16460.333529525,3.925340893),
     (259,1,0.000053,5842.250565114,0.493758985),
     (260,1,0.000056,5657.405657679,3.556713363),
     (261,1,0.000051,-3128.388765096,2.743157645),
     (262,1,0.000051,956.289155971,6.035564291),
     (263,1,0.000066,5120.601145584,4.766277828),
     (264,1,0.000050,18052.929543158,5.059103676),
     (265,1,0.000056,7342.457780181,4.214219515),
     (266,1,0.000067,-6262.300454499,4.652697137),
     (267,1,0.000055,25158.601719765,3.654376925),
     (268,1,0.000050,20199.094959633,3.135330919),
     (269,1,0.000058,8827.390269875,2.578621034),
     (270,1,0.000048,29296.615389579,2.455676343),
     (271,1,0.000067,9388.005909415,1.489286970),
     (272,1,0.000055,23006.425992586,3.255181819),
     (273,1,0.000047,18422.629359098,6.060284922),
     (274,1,0.000057,15671.081759407,3.059809026),
     (275,1,0.000061,11769.853693166,4.264670343),
     (276,1,0.000062,9380.959672717,4.728128557),
     (277,1,0.000048,-533.214083444,6.197921344),
     (278,1,0.000045,11609.862544012,5.177103348),
     (279,1,0.000045,6080.822454817,5.992211521),
     (280,1,0.000044,20426.571092422,3.823687085),
     (281,1,0.000042,853.196381752,1.195505176),
     (282,1,0.000041,-216.480489176,6.082624245),
     (283,1,0.000048,846.082834751,2.136353714),
     (284,1,0.000042,22805.735565994,5.663164707),
     (285,1,0.000040,11856.218651798,1.406970403),
     (286,1,0.000041,22743.409379516,0.124796618),
     (287,1,0.000039,224.344795702,4.548264526),
     (288,1,0.000040,21947.111372700,2.945388897),
     (289,1,0.000047,8662.240323563,2.959798417),
     (290,1,0.000038,15720.838784878,4.760348491),
     (291,1,0.000038,-143571.324283697,0.434280581),
     (292,1,0.000040,-2787.043023857,2.006612247),
     (293,1,0.000045,4804.209275927,4.986384367),
     (294,1,0.000037,-532.872358832,1.867545000),
     (295,1,0.000036,17796.959166786,0.416100466),
     (296,1,0.000036,12721.572099417,4.032747411),
     (297,1,0.000036,-647.010833315,0.976478863),
     (298,1,0.000039,6243.458341645,5.715908471),
     (299,1,0.000046,5749.861766548,4.268268050),
     (300,1,0.000046,-5756.908003246,1.969103158),
     (301,1,0.000038,6246.427287062,2.875294704),
     (302,1,0.000033,5209.466825801,2.742324036),
     (303,1,0.000034,18875.525869774,4.232613789),
     (304,1,0.000034,16062.184526117,5.673692508),
     (305,1,0.000033,26735.945262213,0.714253956),
     (306,1,0.000037,12323.423096009,0.408922226),
     (307,1,0.000034,22779.437246194,5.432193476),
     (308,1,0.000032,8982.810669309,6.124060843),
     (309,1,0.000036,-14919.017853755,0.671249206),
     (310,1,0.000030,20995.392966449,0.527334375),
     (311,1,0.000038,19402.796952817,5.884183946),
     (312,1,0.000031,533.623118358,3.119083116),
     (313,1,0.000033,28286.990484861,1.669084181),
     (314,1,0.000031,-1039.026610790,1.683279016),
     (315,1,0.000034,23013.539539587,1.713342965),
     (316,1,0.000033,526.509571357,1.721246928),
     (317,1,0.000030,28237.233459389,6.249867664),
     (318,1,0.000029,23141.558382925,4.991493870),
     (319,1,0.000031,52670.069593303,1.045413384),
     (320,1,0.000029,24889.574795992,4.776837901),
     (321,1,0.000036,2301.585815909,2.103789007),
     (322,1,0.000028,2118.763860378,6.180682032),
     (323,1,0.000031,29088.811415985,5.075736722),
     (324,1,0.000032,5870.704753117,0.267523707),
     (325,1,0.000037,942.062061969,0.683976305),
     (326,1,0.000027,6165.755981771,5.343912395),
     (327,1,0.000035,1045.154836188,1.216811833),
     (328,1,0.000026,-525.758811831,1.649705014),
     (329,1,0.000033,6805.653268085,2.453444103),
     (330,1,0.000026,22003.914634870,0.478748233),
     (331,1,0.000031,21424.466644303,1.041870978),
     (332,1,0.000026,1155.361157407,2.202375553),
     (333,1,0.000026,25685.872802808,1.985699754),
     (334,1,0.000035,22345.260376108,1.561387531),
     (335,1,0.000025,30666.154958433,2.032676020),
     (336,1,0.000025,5436.993015240,0.572566891),
     (337,1,0.000029,2942.463423292,4.761817233),
     (338,1,0.000033,15265.886519300,5.222770851),
     (339,1,0.000030,6058.731054290,4.060982923),
     (340,1,0.000027,23581.258177318,3.586538224),
     (341,1,0.000024,5422.765921239,1.985124822),
     (342,1,0.000024,6197.248551160,3.685965395),
     (343,1,0.000025,415.552490612,1.510076653),
     (344,1,0.000023,6144.558353121,2.602559359),
     (345,1,0.000023,98068.536716305,3.984496091),
     (346,1,0.000024,16193.659177500,1.134615747),
     (347,1,0.000023,1596.186442285,2.808436690),
     (348,1,0.000023,23937.856389741,2.332803501),
     (349,1,0.000024,5010.394824364,3.177368421),
     (350,1,0.000023,51868.248662179,4.026938933),
     (351,1,0.000023,32217.200181081,3.185367150),
     (352,1,0.000022,26084.021806216,0.536307078),
     (353,1,0.000029,25287.723799400,3.431805064),
     (354,1,0.000023,16496.361396202,6.071091311),
     (355,1,0.000024,302.164775655,1.578438056),
     (356,1,0.000022,10344.295065386,3.256720787),
     (357,1,0.000023,1169.588251409,1.312579463),
     (358,1,0.000024,10021.837280099,0.011567642),
     (359,1,0.000021,6219.339951688,4.538974432),
     (360,1,0.000021,21548.962369292,4.506700848),
     (361,1,0.000021,-401.672121757,1.338297254),
     (362,1,0.000022,19004.647949408,0.528054779),
     (363,1,0.000024,6303.851245484,4.754656437),
     (364,1,0.000023,56600.279289522,2.467263131),
     (365,1,0.000023,60530.488985742,0.743288997),
     (366,1,0.000020,38526.574350872,1.614541487),
     (367,1,0.000022,4907.302050146,2.011116328),
     (368,1,0.000027,11925.274092601,5.218769079),
     (369,1,0.000021,1903.436812501,3.512470739),
     (370,1,0.000021,27832.038219283,0.323253481),
     (371,1,0.000021,-440.825284878,0.298537077),
     (372,1,0.000022,22476.735027492,2.785044065),
     (373,1,0.000020,1478.866574064,5.195283853),
     (374,1,0.000024,508.350324092,0.677447658),
     (375,1,0.000019,34596.364654652,3.396795867),
     (376,1,0.000021,11300.584221356,1.799883619),
     (377,1,0.000020,5973.797527336,5.708258427),
     (378,1,0.000019,709.933048185,3.959335805),
     (379,1,0.000022,1581.959348283,1.508970766),
     (380,1,0.000020,28628.336226100,3.762311147),
     (381,1,0.000019,1368.660252845,1.870509029),
     (382,1,0.000019,4157.198442612,1.757646136),
     (383,1,0.000024,1265.567478626,2.145802902),
     (384,1,0.000021,15664.035522709,0.409963724),
     (385,1,0.000019,4914.415597146,1.241974523),
     (386,1,0.000018,12592.450019783,0.477708834),
     (387,1,0.000018,6702.560493867,1.118975518),
     (388,1,0.000023,7872.148745275,5.219748671),
     (389,1,0.000020,-10988.808157535,2.579532667),
     (390,1,0.000019,16522.659716002,2.426520936),
     (391,1,0.000020,6379.055077209,5.556755786),
     (392,1,0.000018,33019.021112205,0.900011580),
     (393,1,0.000018,6819.880362087,4.400983064),
     (394,1,0.000017,11080.171578918,4.534216648),
     (395,1,0.000018,1692.165669502,5.850005552),
     (396,1,0.000021,5554.312883460,3.602286271),
     (397,1,0.000017,30774.501642575,5.285451289),
     (398,1,0.000017,5860.409812379,0.931858685),
     (399,1,0.000018,55798.458358398,5.450260570),
     (400,1,0.000016,6244.942814354,5.114298736),
     (401,1,0.000017,-226858.238553250,4.362112946),
     (402,1,0.000018,48739.859897083,5.924722911),
     (403,1,0.000016,167283.761587292,2.377757823),
     (404,1,0.000016,6321.208885629,4.854748541),
     (405,1,0.000019,5444.106562241,2.859731465),
     (406,1,0.000017,10770.893256262,0.397051800),
     (407,1,0.000017,4171.425536614,2.895189795),
     (408,1,0.000018,28230.187222691,5.449286241),
     (409,1,0.000016,26087.903141574,0.601783929),
     (410,1,0.000018,-12539.853380183,0.367718999),
     (411,1,0.000017,-7875.671863624,4.075040261),
     (412,1,0.000015,27433.889215875,4.866015153),
     (413,1,0.000015,18606.498946000,1.415997972),
     (414,1,0.000015,16207.886271502,4.894117930),
     (415,1,0.000014,5024.621918366,2.183754522),
     (416,1,0.000014,6915.859589305,0.334976685),
     (417,1,0.000015,5220.512526065,3.049878301),
     (418,1,0.000014,13521.751441591,3.481368100),
     (419,1,0.000015,29822.783236324,5.019746747),
     (420,1,0.000014,526.167846746,3.116928924),
     (421,1,0.000014,6393.282171211,4.946042294),
     (422,1,0.000014,9924.810421511,2.448715141),
     (423,1,0.000013,-6262.720530593,3.977299856),
     (424,1,0.000013,5867.523359379,2.488210286),
     (425,1,0.000013,18451.078546566,4.592665546),
     (426,1,0.000015,4708.230048709,3.296066748),
     (427,1,0.000012,5227.626073065,2.833989822),
     (428,1,0.000014,831.855740750,3.184487666),
     (429,1,0.000016,4797.095728926,5.110364388),
     (430,1,0.000012,29026.485229508,0.645743376),
     (431,1,0.000012,5859.659052853,5.747860983),
     (432,1,0.000013,1272.681025627,0.819177731),
     (433,1,0.000012,4583.796633488,3.056528417),
     (434,1,0.000013,18202.216716659,1.304465953),
     (435,1,0.000012,6709.674040867,0.880008333),
     (436,1,0.000012,6006.040856250,4.120686133),
     (437,1,0.000013,18216.443810661,0.818192899),
     (438,1,0.000011,5853.296265378,0.839621670),
     (439,1,0.000011,45585.172812187,2.624990083),
     (440,1,0.000011,6208.294251424,5.084832666),
     (441,1,0.000011,5219.761766539,1.597258404),
     (442,1,0.000012,-4176.041342449,0.238908106),
     (443,1,0.000011,12416.588502848,1.062658625),
     (444,1,0.000011,-6277.552925684,4.667956246),
     (445,1,0.000011,36949.230808424,2.491061875),
     (446,1,0.000011,15141.390794312,5.269311212),
     (447,1,0.000011,6303.431169390,3.882649384),
     (448,1,0.000012,5226.875313540,2.183475597),
     (449,1,0.000011,5952.456886333,0.816292878),
     (450,1,0.000011,6132.028180148,6.255123490),
     (451,1,0.000013,-5727.086565097,3.072118152),
     (452,1,0.000013,5779.683204697,5.377455743),
     (453,1,0.000010,7330.728427345,2.565313245),
     (454,1,0.000012,77713.771467747,5.198482069),
     (455,1,0.000010,4061.219215394,4.996773378),
     (456,1,0.000010,6066.253636204,4.001364171),
     (457,1,0.000010,-6073.299872902,2.242367760),
     (1,2,4.322990,6283.075849991,2.642893748),
     (2,2,0.406493,0.000000000,4.712388980),
     (3,2,0.122605,12566.151699983,2.438140634),
     (4,2,0.019484,213.299095438,1.642180117),
     (5,2,0.016916,529.690965095,4.510959303),
     (6,2,0.013374,-3.523118349,1.502204005),
     (7,2,0.008042,26.298319800,0.478548285),
     (8,2,0.007824,155.420399434,5.254710405),
     (9,2,0.004894,5746.271337896,4.683210814),
     (10,2,0.004875,5760.498431898,0.759507730),
     (11,2,0.004416,5223.693919802,6.028853291),
     (12,2,0.004433,77713.771467747,3.627709179),
     (13,2,0.003277,18849.227549974,2.327912542),
     (14,2,0.002704,6062.663207553,1.271941712),
     (15,2,0.003435,-775.522611324,0.747446224),
     (16,2,0.002618,6076.890301554,3.633715694),
     (17,2,0.003146,206.185548437,5.647871769),
     (18,2,0.002544,1577.343542448,6.232904269),
     (19,2,0.002218,-220.412642439,1.309511773),
     (20,2,0.002197,5856.477659115,2.407212144),
     (21,2,0.002897,5753.384884897,5.863842227),
     (22,2,0.001766,426.598190876,0.754108971),
     (23,2,0.001738,-796.298006816,2.714942671),
     (24,2,0.001695,522.577418094,2.629369837),
     (25,2,0.001584,5507.553238667,1.341138228),
     (26,2,0.001503,-242.728603974,0.377699736),
     (27,2,0.001552,-536.804512095,2.904684740),
     (28,2,0.001370,-398.149003408,1.265599209),
     (29,2,0.001889,-5573.142801806,4.413514397),
     (30,2,0.001722,6069.776754553,2.445966124),
     (31,2,0.001124,1059.381930189,5.041800035),
     (32,2,0.001068,-7.113547001,0.253140522),
     (33,2,0.001258,553.569402842,3.849557278),
     (34,2,0.000831,951.718406251,2.471094709),
     (35,2,0.000767,4694.002954708,5.363108729),
     (36,2,0.000756,1349.867409659,1.046195744),
     (37,2,0.000775,-11.045700264,0.245548001),
     (38,2,0.000597,2146.165416475,4.543268798),
     (39,2,0.000568,5216.580372801,4.178857347),
     (40,2,0.000711,1748.016413067,5.934905177),
     (41,2,0.000499,12036.460734888,0.624434370),
     (42,2,0.000671,-1194.447010225,4.136047594),
     (43,2,0.000488,5849.364112115,2.209677475),
     (44,2,0.000621,6438.496249426,4.518851278),
     (45,2,0.000495,-6286.598968340,1.868202908),
     (46,2,0.000456,5230.807466803,1.271206960),
     (47,2,0.000451,5088.628839767,0.084060889),
     (48,2,0.000435,5643.178563677,3.324436875),
     (49,2,0.000547,161000.685737301,2.841633382),
     (50,2,0.000387,10977.078804699,4.051897857),
     (51,2,0.000522,3154.687084896,2.171979967),
     (52,2,0.000375,5486.777843175,4.983027306),
     (53,2,0.000421,5863.591206116,4.546413295),
     (54,2,0.000439,7084.896781115,0.522967921),
     (55,2,0.000309,2544.314419883,3.172606219),
     (56,2,0.000347,4690.479836359,1.479586566),
     (57,2,0.000317,801.820931124,3.553088096),
     (58,2,0.000262,419.484643875,0.606655988),
     (59,2,0.000248,6836.645252834,3.013188798),
     (60,2,0.000245,-1592.596013633,5.519526219),
     (61,2,0.000225,4292.330832950,2.877956536),
     (62,2,0.000214,7234.794256242,1.605227587),
     (63,2,0.000205,5767.611978898,0.625783918),
     (64,2,0.000180,10447.387839604,3.499954340),
     (65,2,0.000214,639.897286314,5.960128299),
     (66,2,0.000228,199.072001436,5.631411166),
     (67,2,0.000176,-433.711737877,2.162707788),
     (68,2,0.000208,515.463871093,2.322038317),
     (69,2,0.000173,6040.347246017,2.556187825),
     (70,2,0.000184,6309.374169791,4.732310175),
     (71,2,0.000227,149854.400133688,5.385810830),
     (72,2,0.000154,8031.092263058,5.120720919),
     (73,2,0.000151,5739.157790895,4.814982485),
     (74,2,0.000198,7632.943259650,0.221599784),
     (75,2,0.000197,74.781598567,3.910456770),
     (76,2,0.000138,6055.549660552,1.397491147),
     (77,2,0.000149,-6127.655450557,5.333731518),
     (78,2,0.000137,3894.181829542,4.279742183),
     (79,2,0.000135,9437.762934887,5.979971884),
     (80,2,0.000139,-2352.866153772,4.715630737),
     (81,2,0.000142,6812.766815086,0.513330037),
     (82,2,0.000120,-4705.732307544,0.194160671),
     (83,2,0.000131,-71430.695617756,0.000379688),
     (84,2,0.000124,6279.552731642,2.122264305),
     (85,2,0.000098,12352.852604545,0.521556675),
     (86,2,0.000096,1589.072895284,5.543868453),
     (87,2,0.000093,412.371096874,1.100475527),
     (88,2,0.000088,6496.374945429,1.773637013),
     (89,2,0.000093,1066.495477190,0.846489095),
     (90,2,0.000088,-1990.745017041,0.652153694),
     (91,2,0.000083,-227.526189440,1.283773439),
     (92,2,0.000089,-110.206321219,6.189848614),
     (93,2,0.000108,-6256.777530192,0.883445696),
     (94,2,0.000078,4164.311989613,4.848098908),
     (95,2,0.000071,95.979227218,6.051607364),
     (96,2,0.000069,5636.065016677,3.074021514),
     (97,2,0.000069,8429.241266467,3.780640198),
     (98,2,0.000067,5429.879468239,4.003962252),
     (99,2,0.000083,11506.769769794,3.982598519),
     (100,2,0.000067,11499.656222793,2.738059018),
     (101,2,0.000084,25132.303399966,2.250974026),
     (102,2,0.000059,14143.495242431,2.322133219),
     (103,2,0.000058,6084.003848555,3.486445063),
     (104,2,0.000060,625.670192312,3.016133645),
     (105,2,0.000061,11513.883316794,5.107656524),
     (106,2,0.000056,17789.845619785,4.906611436),
     (107,2,0.000062,1052.268383188,3.038398253),
     (108,2,0.000055,16730.463689596,5.153707862),
     (109,2,0.000057,10213.285546211,3.719636411),
     (110,2,0.000055,12043.574281889,0.663353197),
     (111,2,0.000054,4686.889407707,3.705021427),
     (112,2,0.000054,-7058.598461315,1.058824083),
     (113,2,0.000071,-640.877607382,1.775118056),
     (114,2,0.000050,6290.189396992,2.433880203),
     (115,2,0.000048,11926.254413669,0.026794787),
     (116,2,0.000044,-21.340641002,3.975001794),
     (117,2,0.000062,-88860.057071360,5.199736128),
     (118,2,0.000056,7860.419392439,1.164414752),
     (119,2,0.000043,-7079.373856808,2.720095295),
     (120,2,0.000045,5237.921013804,1.736345370),
     (121,2,0.000041,-543.918059096,3.135280164),
     (122,2,0.000041,5333.900241022,3.215645458),
     (123,2,0.000047,5650.292110678,3.651568663),
     (124,2,0.000041,10973.555686350,0.686406506),
     (125,2,0.000053,3496.032826134,5.679481369),
     (126,2,0.000039,9917.696874510,2.994855304),
     (127,2,0.000044,13367.972631107,0.892020722),
     (128,2,0.000037,735.876513532,3.822966359),
     (129,2,0.000038,309.278322656,3.324897025),
     (130,2,0.000035,5966.683980335,5.859305840),
     (131,2,0.000050,244287.600006854,5.197294990),
     (132,2,0.000035,10177.257679534,3.439403974),
     (133,2,0.000035,5756.566278635,0.227772690),
     (134,2,0.000047,6681.224853400,2.984635301),
     (135,2,0.000047,83286.914269554,3.926273362),
     (136,2,0.000035,5750.203491159,5.222234970),
     (137,2,0.000042,11015.106477335,1.984658046),
     (138,2,0.000033,3097.883822726,0.800021677),
     (139,2,0.000036,4701.116501708,1.220806268),
     (140,2,0.000032,-6525.804453965,0.577869152),
     (141,2,0.000035,16496.361396202,5.277451718),
     (142,2,0.000043,10575.406682942,2.055504892),
     (143,2,0.000039,632.783739313,4.285523621),
     (144,2,0.000029,12359.966151546,3.520607859),
     (145,2,0.000030,-323.505416657,0.798460273),
     (146,2,0.000031,12029.347187887,4.870207120),
     (147,2,0.000035,5547.199336460,2.028365513),
     (148,2,0.000029,6275.962302991,0.563733764),
     (149,2,0.000024,5842.250565114,2.247974031),
     (150,2,0.000025,20426.571092422,0.612732670),
     (151,2,0.000026,-8635.942003763,0.674722356),
     (152,2,0.000025,6066.595360816,1.810322107),
     (153,2,0.000023,6072.958148291,3.078986044),
     (154,2,0.000022,9779.108676125,4.893198950),
     (155,2,0.000022,728.762966531,3.986568330),
     (156,2,0.000025,-5729.506447149,3.926873915),
     (157,2,0.000021,18319.536584880,0.509493306),
     (158,2,0.000021,2699.734819318,2.205606565),
     (159,2,0.000022,10969.965257698,2.086073774),
     (160,2,0.000021,5749.452731634,0.239530851),
     (161,2,0.000028,-90955.551694869,3.513715957),
     (162,2,0.000020,5757.317038160,5.207035971),
     (163,2,0.000023,316.391869657,1.105664271),
     (164,2,0.000020,709.933048185,1.883952143),
     (165,2,0.000026,233141.314403242,1.458142282),
     (166,2,0.000020,24356.780788642,1.904529811),
     (167,2,0.000018,3340.612426700,0.557260651),
     (168,2,0.000020,5326.786694021,1.203980841),
     (169,2,0.000018,25158.601719765,2.217637073),
     (170,2,0.000020,-532.872358832,3.735278311),
     (171,2,0.000018,12345.739057544,1.405128993),
     (172,2,0.000018,5113.487598583,5.166268520),
     (173,2,0.000016,-5331.357443741,2.494867671),
     (174,2,0.000017,5341.013788022,3.170352145),
     (175,2,0.000019,14945.316173554,0.293850403),
     (176,2,0.000015,-4136.910433516,4.659793141),
     (177,2,0.000015,16200.772724501,4.654873437),
     (178,2,0.000016,853.196381752,5.520852305),
     (179,2,0.000015,5209.466825801,4.406914266),
     (180,2,0.000015,5959.570433334,2.847432864),
     (181,2,0.000015,11712.955318231,0.976699094),
     (182,2,0.000015,5127.714692584,1.336734021),
     (183,2,0.000014,1162.474704408,4.069297934),
     (184,2,0.000016,1045.154836188,2.979722841),
     (185,2,0.000014,526.509571357,4.257677699),
     (186,2,0.000014,-7477.522860216,4.402315063),
     (187,2,0.000016,838.969287750,4.835584477),
     (188,2,0.000014,10984.192351700,5.295849210),
     (189,2,0.000014,5657.405657679,1.947410056),
     (190,2,0.000013,956.289155971,4.577910137),
     (191,2,0.000014,3930.209696220,0.274235114),
     (192,2,0.000013,-647.010833315,2.992559602),
     (193,2,0.000013,6065.844601290,3.155427632),
     (194,2,0.000013,6073.708907817,1.731933967),
     (195,2,0.000016,-143571.324283697,4.087022482),
     (196,2,0.000014,11856.218651798,5.074129957),
     (197,2,0.000011,12168.002696575,3.284455690),
     (198,2,0.000011,12491.370101415,2.060547842),
     (199,2,0.000013,-4535.059436924,4.084381789),
     (200,2,0.000010,12528.018664345,2.229891246),
     (201,2,0.000010,17267.268201691,6.184037750),
     (202,2,0.000010,17253.041107690,3.831870236),
     (203,2,0.000011,942.062061969,1.981749092),
     (204,2,0.000013,72140.628665941,0.187265268),
     (205,2,0.000012,5870.704753117,4.788334182),
     (206,2,0.000012,66567.485864134,3.030212489),
     (207,2,0.000010,5120.601145584,0.252888619),
     (1,3,0.143388,6283.075849991,1.131453581),
     (2,3,0.006671,12566.151699983,0.775148888),
     (3,3,0.001480,155.420399434,0.480016796),
     (4,3,0.000935,213.299095438,6.144033518),
     (5,3,0.000795,529.690965095,2.941366733),
     (6,3,0.000672,5760.498431898,5.316996960),
     (7,3,0.000673,5746.271337896,0.120403304),
     (8,3,0.000390,-220.412642439,3.090643574),
     (9,3,0.000373,6062.663207553,3.003539246),
     (10,3,0.000360,6076.890301554,1.918902460),
     (11,3,0.000316,-21.340641002,5.545798121),
     (12,3,0.000315,-242.728603974,1.884932563),
     (13,3,0.000278,206.185548437,1.266242285),
     (14,3,0.000238,-536.804512095,4.532703798),
     (15,3,0.000185,522.577418094,4.578085209),
     (16,3,0.000245,18849.227549974,0.587466537),
     (17,3,0.000180,426.598190876,5.151178542),
     (18,3,0.000200,553.569402842,5.355983739),
     (19,3,0.000141,5223.693919802,1.336556022),
     (20,3,0.000104,5856.477659115,4.239840158),
     (21,3,0.000088,5849.364112115,3.991224194),
     (22,3,0.000087,5216.580372801,5.783881572),
     (23,3,0.000082,1059.381930189,3.758504128),
     (24,3,0.000081,199.072001436,1.087318010),
     (25,3,0.000056,5767.611978898,5.196059854),
     (26,3,0.000057,5230.807466803,5.679479605),
     (27,3,0.000043,5863.591206116,2.662459239),
     (28,3,0.000041,5739.157790895,0.240575243),
     (29,3,0.000043,515.463871093,3.986419702),
     (30,3,0.000038,6055.549660552,3.109850421),
     (31,3,0.000043,5643.178563677,5.201691746),
     (32,3,0.000035,-6286.598968340,3.483584620),
     (33,3,0.000031,4694.002954708,0.632518797),
     (34,3,0.000041,639.897286314,4.242399959),
     (35,3,0.000033,-6127.655450557,0.658527358),
     (36,3,0.000029,-433.711737877,3.989749946),
     (37,3,0.000026,-543.918059096,4.941560932),
     (38,3,0.000022,412.371096874,2.845444402),
     (39,3,0.000018,6438.496249426,0.573761539),
     (40,3,0.000016,6084.003848555,1.779378028),
     (41,3,0.000016,12036.460734888,5.343576252),
     (42,3,0.000018,419.484643875,2.017408155),
     (43,3,0.000015,5636.065016677,4.893563355),
     (44,3,0.000015,1066.495477190,5.488511328),
     (45,3,0.000011,5237.921013804,6.233852651),
     (1,4,0.003826,6283.075849991,5.705257284),
     (2,4,0.000303,12566.151699983,5.407240514),
     (3,4,0.000209,155.420399434,1.989815753),
     (4,4,0.000069,5746.271337896,1.860313071),
     (5,4,0.000069,5760.498431898,3.566193751),
     (6,4,0.000047,-220.412642439,4.810036089),
     (7,4,0.000039,6062.663207553,4.756300047),
     (8,4,0.000037,6076.890301554,0.190608808),
     (9,4,0.000040,213.299095438,4.016006462),
     (10,4,0.000026,-536.804512095,6.151375713),
     (11,4,0.000028,529.690965095,0.309260642),
     (12,4,0.000027,-5573.142801806,2.842870331),
     (13,4,0.000017,522.577418094,0.204784068),
     (14,4,0.000013,5849.364112115,5.769655119),
     (15,4,0.000012,5767.611978898,3.465355174),
     (16,4,0.000011,5216.580372801,1.095641596),
     (1,5,0.000085,6283.075849991,4.338254017));
var
  T: TJulianDate;
  w: array[0..5] of Double;
  i: Integer;
begin
  for i:= 0 to 5 do
    w[i]:= 0;

  T:= (TDB - J2000)/JulianDaysPerMillenium;

  for i:= Pred(nFBSeries) downto 1 do
    if abs(FBSeries[i,3]) >= AmpCut then
      w[Trunc(FBSeries[i,2])]:= w[Trunc(FBSeries[i,2])] + FBSeries[i,3]*sin(T*FBSeries[i,4] + FBSeries[i,5]);

  Result:= T*(T*(T*(T*(T*w[5] + w[4]) + w[3]) + w[2]) + w[1]) + w[0];
  Result:= Result/MicroSecondsPerSecond;
end;


//     dTDB = TDB - TT   in Seconds
function DeltaTDB_HF2002(TDB: TJulianDate): Double;
{
  REFERENCES:


}
//var

begin

end;

//     dTDB = TDB - TT   in Seconds

function DeltaTDB_HF2002_IERS(TDB: TJulianDate): Double;
{
  REFERENCES:
 IERS Conventions (2010). Petit, G. & Luzum, B. ( IERS Technical Note n 36),
   Frankfurt am Main: Verlag des Bundesamts für Kartographie und Geodäsie, 2010
}
const
  C4TERMS = 1.15E-16;
  T0   = 2443144.5003725;  // JD(TCB) on 1977 January 1, 0h TAI
begin

 // TODO: The current implementation returns TCB − TCG

{
 1. The expression is given in IERS Conventions (2010), eq. 10.5
 2. This function is a Pascal version of IERS Conventions (2010) routine 'XHF2002 IERS.F'.
}
  Result:= DeltaTDB_HF2002(TDB)/(1-LB) + C4TERMS*(TDB-T0)*SecondsPerDay;
end;


//    dUT2 = UT2 - UT1   in Seconds
function DeltaUT2(UT1: TJulianDate): Double;
//  REFERENCE: IERS BULLETIN - A, Vol. XVI No. 36 (4 September 2003)
var
 s2, c2, s4, c4, BPi: float;
begin
 BPi:= Pi*JulianDateToBesselianEpoch(UT1);
 SinCos(2*BPi, s2, c2);
 SinCos(4*BPi, s4, c4);
 Result:= 0.022*s2 - 0.012*c2 - 0.006*s4 + 0.007*c4;
end;

//    dUT0 = UT0 - UT1   in Seconds
function DeltaUT0(UT1: TJulianDate): Double;
begin
 Result:= 0;
//  Result:= tan(lat) * (x * sin(long) + y * cos(long))   // TODO
 {where x and y are the pole offsets published in IERS Bulletin A, and lat and long
  are the observatory's nominal station coordinates.  x and y are published in
  arcseconds so they must be converted to time (one time second = 15 arcseconds).}
end;

// conversion functions

//** Offset between JulianDate and Delphi/FPC TDateTime
var
  DateTimeOffset: Double;

const
  MJDOffset  = 2400000.5;

function DateTimeToJulianDate(DateTime: TDateTime):TJulianDate;
begin
  Result:= DateTimeOffset + DateTime;
end;

function JulianDateToDateTime(JulianDate: TJulianDate):TDateTime;
// convert JulianDate to Delphi TDateTime
begin
  Result:= JulianDate - DateTimeOffset;
end;

function JulianDateToMJD(JulianDate: TJulianDate): TMJD;
begin
  Result:= JulianDate - MJDOffset;
end;

function MJDToJulianDate(MJD: TMJD): TJulianDate;
begin
  Result:= MJD + MJDOffset;
end;

function JulianDateToBesselianEpoch(JD: TJulianDate): Double;
// REFERENCE: Lieske, J.H., 1979, Astron.Astrophys. 73, 282
begin
  Result:= 1900.0 + (JD - B1900)/TropicalDaysPerYear;
end;

function BesselianEpochToJulianDate(BesselianEpoch: Double): TJulianDate;
// REFERENCE: Lieske, J.H., 1979, Astron.Astrophys. 73, 282
begin
   Result:= TropicalDaysPerYear*(BesselianEpoch - 1900.0) + B1900;
end;

function JulianDateToJulianEpoch(JD: TJulianDate): Double;
// REFERENCE: Lieske, J.H., 1979, Astron.Astrophys. 73, 282
begin
 Result:= 2000.0 + (JD - J2000)/JulianDaysPerYear;
end;

function JulianEpochToJulianDate(JulianEpoch: Double): TJulianDate;
// REFERENCE: Lieske, J.H., 1979, Astron.Astrophys. 73, 282
begin
 Result:= JulianDaysPerYear*(JulianEpoch - 2000.0) + J2000;
end;

function StandardTimeToUTC(StandardTime: TDateTime; TimeZone: Double;
  DayLightSavings: Double): TDateTime;
begin
 Result:= StandardTime - (DaylightSavings + TimeZone)/HoursPerDay;
end;

function UTCToStandardTime(UTC: TDateTime; TimeZone: Double;
  DayLightSavings: Double): TDateTime;
begin
 Result:= UTC + (DaylightSavings + TimeZone)/HoursPerDay;
end;

function LocalMeanTimeToUniversalTime(LocalMeanTime: TDateTime; Longitude: Double
  ): TDateTime;
begin
 Result:= LocalMeanTime - Longitude/DegreesPerRev;
end;

function UniversalTimeToLocalMeanTime(UniversalTime: TDateTime; Longitude: Double
  ): TDateTime;
begin
 Result:= UniversalTime + Longitude/DegreesPerRev;
end;

initialization

  DateTimeOffset:= JulianDate2000 - EncodeDate(2000,1,1);

end.

