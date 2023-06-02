{
    almEOP is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2010, 2023 João Marcelo S. Vaz

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

// This unit has the routines to get the Earth Orientation Parameters (EOP)

unit almEOP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, almBase;

type

{

 - EOP(IERS) C01 is a series of the Earth Orientation Parameters given at 0.1
year interval (1846 - 1889) and 0.05 year over the interval 1890 to now.
This series is the basis of the IERS system for long-term studies. Nethertheless
it is updated regularly once per month, and encompasses the last 0.05 year interval.
http://hpiers.obspm.fr/eoppc/eop/eopc01/eopc01.iau2000.1846-now

 - EOP(IERS) 20 C04 is the current series of Earth orientation parameters smoothed values at
1-day intervals) with respect to IAU 2006/2000A precession-nutation model and consistent
with ITRF2020. EOP 20 C04 is updated daily.
https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now
https://datacenter.iers.org/data/234/eopc04_20.1962-now.txt

 - EOP(IERS) 14 C04 is series of Earth orientation parameters smoothed values at 1-day
intervals) with respect to IAU 2006/2000A precession-nutation model and consistent
with ITRF2014. EOP 14 C04 is updated two times per week.
https://hpiers.obspm.fr/iers/eop/eopc04_14/eopc04_IAU2000.62-now
https://datacenter.iers.org/data/224/eopc04_14_IAU2000.62-now.txt

 - Standard Rapid EOP Data since 02. January 1973 (IAU2000)
Quick-look weekly estimates of the EOP determined by combining the most recently available
observed and modeled data (including VLBI 24-hour and intensive, GPS, and AAM).
The combination process involves applying systematic corrections and slightly smoothing,
in order to remove the high frequency noise.
finals2000A.all contains the values from IERS Bulletin A for x/y pole, UT1-UTC, LOD,
dX, dY, their errors and predictions for next 365 days for x/y pole, UT1-UTC, dX and dY
as well as the values from IERS Bulletin B for x/y pole, UT1-UTC, dX, dY at daily
intervals since 02. January 1973. Celestial pole offsets (dX, dY) are related to
the IAU2000A precession/nutation theory.
https://maia.usno.navy.mil/ser7/finals2000A.all


TODO: Leap second data: https://hpiers.obspm.fr/iers/bul/bulc/BULLETINC.GUIDE.html
https://hpiers.obspm.fr/iers/bul/bulc/Leap_Second.dat
https://maia.usno.navy.mil/ser7/tai-utc.dat


}

  { TEOPDownload }

  TEOPDownload = class
    private
      fDownloadPath: string;
      function Download(aURL, aFileName: string): string;
    public
      constructor Create(DownloadPath: String = '');
      { Download the current EOP (IERS) C04 TIME SERIES (currently EOP 20 C04 - ITRF 2020)
      }
      function DownloadEOPC04: string;
      { Download the EOP (IERS) 20 C04 TIME SERIES (consistent with ITRF 2020 - sampled at 0h UTC)
      from https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now
      }
      function DownloadEOP20C04: string;
      { Download the EOP (IERS) 14 C04 TIME SERIES (consistent with ITRF 2014 - sampled at 0h UTC)
      from https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now
      }
      function DownloadEOP14C04: string;
      { Download the EOP (IERS) C01 TIME SERIES
      from http://hpiers.obspm.fr/eoppc/eop/eopc01/eopc01.iau2000.1846-now
      }
      function DownloadEOPC01: string;
      { Download the Standard Rapid EOP Data since 02. January 1973 (IAU2000)
      from https://maia.usno.navy.mil/ser7/finals2000A.all
      }
      function DownloadEOPFinals2000A: string;
    end;

  { TEOPItem }

  TEOPItem = class
    private
      fDUT1: Double;
      fdX: Double;
      fdY: Double;
      fLOD: Double;
      fMJD: TMJD;
      fXp: Double;
      fxrt: Double;
      fYp: Double;
      fyrt: Double;
    public
      constructor Create(aMJD: TMJD; aXp, aYp, aDUT1, adX, adY,axrt, ayrt, aLOD: Double);
      property MJD: TMJD read fMJD;
      property Xp: Double read fXp;
      property Yp: Double read fYp;
      property DUT1: Double read fDUT1;
      property dX: Double read fdX;
      property dY: Double read fdY;
      property xrt: Double read fxrt;
      property yrt: Double read fyrt;
      property LOD: Double read fLOD;
    end;


  { TEOPData }

  TEOPData = class
    private
      fList: TList;
      Sorted: Boolean;
      function GetCount: Integer;
      function GetItem(Index: Integer): TEOPItem;
      function GetMaxDate: TMJD;
      function GetMinDate: TMJD;
      procedure Sort;
    public
      constructor Create;
      destructor Destroy; override;
      function Find(const MJD: TMJD; out Index: Integer): Boolean;
      function Add(aEOPItem: TEOPItem): Integer;
      procedure Clear;
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TEOPItem read GetItem; default;
      property MinDate: TMJD read GetMinDate;
      property MaxDate: TMJD read GetMaxDate;
  end;

  { TEOPReader }

  TEOPReader = class
    public
      { Read EOP (IERS) 20 C04 TIME SERIES  consistent with ITRF 2020 - sampled at 0h UTC
        downloaded from https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now
       Reference Precession-Nutation Model: IAU 2000
      }
      class function ReadEOPC04File(FileName: String): TEOPData;
    end;


  { TEOP  is a class that gets the Earth Orientation Parameters
  The Earth Orientation Parameters may be used to relate the International Celestial and
  Terrestrial Reference Systems as realized operationally by the International and Celestial Reference Frames
  respectively. The rigorous details are outlined in the publications of the International Earth Rotation and
  Reference Systems Service (IERS).
   - x and y: Polar motion refers to the motion of the Celestial Intermediate Pole (CIP) in the International Terrestrial
  Reference System (ITRS). It is described, in practice, by two angular coordinates with respect to an origin at the
  pole of the ITRF: x, along the meridian of 0° longitude and y along the meridian of 90° west longitude. The data
  are derived from astro-geodetic observations using models including high-frequency variations.
   - ∆UT1 (UT1-UTC): Coordinated Universal Time (UTC) is the standard atomic based time scale in normal everyday use
  throughout the world. It is defined by the International Radio Consultative Committee (CCIR) Recommendation
  460-4 (CCIR, 1986) to differ from International Atomic Time (TAI) by an integral number of seconds in such a
  way that UT1-UTC remains smaller than 0.9s in absolute value.
   - dX and dY: Precession-Nutation is referred to the CIP and exhibits, by definition, only motions
  with periods greater than two days with respect to an inertial observer in space. The IERS determines
  observational residuals with respect to the precession and nutation models, called celestial pole offsets.
  The celestial pole offsets values can be represented in two forms. The first, δX and δY refers
  to use with the model IAU 2006/2000A (Capitaine et al., 2009). The second refers to use with
  the classical nutation angles in longitude and obliquity (δ∆ε, and δ∆ψ).
   - LOD: The difference between the astronomically determined duration of the mean solar day (D) and
  86400s of TAI, is called the excess of the length of day (LOD).
  }
  TEOP = class
    private
      FAutoDownload: Boolean;
      fEOPDownload: TEOPDownload;
      fFileLoaded: Boolean;
      fDB: TEOPData;
      fMaxDate: TMJD;
      fMinDate: TMJD;
    private
      fTolerance: Double;
      function Interpolate4(const MJD: TMJD; const Index: Integer): TEOPItem;
    public
      constructor Create(aDownloadPath: string = ''; aAutoDownload: Boolean = False);
      destructor Destroy; override;
      procedure LoadEOPData(aEOPData: TEOPData);
      function Download: boolean;
      { Get Earth Orientation Parameters values, interpolated with a 4 points Lagrangian
        interpolation scheme as recommended by 1997-01-30 IERS Gazette n. 13
      }
      function GetEOP(UTC: TMJD; out DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double): Boolean;
      property AutoDownload: Boolean read FAutoDownload write FAutoDownload;
      property MinDate: TMJD read fMinDate;
      property MaxDate: TMJD read fMaxDate;
      // How many days after MaxDate the values are valid
      property Tolerance: Double read fTolerance write fTolerance;
    end;


implementation

uses   fphttpclient, openssl, opensslsockets, Math;

function CompareEOPItem(Item1, Item2: Pointer): Integer;
begin
  Result:= CompareValue(TEOPItem(Item1).MJD,TEOPItem(Item2).MJD);
end;


{
Interpolate performs a Lagrangian interpolation within a set of nPoints (X,Y) pairs.
}
function Interpolate(x: Double; nPoints: Integer; XArray, YArray: array of Double): Double;
var
  i, j: Integer;
  Term: Double;
begin
  Result:= 0;
  if nPoints < 2 then
    raise Exception.Create('Interpolate needs at least 2 points!');

  for i:= 0 to nPoints - 1 do
    begin
      term:= YArray[i];
      for j:= 0 to nPoints - 1 do
        if i <> j then
          term:= term * (X - XArray[j])/(XArray[i] - XArray[j]);
      Result:= Result + term;
    end;
end;

constructor TEOPData.Create;
begin
  fList:= TList.Create;
  Sorted:= False;
end;

destructor TEOPData.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  inherited Destroy;
end;

function TEOPData.GetCount: Integer;
begin
  Result:= fList.Count;
end;

function TEOPData.GetItem(Index: Integer): TEOPItem;
begin
  Result:= TEOPItem(fList[Index]);
end;

function TEOPData.GetMaxDate: TMJD;
begin
  Result:= 0;
  if Count > 0 then
    Result:= Items[Count-1].MJD;
end;

function TEOPData.GetMinDate: TMJD;
begin
  Result:= 0;
  if Count > 0 then
    Result:= Items[0].MJD;
end;

procedure TEOPData.Sort;
begin
  fList.Sort(@CompareEOPItem);
end;

function TEOPData.Find(const MJD: TMJD; out Index: Integer): Boolean;
// Does a binary search and returns the index of the previous value.
// If the search value exists it returns its index
var
  L, R, I: Integer;
begin
  Index:= -1;
  Result:= False;

  if not Sorted then
    Sort;

  // Edge cases
  if (MJD < MinDate) or (MJD > MaxDate) then
    Exit(False);

  // Binary search
  L:= 0;
  R:= Count - 1;
  while (L <= R) do
    begin
      I:= L + (R - L) div 2;
      case  CompareValue(MJD,TEOPItem(fList[I]).MJD) of
        EqualsValue:
          begin
            Index:= I;
            Exit(True);
          end;
        GreaterThanValue:
          L:= I + 1;
        LessThanValue:
          R:= I - 1;
      end;
    end;
  Index:= R;
  Result:= True;
end;

function TEOPData.Add(aEOPItem: TEOPItem): Integer;
begin
  Result:= fList.Add(aEOPItem);
  Sorted:= False;
end;

procedure TEOPData.Clear;
begin
  if Count = 0 then Exit;
  fList.Clear;
  Sorted:= False;
end;

{ TEOPLoader }

class function TEOPReader.ReadEOPC04File(FileName: String): TEOPData;
var
  aRow: TStringList;
  InputFile: TextFile;
  InputStr: String;
  aEOPItem: TEOPItem;
begin
{
From https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now:
# EARTH ORIENTATION PARAMETER (EOP) PRODUCT CENTER CENTER (PARIS OBSERVATORY) - INTERNATIONAL EARTH ROTATION AND REFERENCE SYSTEMS SERVICE
# EOP (IERS) 20 C04 TIME SERIES  consistent with ITRF 2020 - sampled at 0h UTC
# Description: https://hpiers.obspm.fr/eoppc/eop/eopc04/eopc04.txt        Contact: christian.bizouard@obspm.fr
# Reference Precession-Nutation Model: IAU 2000
# format(4(i4),f10.2,2(f12.6),f12.7,2(f12.6),2(f12.6),f12.7,2(f12.6),f12.7,2(f12.6),2(f12.6),f12.7)
# YR  MM  DD  HH       MJD        x(")        y(")  UT1-UTC(s)     dPsi(")     dEps(")      xrt(")      yrt(")      LOD(s)        x Er        y Er  UT1-UTC Er     dPsi Er     dEps Er      xrt Er      yrt Er      LOD Er
}
  Result:= TEOPData.Create;
  try
    if FileExists(FileName) then
      begin
        AssignFile(InputFile, FileName);
        {$I-}
        try
          Reset(InputFile);
          repeat
            Readln(InputFile, InputStr);
            if InputStr[1] = '#' then
              Continue;
            aRow:= TStringList.Create;
            try
              aRow.DelimitedText:= InputStr;
              aRow.Delimiter:= ' ';
              if aRow.Count = 21 then
                begin
                  aEOPItem:= TEOPItem.Create(StrToFloat(aRow[4]),StrToFloat(aRow[5]),StrToFloat(aRow[6]),
                             StrToFloat(aRow[7]),StrToFloat(aRow[8]),StrToFloat(aRow[9]),
                             StrToFloat(aRow[10]),StrToFloat(aRow[11]),StrToFloat(aRow[12]));
                  Result.Add(aEOPItem);
                end
              else
                raise Exception.Create('Incorrect number of fields.');
            finally
              FreeAndNil(aRow);
            end;
          until(EOF(InputFile));
        finally
          {$I+}
          CloseFile(InputFile);
        end;
      end;
  except
    Result.Clear;
  end;
end;

{ TEOPItem }

constructor TEOPItem.Create(aMJD: TMJD; aXp, aYp, aDUT1, adX, adY, axrt, ayrt,
  aLOD: Double);
begin
  fMJD:= aMJD;
  fXp:= aXp;
  fYp:= aYp;
  fDUT1:= aDUT1;
  fdX:= adX;
  fdY:= adY;
  fxrt:= axrt;
  fyrt:= ayrt;
  fLOD:= aLOD;
end;

{ TEOPDownload }

function TEOPDownload.Download(aURL, aFileName: string): string;
var
  Client: TFPHttpClient;
  FS: TStream;
  FullFileName: string;
begin
  Result:= EmptyStr;
  if DirectoryExists(fDownloadPath) then
    begin
      FullFileName:= fDownloadPath + aFileName;
      Client:= TFPHttpClient.Create(nil);
      FS:= TFileStream.Create(FullFileName,fmCreate or fmOpenWrite);
      try
        Client.AllowRedirect:= True;
        Client.Get(aURL,FS);
        Result:= FullFileName;
      finally
        FS.Free;
        Client.Free;
      end;
    end;
end;

constructor TEOPDownload.Create(DownloadPath: String);
begin
  InitSSLInterface;
  if DownloadPath = EmptyStr then
    fDownloadPath:= GetTempDir(False)
  else
    fDownloadPath:= DownloadPath;
  if not DirectoryExists(fDownloadPath) then
    CreateDir(fDownloadPath);
  fDownloadPath:= IncludeTrailingPathDelimiter(fDownloadPath);
end;

function TEOPDownload.DownloadEOPC04: string;
begin
  Result:= DownloadEOP20C04;
end;

function TEOPDownload.DownloadEOP20C04: string;
const
  URL = 'http://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now';
  FileName = 'eopc04.1962-now';
begin
  Result:= Download(URL, FileName);
end;

function TEOPDownload.DownloadEOP14C04: string;
const
  URL = 'https://hpiers.obspm.fr/iers/eop/eopc04_14/eopc04_IAU2000.62-now.txt';
  FileName = 'eopc01.iau2000.1846-now';
begin
  Result:= Download(URL, FileName);
end;

function TEOPDownload.DownloadEOPC01: string;
const
  URL = 'https://hpiers.obspm.fr/iers/eop/eopc04_14/eopc04_IAU2000.62-now';
  FileName = 'eopc04_IAU2000.62-now';
begin
  Result:= Download(URL, FileName);
end;

function TEOPDownload.DownloadEOPFinals2000A: string;
const
  URL = 'https://maia.usno.navy.mil/ser7/finals2000A.all';
  FileName = 'finals2000A.all';
begin
  Result:= Download(URL, FileName);
end;


{ TEOP }

function TEOP.Interpolate4(const MJD: TMJD; const Index: Integer): TEOPItem;
type
  DoubleArray = array of Double;
var
  i1, i2, i3, i4: Integer;
  Xp, Yp, DUT1, dX, dY, xrt, yrt, LOD: Double;
  XArray, YArray: DoubleArray;
begin
  Result:= nil;
  if (Index < 0) or (Index > fDB.Count - 2) then Exit;

  XArray:= DoubleArray.Create(0,0,0,0);
  YArray:= DoubleArray.Create(0,0,0,0);

  if Index = 0 then
    begin
      i1:= Index;
      i2:= Index + 1;
      i3:= Index + 2;
      i4:= Index + 3;
    end
  else
    if Index = (fDB.Count - 2) then
      begin
        i1:= Index - 2;
        i2:= Index - 1;
        i3:= Index;
        i4:= Index + 1;
      end
    else
      begin
        i1:= Index - 1;
        i2:= Index;
        i3:= Index + 1;
        i4:= Index + 2;
      end;

  XArray[0]:= fDB[i1].MJD;
  XArray[1]:= fDB[i2].MJD;
  XArray[2]:= fDB[i3].MJD;
  XArray[3]:= fDB[i4].MJD;

  YArray[0]:= fDB[i1].Xp;
  YArray[1]:= fDB[i2].Xp;
  YArray[2]:= fDB[i3].Xp;
  YArray[3]:= fDB[i4].Xp;
  Xp:= Interpolate(MJD,4,XArray,YArray);

  YArray[0]:= fDB[i1].Yp;
  YArray[1]:= fDB[i2].Yp;
  YArray[2]:= fDB[i3].Yp;
  YArray[3]:= fDB[i4].Yp;
  Yp:= Interpolate(MJD,4,XArray,YArray);

  YArray[0]:= fDB[i1].DUT1;
  YArray[1]:= fDB[i2].DUT1;
  YArray[2]:= fDB[i3].DUT1;
  YArray[3]:= fDB[i4].DUT1;
  DUT1:= Interpolate(MJD,4,XArray,YArray);

  YArray[0]:= fDB[i1].dX;
  YArray[1]:= fDB[i2].dX;
  YArray[2]:= fDB[i3].dX;
  YArray[3]:= fDB[i4].dX;
  dX:= Interpolate(MJD,4,XArray,YArray);

  YArray[0]:= fDB[i1].dY;
  YArray[1]:= fDB[i2].dY;
  YArray[2]:= fDB[i3].dY;
  YArray[3]:= fDB[i4].dY;
  dY:= Interpolate(MJD,4,XArray,YArray);

  YArray[0]:= fDB[i1].xrt;
  YArray[1]:= fDB[i2].xrt;
  YArray[2]:= fDB[i3].xrt;
  YArray[3]:= fDB[i4].xrt;
  xrt:= Interpolate(MJD,4,XArray,YArray);

  YArray[0]:= fDB[i1].yrt;
  YArray[1]:= fDB[i2].yrt;
  YArray[2]:= fDB[i3].yrt;
  YArray[3]:= fDB[i4].yrt;
  yrt:= Interpolate(MJD,4,XArray,YArray);

  YArray[0]:= fDB[i1].LOD;
  YArray[1]:= fDB[i2].LOD;
  YArray[2]:= fDB[i3].LOD;
  YArray[3]:= fDB[i4].LOD;
  LOD:= Interpolate(MJD,4,XArray,YArray);

  Result:= TEOPItem.Create(MJD,Xp, Yp, DUT1, dX, dY, xrt, yrt, LOD);

  SetLength(XArray,0);
  SetLength(YArray,0);
end;

constructor TEOP.Create(aDownloadPath: string; aAutoDownload: Boolean);
begin
  fMaxDate:= 0;
  fMinDate:= 0;
  fEOPDownload:= TEOPDownload.Create(aDownloadPath);
  AutoDownload:= aAutoDownload;
  fFileLoaded:= False;
  Tolerance:= 1;
end;

destructor TEOP.Destroy;
begin
  if Assigned(fDB) then
    FreeAndNil(fDB);
  FreeAndNil(fEOPDownload);
  inherited Destroy;
end;

function TEOP.GetEOP(UTC: TMJD; out DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double ): Boolean;
var
  id: Integer;
  EOPItem: TEOPItem;
begin
  Result:= False;
  if (not fFileLoaded) and AutoDownload then
    Download;
  if fFileLoaded then
    begin
      if ((UTC >= fDB.MaxDate) and ((UTC - fDB.MaxDate) <= Tolerance)) then
        begin
          id:= fDB.Count - 1;
          DUT1:= fDB[id].DUT1;
          Xp:= fDB[id].Xp;
          Yp:= fDB[id].Yp;
          LOD:= fDB[id].LOD;
          dX:= fDB[id].dX;
          dY:= fDB[id].dY;
          xrt:= fDB[id].xrt;
          yrt:= fDB[id].yrt;
          Result:= True;
        end
      else
        if fDB.Find(UTC,id) then
          begin
            EOPItem:= Interpolate4(UTC, id);
            try
              DUT1:= EOPItem.DUT1;
              Xp:= EOPItem.Xp;
              Yp:= EOPItem.Yp;
              LOD:= EOPItem.LOD;
              dX:= EOPItem.dX;
              dY:= EOPItem.dY;
              xrt:= EOPItem.xrt;
              yrt:= EOPItem.yrt;
              Result:= True;
            finally
              FreeAndNil(EOPItem);
            end;
          end
    end;
end;

function TEOP.Download: boolean;
var
  FileName: string;
begin
  FileName:= fEOPDownload.DownloadEOPC04;
  if FileExists(FileName) then
    LoadEOPData(TEOPReader.ReadEOPC04File(FileName));
  Result:= fFileLoaded;
end;

procedure TEOP.LoadEOPData(aEOPData: TEOPData);
begin
  fFileLoaded:= False;
  if Assigned(fDB) then
    FreeAndNil(fDB);
  if aEOPData.Count > 0 then
    try
      fDB:= aEOPData;
      fMinDate:= fDB.MinDate;
      fMaxDate:= fDB.MaxDate;
      fFileLoaded:= True;
    except;
      if Assigned(fDB) then
        FreeAndNil(fDB);
    end;
end;

end.
