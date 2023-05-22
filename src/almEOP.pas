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

  { TEOPDownload }

  TEOPDownload = class
    private
      fDownloadPath: string;
      function Download(aURL, aFileName: string): string;
    public
      constructor Create(DownloadPath: String = '');
      { Download EOP (IERS) 20 C04 TIME SERIES  consistent with ITRF 2020 - sampled at 0h UTC
      from https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now
      Reference Precession-Nutation Model: IAU 2000
      }
      function DownloadEOPC04: string;
      // TODO: Download leap second data: https://hpiers.obspm.fr/iers/bul/bulc/Leap_Second.dat
      // https://hpiers.obspm.fr/iers/bul/bulc/BULLETINC.GUIDE.html
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
      function Find(const MJD: TMJD; out Index: Integer): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function IndexOf(const MJD: TMJD): Integer;
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
   - DUT1 (UT1-UTC): Coordinated Universal Time (UTC) is the standard atomic based time scale in normal everyday use
  throughout the world. It is defined by the International Radio Consultative Committee (CCIR) Recommendation
  460-4 (CCIR, 1986) to differ from International Atomic Time (TAI) by an integral number of seconds in such a
  way that UT1-UTC remains smaller than 0.9s in absolute value.
   - dX and dY: Precession-Nutation is referred to the CIP and exhibits, by definition, only motions
   with periods greater than two days with respect to an inertial observer in space. The IERS determines
   observational residuals with respect to the precession and nutation models, called celestial pole offsets.
   The celestial pole offsets values can be represented in two forms. The first, X and Y refers
   to use with the model IAU 2006/2000A (Capitaine et al., 2009). The second refers to use with
   the classical nutation angles in longitude and obliquity (δ∆ε, and δ∆ψ).
   LOD: The difference between the astronomically determined duration of the mean solar day (D) and
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
    public
      constructor Create(aDownloadPath: string = ''; aAutoDownload: Boolean = False);
      destructor Destroy; override;
      procedure LoadEOPData(aEOPData: TEOPData);
      function Download: boolean;
      function GetEOP(UTC: TMJD; out DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double): Boolean;
      property AutoDownload: Boolean read FAutoDownload write FAutoDownload;
      property MinDate: TMJD read fMinDate;
      property MaxDate: TMJD read fMaxDate;
    end;


implementation

uses   fphttpclient, openssl, opensslsockets, Math;

function CompareEOPItem(Item1, Item2: Pointer): Integer;
begin
  Result:= CompareValue(TEOPItem(Item1).MJD,TEOPItem(Item2).MJD);
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

function TEOPData.IndexOf(const MJD: TMJD): Integer;
begin
  if not Find(MJD,Result) then
    Result:= -1;
end;

procedure TEOPData.Sort;
begin
  fList.Sort(@CompareEOPItem);
end;

function TEOPData.Find(const MJD: TMJD; out Index: Integer): Boolean;
var
  L, R, I: Integer;
  CompareRes: PtrInt;
begin
  Result:= False;
  Index:= -1;
  if not Sorted then
    Sort;
  // Use binary search.
  L:= 0;
  R:= Count - 1;
  while (L <= R) do
    begin
      I:= L + (R - L) div 2;
      CompareRes:= CompareValue(MJD,TEOPItem(fList[I]).MJD);
      if (CompareRes>0) then
        L:= I + 1
      else
        begin
          R:= I - 1;
          if (CompareRes=0) then
            begin
               Result:= True;
               L:= I; // forces end of while loop
            end;
        end;
    end;
  Index:= L;
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
const
  URL = 'http://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now';
  FileName = 'eopc04.1962-now';
//  URL = 'https://datacenter.iers.org/data/234/eopc04_20.1962-now.txt';
begin
  Result:= Download(URL, FileName);
end;


{ TEOP }

constructor TEOP.Create(aDownloadPath: string; aAutoDownload: Boolean);
begin
  fMaxDate:= 0;
  fMinDate:= 0;
  fEOPDownload:= TEOPDownload.Create(aDownloadPath);
  AutoDownload:= aAutoDownload;
  fFileLoaded:= False;
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
begin
  Result:= False;
  if (not fFileLoaded) and AutoDownload then
    Download;
  if fFileLoaded then
    begin
      id:= fDB.IndexOf(UTC);
      if id >= 0 then
        begin
          DUT1:= fDB[id].DUT1;
          Xp:= fDB[id].Xp;
          Yp:= fDB[id].Yp;
          LOD:= fDB[id].LOD;
          dX:= fDB[id].dX;
          dY:= fDB[id].dY;
          xrt:= fDB[id].xrt;
          yrt:= fDB[id].yrt;
          Result:= True;
        end;
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
