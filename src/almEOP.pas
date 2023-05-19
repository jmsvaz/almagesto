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
      function Download: string;
    end;

  { TEOP  is a class that gets the Earth Orientation Parameters from a file
  http://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now
  Description: https://hpiers.obspm.fr/eoppc/eop/eopc04/eopc04.txt
  }

  TEOP = class
    private
      FAutoDownload: Boolean;
      fEOPDownload: TEOPDownload;
      fFileLoaded: Boolean;
      fDB: TStringList;
      fMaxDate: TMJD;
      fMinDate: TMJD;
    private
      procedure ProcessEOPData(aEOPData: TStringList);
    public
      constructor Create(aAutoDownload: Boolean = False);
      destructor Destroy; override;
      function LoadEOPC04File(FileName: String): Boolean;
      function GetEOP(UTC: TMJD; out DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double): Boolean;
      function Download: boolean;
      property AutoDownload: Boolean read FAutoDownload write FAutoDownload;
      property MinDate: TMJD read fMinDate;
      property MaxDate: TMJD read fMaxDate;
    end;


implementation

uses   fphttpclient, openssl, opensslsockets;

type

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

  { TEOPLoader }

  TEOPLoader = class
    public
      class function LoadEOPC04File(FileName: String): TStringList;
    end;

{ TEOPLoader }

class function TEOPLoader.LoadEOPC04File(FileName: String): TStringList;
var
  aRow: TStringList;
  InputFile: TextFile;
  InputStr: String;
  aEOPItem: TEOPItem;
begin
  Result:= TStringList.Create;
  try
    Result.OwnsObjects:= True;;
    Result.Sorted:= True;
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
                  Result.AddObject(IntToStr(Round(StrToFloat(aRow[4]))),aEOPItem);
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

function TEOPDownload.Download: string;
const
  URL = 'http://hpiers.obspm.fr/iers/eop/eopc04/eopc04.1962-now';
  FileName = 'eopc04.1962-now';
begin
  Result:= Download(URL, FileName);
end;


{ TEOP }

constructor TEOP.Create(aAutoDownload: Boolean);
begin
  fMaxDate:= 0;
  fMinDate:= 0;
  fDB:= TStringList.Create;
  fDB.OwnsObjects:= True;;
  fDB.Sorted:= True;
  fEOPDownload:= TEOPDownload.Create(EmptyStr);
  AutoDownload:= aAutoDownload;
  fFileLoaded:= False;
end;

destructor TEOP.Destroy;
begin
  FreeAndNil(fDB);
  FreeAndNil(fEOPDownload);
  inherited Destroy;
end;

function TEOP.GetEOP(UTC: TMJD; out DUT1, Xp, Yp, LOD, dX, dY, xrt, yrt: Double ): Boolean;
var
  id: Integer;
  MJD: string;
begin
  Result:= False;
  MJD:= IntToStr(Round(UTC));
  if (not fFileLoaded) and AutoDownload then
    Download;
  if fFileLoaded then
    begin
      id:= fDB.IndexOf(MJD);
      if id >= 0 then
        begin
          DUT1:= TEOPItem(fDB.Objects[id]).DUT1;
          Xp:= TEOPItem(fDB.Objects[id]).Xp;
          Yp:= TEOPItem(fDB.Objects[id]).Yp;
          LOD:= TEOPItem(fDB.Objects[id]).LOD;
          dX:= TEOPItem(fDB.Objects[id]).dX;
          dY:= TEOPItem(fDB.Objects[id]).dY;
          xrt:= TEOPItem(fDB.Objects[id]).xrt;
          yrt:= TEOPItem(fDB.Objects[id]).yrt;
        end;
    end;
end;

function TEOP.Download: boolean;
begin
  Result:= LoadEOPC04File(fEOPDownload.Download);
end;

function TEOP.LoadEOPC04File(FileName: String): Boolean;
begin
  if FileExists(FileName) then
    ProcessEOPData(TEOPLoader.LoadEOPC04File(FileName));
  Result:= fFileLoaded;
end;

procedure TEOP.ProcessEOPData(aEOPData: TStringList);
begin
  fFileLoaded:= False;
  fDB.Clear;
  fMaxDate:= 0;
  fMinDate:= 0;
  fDB.Assign(aEOPData);
  if fDB.Count > 0 then
    begin
      fMinDate:= StrToFloat(fDB[0]);
      fMaxDate:= StrToFloat(fDB[fDB.Count-1]);
      fFileLoaded:= True;
    end;
end;


end.
