{
    almEPMModel is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2011 João Marcelo S. Vaz

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

// This unit has the EPM Model classes
unit almEPMModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, almBase, almEphemerides ;

type

  { TEPMModel }

  TEPMModel = class(TEphemeridesModel)
    private
      jdb: array of Integer;      // initial Julian date (int part)
      djb: array of Double;       // initial Julian date (frac part)
      delta: array of Double;     // approximation subinterval
      ncoef: array of Integer;    // number of (polynomial degrees + free term)
      ndim: array of Integer;     // number of dimensions
      nbl: array of Integer;
      coef: array of array of Double;
    protected
      fObjects: Integer;
      fRHO: Double;
      fDisp: Double;
      fFileNames: array of string;
      fTextFileExtension: string;
      procedure InitModel; virtual; abstract;
      procedure ReadText(ObjectNumber: Integer;FileName: string);
      procedure ReadBin(ObjectNumber: Integer;FileName: string);
    public
      constructor Create;
      function HasBody(ABody: TSolarSystemBody): Boolean; override;
      function GetState(ABody: TSolarSystemBody; TDB: Double): TBodyState; override;
      procedure SetTextPath(APath: string);
  end;

  { TEPM04Model }

  TEPM04Model = class(TEPMModel)
    protected
      procedure InitModel; override;
  end;

  { TEPM08Model }

  TEPM08Model = class(TEPMModel)
    protected
      procedure InitModel; override;
  end;

implementation

const

  EPM04_Name = 'EPM04';
  EPM04_RHO = 82.30056;  // Earth/Moon + 1
  EPM04_DISP = -1.24689559838047e-07; //TT-TDB correction
  EPM04_Objects = 11;
  EPM04_FileNames: array[1..EPM04_Objects] of string =
                   ('mercury','venus','earth_m','mars','jupiter','saturn',
                    'uranus','neptune','pluto', 'moon', 'sun');
  EPM04_TextFileExtension = '.04t';

  EPM08_Name = 'EPM08';
  EPM08_RHO = 82.3005676536174207;  // Earth/Moon + 1
  EPM08_DISP = -1.24840002893566e-07; //TT-TDB correction
  EPM08_Objects = 19;
  EPM08_FileNames: array[1..EPM08_Objects] of string =
                   ('mercury','venus','earth_m','mars','jupiter','saturn',
                    'uranus','neptune','pluto','moon','sun','ceres','pallas',
                    'vesta','eris','haumea','makemake','sedna','tdb');
  EPM08_TextFileExtension = '.08t';




type
  TChebValues = array of double;


{This function evaluates the chebyshev polynomial at X, between MinX and MaxX
}
function ChebEval(coef: TChebValues; coef_n: Integer; x: Double; MinX: Double = -1;
                MaxX: Double = 1): Double;
var
  d,dd,sv,y,y2: Double;
  i: Integer;
begin
  if ((x-MinX)*(x-MaxX)) > 0 then exit; // out of range
  d:= 0;
  dd:= 0;
  y:= (2*x-MinX-MaxX)/(MaxX-MinX);
  y2:= 2*y;
  for i:= Pred(coef_n) downto 1 do
    begin
      sv:= d;
      d:= y2*d-dd+coef[i];
      dd:= sv;
    end;
  Result:= y*d-dd+coef[0]/2;
end;

{This function returns the array of chebyshev coefficients of the derivatives of
 the function whose coefficients are coef
}
function ChebCoef_Deriv(coef: TChebValues; coef_n: Integer; MinX: Double = -1;
                MaxX: Double = 1): TChebValues;
var
  con: Double;
  i: Integer;
begin
  con:= 2/(MaxX-MinX);
  SetLength(Result,coef_n);
  Result[coef_n-1]:= 0;
  Result[coef_n-2]:= 2*(coef_n-1)*coef[coef_n-1];
  for i:= (coef_n-3) downto 0 do
    Result[i]:= Result[i+2] + 2*(i+1)*(coef[i+1]);
  for i:= 0 to Pred(coef_n) do
    Result[i]:= con*Result[i];
end;


{ TEPM04Model }

procedure TEPM04Model.InitModel;
begin
  fName:= EPM04_Name;
  fObjects:= EPM04_Objects;
  fRHO:= EPM04_RHO;
  fDisp:= EPM04_Disp;
  SetLength(fFileNames, Length(EPM04_FileNames));
  Move(EPM04_FileNames[Low(EPM04_FileNames)], fFileNames[0], Length(EPM04_FileNames));
  fTextFileExtension:= EPM04_TextFileExtension;
end;

{ TEPM08Model }

procedure TEPM08Model.InitModel;
var
  i: Integer;
begin
  fName:= EPM08_Name;
  fObjects:= EPM08_Objects;
  fRHO:= EPM08_RHO;
  fDisp:= EPM08_Disp;
  SetLength(fFileNames, Length(EPM08_FileNames));
  for i:= 0 to Pred(fObjects) do
    fFileNames[i]:= EPM08_FileNames[i];
//  Move(EPM08_FileNames[Low(EPM08_FileNames)], fFileNames[0], Length(EPM08_FileNames));
  fTextFileExtension:= EPM08_TextFileExtension;
end;

{ TEPMModel }

constructor TEPMModel.Create;
begin
  InitModel;
  SetLength(jdb,fObjects);
  SetLength(djb,fObjects);
  SetLength(delta,fObjects);
  SetLength(ncoef,fObjects);
  SetLength(ndim,fObjects);
  SetLength(nbl,fObjects);
  SetLength(coef,fObjects,0);

end;

function TEPMModel.GetState(ABody: TSolarSystemBody; TDB: Double): TBodyState;
begin
//
end;

function TEPMModel.HasBody(ABody: TSolarSystemBody): Boolean;
begin
//
end;

procedure TEPMModel.ReadBin(ObjectNumber: Integer; FileName: string);
var
    i,coef_size: integer;
    lfn : File;
begin
    AssignFile(lfn, FileName);
    Reset(lfn);
        // Read header = first 32 bytes of the file
        BlockRead(lfn, jdb[ObjectNumber], 1) ;
        BlockRead(lfn, djb[ObjectNumber], 2) ;
        BlockRead(lfn, delta[ObjectNumber], 2) ;
        BlockRead(lfn, ncoef[ObjectNumber], 1) ;
        BlockRead(lfn, ndim[ObjectNumber], 1) ;
        BlockRead(lfn, nbl[ObjectNumber], 1) ;
    coef_size := ncoef[ObjectNumber] * ndim[ObjectNumber] * nbl[ObjectNumber];
    SetLength(coef[ObjectNumber],coef_size);
     for i:= 0 to Pred(coef_size) do
        BlockRead(lfn, coef[ObjectNumber], 2);
    CloseFile(lfn);
end;

procedure TEPMModel.ReadText(ObjectNumber: Integer; FileName: string);
var
    i,coef_size: integer;
    lfn : TextFile;
begin
    AssignFile(lfn, FileName);
    Reset(lfn);
    readln(lfn, jdb[ObjectNumber]);
    readln(lfn, djb[ObjectNumber]);
    readln(lfn, delta[ObjectNumber]);
    readln(lfn, ncoef[ObjectNumber]);
    readln(lfn, ndim[ObjectNumber]);
    readln(lfn, nbl[ObjectNumber]);
    coef_size := ncoef[ObjectNumber] * ndim[ObjectNumber] * nbl[ObjectNumber];
    SetLength(coef[ObjectNumber],coef_size);
     for i:= 0 to Pred(coef_size) do
        readln(lfn, coef[ObjectNumber,i]);
    CloseFile(lfn);
end;

procedure TEPMModel.SetTextPath(APath: string);
var
    i: integer;
    Afile: string;
begin
  for i := 1 to fObjects do
    begin
      Afile := IncludeTrailingPathDelimiter(APath) + fFileNames[i] + fTextFileExtension;
      if FileExists(Afile) then
        ReadText(i,Afile);
    end;
end;

end.

{
{ Calculates values of Chebyshev's polynomials along with their integrals
Input:
    ncoefm - polynomial power
    td - argument [-1 .. 1]
Output:
    tt - array of values of Chebyshev polynomials
    xx - array of integrals
}
procedure calc_cheb_pol(ncoefm: integer; td: double;
                        var tt, xx: TChebValues);
var
    d: double;              // 2.0 * td
    i, j, k: integer;       // counters
    flag: boolean;          // (-1)^n
begin
    tt[1] := 1.0;
    tt[2] := td;
    d := 2.0 * td;
    for i := 3 to ncoefm + 1 do
        tt[i] := tt[i - 1] * d - tt[i - 2];

    xx[1] := td;
    xx[2] := (tt[3] + tt[1]) / 4;
    for i := 3 to ncoefm do
        xx[i] := 0.5 * (tt[i + 1] / i - tt[i - 1] / (i - 2));

    j := 0;
    flag := false;
    i := 4;
    for k := 1 to (ncoefm div 2 - 1) do
    begin
        j := j + 1;
        d := 0.25 / j + 0.25 / (j + 1);
        flag := not flag;
        if (flag) then d := -d;
        xx[i] := xx[i] + d;
        i := i + 2;
    end;
end;    // end of calc_cheb_pol

{ Calculates value of velocity's component.
Input:
    ncoefm - number of coefficients
    c_ptr - position of the current interval in the array coef
    tt - array of values of Chebyshev's polynomials
Output:
    v - velocity's component
}
procedure calc_vel(ncoefm: integer; c_ptr: integer; tt: TChebValues;
                    var v: double);
var
    i : integer;
begin
    v := 0.0;
    for i := ncoefm downto 2 do
        v := v + coef[c_ptr + i - 1] * tt[i];
    v := v + coef[c_ptr]
end;    // end of calc_vel

{ Calculates value of object coordinate
Input:
    ncoefm - number of coefficients
    c_ptr - position of the current interval in the array coef
    xx - array of integrals of Chebyshev's polynomials
    dlt - approximation interval
Output:
    x - object coordinate
}
procedure calc_pos(ncoefm: integer; c_ptr: integer; xx: TChebValues; dlt: double;
                    var x: double);
var
    i, j: integer;
    x0: double;         // free term, c(c_ptr + ncoefm)
begin
    x := 0.0;
    for j := 1 to ncoefm do
    begin
        i := ncoefm - j + 1;
        x := x + coef[c_ptr + i - 1] * xx[i];
    end;
    x0 := coef[c_ptr + ncoefm];
    x := 0.5 * dlt * x + x0;
end;    // end of calc_pos

{ Calculates coordinates and velocities of the following oblects:
  1- Mercury(bar), 2- Venus(bar)  , 3- Earth+Moon(bar),  4- Mars(bar),  5- Jupiter(bar)
  6- Saturn(bar),  7- Uranus(bar) , 8- Neptune(bar), 9 - Pluto(bar) ,
  10- Moon(geo), 11- Sun(bar)
Input:
    pl_num - number of object
    jd - Julian date (integral part) at which interpolation is wanted
    dj - Julian date (frac part)
Output:
    xv - object positions
    vv - object velocities
    status - status of output data
}
procedure calc(pl_num, jd: integer; dj: double;
                var xv, vv: TRealArray; var status: boolean);
var
    jdb_: integer;          // initial Julian date (int part)
    djb_: double;           // initial Julian date (frac part)
    delta_: double;         // approximation subinterval
    nbl_: integer;          // number of subintervals
    ncoef_: integer;           // number of (polunomial degrees + free term)
    ndim_: integer;            // number of dimensions
    shift_: integer;        // position of coeffiients of current object in the array coef

    td: double;             // Julian in [initial date, final date] -> td in [-1, 1]
    cur_bl: integer;        // approximation interval for current Julian date
    tt, xx: TChebValues;    // arrays of values and integrals of Chebyshev polynomials
    coef_ptr: integer;      // position of the current interval in the array coef
    i: integer;             // counter
begin
    status := true;
    jdb_ := jdb[pl_num];
    djb_ := djb[pl_num];
    nbl_ := nbl[pl_num];
    ndim_ := ndim[pl_num];
    delta_ := delta[pl_num];
    ncoef_ := ncoef[pl_num];
    shift_ := shift[pl_num];

    td := ((jd - jdb_) + (dj - djb_)) / delta_;
    cur_bl := trunc(td);

    td := td - cur_bl;
    td := 2.0 * td - 1.0;
    cur_bl := cur_bl + 1;
    if (cur_bl <= 0) or (cur_bl > nbl_) then
    begin
      status := false;
      exit;
    end;
    shift_ := shift_ + (cur_bl - 1) * (ncoef_ * ndim_);
    calc_cheb_pol(ncoef_ - 1, td, tt, xx);
    for i := 1 to ndim_ do
    begin
       coef_ptr := shift_ + ncoef_ * (i - 1);
       calc_pos(ncoef_ - 1, coef_ptr, xx, delta_, xv[i]);
       calc_vel(ncoef_ - 1, coef_ptr, tt, vv[i]);
    end;
end;    // end of calc

}


