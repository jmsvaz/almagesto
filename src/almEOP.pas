{
    almEOP is part of Almagesto, a Free Pascal astronomical library.

    Copyright (C) 2010 João Marcelo S. Vaz

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
  Classes, SysUtils, almBase;

type

  { TEOP  is a class that gets the Earth Orientation Parameters from a file
  http://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now}

  TEOP = class
    private
      fList: TStringList;
    protected
      function LoadEOPC04File(FileName: String): Boolean;

    public
      constructor Create(EOPC04FileName: String);
      destructor Destroy;
      function GetEOP(UTC: TMJD; out DUT1, Xp, Yp, LOD, dX, dY: Double): Boolean;
    end;

implementation

type

  { TEOPItem }

  TEOPItem = class
    private
      fDUT1: Double;
      fdX: Double;
      fdY: Double;
      fLOD: Double;
      fUTC: TMJD;
      fXp: Double;
      fYp: Double;
    public
      constructor Create(aUTC: TMJD; aDUT1, aXp, aYp, aLOD, adX, adY: Double);
      property UTC: TMJD read fUTC;
      property DUT1: Double read fDUT1;
      property Xp: Double read fXp;
      property Yp: Double read fYp;
      property LOD: Double read fLOD;
      property dX: Double read fdX;
      property dY: Double read fdY;
    end;

{ TEOPItem }

constructor TEOPItem.Create(aUTC: TMJD; aDUT1, aXp, aYp, aLOD, adX, adY: Double
  );
begin
  fDUT1:= aDUT1;
  fdX:= adX;
  fdY:= adY;
  fLOD:= aLOD;
  fUTC:= aUTC;
  fXp:= aXp;
  fYp:= aYp;
end;

{ TEOP }

function TEOP.LoadEOPC04File(FileName: String): Boolean;
var
  aRow: TStringList;
  InputFile: TextFile;
  InputStr: String;
begin
  Result:= False;
  if FileExists(FileName) then
    begin
      AssignFile(InputFile, FileName);
      {$I-}
      try
        Reset(InputFile);
        Readln(InputFile, InputStr);
        if InputStr = 'MJD;X;sigma_X;Y;sigma_Y;UT1-UTC;sigma_UT1-UTC;LOD;sigma_LOD;dX;sigma_dX;dY;sigma_dY' then
          repeat
            Readln(InputFile, InputStr); // Reads the whole line from the file
            aRow:= TStringList.Create;
            aRow.CommaText:= InputStr;
            fList.AddObject(aRow[0],aRow);
          until(EOF(InputFile)); // EOF(End Of File) The program will keep reading new lines until there is none.
      except
        //
      end;
      {$I+}
      CloseFile(InputFile);
    end;
end;

constructor TEOP.Create(EOPC04FileName: String);
begin
  fList:= TStringList.Create;

end;

destructor TEOP.Destroy;
begin
  fList.Clear;   // todo: free the objects
  fList.Free;
end;

function TEOP.GetEOP(UTC: TMJD; out DUT1, Xp, Yp, LOD, dX, dY: Double): Boolean;
begin
  Result:= False;
  if fList.Count > 0 then
    begin

    end;

end;

end.

{
function BuscaBinaria (Vetor: array of string; Chave: string; Dim: integer): integer;
     var inicio, fim: integer; {Auxiliares que representam o inicio e o fim do vetor analisado}
         meio: integer; {Meio do vetor}
begin
     fim := Dim; {O valor do último índice do vetor}
     inicio := 1; {O valor do primeiro índice do vetor}
     BuscaBinaria := -1; {Retorna o valor -1 se a chave nao for encontrada.}
     repeat
       meio := (inicio+fim) div 2;
       if (Chave = vetor[meio]) then
         begin
           BuscaBinaria := meio;
           inicio:=fim+1; {Interrompo o repeat quando a chave for encontrada sem ter que testar lá no until. Bonito não?!}
         end;
       if (Chave < vetor[meio]) then
         fim:=(meio-1);
       if (Chave > vetor[meio]) then
         inicio:=(meio+1);
     until (inicio > fim);
 end;

procedure  LinSearch (A: ‹tArray›; key : ‹tKey›;
         var  Pos : integer; var  Success : Boolean);
begin

{Check if we'll stop before we get to the end of the list.}
if  (‹last› = 0) | (key > A[‹last›]) then  begin
   {Belongs after the end of the list.}
   Pos := ‹last› + 1;
   Success := False;
   end
else  begin
   {The correct position is somewhere before the end of the list.}
   Pos := 1;
   while  A[Position] < Key do
      Pos := Pos + 1;

   Success := A[Position] = Key;
   end  ;
end  ;   {Search}

}