{
    almEphemerides is part of Almagesto, a Free Pascal astronomical library.
    This file contains the Ephemerides classes.

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

unit almEphemerides;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, almBase;

type

  { TBody }

  TBody = class
    private
      fBody: TSolarSystemBody;
      fName: string;
      fModel: TEphemeridesModel;
      fTDB: Double;
      fPosition: TPosition;
      fVelocity: TVelocity;
      fAcceleration: TAcceleration;
      procedure SetModel(AValue: TEphemeridesModel);
      procedure SetTDB(AValue: Double);
      procedure DataChanged;
    public
      constructor Create(ABody: TSolarSystemBody; AModel: TEphemeridesModel);
      property TDB: Double read fTDB write SetTDB;
      property Model: TEphemeridesModel read fModel write SetModel;
      property Name: string read fName;
      property Position: TPosition read fPosition;
      property Velocity: TVelocity read fVelocity;
      property Acceleration: TAcceleration read fAcceleration;
  end;


implementation


{ TBody }

procedure TBody.SetModel(AValue: TEphemeridesModel);
begin
  if fModel = AValue then exit;
  if not AValue.HasBody(fBody) then
    raise Exception.Create(Format('s% doesn''t computes s% data',[AValue.Name,Name]));

  fModel:= AValue;
  DataChanged;
end;

procedure TBody.SetTDB(AValue: Double);
begin
  if fTDB = AValue then exit;
  fTDB:= AValue;
  DataChanged;
end;

constructor TBody.Create(ABody: TSolarSystemBody; AModel: TEphemeridesModel);
begin
  fBody:= ABody;
  fTDB:= J2000;
//  fName:= ...
  Model:= AModel;
end;

procedure TBody.DataChanged;
var
  bs: TBodyState;
begin
  bs:= fModel.GetState(fBody,TDB);
  fPosition:= bs.Pos;
  fVelocity:= bs.Vel;
  fAcceleration:= bs.Accel;
end;

end.

