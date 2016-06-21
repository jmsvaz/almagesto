unit testCalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestFixedDateConversion }

  TTestFixedDateConversion= class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFixedDateEpoch;
  end;

implementation

{ TTestFixedDateConversion }

procedure TTestFixedDateConversion.SetUp;
begin
  inherited SetUp;
end;

procedure TTestFixedDateConversion.TearDown;
begin
  inherited TearDown;
end;

procedure TTestFixedDateConversion.TestFixedDateEpoch;
begin

end;

initialization
  RegisterTest(TTestFixedDateConversion);

end.

