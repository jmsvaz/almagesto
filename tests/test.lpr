program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testDateTime, testEphemerides, 
testLocalReferenceSystems, testEarthOrientation;

{$R *.res}

begin
  Application.Title:='Almagesto Tests';
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
