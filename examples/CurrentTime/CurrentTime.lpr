program CurrentTime;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, almagesto
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Current Time Scales';
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

