program TimeGraph;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, uMain, almagesto
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Time Scales Graph';
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

