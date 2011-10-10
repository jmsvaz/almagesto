program fbimporter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fbmain
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='FB Importer';
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

