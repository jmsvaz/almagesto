unit fbmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls;

type

  { TfmMain }

  TfmMain = class(TForm)
    btRun: TButton;
    edInputFile: TFileNameEdit;
    edOutputFile: TFileNameEdit;
    lbOutputFile: TLabel;
    lbInputFile: TLabel;
    mmLog: TMemo;
    procedure btRunClick(Sender: TObject);
    procedure edInputFileEditingDone(Sender: TObject);
    procedure edOutputFileEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure CanRun;
    procedure LogMessage(AMessage: string);
    procedure Run;
    function ParseStr(Str: string): string;
  public
    { public declarations }
  end; 

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Caption:= Application.Title;
end;

procedure TfmMain.edInputFileEditingDone(Sender: TObject);
begin
  CanRun;
end;

procedure TfmMain.btRunClick(Sender: TObject);
begin
  Run;
end;

procedure TfmMain.edOutputFileEditingDone(Sender: TObject);
begin
  CanRun;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  CanRun;
end;

procedure TfmMain.CanRun;
begin
  btRun.Enabled:= FileExists(edInputFile.FileName) and (Length(edOutputFile.FileName)>0);
end;

procedure TfmMain.LogMessage(AMessage: string);
begin
  mmLog.Lines.Add(AMessage);
end;

procedure TfmMain.Run;
var
  InputFile, OutputFile: TextFile;
  InputStr,OutputStr: String;
  LineCount: Integer;
begin
  LineCount:= 0;
  // Exit if InputFile doesn't exists
  if not FileExists(edInputFile.FileName) then
    begin
      ShowMessage(Format('File "%s" doesn''t exist.',[edInputFile.FileName]));
      Exit;
    end;
  // Exit if OutputFile won't be overwritten
  if FileExists(edOutputFile.FileName) then
    if MessageDlg('Question', Format('Do you wish to overwrite file "%s"?',[edOutputFile.FileName]), mtConfirmation, mbYesNo,0) = mrNo then
      Exit;

  AssignFile(InputFile, edInputFile.FileName);
  AssignFile(OutputFile, edOutputFile.FileName);
  {$I-}
  try
    Reset(InputFile);
    LogMessage(Format('Reading from file "%s".',[edInputFile.FileName]));
    Rewrite(OutputFile);  // creating the file
    LogMessage(Format('Writing to file "%s".',[edOutputFile.FileName]));
    repeat
      Readln(InputFile, InputStr); // Reads the whole line from the file
      OutputStr:= ParseStr(InputStr);
      Writeln(OutputFile, OutputStr);
      Inc(LineCount);
    until(EOF(InputFile)); // EOF(End Of File) The the program will keep reading new lines until there is none.
    LogMessage(Format('Number of lines: %d',[LineCount]));
    LogMessage('Done!');
  except
    ShowMessage('Erro');
  end;
  {$I+}
  CloseFile(InputFile);
  CloseFile(OutputFile);
end;

function TfmMain.ParseStr(Str: string): string;
type
  TParseState = (psUnknown,psField);
var
  s, sField: string;
  iState: TParseState;
  i, iLength: Integer;
  aFields: TStringList;
begin
  aFields:= TStringList.Create;
  s:= Trim(Str);
  // determine length of input //
  iLength := Length(s);
  // exit if empty string //
  if iLength = 0 then
    Exit;
  // initialize State Machine //
  iState := psUnknown;
  sField := '';

  // state machine //
  for i := 1 to iLength do
    case iState of
      psUnknown: // unknown //
        begin
          if s[i] = '-' then  // got a minus
            begin
              sField := s[i];
            end
          else
            if s[i] = '.' then  // got a point - missing the left Zero //
              begin
                sField:= sField + '0.';
                iState:= psField;
              end
            else
              if s[i] <> ' ' then
                begin  // start of regular field //
                  sField := sField + s[i];
                  iState := psField;
                  if (i = iLength) then // EOL //
                    begin
                      aFields.Add(sField);
                      sField := '';
                    end;
                end;
        end;
      psField: // continuation of regular field //
        begin
          if s[i] = ' ' then
            begin
              aFields.Add(sField);
              sField := '';
              iState := psUnknown;
            end
          else  // concatenate current char //
            begin
              sField := sField + s[i];
              if (i = iLength) then // EOL //
                begin
                  aFields.Add(sField);
                  sField := '';
                end;
            end;
        end;
    end; // case iState //
  Result:= '(' + aFields[0] + ',' + aFields[1] + ',' + aFields[2]+ ',' + aFields[3]+ ',' + aFields[4] + '),';
end;

end.

