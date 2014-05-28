program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF LINUX} cthreads, cmem {$ENDIF} Interfaces, Forms, formMain, unitMinesweeper, unitMinesweeperDefines;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

