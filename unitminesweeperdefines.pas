unit unitMinesweeperDefines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, Graphics;

type
  TMinesweeperCell = record
    opened: boolean;
    Flagged: boolean;
    CellValue: shortint; //if mines hint = 0..8 if bomb = 9
  end;

  PInteger = ^integer;
  TMinesweeperProc = procedure of object;
  TMinesweeperResultProc = procedure (const TotalTime: integer) of object;
  TMinesweeperFlagProc = procedure(const FlagCount: integer; const MinesLeft: integer) of object;
  TMinesweeperTimeProc = procedure(const CurrentTime: integer) of object;
  TMinesweeperData = array of array of TMinesweeperCell;
  TMinesweeperSprites = array of TPicture;
  PCanvas = ^TCanvas;
  PPanel = ^TPanel;

implementation

end.

