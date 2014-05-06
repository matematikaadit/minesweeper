unit unitMinesweeper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, Dialogs;

type
  TMinesweeperProc = TThreadMethod;

  PWinControl = ^TWinControl;

  TMinesweeperCell = record
    Image: TImage;
    opened: boolean;
    Flagged: boolean;
    CellValue: shortint; //if mines hint = 0..8 if bomb = 9
  end;

  TMinesweeper = class(TObject)
  private
    FLoseState: boolean;
    FSizeH: integer;
    FSizeW: integer;
    FMinesSizeH, FMinesSizeW: integer;
    FMines: integer;
    FOpenedMax: integer;
    FTimeCount: int64;
    FStarted: boolean;
    FParent: PWinControl;
    FData: array of array of TMinesweeperCell;
    FOpenedCount: integer;
    FFirstClick: boolean;
    PIncTime, PWin, PLose: TMinesweeperProc;
    function CheckValidBoard: boolean;
    procedure InitializeData;
    procedure RandomizeMines(AvoidedX, AvoidedY: integer; AvoidedRadius: byte);
    procedure WriteMinesHint;
    procedure DrawBoard;
    procedure OpenCell(x, y: integer; role: integer=0); //recursively open cell
    procedure OnImageClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  const
    cvBomb = 9;
    SpritesDir = '\sprites\';
    FAvoidedRadius = 1;
  public
    property MinesSizeH: integer read FMinesSizeH; // default 32
    property MinesSizeW: integer read FMinesSizeW; // default 32
    property OnWin : TMinesweeperProc write PWin;
    property OnLose : TMinesweeperProc write PLose;
    property OnTimeInc : TMinesweeperProc write PIncTime;
    procedure StartGame(SizeH, SizeW, Mines: integer);
    procedure StopGame(Switch: boolean);
    procedure ReplaySameBoard;
    destructor Destroy; override;
    constructor Create(Parent: PWinControl);
  end;

implementation

procedure TMinesweeper.ReplaySameBoard;
var
  i,j: integer;
begin
  for i := 0 to FSizeH-1 do
    for j:=0 to FSizeW-1 do begin
      Fdata[i,j].Opened := false;
      Fdata[i,j].Flagged := false;
      Fdata[i,j].CellValue := 0;
    end;
  SetLength(FData, FSizeH, FSizeW);
  FOpenedCount := 0;
  FFirstClick := True;
  FStarted := True;
  FLoseState := false;
  if not CheckValidBoard then
    raise Exception.Create('Mines total is more than the board size.');
  DrawBoard;
  FTimeCount := 0;
end;

procedure TMinesweeper.OpenCell(x, y: integer; role: integer=0); //recursively open cell
begin
  if not ((x = -1) or (y = -1) or (x = FSizeW) or (y = FSizeH)) then
  begin
    if FData[y, x].Flagged then
      exit;
    if (FData[y, x].opened) then
      exit;
    inc(FOpenedCount);
    if (FData[y, x].cellValue = 0) then
    begin
      FData[y, x].Image.Picture.loadFromFile(ExtractFilePath(application.exename) + SpritesDir +
        IntToStr(Fdata[y, x].cellvalue) + '.png');
      FData[y, x].opened := True;
        OPenCell(x-1, y-1);
        OPenCell(x-1, y);
        OPenCell(x-1, y+1);
        OPenCell(x, y-1);
        OPenCell(x, y+1);
        OPenCell(x+1, y-1);
        OPenCell(x+1, y);
        OPenCell(x+1, y+1);
    end
    else if (FData[y, x].cellValue <> cvBomb) then
    begin
      FData[y, x].Image.Picture.loadFromFile(ExtractFilePath(application.exename) + SpritesDir +
        IntToStr(Fdata[y, x].cellvalue) + '.png');
        FData[y, x].Opened := True;
    end
    else
    begin
      FData[y, x].Opened := True;
      FData[y, x].Image.Picture.loadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'bomb.png');
      StopGame(true);
    end;
    if role =0 then
      FParent^.Update;
  end;
  if FOpenedCount = FOpenedMax then begin
    //FTimeCounter.terminate;
    PWin;
    StopGame(false);
  end;
end;

procedure TMinesweeper.OnImageClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

function CheckCell(x,y: integer): boolean;
begin
  if (x = -1) or (y = -1) or (x = FSizeW) or (y = FSizeH) then
  begin
    Result := False;
    exit;
  end;
  Result := FData[y, x].Flagged;
end;

function CheckTotal(x, y: integer): integer;
var
  tmp: integer;
begin
  tmp := 0;
  if CheckCell(x-1, y-1) then inc(tmp);
  if CheckCell(x-1, y) then inc(tmp);
  if CheckCell(x-1, y+1) then inc(tmp);
  if CheckCell(x, y-1) then inc(tmp);
  if CheckCell(x, y+1) then inc(tmp);
  if CheckCell(x+1, y-1) then inc(tmp);
  if CheckCell(x+1, y) then inc(tmp);
  if CheckCell(x+1, y+1) then inc(tmp);
  result := tmp;
end;

var
  ax, ay: integer;
  tmp: string;
begin
  if FLoseState then
    exit;
  tmp := TImage(Sender).Name;
  ax := StrToInt(Copy(tmp, Pos('x', tmp) + 1, Pos('y', tmp) - (Pos('x', tmp) + 1)));
  ay := StrToInt(Copy(tmp, Pos('y', tmp) + 1, Length(tmp) - (Pos('y', tmp))));
  if Button = mbLeft then begin

    if FData[ay, ax].Flagged then
      exit;

    if FData[ay, ax].opened then
      if CheckTotal(ax, ay) = FData[ay, ax].CellValue then begin
        OpenCell(ax-1, ay-1);
        OpenCell(ax-1, ay);
        OpenCell(ax-1, ay+1);
        OpenCell(ax, ay-1);
        OpenCell(ax, ay+1);
        OpenCell(ax+1, ay-1);
        OpenCell(ax+1, ay);
        OpenCell(ax+1, ay+1);
      end;

    if FFirstClick then
    begin
      RandomizeMines(ax, ay, 1);
      WriteMinesHint;
      FFirstClick := False;
    end;
    OpenCell(ax, ay);
    FParent^.update;
  end else begin
    if not FData[ay, ax].Opened then
    if FData[ay, ax].flagged then begin
      FData[ay, ax].flagged := false;
      FData[ay, ax].Image.Picture.LoadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'normal.png')
    end
    else begin
      FData[ay, ax].flagged := true;
      FData[ay, ax].Image.Picture.LoadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'flag.png')
    end;
  end;
end;

procedure TMinesweeper.DrawBoard;
var
  x, y: integer;
begin
  for y := 0 to FSizeH - 1 do
    for x := 0 to FSizeW - 1 do
    begin
      FData[y, x].Image.Left := 32 * x;
      FData[y, x].Image.Top := 32 * y;
      FData[y, x].Image.Picture.LoadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'normal.png');
      FData[y, x].Image.OnMouseDown := @OnImageClick;
    end;
end;

procedure TMinesweeper.StopGame(switch: boolean);
begin
  //FTimeCounter.terminate;
  FLoseState :=true;
  if switch then
    PLose;
end;

procedure TMinesweeper.InitializeData;
var
  y, x: integer;
begin
  SetLength(FData, FSizeH, FSizeW);
  for y := 0 to FSizeH - 1 do
    for x := 0 to FSizeW - 1 do
    begin
      with FData[y, x] do
      begin
        Image := TImage.Create(FParent^);
        Image.Name := 'x' + IntToStr(x) + 'y' + IntToStr(y);
        Image.Parent := FParent^;
      end;
    end;
end;

procedure TMinesweeper.RandomizeMines(AvoidedX, AvoidedY: integer; AvoidedRadius: byte);
var
  i, y, x: integer;
  avzonex, avzoney: boolean;
begin
  for i := 0 to FMines - 1 do
  begin
    randomize;
    while True do
    begin
      avzonex := False;
      avzoney := False;
      x := Random(FSizeW);
      y := random(FSizeH);
      if (AvoidedX - AvoidedRadius <= x) and (AVoidedX + AVoidedRadius >= x) then
        avzonex := True;
      if (Avoidedy - AvoidedRadius <= y) and (AVoidedy + AVoidedRadius >= y) then
        avzoney := True;
      if avzonex and avzoney then
        continue;
      if FData[y, x].cellvalue = cvbomb then
        continue;
      FData[y, x].cellValue := cvbomb;
      break;
    end;
  end;
end;

function TMinesweeper.CheckValidBoard: boolean;
begin
  Result := FSizeH * FSizeW - (FAvoidedRadius * 2 + 1) * (FAvoidedRadius * 2 + 1) > FMines;
end;

procedure TMinesweeper.WriteMinesHint;

  function CheckCell(y, x: integer): boolean;
  begin
    if (x = -1) or (y = -1) or (x = FSizeW) or (y = FSizeH) then
    begin
      Result := False;
      exit;
    end;
    Result := FData[y, x].cellvalue = cvbomb;
  end;

var
  x, y: integer;
begin
  for y := 0 to FSizeH - 1 do
    for x := 0 to FSizeW - 1 do
    begin
      if FData[y, x].cellvalue <> cvbomb then
      begin
        FData[y, x].CellValue := 0;
        if CheckCell(y - 1, x - 1) then
          Inc(FData[y, x].CellValue);
        if CheckCell(y - 1, x) then
          Inc(FData[y, x].CellValue);
        if CheckCell(y - 1, x + 1) then
          Inc(FData[y, x].CellValue);

        if CheckCell(y, x - 1) then
          Inc(FData[y, x].CellValue);
        if CheckCell(y, x + 1) then
          Inc(FData[y, x].CellValue);

        if CheckCell(y + 1, x - 1) then
          Inc(FData[y, x].CellValue);
        if CheckCell(y + 1, x) then
          Inc(FData[y, x].CellValue);
        if CheckCell(y + 1, x + 1) then
          Inc(FData[y, x].CellValue);
      end;
    end;
end;

procedure TMinesweeper.StartGame(SizeH, SizeW, Mines: integer);
begin
  FOpenedCount := 0;
  FFirstClick := True;
  FStarted := True;
  FLoseState := false;
  FSizeH := SizeH;
  FSizeW := SizeW;
  FMines := Mines;
  FOpenedMax := (SizeH*SizeW)-Mines;
  if not CheckValidBoard then
    raise Exception.Create('Mines total is more than the board size.');
  InitializeData;
  DrawBoard;
  FTimeCount := 0;
end;

constructor TMinesweeper.Create(Parent: PWinControl);
begin
  inherited Create;
  FParent := Parent;
end;

destructor TMinesweeper.Destroy;
var
  i, j: integer;
begin
  if FStarted then
    StopGame(false);
  for i := Low(FData) to High(FData) do
    for j := Low(FData[i]) to High(FData[i]) do
      FData[i, j].Image.Free;
  SetLength(FData, 0);
  inherited Destroy;
end;


end.
