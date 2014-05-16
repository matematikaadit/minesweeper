unit unitMinesweeper;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  cthreads, cmem,
  {$endif}
  Classes, SysUtils, Controls, Forms, Dialogs, ExtCtrls, Graphics, unitminesweeperdefines;

type
  TMinesweeperTimer = class(TThread)
  protected
    PTime: PInteger;
    FOnInc: TMinesweeperTimeProc;
    procedure UpdateTime;
    procedure Execute; override;
  public
    constructor Create(Counter: PInteger; const OnInc: TMinesweeperTimeProc);
  end;

  TMinesweeper = class(TObject)
  private
    FLeft, FTop: integer;
    FCellHW: integer;
    FSizeH: integer;
    FSizeW: integer;
    FParent: PCustomControl;
    FCanvas: PCanvas;
    FHover: TPoint;
    FData: TMinesweeperData;
    PWin, PLose: TMinesweeperResultProc;
    PIncTime: TMinesweeperTimeProc;
    PFlag: TMinesweeperFlagProc;
    FSprites: TMinesweeperSprites;
    FMines: integer;
    FOpenedMax: integer;
    FTimer: TMinesweeperTimer;
  private
    FFlagCount: integer;
    FOpenedCount: integer;
    FLoseState: boolean;
    FWinState: boolean;
    FTimeCount: int64;
    FFirstClick: boolean;
  private
    procedure RevealBombs;
    procedure CheckValidBoard;
    procedure InitVariables;
    procedure InitializeData;
    procedure RandomizeMines(AvoidedX, AvoidedY: integer; AvoidedRadius: byte);
    procedure WriteMinesHint;
    procedure DrawBoard;
    procedure OpenCell(x, y: integer); //recursively open cell
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  const
    cvBomb = 9;
    cvFlag = 10;
    cvNormal = 11;
    cvWrongFlag = 12;
    cvHover = 13;
    SpritesDir = 'sprites';
    FAvoidedRadius = 1;
  public
    GameRunning: boolean;
    property OnWin: TMinesweeperResultProc write PWin;
    property OnLose: TMinesweeperResultProc write PLose;
    property OnTimeInc: TMinesweeperTimeProc write PIncTime;
    property OnFlag: TMinesweeperFlagProc write PFlag;
    procedure StartGame(SizeH, SizeW, Mines: integer);
    procedure StopGame;
    procedure AutoFit(FromDrawBoard: boolean = False);
    destructor Destroy; override;
    constructor Create(Parent: PCustomControl);
  end;

implementation

procedure TMinesweeper.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  ax, ay, bx, by: integer;
  CellRect: TRect;
begin
  if FloseState or FWinState then
    exit;

  if (x < FLeft) or (x > (FLeft + FCellHW * FSizeW)) then
    exit;

  if (y < FTop) or (y > (FTop + FCellHW * FSizeh)) then
    exit;

  ax := (x - FLeft) div FCellHW;
  ay := (y - FTop) div FCellHW;

  if Fdata[ay, ax].opened or Fdata[ay, ax].flagged then
    exit;

  bx := FLeft + ax * FCellHW;
  by := FTop + ay * FcellHw;

  if (FHover.x <> ax) or (FHover.y <> ay) then
  begin
    CellRect.Top := by;
    CellRect.Left := bx;
    CellRect.Bottom := CellRect.Top + FCellHW;
    CellRect.Right := CellRect.Left + FCellHW;
    FCanvas^.StretchDraw(CelLRect, FSprites[cvHover].Graphic);
    if not (Fdata[fhover.y, fhover.x].opened or Fdata[fhover.y, fhover.x].flagged) then
    begin
      CellRect.Top := FTop + FHover.y * FcellHw;
      CellRect.Left := FLeft + FHover.x * FCellHW;
      CellRect.Bottom := CellRect.Top + FCellHW;
      CellRect.Right := CellRect.Left + FCellHW;
      FCanvas^.StretchDraw(CellRect, FSprites[cvNormal].Graphic);
    end;
    FHover.x := ax;
    FHover.y := ay;
  end;
end;

procedure TMinesweeper.AutoFit(FromDrawBoard: boolean = False);
var
  parentH, parentW: integer;
begin
  parentH := FCanvas^.Height;
  parentW := FCanvas^.Width;
  if (Parenth div FSizeH) > (parentw div FSizeW) then
    FCellHW := (ParentW div FSizeW)
  else
    FCellHW := (ParentH div FSizeH);
  FLeft := (ParentW - FSizeW * FCellHW) div 2;
  FTop := (ParentH - FSizeH * FCellHW) div 2;

  if not fromdrawboard then
    DrawBoard;
end;

procedure TMinesweeper.RevealBombs;
var
  CellRect: TRect;
  x, y: integer;
begin
  for y := 0 to High(FData) do
    for x := 0 to high(FData[y]) do
    begin
      if (FData[y, x].CellValue = cvBomb) and not (FData[y, x].Flagged) then
      begin
        CellRect.Top := FTop + y * FcellHw;
        CellRect.Left := FLeft + x * FCellHW;
        CellRect.Bottom := CellRect.Top + FCellHW;
        CellRect.Right := CellRect.Left + FCellHW;
        FCanvas^.StretchDraw(CellRect, FSprites[cvBomb].Graphic);
      end
      else if (FData[y, x].CellValue <> cvBomb) and (FData[y, x].Flagged) then
      begin
        CellRect.Top := FTop + y * FcellHw;
        CellRect.Left := FLeft + x * FCellHW;
        CellRect.Bottom := CellRect.Top + FCellHW;
        CellRect.Right := CellRect.Left + FCellHW;
        FCanvas^.StretchDraw(CellRect, FSprites[cvWrongFlag].Graphic);
      end;
    end;
end;

procedure TMinesweeper.InitializeData;
begin
  TPanel(FParent^).OnMouseUp := @OnMouseUp;
  //TPanel(FParent^).OnMouseMove := @OnMouseMove;
  SetLength(FData, FSizeH, FSizeW);

  FCanvas^.Brush.Color := clBlack;
  FCanvas^.FillRect(0, 0, FCanvas^.Width, FCanvas^.Height);
end;

procedure TMinesweeper.InitVariables;
var
  i, j: integer;
begin
  FOpenedCount := 0;
  FTimeCount := 0;
  FFlagCount := 0;
  FWinState := False;
  FLoseState := False;
  FFlagCount := 0;
  FFirstClick := True;

  for i := 0 to FSizeH - 1 do
    for j := 0 to FSizeW - 1 do
    begin
      Fdata[i, j].Opened := False;
      Fdata[i, j].Flagged := False;
      Fdata[i, j].CellValue := 0;
    end;
end;

procedure TMinesweeper.OpenCell(x, y: integer);
var
  CellRect: TRect;
begin
  if not (FWinState or FLoseState) then
    if not ((x = -1) or (y = -1) or (x = FSizeW) or (y = FSizeH)) then
    begin
      CellRect.Top := FTop + y * FcellHw;
      CellRect.Left := FLeft + x * FCellHW;
      CellRect.Bottom := CellRect.Top + FCellHW;
      CellRect.Right := CellRect.Left + FCellHW;

      if FData[y, x].Flagged then
        exit;

      if (FData[y, x].Opened) then
        exit;

      Inc(FOpenedCount);

      if (FData[y, x].cellValue = 0) then
      begin
        FCanvas^.StretchDraw(CellRect, FSprites[Fdata[y, x].cellValue].Graphic);
        FData[y, x].opened := True;
        OPenCell(x - 1, y - 1);
        OPenCell(x - 1, y);
        OPenCell(x - 1, y + 1);
        OPenCell(x, y - 1);
        OPenCell(x, y + 1);
        OPenCell(x + 1, y - 1);
        OPenCell(x + 1, y);
        OPenCell(x + 1, y + 1);
      end

      else if (FData[y, x].cellValue <> cvBomb) then
      begin
        FCanvas^.StretchDraw(CellRect, FSprites[Fdata[y, x].cellValue].Graphic);
        FData[y, x].Opened := True;
      end
      else
      begin
        FData[y, x].Opened := True;
        FCanvas^.StretchDraw(CellRect, FSprites[cvBomb].Graphic);
        FLoseState := True;
        StopGame;
      end;
    end;

  if (FOpenedCount = FOpenedMax) and not (FWinState) then
  begin
    FWinState := True;
    StopGame;
  end;
end;

procedure TMinesweeper.StopGame;
begin
  GameRunning := False;
  if Assigned(FTimer) then
    FTimer.Terminate;
  if FLoseState then
  begin
    RevealBombs;
    PLose(FTimeCount);
  end
  else if FWinState then
    PWin(FTimeCount);
end;

procedure TMinesweeper.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);

  function CheckCell(x, y: integer): boolean;
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
    if CheckCell(x - 1, y - 1) then
      Inc(tmp);
    if CheckCell(x - 1, y) then
      Inc(tmp);
    if CheckCell(x - 1, y + 1) then
      Inc(tmp);
    if CheckCell(x, y - 1) then
      Inc(tmp);
    if CheckCell(x, y + 1) then
      Inc(tmp);
    if CheckCell(x + 1, y - 1) then
      Inc(tmp);
    if CheckCell(x + 1, y) then
      Inc(tmp);
    if CheckCell(x + 1, y + 1) then
      Inc(tmp);
    Result := tmp;
  end;

var
  bx, by, ax, ay: integer;
  CellRect: TRect;
begin
  if (x < FLeft) or (x > (FLeft + FCellHW * FSizeW)) then
    exit;
  if (y < FTop) or (y > (FTop + FCellHW * FSizeh)) then
    exit;

  if FloseState or FWinState then
    exit;

  ax := (x - FLeft) div FCellHW;
  ay := (y - FTop) div FCellHW;

  bx := FLeft + ax * FCellHW;
  by := FTop + ay * FcellHw;
  if Button = mbLeft then
  begin

    if FData[ay, ax].Flagged then
      exit;

    if FData[ay, ax].opened then
      if CheckTotal(ax, ay) = FData[ay, ax].CellValue then
      begin
        OpenCell(ax - 1, ay - 1);
        OpenCell(ax - 1, ay);
        OpenCell(ax - 1, ay + 1);
        OpenCell(ax, ay - 1);
        OpenCell(ax, ay + 1);
        OpenCell(ax + 1, ay - 1);
        OpenCell(ax + 1, ay);
        OpenCell(ax + 1, ay + 1);
      end;

    if FFirstClick then
    begin
      RandomizeMines(ax, ay, 1);
      WriteMinesHint;
      FFirstClick := False;
      FTimer := TMinesweeperTimer.Create(@FTimeCount, PIncTime);
    end;
    OpenCell(ax, ay);
  end
  else
  begin
    if not FData[ay, ax].Opened then
    begin
      CellRect.Top := by;
      CellRect.Left := bx;
      CellRect.Bottom := by + FCellHW;
      CellRect.Right := bx + FCellHW;
      if FData[ay, ax].flagged then
      begin
        FData[ay, ax].flagged := False;
        FCanvas^.StretchDraw(CellRect, FSprites[cvNormal].Graphic);
        Dec(FFlagCount);
        PFlag(FFlagCount, FMines - FFlagCount);
      end
      else
      begin
        FData[ay, ax].flagged := True;
        FCanvas^.StretchDraw(CellRect, FSprites[cvFlag].Graphic);
        Inc(FFlagCount);
        PFlag(FFlagCount, FMines - FFlagCount);
      end;
    end;
  end;
end;

procedure TMinesweeper.DrawBoard;
var
  x, y: integer;
  CellRect: TRect;
begin
  AutoFit(True);

  for y := 0 to FSizeH - 1 do
    for x := 0 to FSizeW - 1 do
    begin
      CellRect.Left := FLeft + (FCellHW * x);
      CellRect.Top := FTop + (FCellHW * y);
      CellRect.Bottom := FTop + (FCellHW * y) + FCellHW;
      CellRect.Right := FLeft + (FCellHW * x) + FCellHW;
      FCanvas^.StretchDraw(CellRect, FSprites[cvNormal].Graphic);
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

procedure TMinesweeper.CheckValidBoard;
begin
  if not (FSizeH * FSizeW - (FAvoidedRadius * 2 + 1) * (FAvoidedRadius * 2 + 1) > FMines) then
    raise Exception.Create('Mines total is more than the board size.');
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
  if GameRunning then
    StopGame;
  if not ((SizeH = FSizeH) and (SizeW = FSizeW) and (FMines = Mines)) then
  begin
    FSizeH := SizeH;
    FSizeW := SizeW;
    FMines := Mines;
    CheckValidBoard;
    FOpenedMax := (SizeH * SizeW) - Mines;
    InitializeData;
    Autofit;
  end;
  InitVariables;
  DrawBoard;
  GameRunning := True;
end;

constructor TMinesweeper.Create(Parent: PCustomControl);
const
  SpritesCount = 14;
var
  i: integer;
begin
  inherited Create;
  FParent := Parent;
  FCanvas := @Parent^.Canvas;
  SetLength(FSprites, SpritesCount);
  for i := 0 to SpritesCount - 1 do
  begin
    FSprites[i] := TPicture.Create;
    FSprites[i].LoadFromFile(ExpandFileName(SpritesDir) + '\' + IntToStr(i) + '.png');
  end;
end;

destructor TMinesweeper.Destroy;
var
  i: integer;
begin
  SetLength(FData, 0);
  for i := 0 to High(FSprites) do
    FSprites[i].Free;

  inherited Destroy;
end;

constructor TMinesweeperTimer.Create(Counter: PInteger; const OnInc: TMinesweeperTimeProc);
begin
  inherited Create(True);
  PTime := Counter;
  FOnInc := OnInc;
  Start;
end;

procedure TMinesweeperTimer.UpdateTime;
begin
  if not terminated then
  begin
    Inc(PTime^);
    FOnInc(PTime^);
  end;
end;

procedure TMinesweeperTimer.Execute;
begin
  FreeOnTerminate := True;
  while not terminated do
  begin
    Sleep(1000);
    Synchronize(@UpdateTime);
  end;
end;

end.
