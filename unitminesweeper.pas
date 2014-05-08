unit unitMinesweeper;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  cthreads, cmem,
  {$endif}
  Classes, SysUtils, Controls, Forms, Dialogs, ExtCtrls, unitminesweeperdefines;

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
    FParent: PWinControl;
    FData: TMinesweeperData;
    PWin, PLose: TMinesweeperResultProc;
    PIncTime: TMinesweeperTimeProc;
    PFlag: TMinesweeperFlagProc;
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
    procedure OnImageClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  const
    cvBomb = 9;
    SpritesDir = '\sprites\';
    FAvoidedRadius = 1;
  public
    GameRunning: boolean;
    property OnWin: TMinesweeperResultProc write PWin;
    property OnLose: TMinesweeperResultProc write PLose;
    property OnTimeInc: TMinesweeperTimeProc write PIncTime;
    property OnFlag: TMinesweeperFlagProc write PFlag;
    procedure StartGame(SizeH, SizeW, Mines: integer);
    procedure StopGame;
    procedure AutoFit;
    destructor Destroy; override;
    constructor Create(Parent: PWinControl);
  end;

implementation

procedure TMinesweeper.AutoFit;
// aspect ratio image is 1:1
var
  x, y, parentH, parentW: integer;
begin
  parentH := FParent^.Height;
  parentW := FParent^.Width;
  if (Parenth div FSizeH) > (parentw div FSizeW) then
    FCellHW := (ParentW div FSizeW)
  else
    FCellHW := (ParentH div FSizeH);
  FLeft := (ParentW-FSizeW*FCellHW) div 2;
  FTop := (ParentH-FSizeH*FCellHW) div 2;

  for y := 0 to FSizeH-1 do
    for x := 0 to FSizeW-1 do begin
      FData[y,x].iMage.Height := FCellHW;
      FData[y,x].iMage.Width := FCellHW;
      Fdata[y,x].Image.Left := FLeft+(x*FCellHW);
      Fdata[y,x].Image.Top := FTop+(y*FCellHW);
    end;
end;

procedure TMinesweeper.RevealBombs;
var
  x,y: integer;
begin
  for y := 0 to High(FData) do
    for x := 0 to high(FData[y]) do begin
      if (FData[y,x].CellValue = cvBomb) and not(FData[y,x].Flagged) then
        FData[y, x].Image.Picture.LoadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'bomb.png')
      else if (FData[y,x].CellValue <> cvBomb) and (FData[y,x].Flagged) then
        FData[y, x].Image.Picture.LoadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'wrong flag.png')
    end;
end;

procedure TMinesweeper.InitializeData;
var
  y, x: integer;
begin
  for y := 0 to High(FData) do
  begin
    if y >= FSizeH then
      for x := 0 to High(FData[y]) do
        Fdata[y, x].Image.Free
    else
      for x := FSizeW to High(Fdata[y]) do
        FData[y, x].Image.Free;
  end;

  SetLength(FData, FSizeH, FSizeW);
  for y := 0 to FSizeH - 1 do
    for x := 0 to FSizeW - 1 do
    begin
      with FData[y, x] do
      begin
        if Assigned(Image) then
          continue;
        Image := TImage.Create(FParent^);
        Image.Stretch := true;
        Image.Name := 'x' + IntToStr(x) + 'y' + IntToStr(y);
        Image.Parent := FParent^;
      end;
    end;
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
begin
  if not (FWinState or FLoseState) then
    if not ((x = -1) or (y = -1) or (x = FSizeW) or (y = FSizeH)) then
    begin
      if FData[y, x].Flagged then
        exit;

      if (FData[y, x].Opened) then
        exit;

      Inc(FOpenedCount);

      if (FData[y, x].cellValue = 0) then
      begin
        FData[y, x].Image.Picture.loadFromFile(ExtractFilePath(application.exename) + SpritesDir +
          IntToStr(Fdata[y, x].cellvalue) + '.png');
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
        FData[y, x].Image.Picture.loadFromFile(ExtractFilePath(application.exename) + SpritesDir +
          IntToStr(Fdata[y, x].cellvalue) + '.png');
        FData[y, x].Opened := True;
      end
      else
      begin
        FData[y, x].Opened := True;
        FData[y, x].Image.Picture.loadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'bomb.png');
        FLoseState := True;
        StopGame;
      end;
      FParent^.Update;
    end;

  if (FOpenedCount = FOpenedMax) and not (FWinState) then
  begin
    FWinState := True;
    StopGame;
  end;
end;

procedure TMinesweeper.StopGame;
begin
  GameRunning := false;
  FTimer.Terminate;
  if FLoseState then begin
    RevealBombs;
    PLose(FTimeCount);
  end
  else if FWinState then
    PWin(FTimeCount);
end;

procedure TMinesweeper.OnImageClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);

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
  ax, ay: integer;
  tmp: string;
begin
  if FLoseState or FWinState then
    exit;

  tmp := TImage(Sender).Name;
  ax := StrToInt(Copy(tmp, Pos('x', tmp) + 1, Pos('y', tmp) - (Pos('x', tmp) + 1)));
  ay := StrToInt(Copy(tmp, Pos('y', tmp) + 1, Length(tmp) - (Pos('y', tmp))));
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
    end;
    OpenCell(ax, ay);
    FParent^.update;
  end
  else
  begin
    if not FData[ay, ax].Opened then
      if FData[ay, ax].flagged then
      begin
        FData[ay, ax].flagged := False;
        FData[ay, ax].Image.Picture.LoadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'normal.png');
        Dec(FFlagCount);
        PFlag(FFlagCount, FMines - FFlagCount);
      end
      else
      begin
        FData[ay, ax].flagged := True;
        FData[ay, ax].Image.Picture.LoadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'flag.png');
        Inc(FFlagCount);
        PFlag(FFlagCount, FMines - FFlagCount);
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
      FData[y, x].Image.Picture.LoadFromFile(ExtractFilePath(application.exename) + SpritesDir + 'normal.png');
      FData[y, x].Image.OnMouseDown := @OnImageClick;
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
  FTimer := TMinesweeperTimer.Create(@FTimeCount, PIncTime);
  GameRunning := true;
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
  for i := Low(FData) to High(FData) do
    for j := Low(FData[i]) to High(FData[i]) do
      FData[i, j].Image.Free;
  SetLength(FData, 0);
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
  if not terminated then begin
    inc(PTime^);
    FOnInc(PTime^);
  end;
end;

procedure TMinesweeperTimer.Execute;
begin
  FreeOnTerminate := true;
  while not terminated do begin
    Sleep(1000);
    Synchronize(@UpdateTime);
  end;
end;

end.

