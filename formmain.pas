unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, unitminesweeper;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Refresher: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure RefresherTimer(Sender: TObject);
  public
    board: string;
    procedure Initiate;
    procedure OnWin(const TotalTime: integer);
    procedure OnLose(const TotalTime: integer);
    procedure OnFlag(const FlagCount: integer; const MinesLeft: integer);
    procedure OnTimeInc(const CurrentTime: integer);
  end;

var
  Form1: TForm1;
  Minesweeper: TMinesweeper;

implementation

{$R *.lfm}

{ TForm1 }

procedure Tform1.OnFlag(const FlagCount: integer; const MinesLeft: integer);
begin
  Label2.Caption := 'Flag Count: ' + IntToStr(flagCOunt) + ' Mines Left: ' + IntToStr(MinesLeft);
end;

procedure TForm1.Initiate;
begin
  Minesweeper := TMinesweeper.Create(@Panel2);
  Minesweeper.OnLose := @OnLose;
  Minesweeper.OnWin := @OnWin;
  Minesweeper.OnFlag := @OnFlag;
  Minesweeper.OnTimeInc := @OnTimeInc;
end;

procedure TForm1.OnTimeInc(const CurrentTime: integer);
begin
  Label1.Caption := 'Time: ' + IntToStr(CurrentTime) + ' sec(s).';
end;

procedure TForm1.OnWin(const TotalTime: integer);
begin
  ShowMessage('You won the game in ' + IntToStr(totalTime) + ' seconds');
end;

procedure TForm1.OnLose(const TotalTime: integer);
begin
  ShowMessage('You lost this game. better luck next time.');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WindowState := wsMaximized;
  Initiate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  minesweeper.Autofit;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  board := '9x9';
  Minesweeper.StartGame(9, 9, 10);
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  board := '16x16';
  Minesweeper.StartGame(16, 16, 40);
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  board := '16x30';
  Minesweeper.StartGame(16, 30, 99);
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  if board = '9x9' then
    MenuItem3Click(nil)
  else if board = '16x16' then
    MenuItem5Click(nil)
  else if board = '16x30' then
    Minesweeper.StartGame(16, 30, 99);
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  Application.MessageBox('Created by: Fata Nugraha'+lineending+'Compiled with fpc 2.62 under Lazarus IDE on windows.'+LineEnding+'Email: fatanugraha@outlook.com / github: fatanugraha / twitter: @cybzx09', 'About', 0);
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
var
  str1, str2, str3: String;
begin
  if InputQuery('Custom', 'Width:', str1) then
    if inputQuery('Custom', 'Height:', str2) then
      if inputQuery('Custom', 'Mines:', str3) then
        Minesweeper.StartGame(StrToInt(str2), StrToInt(str1), strtoint(str3));
end;

procedure TForm1.Panel2Resize(Sender: TObject);
begin
  Refresher.Enabled := true;
end;

procedure TForm1.RefresherTimer(Sender: TObject);
begin
  Refresher.Enabled := false;
  if minesweeper.GameRunning then
    Minesweeper.AutoFit;
end;

end.

