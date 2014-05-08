unit formMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, unitminesweeper;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
  private
  public
    procedure Initiate;
    procedure OnWin(const TotalTime: integer);
    Procedure OnLose(const TotalTime: integer);
    procedure OnFlag(const FlagCount: integer; const MinesLeft: integer);
    Procedure OnTimeInc(const CurrentTime: integer);
  end;

var
  Form1: TForm1;
  Minesweeper: TMinesweeper;

implementation

{$R *.lfm}

{ TForm1 }

procedure Tform1.OnFlag(const FlagCount: integer; const MinesLeft: integer);
begin
  Label2.caption := 'Flag Count: '+inttostr(flagCOunt)+' Mines Left: '+inttostr(MinesLeft);
end;

procedure TForm1.Initiate;
begin
  Minesweeper := TMinesweeper.Create(@Panel2);
  Minesweeper.OnLose:= @OnLose;
  Minesweeper.OnWin:= @OnWin;
  Minesweeper.OnFlag:= @OnFlag;
  Minesweeper.OnTimeInc:= @OnTimeInc;
end;

procedure TForm1.OnTimeInc(Const CurrentTime: integer);
begin
  Label1.Caption := 'Time: '+inttostr(CurrentTime)+' sec(s).';
end;

procedure TForm1.OnWin(const TotalTime: integer);
begin
  Showmessage('You won the game in '+IntToStr(totalTime)+' seconds');
end;

procedure TForm1.OnLose(const TotalTime: integer);
begin
  Showmessage('You lost this game. better luck next time.');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Initiate;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if minesweeper.gamerunning then
  Minesweeper.AutoFit;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  Minesweeper.StartGame(9,9, 10);
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  Minesweeper.StartGame(16,16, 40);
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  Minesweeper.StartGame(16,30, 99);
end;

end.

