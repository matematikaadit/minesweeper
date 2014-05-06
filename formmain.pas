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
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
  private
    FTime: integer;
    board: string;
  public
    procedure Initiate;
    procedure Win;
    Procedure Lose;
    Procedure TimeInc;
  end;

var
  Form1: TForm1;
  Minesweeper: TMinesweeper;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Initiate;
begin
  Minesweeper.Destroy;
  Minesweeper := TMinesweeper.Create(@Form1);
  Minesweeper.OnLose:= @Lose;
  Minesweeper.OnWin:= @Win;
  Minesweeper.OnTimeInc:= @TimeINc;
  FTime := 0;
end;

procedure TForm1.TimeInc;
begin
  inc(FTIme);
  Label1.Caption := 'Time: '+inttostr(FTIME)+' sec(s).';
end;

procedure TForm1.Win;
begin
  Showmessage('luck ass.');
end;

procedure TForm1.Lose;
begin
  Showmessage('hahaha loser.');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Minesweeper.ReplaySameBoard;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Minesweeper := TMinesweeper.Create(@Form1)
end;

procedure TForm1.Label1Click(Sender: TObject);
begin

end;

procedure TForm1.Label1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  button1.Visible:= true;
  if board = '9x9' then
    Minesweeper.ReplaySameBoard
  else begin
    Initiate;
    board := '9x9';
    Minesweeper.StartGame(9,9, 10);
    Width := 9*32;
    Height := 9*32+Panel1.Height+20;
  end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  button1.Visible:= true;
  position := poDesktopCenter;
  if board = '16x16' then
    Minesweeper.ReplaySameBoard
  else begin
    Initiate;
    board := '16x16';
    Minesweeper.StartGame(16,16, 40);
    Width := 16*32;
    Height := 16*32+Panel1.Height+20;
  end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  button1.Visible:= true;
  if board = '16x30' then
      Minesweeper.ReplaySameBoard
  else begin
    board := '16x30';
    Initiate;
    Minesweeper.StartGame(16,30, 99);
    Width := 30*32;
    Height := 16*32+Panel1.Height+20;
  end;
  position := poDesktopCenter;
end;

end.

