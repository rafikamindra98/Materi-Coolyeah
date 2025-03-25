unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, jpeg, StdCtrls, Grids;

type
  TForm1 = class(TForm)
    TimerLoad: TTimer;
    start: TImage;
    exit: TImage;
    keterangan: TImage;
    inputnama: TEdit;
    ok: TImage;
    BGLOAD: TImage;
    load10: TImage;
    load25: TImage;
    load50: TImage;
    load75: TImage;
    load100: TImage;
    BGLOGO: TImage;
    BGMENU: TImage;
    BGNAMA: TImage;
    BGDIFF: TImage;
    easy: TImage;
    medium: TImage;
    hard: TImage;
    BGMAIN: TImage;
    skor: TEdit;
    domba: TImage;
    HI1: TImage;
    HI2: TImage;
    HI3: TImage;
    HK1: TImage;
    HK2: TImage;
    HK3: TImage;
    bom: TImage;
    rumput: TImage;
    TimerWaktu: TTimer;
    tiga: TImage;
    dua: TImage;
    satu: TImage;
    TimerRumput: TTimer;
    BGSKOR: TImage;
    SGSkor: TStringGrid;
    retry: TImage;
    exit2: TImage;
    TimerBom: TTimer;
    procedure TimerLoadTimer(Sender: TObject);
    procedure keteranganClick(Sender: TObject);
    procedure exitClick(Sender: TObject);
    procedure startClick(Sender: TObject);
    procedure okClick(Sender: TObject);
    procedure easyClick(Sender: TObject);
    procedure mediumClick(Sender: TObject);
    procedure hardClick(Sender: TObject);
    procedure TimerWaktuTimer(Sender: TObject);
    procedure TimerRumputTimer(Sender: TObject);
    procedure TimerBomTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure retryClick(Sender: TObject);
    procedure exit2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  loading: Integer;
  Names: array[1..1000] of string;
  Nilai: array[1..1000] of Integer;
  Score: Integer;
  Level: Integer;
  Hati: Integer;
  Waktu: Integer;

implementation
uses  KetUnit;

{$R *.dfm}

procedure TForm1.TimerLoadTimer(Sender: TObject);
begin
  loading := loading + 1;
  BGLOAD.Visible := true;
  if loading = 1 then
  begin
    load10.Visible := true;
  end;
  if loading = 3 then
  begin
    load10.Visible := false;
    load25.Visible := true;
  end;
  if loading = 5 then
  begin
    load25.Visible := false;
    load50.Visible := true;
  end;
  if loading = 7 then
  begin
    load50.Visible := false;
    load75.Visible := true;
  end;
  if loading = 9 then
  begin
    load75.Visible := false;
    load100.Visible := true;
  end;
  if loading = 11 then
  begin
    load100.Visible := false;
    BGLOAD.Visible := false;
    BGLOGO.Visible := true;
  end;
  if loading = 17 then
  begin
    BGLOGO.Visible := false;
    BGMENU.Visible := true;
    start.Visible := true;
    exit.Visible := true;
    keterangan.Visible := true;
  end;
end;

procedure TForm1.keteranganClick(Sender: TObject);
begin
  Form2.Show;
end;

procedure TForm1.exitClick(Sender: TObject);
begin
  if(Application.MessageBox('Are you sure you wanna quit the game?','Thank you for playing with us ^_^',MB_YesNo)=ID_Yes)then
  begin
    Application.Terminate;
  end;
end;


procedure TForm1.startClick(Sender: TObject);
begin
  BGMENU.Visible := false;
  start.Visible := false;
  exit.Visible := false;
  keterangan.Visible := false;
  BGNAMA.Visible := true;
  inputnama.Visible := true;
  ok.Visible := true;
end;

procedure TForm1.okClick(Sender: TObject);
var
  F, Fn: Textfile;
  n: Integer;
  nLine: string;

  procedure BacaNama(var F: Textfile);
  begin
    AssignFile(F, 'names.txt');
    Append(F);
    WriteLn(F, InputNama.Text);
    CloseFile(F);
  end;

  procedure TulisNama(var fn:textfile);
  begin
    AssignFile(Fn, 'names.txt');
    Reset(Fn);
    n := 0;
    while not Eof(Fn) do
    begin
      ReadLn(Fn, nLine);
      Inc(n);
      Names[n] := nLine;
    end;
    CloseFile(Fn);
  end;
begin
  Score := 0;
  Hati := 3;
  Waktu := 0;
  BacaNama(F);
  TulisNama(Fn);
  BGNAMA.Visible := false;
  inputnama.Visible := false;
  ok.Visible := false;
  BGDIFF.Visible := true;
  easy.Visible := true;
  medium.Visible := true;
  hard.Visible := true;
end;

procedure TForm1.easyClick(Sender: TObject);
begin
  BGDIFF.Visible := false;
  easy.Visible := false;
  medium.Visible := false;
  hard.Visible := false;
  Level := 5;
  rumput.left := random(336);
  bom.left := random(336);
  BGMAIN.Visible := true;
  domba.Visible := true;
  HI1.Visible := true;
  HI2.Visible := true;
  HI3.Visible := true;
  skor.Visible := true;
  skor.Text := '';
  TimerWaktu.Enabled := true;
end;

procedure TForm1.mediumClick(Sender: TObject);
begin
  BGDIFF.Visible := false;
  easy.Visible := false;
  medium.Visible := false;
  hard.Visible := false;
  level := 10;
  rumput.left := random(336);
  bom.left := random(336);
  BGMAIN.Visible := true;
  domba.Visible := true;
  HI1.Visible := true;
  HI2.Visible := true;
  HI3.Visible := true;
  skor.Visible := true;
  skor.Text := '';
  TimerWaktu.Enabled := true;
end;

procedure TForm1.hardClick(Sender: TObject);
begin
  BGDIFF.Visible := false;
  easy.Visible := false;
  medium.Visible := false;
  hard.Visible := false;
  level := 15;
  rumput.left := random(336);
  bom.left := random(336);
  BGMAIN.Visible := true;
  domba.Visible := true;
  HI1.Visible := true;
  HI2.Visible := true;
  HI3.Visible := true;
  skor.Visible := true;
  skor.Text := '';
  TimerWaktu.Enabled := true;
end;

procedure TForm1.TimerWaktuTimer(Sender: TObject);
begin
  Waktu := Waktu+1;
  if Waktu = 1 then
  begin
    tiga.Visible := true;
  end
  else if Waktu = 2 then
  begin
    tiga.Visible := false;
    dua.Visible := true;
  end
  else if Waktu = 3 then
  begin
    dua.Visible := false;
    satu.Visible := true;
  end
  else if Waktu = 4 then
  begin
    satu.Visible := false;
    TimerWaktu.Enabled := false;
    TimerRumput.Enabled := true;
  end;
end;

procedure TForm1.TimerRumputTimer(Sender: TObject);
var
  F: Textfile;
  i,j,k,n: integer;
  sline: integer;
  l: string;
begin
  rumput.Visible := true;
  bom.Visible := false;
  rumput.Top := rumput.Top + Level;
  if (rumput.Top+48 >= domba.top) and (rumput.Left >= domba.Left) and (rumput.Left+48 <= domba.Left+126) then
  begin
    Score := Score+1;
    skor.text := IntToStr(Score);
  end
  else if (rumput.Top+48 >= domba.top) then
  begin
    Hati := Hati-1;
  end;
  if Hati = 3 then
  begin
    HI1.Visible := true;
    HI2.Visible := true;
    HI3.Visible := true;
  end
  else if Hati = 2 then
  begin
    HI3.Visible := false;
    HK3.Visible := true;
  end
  else if Hati = 1 then
  begin
    HI2.Visible := false;
    HK2.Visible := true;
  end
  else if Hati = 0 then
  begin
    HI1.Visible := false;
    HK1.Visible := true;
    domba.Visible := false;
    skor.Visible := false;
    rumput.visible := false;
    TimerRumput.Enabled := false;
    AssignFile(F, 'number.txt');
    Append(F);
    WriteLn(F, score);
    reset(F);
    n := 0;
    while not eof(F) do
    begin
      readln(F, sline);
      inc(n);
      nilai[n] := sline;
    end;
    CloseFile(F);
    for i:=1 to n-1 do
    begin
      for j:=n downto i+1 do
      begin
        if nilai[j] > nilai[j-1] then
        begin
          k := nilai[j];
          nilai[j] := nilai[j-1];
          nilai[j-1] := k;
          l := names[j];
          names[j] := names[j-1];
          names[j-1] := l;
        end;
      end;
    end;
    for i:=1 to n do
    begin
    SGSkor.Cells[0,i-1] := IntToStr(i);
    SGSkor.Cells[1,i-1] := names[i];
    SGSkor.Cells[2,i-1] := IntToStr(nilai[i]);
    end;
    BGSkor.Visible := true;
    SGSkor.Visible := true;
    retry.Visible := true;
    exit2.Visible := true;
  end;
  if rumput.Top >= 368 then
  begin
    rumput.Visible := false;
    rumput.top := 0;
    if rumput.left mod 6 = 0 then
    begin
      bom.Left := random(336);
      TimerRumput.Enabled := false;
      TimerBom.Enabled := true;
    end
    else
    begin
      rumput.left := random(336);
    end;
  end;
end;

procedure TForm1.TimerBomTimer(Sender: TObject);
begin
  bom.visible := true;
  rumput.Visible := false;
  bom.Top := bom.Top + Level;
  if (bom.Top+48 >= domba.top) and (bom.Left >= domba.Left) and (bom.Left+48 <= domba.Left+126) then
  begin
    Hati := Hati-1;
  end;
  if Hati = 3 then
  begin
    HI1.Visible := true;
    HI2.Visible := true;
    HI3.Visible := true;
  end
  else if Hati = 2 then
  begin
    HI3.Visible := false;
    HK3.Visible := true;
  end
  else if Hati = 1 then
  begin
    HI2.Visible := false;
    HK2.Visible := true;
  end
  else if Hati = 0 then
  begin
    HI1.Visible := false;
    rumput.visible := false;
    HK1.Visible := true;
    TimerBom.Enabled := false;
  end;
  if bom.Top >= 368 then
  begin
    bom.Visible := false;
    bom.Top := 0;
    rumput.Left := random(336);
    if rumput.left mod 6 <> 0 then
    begin
      TimerBom.Enabled := false;
      TimerRumput.Enabled := true;
    end
    else
    begin
      bom.Left := random(336);
    end;
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_LEFT:
      domba.Left := domba.Left-8;
    VK_RIGHT:
      domba.Left := domba.Left+8;
  end;
end;

procedure TForm1.retryClick(Sender: TObject);
begin
  BGSKOR.Visible := false;
  SGSkor.Visible := false;
  retry.Visible := false;
  exit2.Visible := false;
  HK1.Visible := false;
  HK2.Visible := false;
  HK3.Visible := false;
  BGMAIN.Visible := false;
  BGNAMA.Visible := true;
  inputnama.Visible := true;
  inputnama.Text := '';
  ok.Visible := true;
end;

procedure TForm1.exit2Click(Sender: TObject);
begin
  if(Application.MessageBox('Are you sure you wanna quit the game?','Thank you for playing with us ^_^',MB_YesNo)=ID_Yes)then
  begin
    Application.Terminate;
  end;
end;

end.
