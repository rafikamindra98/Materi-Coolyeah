unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg;

type
  TForm4 = class(TForm)
    Panel1: TPanel;
    lblWaktu: TLabel;
    lblSkor: TLabel;
    lbl60: TLabel;
    lbl0: TLabel;
    lblDetik: TLabel;
    btnMain: TButton;
    Ikan: TImage;
    Kucing: TImage;
    ikanTime: TTimer;
    waktuTime: TTimer;
    Keluar: TButton;
    Image1: TImage;
    procedure onMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ikanTimeTimer(Sender: TObject);
    procedure waktuTimeTimer(Sender: TObject);
    procedure btnMainClick(Sender: TObject);
    procedure KeluarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;
  Overlay: TRect;

implementation

uses Unit5;

{$R *.dfm}

procedure TForm4.onMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Kucing.Left := X-Kucing.Width div 5;
  panel1.Cursor := crHelp;
end;

procedure TForm4.ikanTimeTimer(Sender: TObject);
begin
  Ikan.Visible := true;
  Ikan.Top := Ikan.Top+15;
  if Ikan.Top > panel1.Height then
  begin
    Ikan.Top := 0;
    Ikan.Left := random(panel1.Width);
  end;
  if intersectRect(Overlay, Ikan.BoundsRect, Kucing.BoundsRect) then
  begin
    Ikan.Visible := false;
    Ikan.Top := 0;
    lbl0.Tag := lbl0.Tag+1;
    Ikan.Left := random(panel1.Width);
    lbl0.Caption := IntToStr(lbl0.Tag);
  end;
end;

procedure TForm4.waktuTimeTimer(Sender: TObject);
begin
  lbl60.Tag := lbl60.Tag-1;
  lbl60.Caption := IntToStr(lbl60.Tag);
  if lbl60.Tag = 0 then
  begin
    btnMain.Enabled := true;
    waktuTime.Enabled := false;
    panel1.Enabled := false;
    ikanTime.Enabled := false;
    Kucing.Visible := false;
    Ikan.Visible := false;
    panel1.Caption:= 'Click Play, if you want to play again!';
  end;
end;

procedure TForm4.btnMainClick(Sender: TObject);
begin
  btnMain.Caption := 'Play Again?';
  btnMain.Enabled := false;
  panel1.Enabled := true;
  ikanTime.Enabled := true;
  waktuTime.Enabled := true;
  Ikan.Visible := true;
  Kucing.Visible := true;
  panel1.Caption := '';
  Ikan.Top := 0;
  lbl60.Caption := '60';
  lbl60.Tag := 60;
  lbl0.Caption := '0';
  lbl0.Tag := 0;
end;

procedure TForm4.KeluarClick(Sender: TObject);
begin
  Form5.Show;
  Form4.Hide;
end;

end.
