unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    SG1: TStringGrid;
    Label3: TLabel;
    Label4: TLabel;
    EditNBaris: TEdit;
    EditNKolom: TEdit;
    EditBarisKe: TEdit;
    EditKolomKe: TEdit;
    ButtonTampil: TButton;
    EditElemen: TEdit;
    procedure ButtonTampilClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonTampilClick(Sender: TObject);
  const
    NBarisMaks = 5;
    NKolomMaks = 5;
  type
    Matriks = array[1..NBarisMaks, 1..NKolomMaks] of integer;
  var
    M: Matriks;
    NBaris,NKolom: integer;
procedure BacaMatriks(NBaris,NKolom: integer; Var M: Matriks);
  var
    i,j: integer;
  begin
    for i:=1 to NBaris do
      for j:=1 to NKolom do
        M[i,j]:=StrToInt(SG1.Cells[j-1,i-1]);
      end;
procedure TampilElemen(M:Matriks);
  var
    BarisKe,KolomKe: integer;
  begin
    BarisKe:=StrToInt(EditBarisKe.Text);
    KolomKe:=StrToInt(EditKolomKe.Text);
    EditElemen.Text:=IntToStr(M[BarisKe,KolomKe]);
  end;
  begin
    NBaris:=StrToInt(EditNBaris.Text);
    NKolom:=StrToInt(EditNKolom.Text);
    BacaMatriks(NBaris,NKolom,M);
    TampilElemen(M);
  end;

end.