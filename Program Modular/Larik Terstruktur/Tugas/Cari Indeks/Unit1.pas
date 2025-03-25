unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    EditKalimat: TEdit;
    Label2: TLabel;
    EditHuruf: TEdit;
    Button1: TButton;
    EditIndeks: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
    const NMaks = 20;
    type
        ArrChar = array[1..NMaks] of char;
    var
        S: ArrChar;
        M: integer;
        N: string;

    procedure BacaKata(M: integer; var S:ArrChar);
    var i: integer;
    begin
        for i:= 1 to M do
        begin
            S[i]:=EditKalimat.Text[i];
        end;
     end;

    function CariHuruf (S: ArrChar; M: integer; N: string): integer;
    var i, u: integer;
    begin
        u:=0;
        for i:=1 to M do
        begin
            if N = S[i] then
            begin
               u := i;
            end;
        end;
        CariHuruf:=u;
    end;

begin
M:=Length(EditKalimat.Text);
BacaKata(M,S);
N:=EditHuruf.Text;
EditIndeks.Text:=IntToStr(CariHuruf(S,M,N));

end;
end.
