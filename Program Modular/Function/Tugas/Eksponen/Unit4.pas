unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormEksponen = class(TForm)
    Label1: TLabel;
    EditX: TEdit;
    Button1: TButton;
    Label2: TLabel;
    EditExp: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEksponen: TFormEksponen;

implementation

{$R *.dfm}

procedure TFormEksponen.Button1Click(Sender: TObject);
var
   x: real;
     function Faktorial(n: integer): real;
     var
         k: integer;
         fak: real;
     begin
         fak:=1;
         for k:= 1 to n do
             fak:= fak*k;
         Faktorial:=fak;
         end;
     function Pangkat(x: real; var n: integer): real;
     var
         m: integer;
         p: real;
     begin
         p:=1;
         for m:=1 to n do
             p:=p*x;
         Pangkat:=p;
         end;
     function Eksponen(x:real): real;
     var
        n: integer;
        S: real;
      begin
        S:=0;
        for n:=0 to 99 do
            S:= Pangkat(x,n)/Faktorial(n)+S;
        Eksponen:=S;
        end;
begin
     x:=StrToFloat(EditX.Text);
     EditExp.Text:=FloatToStr(Eksponen(x));

end;

procedure TFormEksponen.Button2Click(Sender: TObject);
begin
  application.terminate
end;

end.
