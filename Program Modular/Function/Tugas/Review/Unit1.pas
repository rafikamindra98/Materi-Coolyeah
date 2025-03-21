unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Editx: TEdit;
    ButtonProses: TButton;
    Editeksp: TEdit;
    procedure ButtonProsesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonProsesClick(Sender: TObject);
var
  x:real;
  function Eksp(x:real):real;
  const
    n=5;
  var
    S:real;
    k:integer;
    function Fak(n:integer):integer;
    var
      i,f:integer;
    begin
      f:=1;
      for i:= 0 to n do
         if i=0 then
           f:=1
         else
           f:=f*i;
      Fak:=f;
    end;
    function Pangkat(x:real;n:integer):real;
    var
        i:integer;
        p:real;
      begin
        p:=1;
        for i:=0 to n do
          if i=0 then
              p:=1
          else
              p:=p*x;
      Pangkat:=p;
      end;
  begin
    S:=0 ;
    for k:=0 to n do
      S:=S+Pangkat(x,n)/Fak(n);
      Eksp:=S;
  end;
BEGIN
  x:=StrToFloat(Editx.Text);
  Editeksp.Text:=FloatToStr(Eksp(x));
END;

end.
