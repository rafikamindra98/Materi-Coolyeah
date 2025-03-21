unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
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
var n:integer;
  function Genap(n:integer):boolean;
begin
  Genap:=(n mod 2 = 0);
end;
begin
  n:=StrToInt(Edit1.Text);
  if Genap(n)
  then Edit2.Text:='Genap'
  else Edit2.Text:='Ganjil';
end;

end.
