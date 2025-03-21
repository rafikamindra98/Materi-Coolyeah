unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
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
  procedure TambahEmpat(var x,y:integer);
  begin
    x := x+4;
    y := y+4;
    x := strtoint(edit3.text);
    y := strtoint(edit4.text);
  end;

  var a,b: integer;
  procedure TambahEmpat(var x,y:integer);
  BEGIN
  a := 15;
  b := 10;
  edit1.text := inttostr(a);
  edit2.text := inttostr(b);

  TambahEmpat(a,b);
  edit3.text := inttostr(a);
  edit4.text := inttostr(b);
  END;

end;

end.
