unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Label3: TLabel;
    Edit3: TEdit;
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
  var
    a,t:integer;
    l:real;
    procedure MenghitungLuasSegitiga(alas,tinggi:integer; var luas:real);
    begin
      luas:=(alas*tinggi)/2;
    end;

    begin
      a:=StrToInt(Edit1.Text);
      t:=StrToInt(Edit2.Text);
      MenghitungLuasSegitiga(a,t,l);
      Edit3.Text:=FloatToStr(l);
end;

end.
