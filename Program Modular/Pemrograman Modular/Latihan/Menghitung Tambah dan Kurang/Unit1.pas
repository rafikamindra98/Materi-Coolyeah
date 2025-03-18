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
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label5: TLabel;
    Edit4: TEdit;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  var x,y,z: integer;
begin
  //Menyimpan bilangan dari EditX dan EditY
  //ke dalam x dan y
  x := StrToInt(Edit1.Text);
  y := StrToInt(Edit2.Text);
  
  //Operasi jumlah
  z := x + y;
  //Menampilkan hasil operasi Tambah
  Edit3.Text := IntToStr(z);

  //Operasi kurang
  z := x - y;
  //Menampilkan hasil operasi Kurang
  Edit4.Text := IntToStr(z);

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  application.terminate
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Edit1.Text:='';
  Edit2.Text:='';
  Edit3.Text:='';
  Edit4.Text:='';

end;

end.
