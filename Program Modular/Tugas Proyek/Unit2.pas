unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls;

type
  TFormMulai = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Keluar: TLabel;
    procedure Label1Click(Sender: TObject);
    procedure KeluarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMulai: TFormMulai;

implementation

uses Unit3, Unit5;

{$R *.dfm}

procedure TFormMulai.Label1Click(Sender: TObject);
begin
  Form5.Show;
  FormMulai.Hide;
end;

procedure TFormMulai.KeluarClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
