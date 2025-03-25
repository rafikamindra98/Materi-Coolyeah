unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls;

type
  TForm5 = class(TForm)
    Easy: TLabel;
    Image1: TImage;
    Hard: TLabel;
    Back: TLabel;
    Help: TLabel;
    procedure EasyClick(Sender: TObject);
    procedure HardClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses Unit4, Unit3, Unit2, Unit6;

{$R *.dfm}

procedure TForm5.EasyClick(Sender: TObject);
begin
  Form4.Show;
  Form5.Hide;
end;

procedure TForm5.HardClick(Sender: TObject);
begin
  FormUtama.Show;
  Form5.Hide;
end;

procedure TForm5.BackClick(Sender: TObject);
begin
  FormMulai.Show;
  Form5.Hide;
end;
procedure TForm5.HelpClick(Sender: TObject);
begin
  FormHelp.Show;
  Form5.Hide;
end;

end.
