unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls;

type
  TFormPerkenalan = class(TForm)
    Image1: TImage;
    Next: TLabel;
    procedure NextClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPerkenalan: TFormPerkenalan;

implementation

uses Unit2;

{$R *.dfm}

procedure TFormPerkenalan.NextClick(Sender: TObject);
begin
  FormMulai.Show;
  FormPerkenalan.Hide;

end;

end.
