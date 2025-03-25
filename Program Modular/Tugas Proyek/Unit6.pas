unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls;

type
  TFormHelp = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHelp: TFormHelp;

implementation

uses Unit5;

{$R *.dfm}

procedure TFormHelp.Label1Click(Sender: TObject);
begin
  Form5.Show;
  FormHelp.Hide;
end;

end.
