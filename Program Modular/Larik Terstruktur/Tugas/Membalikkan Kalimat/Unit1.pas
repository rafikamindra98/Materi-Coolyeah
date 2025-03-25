unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    EditKalimat: TEdit;
    ButtonEnkripsi: TButton;
    EditEnkripsi: TEdit;
    procedure ButtonEnkripsiClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonEnkripsiClick(Sender: TObject);
var
   m: string;

   procedure Balikkan;
   var
      s: string;
      j: integer;
   begin
      for j:=Length(m) downto 1 do
      begin
          s:= s + m[j];
      end;
      EditEnkripsi.Text:=s;
   end;

begin
   m:=EditKalimat.Text;
   Balikkan;
end;

end.
