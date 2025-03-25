unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    EditInput: TEdit;
    ButtonInput: TButton;
    ListBox1: TListBox;
    EditDataKe: TEdit;
    ButtonDataKe: TButton;
    EditTampilDataKe: TEdit;
    DataKe: TLabel;
    procedure ButtonInputClick(Sender: TObject);
    procedure ButtonDataKeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonInputClick(Sender: TObject);
begin
  ListBox1.Items.Add(EditInput.Text);
  EditInput.Text:='';
end;

procedure TForm1.ButtonDataKeClick(Sender: TObject);
 type
    ArrInt=array[1..100] of integer; 
var
  k,N:integer;
  A:ArrInt;
 
  procedure BacaData(N:integer;var A:ArrInt);
  var
    i:integer;
  begin
    for i:=1 to N do
    begin
      A[i]:=StrToInt(ListBox1.Items[i]);
    end;
  end;

begin
  N:=ListBox1.Count;
  BacaData(N,A);
  k:=StrToInt(EditDataKe.Text);
  EditTampilDataKe.Text:=StrToInt(A[k]);
end;

end.
