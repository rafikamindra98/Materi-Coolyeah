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
    Cari: TButton;
    Label1: TLabel;
    EditHasilCari: TEdit;
    EditCari: TEdit;
    procedure ButtonInputClick(Sender: TObject);
    procedure CariClick(Sender: TObject);
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

procedure TForm1.CariClick(Sender: TObject);
type
  ArrInt=Array[1..100] of integer;
var
  n,x:integer;
  L:Arrint;
  ketemu:boolean;

  procedure BacaData (Var L:ArrInt; n:integer);
  var
    i:integer;
  begin
    for i:=1 to n do
    begin
      L[i]:=StrToInt(ListBox1.Items[i-1]);
    end
  end;

  procedure BubleSort(var L:ArrInt; n:integer);
  var
    i,k:integer;
    procedure Tukar(var a:integer; var b:integer);
    var
      c:integer;
    begin
      c:=a;
      a:=b;
      b:=c;
    end;

  begin
    for i:=1 to n-1 do
      begin
      for k:=n downto i+1 do
        if L[k]<L[k-1] then
          Tukar(L[k], L[k-1]);
      end
  end;

  procedure TulisData(L:ArrInt; n:integer);
  var
    i:integer;
  begin
    for i:=1 to n do
    ListBox1.Items[i-1]:=IntToStr(L[i]);
  end;

  procedure PencarianBagiDua(L:ArrInt; n:integer; x:integer; var ketemu:boolean);
  var
    i,j,k:integer;
  begin
    i:=1;
    j:=n;
    ketemu:=false;
    while (not ketemu) and (i<=j) do
    begin
      k:=(i+j) div 2;
      if (L[k]=x) then
        begin
          ketemu:=true
        end
      else
        if (L[k]>x) then
          begin
            j:=k-1
          end
        else
          i:=k+1;
    end;
  end;

  procedure SeqSearch(L:ArrInt; n:integer; x:integer; Var ketemu:boolean);
  var
    i: integer;
  begin
    i:=1;
    while (i<n) and (L[i]<>x) do
      i:=i+1;
    if L[i]=x then
      ketemu:=true
    else
      ketemu:=false;
    end;

  procedure TampilHasil(ketemu:boolean);
  begin
    if ketemu then
      EditHasilCari.Text:='Ketemu'
    else
      EditHasilCari.Text:='Tidak Ketemu';
  end;

  BEGIN
  n:=ListBox1.Count;
  BacaData(L,n);
  BubleSort(L,n);
  ListBox1.Clear;
  TulisData(L,n);
  PencarianBagiDua(L,n,x,ketemu);
  x:=StrToInt(EditCari.Text);
  SeqSearch(L,n,x,ketemu);
  TampilHasil(ketemu);
  END;

end.
