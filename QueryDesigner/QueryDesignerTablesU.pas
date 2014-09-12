unit QueryDesignerTablesU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CheckLst,strutils;

type

  { TQueryDesignerTables }

  TQueryDesignerTables = class(TForm)
   btnAccept: TButton;
   btnCancel: TButton;
   chkTables: TCheckListBox;
   txtSearchTable: TEdit;
   procedure txtSearchTableChange(Sender: TObject);
   procedure txtSearchTableEnter(Sender: TObject);
   procedure txtSearchTableExit(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  QueryDesignerTables: TQueryDesignerTables;

implementation

{$R *.lfm}

{ TQueryDesignerTables }

procedure TQueryDesignerTables.txtSearchTableChange(Sender: TObject);
var
  s:string;
  I: Integer;
begin
  for I:=0 to chkTables.Count-1 do
  begin
    if AnsiContainsText(chkTables.Items[I],txtSearchTable.Text) then
    begin
      chkTables.ItemIndex:=I;
      Break;
    end;
  end;
end;

procedure TQueryDesignerTables.txtSearchTableEnter(Sender: TObject);
begin
 txtSearchTable.Clear;
  txtSearchTable.Font.Color:= clWindowText;
end;

procedure TQueryDesignerTables.txtSearchTableExit(Sender: TObject);
begin
 txtSearchTable.Font.Color:= clSilver;
  txtSearchTable.Text:='Search Table';
end;

end.

