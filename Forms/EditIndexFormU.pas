{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit EditIndexFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, AsTableInfo;

type

  { TEditIndexForm }

  TEditIndexForm = class(TForm)
   btnApply: TBitBtn;
   btnClose: TBitBtn;
   chkUnique: TCheckBox;
   cmbSortOrder: TComboBox;
   cmbColumns: TComboBox;
   lblOrder: TLabel;
   lblName: TLabel;
   lblColumn: TLabel;
   pnlMain: TPanel;
   txtName: TEdit;
   procedure FormCreate(Sender: TObject);
   procedure FormShow(Sender: TObject);
  private
   _Table:TAsTableInfo;
   function GetColumn: string;
   function GetIndexName: string;
   function GetSortOrder: string;
   function GetUnique: Boolean;
    { private declarations }
    procedure FillColumns;
  public
    { public declarations }
   property SortOrder:string read GetSortOrder;
   property IndexName:string read GetIndexName;
   property Unique:Boolean read GetUnique;
   property Column:string read GetColumn;
   function ShowModal(table:TAsTableInfo):TModalResult;
  end;

var
  EditIndexForm: TEditIndexForm;

implementation

{$R *.lfm}

{ TEditIndexForm }

procedure TEditIndexForm.FormCreate(Sender: TObject);
begin

end;

procedure TEditIndexForm.FormShow(Sender: TObject);
begin
 txtName.Clear;
 cmbSortOrder.ItemIndex:=0;
 FillColumns;
end;

function TEditIndexForm.GetIndexName: string;
begin
 Result:=txtName.Text;
end;

function TEditIndexForm.GetColumn: string;
begin
  Result:=cmbColumns.Text;
end;

function TEditIndexForm.GetSortOrder: string;
begin
 Result := cmbSortOrder.Text;
end;

function TEditIndexForm.GetUnique: Boolean;
begin
 Result := chkUnique.Checked;
end;

procedure TEditIndexForm.FillColumns;
var
 I: Integer;
begin
  cmbColumns.Clear;
  for I:=0 to _Table.AllFields.Count-1 do
  begin
    cmbColumns.Items.Add(_Table.AllFields[I].FieldName);
  end;
end;

function TEditIndexForm.ShowModal(table: TAsTableInfo): TModalResult;
begin
 _Table:=table;
 inherited ShowModal;
end;

end.

