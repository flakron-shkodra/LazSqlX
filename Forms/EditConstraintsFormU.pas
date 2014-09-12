{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit EditConstraintsFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, DbType, TableInfo,ZConnection;

type

  { TEditConstraintForm }

  TEditConstraintForm = class(TForm)
    btnApply: TBitBtn;
    btnClose: TBitBtn;
    cmbColumns: TComboBox;
    cmbReferenceColumns: TComboBox;
    cmbReferenceTables: TComboBox;
    lblReferenceColumn: TLabel;
    lblTablename: TLabel;
    lblColumnName: TLabel;
    pnlMain: TPanel;
    procedure cmbReferenceColumnsChange(Sender: TObject);
    procedure cmbReferenceTablesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtConstraintNameChange(Sender: TObject);
  private
    { private declarations }
    _DBType:TDatabaseType;
    _Con:TZConnection;
    _Tablename:string;
    _WorkingTable:TTableInfo;
    _Schema:string;
    function GetForeignColumn: string;
    function GetForeignTable: string;
    function GetLocalColumn: string;
    procedure FillColumns;
    procedure FillRefTables;
    procedure FillRefColumns(tblName:string);
    function GetReferenceName: string;
  public
    { public declarations }
    function ShowModal(aCon:TZConnection; Schema:string; WorkingTable:TTableInfo):TModalResult;
    property LocalColumn:string read GetLocalColumn;
    property ReferencedTable:string read GetForeignTable;
    property ReferencedColumn:string read GetForeignColumn;
  end;

var
  EditConstraintForm: TEditConstraintForm;

implementation

uses AsStringUtils;

{$R *.lfm}

{ TEditConstraintForm }

procedure TEditConstraintForm.FormShow(Sender: TObject);
begin
  FillColumns;
  FillRefTables;
end;

procedure TEditConstraintForm.txtConstraintNameChange(Sender: TObject);
begin

end;

procedure TEditConstraintForm.cmbReferenceTablesChange(Sender: TObject);
begin
  FillRefColumns(cmbReferenceTables.Items[cmbReferenceTables.ItemIndex]);
end;

procedure TEditConstraintForm.cmbReferenceColumnsChange(Sender: TObject);
begin

end;

function TEditConstraintForm.GetForeignColumn: string;
begin
  Result := cmbReferenceColumns.Items[cmbReferenceColumns.ItemIndex];
end;

function TEditConstraintForm.GetForeignTable: string;
begin
  Result := cmbReferenceTables.Items[cmbReferenceTables.ItemIndex];
end;

function TEditConstraintForm.GetLocalColumn: string;
begin
  Result := cmbColumns.Items[cmbColumns.ItemIndex];
end;

procedure TEditConstraintForm.FillColumns;
var
  lst:TStringList;
  I: Integer;
begin
  cmbColumns.Clear;

  for I:=0 to _WorkingTable.AllFields.Count-1 do
  begin
    cmbColumns.Items.Add(_WorkingTable.AllFields[I].FieldName);
  end;

end;

procedure TEditConstraintForm.FillRefTables;
var
  lst:TStringList;
  I: Integer;
begin
  cmbReferenceTables.Clear;
  lst := TStringList.Create;
  try
    _Con.GetTableNames(_Schema,'',lst);
    for I:=0 to lst.Count-1 do
    begin
      cmbReferenceTables.Items.Add(lst[I]);
    end;
  finally
    lst.Free;
  end;
end;

procedure TEditConstraintForm.FillRefColumns(tblName: string);
var
  lst:TStringList;
  I: Integer;
begin
  cmbReferenceColumns.Clear;
  lst := TStringList.Create;
  try
    _Con.GetColumnNames(tblName,'',lst);
    for I:=0 to lst.Count-1 do
    begin
      cmbReferenceColumns.Items.Add(lst[I]);
    end;
  finally
    lst.Free;
  end;
end;

function TEditConstraintForm.GetReferenceName: string;
begin

end;

function TEditConstraintForm.ShowModal(aCon: TZConnection; Schema: string;
 WorkingTable: TTableInfo): TModalResult;
begin
  _Con := aCon;
  if _Con.Connected then
  _Con.Disconnect;

  _Con.Connect;

  _Schema:=Schema;
  _DBType:=TDbUtils.DatabaseTypeFromString(_Con.Protocol);
  _WorkingTable := WorkingTable;
  _Tablename:= WorkingTable.Tablename;
  Result := inherited ShowModal;
end;

end.

