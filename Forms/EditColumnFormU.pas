{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit EditColumnFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Spin, AsDbType,LazSqlXResources;

type

  { TEditColumnForm }

  TEditColumnForm = class(TForm)
    btnApply: TBitBtn;
    btnClose: TBitBtn;
    chkIdentity: TCheckBox;
    chkPrimaryKey: TCheckBox;
    chkAllowNull: TCheckBox;
    cmbDataTypes: TComboBox;
    lblLength: TLabel;
    lblColumnType: TLabel;
    lblPrecision: TLabel;
    txtColumnName: TEdit;
    lblColumnName: TLabel;
    pnlMain: TPanel;
    txtLength: TSpinEdit;
    txtPrecision: TSpinEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure cmbDataTypesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDbType: TAsDatabaseType;
    FFillDataTypesOnShow: Boolean;
    FTablename: string;
    FValidate: Boolean;
    function GetAllowNull: Boolean;
    function GetColumnName: string;
    function GetDataType: string;
    function GetIsAutonumber: Boolean;
    function GetIsPrimaryKey: Boolean;
    function GetLength: Integer;
    function GetPrecision: integer;
    procedure SetDbType(AValue: TAsDatabaseType);
    procedure SetFillDataTypesOnShow(AValue: Boolean);
    procedure SetTablename(AValue: string);
    procedure SetValidate(AValue: Boolean);
    procedure UpdateGUI;

    { private declarations }
  public
    { public declarations }
    property DbType:TAsDatabaseType read FDbType write SetDbType;
    property IsPrimaryKey:Boolean read GetIsPrimaryKey;
    property ColumnName:string read GetColumnName;
    property DataType:string read GetDataType;
    property Precision:integer read GetPrecision;
    property Length:Integer read GetLength;
    property AllowNull:Boolean read GetAllowNull;
    function ShowModal(aDbtype:TAsDatabaseType):TModalResult;
    function Validate:Boolean;
    procedure ClearInputs;
    procedure FillDataTypes;
    property Autonumber:Boolean read GetIsAutonumber;
    property FillDataTypesOnShow:Boolean read FFillDataTypesOnShow write SetFillDataTypesOnShow;
    property Tablename:string read FTablename write SetTablename;
  end;

var
  EditColumnForm: TEditColumnForm;

implementation


{$R *.lfm}

{ TEditColumnForm }

procedure TEditColumnForm.cmbDataTypesChange(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TEditColumnForm.btnApplyClick(Sender: TObject);
begin

end;

procedure TEditColumnForm.FormShow(Sender: TObject);
begin
    if FDbType = dtSQLite then
    begin
      lblLength.Visible:=False;
      lblPrecision.Visible:=False;
      txtLength.Visible:=False;
      txtPrecision.Visible:=False;
    end;
    chkIdentity.Checked:=False;
    chkIdentity.Visible:=False;
    UpdateGUI;
end;

function TEditColumnForm.GetAllowNull: Boolean;
begin
  Result := chkAllowNull.Checked;
end;

function TEditColumnForm.GetColumnName: string;
begin
  Result := txtColumnName.Text;
end;

function TEditColumnForm.GetDataType: string;
begin
  Result := cmbDataTypes.Text;
end;

function TEditColumnForm.GetIsAutonumber: Boolean;
begin
  Result :=chkIdentity.Checked;
end;

function TEditColumnForm.GetIsPrimaryKey: Boolean;
begin
  Result := chkPrimaryKey.Checked;
end;

function TEditColumnForm.GetLength: Integer;
begin
  Result := txtLength.Value;
end;

function TEditColumnForm.GetPrecision: integer;
begin
  Result := txtPrecision.Value;
end;

procedure TEditColumnForm.SetDbType(AValue: TAsDatabaseType);
begin
  if FDbType=AValue then Exit;
  FDbType:=AValue;
end;

procedure TEditColumnForm.SetFillDataTypesOnShow(AValue: Boolean);
begin
  if FFillDataTypesOnShow=AValue then Exit;
  FFillDataTypesOnShow:=AValue;
end;

procedure TEditColumnForm.SetTablename(AValue: string);
begin
  if FTablename=AValue then Exit;
  FTablename:=AValue;
end;

procedure TEditColumnForm.SetValidate(AValue: Boolean);
begin
  if FValidate=AValue then Exit;
  FValidate:=AValue;
end;

procedure TEditColumnForm.UpdateGUI;
var
 dt: string;
begin

   lblPrecision.Visible:=False;
   txtPrecision.Visible:=False;
   lblLength.Visible:=False;
   txtLength.Visible:=False;


   dt := lowercase(cmbDataTypes.Text);
   if (dt='varchar') or (dt='nvarchar') or (dt='varchar2') or (dt='nvarchar2')
    or (dt='character varying')
   or (dt='character') then
   begin
     lblLength.Visible:=True;
     txtLength.Visible:=True;
   end else
   if (dt='decimal') or (dt='float') or (dt='numeric') then
   begin
       lblPrecision.Visible:=True;
       txtPrecision.Visible:=True;
       lblLength.Visible:=True;
       txtLength.Visible:=True;
   end;

 if FDbType<> dtSQLite then
 begin
   chkIdentity.Visible := (dt='int') or (dt='numeric');
   chkIdentity.Visible:=False;
 end;

 if DbType=dtOracle then
 begin
   chkIdentity.Caption:='CREATE '+FTablename+'_SEQ (1,1)';
 end else
 begin
   chkIdentity.Caption:='AUTOINC';
 end;
end;

function TEditColumnForm.ShowModal(aDbtype: TAsDatabaseType): TModalResult;
begin
  FDbType:=aDbtype;
  if FFillDataTypesOnShow then
  FillDataTypes;
  Result := inherited ShowModal;
end;

function TEditColumnForm.Validate: Boolean;
begin
  Result := (cmbDataTypes.ItemIndex>-1) and (Trim(txtColumnName.Text)<>EmptyStr);
end;

procedure TEditColumnForm.ClearInputs;
begin
  txtColumnName.Clear;
  chkPrimaryKey.Checked:=False;
  chkAllowNull.Checked:=True;
  cmbDataTypes.ItemIndex:=-1;
  txtLength.Value:=50;
  txtPrecision.Value:=50;
end;

procedure TEditColumnForm.FillDataTypes;
begin
  cmbDataTypes.Clear;
  case FDbType of
   dtMsSql:cmbDataTypes.Items.AddStrings(TLazSqlXResources.MsSqlDataTypes);
   dtOracle:cmbDataTypes.Items.AddStrings(TLazSqlXResources.OracleDataTypes);
   dtMySql:cmbDataTypes.Items.AddStrings(TLazSqlXResources.MySqlDataTypes);
   dtSQLite:cmbDataTypes.Items.AddStrings(TLazSqlXResources.SqliteDataTypes);
   dtFirebirdd:cmbDataTypes.Items.AddStrings(TLazSqlXResources.FirebirdDataTypes);
   dtPostgreSql:cmbDataTypes.Items.AddStrings(TLazSqlXResources.PostgreSqlDataTypes);
  end;
end;

end.

