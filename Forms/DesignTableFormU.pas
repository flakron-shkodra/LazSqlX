{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  mod 2013
  *******************************************************************
}
unit DesignTableFormU;

{$mode objfpc}{$H+}
interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, AsTableInfo, AsSqlGenerator, ExtCtrls, Buttons, ActnList,
  Menus, DB, strutils, AsDbType;

type

  { TDesignTableForm }

  TDesignTableForm = class(TForm)
    actEditColumn: TAction;
    actDropColumn: TAction;
    actCreateTable: TAction;
    actDropConstraint: TAction;
    actCreateIndex: TAction;
    actDropIndex: TAction;
    actNewConstraint: TAction;
    actNewColumn: TAction;
    EditActions: TActionList;
    btnApply: TBitBtn;
    btnClose: TBitBtn;
    EditConstraintsMenu: TPopupMenu;
    EditIndexes: TPopupMenu;
    lsvTriggers: TListView;
    lsvIndexes: TListView;
    mitDropColumn: TMenuItem;
    mitDropColumn1: TMenuItem;
    mitDropColumn2: TMenuItem;
    mitEditColumn: TMenuItem;
    mtiNewColumn: TMenuItem;
    mtiNewColumn1: TMenuItem;
    mtiNewColumn2: TMenuItem;
    pgcMain: TPageControl;
    EditColumnMenu: TPopupMenu;
    tabDependencies: TTabSheet;
    tabFields: TTabSheet;
    tabIndexes: TTabSheet;
    tabTriggers: TTabSheet;
    lsvFields: TListView;
    lsvDependencies: TListView;
    tbtnDrop1: TToolButton;
    tbtnDrop2: TToolButton;
    tbtnDrop3: TToolButton;
    tbtnNew1: TToolButton;
    tbtnNew2: TToolButton;
    tbtnNew3: TToolButton;
    tlbFieldEdit: TToolBar;
    tbtnNew: TToolButton;
    tbtnEdit: TToolButton;
    tbtnDrop: TToolButton;
    lblTablename: TLabel;
    ilTableInfoImages: TImageList;
    imgTable: TImage;
    tlbFieldEdit1: TToolBar;
    tlbFieldEdit2: TToolBar;
    tlbFieldEdit3: TToolBar;
    procedure actCreateIndexExecute(Sender: TObject);
    procedure actCreateTableExecute(Sender: TObject);
    procedure actDropColumnExecute(Sender: TObject);
    procedure actDropConstraintExecute(Sender: TObject);
    procedure actDropIndexExecute(Sender: TObject);
    procedure actEditColumnExecute(Sender: TObject);
    procedure actNewColumnExecute(Sender: TObject);
    procedure actNewConstraintExecute(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    FCreateNew: boolean;
    { Private declarations }
    FSchema: string;
    FTablename: string;
    FWorkingTableInfo: TAsTableInfo;
    FDbInfo:TAsDbConnectionInfo;
    FTableInfos:TAsTableInfos;
    function GetCreateTableSQL: string;
    procedure SetCreateNew(AValue: boolean);
    procedure ExecuteQuery(Query: string);
  public

    procedure PopulateInfo;
    function Showmodal(DbInfo:TAsDbConnectionInfo; Schema: string; Tablename: string): integer;
  end;

const
  LoopSeperator: array [0 .. 1] of char = (' ', ',');

var
  DesignTableForm: TDesignTableForm;

implementation

{$R *.lfm}

uses MainFormU, EditColumnFormU, EditConstraintsFormU, EditIndexFormU,
 AsDatabaseCloner;

{ TDesignTableForm }


procedure TDesignTableForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDesignTableForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if FTableInfos<>nil then
  FTableInfos.Free;
end;

procedure TDesignTableForm.actEditColumnExecute(Sender: TObject);
var
  sql: string;
  cn: string;
  dt: string;
  i: Integer;
  fi:TAsFieldInfo;
  pk: TAsFieldInfo;

begin

  if lsvFields.Selected <> nil then
  begin
    cn := lsvFields.Selected.Caption;
    EditColumnForm.txtColumnName.Text := cn;
    EditColumnForm.txtLength.Value:= StrToint(lsvFields.Selected.SubItems[1]);
    EditColumnForm.txtPrecision.Value:=0;
    dt := lsvFields.Selected.SubItems[0];
    EditColumnForm.DbType:=FDbInfo.DbType;
    EditColumnForm.FillDataTypes;
    i:=EditColumnForm.cmbDataTypes.Items.IndexOf(dt);
    EditColumnForm.cmbDataTypes.ItemIndex:=i;
    EditColumnForm.FillDataTypesOnShow:=False;

    try
      EditColumnForm.chkAllowNull.Checked:= FWorkingTableInfo.FieldByName(cn).AllowNull;
      EditColumnForm.chkPrimaryKey.Checked:= FWorkingTableInfo.FieldByName(cn).IsPrimaryKey;
    except
    end;


    if (EditColumnForm.ShowModal(FDbInfo.DbType) =
      mrOk) and (EditColumnForm.Validate) then
    begin
      cn := EditColumnForm.ColumnName;
      case FDbInfo.DbType of
        dtMsSql: cn := ' ALTER COLUMN ' + cn;
        dtOracle: cn := ' MODIFY ' + cn;
        dtMySql: cn := ' MODIFY ' + cn;
        dtFirebirdd : cn:=' ALTER COLUMN ' + cn +' TYPE ';
      end;

      sql := ' ALTER TABLE ' + FTablename + ' ' + cn + ' ' + EditColumnForm.DataType;

      dt := lowercase(EditColumnForm.DataType);

      if (dt = 'varchar') or (dt = 'nvarchar') or (dt = 'varchar2') then
      begin
        sql := sql + '(' + IntToStr(EditColumnForm.Length) + ')';
      end
      else
      if (dt = 'decimal') then
      begin
        sql := sql + '(' + IntToStr(EditColumnForm.Length) + ',' +
          IntToStr(EditColumnForm.Precision) + ')';
      end;

      if not EditColumnForm.AllowNull then
      begin
        sql := sql + ' NOT NULL';
      end;

      try
        if not FCreateNew then
        begin
          ExecuteQuery(sql);
        end
        else
        begin
          fi := FWorkingTableInfo.FieldByName(lsvFields.Selected.Caption);
          fi.FieldType:=   EditColumnForm.DataType;
          fi.IsPrimaryKey:= EditColumnForm.IsPrimaryKey;
          if EditColumnForm.txtLength.Visible then
            fi.Length:= EditColumnForm.Length;
          if EditColumnForm.txtPrecision.Visible then
            fi.Precision:= EditColumnForm.Precision;
          fi.AllowNull:=EditColumnForm.AllowNull;
          fi.IsIdentity:= EditColumnForm.Autonumber;

         if fi.IsPrimaryKey then
         begin
           pk := FWorkingTableInfo.PrimaryKeys.Add;
           pk.Assign(fi);
         end;

        end;
      except
        on e: Exception do
          ShowMessage(e.Message);
      end;
      PopulateInfo;
    end;

  end;

end;

procedure TDesignTableForm.actDropColumnExecute(Sender: TObject);
var
  sql: string;
  cn: string;
  I: integer;
  commaFields: string;
  backTable: string;
  fi: TAsFieldInfo;
  sql1: String;
  sql2: String;
  sql3: String;
  sql4: String;
  sql5: String;
  sql6: String;
begin
  if lsvFields.Selected <> nil then
  begin
    if MessageDlg('Confirm', 'Are you sure you want to drop this field?', mtConfirmation,
      mbYesNo, 0) = mrYes then
    begin

      if (lsvFields.Items.Count = 1) and (not FCreateNew) then
      begin
        ShowMessage('One column left that you want to drop. Drop the table instead');
        Exit;
      end;

      cn := lsvFields.Selected.Caption;

      fi := FWorkingTableInfo.FieldByName(cn);
        if fi.IsPrimaryKey then
        begin
          i := FWorkingTableInfo.PrimaryKeys.GetIndex(fi.FieldName);
          FWorkingTableInfo.PrimaryKeys.Delete(i);
        end;
       FWorkingTableInfo.AllFields.Delete(fi.Index);


      if FDbInfo.DbType = dtSQLite then
      begin
        backTable := FTablename + '_backup';
        commaFields := '';

        for I := 0 to FWorkingTableInfo.AllFields.Count - 1 do
        begin
          if FWorkingTableInfo.AllFields[I].FieldName <> cn then
          begin
            commaFields := commaFields +
              FWorkingTableInfo.AllFields[I].FieldName +
              LoopSeperator[integer(I < FWorkingTableInfo.AllFields.Count - 1)];
          end;
        end;

        if Trim(commaFields) <> EmptyStr then
          if commaFields[Length(commaFields)] = ',' then
            commaFields[Length(commaFields)] := ' ';

        lsvFields.Selected.Delete;
          sql1:=' CREATE TABLE ' + backTable + ' (' + commaFields + ');';
          sql2:= ' INSERT INTO ' + backTable + ' SELECT ' + commaFields+' FROM ' + FTablename + ';';
          sql3:=' DROP TABLE ' + FTablename +';';
          sql4:= GetCreateTableSQL+';';
          sql5 := ' INSERT INTO ' + FTablename +LineEnding+
          ' SELECT ' + commaFields + ' FROM ' + backTable + ';';
          sql6:= ' DROP TABLE ' +backTable + ';';

      end
      else if FDbInfo.DbType=dtFirebirdd then
      begin
        sql := 'ALTER TABLE ' + FTablename + ' DROP  ' + cn;
      end else
      begin
        sql := 'ALTER TABLE ' + FTablename + ' DROP COLUMN ' + cn;
      end;

      try
        if not FCreateNew then
        begin
          if FDbInfo.DbType<>dtSQLite then
          begin
          ExecuteQuery(sql);
          end
          else
          begin
            ExecuteQuery(sql1);
            ExecuteQuery(sql2);
            ExecuteQuery(sql3);
            ExecuteQuery(sql4);
            ExecuteQuery(sql5);
            ExecuteQuery(sql6);
          end;
        end;
        //else
        //begin
        //   fi := FWorkingTableInfo.FieldByName(cn);
        //   if fi.IsPrimaryKey then
        //   begin
        //     i := FWorkingTableInfo.PrimaryKeys.GetIndex(fi.FieldName);
        //     FWorkingTableInfo.PrimaryKeys.Delete(i);
        //   end;
        //  FWorkingTableInfo.AllFields.Remove(fi);
        //end;
      except
        on e: Exception do
          ShowMessage(e.Message);
      end;

    end;
  end;
  PopulateInfo;
end;

procedure TDesignTableForm.actDropConstraintExecute(Sender: TObject);
var
  sql:string;
  ci:TAsImportedKeyInfo;
begin

  if lsvDependencies.Selected = nil then
  Exit;

  if FDbInfo.DbType = dtMySql then
  begin
    sql := ' ALTER TABLE '+TAsDbUtils.SafeWrap(FDbInfo.DbType, FTablename) +
          ' DROP FOREIGN KEY '+TAsDbUtils.SafeWrap(FDbInfo.DbType,lsvDependencies.Selected.Caption);
  end else
  begin
    sql := '  ALTER TABLE '+TAsDbUtils.SafeWrap(FDbInfo.DbType, FTablename) +
            ' DROP CONSTRAINT '+ TAsDbUtils.SafeWrap(FDbInfo.DbType,lsvDependencies.Selected.Caption);
  end;

  if MessageDlg('Confirm', 'Are you sure you want to drop this field?', mtConfirmation,
      mbYesNo, 0) = mrYes then
  begin
    if Not FCreateNew then
    begin
      try
        ExecuteQuery(sql);
      except on E:Exception do
        ShowMessage(E.Message);
      end;
    end else
    begin
      ci :=FWorkingTableInfo.ImportedKeys.GetByName(lsvDependencies.Selected.SubItems[0]);
      if ci <> nil then
     FWorkingTableInfo.ImportedKeys.Delete(ci.Index);
    end;
    PopulateInfo;
  end;
end;

procedure TDesignTableForm.actDropIndexExecute(Sender: TObject);
var
  sql:string;
  ii:TAsIndexInfo;
begin
  if lsvIndexes.Selected=nil then
  exit;

  if MessageDlg('Are you sure you want to drop this index?',mtConfirmation,mbYesNo,0)=mrNo then
  Exit;

  case FDbInfo.DbType of
    dtMsSql:
      begin
        sql := 'DROP INDEX '+FTablename+'.'+lsvIndexes.Selected.Caption;
      end;
    dtOracle:
      begin
       sql := 'DROP INDEX '+lsvIndexes.Selected.Caption;
      end;
    dtMySql:
      begin
        sql := 'ALTER TABLE '+FTablename+' DROP INDEX '+lsvIndexes.Selected.Caption;
      end;
    dtSQLite:
      begin

      end;
    dtPostgreSql:
      begin
        sql := 'DROP INDEX '+lsvIndexes.Selected.Caption;
      end;
  end;

  if not FCreateNew then
  begin
    try
      ExecuteQuery(Sql);
    except on e:Exception do
     ShowMessage(e.Message);
    end;
    PopulateInfo;
  end else
  begin
      ii := FWorkingTableInfo.Indexes.GetByName(lsvIndexes.Selected.Caption);
      if ii<>nil then
      begin
        FWorkingTableInfo.Indexes.Delete(ii.Index);
      end;
  end;
  PopulateInfo;
end;

procedure TDesignTableForm.actCreateTableExecute(Sender: TObject);
var
  dbcloner:TAsDatabaseCloner;
begin
  try
    try
     dbcloner := TAsDatabaseCloner.Create(FDbInfo,FDbInfo.Database);
     dbcloner.MakeTable(FWorkingTableInfo);

     case FDbInfo.DbType of
       dtOracle,dtFirebirdd: if FWorkingTableInfo.Identities.Count>0 then dbcloner.MakeAutonumber(FWorkingTableInfo);
     end;

    except on e:Exception do
      begin
          ShowMessage(e.Message);
      end;
    end;
  finally
    dbcloner.Free;
  end;
end;

procedure TDesignTableForm.actCreateIndexExecute(Sender: TObject);
var
  sql:string;
  ii:TAsIndexInfo;
begin
  if EditIndexForm.ShowModal(FWorkingTableInfo)=mrOK then
  begin
    if Trim(EditIndexForm.IndexName)=EmptyStr then
    Exit;

    if EditIndexForm.Unique then
    begin
      sql :='CREATE UNIQUE ';
    end else
    begin
      sql := 'CREATE ';
    end;

    if FDbInfo.DbType<>dtFirebirdd then
    begin
      sql :=sql+' INDEX '+EditIndexForm.IndexName + ' ON '+FTablename +' ('+EditIndexForm.Column+' '+EditIndexForm.SortOrder+')';
    end else
    begin
      sql :=sql+' INDEX '+EditIndexForm.IndexName + ' ON '+FTablename +' COMPUTED BY ('+EditIndexForm.Column+')';
    end;

    if not FCreateNew then
    begin
      try
        ExecuteQuery(sql);
      except on E:exception do
        ShowMessage(E.Message);
      end;
    end else
    begin
      ii := FWorkingTableInfo.Indexes.Add;
      ii.Column_Name:=EditIndexForm.Column;
      ii.ASC_OR_DESC:=EditIndexForm.SortOrder;
      ii.INDEX_Name:= EditIndexForm.IndexName;
    end;

    PopulateInfo;
  end;

end;

procedure TDesignTableForm.actNewColumnExecute(Sender: TObject);
var
  cname: string;
  sql: string;
  dt: string;
  I: integer;
  fi,pk:TAsFieldInfo;
  id: TAsFieldInfo;
begin

  EditColumnForm.ClearInputs;
  EditColumnForm.DbType := FDbInfo.DbType;
  EditColumnForm.FillDataTypesOnShow:=True;
  EditColumnForm.chkPrimaryKey.Visible := FCreateNew;

   if (EditColumnForm.ShowModal(FDbInfo.DbType) =
    mrOk) and (EditColumnForm.Validate) then
  begin
    cname := EditColumnForm.ColumnName;

    if FDbInfo.DbType=dtSQLite then
      cname := ' COLUMN ' + cname;

    sql := ' ALTER TABLE ' + FTablename + ' ' + ' ADD ';

    if FDbInfo.DbType=dtOracle then
    sql := sql +' ( ';

    sql := sql + cname +' ' + EditColumnForm.DataType;
    dt := lowercase(EditColumnForm.DataType);

    if (dt = 'varchar') or (dt = 'nvarchar') or (dt = 'varchar2') then
    begin
      sql := sql + '(' + IntToStr(EditColumnForm.Length) + ')';
    end;

    if not EditColumnForm.AllowNull then
    begin
      sql := sql + ' NOT NULL';
    end
    else
    if (dt = 'decimal') then
    begin
      sql := sql + '(' + IntToStr(EditColumnForm.Length) + ',' +
        IntToStr(EditColumnForm.Precision) + ')';
    end;

    if FDbInfo.DbType=dtOracle then
    sql := sql +' ) ';


    try

      if not FCreateNew then
      begin
        ExecuteQuery(sql)
      end else
      begin
        fi := FWorkingTableInfo.AllFields.Add;
        fi.FieldName:= EditColumnForm.ColumnName;
        fi.FieldType:= EditColumnForm.DataType;
        fi.IsPrimaryKey:= EditColumnForm.IsPrimaryKey;


        if EditColumnForm.txtLength.Visible then
            fi.Length := EditColumnForm.Length;
         if EditColumnForm.txtPrecision.Visible then
        fi.Precision := EditColumnForm.Precision;
         fi.AllowNull:= EditColumnForm.AllowNull;

         fi.IsIdentity:= EditColumnForm.Autonumber;
         id := FWorkingTableInfo.Identities.Add;
         id.Assign(fi);

         if fi.IsPrimaryKey then
         begin
           pk := FWorkingTableInfo.PrimaryKeys.Add;
           pk.Assign(fi);
         end;

      end;
    except
      on e: Exception do
        ShowMessage(e.Message);
    end;

    PopulateInfo;

  end;
end;

procedure TDesignTableForm.actNewConstraintExecute(Sender: TObject);
var
  sql:string;
  ci:TAsImportedKeyInfo;
begin
 {

 alter table people
  add constraint fk_people_country
  foreign key (country) references countries(code)
  using desc index ix_people_country

 }
  if EditConstraintForm.ShowModal(FDbInfo,FSchema,FWorkingTableInfo) = mrOK then
  begin

    if trim(EditConstraintForm.LocalColumn) = EmptyStr then
    exit;

    sql := ' ALTER TABLE ' +FTablename;

    sql := sql + ' ADD ';

    if FDbInfo.DbType=dtFirebirdd then
    sql := sql +' CONSTRAINT ' +' fk_'+EditConstraintForm.LocalColumn + '_'+ EditConstraintForm.ReferencedTable;

    sql := sql +
           ' FOREIGN KEY ('+EditConstraintForm.LocalColumn+') ' +
           ' REFERENCES '+EditConstraintForm.ReferencedTable+'('+EditConstraintForm.ReferencedColumn+')';

      if not FCreateNew then
      begin
        try
           ExecuteQuery(sql);
         except on E:Exception do
           ShowMessage(e.Message);
         end;

      end else
      begin
         ci:=FWorkingTableInfo.ImportedKeys.Add;
         ci.ColumnName:=EditConstraintForm.LocalColumn;
         ci.ForeignSchema:=FSchema;
         ci.ForeignColumnName:=EditConstraintForm.ReferencedColumn;
         ci.ForeignTableName:=EditConstraintForm.ReferencedTable;

         ci.ConstraintName:='fk_'+ci.ForeignTableName+ci.ForeignColumnName;

      end;
      PopulateInfo;
  end;
end;

procedure TDesignTableForm.FormShow(Sender: TObject);
begin
  btnApply.Visible := FCreateNew;
  pgcMain.ActivePageIndex := 0;

  tabDependencies.TabVisible := (FDbInfo.DbType <> dtSQLite) or FCreateNew;
  tabIndexes.TabVisible := (FCreateNew=False);

end;

function TDesignTableForm.GetCreateTableSQL: string;
var
  sql: string;
  I: integer;
begin

  sql := 'CREATE TABLE ' + FTablename + ' ( ';

    for I := 0 to FWorkingTableInfo.AllFields.Count - 1 do
    begin
      sql := sql +FWorkingTableInfo.AllFields[I].FieldName + ' ' + FWorkingTableInfo.AllFields[I].FieldType;

      if (lowercase(FWorkingTableInfo.AllFields[I].FieldType) = 'varchar') or
        (lowercase(FWorkingTableInfo.AllFields[I].FieldType) = 'nvarchar') or
        (LowerCase(FWorkingTableInfo.AllFields[I].FieldType) = 'varchar2') or
        (LowerCase(FWorkingTableInfo.AllFields[I].FieldType) = 'nvarchar2')
        then
        sql := sql + '( ' + lsvFields.Items[I].SubItems[1] + ')';

      if (FWorkingTableInfo.AllFields[I].FieldType = 'decimal') or
        (FWorkingTableInfo.AllFields[I].FieldType = 'float') then
        sql := sql + '( ' + IntToStr(FWorkingTableInfo.AllFields[I].Length) + ',' +
          IntToStr(FWorkingTableInfo.AllFields[I].Precision) + ')';

      if FWorkingTableInfo.AllFields[I].IsIdentity then
      begin

        case FDbInfo.DbType of
          dtMsSql:sql := sql + ' IDENTITY ';
          dtOracle:
            begin
              ExecuteQuery('CREATE SEQUENCE '+FTablename+'_seq '+
                          ' START WITH     1 '+
                          ' INCREMENT BY   1 ' +
                          '  NOCACHE ' +
                          ' NOCYCLE;');
            end;
          dtMySql:sql := sql +' auto_increment ';
          //dtSQLite:sql := sql + ' AUTOINCREMENT ';
        end;
      end;

     if not FWorkingTableInfo.AllFields[I].AllowNull then
      begin
        sql:=sql +' NOT NULL';
      end;

      sql := sql + LoopSeperator[integer(I < lsvFields.Items.Count - 1)];
    end;

    if FWorkingTableInfo.HasPrimaryKeys then
    begin
      if FDbInfo.DbType in [dtSQLite,dtOracle,dtMySql] then
      begin
        sql := sql + ',';

      end;

      if FDbInfo.DbType in [dtOracle,dtMySql] then
      sql := sql +' CONSTRAINT '+FTablename+'_PK ';

      sql := sql + ' PRIMARY KEY (';


      for I := 0 to FWorkingTableInfo.PrimaryKeys.Count - 1 do
      begin
        sql := sql + FWorkingTableInfo.PrimaryKeys[I].FieldName + LoopSeperator[integer(I < FWorkingTableInfo.PrimaryKeys.Count - 1)];

        if FDbInfo.DbType = dtSQLite then
        begin
          sql := sql + ' ASC';
          break;
        end;
      end;
      sql := sql + ' )';

    end;


    for I:=0 to FWorkingTableInfo.ImportedKeys.Count-1 do
      begin
         if FDbInfo.DbType in [dtSQLite,dtOracle,dtMySql] then
         begin
           sql := sql + ',';
         end;
        Sql := Sql + ' CONSTRAINT fk_'+FWorkingTableInfo.ImportedKeys[I].ForeignTableName+
        FWorkingTableInfo.ImportedKeys[I].ForeignColumnName+
        ' FOREIGN KEY ('+FWorkingTableInfo.ImportedKeys[I].ColumnName+') '+
                      ' REFERENCES '+FWorkingTableInfo.ImportedKeys[I].ForeignTableName+'('+FWorkingTableInfo.ImportedKeys[I].ForeignColumnName+') ';
      end;

    sql := sql + ')';

    if FDbInfo.DbType = dtMySql then
    sql := sql +';';

    Result := sql;

end;


procedure TDesignTableForm.SetCreateNew(AValue: boolean);
begin
  if FCreateNew = AValue then
    Exit;
  FCreateNew := AValue;
end;

procedure TDesignTableForm.ExecuteQuery(Query: string);
begin
  TAsDbUtils.ExecuteQuery(Query,FDbInfo);
end;

procedure TDesignTableForm.PopulateInfo;
var
  I: integer;
  lst: TStringList;
begin
  //refreshed
  FDbInfo.Close;
  FDbInfo.Open;

    if not FCreateNew then
    begin
      FTableInfos.Clear;
      FWorkingTableInfo := FTableInfos.Add(FSchema, FTablename);
    end;

    //-----Populate FieldInfos----------
    lsvFields.Clear;
    for I := 0 to FWorkingTableInfo.AllFields.Count - 1 do
    begin
      with lsvFields.Items.Add do
      begin
        Caption := FWorkingTableInfo.AllFields[I].FieldName;
        SubItems.Add(FWorkingTableInfo.AllFields[I].FieldType);

        SubItems.Add(IntToStr(FWorkingTableInfo.AllFields[I].Length));

        ImageIndex := 0;

        SubItems.Add(' ');
        if FWorkingTableInfo.AllFields[I].IsPrimaryKey then
        begin
          SubItems[2] := 'PK';
          ImageIndex := 3;
        end
        else
        if FWorkingTableInfo.AllFields[I].IsReference then
        begin
          SubItems[2] := 'FK';
          ImageIndex := 1;
        end;

        if FWorkingTableInfo.AllFields[I].IsPrimaryKey and
          FWorkingTableInfo.AllFields[I].IsReference then
        begin
          SubItems[2] := 'PK and FK';
          ImageIndex := 4;
        end;

        if FWorkingTableInfo.AllFields[I].IsIdentity then
        begin
          SubItems[2] := SubItems[2] +' [Autonumber]';
        end;

        if (FWorkingTableInfo.AllFields[I].FieldType='decimal') or (FWorkingTableInfo.AllFields[I].FieldType='float') then
        begin
          SubItems.Add(IntToStr(FWorkingTableInfo.AllFields[I].Precision));
        end else
        begin
          SubItems.Add(' ');
        end;

        SubItems.Add( BoolToStr(FWorkingTableInfo.AllFields[I].AllowNull,True) );



      end;
    end;

    //--------Populate Dependencies-------
    lsvDependencies.Clear;
    if FWorkingTableInfo.ImportedKeys <> nil then
    for I := 0 to FWorkingTableInfo.ImportedKeys.Count - 1 do
    begin
      with lsvDependencies.Items.Add do
      begin
        ImageIndex := 1;
        Caption := FWorkingTableInfo.ImportedKeys[I].ConstraintName;
        SubItems.AddStrings(FWorkingTableInfo.ImportedKeys[I].ColumnName);
        SubItems.Add(FWorkingTableInfo.ImportedKeys[I].ForeignTableName);
        SubItems.Add(FWorkingTableInfo.ImportedKeys[I].ForeignColumnName);

      end;
    end;

    //-------Populate Triggers -----
    lsvTriggers.Clear;
    for I := 0 to FWorkingTableInfo.Triggers.Count - 1 do
    begin
      with lsvTriggers.Items.Add do
      begin
        ImageIndex:=12;
        Caption:=FWorkingTableInfo.Triggers[I].Name;
        SubItems.Add(FWorkingTableInfo.Triggers[I].Owner);
        SubItems.Add(FWorkingTableInfo.Triggers[I].Event);
        SubItems.Add(FWorkingTableInfo.Triggers[I].Body);
        SubItems.Add(FWorkingTableInfo.Triggers[I].Status);
      end;
    end;

    //-------Populate Indexes -----

    lsvIndexes.Clear;
    for I := 0 to FWorkingTableInfo.Indexes.Count - 1 do
    begin
      if Trim(FWorkingTableInfo.Indexes[I].INDEX_Name) <> EmptyStr then
      with lsvIndexes.Items.Add do
      begin
        ImageIndex:=13;
        Caption:=FWorkingTableInfo.Indexes[I].INDEX_Name;
        SubItems.Add(FWorkingTableInfo.Indexes[I].Column_Name);
        SubItems.Add(FWorkingTableInfo.Indexes[I].ASC_OR_DESC);
      end;
    end;



    if FWorkingTableInfo.HasPrimaryKeys then
    begin
      lblTablename.Caption := FWorkingTableInfo.Tablename;
      lblTablename.Font.Color := clGreen;
    end
    else
    begin
      lblTablename.Caption := FWorkingTableInfo.Tablename + ' (No primary keys found!)';
      lblTablename.Font.Color := clRed;
    end;

end;

function TDesignTableForm.Showmodal(DbInfo: TAsDbConnectionInfo; Schema: string;
 Tablename: string): integer;
begin
  FDbInfo := DbInfo;
  FTableInfos := TAsTableInfos.Create(nil,FDbInfo);

  if Tablename = EmptyStr then
  begin
    if InputQuery('New Table', 'Tablename', FTablename) then
    begin
      FCreateNew := True;
      lsvFields.Clear;
      FWorkingTableInfo := TAsTableInfo.Create(nil);
      FWorkingTableInfo.Tablename:=FTablename;
      FWorkingTableInfo.Schema:=Schema;
    end
    else
      exit;
  end
  else
  begin
    FCreateNew := False;
    FTablename := Tablename;
  end;

  FSchema := Schema;
  actEditColumn.Visible := not(FDbInfo.DbType in [dtSQLite,dtPostgreSql]);

  if not FCreateNew then
    PopulateInfo;

  Result := inherited Showmodal;
end;

end.
