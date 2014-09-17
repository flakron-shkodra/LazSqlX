{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit AsDatabaseCloner;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, TableInfo, DbType, ZConnection, ZDataset,SqlDb, DB, AsStringUtils,LCLType;

type

  { TAsDatabaseCloner }

  TAsDatabaseCloner = class
  private
    FDbInfo:TDbConnectionInfo;
    FDestDbName : string;
  public
    constructor Create(DestDBInfo:TDbConnectionInfo; DestDbName:string);
    function GetCreateScript(Info:TTableInfo;CreateConstraints: boolean; OverrideDefaultTypes: boolean):string;
    procedure MakeTable(info: TTableInfo; CreateConstraints:boolean=true; OverrideDefaultTypes: boolean = False);
    procedure CreateConstraints(info:TTableInfo);
    procedure CreateConstraintsForAll(infos:TTableInfos);
    procedure MakeTables(infos: TTableInfos; OverrideDefaultTypes: boolean = False);
    procedure MakeDatabase;
    procedure UnmakeDatabase;
    destructor Destroy; override;
  end;


const
  LoopSeperator: array [0 .. 1] of char = (' ', ',');
implementation

uses Utils;

{ TAsDatabaseCloner }





constructor TAsDatabaseCloner.Create(DestDBInfo: TDbConnectionInfo;
 DestDbName: string);
var
  dbtyp:TDatabaseType;
begin
  FDbInfo:=DestDBInfo;
  dbtyp:= FDbInfo.DatabaseType;
  FDestDbName:= DestDbName;
end;

function TAsDatabaseCloner.GetCreateScript(Info: TTableInfo;
 CreateConstraints: boolean; OverrideDefaultTypes: boolean): string;
var
  sql: string;
  I: integer;
  HasPK: boolean;
  len:Integer;
begin

  sql := 'CREATE TABLE ' + info.Tablename + ' ( ' ;


  for I := 0 to info.AllFields.Count - 1 do
  begin
    if not OverrideDefaultTypes then
      sql := sql + info.AllFields[I].FieldName + ' ' + info.AllFields[I].FieldType
    else
      sql := sql + info.AllFields[I].GetCompatibleFieldName(FDbInfo.DatabaseType)+ ' ' +
        info.AllFields[I].GetFieldTypeAs(FDbInfo.DatabaseType);

    len := info.AllFields[I].Length;

    if len >2000 then
    len :=2000;




    if (lowercase(info.AllFields[I].GetFieldTypeAs(FDbInfo.DatabaseType)) = 'varchar') or
      (lowercase(info.AllFields[I].GetFieldTypeAs(FDbInfo.DatabaseType)) = 'nvarchar') or
      (LowerCase(info.AllFields[I].GetFieldTypeAs(FDbInfo.DatabaseType)) = 'varchar2') or
      (LowerCase(info.AllFields[I].GetFieldTypeAs(FDbInfo.DatabaseType)) = 'nvarchar2') then
      begin
        if len>0 then
        sql := sql + '( ' + IntToStr(len) + ')'
        else
          sql := sql + '(50)';
      end;



    if (info.AllFields[I].GetFieldTypeAs(FDbInfo.DatabaseType) = 'decimal') or
      (info.AllFields[I].GetFieldTypeAs(FDbInfo.DatabaseType) = 'float') then
      begin

        if info.AllFields[I].Precision>1 then
        info.AllFields[I].Precision := 1;

        sql := sql + '( ' + IntToStr(len) + ',' +
        IntToStr(info.AllFields[I].Precision) + ')';
      end;

    if info.AllFields[I].IsIdentity then
    begin

      case FDbInfo.DatabaseType of
        dtMsSql: sql := sql + ' IDENTITY ';
        dtOracle:
        begin
          TDbUtils.ExecuteQuery
          ('CREATE SEQUENCE ' + info.TableCSharpName + '_seq ' +
            ' START WITH     1 ' +
            ' INCREMENT BY   1 ' + '  NOCACHE ' +
            ' NOCYCLE;',FDbInfo);
        end;
        dtMySql: sql := sql + ' auto_increment ';
        //dtSQLite:sql := sql + ' AUTOINCREMENT ';
      end;

    end;

    if not info.AllFields[I].AllowNull then
    begin
      sql := sql + ' NOT NULL';
    end;

    //if FDBType=dtFirebirdd then
    //if info.HasPrimaryKeys then
    //begin
    //  if info.AllFields[I].IsPrimaryKey then
    //  if info.PrimaryKeys[0].FieldName=info.AllFields[I].FieldName then
    //  sql := sql + ' PRIMARY KEY';
    //end;
    sql := sql + LoopSeperator[integer(I < info.AllFields.Count - 1)];

  end;


  if info.HasPrimaryKeys then
  begin

    if FDbInfo.DatabaseType in [dtSQLite, dtOracle, dtMySql,dtFirebirdd] then
    begin
      sql := sql + ',';

    end;

    if FDbInfo.DatabaseType in [dtOracle, dtMySql,dtFirebirdd] then
      sql := sql + ' CONSTRAINT ' + info.Tablename + '_PK ';

    sql := sql + ' PRIMARY KEY (';


    for I := 0 to info.PrimaryKeys.Count - 1 do
    begin

      if I>15 then
      begin
        sql := Copy(sql, 1, Length(sql) - 1);
        break;
      end;

      sql := sql + info.PrimaryKeys[I].GetCompatibleFieldName(FDbInfo.DatabaseType) +
        LoopSeperator[integer(I < info.PrimaryKeys.Count - 1)];

      if FDbInfo.DatabaseType = dtSQLite then
      begin
        if info.PrimaryKeys.Count > 1 then
        begin
          sql := Copy(sql, 1, Length(sql) - 1);
        end;
        sql := sql + ' ASC';
        break;
      end;

    end;
    sql := sql + ' )';

  end;

  if CreateConstraints then
  if FDbInfo.DatabaseType <> dtSQLite then
    for I := 0 to info.ImportedKeys.Count - 1 do
    begin
      if FDbInfo.DatabaseType in [dtOracle, dtMySql,dtFirebirdd] then
      begin
        sql := sql + ',';
      end;
      if FDbInfo.DatabaseType<>dtFirebirdd then
      begin
      Sql := Sql + ' CONSTRAINT fk_' + info.ImportedKeys[I].ForeignTableName +
        info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DatabaseType) +
        ' FOREIGN KEY (' + info.ImportedKeys[I].GetCompatibleColumnName(FDbInfo.DatabaseType) + ') ' +
        ' REFERENCES ' + info.ImportedKeys[I].ForeignTableName +
        '(' + info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DatabaseType) + ') ';

      end else
      begin
        Sql := Sql + ' CONSTRAINT fk_' + info.ImportedKeys[I].ForeignTableName +
        info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DatabaseType) +
        ' FOREIGN KEY (' + info.ImportedKeys[I].GetCompatibleColumnName(FDbInfo.DatabaseType) + ') ' +
        ' REFERENCES ' + info.ImportedKeys[I].ForeignTableName +
        '(' + info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DatabaseType) + ') ';
      end;

    end;


  sql := sql + ')';

  if FDbInfo.DatabaseType = dtMySql then
    sql := sql + ';';

  Result:=Sql;
end;

procedure TAsDatabaseCloner.MakeTable(info: TTableInfo;
 CreateConstraints: boolean; OverrideDefaultTypes: boolean);
var
  sql:String;
begin
 sql := GetCreateScript(info,CreateConstraints,OverrideDefaultTypes);
 TDbUtils.ExecuteQuery(sql,FDbInfo);
end;

procedure TAsDatabaseCloner.CreateConstraints(info: TTableInfo);
var
  sql:string;
  I: Integer;
begin

  for I:=0 to info.ImportedKeys.Count-1 do
  begin
     sql := ' ALTER TABLE ' +info.Tablename +
              ' ADD FOREIGN KEY ('+info.ImportedKeys[I].ColumnName+') ' +
              ' REFERENCES '+info.ImportedKeys[I].ForeignTableName+'('+info.ImportedKeys[I].ForeignColumnName+')';

     TDbUtils.ExecuteQuery(sql,FDbInfo);
  end;
end;

procedure TAsDatabaseCloner.CreateConstraintsForAll(infos: TTableInfos);
var
I: integer;
begin
 for I := 0 to infos.Count - 1 do
 begin
   CreateConstraints(infos[I]);
 end;
end;

procedure TAsDatabaseCloner.MakeTables(infos: TTableInfos;
  OverrideDefaultTypes: boolean);
var
  I: integer;
begin
  for I := 0 to infos.Count - 1 do
  begin
    MakeTable(infos[I], OverrideDefaultTypes);
  end;
end;

procedure TAsDatabaseCloner.MakeDatabase;
var
  sql: string;
  wt:TStringWrapType;
  dbtyp:TDatabaseType;
begin
  case FDbInfo.DatabaseType of
    dtMsSql, dtSQLite: wt := swtBrackets;
    dtOracle: wt := swtQuotes;
  else
      wt := swtNone;
  end;


  if FDbInfo.DatabaseType in [dtMsSql,dtMySql] then
  begin
    dbtyp := FDbInfo.DatabaseType;
    sql := 'CREATE DATABASE ' + TAsStringUtils.WrapString(FDestDbName,wt);
    TDbUtils.ExecuteQuery(sql,FDbInfo,deZeos);
    FDbInfo.Database:=TAsStringUtils.WrapString(FDestDbName,wt);
  end;
end;

procedure TAsDatabaseCloner.UnmakeDatabase;
begin
 TDbUtils.ExecuteQuery('DROP DATABASE '+FDestDbName,FDbInfo);
end;

destructor TAsDatabaseCloner.Destroy;
begin
 inherited Destroy;
end;

end.


