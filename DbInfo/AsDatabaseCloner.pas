{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit AsDatabaseCloner;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, AsTableInfo, AsDbType, DB, AsStringUtils,LCLType;

type

  { TAsDatabaseCloner }

  TAsDatabaseCloner = class
  private
    FDbInfo:TAsDbConnectionInfo;
    FDestDbName : string;
  public
    constructor Create(DestDBInfo:TAsDbConnectionInfo; DestDbName:string);
    {get sql script for create}
    function GetCreateScript(Info:TAsTableInfo;CreateConstraints: boolean; OverrideDefaultTypes: boolean):string;
    {Create table}
    procedure MakeTable(info: TAsTableInfo; CreateConstraints:boolean=true; OverrideDefaultTypes: boolean = False);
    {Create constraints for Info}
    procedure CreateConstraints(info:TAsTableInfo);
    {Create constraincts for all importedKeys in tableInfos}
    procedure CreateConstraintsForAll(infos:TAsTableInfos);
    {Create tables for all tableInfos}
    procedure MakeTables(infos: TAsTableInfos; OverrideDefaultTypes: boolean = False);
    {Create database}
    procedure MakeDatabase;
    {Drop database}
    procedure UnmakeDatabase;
    {Make autonumber for Oracle (create sequence) and Firebird (create trigger)}
    procedure MakeAutonumber(info:TAsTableInfo);
    destructor Destroy; override;
  end;


const
  LoopSeperator: array [0 .. 1] of char = (' ', ',');
implementation

uses Utils;

{ TAsDatabaseCloner }





constructor TAsDatabaseCloner.Create(DestDBInfo: TAsDbConnectionInfo;
 DestDbName: string);
var
  dbtyp:TAsDatabaseType;
begin
  FDbInfo:=DestDBInfo;
  dbtyp:= FDbInfo.DbType;
  FDestDbName:= DestDbName;
end;

function TAsDatabaseCloner.GetCreateScript(Info: TAsTableInfo;
 CreateConstraints: boolean; OverrideDefaultTypes: boolean): string;
var
  sql: string;
  I: integer;
  HasPK: boolean;
  len:Integer;
  fname:string;
begin

  sql := 'CREATE TABLE ' + info.Tablename + ' ( ' ;


  for I := 0 to info.AllFields.Count - 1 do
  begin
    fname := info.AllFields[I].FieldName;
    if not OverrideDefaultTypes then
      sql := sql + info.AllFields[I].FieldName + ' ' + info.AllFields[I].FieldType
    else
      sql := sql + info.AllFields[I].GetCompatibleFieldName(FDbInfo.DbType)+ ' ' +  info.AllFields[I].GetFieldTypeAs(FDbInfo.DbType);

    len := info.AllFields[I].Length;

    if len >2000 then
    len :=2000;




    if (lowercase(info.AllFields[I].GetFieldTypeAs(FDbInfo.DbType)) = 'varchar') or
      (lowercase(info.AllFields[I].GetFieldTypeAs(FDbInfo.DbType)) = 'nvarchar') or
      (LowerCase(info.AllFields[I].GetFieldTypeAs(FDbInfo.DbType)) = 'varchar2') or
      (LowerCase(info.AllFields[I].GetFieldTypeAs(FDbInfo.DbType)) = 'nvarchar2') then
      begin
        if len>0 then
          sql := sql + '( ' + IntToStr(len) + ')'
        else
          sql := sql + '(50)';
      end;


    if (info.AllFields[I].GetFieldTypeAs(FDbInfo.DbType) = 'decimal') or
      (info.AllFields[I].GetFieldTypeAs(FDbInfo.DbType) = 'float') then
      begin

        if info.AllFields[I].Precision=0 then
        info.AllFields[I].Precision := 2;

        if len=0 then len := 12;

        if len< info.AllFields[I].Precision then
        begin
          len := 12;
          info.AllFields[I].Precision := 2;
        end;

        sql := sql + '( ' + IntToStr(len) + ',' +
        IntToStr(info.AllFields[I].Precision) + ')';
      end;

    if info.AllFields[I].IsIdentity then
    begin

      case FDbInfo.DbType of
        dtMsSql: sql := sql + ' IDENTITY ';
        dtMySql: sql := sql + ' unsigned auto_increment ';
      end;

    end;

    if not info.AllFields[I].AllowNull then
    begin
      sql := sql + ' NOT NULL';
    end;

    sql := sql + LoopSeperator[integer(I < info.AllFields.Count - 1)];

  end;


  if info.HasPrimaryKeys then
  begin

    if FDbInfo.DbType in [dtSQLite, dtOracle, dtMySql,dtFirebirdd] then
    begin
      sql := sql + ',';

    end;

    if FDbInfo.DbType in [dtOracle, dtMySql,dtFirebirdd] then
      sql := sql + ' CONSTRAINT ' + info.Tablename + '_PK ';

    sql := sql + ' PRIMARY KEY (';


    for I := 0 to info.PrimaryKeys.Count - 1 do
    begin

      if I>15 then
      begin
        sql := Copy(sql, 1, Length(sql) - 1);
        break;
      end;

      sql := sql + info.PrimaryKeys[I].GetCompatibleFieldName(FDbInfo.DbType) +
        LoopSeperator[integer(I < info.PrimaryKeys.Count - 1)];

      if FDbInfo.DbType = dtSQLite then
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
  if FDbInfo.DbType <> dtSQLite then
    for I := 0 to info.ImportedKeys.Count - 1 do
    begin
      if FDbInfo.DbType in [dtOracle, dtMySql,dtFirebirdd] then
      begin
        sql := sql + ',';
      end;
      if FDbInfo.DbType<>dtFirebirdd then
      begin
      Sql := Sql + ' CONSTRAINT fk_' + info.ImportedKeys[I].ForeignTableName +
        info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DbType) +
        ' FOREIGN KEY (' + info.ImportedKeys[I].GetCompatibleColumnName(FDbInfo.DbType) + ') ' +
        ' REFERENCES ' + info.ImportedKeys[I].ForeignTableName +
        '(' + info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DbType) + ') ';

      end else
      begin
        Sql := Sql + ' CONSTRAINT fk_' + info.ImportedKeys[I].ForeignTableName +
        info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DbType) +
        ' FOREIGN KEY (' + info.ImportedKeys[I].GetCompatibleColumnName(FDbInfo.DbType) + ') ' +
        ' REFERENCES ' + info.ImportedKeys[I].ForeignTableName +
        '(' + info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DbType) + ') ';
      end;

    end;


  sql := sql + ')';

  if FDbInfo.DbType = dtMySql then
  begin
      sql := sql + ';'
  end;


  Result:=Sql;
end;

procedure TAsDatabaseCloner.MakeTable(info: TAsTableInfo;
 CreateConstraints: boolean; OverrideDefaultTypes: boolean);
var
  sql:String;
begin
  sql := GetCreateScript(info,CreateConstraints,OverrideDefaultTypes);
  TAsDbUtils.ExecuteQuery(sql,FDbInfo);
end;

procedure TAsDatabaseCloner.CreateConstraints(info: TAsTableInfo);
var
  sql:string;
  I: Integer;
begin

  for I:=0 to info.ImportedKeys.Count-1 do
  begin
     sql := ' ALTER TABLE ' +info.Tablename +
              ' ADD FOREIGN KEY ('+info.ImportedKeys[I].ColumnName+') ' +
              ' REFERENCES '+info.ImportedKeys[I].ForeignTableName+'('+info.ImportedKeys[I].ForeignColumnName+')';

     TAsDbUtils.ExecuteQuery(sql,FDbInfo);
  end;
end;

procedure TAsDatabaseCloner.CreateConstraintsForAll(infos: TAsTableInfos);
var
I: integer;
begin
 for I := 0 to infos.Count - 1 do
 begin
   CreateConstraints(infos[I]);
 end;
end;

procedure TAsDatabaseCloner.MakeTables(infos: TAsTableInfos;
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
  wt:TAsStringWrapType;
  dbtyp:TAsDatabaseType;
  dbname:string;
begin
  case FDbInfo.DbType of
    dtMsSql, dtSQLite: wt := swtBrackets;
    dtOracle: wt := swtQuotes;
  else
      wt := swtNone;
  end;



  dbname:= TAsDbUtils.SafeWrap(FDbInfo.DbType, FDestDbName);

  if FDbInfo.DbType in [dtMsSql,dtMySql] then
  begin
    sql := 'CREATE DATABASE ' + dbname;
    TAsDbUtils.ExecuteQuery(sql,FDbInfo);
    sql := 'USE '+dbname;
    TAsDbUtils.ExecuteQuery(sql,FDbInfo);
    FDbInfo.Database:=dbname;
  end else
  if FDbInfo.DbType in [dtSQLite,dtFirebirdd] then
  begin
    if FDbInfo.DbType=dtFirebirdd then
    begin
      FDbInfo.Properties.Clear;
      FDbInfo.ZeosConnection.Protocol:='firebird-1.5';
      FDbInfo.Properties.Add(
        'CreateNewDatabase=CREATE DATABASE ' + QuotedStr (FDbInfo.Database) +
        ' USER ' + QuotedStr (FDbInfo.Username) + ' PASSWORD ' + QuotedStr (FDbInfo.Password) +
        ' PAGE_SIZE 4096 DEFAULT CHARACTER SET ISO8859_1'
      );

    end;
    FDbInfo.Open;
  end;
end;

procedure TAsDatabaseCloner.UnmakeDatabase;
begin
 TAsDbUtils.ExecuteQuery('DROP DATABASE '+FDestDbName,FDbInfo);
end;

procedure TAsDatabaseCloner.MakeAutonumber(info: TAsTableInfo);
var
 I: Integer;
 sql:string;
 n:string;
 t: String;
begin
 for I:=0 to info.Identities.Count -1 do
 begin

     case FDbInfo.DbType of
       dtOracle:
       begin
         sql := 'CREATE SEQUENCE ' + info.TableNameAsControlName+'_'+info.Identities[I].FieldName + '_seq ' + LineEnding+
           ' START WITH     1 ' + LineEnding+
           ' INCREMENT BY   1 ' + LineEnding +
           ' NOCACHE ' + LineEnding +
           ' NOCYCLE;';
         TAsDbUtils.ExecuteQuery(sql,FDbInfo);
       end;
       dtFirebirdd:
       begin
         n :='gen_'+info.Tablename+'_'+info.Identities[I].FieldName;
         t := 't_'+info.Tablename+'_'+info.Identities[I].FieldName;
         sql := ' CREATE GENERATOR '+n+';';
         TAsDbUtils.ExecuteQuery(sql,FDbInfo);
         sql := 'SET GENERATOR '+n+' TO 0;';
         TAsDbUtils.ExecuteQuery(sql,FDbInfo);


         sql :=' CREATE TRIGGER '+t+' FOR '+ info.Tablename+' '+ LineEnding +
                ' ACTIVE BEFORE INSERT POSITION 0 AS '+ LineEnding +
                ' BEGIN ' +LineEnding +
                ' if (NEW.'+info.Identities[I].FieldName+' is NULL) then NEW.'+info.Identities[I].FieldName+' = GEN_ID('+n+', 1); '+ LineEnding +
                ' END';
         TAsDbUtils.ExecuteQuery(sql,FDbInfo);
       end;
     end;

 end;
end;

destructor TAsDatabaseCloner.Destroy;
begin
 inherited Destroy;
end;

end.


