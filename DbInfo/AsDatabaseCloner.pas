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

  TAsDbClonerRowEvent = procedure (CurrentRow,TotalRows:Integer) of object;
  { TAsDatabaseCloner }

  TAsDatabaseCloner = class
  private
    FDbInfo:TAsDbConnectionInfo;
    FDestDbName : string;
    FOnCopyRow:TAsDbClonerRowEvent;
    function GetFieldTypeAs(ftype:TFieldType):String;
  public
    constructor Create(DestDBInfo:TAsDbConnectionInfo; DestDbName:string);
    {get sql script for create}
    function GetCreateScript(Info:TAsTableInfo;CreateConstraints: boolean; OverrideDefaultTypes: boolean):string;
    {Create table}
    procedure MakeTable(info: TAsTableInfo; CreateConstraints:boolean=true; OverrideDefaultTypes: boolean = False);
    {Create constra0ints for Info}
    procedure CreateConstraints(info:TAsTableInfo);
    {Copy data from source db,table to destination table}
    procedure CopyData(FromDb: TAsDbConnectionInfo; srcTablename,
      destTablename: string; OnCopyRow: TAsDbClonerRowEvent);
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




function TAsDatabaseCloner.GetFieldTypeAs(ftype: TFieldType): String;
begin

  case FDbInfo.DbType of
  dtMsSql:
         begin
           case ftype of
             ftCurrency,ftBCD,ftFloat: Result:='decimal';
             ftMemo:Result := 'text';
             ftWideMemo: Result := 'ntext';
             ftWideString: Result:='nvarchar';
             ftBlob,ftOraBlob: Result:='image';
             ftBoolean:Result:='bit';
             ftDate,ftDateTime:Result:='datetime';
             ftInteger,ftSmallint,ftLargeint: Result:='int' ;
             ftAutoInc:Result:='int identity';
             ftBytes,ftVarBytes: Result:='binary'
             else
             Result:='varchar';
           end;
         end;

  dtOracle:
          begin
             case ftype of
               ftCurrency,ftBCD,ftFloat: Result:='decimal';
               ftWideMemo: Result := 'ntext';
               ftWideString: Result:='nvarchar2';
               ftBlob,ftOraBlob: Result :='blob';
               ftBoolean:Result:='numeric';
               ftDate,ftDateTime:Result:='date';
               ftInteger,ftSmallint: Result:='numeric' ;
               ftBytes,ftVarBytes: Result:='binary';
               else
               Result:='varchar2';
             end;
           end;
  dtMySql:
         begin
           case ftype of
             ftCurrency,ftBCD,ftFloat: Result:='decimal';
             ftWideMemo: Result := 'text';
             ftWideString: Result:='varchar';
             ftBlob,ftOraBlob: Result:='blob';
             ftBoolean:Result:='int';
             ftDate,ftDateTime:Result:='varchar';
             ftInteger,ftSmallint: Result:='int' ;
             ftAutoInc:Result:='int unsigned auto_increment';
             ftBytes,ftVarBytes: Result:='blob';
             else
             Result:='varchar';
           end;
         end;

  dtSQLite:
         begin
           case ftype of
              ftCurrency,ftFloat,ftFMTBcd: Result:='real';
              ftWideMemo,ftMemo,ftGuid: Result := 'text';
              ftWideString,ftString: Result:='varchar';
              ftBlob,ftOraBlob: Result:='blob';
              ftBoolean:Result:='integer';
              ftDate,ftDateTime:Result:='datetime';
              ftInteger,ftSmallint, ftLargeint,ftWord,ftAutoInc : Result:='integer';
              ftBytes,ftVarBytes: Result:='blob';
              else
              Result:='text';
            end;
         end;
  dtFirebirdd:
             begin
              case ftype of
               ftCurrency,ftBCD,ftFloat: Result:='decimal';
               ftWideMemo: Result := 'text';
               ftWideString: Result:='varchar';
               ftBlob,ftOraBlob: Result:='blob';
               ftBoolean:Result:='int';
               ftDate,ftDateTime:Result:='varchar';
               ftInteger,ftAutoInc, ftSmallint: Result:='int' ;
               ftBytes,ftVarBytes: Result:='blob';
               else
               Result:='varchar';
             end;
           end;
  dtPostgreSql:
            begin
            case ftype of
             ftCurrency,ftBCD,ftFloat: Result:='numeric';
             ftWideMemo: Result := 'text';
             ftWideString: Result:='character varying';
             ftBlob,ftOraBlob:
                begin
                  Result:='bytea';
                end;

             ftBoolean:Result:='boolean';
             ftDate,ftDateTime:Result:='date';
             ftInteger,ftSmallint: Result:='bigint' ;
             ftAutoInc:Result:='bigserial';
             ftBytes,ftVarBytes: Result:='bytea';
             else
             Result:='character varying';
             end;
            end;
  end;
end;

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
  len:Integer;
  strDefaulType,strNewType:string;
begin

  sql := 'CREATE TABLE ' + info.Tablename + ' (' ;


  for I := 0 to info.AllFields.Count - 1 do
  begin
    if not OverrideDefaultTypes then
      sql := sql + info.AllFields[I].FieldName + ' ' + info.AllFields[I].FieldType
    else
      sql := sql + info.AllFields[I].GetCompatibleFieldName(FDbInfo.DbType)+ ' ' +  GetFieldTypeAs(info.AllFields[I].DataType);

    len := info.AllFields[I].Length;

    if len >2000 then
    len :=2000;

    strDefaulType:= info.AllFields[I].FieldType;
    if OverrideDefaultTypes then
      strNewType:= lowercase(GetFieldTypeAs(info.AllFields[I].DataType))
    else
      strNewType:=strDefaulType;

    if (strNewType = 'varchar') or (strNewType = 'nvarchar') or (strNewType = 'varchar2') or
      (strNewType = 'nvarchar2') or (strNewType='character varying') then
      begin
        if len>0 then
          sql := sql + '(' + IntToStr(len) + ')'
        else
          sql := sql + '(50)';
      end;


      if (strNewType = 'decimal') or (strNewType = 'float') then
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

    if FDbInfo.DbType<>dtPostgreSql then
    begin
    sql := sql + ' PRIMARY KEY (';

      for I := 0 to info.PrimaryKeys.Count - 1 do
      begin
        sql := sql + info.PrimaryKeys[I].GetCompatibleFieldName(FDbInfo.DbType) +
          LoopSeperator[integer(I < info.PrimaryKeys.Count - 1)];
        //if FDbInfo.DbType=dtSQLite then
        //begin
        //  sql[Length(sql)]:=' ';
        //  Break;
        //end;
      end;
    end;
    sql := sql + ' )';

    if FDbInfo.DbType=dtPostgreSql then
    begin
      sql := sql +';' + LineEnding;
      for I:= 0 to info.PrimaryKeys.Count-1 do
      begin
        sql := sql + ' ALTER TABLE ONLY '+Info.Tablename+' ADD CONSTRAINT '+ info.Tablename+'_'+info.PrimaryKeys[I].FieldName+' PRIMARY KEY ('+Info.PrimaryKeys[I].FieldName+');'+LineEnding;
        break;//only one primary key
      end;
    end;

  end;

  if CreateConstraints then

    for I := 0 to info.ImportedKeys.Count - 1 do
    begin
      if FDbInfo.DbType in [dtOracle, dtMySql,dtFirebirdd,dtSQLite] then
      begin
        sql := sql + ',';
      end;
      case FDbInfo.DbType of
      dtMsSql,dtOracle,dtMySql,dtFirebirdd:
        begin
        Sql := Sql + ' CONSTRAINT ' + TAsDbUtils.SafeWrap(FDbInfo.DbType,'fk_'+ info.ImportedKeys[I].ForeignTableName + info.ImportedKeys[I].ForeignColumnName) +
          ' FOREIGN KEY (' + info.ImportedKeys[I].GetCompatibleColumnName(FDbInfo.DbType) + ') ' +
          ' REFERENCES ' + info.ImportedKeys[I].ForeignTableName +
          '(' + info.ImportedKeys[I].GetCompatibleForeignColumnName(FDbInfo.DbType) + ') ';

        end;
      dtSQLite:
        begin
           Sql := Sql+' FOREIGN KEY('+info.ImportedKeys[I].GetCompatibleColumnName(FDbInfo.DbType)+') REFERENCES '+
          info.ImportedKeys[I].ForeignTableName+'('+info.ImportedKeys[I].ForeignColumnName+')';
        end
      else
        begin
        sql :=sql+LineEnding+ ' ALTER TABLE ' +Info.Tablename + ' ADD FOREIGN KEY ('+ TAsDbUtils.SafeWrap(FDbInfo.DbType, info.ImportedKeys[I].ColumnName)+') ' +
        ' REFERENCES '+info.ImportedKeys[I].ForeignTableName+'('+TAsDbUtils.SafeWrap(FDbInfo.DbType, info.ImportedKeys[I].ForeignColumnName)+');';
        end;
      end;
   end;

  if FDbInfo.DbType <> dtPostgreSql then
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
    if FDbInfo.DbType <> dtPostgreSql then
    begin
     sql := ' ALTER TABLE ' +info.Tablename +
              ' ADD FOREIGN KEY ('+info.ImportedKeys[I].ColumnName+') ' +
              ' REFERENCES '+info.ImportedKeys[I].ForeignTableName+'('+info.ImportedKeys[I].ForeignColumnName+')';

    end else
    begin
      sql := 'ALTER TABLE ONLY '+info.Tablename+' ADD CONSTRAINT '+info.ImportedKeys[I].Tablename+'_'+
      info.ImportedKeys[I].ForeignTableName+'_'+info.ImportedKeys[I].ForeignColumnName+' FOREIGN KEY ('+info.ImportedKeys[I].ColumnName+') '+
      ' REFERENCES '+info.ImportedKeys[I].ForeignTableName+'('+info.ImportedKeys[I].ForeignColumnName+')';
    end;
    TAsDbUtils.ExecuteQuery(sql,FDbInfo);
  end;
end;

procedure TAsDatabaseCloner.CopyData(FromDb: TAsDbConnectionInfo; srcTablename,
  destTablename: string; OnCopyRow:TAsDbClonerRowEvent);
var
  src,dest:TAsQuery;
  I: Integer;
  f:TField;
  destSql : string;
  m: TMemoryStream;
begin
  FOnCopyRow:=OnCopyRow;
  src := TAsQuery.Create(FromDb);
  dest := TAsQuery.Create(FDbInfo);
  FDbInfo.Open;
  try
    src.DisableControls;
    dest.DisableControls;

    src.Open('select * from '+TAsDbUtils.SafeWrap(FromDb.DbType,srcTablename));
    case FDbInfo.DbType of
      dtSQLite:destSql := 'select * from '+TAsDbUtils.SafeWrap(FDbInfo.DbType,destTablename);
      dtFirebirdd : destSql := 'select * from '+destTablename;
      dtPostgreSql: destSql := 'select * from '+destTablename;
      dtMySql:destSql := 'select * from '+FDbInfo.Database+'.'+TAsDbUtils.SafeWrap(FDbInfo.DbType,destTablename);
      dtMsSql:destSql := 'select * from '+FDbInfo.Database+'.'+TAsDbUtils.SafeWrap(FDbInfo.DbType,destTablename);
      dtOracle:destSql := 'select * from '+FDbInfo.Database+'.'+TAsDbUtils.SafeWrap(FDbInfo.DbType,destTablename);
    end;
    dest.Open(destSql);

    if dest.Active then
    while not src.EOF do
    begin
      dest.Insert;
      for I:=0 to src.FieldCount-1 do
      begin
        f := dest.FieldByName(src.Fields[I].FieldName);
        if f.ReadOnly then continue;

        if (f.Required) and (src.Fields[I].IsNull) then
          f.Value :='1'
        else
        begin
          if f.IsBlob then
         begin
          m := TMemoryStream.Create;
          try
            TBlobField(src.Fields[I]).SaveToStream(m);
            m.Position:=0;
            TBlobField(f).LoadFromStream(m);
          finally
            m.Free;
          end;
         end else
         begin
          f.Value := src.Fields[I].Value;
         end;
        end;
      end;
      if dest.UpdateStatus in [usModified] then
      dest.Post;
      src.Next;
      if Assigned(FOnCopyRow) then
      FOnCopyRow(src.RecNo,src.RecordCount);
    end;
  finally
    src.Free;
    dest.Free;
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
  dbname:string;
begin

  dbname:= TAsDbUtils.SafeWrap(FDbInfo.DbType, FDestDbName);

  if FDbInfo.DbType in [dtMsSql,dtMySql,dtPostgreSql] then
  begin
    sql := 'CREATE DATABASE ' + dbname;
    TAsDbUtils.ExecuteQuery(sql,FDbInfo);
    if FDbInfo.DbType=dtPostgreSql then
    begin
      FDbInfo.Close;
      FDbInfo.Database:=TAsStringUtils.RemoveChars(dbname,['"']);
      FDbInfo.Open;
    end else
    begin
      sql := 'USE '+dbname;
      TAsDbUtils.ExecuteQuery(sql,FDbInfo);
    end;
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


