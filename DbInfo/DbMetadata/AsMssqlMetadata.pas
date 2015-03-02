unit AsMssqlMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsDbType;

type

  { TAsMssqlMetadata }

  TAsMssqlMetadata = class(TAsDbMetadata)
  private
    FDBInfo:TAsDbConnectionInfo;
  public
    constructor Create(DbInfo:TAsDbConnectionInfo);
    function GetSchemas: TStringList;override;
    function GetTablenames(schema: string): TStringList;override;
    function GetPrimaryKeys(Schema, TableName: string): TStringList;override;
    function GetForeignKeys(Schema, TableName: string): TAsForeignKeys;override;
    function GetColumns(Schema, TableName: string): TAsColumns;override;
    function GetIndexes(Schema, TableName: string): TAsIndexes;override;
    function GetTriggers(Schema, TableName: string): TAsTriggers;override;
    function GetProcedureNames(Schema: string): TStringList;override;
    function GetProcedureParams(ProcedureName: string): TAsProcedureParams;override;
    function GetCatalogNames: TStringList;override;
  end;

implementation

{ TAsMssqlMetadata }

constructor TAsMssqlMetadata.Create(DbInfo: TAsDbConnectionInfo);
begin
 FDBInfo :=DbInfo;
 FDBInfo.DbType:= dtMsSql;
end;



function TAsMssqlMetadata.GetSchemas: TStringList;
var
  qr: TAsQuery;
begin
  Result := TStringList.Create;
    qr := TAsQuery.Create(FDBInfo);
    try
      qr.Open('SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA');
      while not qr.EOF do
      begin
        Result.Add(qr.Fields[0].AsString);
        qr.Next;
      end;
    finally
      qr.Free;
    end;
end;

function TAsMssqlMetadata.GetTablenames(schema: string): TStringList;
var
 sql:string;
 ds:TAsQuery;
begin

  Result := TStringList.Create;

  sql:='select t.TABLE_NAME from information_schema.tables t '+
              ' where t.TABLE_CATALOG='''+FDBInfo.Database+''' and t.TABLE_SCHEMA='''+schema+''' order by TABLE_NAME';


   ds :=  TAsQuery.Create(FDBInfo);
   try
    ds.Open(sql);
     while not ds.EOF do
     begin
      Result.Add(Trim(ds.Fields[0].AsString));
      ds.Next;
     end;
   finally
    ds.Free;
   end;
end;


function TAsMssqlMetadata.GetPrimaryKeys(Schema, TableName: string): TStringList;
var
  sql:string;
  ds:TAsQuery;
begin
 Result := TStringList.Create;

 sql := 'SELECT c.Column_Name '+
 ' FROM INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE c '+
 ' INNER JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc on tc.Constraint_Name = c.Constraint_Name '+
 ' INNER JOIN INFORMATION_SCHEMA.TABLES s on s.TABLE_NAME=c.TABLE_NAME '+
 ' WHERE tc.Constraint_Type = ''PRIMARY KEY'' AND c.Table_Name = '''+TableName+''' and s.TABLE_SCHEMA='''+Schema+'''';
    ds := TAsQuery.Create(FDBInfo);
    try
     ds.Open(sql);
     while not ds.EOF do
      begin
        Result.Add(Trim(ds.Fields[0].AsString));
        ds.Next;
      end;
    finally
     ds.Free;
    end;
end;

function TAsMssqlMetadata.GetForeignKeys(Schema, TableName: string): TAsForeignKeys;
var
  sql:string;
  ds:TAsQuery;
  fk:TAsForeignKey;
begin
 Result := TAsForeignKeys.Create;

 sql:=
 'SELECT Object_name(constraint_object_id) CONSTRAINT_NAME, '+
  ' t.table_schema [SCHEMA], '+
  ' OBJECT_NAME(fkc.parent_object_id) TABLE_NAME,'+
  ' c.NAME COLUMN_NAME,'+
  ' t1.table_schema FOREIGN_SCHEMA,'+
  ' OBJECT_NAME(fkc.referenced_object_id) FOREIGN_TABLE_NAME,'+
  ' cref.NAME FOREIGN_COLUMN_NAME '+
' FROM sys.foreign_key_columns fkc '+
' INNER JOIN sys.columns c ON fkc.parent_column_id = c.column_id AND fkc.parent_object_id = c.object_id '+
' INNER JOIN sys.columns cref ON fkc.referenced_column_id = cref.column_id AND fkc.referenced_object_id = cref.object_id'+
' INNER JOIN information_schema.tables t on t.table_name=object_name(fkc.parent_object_id) '+
' INNER JOIN information_schema.tables t1 on t1.table_name=object_name(fkc.referenced_object_id) '+
' WHERE   t.TABLE_NAME = '''+TableName+''''+
' AND t.TABLE_SCHEMA='''+Schema+'''';
  ds := TAsQuery.Create(FDBInfo);
  try
   ds.Open(sql);
    while not ds.EOF do
     begin
       fk := TAsForeignKey.Create;
       fk.Schema:=Trim(ds.FieldByName('SCHEMA').AsString);
       fk.Constraint_Name:= Trim(ds.FieldByName('CONSTRAINT_NAME').AsString);
       fk.Table_Name:=Trim(ds.FieldByName('TABLE_NAME').AsString);
       fk.Column_Name:= Trim(ds.FieldByName('COLUMN_NAME').AsString);
       fk.Foreign_Schema:= Trim(ds.FieldByName('FOREIGN_SCHEMA').AsString);
       fk.Foreign_Table:= Trim(ds.FieldByName('FOREIGN_TABLE_NAME').AsString);
       fk.Foreign_Column:= Trim(ds.FieldByName('FOREIGN_COLUMN_NAME').AsString);
       Result.Add(fk);
       ds.Next;
     end;
  finally
    ds.Free;
  end;

end;

function TAsMssqlMetadata.GetColumns(Schema, TableName: string): TAsColumns;
var
 sql:string;
 ds:TAsQuery;
 c:TAsColumn;
begin

 Result := TAsColumns.Create;

 sql:='select c.column_name, '+
        ' c.data_type, '+
        ' c.character_maximum_length max_length, '+
        ' c.numeric_precision data_precision, '+
        ' c.numeric_scale data_scale, '+
        ' (case when IS_NULLABLE=''YES'' then 1 else 0 end) allow_null '+
' from information_schema.columns c '+
' where c.TABLE_NAME='''+TableName+''''+
' and c.TABLE_SCHEMA='''+Schema+''' order by ordinal_position';

    ds := TAsQuery.Create(FDBInfo);

    try
     ds.Open(sql);

      while not ds.EOF do
       begin

         c := TAsColumn.Create;

         c.Column_Name:=Trim(ds.FieldByName('COLUMN_NAME').AsString);
         c.Data_Type:=Trim(ds.FieldByName('DATA_TYPE').AsString);

         if not ds.FieldByName('MAX_LENGTH').IsNull then
           if Trim(ds.FieldByName('MAX_LENGTH').AsString)<>'' then
             c.Max_Length:=ds.FieldByName('MAX_LENGTH').AsInteger;

         if not ds.FieldByName('DATA_PRECISION').IsNull then
           if Trim(ds.FieldByName('DATA_PRECISION').AsString)<>'' then
             c.Data_Precision:=ds.FieldByName('DATA_PRECISION').AsInteger;

         if not ds.FieldByName('DATA_SCALE').IsNull then
           if Trim(ds.FieldByName('DATA_SCALE').AsString)<>'' then
             c.Data_Scale:=ds.FieldByName('DATA_SCALE').AsInteger;

         c.Allow_Null:=ds.FieldByName('ALLOW_NULL').AsBoolean;


         Result.Add(c);
         ds.Next;
       end;
    finally
      ds.Free;
    end;
end;

function TAsMssqlMetadata.GetIndexes(Schema, TableName: string): TAsIndexes;
var
  sql:string;
  ds:TAsQuery;
  c:TAsIndex;
  lst:TStringList;
  s:string;
begin

 Result := TAsIndexes.Create;

 sql:='Select i.name INDEX_NAME, '+
        ' c.name COLUMN_NAME, '+
        ' (case when ic.is_descending_key=0 then ''ASC'' else ''DESC'' end) as DESCEND '+
        ' from sys.tables t '+
        ' inner join sys.schemas s on t.schema_id = s.schema_id '+
        ' inner join sys.indexes i on i.object_id = t.object_id '+
        ' inner join sys.index_columns ic on ic.object_id = t.object_id '+
        ' inner join sys.columns c on c.object_id = t.object_id and ic.column_id = c.column_id '+
        ' where i.is_primary_key=0 and t.name='''+TableName+''' and s.name='''+Schema+'''';
    ds := TAsQuery.Create(FDBInfo);
   try
     ds.Open(sql);
     while not ds.EOF do
      begin
        c := TAsIndex.Create;
        c.Index_Name:= Trim(ds.FieldByName('INDEX_NAME').AsString);
        c.Column_Name:=Trim(ds.FieldByName('COLUMN_NAME').AsString);
        c.Descend:= Trim(ds.FieldByName('DESCEND').AsString);
        Result.Add(c);
        ds.Next;
      end;
   finally
     ds.Free;
   end;

end;

function TAsMssqlMetadata.GetTriggers(Schema, TableName: string): TAsTriggers;
var
  sql:string;
  ds,dsMsSql:TAsQuery;
  c:TAsTrigger;
begin

 Result := TAsTriggers.Create;


 sql:='SELECT USER_NAME(sysobjects.uid) AS TRIGGER_OWNER, sysobjects.name TRIGGER_NAME,' +
           ' (case OBJECTPROPERTY( id, ''ExecIsUpdateTrigger'') '+
           '        when  1 then ''UPDATE'''+
           '        else '+
           '        case OBJECTPROPERTY( id, ''ExecIsInsertTrigger'') '+
           '                when 1 then ''INSERT'''+
           '                else '+
           '                case OBJECTPROPERTY( id, ''ExecIsDeleteTrigger'')'+
           '                when 1 then ''DELETE'''+
           '                end '+
           '        end '+
           ' end) TRIGGER_EVENT, '+
           ''' '' TRIGGER_BODY, '+
           ' (case  when OBJECTPROPERTY(id, ''ExecIsTriggerDisabled'')=1 then ''DISABLED'' ELSE ''ENABLED'' end) TRIGGER_STATUS '+
           ' FROM sysobjects ' +
           ' INNER JOIN sysusers ON sysobjects.uid = sysusers.uid '+
           ' INNER JOIN sys.tables t  ON sysobjects.parent_obj = t.object_id '+
           ' INNER JOIN sys.schemas s ON t.schema_id = s.schema_id '+
           ' WHERE sysobjects.type = ''TR'' and t.name ='''+TableName+''' and s.name='''+Schema+'''';

   ds := TAsQuery.Create(FDBInfo);
   try
    ds.Open(sql);
    while not ds.EOF do
     begin
       c := TAsTrigger.Create;
       c.Trigger_Owner:=Trim(ds.FieldByName('TRIGGER_OWNER').AsString);
       c.Trigger_Name:=Trim(ds.FieldByName('TRIGGER_NAME').AsString);
       c.Trigger_Body:=Trim(ds.FieldByName('TRIGGER_BODY').AsString);
       c.Trigger_Event:=Trim(ds.FieldByName('TRIGGER_EVENT').AsString);
       c.Trigger_Status:=Trim(ds.FieldByName('TRIGGER_STATUS').AsString);

       try
         dsMsSql := TAsQuery.Create(FDBInfo);
         try
          dsMsSql.Open('sp_helptext  @objname='''+c.Trigger_Name+'''');
           while not dsMsSql.EOF do
            begin
              c.Trigger_Body:=c.Trigger_Body+dsMsSql.Fields[0].AsString;
              dsMsSql.Next;
            end;
         finally
           dsMsSql.Free;
         end;
       except
         //suppress: if it doesn't find the object it returns no resultset so the Query raises error 'Cannot open a non-select statement'
       end;

       Result.Add(c);
       ds.Next;
     end;
   finally
    ds.Free;
   end;
end;

function TAsMssqlMetadata.GetProcedureNames(Schema: string): TStringList;
var
 sql:string;
  ds:TAsQuery;
  s: String;
begin

  sql:='select SPECIFIC_NAME '+
           'from information_schema.routines '+
           'where routine_type = ''PROCEDURE'''+
           'and SPECIFIC_SCHEMA='''+Schema+''' and ROUTINE_CATALOG='''+FDBInfo.Database+'''';

 Result := TStringList.Create;
 ds := TAsQuery.Create(FDbInfo);
  try
   ds.Open(sql);
    while not ds.EOF do
    begin
      s := ds.Fields[0].AsString;
      Result.Add(s);
      ds.Next;
    end;
  finally
    ds.Free;
  end;
end;

function TAsMssqlMetadata.GetProcedureParams(ProcedureName: string): TAsProcedureParams;
var
 sql:string;
 ds:TAsQuery;
 p:TAsProcedureParam;
begin

Result := TAsProcedureParams.Create;

 sql:='SELECT Parameter_name PARAM_NAME, DATA_TYPE, '+
             ' CHARACTER_MAXIMUM_LENGTH MAX_LENGTH, PARAMETER_MODE PARAM_TYPE '+
             '  FROM   INFORMATION_SCHEMA.PARAMETERS ' +
             ' WHERE SPECIFIC_NAME='''+ProcedureName+'''  and SPECIFIC_CATALOG='''+FDBInfo.Database+'''';
   ds := TAsQuery.Create(FDBInfo);
   try
    ds.Open(sql);
    while not ds.EOF do
    begin
      p := TAsProcedureParam.Create;
      p.Param_name:=trim(ds.FieldByName('PARAM_NAME').AsString);
      p.Data_Type:=trim(ds.FieldByName('DATA_TYPE').AsString);
      p.Max_Length:=ds.FieldByName('MAX_LENGTH').AsInteger;
      p.Param_Type:= trim(ds.FieldByName('PARAM_TYPE').AsString);
      Result.Add(p);
      ds.Next;
    end;
   finally
    ds.Free;
   end;
end;

function TAsMssqlMetadata.GetCatalogNames: TStringList;
var
 ds:TAsQuery;
 sql:string;
 dbi:TAsDbConnectionInfo;
begin
  Result := TstringList.Create;
  dbi := TAsDbConnectionInfo.Create;
  dbi.Assign(FDBInfo);
  try
   dbi.Database:='master';
   sql :='SELECT name from sys.databases';
   ds := TAsQuery.Create(dbi);
    try
     ds.Open(sql);
     while not ds.EOF do
      begin
        Result.Add(ds.Fields[0].AsString);
        ds.Next;
      end;
    finally
      dbi.Free;
      ds.Free;
    end;
  except
  end;

end;

end.




