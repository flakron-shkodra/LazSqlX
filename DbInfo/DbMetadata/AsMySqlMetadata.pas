unit AsMySqlMetadata;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,AsDbType;

type

  { TAsMySqlMetadata }

  TAsMySqlMetadata = class(TAsDbMetadata)
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

{ TAsMySqlMetadata }

constructor TAsMySqlMetadata.Create(DbInfo: TAsDbConnectionInfo);
begin
 FDBInfo:=DbInfo;
 FDBInfo.DbType:=dtMySql;
end;

function TAsMySqlMetadata.GetSchemas: TStringList;
var
 qr:TAsQuery;
begin
  Result := TStringList.Create;
  Result.Add(FDBInfo.Database);
end;

function TAsMySqlMetadata.GetTablenames(schema: string): TStringList;
var
 sql:string;
 ds:TAsQuery;
begin

  Result := TStringList.Create;

   sql:='select t.TABLE_NAME from information_schema.tables t '+
              ' where t.table_schema='''+schema+''' order by TABLE_NAME';

   ds :=  TAsQuery.Create(FDbInfo);
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

function TAsMySqlMetadata.GetPrimaryKeys(Schema, TableName: string
 ): TStringList;
var
  sql:string;
  ds:TAsQuery;
begin
 Result := TStringList.Create;

 sql:='SELECT k.column_name '+
            ' FROM information_schema.table_constraints t '+
            ' JOIN information_schema.key_column_usage k '+
            ' USING(constraint_name,table_schema,table_name) '+
            ' WHERE t.table_name='''+TableName+''' and t.table_schema='''+Schema+''''+
            ' ORDER BY ORDINAL_POSITION ASC;';
    ds := TAsQuery.Create(FDbInfo);
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

function TAsMySqlMetadata.GetForeignKeys(Schema, TableName: string
 ): TAsForeignKeys;
var
  sql:string;
  ds:TAsQuery;
  fk:TAsForeignKey;
begin
 Result := TAsForeignKeys.Create;

 sql:='SELECT  kcu.CONSTRAINT_NAME, '+
       ' t.TABLE_SCHEMA ''SCHEMA'',  '+
        ' kcu.TABLE_NAME, '+
        ' kcu.COLUMN_NAME, '+
        ' t1.TABLE_SCHEMA FOREIGN_SCHEMA, '+
        ' kcu.REFERENCED_TABLE_NAME FOREIGN_TABLE_NAME, '+
        ' kcu.REFERENCED_COLUMN_NAME FOREIGN_COLUMN_NAME '+
' FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu '+
' INNER JOIN information_schema.Tables t on t.TABLE_NAME=kcu.TABLE_NAME '+
' INNER JOIN information_schema.Tables t1 on t1.TABLE_NAME=kcu.REFERENCED_TABLE_NAME '+
' where kcu.REFERENCED_COLUMN_NAME<>'''''+
' AND t.TABLE_NAME='''+TableName+''''+
' AND t.TABLE_SCHEMA='''+Schema+'''';

 ds := TAsQuery.Create(FDbInfo);

  try
    ds.Open(sql);
    while not ds.EOF do
     begin
       fk := TAsForeignKey.Create;
       fk.Constraint_Name:=Trim(ds.FieldByName('CONSTRAINT_NAME').AsString);
       fk.Schema:=Trim(ds.FieldByName('SCHEMA').AsString);
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

function TAsMySqlMetadata.GetColumns(Schema, TableName: string): TAsColumns;
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

function TAsMySqlMetadata.GetIndexes(Schema, TableName: string): TAsIndexes;
var
  sql:string;
  ds:TAsQuery;
  c:TAsIndex;
  lst:TStringList;
  s:string;
begin

 Result := TAsIndexes.Create;


 sql:='SELECT c.INDEX_NAME, ' +
            ' c.COLUMN_NAME, '+
           ''''' DESCEND '+
           ' FROM INFORMATION_SCHEMA.STATISTICS c '+
           ' WHERE TABLE_NAME='''+TableName+''' and TABLE_SCHEMA='''+Schema+'''';

   ds := TAsQuery.Create(FDbInfo);
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

function TAsMySqlMetadata.GetTriggers(Schema, TableName: string): TAsTriggers;
var
 sql:string;
  ds,dsMsSql:TAsQuery;
  c:TAsTrigger;
begin

 Result := TAsTriggers.Create;

 sql:='select definer TRIGGER_OWNER, TRIGGER_NAME, event_manipulation TRIGGER_EVENT, '+
            ' action_statement TRIGGER_BODY,'' '' TRIGGER_STATUS '+
            ' from information_schema.triggers '+
            ' where event_object_table='''+TableName+''' and event_object_schema='''+Schema+'''';

   ds := TAsQuery.Create(FDbInfo);
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
       Result.Add(c);
       ds.Next;
     end;
   finally
    ds.Free;
   end;
end;

function TAsMySqlMetadata.GetProcedureNames(Schema: string): TStringList;
var
  sql:string;
  ds:TAsQuery;
  s: String;
begin

 sql:='select SPECIFIC_NAME '+
            ' from information_schema.routines '+
            ' where routine_type = ''PROCEDURE'' and routine_schema='''+Schema+'''';

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

function TAsMySqlMetadata.GetProcedureParams(ProcedureName: string
 ): TAsProcedureParams;
var
 sql:string;
 ds:TAsQuery;
 p:TAsProcedureParam;
begin
 Result := TAsProcedureParams.Create;

 sql:='SELECT Parameter_name PARAM_NAME, DATA_TYPE, '+
              ' CHARACTER_MAXIMUM_LENGTH MAX_LENGTH, PARAMETER_MODE PARAM_TYPE '+
              ' FROM   INFORMATION_SCHEMA.PARAMETERS '+
              ' WHERE SPECIFIC_NAME='''+ProcedureName+''' AND SPECIFIC_SCHEMA='''+FDbInfo.Database+'''';
   ds := TAsQuery.Create(FDbInfo);
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

function TAsMySqlMetadata.GetCatalogNames: TStringList;
var
 ds:TAsQuery;
 sql:string;
 dbi:TAsDbConnectionInfo;
begin
  Result := TstringList.Create;
  try
    dbi := TAsDbConnectionInfo.Create;
    dbi.Assign(FDBInfo);
    dbi.Database:='INFORMATION_SCHEMA';
    sql := 'SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA';
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

