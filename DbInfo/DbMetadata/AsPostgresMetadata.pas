unit AsPostgresMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AsDbType;

type

  { TAsPostgresMetadata }

  TAsPostgresMetadata = class(TAsDbMetadata)
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

{ TAsPostgresMetadata }

constructor TAsPostgresMetadata.Create(DbInfo: TAsDbConnectionInfo);
begin
 FDBInfo :=DbInfo;
 FDBInfo.DbType:= dtPostgreSql;
end;



function TAsPostgresMetadata.GetSchemas: TStringList;
var
  qr: TAsQuery;
begin
  Result := TStringList.Create;
    qr := TAsQuery.Create(FDBInfo);
    try
      qr.Open('select schema_name from information_schema.schemata');
      while not qr.EOF do
      begin
        Result.Add(qr.Fields[0].AsString);
        qr.Next;
      end;
    finally
      qr.Free;
    end;
end;

function TAsPostgresMetadata.GetTablenames(schema: string): TStringList;
var
 sql:string;
 ds:TAsQuery;
begin

  Result := TStringList.Create;

  sql:='SELECT table_name FROM information_schema.tables WHERE table_schema='+QuotedStr(schema);

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


function TAsPostgresMetadata.GetPrimaryKeys(Schema, TableName: string): TStringList;
var
  sql:string;
  ds:TAsQuery;
begin
 Result := TStringList.Create;

 sql :='SELECT  c.column_name FROM information_schema.table_constraints tc '+
' JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name) '+
' JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema AND tc.table_name = c.table_name AND ccu.column_name = c.column_name '+
' where constraint_type = ''PRIMARY KEY'' and tc.table_name = '''+TableName+''' and tc.table_schema='''+Schema+'''';

    try
     ds := TAsQuery.Create(FDBInfo);
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

function TAsPostgresMetadata.GetForeignKeys(Schema, TableName: string): TAsForeignKeys;
var
  sql:string;
  ds:TAsQuery;
  fk:TAsForeignKey;
begin
 Result := TAsForeignKeys.Create;

 sql := 'SELECT  kcu.constraint_name, tc.table_schema "SCHEMA",   tc.table_name "TABLE_NAME",   kcu.column_name "COLUMN_NAME", '+
' ccu.table_schema "FOREIGN_SCHEMA",   ccu.table_name AS "FOREIGN_TABLE_NAME",   ccu.column_name AS "FOREIGN_COLUMN_NAME" '+
' FROM     information_schema.table_constraints AS tc '+
 '   JOIN information_schema.key_column_usage AS kcu '+
 '   ON tc.constraint_name = kcu.constraint_name '+
 '   JOIN information_schema.constraint_column_usage AS ccu '+
 '   ON ccu.constraint_name = tc.constraint_name '+
' WHERE constraint_type = ''FOREIGN KEY'' AND tc.table_name='+QuotedStr(TableName)+' and tc.table_schema='''+Schema+'''';


  ds := TAsQuery.Create(FDBInfo);
  try
   ds.Open(sql);
    while not ds.EOF do
     begin
       fk := TAsForeignKey.Create;
       fk.Constraint_Name:=Trim(ds.FieldByName('constraint_name').AsString);
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

function TAsPostgresMetadata.GetColumns(Schema, TableName: string): TAsColumns;
var
 sql:string;
 ds:TAsQuery;
 c:TAsColumn;
begin

 Result := TAsColumns.Create;

 sql:='select column_name, '+
        ' data_type, '+
        ' character_maximum_length max_length, '+
        ' numeric_precision data_precision, '+
        ' numeric_scale data_scale, '+
        ' (case when IS_NULLABLE=''YES'' then 1 else 0 end) allow_null '+
' from information_schema.columns  '+
' where TABLE_NAME='''+TableName+''''+
' and TABLE_SCHEMA='''+Schema+''' order by ordinal_position';

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

function TAsPostgresMetadata.GetIndexes(Schema, TableName: string): TAsIndexes;
var
  sql:string;
  ds:TAsQuery;
  c:TAsIndex;
  lst:TStringList;
  s:string;
begin

 Result := TAsIndexes.Create;

 sql:=' select i.relname as index_name, a.attname as column_name, '''' as "DESCEND" '+
      ' from     pg_class t, pg_class i, pg_index ix,  pg_attribute a '+
      ' where     t.oid = ix.indrelid   and i.oid = ix.indexrelid   and a.attrelid = t.oid  and a.attnum = ANY(ix.indkey)    and t.relkind = ''r'' '+
      '  and t.relname like '''+TableName+'''';

   try
      ds := TAsQuery.Create(FDBInfo);
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

function TAsPostgresMetadata.GetTriggers(Schema, TableName: string): TAsTriggers;
var
  sql:string;
  ds,dsMsSql:TAsQuery;
  c:TAsTrigger;
begin

 Result := TAsTriggers.Create;
                      { select prosrc from pg_trigger,pg_proc where
 pg_proc.oid=pg_trigger.tgfoid
 and pg_trigger.tgname = '<name>'}

 sql:='SELECT trigger_schema as trigger_Owner, trigger_name,event_manipulation as trigger_event, action_statement trigger_body, '''' trigger_status '+
      '  FROM information_schema.triggers  WHERE event_object_table='''+TableName+''' AND event_object_schema='''+Schema+'''';

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

function TAsPostgresMetadata.GetProcedureNames(Schema: string): TStringList;
var
 sql:string;
  ds:TAsQuery;
  s: String;
begin

 sql:='select routine_name '+
      ' from information_schema.routines '+
      ' where (routine_type = ''FUNCTION''  OR routine_type = ''PROCEDURE'' ) and routine_catalog='''+FDBInfo.Database+''' and routine_schema='''+Schema+'''';

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

function TAsPostgresMetadata.GetProcedureParams(ProcedureName: string): TAsProcedureParams;
var
 sql:string;
 ds:TAsQuery;
 p:TAsProcedureParam;
begin

Result := TAsProcedureParams.Create;

  sql:='SELECT Parameter_name PARAM_NAME, DATA_TYPE, '+
              ' CHARACTER_MAXIMUM_LENGTH MAX_LENGTH, PARAMETER_MODE PARAM_TYPE '+
              ' FROM   INFORMATION_SCHEMA.PARAMETERS '+
              ' WHERE SPECIFIC_NAME like '''+ProcedureName+'%'' AND SPECIFIC_CATALOG='''+FDbInfo.Database+'''';

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

function TAsPostgresMetadata.GetCatalogNames: TStringList;
var
 ds:TAsQuery;
 sql:string;
 dbi:TAsDbConnectionInfo;
begin
  Result := TstringList.Create;
  dbi := TAsDbConnectionInfo.Create;
  dbi.Assign(FDBInfo);
  try
   dbi.Database:='postgres';
   sql :='SELECT datname FROM pg_database WHERE datistemplate = false;';
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




