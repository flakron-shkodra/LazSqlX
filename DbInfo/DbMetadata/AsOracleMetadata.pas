unit AsOracleMetadata;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,AsDbType;

type

{ TAsOracleMetadata }

TAsOracleMetadata = class(TAsDbMetadata)
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

{ TAsOracleMetadata }

constructor TAsOracleMetadata.Create(DbInfo: TAsDbConnectionInfo);
begin
 FDBInfo:=DbInfo;
 FDBInfo.DbType:=dtOracle;
end;

function TAsOracleMetadata.GetSchemas: TStringList;
var
 ds:TAsQuery;
begin
  Result := TStringList.Create;
    ds :=TAsQuery.Create(FDBInfo);
    try
      ds.Open('select distinct username from dba_users');
       while not ds.EOF do
        begin
          Result.Add(ds.Fields[0].AsString);
          ds.Next;
        end;
    finally
      ds.Free;
    end;
end;

function TAsOracleMetadata.GetTablenames(schema: string): TStringList;
var
 sql:string;
 ds:TAsQuery;
begin

  Result := TStringList.Create;

   sql:='SELECT table_name '+
        ' FROM all_tables where owner='''+schema+''' order by table_name';
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

function TAsOracleMetadata.GetPrimaryKeys(Schema, TableName: string
 ): TStringList;
var
  sql:string;
  ds:TAsQuery;
begin
 Result := TStringList.Create;

 sql := 'SELECT cols.column_name '+
          ' FROM all_constraints cons, all_cons_columns cols ' +
          ' WHERE cols.table_name = '''+Uppercase(TableName)+''''+
          ' AND cons.constraint_type = ''P'''+
          ' AND cons.constraint_name = cols.constraint_name '+
          ' AND cons.owner = cols.owner '+
          ' ORDER BY cols.table_name, cols.position ';
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

function TAsOracleMetadata.GetForeignKeys(Schema, TableName: string
 ): TAsForeignKeys;
var
  sql:string;
  sqlExec:string;
  ds:TAsQuery;
  fk:TAsForeignKey;
begin
 Result := TAsForeignKeys.Create;

 sql:=
 'SELECT  uc.constraint_name, '+
        ' a.owner schema, '+
        ' a.table_name, '+
        ' a.column_name, '+
        ' uc.owner foreign_schema, '+
        ' uc.table_name foreign_table_name, '+
        ' uc.column_name foreign_column_name '+
' FROM all_cons_columns a '+
' JOIN all_constraints c ON a.owner = c.owner AND a.constraint_name = c.constraint_name '+
' JOIN all_constraints c_pk ON c.r_owner = c_pk.owner AND c.r_constraint_name = c_pk.constraint_name '+
' JOIN USER_CONS_COLUMNS uc ON uc.constraint_name = c.r_constraint_name '+
' WHERE  C.R_OWNER = '''+Schema+''''+
' AND a.table_name='''+TableName+'''';

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

function TAsOracleMetadata.GetColumns(Schema, TableName: string): TAsColumns;
var
  sql:string;
  ds:TAsQuery;
  c:TAsColumn;
begin

 Result := TAsColumns.Create;

 sql:='select  t.column_name, '+
        ' t.data_type, '+
        ' t.data_length max_length, '+
        ' t.data_precision data_precision, '+
        ' t.data_scale data_scale, '+
        ' (case when t.nullable=''Y'' then 1 ELSE 0 END) allow_null '+
' from user_tab_columns t '+
' inner join sys.all_tables st on st.table_name=t.table_name '+
' where st.table_name = '''+TableName+''' and st.owner='''+Schema+''' order by t.column_id ';

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

function TAsOracleMetadata.GetIndexes(Schema, TableName: string): TAsIndexes;
var
  sql:string;
  ds:TAsQuery;
  c:TAsIndex;
begin

 Result := TAsIndexes.Create;

 sql:= 'SELECT  ind.INDEX_NAME,ind.COLUMN_NAME,ind.DESCEND '+
            ' FROM dba_ind_columns ind WHERE ind.TABLE_NAME='''+TableName+''' and ind.TABLE_OWNER='''+Schema+'''';

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

function TAsOracleMetadata.GetTriggers(Schema, TableName: string): TAsTriggers;
var
  sql:string;
  ds,dsMsSql:TAsQuery;
  c:TAsTrigger;
begin

 Result := TAsTriggers.Create;

 sql:='select a.OWNER TRIGGER_OWNER, a.TRIGGER_NAME, a.TRIGGERING_EVENT TRIGGER_EVENT, '+
            '  a.TRIGGER_BODY, a.STATUS TRIGGER_STATUS'+
            ' from ALL_TRIGGERS a '+
            ' where a.TABLE_NAME = '''+TableName+''' and a.TABLE_OWNER='''+Schema+'''';

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
       Result.Add(c);
       ds.Next;
     end;
   finally
    ds.Free;
   end;
end;


function TAsOracleMetadata.GetProcedureNames(Schema: string): TStringList;
var
  sql:string;
  ds:TAsQuery;
  s: String;
begin

 sql:='SELECT OBJECT_NAME,STATUS FROM ALL_OBJECTS WHERE OBJECT_TYPE IN (''PROCEDURE'',''PACKAGE'',''FUNCTION'') '+
              ' and OWNER='''+Schema+'''';

 Result := TStringList.Create;
  ds := TAsQuery.Create(FDBInfo);
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

function TAsOracleMetadata.GetProcedureParams(ProcedureName: string
 ): TAsProcedureParams;
var
 sql:string;
 ds:TAsQuery;
 p:TAsProcedureParam;
begin
  Result := TAsProcedureParams.Create;
  sql:= 'SELECT Argument_Name PARAM_NAME, DATA_TYPE, '+
              '  DATA_LENGTH MAX_LENGTH, IN_OUT PARAM_TYPE '+
              ' FROM SYS.ALL_ARGUMENTS WHERE OBJECT_NAME='''+ProcedureName+'''';
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

function TAsOracleMetadata.GetCatalogNames: TStringList;
begin
  Result := TstringList.Create;
  //not implemented
end;

end.

