unit AsFirebirdMetadata;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,AsDbType;

type

  { TAsFirebirdMetadata }

  TAsFirebirdMetadata = class(TAsDbMetadata)
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


{ TAsFirebirdMetadata }


constructor TAsFirebirdMetadata.Create(DbInfo: TAsDbConnectionInfo);
begin
 FDBInfo:=DbInfo;
 FDBInfo.DbType:=dtFirebirdd;
end;

function TAsFirebirdMetadata.GetSchemas: TStringList;
begin
  Result := TStringList.Create;
  Result.Add(ChangeFileExt(ExtractFileName(FDBInfo.Database), ''));
end;

function TAsFirebirdMetadata.GetTablenames(schema: string): TStringList;
var
 sql:string;
 ds:TAsQuery;
begin

  Result := TStringList.Create;
    sql:='select rdb$relation_name '+
                ' from rdb$relations '+
                ' where rdb$view_blr is null and (rdb$system_flag is null or rdb$system_flag = 0)';

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

function TAsFirebirdMetadata.GetPrimaryKeys(Schema, TableName: string
 ): TStringList;
var
  sql:string;
  ds:TAsQuery;
begin
 Result := TStringList.Create;

 sql :='SELECT s.rdb$field_name '+
              ' from rdb$indices i '+
              ' left join rdb$index_segments s on i.rdb$index_name = s.rdb$index_name '+
              ' left join rdb$relation_constraints rc on rc.rdb$index_name = i.rdb$index_name '+
              ' where rc.rdb$constraint_type = ''PRIMARY KEY'''+
              ' AND rc.rdb$relation_name='''+TableName+'''';
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

function TAsFirebirdMetadata.GetForeignKeys(Schema, TableName: string
 ): TAsForeignKeys;
var
  sql:string;
  ds:TAsQuery;
  fk:TAsForeignKey;
begin
 Result := TAsForeignKeys.Create;

 sql:='SELECT drc.rdb$constraint_name as CONSTRAINT_NAME,'+
        ''''+Schema+''' as SCHEMA, '+
        ' drc.rdb$relation_name as TABLE_NAME, '+
        ' dis.rdb$field_name AS COLUMN_NAME, '+
        ''''+Schema+''' as FOREIGN_SCHEMA, '+
        ' mrc.rdb$relation_name AS FOREIGN_TABLE_NAME,'+
        ' mis.rdb$field_name AS FOREIGN_COLUMN_NAME '+
' FROM rdb$relation_constraints drc '+
'  JOIN rdb$index_segments dis ON drc.rdb$index_name = dis.rdb$index_name '+
' JOIN rdb$ref_constraints ON drc.rdb$constraint_name = rdb$ref_constraints.rdb$constraint_name '+
' JOIN rdb$relation_constraints mrc ON rdb$ref_constraints.rdb$const_name_uq = mrc.rdb$constraint_name '+
' JOIN rdb$index_segments mis ON mrc.rdb$index_name = mis.rdb$index_name '+
' WHERE drc.rdb$constraint_type = ''FOREIGN KEY'' '+
' AND drc.rdb$relation_name = '''+TableName+'''';

 ds := TAsQuery.Create(FDBInfo);

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

function TAsFirebirdMetadata.GetColumns(Schema, TableName: string): TAsColumns;
var
  sql:string;
  ds:TAsQuery;
  c:TAsColumn;
begin

 Result := TAsColumns.Create;

  sql:='SELECT  r.RDB$FIELD_NAME AS column_name, '+
       '  CASE f.RDB$FIELD_TYPE '+
                ' WHEN 261 THEN ''BLOB'' '+
                ' WHEN 14 THEN ''CHAR'' '+
                ' WHEN 40 THEN ''CSTRING'' '+
                ' WHEN 11 THEN ''D_FLOAT'' '+
                ' WHEN 27 THEN ''DOUBLE'' '+
                ' WHEN 10 THEN ''FLOAT'' '+
                ' WHEN 16 THEN ''INT64'' '+
                ' WHEN 8 THEN ''INTEGER'' '+
                ' WHEN 9 THEN ''QUAD'''+
                ' WHEN 7 THEN ''SMALLINT'' '+
                ' WHEN 12 THEN ''DATE'' '+
                ' WHEN 13 THEN ''TIME'' '+
                ' WHEN 35 THEN ''TIMESTAMP'' '+
                ' WHEN 37 THEN ''VARCHAR'' '+
                ' ELSE ''UNKNOWN'' '+
              ' END AS data_type, '+
        ' f.RDB$FIELD_LENGTH AS max_length, '+
        ' f.RDB$FIELD_PRECISION as data_precision, '+
        ' f.RDB$FIELD_SCALE AS data_scale, '+
        ' case when r.RDB$NULL_FLAG=1 then 0 else 1 end AS Allow_Null '+
   '  FROM RDB$RELATION_FIELDS r '+
   '  INNER JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME '+
  ' WHERE r.RDB$RELATION_NAME='''+TableName+''''+
  ' ORDER BY r.RDB$FIELD_POSITION';

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

function TAsFirebirdMetadata.GetIndexes(Schema, TableName: string): TAsIndexes;
var
  sql:string;
  ds:TAsQuery;
  c:TAsIndex;
  lst:TStringList;
  s:string;
begin

 Result := TAsIndexes.Create;

 sql:='SELECT RDB$INDEX_SEGMENTS.RDB$INDEX_NAME AS INDEX_NAME, '+
              ' RDB$INDEX_SEGMENTS.RDB$FIELD_NAME AS COLUMN_NAME, '+
              ' '' '' AS DESCEND '+
              ' FROM RDB$INDEX_SEGMENTS '+
              ' LEFT JOIN RDB$INDICES ON RDB$INDICES.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME '+
              ' LEFT JOIN RDB$RELATION_CONSTRAINTS ON RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME = RDB$INDEX_SEGMENTS.RDB$INDEX_NAME '+
              ' WHERE UPPER(RDB$INDICES.RDB$RELATION_NAME)='''+TableName+''' AND RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_TYPE IS NULL; ';
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


function TAsFirebirdMetadata.GetTriggers(Schema, TableName: string
 ): TAsTriggers;
var
  sql:string;
  ds:TAsQuery;
  c:TAsTrigger;
begin

 Result := TAsTriggers.Create;

 sql:=
 'SELECT '' '' TRIGGER_OWNER, '+
 ' t.RDB$TRIGGER_NAME TRIGGER_NAME, '+
' (case t.RDB$TRIGGER_TYPE '+
' when 1 then ''BEFORE INSERT'''+
' when 2 then ''AFTER INSERT'''+
' when 3 then ''BEFORE UPDATE'''+
' when 4 then ''AFTER UPDATE'''+
' when 5 then ''BEFORE DELETE'''+
' when 6 then ''AFTER DELETE'''+
' when 17 then ''BEFORE INSERT/UPDATE'''+
' when 18 then ''AFTER INSERT/UPDATE'''+
' when 25 then ''BEFORE INSERT/DELETE'''+
' when 26 then ''AFTER INSERT/DELETE'''+
' when 27 then ''BEFORE UPDATE/DELETE'''+
' when 28 then ''AFTER UPDATE/DELETE'''+
' when 113 then ''BEFORE INSERT/UPDATE/DELETE'''+
' when 114 then ''AFTER INSERT/UPDATE/DELETE'''+
' end) as TRIGGER_EVENT,'+
' t.RDB$TRIGGER_SOURCE TRIGGER_BODY, '+
' (CASE RDB$TRIGGER_INACTIVE WHEN 0 THEN ''ENABLED'' ELSE ''DISABLED'' END) TRIGGER_STATUS '+
' FROM RDB$TRIGGERS t '+
' WHERE RDB$SYSTEM_FLAG=0 and RDB$RELATION_NAME='''+TableName+'''';

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

function TAsFirebirdMetadata.GetProcedureNames(Schema: string): TStringList;
var
  sql:string;
  ds:TAsQuery;
  s: String;
begin

  sql:='SELECT rdb$Procedure_name '+
               ' FROM rdb$procedures '+
               ' WHERE rdb$system_flag IS NULL OR rdb$system_flag = 0';

 Result := TStringList.Create;

  ds := TAsQuery.Create(FDbInfo);
  try
   ds.Open(sql);
    while not ds.EOF do
    begin
      s := Trim(ds.Fields[0].AsString);
      Result.Add(s);
      ds.Next;
    end;
  finally
    ds.Free;
  end;
end;

function TAsFirebirdMetadata.GetProcedureParams(ProcedureName: string
 ): TAsProcedureParams;
var
 sql:string;
 ds:TAsQuery;
 p:TAsProcedureParam;
begin

 Result := TAsProcedureParams.Create;

 sql:='SELECT p.rdb$parameter_name PARAM_NAME,  '+
 ' (case rf.rdb$field_type '+
 '       when 261 then ''BLOB'''+
 '       when 14 then ''CHAR'''+
 '       when 40 then ''CSTRING'''+
 '       when 11 then ''D_FLOAT'''+
 '       when 27 then ''DOUBLE'''+
 '       when 10 then ''FLOAT'''+
 '       when 16 then ''INT64'''+
 '       when 8 then ''INTEGER'''+
 '       when 9 then ''QUAD'''+
 '       when 7 then ''SMALLINT'''+
 '       when 12 then ''DATE'''+
 '       when 13 then ''TIME'''+
 '       when 35 then ''TIMESTAMP'''+
 '       when 37 then ''VARCHAR'''+
' end) DATA_TYPE, '+
' RDB$FIELD_LENGTH MAX_LENGTH, '+
' (case RDB$PARAMETER_TYPE when 0 then ''IN'' else ''OUT'' end) PARAM_TYPE '+
' FROM rdb$procedure_parameters p '+
' INNER JOIN rdb$fields rf on rf.rdb$field_name = p.rdb$field_source '+
' WHERE p.rdb$procedure_name='''+ProcedureName+'''';
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

function TAsFirebirdMetadata.GetCatalogNames: TStringList;
begin
  Result := TstringList.Create;
end;

end.

