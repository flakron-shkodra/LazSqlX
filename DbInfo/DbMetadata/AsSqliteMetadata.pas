unit AsSqliteMetadata;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,AsDbType, AsStringUtils;

type

  { TAsSqliteMetadata }

  TAsSqliteMetadata = class(TInterfacedObject, IAsDbMetadata)
  private
    FDBInfo:TAsDbConnectionInfo;
  public
    constructor Create(DbInfo:TAsDbConnectionInfo);
    function GetSchemas: TStringList;
    function GetTablenames(schema: string): TStringList;
    function GetPrimaryKeys(Schema, TableName: string): TStringList;
    function GetForeignKeys(Schema, TableName: string): TAsForeignKeys;
    function GetColumns(Schema, TableName: string): TAsColumns;
    function GetIndexes(Schema, TableName: string): TAsIndexes;
    function GetTriggers(Schema, TableName: string): TAsTriggers;
    function GetProcedureNames(Schema: string): TStringList;
    function GetProcedureParams(ProcedureName: string): TAsProcedureParams;
    function GetCatalogNames: TStringList;
  end;

implementation

{ TAsSqliteMetadata }

constructor TAsSqliteMetadata.Create(DbInfo: TAsDbConnectionInfo);
begin
 FDBInfo := DbInfo;
 FDBInfo.DbType:=dtSQLite;
end;

function TAsSqliteMetadata.GetSchemas: TStringList;
begin
  Result := TStringList.Create;
  try
    Result.Add(ChangeFileExt(ExtractFileName(FDbInfo.Database), ''));
  except
    Result.Free;
    raise;
  end
end;

function TAsSqliteMetadata.GetTablenames(schema: string): TStringList;
var
 sql:string;
 ds:TAsQuery;
begin
  Result := TStringList.Create;
  try
   sql:='SELECT name from sqlite_master where type=''table''';
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

  except
    Result.Free;
    raise;
  end;
end;

function TAsSqliteMetadata.GetPrimaryKeys(Schema, TableName: string
 ): TStringList;
var
  sql:string;
  ds:TAsQuery;
begin
 Result := TStringList.Create;
 sql:='PRAGMA table_info('+TableName+')';
 try
    ds := TAsQuery.Create(FDbInfo);
    try
     ds.Open(sql);
     while not ds.EOF do
      begin
        if ds.FieldByName('pk').AsInteger=1 then
        Result.Add(ds.Fields[1].AsString);
        ds.Next;
      end;
    finally
     ds.Free;
    end;
 except
   Result.Free;
   raise;
 end;

end;

function TAsSqliteMetadata.GetForeignKeys(Schema, TableName: string
 ): TAsForeignKeys;
var
  sql:string;
  fk:TAsForeignKey;
begin
 Result := TAsForeignKeys.Create;
end;

function TAsSqliteMetadata.GetColumns(Schema, TableName: string): TAsColumns;
var
  sql:string;
  ds:TAsQuery;
  c:TAsColumn;
begin

 Result := TAsColumns.Create;

 sql:='pragma table_info('+TableName+')';

  try
    ds := TAsQuery.Create(FDbInfo);
    try
     ds.Open(sql);
      while not ds.EOF do
       begin
         c := TAsColumn.Create;
         c.Column_Name:=Trim(ds.FieldByName('name').AsString);
         c.Data_Type:=Trim(ds.FieldByName('type').AsString);
         c.Allow_Null:=not ds.FieldByName('notnull').AsBoolean;
         Result.Add(c);
         ds.Next;
       end;
    finally
      ds.Free;
    end;

  except
    Result.Free;
    raise;
  end;

end;

function TAsSqliteMetadata.GetIndexes(Schema, TableName: string): TAsIndexes;
var
  sql:string;
  ds:TAsQuery;
  c:TAsIndex;
  lst:TStringList;
  s:string;
begin

 Result := TAsIndexes.Create;

 sql:='select name INDEX_NAME,sql COLUMN_NAME, sql DESCEND '+
            ' from sqlite_master where type = ''index'' and tbl_name='''+TableName+'''';

 try
    ds := TAsQuery.Create(FDbInfo);
   try
     ds.Open(sql);
     while not ds.EOF do
      begin
        c := TAsIndex.Create;
        c.Index_Name:= Trim(ds.FieldByName('INDEX_NAME').AsString);
        c.Column_Name:=Trim(ds.FieldByName('COLUMN_NAME').AsString);
        c.Descend:= Trim(ds.FieldByName('DESCEND').AsString);
        try
          lst := TStringList.Create;
          TAsRegExUtils.RunRegex(c.Column_Name, '\((.*?)\)',lst);
          if lst.Count>0 then
          begin
            try
              s:= TAsStringUtils.RemoveChars(lst[0],['(',')']);
              c.Column_Name:= TAsStringUtils.SplitString(s,' ')[0];
              c.Descend:=TAsStringUtils.SplitString(s,' ')[1];
            except
            end;
          end;
        finally
          lst.Free;
        end;
        Result.Add(c);
        ds.Next;
      end;
   finally
     ds.Free;
   end;

 except
   Result.Free;
   raise;
 end;



end;

function TAsSqliteMetadata.GetTriggers(Schema, TableName: string): TAsTriggers;
var
  sql:string;
  ds,dsMsSql:TAsQuery;
  c:TAsTrigger;
begin

 Result := TAsTriggers.Create;

 sql:='select '' '' TRIGGER_OWNER, name TRIGGER_NAME, '' '' TRIGGER_EVENT,sql TRIGGER_BODY,'' '' TRIGGER_STATUS '+
            ' from sqlite_master where type = ''trigger'' and tbl_name='''+TableName+'''';

 try

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

 except
   Result.Free;
   raise;
 end;

end;

function TAsSqliteMetadata.GetProcedureNames(Schema: string): TStringList;
begin
 Result := TStringList.Create;
end;

function TAsSqliteMetadata.GetProcedureParams(ProcedureName: string
 ): TAsProcedureParams;
begin
 Result := TAsProcedureParams.Create;
end;

function TAsSqliteMetadata.GetCatalogNames: TStringList;
begin
 Result := TStringList.Create;
end;

end.

