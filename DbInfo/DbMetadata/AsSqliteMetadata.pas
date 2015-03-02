unit AsSqliteMetadata;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,AsDbType, AsStringUtils;

type

  { TAsSqliteMetadata }

  TAsSqliteMetadata = class(TAsDbMetadata)
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

{ TAsSqliteMetadata }

constructor TAsSqliteMetadata.Create(DbInfo: TAsDbConnectionInfo);
begin
 FDBInfo := DbInfo;
 FDBInfo.DbType:=dtSQLite;
end;

function TAsSqliteMetadata.GetSchemas: TStringList;
begin
  Result := TStringList.Create;
  Result.Add(ChangeFileExt(ExtractFileName(FDbInfo.Database), ''));
end;

function TAsSqliteMetadata.GetTablenames(schema: string): TStringList;
var
 sql:string;
 ds:TAsQuery;
begin
  Result := TStringList.Create;
   sql:='SELECT name from sqlite_master where type=''table'' order by name';
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

function TAsSqliteMetadata.GetPrimaryKeys(Schema, TableName: string
 ): TStringList;
var
  sql:string;
  ds:TAsQuery;
begin
 Result := TStringList.Create;
 sql:='PRAGMA table_info('+TableName+')';
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

end;

function TAsSqliteMetadata.GetForeignKeys(Schema, TableName: string
 ): TAsForeignKeys;
var
  sql:string;
  fk:TAsForeignKey;
  lst,lst2,lst3:TStringList;
  ds:TAsQuery;
  s,ln:string;
  I: Integer;
begin
 Result := TAsForeignKeys.Create;
 lst := TStringList.Create;
 lst2 := TStringList.Create;

 ds := TAsQuery.Create(FDBInfo);
 try
    ds.Open('SELECT sql FROM sqlite_master WHERE type=''table'' and tbl_name='+QuotedStr(TableName));
    if ds.RecordCount>0 then
    begin
      s := ds.Fields[0].AsString;
      TAsRegExUtils.RunRegex(s, '(FOREIGN KEY.*?,)|(FOREIGN KEY.*?;)|(FOREIGN KEY.*?\)\))',lst);
      for I:=0 to lst.Count-1 do
      begin
        fk := TAsForeignKey.Create;
        fk.Schema:=Schema;
        fk.Table_Name:=TableName;
        ln := lst[I];
        //get local column shoud return  KEY([Column])
        TAsRegExUtils.RunRegex(lst[I],'(KEY.*?\))|(KEY .*?\))',lst2);
        s := lst2.Text;
        if s<>'' then
        begin
          fk.Column_Name:= Trim(StringReplace(TAsStringUtils.RemoveChars(s,['(',')','[',']']),'KEY','',[rfReplaceAll]));
          TAsRegExUtils.RunRegex(lst[I],'(REFERENCES .*?\))',lst2); //get refernced column shoud return  REFERENCES TableName([Column])
          s := StringReplace(lst2.Text,'REFERENCES','',[rfReplaceAll]); //should return TableName([Column])
          fk.Foreign_Schema:=Schema;
          fk.Foreign_Table:=TAsStringUtils.SplitString(s,'(')[0];
          fk.Foreign_Column:=TAsStringUtils.RemoveChars(TAsStringUtils.SplitString(s,'(')[1],[')','[',']']);
          Result.Add(fk);
        end else
        fk.Free;
      end;

    end;
 finally

  lst2.Free;
  lst.Free;
 end;

end;

function TAsSqliteMetadata.GetColumns(Schema, TableName: string): TAsColumns;
var
  sql:string;
  ds:TAsQuery;
  c:TAsColumn;
  lst:TStringList;
  s:string;
begin

 Result := TAsColumns.Create;

 sql:='pragma table_info('+TableName+')';

    ds := TAsQuery.Create(FDbInfo);
    lst :=TStringLIst.Create;
    try
     ds.Open(sql);
      while not ds.EOF do
       begin
         lst.Clear;
         c := TAsColumn.Create;
         c.Column_Name:=Trim(ds.FieldByName('name').AsString);
         c.Data_Type:=Trim(ds.FieldByName('type').AsString);
         TAsRegExUtils.RunRegex(c.Data_Type, '\(.*\)', lst);
         s := Trim(lst.Text);
         c.Data_Type:=StringReplace(c.Data_Type,s,'',[rfReplaceAll]);
         TryStrToInt(TAsStringUtils.RemoveChars(s,['(',')']),c.Max_Length);
         c.Allow_Null:=not ds.FieldByName('notnull').AsBoolean;
         Result.Add(c);
         ds.Next;
       end;
    finally
      ds.Free;
      lst.Free;
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

