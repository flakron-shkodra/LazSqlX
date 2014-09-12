{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}

unit DbType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo,types, fgl, strutils, AsStringUtils, ZConnection,ZDataset,
  sqldb, db, fpsqlparser, fpsqltree;

type

 TDatabaseType =
   (
    dtMsSql = 0,
    dtOracle = 1,
    dtMySql = 2,
    dtSQLite = 3,
    dtFirebirdd = 4
   );

 TDatabaseEngine =
   (
    deSqlDB=0,
    deZeos=1
   );



 { TSqlSyntaxError }

 TSqlSyntaxError = class
 private
   FMessage:string;
   procedure SetMessage(AValue: string);
 public
   Line:Integer;
   Position:Integer;
   property Message:string read FMessage write SetMessage;
 end;




  { TDbConnectionInfo }

  TDbConnectionInfo = class
  private
   FDatabase: string;
   FDbType: TDatabaseType;
   FPassword: string;
   FPort: Integer;
   FServer: string;
   FUsername: string;
   procedure SetDatabase(AValue: string);
   procedure SetDbType(AValue: TDatabaseType);
   procedure SetPassword(AValue: string);
   procedure SetPort(AValue: Integer);
   procedure SetServer(AValue: string);
   procedure SetUsername(AValue: string);
  public
    property DatabaseType:TDatabaseType read FDbType write SetDbType;
    property Server:string read FServer write SetServer;
    property Database:string read FDatabase write SetDatabase;
    property Username:string read FUsername write SetUsername;
    property Password:string read FPassword write SetPassword;
    property Port:Integer read FPort write SetPort;
    function ToFullString:string;
    function ToZeosConnection:TZConnection;
    function ToSqlConnector:TSQLConnector;
  end;

  { TDbUtils }

 TDbUtils = object

  public
    class function IsBlobGUID(var aField: TField): boolean;

    class function GetOracleDescriptor(server, database: string; port: integer): string;overload;

    class function GetOracleDescriptor(dbInfo:TDbConnectionInfo): string;overload;

    {Converts TDatabaseType to relevant SqlConnector.ConnectorType }
    class function DatabaseTypeAsConnectorType(DbType:TDatabaseType):string;

    {Converts TDatabaseType to relevant zeos protocol in string format}
    class function DatabaseTypeAsString(DbType:TDatabaseType;ALowerCase:Boolean):string;

    {Converts zeos protocol to  TDatabaseType }
    class function DatabaseTypeFromString(DbTypeString:string):TDatabaseType;

    {Converts zeos protocol to  sqldb connectorType}
    class function ZeosProtocolToSqlDbConnectorType(ZeosProtocol:string):string;

    {Checks syntax using TSQLParser}
    class function CheckSqlSyntax(SqlCommand:string; out Error:TSqlSyntaxError):Boolean;

    {Executes SQL Command using given DBConnectionInfo }
    class procedure ExecuteQuery(SqlQuery:string; DbInfo:TDbConnectionInfo);overload;

    {Executes SQL Command using SQLDB or ZEOS Engine and given DBConnectionInfo}
    class procedure ExecuteQuery(SqlQuery:string; DbInfo:TDbConnectionInfo; DbEngine:TDatabaseEngine);overload;

    {Gets db schemas}
    class procedure GetSchemas(DBInfo:TDbConnectionInfo; var list:TStringList);

    {Gets db Tables for given schame}
    class procedure GetTablenames(DbInfo:TDbConnectionInfo; schema:string; var list:TStringList);

    class procedure GetColumnNames(DbInfo:TDbConnectionInfo; tablename:string; var list:TStringList);
  end;



  { TDbConnectionInfos }

  TDbConnectionInfos = class (specialize TFPGObjectList<TDbConnectionInfo>)
  public
    procedure LoadFromFile(Filename:string);
    procedure SaveToFile(Filename:string);
    function Exists(aDbInfo:TDbConnectionInfo):Boolean;
  end;


implementation

{ TSqlSyntaxError }

procedure TSqlSyntaxError.SetMessage(AValue: string);
var
 lst:TStringList;
 strLineNo,strPosNo:string;
begin
 if FMessage=AValue then Exit;
 FMessage:=AValue;
 if AnsiStartsText('Error',FMessage) then
 begin
 try
  strLineNo:=EmptyStr;
  strPosNo:=EmptyStr;

  lst := TStringList.Create;
  lst.DelimitedText:=FMessage;
  strLineNo :=TAsStringUtils.ExtractNumbers(lst[2]);
  strPosNo:=TAsStringUtils.ExtractNumbers(lst[4]);

  TryStrToInt(strLineNo,Line);
  TryStrToInt(strPosNo,Position);

 finally
   lst.Free;
 end;

 end;

end;

{ TDbUtils }

class function TDbUtils.IsBlobGUID(var aField: TField): boolean;
var
  m: TMemoryStream;
  b: TBlobField;
  f:TFieldType;
begin
  if aField<>nil then
  begin
    if aField.DataType = ftUnknown then
    exit;

     if aField.IsBlob then
     b := aField as TBlobField;

     try

       if Assigned(b) then
       begin
        m := TMemoryStream.Create;
        b.SaveToStream(m);
        Result := m.Size = 16;
       end;

     finally
       m.Free;
     end;

  end;
end;

class function TDbUtils.GetOracleDescriptor(server, database: string;
 port: integer): string;
begin
 Result := '(DESCRIPTION = (ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = ' +
      server + ')(PORT = ' + IntToStr(port) +
      '))) (CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME = ' +
     database + ')))';

  //Result := database;

end;

class function TDbUtils.GetOracleDescriptor(dbInfo: TDbConnectionInfo): string;
begin
   Result := '(DESCRIPTION = (ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = ' +
      dbInfo.Server + ')(PORT = ' + IntToStr(dbInfo.Port) +
      '))) (CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME = ' +
     dbInfo.Database + ')))';

end;

class function TDbUtils.DatabaseTypeAsConnectorType(DbType: TDatabaseType
 ): string;
begin
   case DbType of
    dtFirebirdd : Result := 'Firebird';
    dtSQLite : Result := 'SQLite3';
    dtMsSql: Result:='MSSQLServer';
    dtOracle:Result:='Oracle';
    dtMySql:Result:='MySQL 5.0';
  end;
end;

class function TDbUtils.DatabaseTypeAsString(DbType: TDatabaseType;
 ALowerCase: Boolean): string;
var
  s:string;
begin
  if DbType in [dtMsSql,dtMySql,dtOracle,dtSQLite,dtFirebirdd] then
  begin
  s := GetEnumName(TypeInfo(DbType),Integer(DbType));
  if ALowerCase then
  Result := LowerCase(Copy(s,3,Length(s)))
  else
  Result := (Copy(s,3,Length(s)));
  end else
  begin
    Result := '';
  end;

  case DbType of
    dtFirebirdd : Result := Result+'-2.5';
    dtSQLite : Result := Result+'-3';
  end;

end;

class function TDbUtils.DatabaseTypeFromString(DbTypeString: string
 ): TDatabaseType;
var
  s:string;
  I: Integer;
begin

  for I:=0 to 4 do
  begin
    s := GetEnumName(TypeInfo(TDatabaseType),I);
    s := Copy(s,3,Length(s));

    if lowercase(s) = lowercase(TAsStringUtils.RemoveChars(DbTypeString,['-','3','2','.','5'])) then
    begin
      Result := TDatabaseType(I);
      Break;
    end;
  end;

end;

class function TDbUtils.ZeosProtocolToSqlDbConnectorType(ZeosProtocol: string
 ): string;
var
 p:string;
begin
  p := Trim(Lowercase(ZeosProtocol));

  if (p='mssql') then
    Result := 'MSSQLServer'
  else
  if (p='oracle') or (p='oracle9') then
  Result := 'Oracle'
  else
  if (p='firebirdd-2.5') then
  Result := 'Firebird'
  else
  if (p='sqlite') or (p='sqlite-3') then
  Result:='SQLite3'
  else
  if (p='mysql') then
  Result := 'MySQL 5.5';
end;

class function TDbUtils.CheckSqlSyntax(SqlCommand: string; out
 Error: TSqlSyntaxError): Boolean;
var
  i: integer;
  Script:TStringList;
  Parser: TSQLParser;
  ResultList: TSQLElementList;
  ScriptStream: TMemoryStream;
begin
  ScriptStream:=TMemoryStream.Create;
  Script := TStringList.Create;
  Script.Text:=TAsStringUtils.RemoveChars(SqlCommand,['[',']']);
  try
    Script.SaveToStream(ScriptStream);
    ScriptStream.Position:=0;
    try
      Parser:=TSQLParser.Create(ScriptStream);
      try
        ResultList:=Parser.ParseScript();
        Result:=True;
      except on E:Exception do
        begin
          Error := TSqlSyntaxError.Create;
          Error.Message:=E.Message;
          Result:=False;
        end;
      end;

    finally
      Parser.Free;
    end;
  finally
    Script.Free;
    ScriptStream.Free;
    ResultList.Free;
  end;
end;

class procedure TDbUtils.ExecuteQuery(SqlQuery: string;
 DbInfo: TDbConnectionInfo);
var
 zcon:TZConnection;
 zquery:TZQuery;
 con:TSQLConnector;
 query:TSQLQuery;
begin
  case DbInfo.DatabaseType of
    dtMySql:
    begin
      ExecuteQuery(SqlQuery,DbInfo,deZeos);
    end;
    dtMsSql,dtOracle,dtSQLite,dtFirebirdd:
    begin
      ExecuteQuery(SqlQuery,DbInfo,deSqlDB);
    end;

  end;

end;

class procedure TDbUtils.ExecuteQuery(SqlQuery: string;
 DbInfo: TDbConnectionInfo; DbEngine: TDatabaseEngine);
var
 zcon:TZConnection;
 zquery:TZQuery;
 con:TSQLConnector;
 query:TSQLQuery;
begin
  case DbEngine of
    deZeos:
    begin
      try
        zcon := DbInfo.ToZeosConnection;
        zquery := TZQuery.Create(nil);
        zquery.Connection:=zcon;
        zquery.SQL.Text:=SqlQuery;
        zcon.Connect;
        zquery.ExecSQL;
      finally
        zquery.Free;
        zcon.Free;
      end;
    end;
    deSqlDB:
    begin
      try
      con:=DbInfo.ToSqlConnector;
      query:=TSQLQuery.Create(nil);
      query.DataBase:=con;
      query.Transaction:=con.Transaction;
      con.Open;
      query.SQL.Text:=SqlQuery;
      query.ExecSQL;
      con.Transaction.Commit;
      con.Close;
      finally
        query.Free;
        con.Free;
      end;
    end;

  end;

end;

class procedure TDbUtils.GetSchemas(DBInfo: TDbConnectionInfo;
 var list: TStringList);
var
 zcon:TZConnection;
 zquery:TZQuery;
begin
  if not Assigned(list) then exit;

  list.Clear;
  zcon := DBInfo.ToZeosConnection;
  zcon.Connect;
  try
    zquery:=TZQuery.Create(nil);
    zquery.Connection := zcon;

    case DBInfo.DatabaseType of
     dtMsSql:
     begin
        zquery.SQL.Text:= 'SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA';
        zquery.open;
        while not zquery.EOF do
        begin
          list.Add(zquery.Fields[0].AsString);
          zquery.Next;
        end;
     end;
     dtOracle: zcon.GetSchemaNames(list);
     dtMySql: list.Add(ZCon.Database);
     dtSQLite,dtFirebirdd: list.Add(ChangeFileExt(ExtractFileName(DbInfo.Database), ''));
    end;

  finally
    zquery.Free;
    zcon.Free;
  end;
end;

class procedure TDbUtils.GetTablenames(DbInfo: TDbConnectionInfo;
 schema: string; var list: TStringList);
var
 zcon:TZConnection;
 zquery:TZQuery;
begin

  if not Assigned(list) then exit;

  list.Clear;
  zcon := DBInfo.ToZeosConnection;
  try
    zcon.Connect;
    zcon.GetTableNames(schema,'',list);
  finally
    zcon.Free;
  end;

end;

class procedure TDbUtils.GetColumnNames(DbInfo: TDbConnectionInfo;
 tablename: string; var list: TStringList);
var
 zcon:TZConnection;
 zquery:TZQuery;
begin

  if not Assigned(list) then exit;

  list.Clear;
  zcon := DBInfo.ToZeosConnection;
  try
    zcon.Connect;
    zcon.GetColumnNames(tablename,'',list);
  finally
    zcon.Free;
  end;

end;
{ TDbConnectionInfo }

procedure TDbConnectionInfo.SetDatabase(AValue: string);
begin
 if FDatabase=AValue then Exit;
 FDatabase:=AValue;
end;

procedure TDbConnectionInfo.SetDbType(AValue: TDatabaseType);
begin
 if FDbType=AValue then Exit;
 FDbType:=AValue;
end;

procedure TDbConnectionInfo.SetPassword(AValue: string);
begin
 if FPassword=AValue then Exit;
 FPassword:=AValue;
end;

procedure TDbConnectionInfo.SetPort(AValue: Integer);
begin
 if FPort=AValue then Exit;
 FPort:=AValue;
end;

procedure TDbConnectionInfo.SetServer(AValue: string);
begin
 if FServer=AValue then Exit;
 FServer:=AValue;
end;

procedure TDbConnectionInfo.SetUsername(AValue: string);
begin
 if FUsername=AValue then Exit;
 FUsername:=AValue;
end;

function TDbConnectionInfo.ToFullString: string;
begin
 Result:='aDbType='+ IntToStr(Integer(FDbType))+';aServer='+Server+';aDatabase='+FDatabase+';aUsername='+FUsername+';aPassword='+FPassword+';aPort='+IntToStr(FPort)+';';
end;

function TDbConnectionInfo.ToZeosConnection: TZConnection;
var
  con:TZConnection;
begin
   con := TZConnection.Create(nil);
   if FDbType = dtOracle then
     con.Database := TDbUtils.GetOracleDescriptor(Self)
   else if FDbType = dtMsSql then
   begin
     con.Database :=TAsStringUtils.WrapString(Database,TStringWrapType.swtBrackets);
   end else
   begin
     con.Database :=FDatabase;
   end;
   con.HostName := FServer;
   con.User := FUsername;
   con.Password := FPassword;
   con.Protocol := TDbUtils.DatabaseTypeAsString(FDbType, True);
   con.Port := FPort;

   Result:=con;
end;

function TDbConnectionInfo.ToSqlConnector: TSQLConnector;
var
 con:TSQLConnector;
 trans:TSQLTransaction;
begin
   con := TSQLConnector.Create(nil);
   trans := TSQLTransaction.Create(con);

   //if FDbType = dtMsSql then
   //begin
   //  con.DatabaseName :=TAsStringUtils.WrapString(Database,TStringWrapType.swtBrackets);
   //end else
   //begin
   //  con.DatabaseName :=FDatabase;
   //end;

   con.DatabaseName :=FDatabase;
   trans.Action:=caCommit;
   con.Transaction := trans;
   con.HostName := FServer;
   con.UserName := FUsername;
   con.Password := FPassword;
   con.ConnectorType := TDbUtils.DatabaseTypeAsConnectorType(FDbType);
   Result:=con;
end;

{ TDbConnectionInfos }

procedure TDbConnectionInfos.LoadFromFile(Filename: string);
var
 s:TDbConnectionInfo;
 I:Integer;
 lstLine,lstLines:TStringList;
begin

 if not FileExists(Filename) then
 Exit;

  lstLines := TStringList.Create;
  lstLine := TStringList.Create;

  try
    Clear;

    lstLines.LoadFromFile(Filename);

    for I:=0 to lstLines.Count-1 do
    begin

      lstLine.Delimiter:=';';
      lstLine.StrictDelimiter:=True;
      lstLine.DelimitedText :=lstLines[I];
      s := TDbConnectionInfo.Create;
      try
        s.DatabaseType:= TDatabaseType(StrToInt(lstLine.Values['aDbType']));
        s.Server:=lstLine.Values['aServer'];
        s.Database:=lstLine.Values['aDatabase'];
        s.Username:=lstLine.Values['aUsername'];
        s.Password:=lstLine.Values['aPassword'];
        s.Port:= StrToInt(lstLine.Values['aPort']);
        Add(s);
      except

      end;
    end;

  finally
    lstLines.Free;
    lstLine.Free;
  end;
end;

procedure TDbConnectionInfos.SaveToFile(Filename: string);
var
 I: Integer;
 s:TDbConnectionInfo;
 lst:TStringList;
begin

  try
   lst := TStringList.Create;
   for I:=0 to Count-1 do
   begin
     s := Items[I];
     lst.Add(s.ToFullString);
   end;
   lst.SaveToFile(Filename);
  finally
    lst.Free;
  end;

end;

function TDbConnectionInfos.Exists(aDbInfo: TDbConnectionInfo): Boolean;
var
 I: Integer;
begin
  Result:=False;
 for I:=0 to Count-1 do
 begin
   if (aDbInfo.DatabaseType=Items[I].DatabaseType) and (LowerCase(aDbInfo.Server)=Lowercase(Items[I].Server)) and (LowerCase(aDbInfo.Database)=Lowercase(Items[I].Database))
   and (LowerCase(aDbInfo.Username)=Lowercase(Items[I].Username)) and (LowerCase(aDbInfo.Password)=Lowercase(Items[I].Password)) and (LowerCase(aDbInfo.Port)=Lowercase(Items[I].Port)) then
   begin
     Result:=True;
     Break;
   end;
 end;
end;

end.

