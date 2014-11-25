{
  *******************************************************************
  Version1 - Flakron Shkodra 2011
  Version2 - Flakron Shkodra 2014
  *******************************************************************
}

unit AsDbType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, types, fgl, strutils, AsStringUtils, RegExpr,
  ZConnection, ZDataset, sqldb, db, fpsqlparser, fpsqltree;

type

 TAsDatabaseType =
   (
    dtMsSql = 0,
    dtOracle = 1,
    dtMySql = 2,
    dtSQLite = 3,
    dtFirebirdd = 4
   );

 TAsDatabaseEngineType =
   (
    deNone=-1,
    deSqlDB=0,
    deZeos=1
   );



 { TAsSqlSyntaxError }

 TAsSqlSyntaxError = class
 private
   FMessage:string;
   procedure SetMessage(AValue: string);
 public
   Line:Integer;
   Position:Integer;
   property Message:string read FMessage write SetMessage;
 end;


 { TAsRegExUtils }

  TAsRegExUtils = class
  public
    {Exectues a regex in the given string and returns matches in stringList, if output var is NIL, the method initializes it}
    class procedure RunRegex(InputStr: string; RegEx: string; var Output: TStringList);
  end;



  { TAsDbConnectionInfo }

  TAsDbConnectionInfo = class(TComponent)
  private
   FDatabase: string;
   FDbEngine: TAsDatabaseEngineType;
   FDbType: TAsDatabaseType;
   FPassword: string;
   FPort: Integer;
   FServer: string;
   FUsername: string;
   FSqlCon:TSQLConnector;
   FTrans: TSQLTransaction;
   FZCon:TZConnection;
   FConnectionsCreated:Boolean;
   function GetIdentifier: string;
   function GetIsConnected: Boolean;
   procedure SetDatabase(AValue: string);
   procedure SetDbEngine(AValue: TAsDatabaseEngineType);
   procedure SetDbType(AValue: TAsDatabaseType);
   procedure SetPassword(AValue: string);
   procedure SetPort(AValue: Integer);
   procedure SetServer(AValue: string);
   procedure SetUsername(AValue: string);
    {creates a new instance of Zeos connection according to properties of this class}
    //function ToZeosConnection:TZConnection;
    {creates a new instance of SqlDbConnector according to properties of this class}
    //function ToSqlConnector:TSQLConnector;
  public
    {Constructor: CreateConnections set to false when sqlcon and zcon not needed to be created, for instance for RecentConnections}
    constructor Create(CreateConnections:Boolean=true; AOwner:TComponent=nil);
    destructor Destroy;override;
    {returns a string made up of all properties, could be used for easy compare}
    function ToFullString:string;

    procedure Assign(Source: TPersistent); override;

    procedure Open;
    procedure Close;
  published

    {returns Server+Database+DbType as string processed by AsDbUtils.GetSafeName meaning no symbols or spaces included}
    property Identifier:string read GetIdentifier;
    {Database type dtMsSql,dtOracle,dtMySql,dtFirebird,dtSqlite}
    property DbType:TAsDatabaseType read FDbType write SetDbType;
    {Server name or host}
    property Server:string read FServer write SetServer;
    {Database name}
    property Database:string read FDatabase write SetDatabase;
    {Server Login Username}
    property Username:string read FUsername write SetUsername;
    {Server Login Password}
    property Password:string read FPassword write SetPassword;
    {Server Port}
    property Port:Integer read FPort write SetPort;
    {DatabaseEngine [deSqlDB or deZeos]}
    property DbEngineType:TAsDatabaseEngineType read FDbEngine write SetDbEngine;

    property SqlConnection:TSQLConnector read FSqlCon;

    property ZeosConnection:TZConnection read FZCon;

    property Connected:Boolean read GetIsConnected;

  end;

  { TAsDbConnectionInfos }

  TAsDbConnectionInfos = class (specialize TFPGObjectList<TAsDbConnectionInfo>)
  public
   {Loads connection info from file}
    procedure LoadFromFile(Filename:string);
    {saves connection info to file}
    procedure SaveToFile(Filename:string);
    {compares given connection to other items in the list and returns true if found}
    function Exists(aDbInfo:TAsDbConnectionInfo):Boolean;
  end;

  { TAsQuery }

  TAsQuery = class(TComponent)
  private
    FZCon:TZConnection;
    FCon:TSQLConnector;
    FZQuery:TZQuery;
    FQuery:TSQLQuery;
    FDBInfo:TAsDbConnectionInfo;
    function GetDataSet: TDataSet;
    function GetEof: Boolean;
    function GetField(Index: Integer): TField;
    function GetFieldCount: Integer;
    function GetFilter: string;
    function GetFiltered: Boolean;
    function GetFilterOption: TFilterOptions;
    function GetIsActive: Boolean;
    function GetSQL: TStrings;
    function GetState: TDataSetState;
    procedure SetFilter(AValue: string);
    procedure SetFiltered(AValue: Boolean);
    procedure SetFilterOption(AValue: TFilterOptions);
    procedure AfterDelete(DataSet: TDataSet);
    procedure AfterPost(DataSet: TDataSet);
  public
   constructor Create(dbInfo:TAsDbConnectionInfo);
   destructor Destroy;override;
   procedure Open(sql:string);overload;
   procedure Open;
   procedure Close;
   procedure Next;
   procedure First;
   procedure Last;
   procedure Prior;
   procedure Insert;
   procedure Post;
   procedure Refresh;
   procedure ExecSQL;
   procedure Edit;
   procedure ApplyUpdates;
   function FieldByName(FieldName:string):TField;
   property DataSet:TDataSet read GetDataSet;
   property EOF:Boolean read GetEof;
   property Fields[Index:Integer]:TField read GetField;
   property FieldCount:Integer read GetFieldCount;
   property SQL:TStrings read GetSQL;
   property Active:Boolean read GetIsActive;
   property State:TDataSetState read GetState;
   property FilterOptions:TFilterOptions read GetFilterOption write SetFilterOption;
   property Filtered:Boolean read GetFiltered write SetFiltered;
   property Filter:string read GetFilter write SetFilter;

  end;

  TAsForeignKey = class
  public
    Schema:string;
    Table_Name:string;
    Column_Name:string;
    Foreign_Schema:string;
    Foreign_Table:string;
    Foreign_Column:string;
  end;

  TAsColumn = class
  public
   Column_Name:string;
   Data_Type:string;
   Allow_Null:Boolean;
   Max_Length:integer;
   Data_Precision:Integer;
   Data_Scale:Integer;
  end;

  TAsIndex = class
  public
   Index_Name:string;
   Column_Name:string;
   Descend:string;
  end;

  TAsTrigger = class
  public
    Trigger_Owner:string;
    Trigger_Name:string;
    Trigger_Event:string;
    Trigger_Body:string;
    Trigger_Status:string;
  end;

  TAsProcedureParam = class
  public
   Param_name:string;
   Param_Type:string;
   Data_Type:string;
   Max_Length:Integer;
  end;

  TAsForeignKeys = specialize TFPGObjectList<TAsForeignKey>;

  TAsColumns = specialize TFPGObjectList<TAsColumn>;

  TAsIndexes = specialize TFPGObjectList<TAsIndex>;

  TAsTriggers = specialize TFPGObjectList<TAsTrigger>;

  TAsProcedureParams = specialize TFPGObjectList<TAsProcedureParam>;

  { IAsDbMetadata }

  IAsDbMetadata = interface (IInterface)
    function GetSchemas:TStringList;
    function GetTablenames(schema:string):TStringList;
    function GetPrimaryKeys(Schema,TableName: string):TStringList;
    function GetForeignKeys(Schema,TableName: string):TAsForeignKeys;
    function GetColumns(Schema,TableName:string):TAsColumns;
    function GetIndexes(Schema,TableName:string):TAsIndexes;
    function GetTriggers(Schema,TableName:string):TAsTriggers;
    function GetProcedureNames(Schema:string):TStringList;
    function GetProcedureParams(ProcedureName:string):TAsProcedureParams;
    function GetCatalogNames:TStringList;
  end;

  { TAsDbUtils }

 TAsDbUtils = object
  private
    class function MakeEngine(DbInfo:TAsDbConnectionInfo):IAsDbMetadata;
    class procedure DisposeEngine(dbinfo:TAsDbConnectionInfo; a:IAsDbMetadata);
  public
    {this was supposed to 'know' the difference between GUID percieved as BLOB and a real BLOB}
    class function IsBlobGUID(var aField: TField): boolean;experimental;

    {Returns an sql query limiting number of records depending dbtype}
    class function GetTopRecordsSelect(dbtyp:TAsDatabaseType; TableName:string;NumberOfRecords:integer):string;overload;

    {Returns an sql query for a single column limiting number of records depending dbtype}
    class function GetTopRecordsSelect(dbtyp:TAsDatabaseType; FieldName,TableName:string;NumberOfRecords:integer):string;overload;

    {Surrounds given expression with relevant symbols ie [],"",or '' according to db}
    class function SafeWrap(dbtyp:TAsDatabaseType; TableOrField:string):string;

    class function GetOracleDescriptor(server, database: string; port: integer): string;overload;

    {Gets OracleDescriptor as in TNS file }
    class function GetOracleDescriptor(dbInfo:TAsDbConnectionInfo): string;overload;

    {Converts TAsDatabaseType to relevant SqlConnector.ConnectorType }
    class function DatabaseTypeAsConnectorType(DbType:TAsDatabaseType):string;

    {Converts TAsDatabaseType to relevant zeos protocol in string format}
    class function DatabaseTypeAsString(DbType:TAsDatabaseType;ALowerCase:Boolean):string;

    {Converts zeos protocol to  TAsDatabaseType }
    class function DatabaseTypeFromString(DbTypeString:string):TAsDatabaseType;

    {Converts zeos protocol to  sqldb connectorType}
    class function ZeosProtocolToSqlDbConnectorType(ZeosProtocol:string):string;

    {Checks syntax using TSQLParser}
    class function CheckSqlSyntax(SqlCommand:string; out Error:TAsSqlSyntaxError):Boolean;


    {Executes SQL Command using SQLDB or ZEOS Engine and given DBConnectionInfo}
    class procedure ExecuteQuery(SqlQuery:string; DbInfo:TAsDbConnectionInfo);

    {Gets db schemas using Zeos}
    class function GetSchemas(DBInfo:TAsDbConnectionInfo):TStringList;

    {Gets db Tables for given scheme}
    class function GetTablenames(DbInfo:TAsDbConnectionInfo; schema:string):TStringList;

    {Gets column names from db}
    class function GetColumnNames(DbInfo:TAsDbConnectionInfo; TableName:string):TStringList;

    {Gets primary keys of the given table}
    class function GetPrimaryKeys(DbInfo:TAsDbConnectionInfo; Schema,TableName: string):TStringList;

    {Gets Foreign Keys of the giben table }
    class function GetForeignKeys(DbInfo:TAsDbConnectionInfo; Schema,TableName: string):TAsForeignKeys;

    {Gets basic column info for the given table}
    class function GetColumns(DbInfo:TAsDbConnectionInfo; Schema,TableName:string):TAsColumns;

    {Gets info about indexes of the given table}
    class function GetIndexes(DbInfo:TAsDbConnectionInfo; Schema,TableName:string):TAsIndexes;

    {Gets all string/text columns from the given table}
    class function GetTextFields(DbInfo:TAsDbConnectionInfo; Schema,TableName:string):TStringList;

    {Gets basic trigger information about the given table}
    class function GetTriggers(DbInfo:TAsDbConnectionInfo; Schema,TableName:string):TAsTriggers;

    {Gets procedure names from the given dbInfo}
    class function GetProcedureNames(DbInfo:TAsDbConnectionInfo; Schema:string):TStringList;

    {Gets paramteres a given procedure}
    class function GetProcedureParams(DbInfo:TAsDbConnectionInfo; ProcedureName:string):TAsProcedureParams;

    {gets database list from the server}
    class function GetCatalogNames(aDbInfo:TAsDbConnectionInfo):TStringList;

  end;


implementation

uses
  AsMssqlMetadata,
  AsOracleMetadata,
  AsMySqlMetadata,
  AsFirebirdMetadata,
  AsSqliteMetadata;

{ TAsRegExUtils }

class procedure TAsRegExUtils.RunRegex(InputStr: string; RegEx: string;
 var Output: TStringList);
var
  r: TRegExpr;
begin

  if Output = nil then
    Output := TStringList.Create;

  try

    r := TRegExpr.Create;
    r.InputString := InputStr;
    r.Expression := RegEx;
    r.Compile;
    if r.Exec(InputStr) then
      repeat
        Output.Add(r.Match[0]);
      until not r.ExecNext;

  finally
    r.Free;
  end;

end;

{ TAsQuery }

procedure TAsQuery.AfterDelete(DataSet: TDataSet);
begin
  FQuery.ApplyUpdates(0);
  FCon.Transaction.CommitRetaining;
end;

procedure TAsQuery.AfterPost(DataSet: TDataSet);
begin
  FQuery.ApplyUpdates(0);
  FCon.Transaction.CommitRetaining;
end;

function TAsQuery.GetDataSet: TDataSet;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result:=FZQuery;
   deSqlDB:Result:=FQuery;
 end;
end;

function TAsQuery.GetEof: Boolean;
begin
 Result := DataSet.EOF;
end;

function TAsQuery.GetField(Index: Integer): TField;
begin
 Result:=DataSet.Fields[Index];
end;

function TAsQuery.GetFieldCount: Integer;
begin
 Result:=0;
 if DataSet<>nil then
 Result := DataSet.FieldCount;
end;

function TAsQuery.GetFilter: string;
begin
 Result := DataSet.Filter;
end;

function TAsQuery.GetFiltered: Boolean;
begin
  Result := DataSet.Filtered;
end;

function TAsQuery.GetFilterOption: TFilterOptions;
begin
 Result := DataSet.FilterOptions;
end;

function TAsQuery.GetIsActive: Boolean;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result := FZQuery.Active;
   deSqlDB:Result := FZQuery.Active;
 end;
end;

function TAsQuery.GetSQL: TStrings;
begin
 case FDBInfo.DbEngineType of
   deZeos : Result :=  FZQuery.SQL;
   deSqlDB : Result := FQuery.SQL;
 end;
end;

function TAsQuery.GetState: TDataSetState;
begin
 Result := DataSet.State;
end;

procedure TAsQuery.SetFilter(AValue: string);
begin
 DataSet.Filter:=AValue;
end;

procedure TAsQuery.SetFiltered(AValue: Boolean);
begin
 DataSet.Filtered:=AValue;
end;

procedure TAsQuery.SetFilterOption(AValue: TFilterOptions);
begin
  DataSet.FilterOptions:= AValue;
end;

constructor TAsQuery.Create(dbInfo: TAsDbConnectionInfo);
begin
 inherited Create(nil);
  FDBInfo:=dbInfo;
  case FDBInfo.DbEngineType of
    deZeos:
     begin
       FZCon:=FDBInfo.ZeosConnection;
       FZQuery:=TZQuery.Create(nil);
       FZQuery.Connection:=FZCon;
     end;
     deSqlDB:
      begin
        FCon:=FDBInfo.SqlConnection;
        FQuery:=TSQLQuery.Create(nil);
        FQuery.DataBase:=FCon;
        FQuery.PacketRecords:=-1;
        FQuery.AfterPost:=@AfterPost;
        FQuery.AfterDelete:=@AfterDelete;
      end;
  end;

end;

destructor TAsQuery.Destroy;
begin
   inherited Destroy;
end;

procedure TAsQuery.Open(sql: string);
begin
   case FDBInfo.DbEngineType of
      deZeos:
       begin
         if not FZCon.Connected then FZCon.Connect;
         FZQuery.Close;
         FZQuery.SQL.Text:=sql;
         FZQuery.Open;
       end;
      deSqlDB:
       begin
         if not FCon.Connected then FCon.Open;
         FQuery.SQL.Text:=sql;
         FQuery.Close;
         FQuery.Open;
       end;
   end;
end;

procedure TAsQuery.Open;
begin
 case FDBInfo.DbEngineType of
    deZeos:
     begin
      if not FZCon.Connected then FZCon.Connect;
      FZQuery.Close;
      FZQuery.Open;
     end;
    deSqlDB:
     begin
      if not FCon.Connected then FCon.Open;
      FQuery.Close;
      FQuery.Open;
     end;
 end;
end;

procedure TAsQuery.Close;
begin
 case FDBInfo.DbEngineType of
    deZeos:   FZQuery.Close;
    deSqlDB:  FQuery.Close;
 end;
end;

procedure TAsQuery.Next;
begin
 DataSet.Next;
end;

procedure TAsQuery.First;
begin
 DataSet.First;
end;

procedure TAsQuery.Last;
begin
   DataSet.Last;
end;

procedure TAsQuery.Prior;
begin
 DataSet.Prior;
end;

procedure TAsQuery.Insert;
begin
 DataSet.Insert;
end;

procedure TAsQuery.Post;
begin
 case FDBInfo.DbEngineType of
    deZeos:FZQuery.Post;
    deSqlDB: FQuery.Post;
 end;
end;

procedure TAsQuery.Refresh;
begin
 DataSet.Refresh;
end;

procedure TAsQuery.ExecSQL;
begin
 case FDBInfo.DbEngineType of
    deZeos:FZQuery.ExecSQL;
    deSqlDB:
    begin
      FQuery.ExecSQL;
      FCon.Transaction.CommitRetaining;
    end;
 end;
end;

procedure TAsQuery.Edit;
begin
 DataSet.Edit;
end;

procedure TAsQuery.ApplyUpdates;
begin
 if FDBInfo.DbEngineType=deSqlDB then
 FQuery.ApplyUpdates;
end;

function TAsQuery.FieldByName(FieldName: string): TField;
begin
 Result := DataSet.FieldByName(FieldName);
end;

{ TAsSqlSyntaxError }

procedure TAsSqlSyntaxError.SetMessage(AValue: string);
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
  if lst.Count>=3 then
  begin
    strLineNo :=TAsStringUtils.ExtractNumbers(lst[2]);
    if lst.Count >= 4 then
    strPosNo:=TAsStringUtils.ExtractNumbers(lst[4]);
  end;

  TryStrToInt(strLineNo,Line);
  TryStrToInt(strPosNo,Position);

 finally
   lst.Free;
 end;

 end;

end;

{ TAsDbUtils }

class function TAsDbUtils.MakeEngine(DbInfo: TAsDbConnectionInfo
 ): IAsDbMetadata;
begin
 case DbInfo.DbType of
    dtMsSql: Result := TAsMssqlMetadata.Create(DbInfo);
    dtOracle: Result := TAsOracleMetadata.Create(DbInfo);
    dtMySql: Result := TAsMySqlMetadata.Create(DbInfo);
    dtFirebirdd: Result := TAsFirebirdMetadata.Create(DbInfo);
    dtSQLite: Result := TAsSqliteMetadata.Create(DbInfo);
 end;
end;

class procedure TAsDbUtils.DisposeEngine(dbinfo: TAsDbConnectionInfo;
 a: IAsDbMetadata);
begin
 case DBInfo.DbType of
    dtMsSql : TAsMssqlMetadata(a).Free;
    dtOracle : TAsOracleMetadata(a).Free;
    dtMySql : TAsMySqlMetadata(a).Free;
    dtFirebirdd : TAsFirebirdMetadata(a).Free;
    dtSQLite : TAsSqliteMetadata(a).Free;
 end;
end;

class function TAsDbUtils.IsBlobGUID(var aField: TField): boolean;
var
  m: TMemoryStream;
  b: TBlobField;
  f:TFieldType;
begin

  if aField<>nil then
  begin
    try

      if aField.DataType = ftUnknown then
      exit;
       if aField.DataType in [ftBlob,ftOraBlob,ftOraClob] then
       b := aField as TBlobField;

       try

         if Assigned(b) then
         begin
          m := TMemoryStream.Create;
          try

            b.SaveToStream(m);
            Result := m.Size = 16;
          except
          end;
         end;

       finally
         m.Free;
       end;

    except
      Result := False;
    end;

  end;
end;

class function TAsDbUtils.GetTopRecordsSelect(dbtyp: TAsDatabaseType;
 TableName: string; NumberOfRecords: integer): string;
begin
 case dbtyp of
  dtMsSql:Result :='SELECT TOP '+IntToStr(NumberOfRecords)+' * FROM ['+TableName+']';
  dtOracle:Result := 'SELECT * FROM "'+TableName+'" WHERE ROWNUM <= '+IntToStr(NumberOfRecords);
  dtMySql,dtSQLite: Result:='SELECT * FROM '+TableName+' LIMIT '+IntToStr(NumberOfRecords);
  dtFirebirdd: Result:='SELECT FIRST '+IntToStr(NumberOfRecords)+' * FROM '+TableName;
 end;
end;

class function TAsDbUtils.GetTopRecordsSelect(dbtyp: TAsDatabaseType;
 FieldName, TableName: string; NumberOfRecords: integer): string;
begin
 case dbtyp of
  dtMsSql:Result :='SELECT TOP '+IntToStr(NumberOfRecords)+' ['+FieldName+'] FROM ['+TableName+']';
  dtOracle:Result := 'SELECT "'+FieldName+'" FROM "'+TableName+'" WHERE ROWNUM <= '+IntToStr(NumberOfRecords);
  dtMySql,dtSQLite: Result:='SELECT '+FieldName+' FROM '+TableName+' LIMIT '+IntToStr(NumberOfRecords);
  dtFirebirdd: Result:='SELECT FIRST '+IntToStr(NumberOfRecords)+' "'+FieldName+'" FROM '+TableName;
 end;
end;

class function TAsDbUtils.SafeWrap(dbtyp: TAsDatabaseType; TableOrField: string
 ): string;
begin
 case dbtyp of
  dtMsSql,dtSQLite:Result:='['+TableOrField+']';
  dtMySql:Result:=TableOrField;
  dtOracle,dtFirebirdd:Result:='"'+TableOrField+'"';
 end;
end;

class function TAsDbUtils.GetOracleDescriptor(server, database: string;
 port: integer): string;
begin
 Result := '(DESCRIPTION = (ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = ' +
      server + ')(PORT = ' + IntToStr(port) +
      '))) (CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME = ' +
     database + ')))';
end;

class function TAsDbUtils.GetOracleDescriptor(dbInfo: TAsDbConnectionInfo): string;
begin
 Result := GetOracleDescriptor(dbInfo.Server,dbInfo.Database,dbInfo.Port);
end;

class function TAsDbUtils.DatabaseTypeAsConnectorType(DbType: TAsDatabaseType
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

class function TAsDbUtils.DatabaseTypeAsString(DbType: TAsDatabaseType;
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

class function TAsDbUtils.DatabaseTypeFromString(DbTypeString: string
 ): TAsDatabaseType;
var
  s:string;
  I: Integer;
begin

  for I:=0 to 4 do
  begin
    s := GetEnumName(TypeInfo(TAsDatabaseType),I);
    s := Copy(s,3,Length(s));

    if lowercase(s) = lowercase(TAsStringUtils.RemoveChars(DbTypeString,['-','3','2','.','5'])) then
    begin
      Result := TAsDatabaseType(I);
      Break;
    end;
  end;

end;

class function TAsDbUtils.ZeosProtocolToSqlDbConnectorType(ZeosProtocol: string
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

class function TAsDbUtils.CheckSqlSyntax(SqlCommand: string; out
 Error: TAsSqlSyntaxError): Boolean;
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
          Error := TAsSqlSyntaxError.Create;
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

class procedure TAsDbUtils.ExecuteQuery(SqlQuery: string;
 DbInfo: TAsDbConnectionInfo);
var
 query:TAsQuery;
begin
  query := TAsQuery.Create(DbInfo);
  try
    query.SQL.Text:=SqlQuery;
    query.ExecSQL;
  finally
    query.Free;
  end;
end;

class function TAsDbUtils.GetSchemas(DBInfo: TAsDbConnectionInfo): TStringList;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetSchemas;
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetTablenames(DbInfo: TAsDbConnectionInfo;
 schema: string): TStringList;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetTablenames(schema);
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetColumnNames(DbInfo: TAsDbConnectionInfo;
 TableName: string): TStringList;
var
 ds:TAsQuery;
 I: Integer;
begin
  Result := TStringList.Create;
  try
     ds := TAsQuery.Create(DbInfo);
     try
      ds.Open(GetTopRecordsSelect(DbInfo.DbType,TableName,1));
      for I:=0 to ds.FieldCount-1 do
       begin
        Result.Add(ds.Fields[I].FieldName);
        ds.Next;
       end;
     finally
        ds.Free;
     end;
  except on e:Exception do
    begin
      Result.Free;
      raise e;
    end;
  end;
end;

class function TAsDbUtils.GetPrimaryKeys(DbInfo: TAsDbConnectionInfo; Schema,
 TableName: string): TStringList;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetPrimaryKeys(schema,TableName);
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetForeignKeys(DbInfo: TAsDbConnectionInfo; Schema,
 TableName: string): TAsForeignKeys;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetForeignKeys(schema,TableName);
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetColumns(DbInfo: TAsDbConnectionInfo; Schema,
 TableName: string): TAsColumns;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetColumns(schema,TableName);
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetIndexes(DbInfo: TAsDbConnectionInfo; Schema,
 TableName: string): TAsIndexes;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetIndexes(schema,TableName);
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetTextFields(DbInfo: TAsDbConnectionInfo; Schema,
 TableName: string): TStringList;
var
  ds:TAsQuery;
  I: Integer;
begin
 Result := TStringList.Create;

 ds := TAsQuery.Create(DbInfo);
 try
  try
    ds.Open(GetTopRecordsSelect(DbInfo.DbType,TableName,1));
    for I:=0 to ds.FieldCount-1 do
    begin
      if ds.Fields[I].DataType in [ftString,ftWideString] then
      Result.Add(ds.Fields[I].FieldName);
    end;
  except
  end;
 finally
  ds.Free;
 end;

end;

class function TAsDbUtils.GetTriggers(DbInfo: TAsDbConnectionInfo; Schema,
 TableName: string): TAsTriggers;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetTriggers(schema,TableName);
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetProcedureNames(DbInfo: TAsDbConnectionInfo;
 Schema: string): TStringList;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetProcedureNames(schema);
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetProcedureParams(DbInfo: TAsDbConnectionInfo;
 ProcedureName: string): TAsProcedureParams;
var
 md:IAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  Result := md.GetProcedureParams(ProcedureName);
  DisposeEngine(DbInfo,md);
end;

class function TAsDbUtils.GetCatalogNames(aDbInfo: TAsDbConnectionInfo
 ): TStringList;
var
 md:IAsDbMetadata;
begin
   md := MakeEngine(aDbInfo);
   Result := md.GetCatalogNames;
   DisposeEngine(aDbInfo,md);
end;

{ TAsDbConnectionInfo }

procedure TAsDbConnectionInfo.SetDatabase(AValue: string);
begin
 FDatabase:=AValue;
 if FConnectionsCreated then
 begin
   case FDBType of
       dtOracle: FZCon.Database := TAsDbUtils.GetOracleDescriptor(Self);
       dtMsSql: FZCon.Database := TAsStringUtils.WrapString(Database,TAsStringWrapType.swtBrackets);
   else
       FZCon.Database := FDatabase;
   end;
   FSqlCon.DatabaseName:= FDatabase;
 end;

end;

function TAsDbConnectionInfo.GetIdentifier: string;
var
 s:string;
begin
 if FDbType=dtSQLite then
  s := TAsStringUtils.GetSafeName(ExtractFileName(FDatabase)+ IntToStr(Integer(FDbType)))
 else
  s := TAsStringUtils.GetSafeName(FServer+FDatabase+ IntToStr(Integer(FDbType)));
 Result :='ID'+s;
end;

function TAsDbConnectionInfo.GetIsConnected: Boolean;
begin
  if FConnectionsCreated then
  begin
    Result := FSqlCon.Connected or FZCon.Connected;
  end;
end;

procedure TAsDbConnectionInfo.SetDbEngine(AValue: TAsDatabaseEngineType);
begin
 if FDbEngine=AValue then Exit;
 FDbEngine:=AValue;
end;

procedure TAsDbConnectionInfo.SetDbType(AValue: TAsDatabaseType);
begin
 FDbType:=AValue;
 if FConnectionsCreated then
 begin
  FSqlCon.ConnectorType:= TAsDbUtils.DatabaseTypeAsConnectorType(FDbType);
  FZCon.Protocol := TAsDbUtils.DatabaseTypeAsString(FDbType, True);
 end;
end;

procedure TAsDbConnectionInfo.SetPassword(AValue: string);
begin
 FPassword:=AValue;
 if FConnectionsCreated then
 begin
  FSqlCon.Password:=FPassword;
  FZCon.Password:=FPassword;
 end;
end;

procedure TAsDbConnectionInfo.SetPort(AValue: Integer);
begin
 FPort:=AValue;
 if FConnectionsCreated then
 begin
  FZCon.Port:=FPort;
//
//  if FPort<>0 then
//  if FSqlCon.HostName<>EmptyStr then
//  begin
//   if not AnsiContainsText(FSqlCon.HostName,':') then
//   FSqlCon.HostName:=FSqlCon.HostName+':'+IntToStr(FPort);
//  end;
 end;
end;

procedure TAsDbConnectionInfo.SetServer(AValue: string);
begin
 FServer:=AValue;
 if FConnectionsCreated then
 begin
  FSqlCon.HostName:= FServer;
  FZCon.HostName:=FServer;
 end;
end;

procedure TAsDbConnectionInfo.SetUsername(AValue: string);
begin
 FUsername:=AValue;
 if FConnectionsCreated then
 begin
  FSqlCon.UserName:=FUsername;
  FZCon.User:=FUsername;
 end;
end;

constructor TAsDbConnectionInfo.Create(CreateConnections: Boolean;
 AOwner: TComponent);
begin
 inherited Create(AOwner);
 FConnectionsCreated:= CreateConnections;

 if CreateConnections then
 begin
  FSqlCon:=TSQLConnector.Create(nil);
  FTrans := TSQLTransaction.Create(FSqlCon);
  FSqlCon.Transaction:=FTrans;
  FZCon := TZConnection.Create(nil);
 end;

end;

destructor TAsDbConnectionInfo.Destroy;
begin
 if FConnectionsCreated then
 begin
 FSqlCon.Destroy;
 FZCon.Destroy;
 end;
 inherited Destroy;
end;

function TAsDbConnectionInfo.ToFullString: string;
begin
 Result:='aDbType='+ IntToStr(Integer(FDbType))+';aServer='+Server+';aDatabase='+FDatabase+';aUsername='+FUsername+';aPassword='+FPassword+';aPort='+IntToStr(FPort)+';';
end;

//function TAsDbConnectionInfo.ToZeosConnection: TZConnection;
//var
//  con:TZConnection;
//begin
//   con := TZConnection.Create(nil);
//   case FDBType of
//     dtOracle: con.Database := TAsDbUtils.GetOracleDescriptor(Self);
//     dtMsSql: con.Database := TAsStringUtils.WrapString(Database,TAsStringWrapType.swtBrackets);
//   else
//     con.Database := FDatabase;
//   end;
//   con.HostName := FServer;
//   con.User := FUsername;
//   con.Password := FPassword;
//   con.Protocol := TAsDbUtils.DatabaseTypeAsString(FDbType, True);
//   con.Port := FPort;
//
//   Result:=con;
//end;
//
//function TAsDbConnectionInfo.ToSqlConnector: TSQLConnector;
//var
// con:TSQLConnector;
// trans:TSQLTransaction;
//begin
//   con := TSQLConnector.Create(nil);
//   trans := TSQLTransaction.Create(con);
//
//   con.DatabaseName :=FDatabase;
//   trans.Action:=caCommit;
//   con.Transaction := trans;
//   con.HostName := FServer;
//   con.UserName := FUsername;
//   con.Password := FPassword;
//   con.ConnectorType := TAsDbUtils.DatabaseTypeAsConnectorType(FDbType);
//   Result:=con;
//end;

procedure TAsDbConnectionInfo.Assign(Source: TPersistent);
begin
 if Source is TAsDbConnectionInfo then
 begin
  FDatabase:=TAsDbConnectionInfo(Source).Database;
  FDbEngine:= TAsDbConnectionInfo(Source).DbEngineType;
  FDbType:=TAsDbConnectionInfo(Source).DbType;
  FServer:=TAsDbConnectionInfo(Source).Server;
  FUsername:=TAsDbConnectionInfo(Source).Username;
  FPassword:=TAsDbConnectionInfo(Source).Password;
  FPort:=TAsDbConnectionInfo(Source).Port;
  if TAsDbConnectionInfo(Source).SqlConnection <> nil then
  begin
    if FSqlCon=nil then
      FSqlCon := TSQLConnector.Create(Self);

    FSqlCon.HostName := TAsDbConnectionInfo(Source).SqlConnection.HostName;
    FSqlCon.ConnectorType:=TAsDbConnectionInfo(Source).SqlConnection.ConnectorType;
    FSqlCon.DatabaseName:=TAsDbConnectionInfo(Source).SqlConnection.DatabaseName;
    FSqlCon.UserName:=TAsDbConnectionInfo(Source).SqlConnection.UserName;
    FSqlCon.Password:=TAsDbConnectionInfo(Source).SqlConnection.Password;
  end;

  if TAsDbConnectionInfo(Source).ZeosConnection<>nil then
  begin
    if FZCon=nil then
      FZCon := TZConnection.Create(Self);

    FZCon.HostName := TAsDbConnectionInfo(Source).Server;
    FZCon.User:=TAsDbConnectionInfo(Source).Username;
    FZCon.Password:=TAsDbConnectionInfo(Source).Password;
    FZCon.Protocol:=TAsDbUtils.DatabaseTypeAsString(TAsDbConnectionInfo(Source).DbType,true);
    FZCon.Port:=TAsDbConnectionInfo(Source).Port;
    case FDBType of
     dtOracle: FZCon.Database := TAsDbUtils.GetOracleDescriptor(Self);
     dtMsSql: FZCon.Database := TAsStringUtils.WrapString(Database,TAsStringWrapType.swtBrackets);
    else
      FZCon.Database := FDatabase;
    end;
  end;

 end else
 inherited Assign(Source);
end;

procedure TAsDbConnectionInfo.Open;
begin
 case FDbEngine of
   deSqlDB:FSqlCon.Open;
   deZeos:FZCon.Connect;
 end;
end;

procedure TAsDbConnectionInfo.Close;
begin
 case FDbEngine of
   deSqlDB:FSqlCon.Close();
   deZeos:FZCon.Disconnect;
 end;
end;

{ TAsDbConnectionInfos }

procedure TAsDbConnectionInfos.LoadFromFile(Filename: string);
var
 s:TAsDbConnectionInfo;
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
      s := TAsDbConnectionInfo.Create(False);
      try
        s.DbType:= TAsDatabaseType(StrToInt(lstLine.Values['aDbType']));
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

procedure TAsDbConnectionInfos.SaveToFile(Filename: string);
var
 I: Integer;
 s:TAsDbConnectionInfo;
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

function TAsDbConnectionInfos.Exists(aDbInfo: TAsDbConnectionInfo): Boolean;
var
 I: Integer;
begin
  Result:=False;
 for I:=0 to Count-1 do
 begin
   if (aDbInfo.DbType=Items[I].DbType) and (LowerCase(aDbInfo.Server)=Lowercase(Items[I].Server)) and (LowerCase(aDbInfo.Database)=Lowercase(Items[I].Database))
   and (LowerCase(aDbInfo.Username)=Lowercase(Items[I].Username)) and (LowerCase(aDbInfo.Password)=Lowercase(Items[I].Password)) and (LowerCase(aDbInfo.Port)=Lowercase(Items[I].Port)) then
   begin
     Result:=True;
     Break;
   end;
 end;
end;

end.


