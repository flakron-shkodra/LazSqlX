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
  ZConnection, ZDataset, sqldb, db, fpsqlparser, fpsqltree,sqldblib;

type

 TAsDatabaseType =
   (
    dtMsSql = 0,
    dtOracle = 1,
    dtMySql = 2,
    dtSQLite = 3,
    dtFirebirdd = 4,
    dtPostgreSql = 5
   );

 TAsDatabaseEngineType =
   (
    deNone=-1,
    deSqlDB=0,
    deZeos=1
   );



 { TAsSyntaxError }

 TAsSyntaxError = class
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

  TAsDbConnectionInfo = class(TPersistent)
  private
   FDatabase: string;
   FDbEngine: TAsDatabaseEngineType;
   FDbType: TAsDatabaseType;
   FPassword: string;
   FPort: Integer;
   FSchema: string;
   FServer: string;
   FUsername: string;
   FLibraryLocation: string;
   FSqlCon:TSQLConnector;
   FTrans: TSQLTransaction;
   FLibraryLoader:TSQLDBLibraryLoader;
   FZCon:TZConnection;
   FConnectionsCreated:Boolean;
   function GetIdentifier: string;
   function GetIsConnected: Boolean;
   function GetLibLocation: String;
   function GetProperties: TStrings;
   procedure Instantiate;
   procedure SetDatabase(AValue: string);
   procedure SetDbEngine(AValue: TAsDatabaseEngineType);
   procedure SetDbType(AValue: TAsDatabaseType);
   procedure SetLibLocation(AValue: String);
   procedure SetPassword(AValue: string);
   procedure SetPort(AValue: Integer);
   procedure SetProperties(AValue: TStrings);
   procedure SetSchema(AValue: String);
   procedure SetServer(AValue: string);
   procedure SetUsername(AValue: string);
   function GetConnectionsCreated:Boolean;
   procedure AssignProperties;
  public
    {Constructor: CreateConnections set to false when sqlcon and zcon not needed to be created, for instance for RecentConnections}
    constructor Create(CreateConnections: Boolean=True);
    destructor Destroy;override;
    {returns a string made up of all properties, used for save/load and then parse recent connections to external file }
    function ToFullString:string;

    procedure Assign(Source: TPersistent); override;

    procedure Open;
    procedure Close;
    procedure StartTransaction;
    procedure Rollback;
    procedure Commit;
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

    property Properties:TStrings read GetProperties write SetProperties;

    property Schema:String read FSchema write SetSchema;

    property LibraryLocation:String read GetLibLocation write SetLibLocation;

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
    function GetAfterDelete: TDataSetNotifyEvent;
    function GetAfterRefresh: TDataSetNotifyEvent;
    function GetAfterSave: TDataSetNotifyEvent;
    function GetAfterScroll: TDataSetNotifyEvent;
    function GetBeforeDelete: TDataSetNotifyEvent;
    function GetBeforeSave: TDataSetNotifyEvent;
    function GetDataSet: TDataSet;
    function GetEof: Boolean;
    function GetField(Index: Integer): TField;
    function GetFieldCount: Integer;
    function GetFilter: string;
    function GetFiltered: Boolean;
    function GetFilterOption: TFilterOptions;
    function GetIndexname: string;
    function GetIsActive: Boolean;
    function GetPacketRecord: Integer;
    function GetRecNo: Integer;
    function GetRecordCount: Integer;
    function GetRowsAffected: Integer;
    function GetSQL: TStrings;
    function GetState: TDataSetState;
    procedure SetAfterDelete(AValue: TDataSetNotifyEvent);
    procedure SetAfterRefresh(AValue: TDataSetNotifyEvent);
    procedure SetAfterSave(AValue: TDataSetNotifyEvent);
    procedure SetAfterScroll(AValue: TDataSetNotifyEvent);
    procedure SetBeforeDelete(AValue: TDataSetNotifyEvent);
    procedure SetBeforeSave(AValue: TDataSetNotifyEvent);
    procedure SetFilter(AValue: string);
    procedure SetFiltered(AValue: Boolean);
    procedure SetFilterOption(AValue: TFilterOptions);
    procedure AfterDelete(DataSet: TDataSet);
    procedure AfterPost(DataSet: TDataSet);
    procedure SetIndexName(AValue: string);
    procedure SetPackedRecord(AValue: Integer);
    procedure SetRecNo(AValue: Integer);
    procedure SetSort(AValue: string);
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
   procedure DisableControls;
   procedure EnableControls;
   procedure AddIndex(const AName, AFields : string; AOptions : TIndexOptions; const ADescFields: string = ''; const ACaseInsFields: string = '');
   function GetFieldNames:TStringList;
   function FieldByName(FieldName:string):TField;
   property Active:Boolean read GetIsActive;
   property DataSet:TDataSet read GetDataSet;
   property EOF:Boolean read GetEof;
   property Fields[Index:Integer]:TField read GetField;
   property FieldCount:Integer read GetFieldCount;
   property SQL:TStrings read GetSQL;
   property State:TDataSetState read GetState;
   property FilterOptions:TFilterOptions read GetFilterOption write SetFilterOption;
   property Filtered:Boolean read GetFiltered write SetFiltered;
   property Filter:string read GetFilter write SetFilter;
   property OnBeforeSave:TDataSetNotifyEvent read GetBeforeSave write SetBeforeSave;
   property OnAfterSave:TDataSetNotifyEvent read GetAfterSave write SetAfterSave;
   property OnBeforeDelete:TDataSetNotifyEvent read GetBeforeDelete write SetBeforeDelete;
   property OnAfterDelete:TDataSetNotifyEvent read GetAfterDelete write SetAfterDelete;
   property OnAfterScroll:TDataSetNotifyEvent read GetAfterScroll write SetAfterScroll;
   property OnAfterRefresh:TDataSetNotifyEvent read GetAfterRefresh write SetAfterRefresh;
   property RecNo:Integer read GetRecNo write SetRecNo;
   property IndexName:string read GetIndexname write SetIndexName;
   property PacketRecords:Integer read GetPacketRecord write SetPackedRecord;
   property RowsAffected:Integer read GetRowsAffected;
   property RecordCount:Integer read GetRecordCount;

  end;

  TAsForeignKey = class
  public
    Constraint_Name:string;
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

   TAsDbMetadata = class (TInterfacedObject,IInterface)
    function GetSchemas:TStringList;virtual;abstract;
    function GetTablenames(schema:string):TStringList;virtual;abstract;
    function GetPrimaryKeys(Schema,TableName: string):TStringList;virtual;abstract;
    function GetForeignKeys(Schema,TableName: string):TAsForeignKeys;virtual;abstract;
    function GetColumns(Schema,TableName:string):TAsColumns;virtual;abstract;
    function GetIndexes(Schema,TableName:string):TAsIndexes;virtual;abstract;
    function GetTriggers(Schema,TableName:string):TAsTriggers;virtual;abstract;
    function GetProcedureNames(Schema:string):TStringList;virtual;abstract;
    function GetProcedureParams(ProcedureName:string):TAsProcedureParams;virtual;abstract;
    function GetCatalogNames:TStringList;virtual;abstract;
  end;

  { TAsDbUtils }

 TAsDbUtils = object
  private
   
    class function MakeEngine(DbInfo:TAsDbConnectionInfo):TAsDbMetadata;
    class procedure DisposeEngine(a:TAsDbMetadata);
  public
    {this was supposed to 'know' the difference between GUID percieved as BLOB and a real BLOB}
    class function IsBlobGUID(var aField: TField): boolean;experimental;

    {Returns an sql query limiting number of records depending dbtype}
    class function GetTopRecordsSelect(dbtyp:TAsDatabaseType; Schema,TableName:string;NumberOfRecords:integer):string;overload;

    {Returns an sql query for a single column limiting number of records depending dbtype}
    class function GetTopRecordsSelect(dbtyp:TAsDatabaseType; Schema,TableName,FieldName:string;NumberOfRecords:integer):string;overload;

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
    class function CheckSqlSyntax(SqlCommand:string; out Error:TAsSyntaxError):Boolean;


    {Executes SQL Command using SQLDB or ZEOS Engine and given DBConnectionInfo}
    class procedure ExecuteQuery(SqlQuery:string; DbInfo:TAsDbConnectionInfo);

    {Gets db schemas using Zeos}
    class function GetSchemas(DBInfo:TAsDbConnectionInfo):TStringList;

    {Gets db Tables for given scheme}
    class function GetTablenames(DbInfo:TAsDbConnectionInfo):TStringList;

    {Gets column names from db}
    class function GetColumnNames(DbInfo: TAsDbConnectionInfo; TableName: string): TStringList;

    {Gets primary keys of the given table}
    class function GetPrimaryKeys(DbInfo:TAsDbConnectionInfo; TableName: string):TStringList;

    {Gets Foreign Keys of the giben table }
    class function GetForeignKeys(DbInfo:TAsDbConnectionInfo; TableName: string):TAsForeignKeys;

    {Gets basic column info for the given table}
    class function GetColumns(DbInfo:TAsDbConnectionInfo; TableName:string):TAsColumns;

    {Gets info about indexes of the given table}
    class function GetIndexes(DbInfo:TAsDbConnectionInfo; TableName:string):TAsIndexes;

    {Gets all string/text columns from the given table}
    class function GetTextFields(DbInfo: TAsDbConnectionInfo; TableName: string): TStringList;

    {Gets basic trigger information about the given table}
    class function GetTriggers(DbInfo:TAsDbConnectionInfo; TableName:string):TAsTriggers;

    {Gets procedure names from the given dbInfo}
    class function GetProcedureNames(DbInfo:TAsDbConnectionInfo):TStringList;

    {Gets paramteres a given procedure}
    class function GetProcedureParams(DbInfo:TAsDbConnectionInfo; ProcedureName:string):TAsProcedureParams;

    {gets database list from the server}
    class function GetCatalogNames(aDbInfo:TAsDbConnectionInfo):TStringList;

    {returns formated sqlquery}
    class function FormatQuery(SqlQuery:string):String;

  end;


implementation

uses
  AsMssqlMetadata,
  AsOracleMetadata,
  AsMySqlMetadata,
  AsFirebirdMetadata,
  AsSqliteMetadata, AsPostgresMetadata;

{ TAsRegExUtils }

class procedure TAsRegExUtils.RunRegex(InputStr: string; RegEx: string;
 var Output: TStringList);
var
  r: TRegExpr;
begin

  if Output = nil then
    Output := TStringList.Create;

  try
    Output.Clear;
    r := TRegExpr.Create;
    r.InputString := InputStr;
    r.Expression := RegEx;
    r.LineSeparators:=#13#10;
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
  ApplyUpdates;
end;

procedure TAsQuery.AfterPost(DataSet: TDataSet);
begin
  ApplyUpdates;
end;

procedure TAsQuery.SetIndexName(AValue: string);
begin
  case FDBInfo.DbEngineType of
   deZeos:FZQuery.IndexFieldNames := AValue;
   deSqlDB:FQuery.IndexName := AValue;
  end;
end;

procedure TAsQuery.SetPackedRecord(AValue: Integer);
begin
 case FDBInfo.DbEngineType of
   deZeos:;
   deSqlDB:FQuery.PacketRecords:=AValue;
 end;
end;


procedure TAsQuery.SetRecNo(AValue: Integer);
begin
 case FDBInfo.DbEngineType of
   deZeos : FZQuery.RecNo:=AValue;
   deSqlDB : FQuery.RecNo:=AValue;
 end;
end;

procedure TAsQuery.SetSort(AValue: string);
begin

end;

function TAsQuery.GetDataSet: TDataSet;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result:=FZQuery;
   deSqlDB:Result:=FQuery;
 end;
end;

function TAsQuery.GetAfterSave: TDataSetNotifyEvent;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result:=FZQuery.AfterPost;
   deSqlDB:Result:=FQuery.AfterPost;
 end;
end;

function TAsQuery.GetAfterScroll: TDataSetNotifyEvent;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result:=FZQuery.AfterScroll;
   deSqlDB:Result:=FQuery.AfterScroll;
 end;
end;

function TAsQuery.GetAfterDelete: TDataSetNotifyEvent;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result:=FZQuery.AfterDelete;
   deSqlDB:Result:=FQuery.AfterDelete;
 end;
end;

function TAsQuery.GetAfterRefresh: TDataSetNotifyEvent;
begin
  case FDBInfo.DbEngineType of
   deZeos:Result:=FZQuery.AfterRefresh;
   deSqlDB:Result:=FQuery.AfterRefresh;
  end;
end;

function TAsQuery.GetBeforeDelete: TDataSetNotifyEvent;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result:=FZQuery.BeforeDelete;
   deSqlDB:Result:=FQuery.BeforeDelete;
 end;
end;

function TAsQuery.GetBeforeSave: TDataSetNotifyEvent;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result:=FZQuery.BeforePost;
   deSqlDB:Result:=FQuery.BeforePost;
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

function TAsQuery.GetIndexname: string;
begin
  case FDBInfo.DbEngineType of
   deZeos:Result := TAsStringUtils.SplitString(FZQuery.IndexFieldNames,',')[0];
   deSqlDB:Result := FQuery.IndexName;
  end;
end;

function TAsQuery.GetIsActive: Boolean;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result := FZQuery.Active;
   deSqlDB:Result := FQuery.Active;
 end;
end;

function TAsQuery.GetPacketRecord: Integer;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result := FQuery.PacketRecords;
   deSqlDB:Result := -1;
 end;
end;

function TAsQuery.GetRecNo: Integer;
begin
  case FDBInfo.DbEngineType of
   deZeos : Result :=  FZQuery.RecNo;
   deSqlDB : Result := FQuery.RecNo;
 end;
end;

function TAsQuery.GetRecordCount: Integer;
begin
 case FDBInfo.DbEngineType of
   deZeos:FZQuery.RecordCount;
   deSqlDB:FQuery.RecordCount;
 end;
end;

function TAsQuery.GetRowsAffected: Integer;
begin
 case FDBInfo.DbEngineType of
   deZeos:Result := FZQuery.RowsAffected;
   deSqlDB:Result := FQuery.RowsAffected;
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

procedure TAsQuery.SetAfterDelete(AValue: TDataSetNotifyEvent);
begin
 case FDBInfo.DbEngineType of
   deZeos : FZQuery.AfterDelete:=AValue;
   deSqlDB : FQuery.AfterDelete:=AValue;
 end;
end;

procedure TAsQuery.SetAfterRefresh(AValue: TDataSetNotifyEvent);
begin
  case FDBInfo.DbEngineType of
   deZeos:FZQuery.AfterRefresh := AValue;
   deSqlDB:FQuery.AfterRefresh := AValue;
  end;
end;


procedure TAsQuery.SetAfterSave(AValue: TDataSetNotifyEvent);
begin
 case FDBInfo.DbEngineType of
   deZeos : FZQuery.AfterPost:=AValue;
   deSqlDB : FQuery.AfterPost:=AValue;
 end;
end;

procedure TAsQuery.SetAfterScroll(AValue: TDataSetNotifyEvent);
begin
 case FDBInfo.DbEngineType of
   deZeos:FZQuery.AfterScroll := AValue;
   deSqlDB:FQuery.AfterScroll:= AValue;
 end;
end;

procedure TAsQuery.SetBeforeDelete(AValue: TDataSetNotifyEvent);
begin
 case FDBInfo.DbEngineType of
   deZeos : FZQuery.BeforeDelete:=AValue;
   deSqlDB : FQuery.BeforeDelete:=AValue;
 end;
end;

procedure TAsQuery.SetBeforeSave(AValue: TDataSetNotifyEvent);
begin
 case FDBInfo.DbEngineType of
   deZeos : FZQuery.BeforePost:=AValue;
   deSqlDB : FQuery.BeforePost:=AValue;
 end;
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

  FZCon:=FDBInfo.ZeosConnection;
  FZQuery:=TZQuery.Create(Self);
  FZQuery.Connection:=FZCon;
  FZQuery.AfterPost:= @AfterPost;
  FZQuery.AfterDelete:=@AfterDelete;

  FCon:=FDBInfo.SqlConnection;
  FQuery:=TSQLQuery.Create(Self);
  FQuery.DataBase:=FCon;
  FQuery.PacketRecords:=-1;
  FQuery.AfterPost:=@AfterPost;
  FQuery.AfterDelete:=@AfterDelete;

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
    deZeos:
    begin
      FZQuery.Post;
      FZQuery.ApplyUpdates;
    end;
    deSqlDB:
    begin
      FQuery.Post;
      FCon.Transaction.CommitRetaining;
    end;
 end;
end;

procedure TAsQuery.Refresh;
begin
 DataSet.Refresh;
end;

procedure TAsQuery.ExecSQL;
begin
 case FDBInfo.DbEngineType of
    deZeos:
    begin
      if not FZCon.Connected then FZCon.Connect;
      FZQuery.ExecSQL;
      FZQuery.ApplyUpdates;
      FZCon.Commit;
    end;
    deSqlDB:
    begin
      if not FCon.Connected then FCon.Open;
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
 case FDBInfo.DbEngineType of
  deSqlDB:
      begin
       FQuery.ApplyUpdates;
       FCon.Transaction.CommitRetaining;
      end;
  deZeos:
  begin
    if not FZCon.AutoCommit then
    begin
      FZQuery.ApplyUpdates;
      FZCon.Commit;
    end;
  end;
 end;

end;

procedure TAsQuery.DisableControls;
begin
  DataSet.DisableControls;
end;

procedure TAsQuery.EnableControls;
begin
 DataSet.EnableControls;
end;

procedure TAsQuery.AddIndex(const AName, AFields: string;
 AOptions: TIndexOptions; const ADescFields: string;
 const ACaseInsFields: string);
begin

end;

function TAsQuery.GetFieldNames: TStringList;
var
 I: Integer;
begin
 Result := TStringList.Create;
 for I:=0 to FieldCount-1 do
 begin
  Result.Add(Fields[I].FieldName);
 end;
end;

function TAsQuery.FieldByName(FieldName: string): TField;
begin
 Result := DataSet.FieldByName(FieldName);
end;

{ TAsSyntaxError }

procedure TAsSyntaxError.SetMessage(AValue: string);
var
 lst,lst2:TStringList;
 strLineNo,strPosNo:string;
begin

 FMessage:=AValue;
 lst := TStringList.Create;
 lst2 := TStringList.Create;
 if AnsiContainsText(FMessage,'Error') then
 begin
 try
  strLineNo:=EmptyStr;
  strPosNo:=EmptyStr;
  TAsRegExUtils.RunRegex(FMessage,'\([^\=\n\r]*\)',lst);
  lst2.DelimitedText:=lst.Text;

  if lst2.Count>0 then
  begin
  {(14:54)}
    strLineNo:= StringReplace( TAsStringUtils.SplitString(lst2[0],':')[0],'(','',[rfReplaceAll]);
    strPosNo:= StringReplace( TAsStringUtils.SplitString(lst2[0],':')[1],')','',[rfReplaceAll]);
    Line := 0;
    Position := 0;
    TryStrToInt(strLineNo,Line);
    TryStrToInt(strPosNo,Position)
  end;

 finally
   lst2.Free;
   lst.Free;
 end;

 end;

end;

{ TAsDbUtils }

class function TAsDbUtils.MakeEngine(DbInfo: TAsDbConnectionInfo
 ): TAsDbMetadata;
begin
 case DbInfo.DbType of
    dtMsSql: Result := TAsMssqlMetadata.Create(DbInfo);
    dtOracle: Result := TAsOracleMetadata.Create(DbInfo);
    dtMySql: Result := TAsMySqlMetadata.Create(DbInfo);
    dtFirebirdd: Result := TAsFirebirdMetadata.Create(DbInfo);
    dtSQLite: Result := TAsSqliteMetadata.Create(DbInfo);
    dtPostgreSql: Result := TAsPostgresMetadata.Create(DbInfo);
 end;
end;

class procedure TAsDbUtils.DisposeEngine(a: TAsDbMetadata);
begin
 a.Free;
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
     if aField.Tag=0 then
     begin


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
            Result := False;
          end;
         end;

       finally
         if m<>nil then
         m.Free;
       end;

     end else
     Result := False;

    except
      Result := False;
    end;
  end;
end;

class function TAsDbUtils.GetTopRecordsSelect(dbtyp: TAsDatabaseType; Schema,
 TableName: string; NumberOfRecords: integer): string;
begin
  case dbtyp of
  dtMsSql:Result :='SELECT TOP '+IntToStr(NumberOfRecords)+' * FROM '+SafeWrap(dbtyp, Schema)+'.'+SafeWrap(dbtyp,TableName);
  dtOracle:Result := 'SELECT * FROM '+SafeWrap(dbtyp,Schema)+'.'+SafeWrap(dbtyp,TableName)+' WHERE ROWNUM <= '+IntToStr(NumberOfRecords);
  dtMySql,dtSQLite:Result:='SELECT * FROM '+SafeWrap(dbtyp,TableName)+' LIMIT '+IntToStr(NumberOfRecords);
  dtPostgreSql: Result:='SELECT * FROM '+SafeWrap(dbtyp,Schema)+'.'+SafeWrap(dbtyp,TableName)+' LIMIT '+IntToStr(NumberOfRecords);
  dtFirebirdd: Result:='SELECT FIRST '+IntToStr(NumberOfRecords)+' * FROM '+SafeWrap(dbtyp,TableName);
 end;
end;

class function TAsDbUtils.GetTopRecordsSelect(dbtyp: TAsDatabaseType;
 Schema, TableName,FieldName: string; NumberOfRecords: integer): string;
begin
 case dbtyp of
  dtMsSql:Result :='SELECT TOP '+IntToStr(NumberOfRecords)+' '+SafeWrap(dbtyp,FieldName)+' FROM '+SafeWrap(dbtyp,Schema)+'.'+SafeWrap(dbtyp,TableName)+'';
  dtOracle:Result := 'SELECT '+SafeWrap(dbtyp,FieldName)+' FROM '+SafeWrap(dbtyp,Schema)+'.'+SafeWrap(dbtyp,TableName)+' WHERE ROWNUM <= '+IntToStr(NumberOfRecords);
  dtMySql,dtSQLite:Result:='SELECT '+SafeWrap(dbtyp,FieldName)+' FROM '+SafeWrap(dbtyp,TableName)+' LIMIT '+IntToStr(NumberOfRecords);
  dtPostgreSql: Result:='SELECT '+SafeWrap(dbtyp,FieldName)+' FROM '+SafeWrap(dbtyp,Schema)+'.'+SafeWrap(dbtyp,TableName)+' LIMIT '+IntToStr(NumberOfRecords);
  dtFirebirdd: Result:='SELECT FIRST '+IntToStr(NumberOfRecords)+' '+SafeWrap(dbtyp,FieldName)+' FROM '+SafeWrap(dbtyp,TableName);
 end;
end;

class function TAsDbUtils.SafeWrap(dbtyp: TAsDatabaseType; TableOrField: string
 ): string;
begin
   case dbtyp of
    dtMsSql:Result:='['+TableOrField+']';
    dtMySql:Result:='`'+TableOrField+'`';
    dtOracle,dtFirebirdd,dtPostgreSql,dtSQLite:Result:='"'+TableOrField+'"';
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
    dtPostgreSql:Result:='PostgreSQL';
  end;
end;

class function TAsDbUtils.DatabaseTypeAsString(DbType: TAsDatabaseType;
 ALowerCase: Boolean): string;
begin
  case DbType of
    dtFirebirdd : Result := 'firebirdd-2.5';
    dtSQLite : Result := 'sqlite-3';
    dtMsSql: Result:='mssql';
    dtOracle:Result:='oracle';
    dtMySql:Result:='mysql';
    dtPostgreSql:Result:='postgresql-9';
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

    if lowercase(s) = lowercase(TAsStringUtils.RemoveChars(DbTypeString,['-','3','2','.','5','9'])) then
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
  Result := 'MySQL 5.5'
  else
  if (p='postgresql-9') then
  Result := 'postgresql';
end;

class function TAsDbUtils.CheckSqlSyntax(SqlCommand: string; out
 Error: TAsSyntaxError): Boolean;
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
          Error := TAsSyntaxError.Create;
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
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetSchemas;
  finally
    DisposeEngine(md);
  end;
end;

class function TAsDbUtils.GetTablenames(DbInfo: TAsDbConnectionInfo
  ): TStringList;
var
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetTablenames(DbInfo.Schema);
  finally
    DisposeEngine(md);
  end;
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
      ds.Open(GetTopRecordsSelect(DbInfo.DbType,DbInfo.Schema ,TableName,1));
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

class function TAsDbUtils.GetPrimaryKeys(DbInfo: TAsDbConnectionInfo;
  TableName: string): TStringList;
var
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetPrimaryKeys(DbInfo.Schema,TableName);
  finally
     DisposeEngine(md);
  end;

end;

class function TAsDbUtils.GetForeignKeys(DbInfo: TAsDbConnectionInfo;
  TableName: string): TAsForeignKeys;
var
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetForeignKeys(DbInfo.Schema,TableName);
  finally
     DisposeEngine(md);
  end;
end;

class function TAsDbUtils.GetColumns(DbInfo: TAsDbConnectionInfo;
  TableName: string): TAsColumns;
var
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetColumns(DbInfo.Schema,TableName);
  finally
     DisposeEngine(md);
  end;
end;

class function TAsDbUtils.GetIndexes(DbInfo: TAsDbConnectionInfo;
  TableName: string): TAsIndexes;
var
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetIndexes(DbInfo.Schema,TableName);
  finally
     DisposeEngine(md);
  end;
end;

class function TAsDbUtils.GetTextFields(DbInfo: TAsDbConnectionInfo;
  TableName: string): TStringList;
var
  ds:TAsQuery;
  I: Integer;
begin
 Result := TStringList.Create;

 ds := TAsQuery.Create(DbInfo);
 try
  try
    ds.Open(GetTopRecordsSelect(DbInfo.DbType,DbInfo.Schema,TableName,1));
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

class function TAsDbUtils.GetTriggers(DbInfo: TAsDbConnectionInfo;
  TableName: string): TAsTriggers;
var
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetTriggers(DbInfo.Schema,TableName);
  finally
   DisposeEngine(md);
  end;
end;

class function TAsDbUtils.GetProcedureNames(DbInfo: TAsDbConnectionInfo
  ): TStringList;
var
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetProcedureNames(DbInfo.Schema);
  finally
   DisposeEngine(md);
  end;
end;

class function TAsDbUtils.GetProcedureParams(DbInfo: TAsDbConnectionInfo;
 ProcedureName: string): TAsProcedureParams;
var
 md:TAsDbMetadata;
begin
  md := MakeEngine(DbInfo);
  try
    Result := md.GetProcedureParams(ProcedureName);
  finally
   DisposeEngine(md);
  end;
end;

class function TAsDbUtils.GetCatalogNames(aDbInfo: TAsDbConnectionInfo
 ): TStringList;
var
 md:TAsDbMetadata;
begin
   md := MakeEngine(aDbInfo);
   try
    Result := md.GetCatalogNames;
   finally
    DisposeEngine(md);
   end;
end;

class function TAsDbUtils.FormatQuery(SqlQuery: string): String;
const
  ls:array[0..1] of string=('',',');
var
  Parser: TSQLParser;
  ResultList: TSQLElementList;
  lst: TStringList;
  m:TMemoryStream;
  I: Integer;
  o:TObject;
  ParserField: Boolean;
begin
  lst := TStringList.Create;
  m := TMemoryStream.Create;
  lst.Text:= TAsStringUtils.RemoveChars(SqlQuery,['[',']','`','"']);
  lst.SaveToStream(m);
  m.Position:=0;;
  try
   Parser := TSQLParser.Create(m);
   try
    ResultList := Parser.ParseScript(True);
    lst.Clear;
    for I:= 0 to ResultList.Count-1 do
    begin
      lst.Add(ResultList[i].GetAsSQL([sfoOneFieldPerLine,sfoWhereOnSeparateLine,sfoLowercaseKeyword]));
    end;
   except
    ParserField := True;
   end;

   if ParserField then
   begin
    lst.Clear;
    lst.Delimiter:=',';
    lst.StrictDelimiter:=True;
    lst.DelimitedText:=SqlQuery;
    for I:=0 to lst.Count-1 do
    begin
      lst[I] := lst[I]+ls[Integer(I<lst.Count-1)];
    end;
   end;

   Result := lst.Text;
  finally
   lst.Free;
   m.Free;
  end;
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

function TAsDbConnectionInfo.GetLibLocation: String;
begin
  Result := FLibraryLocation;
end;

function TAsDbConnectionInfo.GetProperties: TStrings;
begin
 case FDbEngine of
     deZeos: Result := FZCon.Properties;
     deSqlDB: Result := FSqlCon.Params;
 end;
end;

procedure TAsDbConnectionInfo.Instantiate;
begin
   FSqlCon:=TSQLConnector.Create(nil);
   FTrans := TSQLTransaction.Create(FSqlCon);
   FSqlCon.Transaction:=FTrans;
   FZCon := TZConnection.Create(nil);
   FZCon.AutoCommit:=False;
   FLibraryLoader := TSQLDBLibraryLoader.Create(FSqlCon);
end;

procedure TAsDbConnectionInfo.SetDbEngine(AValue: TAsDatabaseEngineType);
begin
 if FDbEngine=AValue then Exit;
 FDbEngine:=AValue;
end;

procedure TAsDbConnectionInfo.SetDbType(AValue: TAsDatabaseType);
begin
 FDbType:=AValue;
 if GetConnectionsCreated then
 begin
  FSqlCon.ConnectorType:= TAsDbUtils.DatabaseTypeAsConnectorType(FDbType);
  FZCon.Protocol := TAsDbUtils.DatabaseTypeAsString(FDbType, True);
 end;
end;

procedure TAsDbConnectionInfo.SetLibLocation(AValue: String);
begin
  FLibraryLocation:= AValue;
  if FileExists(FLibraryLocation) then
  begin
    FZCon.LibraryLocation := FLibraryLocation;
    FLibraryLoader.LibraryName := FLibraryLocation;
    FLibraryLoader.ConnectionType := TAsDbUtils.DatabaseTypeAsConnectorType(FDbType);
    FLibraryLoader.Enabled := True;
  end else
  begin
    FLibraryLoader.Enabled := False;
    FZCon.LibraryLocation := EmptyStr;
    FLibraryLoader.LibraryName := '';
  end;

end;

procedure TAsDbConnectionInfo.SetPassword(AValue: string);
begin
 FPassword:=AValue;
 if GetConnectionsCreated then
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

procedure TAsDbConnectionInfo.SetProperties(AValue: TStrings);
begin
 case FDbEngine of
  deZeos:
  begin
    FZCon.Properties.Clear;
    FZCon.Properties.AddStrings(AValue);
  end;
  deSqlDB:
  begin
    FSqlCon.Params.Clear;
    FSqlCon.Params.AddStrings(AValue);
  end;
 end;
end;

procedure TAsDbConnectionInfo.SetSchema(AValue: String);
begin
  if FSchema=AValue then Exit;
  FSchema:=AValue;
end;

procedure TAsDbConnectionInfo.SetServer(AValue: string);
begin
 FServer:=AValue;
 if GetConnectionsCreated then
 begin
  FSqlCon.HostName:= FServer;
  FZCon.HostName:=FServer;
 end;
end;

procedure TAsDbConnectionInfo.SetUsername(AValue: string);
begin
 FUsername:=AValue;
 if GetConnectionsCreated then
 begin
  FSqlCon.UserName:=FUsername;
  FZCon.User:=FUsername;
 end;
end;

function TAsDbConnectionInfo.GetConnectionsCreated: Boolean;
begin
  Result := (FZCon<>nil) and (FSqlCon<>nil);
end;

procedure TAsDbConnectionInfo.AssignProperties;
begin
 if GetConnectionsCreated then
 begin
  FSqlCon.UserName:=FUsername;
  FSqlCon.HostName:= FServer;
  FSqlCon.Password:=FPassword;
  FSqlCon.ConnectorType:= TAsDbUtils.DatabaseTypeAsConnectorType(FDbType);
  FSqlCon.DatabaseName:= FDatabase;

  FZCon.User:=FUsername;
  FZCon.HostName:=FServer;
  FZCon.Port:=FPort;
  FZCon.Password:=FPassword;
  FZCon.Protocol := TAsDbUtils.DatabaseTypeAsString(FDbType, True);

   case FDBType of
       dtOracle: FZCon.Database := TAsDbUtils.GetOracleDescriptor(Self);
       dtMsSql: FZCon.Database := TAsStringUtils.WrapString(Database,TAsStringWrapType.swtBrackets);
   else
       FZCon.Database := FDatabase;
   end;

 end;
end;

constructor TAsDbConnectionInfo.Create(CreateConnections: Boolean);
begin
 inherited Create;
 FConnectionsCreated:= CreateConnections;

 if CreateConnections then
 begin
  Instantiate;
 end;

end;

destructor TAsDbConnectionInfo.Destroy;
begin
 if GetConnectionsCreated then
 begin
  FSqlCon.Free;
  FZCon.Free;
 end;
 inherited Destroy;
end;

function TAsDbConnectionInfo.ToFullString: string;
begin
 Result:='aDbType='+ IntToStr(Integer(FDbType))+';aServer='+Server+';aDatabase='+FDatabase+';aUsername='+FUsername+';aPassword='+FPassword+';aPort='+IntToStr(FPort)+';';
end;

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
  FSchema:= TAsDbConnectionInfo(Source).Schema;
  if TAsDbConnectionInfo(Source).SqlConnection <> nil then
  begin
      if FSqlCon<>nil then
      FSqlCon.Free;

      FSqlCon := TSQLConnector.Create(nil);

    FSqlCon.HostName := TAsDbConnectionInfo(Source).SqlConnection.HostName;
    FSqlCon.ConnectorType:=TAsDbConnectionInfo(Source).SqlConnection.ConnectorType;
    FSqlCon.DatabaseName:=TAsDbConnectionInfo(Source).SqlConnection.DatabaseName;
    FSqlCon.UserName:=TAsDbConnectionInfo(Source).SqlConnection.UserName;
    FSqlCon.Password:=TAsDbConnectionInfo(Source).SqlConnection.Password;
    FSqlCon.Transaction := TSQLTransaction.Create(FSqlCon);
  end;

  if TAsDbConnectionInfo(Source).ZeosConnection<>nil then
  begin
    if FZCon<>nil then
    FZCon.Free;

    FZCon := TZConnection.Create(nil);

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
 AssignProperties;
 case FDbEngine of
   deSqlDB:FSqlCon.Open;
   deZeos:FZCon.Connect;
 end;
end;

procedure TAsDbConnectionInfo.Close;
begin
 case FDbEngine of
   deSqlDB:FSqlCon.Close(True);
   deZeos:FZCon.Disconnect;
 end;
end;

procedure TAsDbConnectionInfo.StartTransaction;
begin
  case FDbEngine of
  deSqlDB: FSqlCon.StartTransaction;
  deZeos: FZCon.StartTransaction;
  end;
end;

procedure TAsDbConnectionInfo.Rollback;
var
 c:Boolean;
begin
 try
  case FDbEngine of
    deZeos:  FZCon.Rollback;
    deSqlDB: FSqlCon.Transaction.Rollback;
  end;
 except
    c := Connected;
    FZCon.Free;
    FSqlCon.Free;
    Instantiate;
    if (c) then
    Open;
 end;
end;

procedure TAsDbConnectionInfo.Commit;
begin
 case FDbEngine of
   deZeos:
   begin
     if not FZCon.AutoCommit then
     FZCon.Commit;
   end;
   deSqlDB:
   begin
     FSqlCon.Transaction.CommitRetaining;
   end;
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


