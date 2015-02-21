{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit LazSqlXResources;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,strutils;

type


  { TLazSqlXResources }

  TLazSqlXResources = object
  public
    MsSqlDataTypes:TStringList;static;
    OracleDataTypes:TStringList;static;
    MySqlDataTypes:TStringList;static;
    SqliteDataTypes:TStringList;static;
    FirebirdDataTypes:TStringList;static;
    PostgreSqlDataTypes:TStringList;static;
    SqlReservedKeywords:TStringList;static;
    FileTypeDetector:TStringList;static;
    LoadingGif:TMemoryStream;static;
    ReportTemplatePath:string;static;
    class function IsReserved(Keyword:string):Boolean;
    class function IsAggregate(Keyword:string):Boolean;
    class function ContainsAggregate(Line:string):Boolean;
  end;


const

  POSTGRESQL_TYPES_FILENAME='PostgreSqlDataTypes.dt';
  FIREBIRD_TYPES_FILENAME = 'FirebirdDataTypes.dt';
  MSSQL_TYPES_FILENAME='MSSqlDataTypes.dt';
  MYSQL_TYPES_FILENAME = 'MySqlDataTypes.dt';
  ORACLE_TYPES_FILENAME = 'OracleDataTypes.dt';
  SQLITE_TYPES_FILENAME = 'SqliteDataTypes.dt';
  SQL_KEYWORDS_FILENAME = 'SqlReservedKeywords.dt';
  FILETYPE_DETECTOR_FILENAME ='FileTypeDetector.ftd';
  REPORT_TEMPLATE_FILENAME='ReportTemplate.lrf';
  LOADING_GIF = 'loading.gif';


  SQL_FROM_WORD = 'FROM ';
  SQL_WHERE_WORD = 'WHERE ';
  SQL_AND_WORD = 'AND ';
  SQL_SELECT_WORD = 'SELECT ';
  SQL_INSERT_WORD = 'INSERT ';
  SQL_UPDATE_WORD = 'UPDATE ';
  SQL_JOIN_WORD = 'JOIN ';
  SQL_TOP_WORD ='TOP ';
  SQL_ORDER_WORD='ORDER ';
  SQL_GROUP_WORD ='GROUP ';

implementation

{ TLazSqlXResources }

class function TLazSqlXResources.IsReserved(Keyword: string): Boolean;
begin
  Result := SqlReservedKeywords.IndexOf(Trim(Uppercase(Keyword)))>-1;
end;

class function TLazSqlXResources.IsAggregate(Keyword: string): Boolean;
begin
  Result := (UpperCase(Keyword)='COUNT') or
  (UpperCase(Keyword)='SUM') or
  (UpperCase(Keyword)='MAX') or
  (UpperCase(Keyword)='MIN') or
  (UpperCase(Keyword)='AVG');
end;

class function TLazSqlXResources.ContainsAggregate(Line: string): Boolean;
var
  lst:TStringList;
  I: Integer;
begin
  try
    lst:=TStringList.Create;
    lst.Delimiter:='(';
    lst.StrictDelimiter:=True;
    lst.DelimitedText:= Line;
    for I:=0 to lst.Count-1 do
    begin
      if IsAggregate(lst[I]) then
      begin
        Result := true;
        Break;
      end;
    end;
  finally
    lst.Free;
  end;
end;


var
  rc:TResourceStream;
  DefPath:string;
initialization

   DefPath := ExtractFilePath(ParamStr(0))+'Resources\';

   TLazSqlXResources.SqlReservedKeywords := TStringList.Create;
   TLazSqlXResources.MsSqlDataTypes := TStringList.Create;
   TLazSqlXResources.MySqlDataTypes := TStringList.Create;
   TLazSqlXResources.OracleDataTypes := TStringList.Create;
   TLazSqlXResources.SqliteDataTypes := TStringList.Create;
   TLazSqlXResources.FirebirdDataTypes := TStringList.Create;
   TLazSqlXResources.PostgreSqlDataTypes := TStringList.Create;

   TLazSqlXResources.FileTypeDetector := TStringList.Create;
   TLazSqlXResources.LoadingGif := TMemoryStream.Create;


   if FileExists(DefPath+SQL_KEYWORDS_FILENAME) then
    TLazSqlXResources.SqlReservedKeywords.LoadFromFile(DefPath+SQL_KEYWORDS_FILENAME);


  if FileExists(DefPath+MSSQL_TYPES_FILENAME) then
   TLazSqlXResources.MsSqlDataTypes.LoadFromFile(DefPath+MSSQL_TYPES_FILENAME);


  if FileExists(DefPath+ORACLE_TYPES_FILENAME) then
  TLazSqlXResources.OracleDataTypes.LoadFromFile(DefPath+ORACLE_TYPES_FILENAME);

  if FileExists(DefPath+MYSQL_TYPES_FILENAME) then
   TLazSqlXResources.MySqlDataTypes.LoadFromFile(DefPath+MYSQL_TYPES_FILENAME);

  if FileExists(DefPath+SQLITE_TYPES_FILENAME) then
   TLazSqlXResources.SqliteDataTypes.LoadFromFile(DefPath+SQLITE_TYPES_FILENAME);

  if FileExists(DefPath+FIREBIRD_TYPES_FILENAME) then
  TLazSqlXResources.FirebirdDataTypes.LoadFromFile(DefPath+FIREBIRD_TYPES_FILENAME);

  if FileExists(DefPath+POSTGRESQL_TYPES_FILENAME) then
  TLazSqlXResources.PostgreSqlDataTypes.LoadFromFile(DefPath+POSTGRESQL_TYPES_FILENAME);

  if FileExists(DefPath+FIREBIRD_TYPES_FILENAME) then
  TLazSqlXResources.FirebirdDataTypes.LoadFromFile(DefPath+FIREBIRD_TYPES_FILENAME);

  if FileExists(DefPath+FILETYPE_DETECTOR_FILENAME) then
  TLazSqlXResources.FileTypeDetector.LoadFromFile(DefPath+FILETYPE_DETECTOR_FILENAME);

  if FileExists(DefPath+LOADING_GIF) then
  TLazSqlXResources.LoadingGif.LoadFromFile(DefPath+LOADING_GIF);

  if FileExists(DefPath+REPORT_TEMPLATE_FILENAME) then
    TLazSqlXResources.ReportTemplatePath:=DefPath+REPORT_TEMPLATE_FILENAME
  else
    TLazSqlXResources.ReportTemplatePath := EmptyStr;


finalization
  TLazSqlXResources.PostgreSqlDataTypes.Free;
  TLazSqlXResources.SqlReservedKeywords.Free;
  TLazSqlXResources.MsSqlDataTypes.Free;
  TLazSqlXResources.OracleDataTypes.Free;
  TLazSqlXResources.MySqlDataTypes.Free;
  TLazSqlXResources.SqliteDataTypes.Free;
  TLazSqlXResources.FirebirdDataTypes.Free;
  TLazSqlXResources.FileTypeDetector.Free;
  TLazSqlXResources.LoadingGif.Free;

end.

