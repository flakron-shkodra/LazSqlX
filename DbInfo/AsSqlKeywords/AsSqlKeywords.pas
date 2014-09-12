{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit AsSqlKeywords;

{$mode objfpc}{$H+}
{$STATIC ON}

interface

uses
  Classes, SysUtils,strutils;

type



  { TSqlKeywords }

  TSqlKeywords = object
  public
    MsSqlDataTypes:TStringList;static;
    OracleDataTypes:TStringList;static;
    MySqlDataTypes:TStringList;static;
    SqliteDataTypes:TStringList;static;
    FirebirdDataTypes:TStringList;static;
    ReservedKeywords:TStringList;static;
    class function IsReserved(Keyword:string):Boolean;
    class function IsAggregate(Keyword:string):Boolean;
    class function ContainsAggregate(Line:string):Boolean;
  end;


const
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

{$R AsSqlKeywords.rc}

{ TSqlKeywords }

class function TSqlKeywords.IsReserved(Keyword: string): Boolean;
begin
  Result := ReservedKeywords.IndexOf(Trim(Uppercase(Keyword)))>-1;
end;

class function TSqlKeywords.IsAggregate(Keyword: string): Boolean;
begin
  Result := (UpperCase(Keyword)='COUNT') or
  (UpperCase(Keyword)='SUM') or
  (UpperCase(Keyword)='MAX') or
  (UpperCase(Keyword)='MIN') or
  (UpperCase(Keyword)='AVG');
end;

class function TSqlKeywords.ContainsAggregate(Line: string): Boolean;
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
initialization

  try
    rc := TResourceStream.Create(HINSTANCE,'RESERVED','AsSQLKeywords');
    TSqlKeywords.ReservedKeywords := TStringList.Create;
    TSqlKeywords.ReservedKeywords.LoadFromStream(rc);
  finally
    rc.Free;
  end;

  try
   rc := TResourceStream.Create(HINSTANCE,'MsSqlDataTypes','AsSQLKeywords');
   TSqlKeywords.MsSqlDataTypes := TStringList.Create;
   TSqlKeywords.MsSqlDataTypes.LoadFromStream(rc);
  finally
   rc.Free;
  end;

  try
    rc := TResourceStream.Create(HINSTANCE,'OracleDataTypes','AsSQLKeywords');
    TSqlKeywords.OracleDataTypes := TStringList.Create;
    TSqlKeywords.OracleDataTypes.LoadFromStream(rc);
  finally
    rc.Free;
  end;

  try
   rc := TResourceStream.Create(HINSTANCE,'MySqlDataTypes','AsSQLKeywords');
   TSqlKeywords.MySqlDataTypes := TStringList.Create;
   TSqlKeywords.MySqlDataTypes.LoadFromStream(rc);
  finally
   rc.Free;
  end;


  try
   rc := TResourceStream.Create(HINSTANCE,'SqliteDataTypes','AsSQLKeywords');
   TSqlKeywords.SqliteDataTypes := TStringList.Create;
   TSqlKeywords.SqliteDataTypes.LoadFromStream(rc);
  finally
   rc.Free;
  end;


  try
   rc := TResourceStream.Create(HINSTANCE,'FirebirdDataTypes','AsSQLKeywords');
   TSqlKeywords.FirebirdDataTypes := TStringList.Create;
   TSqlKeywords.FirebirdDataTypes.LoadFromStream(rc);
  finally
   rc.Free;
  end;





finalization
  TSqlKeywords.ReservedKeywords.Free;
  TSqlKeywords.MsSqlDataTypes.Free;
  TSqlKeywords.OracleDataTypes.Free;
  TSqlKeywords.MySqlDataTypes.Free;
  TSqlKeywords.SqliteDataTypes.Free;
  TSqlKeywords.FirebirdDataTypes.Free;

end.

