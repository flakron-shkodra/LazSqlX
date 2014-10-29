{*******************************************************************
AUTHOR : Flakron Shkodra
*******************************************************************
Date Modified:  18/06/2013 (added qtAll queryType)
*******************************************************************
Date Modified : 18/05/2010 14:10:31
*******************************************************************
}


unit SqlGenerator;

{$mode objfpc}{$H+}
interface

uses SysUtils, Classes, Dialogs, StdCtrls, ComCtrls, ExtCtrls, DB,
  Forms, StrUtils, TableInfo, DbType, LazSqlXResources, ZConnection, ZDataset,
  ZSqlMetadata, ZDbcIntfs;

type

  TQueryType =
    (
    qtSelect = 0,
    qtSelectItem = 1,
    qtInsert = 2,
    qtUpdate = 3,
    qtDelete = 4,
    qtAll = 5
    );

  TQueryTypes = set of TQueryType;

  { TSqlGenerator }
TProcedureNames = object
 public
   PnSelect:string;
   PnSelectItem:string;
   PnInsert:string;
   PnUpdate:string;
   PnDelete:string;
   Prefix:string;
   TablenameAfter:Boolean;
 end;

  TSqlGenerator = class
  strict private
    adoConnection: TZConnection;
    _procNames: TProcedureNames;
    _LogList: TStrings;
    _schema: string;
    _cmd: TZQuery;
    _application: TApplication;
    _lstExistingProcedures: TStringList;
    _spPrefix: string;
    _DbType: TDatabaseType;

    procedure CheckProcedureName(Tablename: string);
    procedure DropProcedure(spName: string);
    {Used in GetCreateSql}
    function GetDataType(FieldInfo: TFieldInfo; DbType: TDatabaseType): string;

    procedure WriteLog(Msg: string);
    procedure _cmdBeforeOpen(DataSet: TDataSet);
    procedure _DataSetGetText(Sender: TField; var aText: string;
      DisplayText: boolean);
    procedure GetStoredProcedures(var storedProcedures: TStringList);
    function GetDbParamPrefix: string;

    function GetSql(Ident: integer; TableInfo: TTableInfo;
      QueryType: TQueryType): TStringList;
  public
    constructor Create(Server, Database, Userame, Password: string;
      ProcedureNames: TProcedureNames; ADatabaseType: TDatabaseType;
      Port: integer = 0); overload;
    constructor Create(Server, Database, Userame, Password: string;
      LogList: TStrings; ProcedureNames: TProcedureNames;
      ADatabaseType: TDatabaseType; Port: integer = 0); overload;
    destructor Destroy; override;
    procedure Generate(Tables: TTableInfos; QueryTypes: TQueryTypes;
      Prefix: string = 'usp'); overload;
    procedure Generate(TableInfo: TTableInfo; QueryTypes: TQueryTypes;
      Prefix: string = 'usp'); overload;
    function GenerateStoredProcedure(TableInfo: TTableInfo;
      QueryType: TQueryType; Prefix: string = 'usp'): string;
    {Used in Generate procedures and functions}
    function GetCreateSql(TableInfo: TTableInfo; QueryType: TQueryType;
      SqlSpKeyword: string = 'CREATE '): TStringList;
    procedure GetProcedures(Tablename: string; Items: TStrings);
    function GenerateQuery(Ident: integer; TableInfo: TTableInfo;
      QueryType: TQueryType): TStringList;
  end;

implementation

uses AsStringUtils;

{ TSqlGenerator }

{$REGION 'Constructor/Destructor'}

constructor TSqlGenerator.Create(Server, Database, Userame, Password: string;
  ProcedureNames: TProcedureNames; ADatabaseType: TDatabaseType; Port: integer = 0);
var
  s: string;
  i: integer;
begin
  inherited Create;
  _application := Application;
  _procNames := ProcedureNames;

  if ADatabaseType = dtOracle then
  begin
    _spPrefix := UpperCase(_spPrefix);
    _procNames.PnSelect := UpperCase(_procNames.PnSelect);
    _procNames.PnSelectItem := UpperCase(_procNames.PnSelectItem);
    _procNames.PnInsert := UpperCase(_procNames.PnInsert);
    _procNames.PnUpdate := UpperCase(_procNames.PnUpdate);
    _procNames.PnDelete := UpperCase(_procNames.PnDelete);
    _procNames.Prefix := UpperCase(_procNames.Prefix);
  end;
  adoConnection := TZConnection.Create(nil);
  _cmd := TZQuery.Create(nil);
  _DbType := ADatabaseType;


  adoConnection.Protocol := TDbUtils.DatabaseTypeAsString(ADatabaseType, True);
  adoConnection.Database := Database;
  adoConnection.Properties.Add('Encoding=UTF8');
  if _DbType <> dtSQLite then
  begin
    adoConnection.HostName := Server;
    adoConnection.User := Userame;
    adoConnection.Password := Password;
    adoConnection.Catalog := Database;
    adoConnection.LoginPrompt := False;
    adoConnection.Port := Port;
  end;


  _cmd.DisableControls;
  _cmd.Connection := adoConnection;
  adoConnection.Connect;
  _lstExistingProcedures := TStringList.Create;
  GetStoredProcedures(_lstExistingProcedures);
end;


constructor TSqlGenerator.Create(Server, Database, Userame, Password: string;
  LogList: TStrings; ProcedureNames: TProcedureNames; ADatabaseType: TDatabaseType;
  Port: integer = 0);
begin
  Create(Server, Database, Userame, Password, ProcedureNames, ADatabaseType, Port);
  _LogList := LogList;
end;



destructor TSqlGenerator.Destroy;
begin
  _cmd.Free;
  adoConnection.Free;
  _lstExistingProcedures.Free;
  inherited Destroy;
end;

procedure TSqlGenerator.DropProcedure(spName: string);
begin
  try
    case _DbType of
      dtMsSql: _cmd.SQL.Text := ' drop procedure ' + spName;
      dtMySql: _cmd.SQL.Text := ' drop procedure ' + spName;
      dtOracle: _cmd.SQL.Text := ' drop procedure ' + spName;
    end;
    _cmd.ExecSQL;
  except
  end;

end;

{$ENDREGION}

{$REGION 'Public Methods'}


procedure TSqlGenerator.Generate(TableInfo: TTableInfo; QueryTypes: TQueryTypes;
  Prefix: string);
var
  tmpLst: TStringList;
  lstStoredProcs: TStringList;
begin
  _LogList.Clear;
  lstStoredProcs := TStringList.Create;
  _spPrefix := UpperCase(Prefix);

  adoConnection.GetStoredProcNames('*', lstStoredProcs);

  WriteLog('===============STORED PROCEDURE GENERATION BEGIN===============');

  if not adoConnection.Connected then
  begin
    ShowMessage('Not connected to database');
    Exit;
  end;
  _schema := TableInfo.Schema;

  WriteLog('*************Processing [' + TableInfo.Tablename + ']*************');

  //checks if sp Names are identical with those to be generated
  //if so, existing procedures will be dropped
  CheckProcedureName(TableInfo.Tablename);

  if qtSelect in QueryTypes then
  begin
    try
      tmpLst := GetCreateSql(TableInfo, qtSelect);

      _cmd.SQL.Text := tmpLst.Text;
      _cmd.ExecSQL;
    except
      on e: Exception do
      begin
        WriteLog('Select for [' + TableInfo.Tablename + '] : ' + e.Message);
      end;
    end;
    tmpLst.Free;
  end;

  if qtSelectItem in QueryTypes then
  begin
    try
      tmpLst := GetCreateSql(TableInfo, qtSelectItem);
      _cmd.SQL.Text := tmpLst.Text;
      _cmd.ExecSQL;
    except
      on e: Exception do
      begin
        WriteLog('SelectItem for [' + TableInfo.Tablename + '] : ' + e.Message);
      end;
    end;
    tmpLst.Free;
  end;

  if qtInsert in QueryTypes then
  begin
    try
      tmpLst := GetCreateSql(TableInfo, qtInsert);
      _cmd.SQL.Text := tmpLst.Text;
      _cmd.ExecSQL;
    except
      on e: Exception do
      begin
        WriteLog('Insert for [' + TableInfo.Tablename + '] : ' + e.Message);
      end;
    end;
    tmpLst.Free;
  end;

  if qtUpdate in QueryTypes then
  begin
    try
      tmpLst := GetCreateSql(TableInfo, qtUpdate);
      _cmd.SQL.Text := tmpLst.Text;
      _cmd.ExecSQL;
    except
      on e: Exception do
      begin
        WriteLog('Update for [' + TableInfo.Tablename + '] : ' + e.Message);
      end;
    end;
    tmpLst.Free;
  end;

  if qtDelete in QueryTypes then
  begin
    try
      tmpLst := GetCreateSql(TableInfo, qtDelete);
      _cmd.SQL.Text := tmpLst.Text;
      _cmd.ExecSQL;
    except
      on e: Exception do
      begin
        WriteLog('Delete for [' + TableInfo.Tablename + '] : ' + e.Message);
      end;
    end;
    tmpLst.Free;
  end;

  lstStoredProcs.Free;
  WriteLog('===========STORED PROCEDURE GENERATION END===========');
end;

function TSqlGenerator.GenerateStoredProcedure(TableInfo: TTableInfo;
  QueryType: TQueryType; Prefix: string): string;
var
  lstAll: TStringList;
  lst: TStringList;
begin
  lst := TStringList.Create;
  lstAll := TStringList.Create;
  try
    if QueryType = qtAll then
    begin
      lst := GetCreateSql(TableInfo, qtSelect);
      lstAll.AddStrings(lst);
      lst := GetCreateSql(TableInfo, qtSelectItem);
      lstAll.AddStrings(lst);
      lst := GetCreateSql(TableInfo, qtInsert);
      lstAll.AddStrings(lst);
      lst := GetCreateSql(TableInfo, qtUpdate);
      lstAll.AddStrings(lst);
      lst := GetCreateSql(TableInfo, qtDelete);
      lstAll.AddStrings(lst);
    end
    else
    begin
      lst := GetCreateSql(TableInfo, QueryType);
      lstAll.AddStrings(lst);
    end;


    Result := lstAll.Text;
  finally
    lstAll.Free;
    lst.Free;
  end;
end;

function TSqlGenerator.GetSql(Ident: integer; TableInfo: TTableInfo;
  QueryType: TQueryType): TStringList;
var
  seperator, tmpFTablename, tmpTablename: string;

  SqlOpenBr, SqlCloseBr, SqlDot, SqlParamChar, SqlTable, SqlFromDot: string;

  I, oIndex: integer;

  tmpSelect, tmpSelectInsert, tmpInsert, tmpUpdate, tmpUpdateAll,
  tmpSelectJoins: TStringList;
  r: TStringList;
  field: TField;

  selectTablename:string;
  selectFieldname:string;

  strTest: string;
  strIdent: string;
  F: Integer;
begin

  _schema := TableInfo.Schema;

  if _DbType=dtFirebirdd then
  begin
    _schema:='';
  end;

  for I := 0 to Ident - 1 do
  begin
    strIdent := strIdent + #9;
  end;

  SqlFromDot := '.';

  if _DbType= dtFirebirdd then
  begin
    _schema:='';
  end;
  case _DbType of
    dtMsSql:
    begin
      SqlOpenBr := '[';
      SqlCloseBr := ']';
      SqlTable := TableInfo.Tablename;
      SqlDot := '.';
      SqlParamChar := ':';
    end;
    dtOracle:
    begin
      SqlOpenBr := '"';
      SqlCloseBr := '"';
      SqlTable := TableInfo.Tablename;
      SqlDot := '.';
      SqlParamChar := ' :';
    end;
    dtMySql:
    begin
      SqlOpenBr := '';
      SqlCloseBr := '';
      SqlTable := '';
      SqlTable := TableInfo.Tablename;
      SqlDot := '.';
      SqlParamChar := ':';
    end;
    dtSQLite:
    begin
      SqlOpenBr := '[';
      SqlCloseBr := ']';
      SqlTable := TableInfo.Tablename;
      SqlDot := '.';
      SqlParamChar := ':';
      SqlFromDot := '';
    end;
    dtFirebirdd:
    begin
      SqlOpenBr := '';
      SqlCloseBr := '';
      SqlTable := TableInfo.Tablename;
      SqlDot := '.';
      SqlParamChar := ':';
      SqlFromDot := '';
    end;
  end;

  if Trim(TableInfo.TableAlias)=EmptyStr then
  TableInfo.TableAlias:=TableInfo.Tablename[1];


  r := TStringList.Create;

  if (TableInfo.PrimaryKeys.Count = 0) and not (QueryType in [qtSelect, qtInsert]) then
    raise Exception.Create('No primary keys found!');



  tmpSelect := TStringList.Create;
  tmpInsert := TStringList.Create;
  tmpUpdate := TStringList.Create;
  tmpUpdateAll := TStringList.Create;
  tmpSelectJoins := TStringList.Create;
  tmpSelectInsert := TStringList.Create;

  for I := 0 to TableInfo.AllFields.Count - 1 do
  begin
    if I < (TableInfo.AllFields.Count - 1) then
      seperator := ','
    else
      seperator := '';

    if (I + 1) = TableInfo.AllFields.Count - 1 then
      if (QueryType in [qtInsert]) then
        if TableInfo.AllFields[I + 1].IsIdentity then
          seperator := '';

    if TableInfo.AllFields[I].IsReference then
    begin

      oIndex := TableInfo.ImportedKeys.GetIndex(TableInfo.AllFields[I].FieldName);
      tmpSelect.Add(strIdent + '    ' + TableInfo.TableAlias + SqlDot + SqlOpenBr + TableInfo.AllFields[I].FieldName + SqlCloseBr + ',');

      //tmpSelect.Add(strIdent + '    ' +  't' + IntToStr(oIndex + 1) +
      //  SqlDot + SqlOpenBr +
      //  TableInfo.ImportedKeys[oIndex].ForeignFirstTextField + SqlCloseBr + seperator);

      if tableInfo.ImportedKeys[oIndex]<>nil then
      if tableInfo.ImportedKeys[oIndex].TableName<>'' then
      for F := 0 to TableInfo.ImportedKeys[oIndex].SelectFields.Count-1 do
      begin
        tmpSelect.Add(strIdent + '    ' + 't' + IntToStr(oIndex + 1) +
          SqlDot + SqlOpenBr + TableInfo.ImportedKeys[oIndex].SelectFields[F] +
          SqlCloseBr + seperator);
      end;

    end
    else
    begin
      tmpSelect.Add(strIdent + '    ' + TableInfo.TableAlias +
        SqlDot + SqlOpenBr + TableInfo.AllFields[I].FieldName + SqlCloseBr + seperator);
    end;

    if (not TableInfo.AllFields[I].IsIdentity) then
    begin
      tmpInsert.Add(strIdent + '    ' + SqlParamChar +
        TableInfo.AllFields[I].CSharpName + seperator);
      tmpSelectInsert.Add(strIdent + '    ' + SqlOpenBr +
        TableInfo.AllFields[I].FieldName + SqlCloseBr + seperator);

      //this check will prevent some unwanted stuff like : (..set Status=@Status, where.. )
      if (I + 1) = TableInfo.AllFields.Count - 1 then
        if (TableInfo.AllFields[I + 1].IsPrimaryKey) and
          (TableInfo.PrimaryKeys.Count <> TableInfo.AllFields.Count) then
          seperator := '';

      if not TableInfo.AllFields[I].IsPrimaryKey then
        tmpUpdate.Add(strIdent + '    ' + SqlOpenBr + TableInfo.AllFields[I].FieldName +
          SqlCloseBr + '=' + SqlParamChar + TableInfo.AllFields[I].CSharpName +
          seperator);

      tmpUpdateAll.Add(strIdent + '    ' + SqlOpenBr +
        TableInfo.AllFields[I].FieldName + SqlCloseBr + '=' +
        SqlParamChar + TableInfo.AllFields[I].CSharpName + seperator);

    end;
  end;

  //make joins for SELECTs
  for I := 0 to TableInfo.ImportedKeys.Count - 1 do
  begin
    tmpFTablename := TableInfo.ImportedKeys[I].ForeignTableName;
    tmpTablename := TableInfo.ImportedKeys[i].TableAlias;
    if trim(tmpTablename)=EmptyStr then
    tmpTablename:=TableInfo.ImportedKeys[i].Tablename[1];

    tmpSelectJoins.Add
    (
      strIdent + ' INNER JOIN  ' + tmpFTablename + ' t' +
      IntToStr(I + 1) + ' ON ' + tmpTablename + '.' + SqlOpenBr +
      TableInfo.ImportedKeys[I].ColumnName + SqlCloseBr + ' = ' +
      't' + IntToStr(I + 1) + '.' + SqlOpenBr +
      TableInfo.ImportedKeys[I].ForeignColumnName + SqlCloseBr
      );
  end;


  tmpTablename:=SqlOpenBr + TableInfo.Tablename + SqlCloseBr;

  case QueryType of
    qtSelect:
    begin

      r.Add(strIdent + 'SELECT ');
      r.Add(tmpSelect.Text);
      r.Add(strIdent + 'FROM ' + tmpTablename+' '+TableInfo.TableAlias);
      r.Add(tmpSelectJoins.Text);
    end;
    qtSelectItem:
    begin
      r.Add(strIdent + 'SELECT ');
      r.Add(tmpSelect.Text);
      r.Add(strIdent + 'FROM ' + tmpTablename+' '+TableInfo.TableAlias);
      r.Add(tmpSelectJoins.Text);
      if TableInfo.PrimaryKeys.Count > 0 then
      begin
        r.Add(strIdent + 'WHERE ' + TableInfo.TableAlias+ '.' + SqlOpenBr + TableInfo.PrimaryKeys[0].FieldName +
          SqlCloseBr + '=' + SqlParamChar + TableInfo.PrimaryKeys[0].CSharpName);
        if (TableInfo.PrimaryKeys.Count > 1) then
        begin
          for I := 1 to TableInfo.PrimaryKeys.Count - 1 do
            r.Add(strIdent + ' AND ' + SqlOpenBr + TableInfo.Tablename +
              SqlCloseBr + '.' + SqlOpenBr + TableInfo.PrimaryKeys[I].FieldName +
              SqlCloseBr + '=' + SqlParamChar + TableInfo.PrimaryKeys[I].CSharpName);
        end;
      end;

    end;
    qtInsert:
    begin
      r.Add(strIdent + 'INSERT INTO ' + SqlOpenBr + TableInfo.TableName + SqlCloseBr);
      r.Add(strIdent + '(');
      r.Add(tmpSelectInsert.Text);
      r.Add(strIdent + ')');
      r.Add(strIdent + 'VALUES ');
      r.Add(strIdent + '(');
      r.Add(tmpInsert.Text);
      r.Add(strIdent + ')');
    end;
    qtUpdate:
    begin
      r.Add(strIdent + 'UPDATE ' + SqlOpenBr + TableInfo.TableName + SqlCloseBr);
      r.Add(strIdent + 'SET ');

      //if tmpUpdate.Count > 0 then
      //  r.Add(tmpUpdate.Text)
      //else
        r.Add(tmpUpdateAll.Text);

      if TableInfo.PrimaryKeys.Count > 0 then
      begin
        r.Add(strIdent + 'WHERE ' + SqlOpenBr + SqlTable + SqlCloseBr +
          SqlDot + SqlOpenBr + TableInfo.PrimaryKeys[0].FieldName +
          SqlCloseBr + '=' + SqlParamChar + 'p1');
        if (TableInfo.PrimaryKeys.Count > 1) then
        begin
          for I := 1 to TableInfo.PrimaryKeys.Count - 1 do
            r.Add(strIdent + ' AND ' + SqlOpenBr + SqlTable +
              SqlCloseBr + SqlDot + SqlOpenBr + TableInfo.PrimaryKeys[I].FieldName +
              SqlCloseBr + '=' + SqlParamChar + 'p'+IntToStr(I+1));
        end;
      end;
    end;
    qtDelete:
    begin
      r.Add(strIdent + 'DELETE FROM ' + SqlOpenBr+SqlTable+SqlCloseBr);
      if TableInfo.PrimaryKeys.Count > 0 then
      begin
        r.Add(strIdent + 'WHERE ' + SqlOpenBr + SqlTable + SqlCloseBr +
          SqlDot + SqlOpenBr + TableInfo.PrimaryKeys[0].FieldName +
          SqlCloseBr + '=' + SqlParamChar + 'p1');
        if (TableInfo.PrimaryKeys.Count > 1) then
        begin
          for I := 1 to TableInfo.PrimaryKeys.Count - 1 do
            r.Add(strIdent + ' AND ' + SqlOpenBr + SqlTable +
              SqlCloseBr + SqlDot + SqlOpenBr + TableInfo.PrimaryKeys[I].FieldName +
              SqlCloseBr + '=' + SqlParamChar + 'p'+IntToStr(I+1));
        end;
      end;
    end;

  end;

  //r[r.Count-1] := r[r.Count-1]+';';
  tmpSelectJoins.Free;
  tmpSelect.Free;
  tmpInsert.Free;
  tmpUpdate.Free;
  tmpUpdateAll.Free;
  tmpSelectInsert.Free;
  Result := r;
  strTest := StringReplace(Result.Text,'[]','',[rfReplaceAll]);
end;

procedure TSqlGenerator.Generate(Tables: TTableInfos; QueryTypes: TQueryTypes;
  Prefix: string = 'usp');
var
  I: integer;
  tmpLst, tmp2: TStringList;
  lstStoredProcs: TStringList;
begin
  _LogList.Clear;
  lstStoredProcs := TStringList.Create;
  _spPrefix := Prefix;

  adoConnection.GetStoredProcNames('*', lstStoredProcs);

  WriteLog('===============STORED PROCEDURE GENERATION BEGIN===============');

  if not adoConnection.Connected then
  begin
    ShowMessage('Not connected to database');
    Exit;
  end;
  _schema := Tables[0].Schema;
  for I := 0 to Tables.Count - 1 do
  begin
    WriteLog('*************Processing [' + Tables[i].Tablename + ']*************');

    //checks if sp Names are identical with those to be generated
    //if so, existing procedures will be dropped
    CheckProcedureName(Tables[i].Tablename);

    if qtSelect in QueryTypes then
    begin
      try
        tmpLst := GetCreateSql(Tables[i], qtSelect);
        _cmd.SQL.Clear;
        _cmd.SQL.AddStrings(tmpLst);
        _cmd.ExecSQL;
      except
        on e: Exception do
        begin
          WriteLog('Select for [' + Tables[i].Tablename + '] : ' + e.Message);
        end;
      end;
      tmpLst.Free;
    end;

    if qtSelectItem in QueryTypes then
    begin
      try
        tmpLst := GetCreateSql(Tables[i], qtSelectItem);
        _cmd.SQL.Clear;
        _cmd.SQL.AddStrings(tmpLst);
        _cmd.ExecSQL;
      except
        on e: Exception do
        begin
          WriteLog('SelectItem for [' + Tables[i].Tablename + '] : ' + e.Message);
        end;
      end;
      tmpLst.Free;
    end;

    if qtInsert in QueryTypes then
    begin
      try
        tmpLst := GetCreateSql(Tables[i], qtInsert);
        _cmd.SQL.Text := tmpLst.Text;
        _cmd.ExecSQL;
      except
        on e: Exception do
        begin
          WriteLog('Insert for [' + Tables[i].Tablename + '] : ' + e.Message);
        end;
      end;
      tmpLst.Free;
    end;

    if qtUpdate in QueryTypes then
    begin
      try
        tmpLst := GetCreateSql(Tables[i], qtUpdate);
        _cmd.SQL.Text := tmpLst.Text;
        _cmd.ExecSQL;
      except
        on e: Exception do
        begin
          WriteLog('Update for [' + Tables[i].Tablename + '] : ' + e.Message);
        end;
      end;
      tmpLst.Free;
    end;

    if qtDelete in QueryTypes then
    begin
      try
        tmpLst := GetCreateSql(Tables[i], qtDelete);
        _cmd.SQL.Text := tmpLst.Text;
        _cmd.ExecSQL;
      except
        on e: Exception do
        begin
          WriteLog('Delete for [' + Tables[i].Tablename + '] : ' + e.Message);
        end;
      end;
      tmpLst.Free;
    end;

    _application.ProcessMessages;
  end;

  lstStoredProcs.Free;
  WriteLog('===============STORED PROCEDURE GENERATION END===============');
end;

procedure TSqlGenerator.GetProcedures(Tablename: string; Items: TStrings);
var
  I: integer;
begin

  if not adoConnection.Connected then
  begin
    WriteLog('Not connected to any database');
    Exit;
  end;

  _lstExistingProcedures.Clear;
  adoConnection.GetStoredProcNames('*', _lstExistingProcedures);

  Items.Clear;

  for I := 0 to _lstExistingProcedures.Count - 1 do
  begin
    if AnsiContainsStr(_lstExistingProcedures[I], Tablename) then
      Items.Add(StringReplace(_lstExistingProcedures[I], ';1', '', [rfReplaceAll]));
  end;

end;

function TSqlGenerator.GenerateQuery(Ident: integer; TableInfo: TTableInfo;
  QueryType: TQueryType): TStringList;
var
  lst: TStringList;
begin

    if QueryType = qtAll then
    begin
      Result := TStringList.Create;
      lst := GetSql(0, TableInfo, qtSelect);
      Result.AddStrings(lst);
      lst.Free;

      lst := GetSql(0, TableInfo, qtSelectItem);
      Result.AddStrings(lst);
      lst.Free;

      lst := GetSql(0, TableInfo, qtInsert);
      Result.AddStrings(lst);
      lst.Free;

      lst := GetSql(0, TableInfo, qtUpdate);
      Result.AddStrings(lst);
      lst.Free;

      lst := GetSql(0, TableInfo, qtDelete);
      Result.AddStrings(lst);
      lst.Free;
    end
    else
    begin
      Result := GetSql(0, TableInfo, QueryType);
    end;


end;

{$ENDREGION}

{$REGION 'Private Methods'}

procedure TSqlGenerator._cmdBeforeOpen(DataSet: TDataSet);
var
  I: integer;
begin
  for I := 0 to DataSet.Fields.Count - 1 do
  begin
    DataSet.Fields[I].OnGetText := @_DataSetGetText;
  end;
end;

procedure TSqlGenerator._DataSetGetText(Sender: TField; var aText: string;
  DisplayText: boolean);
begin
  aText := Utf8ToAnsi(Sender.AsWideString);
  DisplayText := True;
end;

procedure TSqlGenerator.GetStoredProcedures(var storedProcedures: TStringList);
begin
  storedProcedures.Clear;
  case _DbType of
    dtMsSql: adoConnection.GetStoredProcNames('', storedProcedures);
    dtOracle:
    begin
      if _cmd.Active then
        _cmd.Close;
      _cmd.SQL.Text := 'select OBJECT_NAME from SYS.ALL_OBJECTS' +
        ' where upper(OBJECT_TYPE) = upper(''PROCEDURE'') and' +
        ' Owner=''' + UpperCase(adoConnection.User) + '''' +
        ' order by OBJECT_NAME';
      try
        _cmd.Open;
        while not _cmd.EOF do
        begin
          storedProcedures.Add(_cmd.Fields[0].AsString + ';1');
          _cmd.Next;
        end;
      finally
        _cmd.Close;
      end;
    end;
    dtMySql:
    begin
      if _cmd.Active then
        _cmd.Close;
      _cmd.SQL.Text := 'SELECT SPECIFIC_NAME FROM INFORMATION_SCHEMA.ROUTINES ' +
        ' WHERE ROUTINE_SCHEMA=''' + adoConnection.Database + '''';
      try

        _cmd.Open;
        while not _cmd.EOF do
        begin
          storedProcedures.Add(_cmd.Fields[0].AsString + ';1');
          _cmd.Next;
        end;
      finally
        _cmd.Close;
      end;
    end;
  end;
end;

function TSqlGenerator.GetDbParamPrefix: string;
begin
  case _DbType of
    dtMsSql: Result := '@';
    dtOracle: Result := '_';
    dtMySql: Result := '_';
  end;
end;

procedure TSqlGenerator.CheckProcedureName(Tablename: string);
var
  spInsertName, spSelectName, spSelectItemName, spUpdateName, spDeleteName, uid: string;
begin
  uid := ';1';
  spSelectName := _spPrefix + '' + Tablename + '' + _procNames.pnSelect;
  spSelectItemName := _spPrefix + '' + Tablename + '' + _procNames.pnSelectItem;
  spInsertName := _spPrefix + '' + Tablename + '' + _procNames.pnInsert;
  spDeleteName := _spPrefix + '' + Tablename + '' + _procNames.pnUpdate;
  spUpdateName := _spPrefix + '' + Tablename + '' + _procNames.pnDelete;

  if _lstExistingProcedures.IndexOf(spSelectName + uid) > -1 then
  begin
    WriteLog('Existing storedProc [' + spSelectName + '], executing cleanup...');
    DropProcedure(spSelectName);
  end;

  if _lstExistingProcedures.IndexOf(spSelectItemName + uid) > -1 then
  begin
    WriteLog('Existing storedProc found [' + spSelectItemName +
      '], executing cleanup...');
    DropProcedure(spSelectItemName);
  end;

  if _lstExistingProcedures.IndexOf(spInsertName + uid) > -1 then
  begin
    WriteLog('Existing storedProc found [' + spInsertName + '], executing cleanup...');
    DropProcedure(spInsertName);
  end;

  if _lstExistingProcedures.IndexOf(spUpdateName + uid) > -1 then
  begin
    WriteLog('Existing storedProc found [' + spUpdateName + '], executing cleanup...');
    DropProcedure(spUpdateName);
  end;

  if _lstExistingProcedures.IndexOf(spDeleteName + uid) > -1 then
  begin
    WriteLog('Existing storedProc found [' + spDeleteName + '], executing cleanup...');
    DropProcedure(spDeleteName);
  end;
end;

{Used in GetCreateSql}
function TSqlGenerator.GetDataType(FieldInfo: TFieldInfo; DbType: TDatabaseType): string;
var
  dtype: string;
begin
  case FieldInfo.DataType of
    ftFloat, ftCurrency, ftFMTBcd, ftBCD:
    begin
      if DbType <> dtOracle then
      begin

        if (FieldInfo.FieldType = 'real') or (FieldInfo.FieldType =
          'float') then
          dtype := FieldInfo.FieldType
        else
          dtype := 'decimal(' + IntToStr(FieldInfo.Length) + ',' +
            IntToStr(FieldInfo.Precision) + ')';

      end
      else
      begin
        //ORACLE
        dtype := 'number(' + IntToStr(FieldInfo.Length) + ',' +
          IntToStr(FieldInfo.Precision) + ')';
      end;
    end;
    ftInteger, ftSmallint, ftWord, ftLargeint:
    begin
      if DbType <> dtOracle then
        dtype := 'int'
      else
        dtype := 'number';
    end;
    ftBoolean:
    begin
      case DbType of
        dtMsSql: dtype := 'bit';
        dtMySql: dtype := 'boolean';
        dtOracle: dtype := 'number(1)';//>9i supports boolean
      end;
    end;
    ftBlob: dtype := 'binary';
    ftDateTime, ftDate: dtype := 'datetime';
    ftGuid:
    begin
      case DbType of
        dtMsSql: dtype := 'uniqueidentifier';
        //dtMySql: dtype:='uniqueidentifier'; not supported
        dtOracle: dtype := 'uuid';
      end;
    end;
    ftMemo, ftWideMemo: dtype := 'text';
    else
    begin
      dtype := FieldInfo.FieldType;
      if FieldInfo.Length > 0 then
        dtype := dtype + '(' + IntToStr(FieldInfo.Length) + ')';
    end;
  end;

  //Zeos returns unique identifier as Binary
  if FieldInfo.FieldType = 'uniqueidentifier' then
    dtype := 'uniqueidentifier';

  if (FieldInfo.FieldType = 'ntext') or (FieldInfo.FieldType = 'xml') then
    dtype := 'text';
  if (FieldInfo.FieldType = 'nvarchar') then
    dtype := 'nvarchar(' + IntToStr(FieldInfo.Length) + ')';

  //no need for any param length when oracle ;(
  if DbType = dtOracle then
  begin
    dtype := FieldInfo.FieldType;
  end;

  Result := dtype;
end;

{Used in Generate procedures and functions}
function TSqlGenerator.GetCreateSql(TableInfo: TTableInfo; QueryType: TQueryType;
  SqlSpKeyword: string = 'CREATE '): TStringList;
var
  dtype, seperator, tmpFTablename, tmpTablename: string;

  SqlOpenBr, SqlCloseBr, SqlBeginProc, SqlEndProc, SqlBegin, SqlEnd,
  SqlAsProc, SqlParamChar, SqlPreParam, SqlPostParam, SqlTablename,
  SqlDot, SqlSpSep: string;

  I, oIndex: integer;

  tmpSelect, tmpSelectInsert, tmpInsert, tmpUpdate,
  tmpUpdateAll,//this includes all fields, including PrimaryKeys-s
  tmpParamDeclare, tmpDelParamDeclare, tmpSelectItemParam, tmpSelectJoins: TStringList;

  r: TStringList;
  field: TField;
  tmpInsertParamDeclare: TStringList;
  strTest: string;
  EC: char;
  TC: char;
begin

  EC := ' ';
  TC := #9;

  _schema := TableInfo.Schema;

  case _DbType of
    dtOracle:
    begin
      _spPrefix := UpperCase(_spPrefix);
      SqlOpenBr := '"';
      SqlCloseBr := '"';
      SqlBeginProc := SqlSpKeyword + 'PROCEDURE ';
      SqlAsProc := ' AS ';
      SqlParamChar := 'P';//to differentiate from original column name
      SqlPreParam := '';
      SqlPostParam := ' IN ';
      SqlTablename := TableInfo.Tablename;
      SqlDot := '.';
      SqlBegin := 'BEGIN';
      SqlEnd := 'END;';
    end;
    dtMsSql:
    begin
      SqlOpenBr := '[';
      SqlCloseBr := ']';
      SqlBeginProc := SqlSpKeyword + 'PROCEDURE ';
      SqlAsProc := ' AS ';
      SqlParamChar := '@';
      SqlPreParam := '';
      SqlPostParam := '';
      SqlTablename := TableInfo.Tablename;
      SqlDot := '.';
      SqlBegin := 'BEGIN';
      SqlEnd := 'END';
    end;
    dtMySql:
    begin
      SqlOpenBr := '';
      SqlCloseBr := '';
      SqlBeginProc := SqlSpKeyword + 'PROCEDURE ';
      SqlAsProc := ' ';
      SqlParamChar := '_';//to differentiate from original column name
      SqlPreParam := ' ';
      SqlPostParam := '';
      SqlTablename := '';
      SqlDot := '';
      SqlBegin := '';
      if QueryType = qtSelect then
        SqlBegin := '()'
      else
        SqlBegin := 'BEGIN';
      SqlEnd := 'END;';
    end;
    dtFirebirdd:
    begin
       SqlOpenBr := '"';
      SqlCloseBr := '"';
      SqlBeginProc := SqlSpKeyword + 'PROCEDURE ';
      SqlAsProc := ' AS ';
      SqlParamChar := 'P';
      SqlPreParam := '';
      SqlPostParam := '';
      SqlTablename := TableInfo.Tablename;
      SqlDot := '.';
      SqlBegin := 'BEGIN';
      SqlEnd := 'END';

    end;
  end;

  r := TStringList.Create;

  if (TableInfo.PrimaryKeys.Count = 0) and not (QueryType in [qtSelect, qtInsert]) then
    raise Exception.Create('No primary keys found!');



  tmpSelect := TStringList.Create;
  tmpInsert := TStringList.Create;
  tmpUpdate := TStringList.Create;
  tmpUpdateAll := TStringList.Create;
  tmpInsertParamDeclare := TStringList.Create;
  tmpParamDeclare := TStringList.Create;
  tmpDelParamDeclare := TStringList.Create;
  tmpSelectItemParam := TStringList.Create;
  tmpSelectJoins := TStringList.Create;
  tmpSelectInsert := TStringList.Create;

  for I := 0 to TableInfo.AllFields.Count - 1 do
  begin
    if I < (TableInfo.AllFields.Count - 1) then
      seperator := ','
    else
      seperator := '';

    if (I + 1) = TableInfo.AllFields.Count - 1 then
      if (QueryType in [qtInsert]) then
        if TableInfo.AllFields[I + 1].IsIdentity then
          seperator := '';

    dtype := GetDataType(TableInfo.AllFields[I], _DbType);

    tmpParamDeclare.Add(EC + EC + SqlParamChar + TableInfo.AllFields[I].CSharpName +
      SqlPostParam + EC + dtype + seperator);

    if TableInfo.AllFields[I].IsReference then
    begin
      oIndex := TableInfo.ImportedKeys.GetIndex(TableInfo.AllFields[I].FieldName);
      tmpSelect.Add(TC + TC + SqlOpenBr + SqlTablename + SqlCloseBr +
        SqlDot + SqlOpenBr + TableInfo.AllFields[I].FieldName + SqlCloseBr + ',');
      tmpSelect.Add(TC + TC + 't' + IntToStr(oIndex + 1) + '.' +
        SqlOpenBr + TableInfo.ImportedKeys[oIndex].ForeignFirstTextField +
        SqlCloseBr + seperator);
    end
    else
    begin
      tmpSelect.Add(TC + TC + SqlOpenBr + SqlTablename + SqlCloseBr +
        SqlDot + SqlOpenBr + TableInfo.AllFields[I].FieldName + SqlCloseBr + seperator);
    end;

    if (not TableInfo.AllFields[I].IsIdentity) then
    begin

      tmpInsertParamDeclare.Add(EC + SqlPreParam + SqlParamChar +
        TableInfo.AllFields[I].CSharpName + SqlPostParam + EC + dtype + seperator);

      tmpInsert.Add(TC + TC + SqlParamChar + TableInfo.AllFields[I].CSharpName +
        seperator);
      tmpSelectInsert.Add('    ' + SqlOpenBr + TableInfo.AllFields[I].FieldName +
        SqlCloseBr + seperator);

      //this check will prevent some unwanted stuff like : (..set Status=@Status, where.. )
      if (I + 1) = TableInfo.AllFields.Count - 1 then
        if (TableInfo.AllFields[I + 1].IsPrimaryKey) and
          (TableInfo.PrimaryKeys.Count <> TableInfo.AllFields.Count) then
          seperator := '';

      if not TableInfo.AllFields[I].IsPrimaryKey then
        tmpUpdate.Add(TC + TC + TC + SqlOpenBr + TableInfo.AllFields[I].FieldName +
          SqlCloseBr + '=' + SqlParamChar + TableInfo.AllFields[I].CSharpName +
          seperator);

      tmpUpdateAll.Add(TC + TC + SqlOpenBr + TableInfo.AllFields[I].FieldName +
        SqlCloseBr + '=' + SqlParamChar + TableInfo.AllFields[I].CSharpName + seperator);

    end;

  end;

  //declare params for delete (only ids)
  for I := 0 to TableInfo.PrimaryKeys.Count - 1 do
  begin
    dtype := GetDataType(TableInfo.PrimaryKeys[I], _DbType);
    if I < TableInfo.PrimaryKeys.Count - 1 then
      seperator := ','
    else
      seperator := '';

    tmpDelParamDeclare.Add(TC + TC + SqlPreParam + SqlParamChar +
      TableInfo.PrimaryKeys[I].CSharpName + SqlPostParam + ' ' + dtype + seperator);
    tmpSelectItemParam.Add(TC + TC + SqlPreParam + SqlParamChar +
      TableInfo.PrimaryKeys[I].CSharpName + SqlPostParam + ' ' + dtype + seperator);
  end;

  //make joins for SELECTs
  for I := 0 to TableInfo.ImportedKeys.Count - 1 do
  begin
    tmpFTablename := SqlOpenBr + TableInfo.ImportedKeys[I].ForeignSchema +
      SqlCloseBr + '.' + SqlOpenBr + TableInfo.ImportedKeys[I].ForeignTableName +
      SqlCloseBr;
    tmpTablename := SqlOpenBr + TableInfo.Schema + SqlCloseBr + '.' +
      SqlOpenBr + TableInfo.ImportedKeys[i].TableName + SqlCloseBr;
    tmpSelectJoins.Add
    (
      EC + 'INNER' + EC + 'JOIN' + EC + tmpFTablename + EC + 't' +
      IntToStr(I + 1) + EC + 'ON' + EC + tmpTablename + '.' + SqlOpenBr +
      TableInfo.ImportedKeys[I].ColumnName + SqlCloseBr + ' = ' +
      't' + IntToStr(I + 1) + '.' + SqlOpenBr +
      TableInfo.ImportedKeys[I].ForeignColumnName + SqlCloseBr
      );
  end;

  case QueryType of
    qtSelect:
    begin
      r.Add(SqlBeginProc + TableInfo.Schema + '.' + _spPrefix + '' +
        TableInfo.TableName + '' + _procNames.pnSelect);

      if _DbType = dtOracle then
      begin
        r[r.Count - 1] :=
          r[r.Count - 1] + ' (' + EC + 'RESULT' + EC + 'OUT' + EC +
          'SYS_REFCURSOR' + EC + ')';
      end;

      r.Add(SqlAsProc + EC + SqlBegin);

      if _DbType = dtOracle then
      begin
        r.Add('OPEN' + EC + 'RESULT' + EC + 'FOR');
      end;

      r.Add('SELECT' + EC);
      r.Add(tmpSelect.Text);
      r.Add('FROM' + EC + SqlOpenBr + TableInfo.Schema + SqlCloseBr +
        '.' + SqlOpenBr + TableInfo.Tablename + SqlCloseBr);
      if Trim(tmpSelectJoins.Text) <> EmptyStr then
        r.Add(tmpSelectJoins.Text);
    end;
    qtSelectItem:
    begin
      r.Add(SqlBeginProc + TableInfo.schema + '.' + _spPrefix + '' +
        TableInfo.TableName + '' + _procNames.pnSelectItem);
      r.Add('(');
      r.Add('' + tmpSelectItemParam.Text);
      if _DbType = dtOracle then
      begin
        r[r.Count - 1] :=
          r[r.Count - 1] + ',' + EC + 'RESULT' + EC + 'OUT' + EC + 'SYS_REFCURSOR';
      end;

      r.Add(') ' + SqlAsProc + ' ' + SqlBegin);

      if _DbType = dtOracle then
      begin
        r.Add('OPEN' + EC + 'RESULT' + EC + 'FOR');
      end;
      r.Add('SELECT' + EC);
      r.Add(tmpSelect.Text);
      r.Add('FROM' + EC + SqlOpenBr + TableInfo.schema + SqlCloseBr +
        '.' + SqlOpenBr + TableInfo.TableName + SqlCloseBr);

      if Trim(tmpSelectJoins.Text) <> EmptyStr then
        r.Add(tmpSelectJoins.Text);

      if TableInfo.PrimaryKeys.Count > 0 then
      begin
        r.Add('WHERE' + EC + SqlOpenBr + SqlTablename + SqlCloseBr +
          SqlDot + SqlOpenBr + TableInfo.PrimaryKeys[0].FieldName +
          SqlCloseBr + '=' + SqlParamChar + TableInfo.PrimaryKeys[0].CSharpName);
        if (TableInfo.PrimaryKeys.Count > 1) then
        begin
          for I := 1 to TableInfo.PrimaryKeys.Count - 1 do
            r.Add(EC + 'AND' + EC + SqlOpenBr + TableInfo.Tablename +
              SqlCloseBr + '.' + SqlOpenBr + TableInfo.PrimaryKeys[I].FieldName +
              SqlCloseBr + '=' + SqlParamChar + TableInfo.PrimaryKeys[I].CSharpName);
        end;
      end;
    end;
    qtInsert:
    begin
      r.Add(SqlBeginProc + TableInfo.schema + '.' + _spPrefix + '' +
        TableInfo.TableName + '' + _procNames.pnInsert);
      r.Add('(');
      r.Add(tmpInsertParamDeclare.Text);
      r.Add(')' + EC + SqlAsProc + EC + SqlBegin);
      r.Add('INSERT INTO ' + SqlOpenBr + TableInfo.schema + SqlCloseBr +
        '.' + SqlOpenBr + TableInfo.TableName + SqlCloseBr);
      r.Add('(');
      r.Add(tmpSelectInsert.Text);
      r.Add(')');
      r.Add('VALUES' + EC);
      r.Add('(');
      r.Add(tmpInsert.Text);
      r.Add(')');
    end;
    qtUpdate:
    begin
      r.Add(SqlBeginProc + TableInfo.schema + '.' + _spPrefix + '' +
        TableInfo.TableName + '' + _procNames.pnUpdate);
      r.Add('(');
      r.Add('' + tmpParamDeclare.Text);
      r.Add(') ' + SqlAsProc + ' ' + SqlBegin);
      r.Add('UPDATE' + EC + SqlOpenBr + TableInfo.schema + SqlCloseBr +
        '.' + SqlOpenBr + TableInfo.TableName + SqlCloseBr);
      r.Add('SET' + EC);

      if tmpUpdate.Count > 0 then
        r.Add(tmpUpdate.Text)
      else
        r.Add(tmpUpdateAll.Text);

      if TableInfo.PrimaryKeys.Count > 0 then
      begin
        r.Add('WHERE' + EC + SqlOpenBr + TableInfo.Tablename +
          SqlCloseBr + '.' + SqlOpenBr + TableInfo.PrimaryKeys[0].FieldName +
          SqlCloseBr + '=' + SqlParamChar + TableInfo.PrimaryKeys[0].CSharpName);
        if (TableInfo.PrimaryKeys.Count > 1) then
        begin
          for I := 1 to TableInfo.PrimaryKeys.Count - 1 do
            r.Add(EC + 'AND' + EC + SqlOpenBr + TableInfo.Tablename +
              SqlCloseBr + '.' + SqlOpenBr + TableInfo.PrimaryKeys[I].FieldName +
              SqlCloseBr + '=' + SqlParamChar + TableInfo.PrimaryKeys[I].CSharpName);
        end;
      end;
    end;
    qtDelete:
    begin
      r.Add(SqlBeginProc + TableInfo.schema + '.' + _spPrefix + '' +
        TableInfo.TableName + _procNames.pnDelete);
      r.Add('(');
      r.Add('' + tmpDelParamDeclare.Text);
      r.Add(')' + EC + SqlAsProc + EC + SqlBegin);
      r.Add('DELETE' + EC + 'FROM' + EC + SqlOpenBr + TableInfo.schema +
        SqlCloseBr + '.' + SqlOpenBr + TableInfo.TableName + SqlCloseBr);
      if TableInfo.PrimaryKeys.Count > 0 then
      begin
        r.Add('WHERE' + EC + SqlOpenBr + TableInfo.Tablename +
          SqlCloseBr + '.' + SqlOpenBr + TableInfo.PrimaryKeys[0].FieldName +
          SqlCloseBr + '=' + SqlParamChar + TableInfo.PrimaryKeys[0].CSharpName);
        if (TableInfo.PrimaryKeys.Count > 1) then
        begin
          for I := 1 to TableInfo.PrimaryKeys.Count - 1 do
            r.Add(EC + 'AND' + EC + SqlOpenBr + TableInfo.Tablename +
              SqlCloseBr + '.' + SqlOpenBr + TableInfo.PrimaryKeys[I].FieldName +
              SqlCloseBr + '=' + SqlParamChar + TableInfo.PrimaryKeys[I].CSharpName);
        end;
      end;
    end;
  end;

  case _DbType of
    dtMySql: if (QueryType <> qtSelect) then
        r[r.Count - 1] := r[r.Count - 1] + ';';
    dtOracle: r[r.Count - 1] := r[r.Count - 1] + ';';
  end;


  r.Add(SqlEnd);

  if _DbType = dtOracle then
    r.Add('/');

  tmpDelParamDeclare.Free;
  tmpSelectItemParam.Free;
  tmpParamDeclare.Free;
  tmpSelectJoins.Free;
  tmpSelect.Free;
  tmpInsert.Free;
  tmpUpdate.Free;
  tmpUpdateAll.Free;
  tmpSelectInsert.Free;
  tmpInsertParamDeclare.Free;
  Result := r;
  strTest := Result.Text;
end;


procedure TSqlGenerator.WriteLog(Msg: string);
begin

end;

{$ENDREGION}

end.

