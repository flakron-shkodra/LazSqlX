{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit AsSqlParser;



interface

uses
  Classes, SysUtils, db, AsDbType,fgl,strutils,LazSqlXResources,AsStringUtils;

type

  { TAsPColumnInfo }


  TAsPColumnInfo = class
  private
    FName: string;
    FDataType: string;
    FParentAlias: string;
    FParent: string;
    procedure SetDataType(const Value: string);
    procedure SetName(const Value: string);
    procedure SetParentAlias(const Value: string);
    function GetNameWithAlias: string;
    function GetParentWithAlias: string;
    procedure SetParent(const Value: string);
  public
    property DataType: string read FDataType write SetDataType;
    property ParentAlias: string read FParentAlias write SetParentAlias;
    property Parent: string read FParent write SetParent;
    property ParentWithReference: string read GetParentWithAlias;
    property Name: string read FName write SetName;
    property NameWithRefernce: string read GetNameWithAlias;
  end;

  { TAsPColumnInfoList }

  TAsPColumnInfoObjectList = specialize TFPGObjectList<TAsPColumnInfo>;

  TAsPColumnInfoList = class(TAsPColumnInfoObjectList)
  private
    function GetItm(Name: string): TAsPColumnInfo;
    procedure SetItm(Name: string; const Value: TAsPColumnInfo);
    property Itm[NameWithDotRef: string]: TAsPColumnInfo read GetItm
      write SetItm; default;
  end;

  { TAsPTableInfo }

  TAsPTableInfo = class
  private
    FJoinKeyword: string;
    FName: string;
    FColumns: TAsPColumnInfoList;
    FAlias: string;
    FJoinCondition: string;
    FJoinTableAlias: string;
    procedure SetAlias(const Value: string);
    procedure SetColumns(const Value: TAsPColumnInfoList);
    procedure SetJoinKeyword(AValue: string);
    procedure SetName(const Value: string);
    function GetNameWithAlias: string;
    procedure SetJoinCondition(const Value: string);
    procedure SetJoinTableAlias(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property TableAlias: string read FAlias write SetAlias;
    property Columns: TAsPColumnInfoList read FColumns write SetColumns;
    property JoinCondition: string read FJoinCondition write SetJoinCondition;
    property JoinTableAlias: string read FJoinTableAlias write SetJoinTableAlias;
    property JoinKeyword:string read FJoinKeyword write SetJoinKeyword;
    property Name: string read FName write SetName;
    property NameWithAlias: string read GetNameWithAlias;
  end;

  { TAsPTableInfoList }
  TAsPTableInfoObjectList = specialize TFPGObjectList<TAsPTableInfo>;
  TAsPTableInfoList = class(TAsPTableInfoObjectList)
  public
    function FindByAlias(TableAlias: string): TAsPTableInfo;
    function FindByName(Tablename:string):TAsPTableInfo;
  end;

  { TFlamerSqlParser }

  { TAsSqlParser }

  TAsSqlParser = class
  private
    FCommand:String;
    FAfterConditionInput: string;
    FBeforeSelectInput: string;
    FGroupByClause: string;
    FIsParsed: Boolean;
    FOrderByClause: string;
    FSchema: string;
    FDBInfo:TAsDbConnectionInfo;
    FDBType: TAsDatabaseType;
    FCondition: string;
    FFromTables: TAsPTableInfoList;
    FJoinTables: TAsPTableInfoList;
    FErrorList: TStringList;
    FSelectColumns: TAsPColumnInfoList;
    FAllColumns: TAsPColumnInfoList;
    FProcessedTables: TStringList;
    FQueries:TStringList;
    FTopCount: Integer;
    FHasWhere:Boolean;
    FHasJoin:Boolean;
    function GetErrors: string;
    function GetFieldType(aSchema, aTable, aFieldName: string): string;
    function GetColumnInfos(Table: TAsPTableInfo): TAsPColumnInfoList;
    function ParseSelect(Sql:string):boolean;
    function ParseInsert(Sql:string):boolean;
    function ParseUpdate(Sql:string):boolean;
    function ParseDelete(Sql:string):boolean;
    procedure SetAfterConditionInput(AValue: string);
    procedure SetBeforeSelectInput(AValue: string);
    procedure SetGroupByClause(AValue: string);
    procedure SetOrderByClause(AValue: string);
    procedure SetTopCount(AValue: Integer);
    function IsReserved(aKeyWord:String):Boolean;
  public
    constructor Create(Schema: string; DbInfo: TAsDbConnectionInfo);
    destructor Destroy; override;
    function ParseCommand(Sql: string): Boolean;
    function RegenerateSelect(CastWidestrings: Boolean): string;
    property SelectColumns: TAsPColumnInfoList read FSelectColumns;
    property FromTables: TAsPTableInfoList read FFromTables;
    property JoinTables: TAsPTableInfoList read FJoinTables;
    property TopCount:Integer read FTopCount write SetTopCount;
    property Condition: string read FCondition;
    property BeforeSelectInput: string read FBeforeSelectInput write SetBeforeSelectInput;
    property AfterConditionInput : string read FAfterConditionInput write SetAfterConditionInput;
    property ErrorDescription: string read GetErrors;
    property OrderByClause:string read FOrderByClause write SetOrderByClause;
    property GroupByClause:string read FGroupByClause write SetGroupByClause;
  end;


  TIntegerList = specialize TFPGList<Integer>;

  const
  LoopSeperator: array [0 .. 1] of char = (' ',',');
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

{ TAsPTableInfoList }

function RemoveBrackets(input: string): string;
begin
  Result := TAsStringUtils.RemoveChars(input,['[',']']);
end;

function SplitString(input: string; seperator: char): TArrayOfString;
begin
  Result := TAsStringUtils.SplitString(input,seperator);
end;

function TAsPTableInfoList.FindByAlias(TableAlias: string): TAsPTableInfo;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if uppercase(Items[I].TableAlias) = uppercase(TableAlias) then
    begin
      Result := Items[I];
      Break;
    end;

  end;
end;

function TAsPTableInfoList.FindByName(Tablename: string): TAsPTableInfo;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if uppercase(Items[I].Name) = uppercase(Tablename) then
    begin
      Result := Items[I];
      Break;
    end;

  end;
end;

{ TAsPTableInfo }

procedure TAsPTableInfo.SetAlias(const Value: string);
begin
  FAlias:=Value;
end;

procedure TAsPTableInfo.SetColumns(const Value: TAsPColumnInfoList);
begin
  FColumns := Value;
end;

procedure TAsPTableInfo.SetJoinKeyword(AValue: string);
begin
  if FJoinKeyword=AValue then Exit;
  FJoinKeyword:=AValue;
end;

procedure TAsPTableInfo.SetName(const Value: string);
begin
  FName:=Value;
end;

function TAsPTableInfo.GetNameWithAlias: string;
begin
  if Trim(FAlias) <> EmptyStr then
    Result := FName + ' '+FAlias
  else
    Result := FName;

end;

procedure TAsPTableInfo.SetJoinCondition(const Value: string);
begin
   FJoinCondition:=Value;
end;

procedure TAsPTableInfo.SetJoinTableAlias(const Value: string);
begin
   FJoinTableAlias:=Value;
end;

constructor TAsPTableInfo.Create;
begin
    inherited Create;
   FColumns := TAsPColumnInfoList.Create;
end;

destructor TAsPTableInfo.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

{ TAsPColumnInfoList }

function TAsPColumnInfoList.GetItm(Name: string): TAsPColumnInfo;
var
  I: integer;
  modName: string;
begin
  Result := nil;
  modName := RemoveBrackets( UpperCase(Name));
  for I := 0 to Count - 1 do
  begin
    if (Items[I].NameWithRefernce = modName) or (Items[I].Name = modName)  then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

procedure TAsPColumnInfoList.SetItm(Name: string; const Value: TAsPColumnInfo);
var
  I: integer;
begin
  for I := 0 to Count - 1 do
  begin
    if (Items[I].NameWithRefernce = Name) or (Items[I].Name = Name)  then
    begin
      Items[I] := Value;
      Break;
    end;
  end;
end;

{ TAsPColumnInfo }

procedure TAsPColumnInfo.SetDataType(const Value: string);
begin
  FDataType:=Value;
end;

procedure TAsPColumnInfo.SetName(const Value: string);
begin
    FName:=Value;
end;

procedure TAsPColumnInfo.SetParentAlias(const Value: string);
begin
  FParentAlias:= Value;
end;

function TAsPColumnInfo.GetNameWithAlias: string;
var
  dot: string;
  p:string;
begin
  dot := '.';
  if Trim(FParentAlias) <> '' then
  begin
    p := FParentAlias;
  end
  else
  begin
    p := FParent;
  end;

  Result := RemoveBrackets( UpperCase(p + dot + FName) );
end;

function TAsPColumnInfo.GetParentWithAlias: string;
var
  dot: string;
begin
  if Trim(ParentAlias) <> '' then
    dot := '.'
  else
    dot := '';

  Result := RemoveBrackets( UpperCase(ParentAlias + dot + Parent) );
end;

procedure TAsPColumnInfo.SetParent(const Value: string);
begin
  FParent := Value;
end;

{ TAsSqlParser }

function TAsSqlParser.GetErrors: string;
begin
  Result := FErrorList.Text
end;

function TAsSqlParser.GetFieldType(aSchema, aTable, aFieldName: string): string;
var
  fn:string;
  cols:TAsColumns;
  c:TAsColumn;
begin
  fn :=StringReplace( StringReplace(aFieldName,'[','',[rfReplaceAll]),']','',[rfReplaceAll]);
  try
    cols := TAsDbUtils.GetColumns(FDBInfo,aTable);
    for c in cols do
    begin
      if CompareText(Trim(c.Column_Name),aFieldName)=0 then
      begin
        Result := c.Data_Type;
        Break;
      end;
    end;
  finally
    cols.Free;
  end;
end;

function TAsSqlParser.GetColumnInfos(Table: TAsPTableInfo): TAsPColumnInfoList;
var
  j: integer;
  tmpFields: TStringList;
  Field: TAsPColumnInfo;
  s: String;
begin
  try

    Result := TAsPColumnInfoList.Create;

    // Get Columns from DB for this Table
    tmpFields := TAsDbUtils.GetColumnNames(FDBInfo,Table.Name);

    for s in tmpFields do
    begin
      Field := TAsPColumnInfo.Create;
      Field.Name := UpperCase(s);
      Field.ParentAlias := Table.TableAlias;
      Field.Parent:=UpperCase(Table.Name);
      Field.DataType := GetFieldType(FSchema, Table.Name, Field.Name);
      Result.Add(Field);

      FAllColumns.Add(Field);
    end;

  finally
    tmpFields.Free;
  end;

end;

function TAsSqlParser.ParseSelect(Sql: string): boolean;
var
  CMD: string;
  AllTablenames: TStringList;
  tmpFields: TStringList;
  I: integer;
  DelimitedList: TStringList;
  Table: TAsPTableInfo;
  Field: TAsPColumnInfo;
  j, fp, ip: integer;

  strTableName:string;
  InnerPos,
    WherePos,
    FromPos,
    SelectPos,
    OrderPos,
    GroupPos:  TIntegerList;
  // A var to store INNER JOIN Positions
  ciList:TAsPColumnInfoList;
  strColumn: String;
  strPref: String;
  LastJoinKeyword: String;
begin

  CMD := RemoveBrackets( UpperCase(Sql) );
  Result := True;
  FAllColumns.Clear;
  FIsParsed := False;
  try
    try

      // Im just getting all fields that I can, so I have what to compare with substrings
      tmpFields := TStringList.Create;
      DelimitedList := TStringList.Create;

      InnerPos := TIntegerList.Create;
      WherePos := TIntegerList.Create;
      FromPos := TIntegerList.Create;
      SelectPos := TIntegerList.Create;
      OrderPos := TIntegerList.Create;
      GroupPos := TIntegerList.Create;

      AllTablenames := TAsDbUtils.GetTablenames(FDBInfo);

      // would be delimiting with whitespace and comma
      DelimitedList.Delimiter := ',';
      DelimitedList.DelimitedText := CMD;

      for I := 0 to DelimitedList.Count - 1 do
      begin
        // Find SELECT COUNT(how many queries), FROM,INNER,WHERE POSITIONS

        if (Trim(DelimitedList[I]) = Trim(SQL_SELECT_WORD)) then
        begin
          SelectPos.Add(I);
          if I+1<DelimitedList.Count-1 then
          if DelimitedList[I+1]=Trim(SQL_TOP_WORD) then
          begin
            if I+2<DelimitedList.Count-1 then
            begin
              TryStrToInt(DelimitedList[I+2],FTopCount);
            end;
          end;
        end;
        if (Trim(DelimitedList[I]) = Trim(SQL_FROM_WORD)) then
        begin
          FromPos.Add(I);
        end;
        if (Trim(DelimitedList[I]) = Trim(SQL_JOIN_WORD)) then
        begin
            InnerPos.Add(I);
        end;
        if (Trim(DelimitedList[I]) = Trim(SQL_WHERE_WORD)) then
        begin
          WherePos.Add(I);
        end;

      end;

      if SelectPos.Count=0 then
      SelectPos.Add(0);

      if FromPos.Count = 0 then
        FromPos.Add(0);

      if InnerPos.Count = 0 then
      begin
        InnerPos.Add(DelimitedList.Count);
        FHasJoin:=False;
      end;

      if WherePos.Count = 0 then
      begin
        WherePos.Add(DelimitedList.Count);
        FHasWhere:=False;
      end;


      // EXTRACT FROM CLAUSE TABLES
      for fp in FromPos do
        for I := fp to InnerPos[0]-1  do
        begin

          // it is a table
          strTableName := DelimitedList[I];
          if AnsiContainsStr(strTableName,FSchema+'.') then
          begin
            strTableName:=SplitString(DelimitedList[I],'.')[1];
          end;

          if (AllTablenames.IndexOf(strTableName) > -1) and (not FProcessedTables.IndexOf(strTableName)>-1) then
          begin
            Table := TAsPTableInfo.Create;
            Table.Name :=strTableName;
            if I < DelimitedList.Count - 1 then
            begin
              Table.TableAlias := DelimitedList[I + 1];
              if  IsReserved(Table.TableAlias)  then
              Table.TableAlias:='';
            end;

            ciList := GetColumnInfos(Table);
            for J := 0 to ciList.Count-1 do
            begin
              Table.Columns.Add(ciList.Items[J]);
            end;
            FFromTables.Add(Table);
            FProcessedTables.Add(Table.Name);
          end;

        end;


      InnerPos[0]:=InnerPos[0]-2;//this will help JoinKeyword extraction if it's the first position of Join keyword

      // EXTRACT INNER JOINED TABLES
      for ip in InnerPos do
        for I := ip to WherePos[0] - 1 do
        begin
          //Check the keyword of join
            if  (DelimitedList[I]='INNER') or (DelimitedList[I]='LEFT') OR (DelimitedList[I]='RIGHT') OR (DelimitedList[I]='CROSS')  then
            begin
              //check if it's with two keywords like left outer
              if DelimitedList[I+1] = 'OUTER' then
              begin
                LastJoinKeyword := DelimitedList[I]+' '+DelimitedList[I+1] + ' JOIN ';
              end else
              begin
                LastJoinKeyword :=DelimitedList[I]+' JOIN ';
              end;
            end;


          // it is a table
          strTableName := DelimitedList[I];
          if AnsiContainsStr(strTableName,FSchema+'.') then
          begin
            strTableName:=SplitString(DelimitedList[I],'.')[1];
          end;

          //this verifies if exists as table                this checks if it is already procecessed
          if (AllTablenames.IndexOf(strTableName) > -1) and (not FProcessedTables.IndexOf(strTableName)>-1) then
          begin
            Table := TAsPTableInfo.Create;
            Table.Name := strTableName;
            if I < DelimitedList.Count - 1 then
            begin
              Table.TableAlias := DelimitedList[I + 1];
              if IsReserved(Table.TableAlias) then
              Table.TableAlias:='';

              Table.JoinKeyword:= LastJoinKeyword;
              LastJoinKeyword:='';

              // check condition of join
              if I < DelimitedList.Count - 2 then
              begin
                if DelimitedList[I + 2] = 'ON' then
                begin
                  try
                    Table.JoinCondition := DelimitedList[I + 3] + DelimitedList[I + 4] + DelimitedList[I + 5];
                    Table.JoinCondition:= SplitString(Table.JoinCondition,'=')[0]+' = '+ SplitString(Table.JoinCondition,'=')[1];
                    Table.JoinTableAlias :=SplitString( SplitString(Table.JoinCondition,'=')[0],'.')[0];
                    if Table.JoinTableAlias = Table.TableAlias then
                      Table.JoinTableAlias := SplitString( SplitString( Table.JoinCondition,'=')[1],'.')[0];

                    if IsReserved(Table.JoinTableAlias) then
                    begin
                      if Table.JoinTableAlias=FSchema then
                      Table.JoinTableAlias:='';
                    end;

                  except
                  end;

                end;

              end;

            end;
            ciList := GetColumnInfos(Table);
            for J:=0 to ciList.Count-1 do
            begin
              Table.Columns.Add(ciList.Items[J]);

            end;
            FJoinTables.Add(Table);
            FProcessedTables.Add(Table.Name);
          end;

        end;

      // FIND SELECT FIELDS
      for I := 0 to FromPos[0] - 1 do
      begin
        if Trim(DelimitedList[I]) <> Trim(SQL_SELECT_WORD) then
        begin
          strColumn := DelimitedList[I];
          if AnsiContainsText(strColumn,'.') then
          begin
            strPref := SplitString(DelimitedList[I],'.')[0];
            strColumn:= SplitString( DelimitedList[I],'.')[1];
          end;

          //Handling of all sign (*)
          if Trim(strColumn)='*' then
          begin

            //find alias called (*) if it's empty, it's main from table
            if strPref=EmptyStr then
            begin
              strPref:=FFromTables[0].Name;
            end;

            Table:=FFromTables.FindByAlias(strPref);
            if Table=nil then
            Table := FFromTables.FindByName(strPref);

            if Table=nil then
            Table := FJoinTables.FindByAlias(strPref);

            if Table=nil then
            Table := FJoinTables.FindByName(strPref);


            ciList := GetColumnInfos(Table);
            for J:=0 to ciList.Count-1 do
            begin
              SelectColumns.Add(ciList.Items[J]);
            end;

          end else
          begin
              //this finds column in variable that has been previously assigned
              if (FAllColumns[strColumn]) <> nil then
              begin
                Field := TAsPColumnInfo.Create;

                try
                  Field.Name := SplitString( DelimitedList[I],'.')[1];
                  Field.ParentAlias := SplitString( DelimitedList[I],'.')[0];
                except
                  //the field has no alias callaer, only fieldname
                  Field.Name:=SplitString( DelimitedList[I],'.')[0];
                end;

                //this means a entire tablename was used instead of alias, before columnname, like tablename.columnname
                if FProcessedTables.IndexOf(Field.ParentAlias)>-1 then
                begin
                  Field.Parent:=Field.ParentAlias;
                  Field.ParentAlias:='';
                  Table := FFromTables.FindByName(Field.Parent);

                  if not Assigned(Table) then
                  Table := FJoinTables.FindByName(Field.Parent);

                end else
                begin
                  Table := FFromTables.FindByAlias(Field.ParentAlias);

                  if not Assigned(Table) then
                    Table := FJoinTables.FindByAlias(Field.ParentAlias);

                  Field.Parent:=Table.Name;
                end;

                Field.Parent := Table.Name;
                Field.DataType := GetFieldType(FSchema, Field.Parent, Field.Name);

                SelectColumns.Add(Field);
              end
              else
              begin
                if (not IsReserved(DelimitedList[I])) and (TAsStringUtils.IsNumber(DelimitedList[I])) then
                begin
                  FErrorList.Add('Column [' + DelimitedList[I] + '] doesn''t exist');
                  Result := False;
                end;
              end;
          end;//if not (*) handler
        end;
      end;
//
//      If not FHasWhere then
//      if FromPos[0]+1 < DelimitedList.Count-1 then
//      WherePos[0] :=FromPos[0]+1;

      for ip in WherePos do
        for I := ip to DelimitedList.Count - 1 do
        begin
          FCondition := FCondition + ' ' + DelimitedList[I];
        end;

      FIsParsed := True;
    except
      on e: exception do
      begin
        FErrorList.Add(e.Message);
        Result := False;
      end;
    end;

  finally
    OrderPos.Free;
    GroupPos.Free;
    SelectPos.Free;
    InnerPos.Free;
    WherePos.Free;
    FromPos.Free;
    DelimitedList.Free;
    tmpFields.Free;
    AllTablenames.Free;
  end;

end;

function TAsSqlParser.ParseInsert(Sql: string): boolean;
begin

end;

function TAsSqlParser.ParseUpdate(Sql: string): boolean;
begin

end;

function TAsSqlParser.ParseDelete(Sql: string): boolean;
begin

end;

procedure TAsSqlParser.SetAfterConditionInput(AValue: string);
begin
  if FAfterConditionInput=AValue then Exit;
  FAfterConditionInput:=AValue;
end;

procedure TAsSqlParser.SetBeforeSelectInput(AValue: string);
begin
  if FBeforeSelectInput=AValue then Exit;
  FBeforeSelectInput:=AValue;
end;

procedure TAsSqlParser.SetGroupByClause(AValue: string);
begin
  if FGroupByClause=AValue then Exit;
  FGroupByClause:=AValue;
end;

procedure TAsSqlParser.SetOrderByClause(AValue: string);
begin
  if FOrderByClause=AValue then Exit;
  FOrderByClause:=AValue;
end;

procedure TAsSqlParser.SetTopCount(AValue: Integer);
begin
  if FTopCount=AValue then Exit;
  FTopCount:=AValue;
end;

function TAsSqlParser.IsReserved(aKeyWord: String): Boolean;
begin
 Result := TLazSqlXResources.SqlReservedKeywords.IndexOf(Trim(Uppercase(aKeyword)))>-1
end;

constructor TAsSqlParser.Create(Schema: string; DbInfo: TAsDbConnectionInfo);
var
  r:TResourceStream;
begin
  FSchema := UpperCase(Schema);

  FDBInfo := DbInfo;

  FErrorList := TStringList.Create;

  FAllColumns := TAsPColumnInfoList.Create;
  FFromTables := TAsPTableInfoList.Create;
  FJoinTables := TAsPTableInfoList.Create;
  FSelectColumns := TAsPColumnInfoList.Create;
  FProcessedTables := TStringList.Create;
  FQueries := TStringList.Create;

end;

destructor TAsSqlParser.Destroy;
begin
  FFromTables.Free;
  FJoinTables.Free;
  FSelectColumns.Free;
  FErrorList.Free;
  FProcessedTables.Free;
  FQueries.Free;
  inherited Destroy;
end;

function TAsSqlParser.ParseCommand(Sql: string): Boolean;
begin
  FCommand:=Sql;
  if AnsiStartsText(SQL_SELECT_WORD,Trim(Sql)) then
  begin
    Result:=ParseSelect(Sql);
  end else
  if AnsiContainsText(SQL_INSERT_WORD,Trim(Sql)) then
  begin
    Result :=ParseInsert(Sql);
  end else
  if AnsiContainsText(SQL_UPDATE_WORD,Trim(Sql)) then
  begin
    Result := ParseUpdate(Sql);
  end;
end;

function TAsSqlParser.RegenerateSelect(CastWidestrings: Boolean): string;
var
  I: integer;
  CMD: TStringList;
  s: string;
  tab : char;
  c:TAsPColumnInfo;
  pref,suf:string;
  strJoinCondition: String;
begin
  tab := #9;

  if FIsParsed then
  begin
    try
      CMD := TStringList.Create;
      CMD.Add(FBeforeSelectInput);

      if FTopCount>0 then
      begin
        CMD.Add(SQL_SELECT_WORD + ' '+SQL_TOP_WORD+IntToStr(FTopCount)+' ');
      end else
      begin
        CMD.Add(SQL_SELECT_WORD);
      end;

      for I := 0 to FSelectColumns.Count - 1 do
      begin
        c :=  FSelectColumns.Items[I];
        if CastWidestrings then
        begin
          pref:='';
          suf:='';
          if (c.DataType = 'nvarchar') then
          begin
            pref:='CAST(';
            suf :=' AS VARCHAR) as '+FSelectColumns.Items[I].Name;
          end;

          if (FSelectColumns.Items[I].DataType = 'ntext') then
          begin
            pref:='CAST(';
            suf :=' AS TEXT) as '+FSelectColumns.Items[I].Name;
          end;


            s := pref+ c.NameWithRefernce + suf +
              LoopSeperator[integer(I < FSelectColumns.Count - 1)];
        end
        else
          s := c.NameWithRefernce +
            LoopSeperator[integer(I < FSelectColumns.Count - 1)];

        CMD.Add(tab+s);

      end;

      CMD.Add('FROM ');

      for I := 0 to FFromTables.Count - 1 do
      begin
        CMD.Add(FFromTables[I].NameWithAlias +
          LoopSeperator[integer(I < FFromTables.Count - 1)]);
      end;

      for I := 0 to FJoinTables.Count - 1 do
      begin
        strJoinCondition := FJoinTables[I].JoinCondition;
        CMD.Add(FJoinTables[I].JoinKeyword+' '+ FJoinTables[I].NameWithAlias + ' ON ' +FJoinTables[I].JoinCondition);
      end;

      CMD.Add(FCondition);

      Result := CMD.Text;
    finally
      CMD.Free;
    end;

  end;
end;


end.
