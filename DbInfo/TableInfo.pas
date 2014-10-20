{*******************************************************************
AUTHOR : Flakron Shkodra
*******************************************************************
version 1.1, moddate:23.08.2011
*******************************************************************

*******************************************************************
}

unit TableInfo;

{$mode objfpc}{$H+}

interface

uses SysUtils,Classes,DbType,ZConnection,ZDataset,ZDbcIntfs,ZSqlMetadata,
DB,typinfo, fgl,Forms,StdCtrls,AsStringUtils,LazSqlXResources;


type


 ISomething = interface(IInterface)

 end;

 TControlType = (ctUnknown,ctTextBox,ctComboBox,ctDateTimePicker,ctCheckBox,ctNumeric);

 {this enum not used yet}
 TControlNameStyle = (cnsShortPrefixName,cnsNameLongPrefix);

 TWinFormType = (wfTransaction,wfDefinition);

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

 TTriggerInfo = class
 public
  Cat:string;
  Schem:string;
  Name:string;
  Relation:string;
  TriggerType:Integer;
  Inactive:Boolean;
  TriggerSource:string;
  Description:string;
 end;

 { TTriggerInfos }

 TTriggerInfos = class(specialize TFPGObjectList<TTriggerInfo>)
 public
  function GetByName(trigger:string):TTriggerInfo;
 end;

 TIndexInfo = class
  INDEX_Qualifier:string;
  INDEX_Name:string;
  IndexType:Integer;
  Ordinal_Position:Integer;
  Column_Name:string;
  ASC_OR_DESC:string;
  FILTER_CONDITION:string;
  Is_Unique:Boolean;
 end;

 { TIndexInfos }

 TIndexInfos = class(Specialize TFPGObjectList<TIndexInfo>)
 public
  function GetByName(IndexName:string):TIndexInfo;
 end;

 { TFieldInfo }

 TFieldInfo = class
 public
  FieldName:string;

  FieldType:string;
  Field:TField;
  CSharpType:string;
  CSharpName:string;
  DataType:TFieldType;
  IsPrimaryKey:Boolean;
  IsIdentity:Boolean;
  AllowNull:Boolean;
  Length:Integer;
  Precision:Integer;
  ControlType:TControlType;
  Validate:Boolean;//added for DevBooster Form Generator Compat
  IsReference:Boolean;
  Reserved:string;
  FieldDbType:TDatabaseType;
 public
 		constructor Create;
    procedure CopyFrom(FieldInfo:TFieldInfo);
    function WebControlName:string;
    function WebControlNameWithProp:string;
    function IsNumeric:Boolean;
    function WebControlPrefix:string;
    function WinControlPrefix:string;
    function WinControlName:string;
    function GetFieldTypeAs(db:TDatabaseType):String;
    function GetCompatibleFieldName(dbType: TDatabaseType): string;
 end;

 { TFieldInfos }

 TFieldInfosClass = specialize TFPGObjectList<TFieldInfo>;

 TFieldInfos = class(TFieldInfosClass)
 private
   function GetFieldInfo(Index: Integer): TFieldInfo;
 private
    property Item[Index:Integer]:TFieldInfo read GetFieldInfo;default;
 public
    function GetIndex(FieldName:string):Integer;
    function ToStringList:TStringList;
 end;

 { TImportedKey }

 TImportedKey = class
 private
    FTablename:string;
    FTableAlias:string;
    FForeignTableAlias:string;
    FForeignTablename:string;
    procedure SetForiegnTablename(AValue: string);
    procedure SetTablename(AValue: string);
 public
   ConstraintName:string;
   ColumnName:string;
   SelectFields: TStringList;
   ForeignSchema:string;
   ForeignColumnName:string;
   ForeignFirstTextField:string;
   Index:Integer;
   function TableAndColumn:string;
   constructor Create;
   destructor Destroy;
   function GetCompatibleColumnName(dbType: TDatabaseType): string;
   function GetCompatibleForeignColumnName(dbType: TDatabaseType): string;
   property Tablename:string read FTablename write SetTablename;
   property TableAlias:string read FTableAlias write FTableAlias;
   property ForeignTableName:string read FForeignTablename write SetForiegnTablename;
   property ForeignTableAlias:string read FForeignTableAlias write FForeignTableAlias;
 end;

 { TExportedKeyInfo }

 TExportedKeyInfo = class
 private
   FTable: string;
   FTableAlias:string;
   procedure SetTablename(AValue: string);

 public
   ColumnName:string;
   ForeignSchemName:string;
   ForeignColumnName:string;
   property ForeignTableName:string read FTable write SetTablename;
   property ForeignTableAlias:string read FTableAlias write FTableAlias;
 end;


 { TExportedKeyInfos }

 TExportedKeyInfos = class(specialize TFPGObjectList<TExportedKeyInfo>)
 private
   function GetItm(Index: Integer): TExportedKeyInfo;
 private
   property Item[Index:Integer]:TExportedKeyInfo read GetItm; default;

 end;

 { TImportedKeys }

 TImportedKeys = class(specialize TFPGObjectList<TImportedKey>)
 private
   function GetRefInfo(Index: integer): TImportedKey;
 private
   property Item[Index:integer]:TImportedKey read GetRefInfo;default;
 public
    function Add(const Itm: TImportedKey): Integer;
    function ContainsColumn(Fieldname:string):Boolean;
    function ContainsTable(Tablename:string):Boolean;
    function GetIndex(Fieldname:string):Integer;
    function GetByName(Fieldname:string):TImportedKey;
    function GetByField(Field:TFieldInfo):TImportedKey;
 end;

 { TTableInfo }

 TTableInfo = class
 strict private
   FExportedKeys: TExportedKeyInfos;
   FGenerateWebForm: Boolean;
   FGenerateWinForm: Boolean;
  FAllFields:TFieldInfos;//to contain all fields
  FIndexInfos: TIndexInfos;
  FPrimaryKeys:TFieldInfos;//to contain only Pks
  FFields:TFieldInfos; //to contain only fields that are not PKs
  FIdentities:TFieldInfos;
  FTableAlias: string;
  FTablename:string;
  FSchema:string;
  FReferences:TImportedKeys;
  FHasPrimaryKey:Boolean;
  FTriggerInfos: TTriggerInfos;
  FWinFormType:TWinFormType;
  FTableCSharpName:string;
  function GetHasPrimaryKeys: Boolean;
  procedure SetGenerateWebForm(const AValue: Boolean);
  procedure SetGenerateWinForm(const AValue: Boolean);
  procedure SetTableName(AValue: string);
 public

  constructor Create;
  destructor Destroy;override;
  property Tablename:string read FTablename write SetTableName;
  property TableCSharpName:string read FTableCSharpName write FTableCSharpName;
  property Schema:string read FSchema write FSchema;
  {All fields}
  property AllFields:TFieldInfos read FAllFields write FAllFields;
  {PrimaryKey fields only}
  property PrimaryKeys:TFieldInfos read FPrimaryKeys write FPrimaryKeys;
  {Fields that are not primary keys or identities}
  property Fields:TFieldInfos read FFields write FFields;
  {Fields that are identities only}
  property Identities:TFieldInfos read FIdentities write FIdentities;
  {Constraints of table}
  property ImportedKeys:TImportedKeys read FReferences write FReferences;
  {Shows if this tables PK is reference of another table}
  property ExportedKeys:TExportedKeyInfos read FExportedKeys write FExportedKeys;

  property Triggers:TTriggerInfos read FTriggerInfos write FTriggerInfos;
  property Indexes:TIndexInfos read FIndexInfos write FIndexInfos;

  property HasPrimaryKeys:Boolean read GetHasPrimaryKeys;
  property FormType:TWinFormType read FWinFormType write FWinFormType;
  function FieldByName(Fieldname:string):TFieldInfo;
  function PrimaryKeysAsDelText:string;
  property GenerateWinForm:Boolean read FGenerateWinForm write SetGenerateWinForm;
  property GenerateWebForm:Boolean read FGenerateWebForm write SetGenerateWebForm;
  {Compares this instance to given tableInfo returns SQL valid code for altering if they are different}
  function Compare(otherTable:TTableInfo;DbType:TDatabaseType):string;
  property TableAlias:string read FTableAlias write FTableAlias;

 end;

 { TTableInfos }

 TTableInfos = class(specialize TFPGObjectList<TTableInfo>)

 private
   function GetTable(Index: integer): TTableInfo;
 strict private
    adoCon:TZConnection;
    dsMetaData:TZSQLMetadata;
    FLogStrings:TStrings;
    FApplication:TApplication;

    FLogListBox:TListBox;
    FDBinfo:TDbConnectionInfo;
    procedure GetConstraintInfos(Schema:string;Tablename:string;var ConstraintInfos:TImportedKeys);
    procedure GetExportedKeyInfos(Schema:string;Tablename:string;var ExportKeyInfos:TExportedKeyInfos);
    procedure GetPrimaryKeys(Schema:string;TableName:string;var KeyFields:TStringList);
    function GetSqlType(AdoType:TFieldType):string;
    function GetCSharpType(SqlType:String):string;
    function GetFieldType(SqlType:String):TFieldType;

    function GetCSharpName(Tablename,SqlFieldName:string):string;
    function GetCSharpTableName(TableName:string):string;
    function GetControlType(SqlType:string):TControlType;
    procedure WriteLog(msg:string);
    property Item[Index:integer]:TTableInfo read GetTable;default;
 public
    constructor Create(DbInfo:TDbConnectionInfo);
    destructor Destroy;override;
    procedure LoadFromTables(Schema:string; Tables:TStrings);
    function GetTableInfo(Schema:string; Tablename:string):TTableInfo;
    function  TableByName(Tablename:string):TTableInfo;
    function IndexOf(TableName:string):Integer;
    function GetCreateSQL(TableIndex:Integer):string;
    procedure AddTable(Schema,Tablename:string);
    property LogStrings:TStrings read FLogStrings write FLogStrings;
    property LogList:TListBox read FLogListBox write FLogListBox;
 end;



implementation



{ TExportedKeyInfo }

procedure TExportedKeyInfo.SetTablename(AValue: string);
begin
 if FTable=AValue then Exit;
 FTable:=AValue;

 FTableAlias:=TAsStringUtils.GetFriendlyAlias(FTable);

end;



{ TIndexInfos }

function TIndexInfos.GetByName(IndexName: string): TIndexInfo;
var
 I: Integer;
begin
 for I:=0 to Count-1 do
 begin
  if Items[I].INDEX_Name = IndexName then
  begin
    Result := Items[I];
    Break;
  end;

 end;
end;

{ TTriggerInfos }

function TTriggerInfos.GetByName(trigger: string): TTriggerInfo;
begin

end;


{ TExportedKeyInfos }

function TExportedKeyInfos.GetItm(Index: Integer): TExportedKeyInfo;
begin
  Result := Items[Index];
end;

{ TFieldInfos }

function TFieldInfos.GetFieldInfo(Index: Integer): TFieldInfo;
begin
  Result := Items[Index];
end;

function TFieldInfos.GetIndex(FieldName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I:=0 to Count-1 do
  begin
    if Items[I].FieldName = FieldName then
    begin
      Result := I;
      break;
    end;
  end;
end;

function TFieldInfos.ToStringList: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I:=0 to Count-1 do
  begin
    Result.Add(Self[I].FieldName);
  end;
end;


{$Region '   TFieldInfo Implemenatation  ' }

constructor TFieldInfo.Create;
begin

end;

function TFieldInfo.IsNumeric: Boolean;
begin
   Result := (CSharpType='int') or (CSharpType='decimal');
end;

function TFieldInfo.WebControlName: string;
var
	s:string;
begin
    case ControlType of
      ctUnknown: s:='Unknown';
      ctTextBox,ctNumeric,ctDateTimePicker:
        begin
        	s:='txt'
        end;
      ctComboBox:
        begin
          s:='cmb'
        end;
      ctCheckBox:
        begin
        	s:='chk'
        end;
    end;
    Result := s+CSharpName
end;

function TFieldInfo.WebControlNameWithProp: string;
var
 s:string;
begin
 case ControlType of
   ctTextBox,
   ctDateTimePicker,
   ctNumeric: 	s := '.Text';
   ctComboBox:	s:= '.SelectedValue';
   ctCheckBox:	s:= '.Checked';
 end;
 Result := WebControlName+s;
end;

function TFieldInfo.WebControlPrefix: string;
var
 s:string;
begin
 case ControlType of
   ctTextBox,
   ctDateTimePicker,
   ctNumeric: 	s := 'txt';
   ctComboBox:	s:= 'cmb';
   ctCheckBox:	s:= 'chk';
 end;
 Result := s;
end;

function TFieldInfo.WinControlName: string;
begin
  Result := WinControlPrefix+CSharpName;
end;

function TFieldInfo.GetFieldTypeAs(db: TDatabaseType): String;
begin

if DataType=ftUnknown then
begin
 Result := FieldType;
end
else
case db of
 dtMsSql:
 begin
    case DataType of
      ftCurrency,ftBCD,ftFloat: Result:='decimal';
      ftMemo:Result := 'text';
      ftWideMemo: Result := 'ntext';
      ftWideString: Result:='nvarchar';
      ftBlob,ftOraBlob:
          begin
           Result:='blob';
           if TDbUtils.IsBlobGUID(Field) then
           begin
             Result:='varchar';
           end else
           begin
            Result :='blob';
           end;
         end;
      ftBoolean:Result:='bit';
      ftDate,ftDateTime:Result:='datetime';
      ftInteger,ftSmallint,ftLargeint: Result:='int' ;
      ftBytes,ftVarBytes: Result:='binary'
      else
      Result:='varchar';
    end;
 end;

 dtOracle:
 begin
    case DataType of
      ftCurrency,ftBCD,ftFloat: Result:='decimal';
      ftWideMemo: Result := 'ntext';
      ftWideString: Result:='nvarchar2';
      ftBlob,ftOraBlob:
        begin
           Result:='blob';
           if TDbUtils.IsBlobGUID(Field) then
           begin
             Result:='varchar';
           end else
           begin
            Result :='blob';
           end;
         end;
      ftBoolean:Result:='numeric';
      ftDate,ftDateTime:Result:='date';
      ftInteger,ftSmallint: Result:='numeric' ;
      ftBytes,ftVarBytes: Result:='binary';
      else
      Result:='varchar2';
    end;
 end;

 dtMySql:
 begin
    case DataType of
      ftCurrency,ftBCD,ftFloat: Result:='decimal';
      ftWideMemo: Result := 'text';
      ftWideString: Result:='varchar';
      ftBlob,ftOraBlob:
         begin
           Result:='blob';
           if TDbUtils.IsBlobGUID(Field) then
           begin
             Result:='varchar';
           end else
           begin
            Result :='blob';
           end;
         end;

      ftBoolean:Result:='int';
      ftDate,ftDateTime:Result:='varchar';
      ftInteger,ftSmallint: Result:='int' ;
      ftBytes,ftVarBytes: Result:='blob';
      else
      Result:='varchar';
    end;



 end;
 dtSQLite:
 begin
   case DataType of
      ftCurrency,ftBCD,ftFloat: Result:='real';
      ftWideMemo: Result := 'text';
      ftWideString: Result:='text';
      ftBlob,ftOraBlob:
        begin
           Result:='blob';
           if TDbUtils.IsBlobGUID(Field) then
           begin
             Result:='varchar';
           end else
           begin
            Result :='blob';
           end;
         end;
      ftBoolean:Result:='integer';
      ftDate,ftDateTime:Result:='text';
      ftInteger: Result:='integer' ;
      ftBytes,ftVarBytes: Result:='blob';
      else
      Result:='text';
    end;
 end;
 dtFirebirdd:
    begin
     case DataType of
      ftCurrency,ftBCD,ftFloat: Result:='decimal';
      ftWideMemo: Result := 'text';
      ftWideString: Result:='varchar';
      ftBlob,ftOraBlob:
         begin
           Result:='blob';
           if TDbUtils.IsBlobGUID(Field) then
           begin
             Result:='varchar';
           end else
           begin
            Result :='blob';
           end;
         end;

      ftBoolean:Result:='int';
      ftDate,ftDateTime:Result:='varchar';
      ftInteger,ftSmallint: Result:='int' ;
      ftBytes,ftVarBytes: Result:='blob';
      else
      Result:='varchar';
    end;
    end;
end;

end;

function TFieldInfo.GetCompatibleFieldName(dbType:TDatabaseType): string;
var
 o,c:string;
begin
 case dbType of
  dtMsSql:
   begin
     o:='[';
     c:=']';
   end;
  dtOracle:
      begin
     o:='"';
     c:='"';
   end;
  dtMySql:
       begin
     o:='';
     c:='';
   end;
  dtSQLite:
     begin
     o:='[';
     c:=']';
   end;
 end;
 Result:=o+FieldName+c;
end;

function TFieldInfo.WinControlPrefix: string;
var
 s:string;
begin
 case ControlType of
   ctTextBox,
   ctNumeric: 	s := 'txt';
   ctComboBox:	s:= 'cmb';
   ctCheckBox:	s:= 'chk';
   ctDateTimePicker: s:='dtp';
 end;
 Result := s;
end;

procedure TFieldInfo.CopyFrom(FieldInfo: TFieldInfo);
begin
  if Self<>nil then
  begin
   FieldName := FieldInfo.FieldName;
   FieldType := FieldInfo.FieldType;
   Length := FieldInfo.Length;
   Precision:= FieldInfo.Precision;
   DataType := FieldInfo.DataType;
   CSharpType := FieldInfo.CSharpType;
   CSharpName := FieldInfo.CSharpName;
   IsPrimaryKey := FieldInfo.IsPrimaryKey;
   IsIdentity := FieldInfo.IsIdentity;
   IsReference := FieldInfo.IsReference;
   Validate := FieldInfo.Validate;
   AllowNull := FieldInfo.AllowNull;
   ControlType := FieldInfo.ControlType;
   Reserved := FieldInfo.Reserved;
   Field := FieldInfo.Field;
   FieldDbType:= FieldInfo.FieldDbType;
  end;
end;

{$ENDREGION}

{$REGION '   TTableInfo Implementation   ' }

{$REGION 'Constructor/Destructor'}

procedure TTableInfo.SetGenerateWebForm(const AValue: Boolean);
begin
  if FGenerateWebForm=AValue then exit;
  FGenerateWebForm:=AValue;
end;

function TTableInfo.GetHasPrimaryKeys: Boolean;
begin
 Result := False;
 if FPrimaryKeys <> nil then
  Result := FPrimaryKeys.Count>0;
end;

procedure TTableInfo.SetGenerateWinForm(const AValue: Boolean);
begin
  if FGenerateWinForm=AValue then exit;
  FGenerateWinForm:=AValue;
end;

procedure TTableInfo.SetTableName(AValue: string);
begin
 if FTablename=AValue then Exit;
 FTablename:=AValue;
 FTableAlias:=TAsStringUtils.GetFriendlyAlias(FTablename);
end;


constructor TTableInfo.Create;
begin
  inherited Create;
  FFields := TFieldInfos.Create;
  FAllFields := TFieldInfos.Create;
  FIdentities := TFieldInfos.Create;
  FPrimaryKeys := TFieldInfos.Create;

  FExportedKeys := TExportedKeyInfos.Create;
  FReferences := TImportedKeys.Create;

  FTriggerInfos := TTriggerInfos.Create;
  FIndexInfos := TIndexInfos.Create;

  FGenerateWinForm:=True;
  FGenerateWebForm:=True;
end;

destructor TTableInfo.Destroy;
begin
  FAllFields.Free;
  FFields.Free;
  FPrimaryKeys.Free;
  FIdentities.Free;
  FReferences.Free;
  FExportedKeys.Free;
  FTriggerInfos.Free;
  FIndexInfos.Free;
  inherited Destroy;
end;

function TTableInfo.FieldByName(Fieldname: string): TFieldInfo;
var
 i:Integer;
begin
  for i := 0 to AllFields.Count do
  begin
      if AllFields[I].FieldName=Fieldname then
      begin
        Result := AllFields[I];
        Break;
      end;
  end;
end;

function TTableInfo.PrimaryKeysAsDelText: string;
var
  I: Integer;
  s: string;
  k: string;
begin
  k:='';
	for I := 0 to FPrimaryKeys.Count - 1 do
  begin
  		if (I<FPrimaryKeys.Count-1) then
      	s := ','
      else
      	s :='';
      k := k + FPrimaryKeys[I].FieldName+s;
  end;
  Result := k;
end;

function TTableInfo.Compare(otherTable: TTableInfo;DbType:TDatabaseType): string;
var
  I: Integer;
  lst:TStringList;
  SqlFromDot: string;
  SqlOpenBr: string;
  SqlCloseBr: string;
  SqlDot: string;
begin

 SqlFromDot:='.';

  case DbType of
    dtMsSql:
    begin
      SqlOpenBr:='[';
      SqlCloseBr:=']';
      SqlDot:='.';
    end;
    dtOracle:
    begin
      SqlOpenBr:='';
      SqlCloseBr:='';
      SqlDot:='.';
    end;
    dtMySql:
    begin
      SqlOpenBr:='';
      SqlCloseBr:='';
      SqlDot:='';
    end;
    dtSQLite:
     begin
      SqlOpenBr:='';
      SqlCloseBr:='';
      SqlDot:='.';
     end;
  end;

 lst := TStringList.Create;
 try

  lst.Add('--Compare Source ['+Tablename+']  with Destination ['+otherTable.Tablename+']');

  if otherTable.AllFields.Count <> AllFields.Count then
  begin
    lst.Add('--FieldCount is different');
  end;

  for I:=0 to FAllFields.Count-1 do
  begin

      if otherTable.AllFields.GetIndex(FAllFields[I].FieldName) < 0 then
      begin
        lst.Add('ALTER TABLE '+SqlOpenBr+otherTable.Tablename+SqlCloseBr+' ADD '+SqlOpenBr+AllFields[I].FieldName+SqlCloseBr+' '+AllFields[I].FieldType);
      end;

  end;

  Result := lst.Text;

 finally
  lst.Free;
 end;

end;



{$ENDREGION}

{$ENDREGION}

{$REGION '  TTableInfos Implementation   ' }

{$REGION 'Constructor/Desctructor'}

constructor TTableInfos.Create(DbInfo: TDbConnectionInfo);
var
 r : TResourceStream;
begin

  inherited Create;

  FDBinfo := DbInfo;
  adoCon := DbInfo.ToZeosConnection;
  dsMetaData := TZSQLMetadata.Create(nil);
  dsMetaData.Connection := adoCon;
  dsMetaData.DisableControls;
  adoCon.Connect;

end;

destructor TTableInfos.Destroy;
begin
  adoCon.Free;
  dsMetaData.Free;
  inherited Destroy;
end;


{$ENDREGION}

{$REGION 'Private'}

function TTableInfos.GetFieldType(SqlType:String):TFieldType;
begin
  if  (SqlType='varchar') or
      (SqlType='nvarchar') or
      (SqlType='text') or
      (SqlType='nchar') or
      (SqlType='char') or
      (SqlType='ntext') or
      (SqlType='xml')
  then
  	Result := ftString
  else
  if SqlType='image' then
  Result := ftBlob
  else
  if (SqlType='tinyint') or
     (SqlType='int') or
     (SqlType='bigint') or
     (SqlType='smallint')
  then
  	Result:=ftInteger
  else
  if  (SqlType='real') or
     (SqlType='float') or
     (SqlType='decimal') or
     (SqlType='money') or
     (SqlType='smallmoney')
  then
    Result := ftFloat
  else
  if SqlType='bit'
  then
  	Result:=ftBoolean
  else
  if SqlType='datetime' then
  	Result:=ftDateTime
  else
  Result:=ftString;
end;


function TTableInfos.GetControlType(SqlType: string): TControlType;
begin

  if  (SqlType='varchar') or
      (SqlType='nvarchar') or
      (SqlType='text') or
      (SqlType='nchar') or
      (SqlType='char') or
      (SqlType='ntext') or
      (SqlType='image')
  then
  	Result := ctTextBox
  else
  if (SqlType='tinyint') or
     (SqlType='int') or
     (SqlType='bigint') or
     (SqlType='smallint') or
     (SqlType='real') or
     (SqlType='float') or
     (SqlType='decimal') or
     (SqlType='money') or
     (SqlType='smallmoney')
  then
  	Result:=ctNumeric
  else
  if SqlType='bit'
  then
  	Result:=ctCheckBox
  else
  if SqlType='datetime' then
  	Result:=ctDateTimePicker
  else
  Result:=ctTextBox;
end;

function TTableInfos.GetCSharpName(Tablename,SqlFieldName: string): string;
var
 i:Integer;
 CType:string;
begin
    CType:=EmptyStr;
    for I := 1 to Length(SqlFieldName) do
    begin
      if (SqlFieldName[I] in (['a'..'z']+['A'..'Z']+['0'..'9'])) or (SqlFieldName[I]='_') then
      CType := CType+SqlFieldName[I];
    end;

    if CType = Tablename then
    CType := CType +'Field';

    //if SQL Field has as stupid name like a C# reserved Keyword [abstract,base,int etc]
	 	if TLazSqlXResources.TableInfoReservedKeywords.IndexOf(CType)>-1 then
   	CType := CType+'_sf';

    Result := CType;
end;

function TTableInfos.GetCSharpTableName(TableName: string): string;
var
 i:Integer;
 CType:string;
begin
    CType:=EmptyStr;
    for I := 1 to Length(TableName) do
    begin
      if (TableName[I] in (['a'..'z']+['A'..'Z']+['0'..'9'])) or (TableName[I]='_') then
      CType := CType+TableName[I];
    end;

    //if SQL Field has as stupid name like a C# reserved Keyword [abstract,base,int etc]
	 	if TLazSqlXResources.TableInfoReservedKeywords.IndexOf(CType)>-1 then
   	CType := CType+'_sf';

    Result := CType;
end;

function TTableInfos.GetCSharpType(SqlType: String): string;
begin
  if  (SqlType='varchar') or
      (SqlType='nvarchar') or
      (SqlType='text') or
      (SqlType='char') or
      (SqlType='nchar') or
      (SqlType='char') or
      (SqlType='ntext') or
      (SqlType='varchar2')
  then
  begin
   Result := 'string';
  end
  else

  if (SqlType='tinyint') or
     (SqlType='int') or
     (SqlType='bigint') or
     (SqlType='smallint') or
     (SqlType='integer')
  then Result:='int'
  else

  if  (SqlType='real') or
      (SqlType='float') or
      (SqlType='decimal') or
      (SqlType='money') or
      (SqlType='smallmoney')
  then Result:='decimal'
  else

  if SqlType='bit'
  then Result:='bool'
  else
  if SqlType='datetime' then
  Result:='DateTime'
  else
  if SqlType='image' then
  Result:='byte[]'
  else
  Result:='string';

end;


function TTableInfos.GetTable(Index: integer): TTableInfo;
begin
  Result := Items[Index];
end;

procedure TTableInfos.GetConstraintInfos(Schema: string; Tablename: string;
  var ConstraintInfos: TImportedKeys);
var
 CI:TImportedKey;
  dsLocalHelper:TZSQLMetadata;
  strType:string;
  qr:TZQuery;
begin


  if ConstraintInfos=nil then
  ConstraintInfos := TImportedKeys.Create;

  if ConstraintInfos=nil then
  Exit;
  try
  dsLocalHelper := TZSQLMetadata.Create(nil);
  dsLocalHelper.Connection := adoCon;


    if FDBinfo.DatabaseType in [dtOracle,dtMySql] then
    begin
      try
        qr := TZQuery.Create(nil);
        qr.DisableControls;
        qr.Connection := adoCon;

        if FDBinfo.DatabaseType=dtOracle then
        begin
          qr.SQL.Add('SELECT ucc1.Constraint_Name, ucc1.Table_Name FKTABLE_NAME,');
          qr.SQL.Add('ucc1.column_name FKCOLUMN_NAME,');
          qr.SQL.Add('ucc2.Owner PKTABLE_SCHEM,');
          qr.SQL.Add('ucc2.Table_Name PKTABLE_NAME,');
          qr.SQL.Add('ucc2.column_name PKCOLUMN_NAME');
          qr.SQL.Add('FROM user_constraints uc,');
          qr.SQL.Add('user_cons_columns ucc1,');
          qr.SQL.Add('user_cons_columns ucc2');
          qr.SQL.Add('WHERE uc.constraint_name = ucc1.constraint_name AND');
          qr.SQL.Add('uc.r_constraint_name = ucc2.constraint_name AND');
          qr.SQL.Add('ucc1.POSITION = ucc2.POSITION AND');
          qr.SQL.Add('uc.constraint_type = ''R'' AND');
          qr.SQL.Add('ucc1.Table_Name=:TABLENAME');
          qr.SQL.Add('ORDER BY ucc1.TABLE_NAME,');
          qr.SQL.Add('uc.constraint_name');
        end;

        if FDBinfo.DatabaseType=dtMySql then
        begin
          qr.SQL.Add('SELECT kcu.CONSTRAINT_NAME,');
          qr.SQL.Add('kcu.TABLE_NAME AS FKTABLE_NAME,');
          qr.SQL.Add('kcu.COLUMN_NAME AS FKCOLUMN_NAME,');
          qr.SQL.Add('kcu.REFERENCED_TABLE_SCHEMA AS PKTABLE_SCHEM,');
          qr.SQL.Add('kcu.REFERENCED_TABLE_NAME AS PKTABLE_NAME,');
          qr.SQL.Add('kcu.REFERENCED_COLUMN_NAME AS PKCOLUMN_NAME');
          qr.SQL.Add('FROM');
          qr.SQL.Add('information_schema.TABLE_CONSTRAINTS tc');
          qr.SQL.Add('INNER JOIN information_schema.KEY_COLUMN_USAGE kcu');
            qr.SQL.Add('ON   tc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME');
          qr.SQL.Add('WHERE');
          qr.SQL.Add('tc.CONSTRAINT_SCHEMA = '''+adoCon.Database+'''');
          qr.SQL.Add('AND kcu.TABLE_NAME = :TABLENAME');
          qr.SQL.Add('AND tc.CONSTRAINT_TYPE = ''FOREIGN KEY''');
        end;

        qr.ParamByName('TABLENAME').Value:=Tablename;
        qr.Open;

        while not qr.EOF do
        begin
           CI := TImportedKey.Create;
            CI.ConstraintName:= qr.FieldByName('CONSTRAINT_NAME').AsString;
            CI.TableName := qr.FieldByName('FKTABLE_NAME').AsString;
            CI.ColumnName := qr.FieldByName('FKCOLUMN_NAME').AsString;

            if FDBinfo.DatabaseType<> dtFirebirdd then
            CI.ForeignSchema := qr.FieldByName('PKTABLE_SCHEM').AsString;

            CI.ForeignTableName := qr.FieldByName('PKTABLE_NAME').AsString;
            CI.ForeignColumnName := qr.FieldByName('PKCOLUMN_NAME').AsString;


            if dsLocalHelper.Active then dsLocalHelper.Close;

            dsLocalHelper.Catalog:=CI.ForeignSchema;
            dsLocalHelper.TableName:=CI.ForeignTableName;
            dsLocalHelper.MetadataType:=mdColumns;
            dsLocalHelper.Open;

            while not dsLocalHelper.EOF do
            begin
             strType := dsLocalHelper.FieldByName('TYPE_NAME').AsString;
             if (strType='varchar') or (strType='nvarchar') then
             begin
               CI.ForeignFirstTextField:=dsLocalHelper.FieldByName('COLUMN_NAME').AsString;
               CI.SelectFields.Add(CI.ForeignFirstTextField);
               Break;
             end;
             dsLocalHelper.Next;
            end;

            dsLocalHelper.Close;

            if Trim(CI.ForeignFirstTextField)=EmptyStr then
              CI.ForeignFirstTextField:= CI.ForeignColumnName;

            ConstraintInfos.Add(CI);
            qr.Next;
        end;

      finally
        qr.Free;
      end;

    end else
    begin

      try

         if dsMetaData.Active then dsMetaData.Close;

         dsMetaData.MetadataType := mdImportedKeys;
         dsMetaData.TableName:=Tablename;
         dsMetaData.Schema:= Schema;
         dsLocalHelper.MetadataType:=mdColumns;

         dsMetaData.Open;
         while not dsMetaData.Eof do
         begin
            CI := TImportedKey.Create;
            CI.ConstraintName:= dsMetaData.FieldByName('FK_NAME').AsString;
            CI.TableName := dsMetaData.FieldByName('FKTABLE_NAME').AsString;
            CI.ColumnName := dsMetaData.FieldByName('FKCOLUMN_NAME').AsString;

            if FDBinfo.DatabaseType<>dtFirebirdd then
            CI.ForeignSchema := dsMetaData.FieldByName('PKTABLE_SCHEM').AsString;

            CI.ForeignTableName := dsMetaData.FieldByName('PKTABLE_NAME').AsString;
            CI.ForeignColumnName := dsMetaData.FieldByName('PKCOLUMN_NAME').AsString;


            if dsLocalHelper.Active then dsLocalHelper.Close;
            dsLocalHelper.TableName:=CI.ForeignTableName;
            dsLocalHelper.Open;

            while not dsLocalHelper.EOF do
            begin
             strType := dsLocalHelper.FieldByName('TYPE_NAME').AsString;
             if (strType='varchar') or (strType='nvarchar') then
             begin
               CI.ForeignFirstTextField:=dsLocalHelper.FieldByName('COLUMN_NAME').AsString;
               CI.SelectFields.Add(CI.ForeignFirstTextField);
               Break;
             end;
             dsLocalHelper.Next;
            end;

            dsLocalHelper.Close;

            if Trim(CI.ForeignFirstTextField)=EmptyStr then
              CI.ForeignFirstTextField:= CI.ForeignColumnName;

            ConstraintInfos.Add(CI);
            dsMetaData.Next;
         end;

      finally
        if dsMetaData.Active then dsMetaData.Close;
      end;

    end;

  finally
    dsLocalHelper.Free;
  end;



end;

procedure TTableInfos.GetExportedKeyInfos(Schema: string; Tablename: string;
  var ExportKeyInfos: TExportedKeyInfos);
var
  ei:TExportedKeyInfo;
begin

  if ExportKeyInfos=nil then
  ExportKeyInfos := TExportedKeyInfos.Create;

  try
      if dsMetaData.Active then dsMetaData.Close;
      dsMetaData.MetadataType := mdCrossReference;
      dsMetaData.TableName:=Tablename;
      dsMetaData.Schema:= Schema;
      dsMetaData.Open;
      while not dsMetaData.EOF do
      begin
        ei := TExportedKeyInfo.Create;
        ei.ColumnName:=dsMetaData.FieldByName('PKCOLUMN_NAME').AsString;

         if FDBinfo.DatabaseType<>dtFirebirdd then
        ei.ForeignSchemName:=dsMetaData.FieldByName('FKTABLE_SCHEM').AsString;

        ei.ForeignTableName:=dsMetaData.FieldByName('FKTABLE_NAME').AsString;
        ei.ColumnName:=dsMetaData.FieldByName('FKCOLUMN_NAME').AsString;
        ExportKeyInfos.Add(ei);
        dsMetaData.Next;
      end;
      dsMetaData.Close;
  finally
  end;

end;

procedure TTableInfos.GetPrimaryKeys(Schema: string; TableName: string;
  var KeyFields: TStringList);
begin
    if KeyFields=nil then
    KeyFields := TStringList.Create;
    try
      if dsMetaData.Active then dsMetaData.Close;
      dsMetaData.Schema:=Schema;
      dsMetaData.TableName:=TableName;
      dsMetaData.MetadataType:=mdPrimaryKeys;

      if FDBinfo.DatabaseType=dtOracle then
      dsMetaData.MetadataType:= mdBestRowIdentifier;

      dsMetaData.Open;
      while not dsMetaData.EOF do
      begin
        KeyFields.Add(dsMetaData.FieldByName('COLUMN_NAME').Value);
        dsMetaData.Next;
      end;
    finally
      if dsMetaData.Active then dsMetaData.Close;
    end;
end;


function TTableInfos.GetSqlType(AdoType: TFieldType): string;
begin
   case AdoType of
     ftAutoInc: Result:='int';
     ftLargeint: Result:='BigInt';
     ftBytes: Result:='Binary';
//     ftString: Result:='Char';
     ftDateTime: Result:='DateTime';
     //ftInteger: Result:='Decimal';
     ftFloat : Result:='Float';
     ftBlob: Result:='Image';
     ftInteger: Result:='Int';
     ftBCD: Result:='Money';
     //ftWideString: Result:='Nchar';
     ftWideMemo: Result:='Ntext';
     //ftInteger: Result:='Numeric';
     ftWideString: Result:='Nvarchar';
     //ftFloat: Result:='Real';
//     ftDateTime: Result:='SmallDateTime';
     ftSmallint: Result:='SmallInt';
//     ftBCD: Result:='SmallMoney';
     ftVariant: Result:='Sql_Variant';
     //ftWideString: Result:='sysname';
     ftMemo: Result:='Text';
//     ftBytes: Result:='Timestamp';
     ftWord: Result:='TinyInt';
     ftGuid: Result:='UniqueIdentifier';
     ftVarBytes: Result:='Varbinary';
     ftString: Result:='Varchar';
     else
       begin
       Result := 'Varchar';
       end;
   end;


   Result := lowercase(Result);
end;


procedure TTableInfos.WriteLog(msg: string);
begin
  if LogStrings<>nil then
  LogStrings.Add(DateTimeToStr(Now)+'          '+Msg);

  if FLogListBox<>nil then
  begin
    FLogListBox.Items.Add(DateTimeToStr(Now)+'          '+Msg);
    FLogListBox.ItemIndex:=FLogListBox.Count-1;
    FApplication.ProcessMessages;
  end;
end;

{$ENDREGION}

{$REGION 'Public'}

procedure TTableInfos.LoadFromTables(Schema:string; Tables: TStrings);
  //CheckDependencies will also do recursive
	procedure CheckDependencies(tablename:string);
  var
  	J:Integer;
    tbl:TTableInfo;
  begin
  	tbl := TableByName(tablename);
    if tbl<>nil then
    if tbl.ImportedKeys.Count>0 then
    begin
       for J := 0 to tbl.ImportedKeys.Count-1 do
         begin
         		if TableByName(tbl.ImportedKeys[J].ForeignTableName)=nil then
            begin
               Add(GetTableInfo(tbl.ImportedKeys[J].ForeignSchema,tbl.ImportedKeys[J].ForeignTableName));
               CheckDependencies(tbl.ImportedKeys[J].ForeignTableName);
            end;
         end;
    end;
  end;
var
  I: Integer;
begin
    WriteLog('Extracting table informations...');

    for I := 0 to Tables.Count - 1 do
    begin
      WriteLog('Extracting information ['+Tables[I]+']');
      Add(GetTableInfo(Schema,Tables[I]));
    end;

    //if table has constraint, if foreign tables not checked for generate,
    //then generate, because they will be needed for filling comboBoxes
    for I := 0 to Count - 1 do
    begin
    	CheckDependencies(Items[I].Tablename);
    end;

    WriteLog('Done');
end;

function TTableInfos.GetTableInfo(Schema: string; Tablename: string
  ): TTableInfo;
var
 lstPKs,lstIdentity:Tstringlist;
 table:TTableInfo;
 field,pkField,idField,nField:TFieldInfo;
 ci:TImportedKeys;
 ei:TExportedKeyInfos;
 ado:TZQuery;
 strSqlCastExpr:string;
 ti:TTriggerInfo;
 ii:TIndexInfo;
begin
   try
     ci := TImportedKeys.Create;
     ei := TExportedKeyInfos.Create;

     GetConstraintInfos(Schema,Tablename,ci);
     GetExportedKeyInfos(Schema,Tablename,ei);

     ado:=TZQuery.Create(nil);
     ado.Connection := adoCon;
     ado.DisableControls;

     lstPKs := TStringList.Create;
     lstIdentity := TStringList.Create;

     if FDBinfo.DatabaseType=dtSQLite then Schema:='';

     GetPrimaryKeys(Schema,Tablename,lstPKs);

     table := TTableInfo.Create;
     table.Tablename := Tablename;
     table.TableCSharpName:=GetCSharpTableName(Tablename);

     if FDBinfo.DatabaseType = dtSQLite then Schema:='';

     table.Schema := Schema;
     table.ImportedKeys := ci;
     table.ExportedKeys := ei;

     if dsMetaData.Active then dsMetaData.Close;

     dsMetaData.Schema:=Schema;
     dsMetaData.TableName:=Tablename;
     dsMetaData.MetadataType := mdColumns;
     dsMetaData.Open;

     //Get FIELD INFOS
     while not dsMetaData.Eof do
     begin

      if FDBinfo.DatabaseType=dtOracle then
      if (table.AllFields.GetIndex(dsMetaData.FieldByName('COLUMN_NAME').AsString)>-1 ) then {sometimes zsqlmeta  returns duplicate fields}
      begin
        dsMetaData.Next;
        Continue;
      end;


      field := TFieldInfo.Create;
      field.FieldName := dsMetaData.FieldByName('COLUMN_NAME').AsString;
      field.FieldType := LowerCase(dsMetaData.FieldByName('TYPE_NAME').AsString);
      if field.FieldType='int identity' then field.FieldType:='int';
      if FDBinfo.DatabaseType in [dtOracle,dtMySql] then
      field.Length := (dsMetaData.FieldByName('COLUMN_SIZE').AsInteger DIV 4 )
      else
      field.Length := dsMetaData.FieldByName('COLUMN_SIZE').AsInteger;

      field.Precision := dsMetaData.FieldByName('DECIMAL_DIGITS').AsInteger;

      ado.Close;
      strSqlCastExpr:='';

      case FDBinfo.DatabaseType of
      dtMsSql:
        begin
          if (field.FieldType='nchar') or (field.FieldType='nvarchar')
          then
            field.Length := dsMetaData.FieldByName('COLUMN_SIZE').AsInteger div 2;

          if (field.FieldType='ntext') then
          strSqlCastExpr:='text';
          if (field.FieldType='nvarchar') or (field.FieldType='xml') then
          strSqlCastExpr:='varchar(max)';
          if (strSqlCastExpr<>'') then
            ado.SQL.Text:='SELECT TOP 1 CAST(['+field.FieldName+'] AS '+strSqlCastExpr+') AS ['+field.FieldName+'] FROM ['+Schema+'].['+Tablename+']'
          else
            ado.SQL.Text:='SELECT TOP 1 ['+field.FieldName+'] FROM ['+Schema+'].['+Tablename+']';
        end;
      dtOracle:ado.SQL.Text:='SELECT "'+field.FieldName+'" FROM "'+Schema+'"."'+Tablename+'" WHERE ROWNUM = 1';
      dtMySql:ado.SQL.Text:='SELECT "'+field.FieldName+'" FROM '+Tablename+' LIMIT 1';
      dtSQLite:ado.SQL.Text:='SELECT ['+field.FieldName+'] FROM '+Tablename+' LIMIT 1';
      dtFirebirdd:ado.SQL.Text:='SELECT FIRST 1 "'+field.FieldName+'" FROM '+Tablename;
      end;

      try
        ado.Open;

        field.DataType := ado.Fields[0].DataType;
        field.Field := ado.Fields[0];
        field.AllowNull := dsMetaData.FieldByName('NULLABLE').AsBoolean;
        field.CSharpType := GetCSharpType(field.FieldType);
        field.CSharpName := GetCSharpName(Tablename,field.FieldName);
        field.Validate := (not field.AllowNull) or (lstPKs.IndexOf(field.FieldName)>-1);

        if ci.ContainsColumn(field.FieldName) then
        begin
          field.ControlType := ctComboBox;
          field.IsReference := True;
        end else
        begin
          field.ControlType := GetControlType(field.FieldType);
        end;

        if lstPKs.IndexOf(field.FieldName)>-1 then
        begin
          field.IsPrimaryKey := True;
          pkField := TFieldInfo.Create;
          pkField.CopyFrom(field);
          table.PrimaryKeys.Add(pkField);
        end else
        begin
         field.IsPrimaryKey := False;
         nField := TFieldInfo.Create;
         nField.CopyFrom(field);
         table.Fields.Add(nField);
        end;

        if (dsMetaData.FieldByName('AUTO_INCREMENT').AsBoolean) or (field.DataType=ftAutoInc) then
        begin
          field.IsIdentity := True;
          idField := TFieldInfo.Create;
          idField.CopyFrom(field);

          table.Identities.Add(idField);
        end;
        field.FieldDbType:=FDBinfo.DatabaseType;
        table.AllFields.Add(field);
      except
        if Assigned(field) then
        field.Free;
      end;
      dsMetaData.Next;
     end;


     //now get trigger infos
     dsMetaData.Close;
     dsMetaData.MetadataType:=mdTriggers;
     dsMetaData.TableName:=Tablename;
     dsMetaData.Open;

     while not dsMetaData.EOF do
     begin
      ti := TTriggerInfo.Create;
      ti.Schem:=dsMetaData.FieldByName('TRIGGER_SCHEM').AsString;
      ti.Cat:= dsMetaData.FieldByName('TRIGGER_CAT').AsString;
      ti.Description:= dsMetaData.FieldByName('TRIGGER_DESCRIPTION').AsString;
      ti.Inactive:= dsMetaData.FieldByName('TRIGGER_INACTIVE').AsBoolean;
      ti.TriggerType:=dsMetaData.FieldByName('TRIGGER_TYPE').AsInteger;
      ti.TriggerSource:=dsMetaData.FieldByName('TRIGGER_SOURCE').AsString;
      ti.Relation:=dsMetaData.FieldByName('TRIGGER_RELATION').AsString;
      dsMetaData.Next;
      table.Triggers.Add(ti);
     end;

     //now get index infos
     dsMetaData.Close;
     dsMetaData.MetadataType:=mdIndexInfo;
     dsMetaData.TableName:=Tablename;
     dsMetaData.Open;

     while not dsMetaData.EOF do
     begin
      ii := TIndexInfo.Create;
      ii.INDEX_Name:=dsMetaData.FieldByName('INDEX_NAME').AsString;
      ii.INDEX_Qualifier:=dsMetaData.FieldByName('INDEX_QUALIFIER').AsString;
      ii.IndexType:= dsMetaData.FieldByName('TYPE').AsInteger;
      ii.ASC_OR_DESC:= dsMetaData.FieldByName('ASC_OR_DESC').AsString;
      ii.FILTER_CONDITION:=dsMetaData.FieldByName('FILTER_CONDITION').AsString;
      ii.Ordinal_Position:=dsMetaData.FieldByName('Ordinal_Position').AsInteger;
      table.Indexes.Add(ii);
      dsMetaData.Next;
     end;



   finally
    if dsMetaData.Active then dsMetaData.Close;
    lstPKs.Free;
    lstIdentity.Free;
    ado.Free;
   end;

   Result := table;

end;

function TTableInfos.TableByName(Tablename: string): TTableInfo;
var
  I:Integer;
begin
	Result := nil;
  for I := 0 to Count-1 do
  begin
    if Items[I].Tablename=Tablename then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TTableInfos.IndexOf(TableName: string): Integer;
var
  i:integer;
begin
  Result := -1;
  for i:=0 to Count-1 do
  begin
   if Items[i].Tablename = TableName then
   begin
     Result := i;
     Break;
   end;
  end;
end;

function TTableInfos.GetCreateSQL(TableIndex:Integer): string;
var
  lst:TStringList;
  j: Integer;
  f:TFieldInfo;
  s:string;
  SqlFromDot: string;
  SqlOpenBr: string;
  SqlCloseBr: string;
  SqlDot: string;
  i: Integer;
  tbl:TTableInfo;
begin

 SqlFromDot:='.';

  case FDBinfo.DatabaseType of
    dtMsSql:
    begin
      SqlOpenBr:='[';
      SqlCloseBr:=']';
      SqlDot:='.';
    end;
    dtOracle:
    begin
      SqlOpenBr:='';
      SqlCloseBr:='';
      SqlDot:='.';
    end;
    dtMySql:
    begin
      SqlOpenBr:='';
      SqlCloseBr:='';
      SqlDot:='';
    end;
    dtSQLite:
     begin
      SqlOpenBr:='';
      SqlCloseBr:='';
      SqlDot:='.';
     end;
  end;
  tbl := Self[TableIndex];

  lst := TStringList.Create;
  try

   lst.Add('CREATE TABLE '+tbl.Tablename +' (');

   for j:=0 to tbl.AllFields.Count -1 do
   begin
      f := tbl.AllFields[j];
      s := SqlOpenBr+f.FieldName+SqlCloseBr+' '+f.FieldType+' ';


      if (f.Length>0) and
      (f.FieldType<>'uniqueidentifier') and
      (f.FieldType<>'image') and
      (f.FieldType <> 'int') and
      (f.FieldType<>'bit') and
      (f.FieldType <> 'ntext') and
      (f.FieldType <> 'text') and
      (f.FieldType <> 'datetime') then
      s:= s+'('+IntToStr(f.Length)+')';

      if f.IsIdentity then
      begin
        s:=s +' IDENTITY(1,1) NOT NULL';
      end else
      if (f.AllowNull) then
      begin
        s:= s+'NULL';
      end else
      s:=s+' NOT NULL';


       if (j<tbl.AllFields.Count-1) then
       s:=s+',';

      lst.Add(s);
   end;


   //Add primary Keys
   if tbl.HasPrimaryKeys then
   begin
     lst.Add('CONSTRAINT '+SqlOpenBr+tbl.Tablename+'_PK'+SqlCloseBr+' PRIMARY KEY CLUSTERED ');
     lst.Add('(');
      for i:=0 to tbl.PrimaryKeys.Count-1 do
      begin
       s:= SqlOpenBr+tbl.PrimaryKeys[i].FieldName+SqlCloseBr;

       if i<tbl.PrimaryKeys.Count-1 then
       s := s+',';

       lst.Add(s);
      end;
     lst.Add(')');
   end;

   lst.Add(')');

   //Add constraints if any
   if tbl.ImportedKeys.Count>0 then
   begin
      lst.Add('');

      for i:=0 to tbl.ImportedKeys.Count-1 do
      begin
       s:= 'ALTER TABLE '+SqlOpenBr+tbl.Tablename+SqlCloseBr+' WITH CHECK ADD CONSTRAINT '+
       SqlOpenBr+tbl.ImportedKeys[I].TableAndColumn+SqlCloseBr +' FOREIGN KEY ('+SqlOpenBr+
       tbl.ImportedKeys[I].ColumnName+SqlCloseBr+')'+
       ' REFERENCES '+SqlOpenBr+tbl.ImportedKeys[I].ForeignTableName+SqlCloseBr+
       ' ('+SqlOpenBr+tbl.ImportedKeys[I].ForeignColumnName+SqlCloseBr+')';
       lst.Add(s);
      end;
   end;

   Result := lst.Text;
  finally
   lst.Free;
  end;
end;

procedure TTableInfos.AddTable(Schema, Tablename: string);
var
  ti:TTableInfo;
begin
  ti := GetTableInfo(Schema,Tablename);
  Add(ti);
end;

{$ENDREGION}

{$ENDREGION}

{$REGION '  TConstraintInfos Implementation  ' }

function TImportedKeys.GetByField(Field: TFieldInfo): TImportedKey;
var
  i:Integer;
begin
   Result := nil;
  for i := 0 to Count -1  do
  begin
    if (Items[i].ColumnName=Field.FieldName) and (Items[I].TableName<>Items[I].ForeignTablename) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TImportedKeys.GetByName(Fieldname: string): TImportedKey;
var
  i:Integer;
begin
   Result := nil;
  for i := 0 to Count -1  do
    if (Items[i].ColumnName=Fieldname) and (Items[I].TableName<>Items[I].ForeignTableName) then
    begin
      Result := Items[i];
      break;
    end;
end;

function TImportedKeys.GetRefInfo(Index: integer): TImportedKey;
begin
  Result := Items[Index];
end;

function TImportedKeys.Add(const Itm: TImportedKey): Integer;
begin
  Itm.Index := Count - 1;
  Result := inherited Add(Itm);
end;

function TImportedKeys.ContainsColumn(Fieldname: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if (Items[I].ColumnName=Fieldname) and (Items[I].TableName<>Items[I].ForeignTableName) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TImportedKeys.ContainsTable(Tablename: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if (Self[I].ForeignTableName=Tablename) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TImportedKeys.GetIndex(Fieldname: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if (Items[I].ColumnName=Fieldname) and (Items[I].TableName<>Items[I].ForeignTableName) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TImportedKey.SetTablename(AValue: string);

begin
 if FTablename=AValue then Exit;
 FTablename:=AValue;
 FTableAlias:=TAsStringUtils.GetFriendlyAlias(FTablename);
end;

procedure TImportedKey.SetForiegnTablename(AValue: string);
begin
 if FForeignTablename=AValue then Exit;
 FForeignTablename:=AValue;
 FForeignTableAlias:= TAsStringUtils.GetFriendlyAlias(FForeignTablename);
end;


function TImportedKey.TableAndColumn: string;
begin
 Result:= TableName+ColumnName;
end;

constructor TImportedKey.Create;
begin
 SelectFields := TStringList.Create;
end;

destructor TImportedKey.Destroy;
begin
 SelectFields.Free;
end;

function TImportedKey.GetCompatibleColumnName(dbType: TDatabaseType): string;
var
 o,c:string;
begin
 case dbType of
  dtMsSql:
   begin
     o:='[';
     c:=']';
   end;
  dtOracle:
      begin
     o:='"';
     c:='"';
   end;
  dtMySql:
       begin
     o:='';
     c:='';
   end;
  dtSQLite:
     begin
     o:='[';
     c:=']';
   end;
  dtFirebirdd:
     begin
     o:='"';
     c:='"';
     end;
 end;
 Result:=o+ColumnName+c;
end;

function TImportedKey.GetCompatibleForeignColumnName(dbType: TDatabaseType
 ): string;
var
 o,c:string;
begin
 case dbType of
  dtMsSql:
   begin
     o:='[';
     c:=']';
   end;
  dtOracle:
      begin
     o:='"';
     c:='"';
   end;
  dtMySql:
       begin
     o:='';
     c:='';
   end;
  dtSQLite:
     begin
     o:='[';
     c:=']';
   end;
  dtFirebirdd:
     begin
     o:='"';
     c:='"';
     end;
 end;
 Result:=o+ForeignColumnName+c;
end;

{$ENDREGION}

end.

