{*******************************************************************
AUTHOR : Flakron Shkodra
*******************************************************************
version 1.1, moddate:23.08.2011
*******************************************************************
Versopm 2 moddate 27.10.2014 (removed FGL usage, used collections instead)
*******************************************************************
}

unit TableInfo;

{$mode objfpc}{$H+}

interface

uses SysUtils,Classes,DbType,ZConnection,ZDataset,ZDbcIntfs,ZSqlMetadata,
DB,typinfo, Forms,StdCtrls,AsStringUtils,LazSqlXResources,LResources;


type


 TControlType = (ctUnknown,ctTextBox,ctComboBox,ctDateTimePicker,ctCheckBox,ctNumeric);

 {this enum not used yet}
 TControlNameStyle = (cnsShortPrefixName,cnsNameLongPrefix);

 TWinFormType = (wfTransaction,wfDefinition);

 { TTriggerInfo }

 TTriggerInfo = class(TCollectionItem)
 private
  FCat: string;
  FDescription: string;
  FInactive: Boolean;
  FName: string;
  FRelation: string;
  FSchem: string;
  FTriggerSource: string;
  FTriggerType: Integer;
  procedure SetCat(AValue: string);
  procedure SetDescription(AValue: string);
  procedure SetInactive(AValue: Boolean);
  procedure SetName(AValue: string);
  procedure SetRelation(AValue: string);
  procedure SetSchem(AValue: string);
  procedure SetTriggerSource(AValue: string);
  procedure SetTriggerType(AValue: Integer);
 public
  procedure Assign(Source: TPersistent); override;
 published
  property Cat:string read FCat write SetCat;
  property Schem:string read FSchem write SetSchem;
  property Name:string read FName write SetName;
  property Relation:string read FRelation write SetRelation;
  property TriggerType:Integer read FTriggerType write SetTriggerType;
  property Inactive:Boolean read FInactive write SetInactive;
  property TriggerSource:string read FTriggerSource write SetTriggerSource;
  property Description:string read FDescription write SetDescription;
 end;

 { TTriggerInfos }

 TTriggerInfos = class(TOwnedCollection)
 private
  function GetItems(Index: Integer): TTriggerInfo;
  procedure SetItems(Index: Integer; AValue: TTriggerInfo);
 public
  constructor Create(AOwner:TPersistent);
  function Add:TTriggerInfo;
  property Items[Index:Integer]:TTriggerInfo read GetItems write SetItems;default;
 end;

 { TIndexInfo }

 TIndexInfo = class(TCollectionItem)
 private
  FASC_OR_DESC: string;
  FColumn_Name: string;
  FFILTER_CONDITION: string;
  FIndexType: Integer;
  FINDEX_Name: string;
  FINDEX_Qualifier: string;
  FIs_Unique: Boolean;
  FOrdinal_Position: Integer;
  FOwner:TCollection;
  procedure SetASC_OR_DESC(AValue: string);
  procedure SetColumn_Name(AValue: string);
  procedure SetFILTER_CONDITION(AValue: string);
  procedure SetIndexType(AValue: Integer);
  procedure SetINDEX_Name(AValue: string);
  procedure SetINDEX_Qualifier(AValue: string);
  procedure SetIs_Unique(AValue: Boolean);
  procedure SetOrdinal_Position(AValue: Integer);
 protected
    function GetDisplayName: string; override;
 public
    procedure Assign(Source: TPersistent); override;
 published
  property INDEX_Qualifier:string read FINDEX_Qualifier write SetINDEX_Qualifier;
  property INDEX_Name:string read FINDEX_Name write SetINDEX_Name;
  property IndexType:Integer read FIndexType write SetIndexType;
  property Ordinal_Position:Integer read FOrdinal_Position write SetOrdinal_Position;
  property Column_Name:string read FColumn_Name write SetColumn_Name;
  property ASC_OR_DESC:string read FASC_OR_DESC write SetASC_OR_DESC;
  property FILTER_CONDITION:string read FFILTER_CONDITION write SetFILTER_CONDITION;
  property Is_Unique:Boolean read FIs_Unique write SetIs_Unique;
 end;

 { TIndexInfos }

 TIndexInfos = class(TOwnedCollection)
 private
  function GetItems(Index: Integer): TIndexInfo;
  procedure SetItems(Index: Integer; AValue: TIndexInfo);
 public
  constructor Create(AOwner:TPersistent);
  function Add:TIndexInfo;
  function GetByName(IndexName:string):TIndexInfo;
  property Items[Index:Integer]:TIndexInfo read GetItems write SetItems;default;
 end;

 { TFieldInfo }
 TFieldInfos = class;

 TFieldInfo = class(TCollectionItem)
 private
  FAllowNull: Boolean;
  FControlType: TControlType;
  FCSharpName: string;
  FCSharpType: string;
  FDataType: TFieldType;
  FFieldRef: TField;
  FFieldDbType: TDatabaseType;
  FFieldName: string;
  FFieldType: string;
  FIsIdentity: Boolean;
  FIsPrimaryKey: Boolean;
  FIsReference: Boolean;
  FLength: Integer;
  FPrecision: Integer;
  FReserved: string;
  FValidate: Boolean;
  procedure SetAllowNull(AValue: Boolean);
  procedure SetControlType(AValue: TControlType);
  procedure SetCSharpName(AValue: string);
  procedure SetCSharpType(AValue: string);
  procedure SetDataType(AValue: TFieldType);
  procedure SetFCollection(AValue: TFieldInfos);
  procedure SetField(AValue: TField);
  procedure SetFieldDbType(AValue: TDatabaseType);
  procedure SetFieldName(AValue: string);
  procedure SetFieldType(AValue: string);
  procedure SetIsIdentity(AValue: Boolean);
  procedure SetIsPrimaryKey(AValue: Boolean);
  procedure SetIsReference(AValue: Boolean);
  procedure SetLength(AValue: Integer);
  procedure SetPrecision(AValue: Integer);
  procedure SetReserved(AValue: string);
  procedure SetValidate(AValue: Boolean);
 protected
  function GetDisplayName: string; override;
 public
  procedure CopyFrom(FieldInfo:TFieldInfo);
  function WebControlName:string;
  function WebControlNameWithProp:string;
  function IsNumeric:Boolean;
  function WebControlPrefix:string;
  function WinControlPrefix:string;
  function WinControlName:string;
  function GetFieldTypeAs(db:TDatabaseType):String;
  function GetCompatibleFieldName(dbType: TDatabaseType): string;
  procedure Assign(Source: TPersistent); override;
 published
  property FieldName:string read FFieldName write SetFieldName;
  property FieldType:string read FFieldType write SetFieldType;
  property FieldRef:TField read FFieldRef write SetField;
  property CSharpType:string read FCSharpType write SetCSharpType;
  property CSharpName:string read FCSharpName write SetCSharpName;
  property DataType:TFieldType read FDataType write SetDataType;
  property IsPrimaryKey:Boolean read FIsPrimaryKey write SetIsPrimaryKey;
  property IsIdentity:Boolean read FIsIdentity write SetIsIdentity;
  property AllowNull:Boolean read FAllowNull write SetAllowNull;
  property Length:Integer read FLength write SetLength;
  property Precision:Integer read FPrecision write SetPrecision;
  property ControlType:TControlType read FControlType write SetControlType;
  property Validate:Boolean read FValidate write SetValidate;//added for DevBooster Form Generator Compat
  property IsReference:Boolean read FIsReference write SetIsReference;
  property Reserved:string read FReserved write SetReserved;
  property FieldDbType:TDatabaseType read FFieldDbType write SetFieldDbType;
 end;

 { TFieldInfos }

 TFieldInfos = class(TOwnedCollection)
 private
   function GetFieldInfo(Index: Integer): TFieldInfo;
 public
    constructor Create(aOwner:TPersistent);
    function Add:TFieldInfo;
    function GetIndex(FieldName:string):Integer;
    function ToStringList:TStringList;
    property Items[Index:Integer]:TFieldInfo read GetFieldInfo;default;
 end;

 { TImportedKeyInfo }

 TImportedKeyInfos = class;

 TImportedKeyInfo = class(TCollectionItem)
 private
    FColumnName: string;
    FConstraintName: string;
    FForeignColumnName: string;
    FForeignFirstTextField: string;
    FForeignSchema: string;
    FSelectFields: TStringList;
    FTablename:string;
    FTableAlias:string;
    FForeignTableAlias:string;
    FForeignTablename:string;
    procedure SetColumnName(AValue: string);
    procedure SetConstraintName(AValue: string);
    procedure SetForeignColumnName(AValue: string);
    procedure SetForeignFirstTextField(AValue: string);
    procedure SetForeignSchema(AValue: string);
    procedure SetForiegnTablename(AValue: string);
    procedure SetSelectFields(AValue: TStringList);
    procedure SetTablename(AValue: string);
 protected
    function GetDisplayName: string; override;
 public
   function TableAndColumn:string;
   constructor Create(aCollection:TCollection);override;
   destructor Destroy;override;
   function GetCompatibleColumnName(dbType: TDatabaseType): string;
   function GetCompatibleForeignColumnName(dbType: TDatabaseType): string;
   procedure Assign(Source: TPersistent); override;
 published
   property ConstraintName:string read FConstraintName write SetConstraintName;
   property ColumnName:string read FColumnName write SetColumnName;
   property SelectFields: TStringList read FSelectFields write SetSelectFields;
   property ForeignSchema:string read FForeignSchema write SetForeignSchema;
   property ForeignColumnName:string read FForeignColumnName write SetForeignColumnName;
   property ForeignFirstTextField:string read FForeignFirstTextField write SetForeignFirstTextField;
   property Tablename:string read FTablename write SetTablename;
   property TableAlias:string read FTableAlias write FTableAlias;
   property ForeignTableName:string read FForeignTablename write SetForiegnTablename;
   property ForeignTableAlias:string read FForeignTableAlias write FForeignTableAlias;
 end;

  { TImportedKeyInfos }

 TImportedKeyInfos = class(TOwnedCollection)
 private
   function GetRefInfo(Index: integer): TImportedKeyInfo;
   procedure SetRefInfo(Index: integer; AValue: TImportedKeyInfo);
 public
    constructor Create(AOwner:TPersistent);
    function Add:TImportedKeyInfo;
    function ContainsColumn(Fieldname:string):Boolean;
    function ContainsTable(Tablename:string):Boolean;
    function GetIndex(Fieldname:string):Integer;
    function GetByName(Fieldname:string):TImportedKeyInfo;
    function GetByField(Field:TFieldInfo):TImportedKeyInfo;
    property Items[Index:integer]:TImportedKeyInfo read GetRefInfo write SetRefInfo; default;
 end;

 { TExportedKeyInfo }

 TExportedKeyInfo = class(TCollectionItem)
 private
  FColumnName: string;
  FForeignColumnName: string;
  FForeignSchemName: string;
   FTable: string;
   FTableAlias:string;
   procedure SetColumnName(AValue: string);
   procedure SetForeignColumnName(AValue: string);
   procedure SetForeignSchemName(AValue: string);
   procedure SetTablename(AValue: string);
 protected
   function GetDisplayName: string; override;
 public
   procedure Assign(Source: TPersistent); override;
 published
   property ColumnName:string read FColumnName write SetColumnName;
   property ForeignSchemName:string read FForeignSchemName write SetForeignSchemName;
   property ForeignColumnName:string read FForeignColumnName write SetForeignColumnName;
   property ForeignTableName:string read FTable write SetTablename;
   property ForeignTableAlias:string read FTableAlias write FTableAlias;
   property Table:string read FTable write FTable;
   property TableAlias:string read FTableAlias write FTableAlias;
 end;


 { TExportedKeyInfos }

 TExportedKeyInfos = class(TOwnedCollection)
 private
  function GetItems(Index: Integer): TExportedKeyInfo;
  procedure SetItems(Index: Integer; AValue: TExportedKeyInfo);
 public
  constructor Create(AOwner:TPersistent);
  function Add:TExportedKeyInfo;
  property Items[Index:Integer]:TExportedKeyInfo read GetItems write SetItems;default;
 end;




 { TTableInfo }

 TTableInfos = class;

 TTableInfo = class(TCollectionItem)
 strict private
  FExportedKeys: TExportedKeyInfos;
  FAllFields:TFieldInfos;//to contain all fields
  FIndexes: TIndexInfos;
  FPrimaryKeys:TFieldInfos;//to contain only Pks
  FFields:TFieldInfos; //to contain only fields that are not PKs
  FIdentities:TFieldInfos;
  FTableAlias: string;
  FTablename:string;
  FSchema:string;
  FImportedKeys:TImportedKeyInfos;
  FHasPrimaryKey:Boolean;
  FTriggerInfos: TTriggerInfos;
  FFormType:TWinFormType;
  FTableNameAsControlName:string;
  function GetHasPrimaryKeys: Boolean;
  procedure SetTableName(AValue: string);
 protected
  function GetDisplayName: string; override;
 public

  constructor Create(aCollection:TCollection);override;
  destructor Destroy;override;
  procedure Assign(Source: TPersistent); override;
  function FieldByName(Fieldname:string):TFieldInfo;
  function PrimaryKeysAsDelText:string;
  {Compares this instance to given tableInfo returns SQL valid code for altering if they are different}
  function Compare(otherTable:TTableInfo;DbType:TDatabaseType):string;
 published
  property Tablename:string read FTablename write SetTableName;
  property TableNameAsControlName:string read FTableNameAsControlName write FTableNameAsControlName;
  property Schema:string read FSchema write FSchema;
  {All fields}
  property AllFields:TFieldInfos read FAllFields write FAllFields;
  {PrimaryKey fields only}
  property PrimaryKeys:TFieldInfos read FPrimaryKeys write FPrimaryKeys;
  {Fields that are not primary keys or identities}
  property Fields:TFieldInfos read FFields write FFields;
  {Fields that are identities only}
  property Identities:TFieldInfos read FIdentities write FIdentities;
  {References to other tables}
  property ImportedKeys:TImportedKeyInfos read FImportedKeys write FImportedKeys;
  {Shows if this tables PK is reference of another table}
  property ExportedKeys:TExportedKeyInfos read FExportedKeys write FExportedKeys;

  property Triggers:TTriggerInfos read FTriggerInfos write FTriggerInfos;
  property Indexes:TIndexInfos read FIndexes write FIndexes;

  property HasPrimaryKeys:Boolean read GetHasPrimaryKeys;
  property FormType:TWinFormType read FFormType write FFormType;

  property TableAlias:string read FTableAlias write FTableAlias;

 end;

 { TTableInfos }

 TTableInfos = class(TOwnedCollection)
 private
    adoCon:TZConnection;
    dsMetaData:TZSQLMetadata;
    FLogStrings:TStrings;
    FApplication:TApplication;

    FLogListBox:TListBox;
    FDBinfo:TDbConnectionInfo;
    FAutoConnect:Boolean;
    procedure GetImportedKeyInfos(Schema:string;Tablename:string;var ConstraintInfos:TImportedKeyInfos);
    procedure GetExportedKeyInfos(Schema:string;Tablename:string;var ExportKeyInfos:TExportedKeyInfos);
    function GetItem(Index: Integer): TTableInfo;
    procedure GetPrimaryKeys(Schema:string;TableName:string;var KeyFields:TStringList);
    function GetSqlType(AdoType:TFieldType):string;
    function GetCSharpType(SqlType:String):string;
    function GetFieldType(SqlType:String):TFieldType;

    function GetCSharpName(Tablename,SqlFieldName:string):string;
    function GetTableNameAsControl(TableName:string):string;
    function GetControlType(SqlType:string):TControlType;
    procedure SetDBInfo(AValue: TDbConnectionInfo);
    procedure SetItem(Index: Integer; AValue: TTableInfo);
    procedure WriteLog(msg:string);
    function GetTable(Index: integer): TTableInfo;

 public
    constructor Create(aOwner: TComponent; DbInfo: TDbConnectionInfo; AutoConnect:Boolean=true);
    destructor Destroy;override;
    procedure LoadFromTables(Schema:string; Tables:TStrings);
    function Add:TTableInfo;overload;
    function Add(Schema:string; Tablename:string):TTableInfo;overload;
    function TableByName(Tablename:string):TTableInfo;
    function IndexOf(TableName:string):Integer;
    function GetCreateSQL(TableIndex:Integer):string;
    procedure Reconnect;
    procedure AddTable(Schema,Tablename:string);
    property LogStrings:TStrings read FLogStrings write FLogStrings;
    property LogList:TListBox read FLogListBox write FLogListBox;
    property Items[Index:Integer]:TTableInfo read GetItem write SetItem;default;
    property DbInfo:TDbConnectionInfo read FDBinfo write SetDBInfo;
    procedure InitializeConnection;{public for use after ReadingComponentFromStream}
 end;

 { TDbTablesInfo }

 TDbTablesInfo = class(TComponent)
 private
    FItems:TTableInfos;
    FName: string;
    FDBInfo:TDbConnectionInfo;
    FShouldFreeDbInfo : Boolean;
    procedure SetDBInfo(AValue: TDbConnectionInfo);
    procedure SetName(AValue: string);
    procedure OnFindClass(Reader: TReader; const AClassName: string; var ComponentClass: TComponentClass);
 public
    constructor Create(aDbInfo:TDbConnectionInfo; AutoConnect:Boolean=True);overload;
    {this constructor could be used when the object is going to be loaded from file}
    constructor Create;overload;
    destructor Destroy; override;
    procedure Reconnect;
    procedure Clear;
    procedure LoadFromFile(Filename:string);
    procedure SaveToFile(Filename:string);
 published
    property Name:string read FName write SetName;
    property TableInfos:TTableInfos read FItems write FItems;
    property DbInfo:TDbConnectionInfo read FDBInfo write SetDBInfo;
 end;



implementation

{ TTriggerInfo }

procedure TTriggerInfo.SetCat(AValue: string);
begin
 if FCat=AValue then Exit;
 FCat:=AValue;
end;

procedure TTriggerInfo.SetDescription(AValue: string);
begin
 if FDescription=AValue then Exit;
 FDescription:=AValue;
end;

procedure TTriggerInfo.SetInactive(AValue: Boolean);
begin
 if FInactive=AValue then Exit;
 FInactive:=AValue;
end;

procedure TTriggerInfo.SetName(AValue: string);
begin
 if FName=AValue then Exit;
 FName:=AValue;
end;

procedure TTriggerInfo.SetRelation(AValue: string);
begin
 if FRelation=AValue then Exit;
 FRelation:=AValue;
end;

procedure TTriggerInfo.SetSchem(AValue: string);
begin
 if FSchem=AValue then Exit;
 FSchem:=AValue;
end;

procedure TTriggerInfo.SetTriggerSource(AValue: string);
begin
 if FTriggerSource=AValue then Exit;
 FTriggerSource:=AValue;
end;

procedure TTriggerInfo.SetTriggerType(AValue: Integer);
begin
 if FTriggerType=AValue then Exit;
 FTriggerType:=AValue;
end;

procedure TTriggerInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;

 if Source is TTriggerInfo then
 begin
   FCat:= TTriggerInfo(Source).Cat;
   FDescription:= TTriggerInfo(Source).Description;
   FInactive:= TTriggerInfo(Source).Inactive;
   FName:= TTriggerInfo(Source).Name;
   FRelation:=TTriggerInfo(Source).Relation;
   FSchem:=TTriggerInfo(Source).Schem;
   FTriggerSource:=TTriggerInfo(Source).TriggerSource;
   FTriggerType:= TTriggerInfo(Source).TriggerType;
 end else
 inherited Assign(Source);
end;

{ TTriggerInfos }

function TTriggerInfos.GetItems(Index: Integer): TTriggerInfo;
begin
 Result := TTriggerInfo(inherited Items[Index]);
end;

procedure TTriggerInfos.SetItems(Index: Integer; AValue: TTriggerInfo);
begin
 Items[Index] := AValue;
end;

constructor TTriggerInfos.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner,TTriggerInfo);
end;

function TTriggerInfos.Add: TTriggerInfo;
begin
 Result := TTriggerInfo(inherited Add);
end;

{ TIndexInfo }

procedure TIndexInfo.SetASC_OR_DESC(AValue: string);
begin
 if FASC_OR_DESC=AValue then Exit;
 FASC_OR_DESC:=AValue;
end;

procedure TIndexInfo.SetColumn_Name(AValue: string);
begin
 if FColumn_Name=AValue then Exit;
 FColumn_Name:=AValue;
end;

procedure TIndexInfo.SetFILTER_CONDITION(AValue: string);
begin
 if FFILTER_CONDITION=AValue then Exit;
 FFILTER_CONDITION:=AValue;
end;

procedure TIndexInfo.SetIndexType(AValue: Integer);
begin
 if FIndexType=AValue then Exit;
 FIndexType:=AValue;
end;

procedure TIndexInfo.SetINDEX_Name(AValue: string);
begin
 if FINDEX_Name=AValue then Exit;
 FINDEX_Name:=AValue;
end;

procedure TIndexInfo.SetINDEX_Qualifier(AValue: string);
begin
 if FINDEX_Qualifier=AValue then Exit;
 FINDEX_Qualifier:=AValue;
end;

procedure TIndexInfo.SetIs_Unique(AValue: Boolean);
begin
 if FIs_Unique=AValue then Exit;
 FIs_Unique:=AValue;
end;

procedure TIndexInfo.SetOrdinal_Position(AValue: Integer);
begin
 if FOrdinal_Position=AValue then Exit;
 FOrdinal_Position:=AValue;
end;

function TIndexInfo.GetDisplayName: string;
begin
 Result:=FINDEX_Name;
end;

procedure TIndexInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;

 if Source is TIndexInfo then
 begin
   FASC_OR_DESC:= TIndexInfo(Source).ASC_OR_DESC;
   FColumn_Name:=TIndexInfo(Source).Column_Name;
   FFILTER_CONDITION:=TIndexInfo(Source).FILTER_CONDITION;
   FIndexType:=TIndexInfo(Source).IndexType;
   FINDEX_Name:=TIndexInfo(Source).INDEX_Name;
   FINDEX_Qualifier:=TIndexInfo(Source).INDEX_Qualifier;
   FIs_Unique:=TIndexInfo(Source).Is_Unique;
   FOrdinal_Position:=TIndexInfo(Source).Ordinal_Position;
 end else
 inherited Assign(Source);
end;

{ TDbTablesInfo }

procedure TDbTablesInfo.SetName(AValue: string);
begin
 if FName=AValue then Exit;
 FName:=AValue;
end;

procedure TDbTablesInfo.SetDBInfo(AValue: TDbConnectionInfo);
begin
 FDBInfo := AValue;
 FItems.DbInfo:=AValue;
 FShouldFreeDbInfo:=False;
end;


procedure TDbTablesInfo.OnFindClass(Reader: TReader; const AClassName: string;
 var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName,'TDbSchemata')=0 then
    ComponentClass:=TDbTablesInfo;
end;

constructor TDbTablesInfo.Create(aDbInfo: TDbConnectionInfo; AutoConnect: Boolean
 );
begin
 inherited Create(nil);
 FDBInfo := aDbInfo;
 if FDBInfo=nil then
 begin
   FDBInfo := TDbConnectionInfo.Create;
   FShouldFreeDbInfo:=True;
 end;
 FItems := TTableInfos.Create(Self,aDbInfo,AutoConnect);
end;

constructor TDbTablesInfo.Create;
begin
  inherited Create(nil);
  FDBInfo := TDbConnectionInfo.Create;
  FShouldFreeDbInfo:=True;
  FItems := TTableInfos.Create(Self,FDBInfo,False);
end;

destructor TDbTablesInfo.Destroy;
begin
 FItems.Destroy;

  if FShouldFreeDbInfo then
   FDBInfo.Destroy;

 inherited Destroy;
end;

procedure TDbTablesInfo.Reconnect;
begin
  FItems.Reconnect;
end;

procedure TDbTablesInfo.Clear;
begin
 FItems.Clear;
end;

procedure TDbTablesInfo.LoadFromFile(Filename: string);
var
  mem:TMemoryStream;
begin
  try
    mem := TMemoryStream.Create;
    mem.LoadFromFile(Filename);
    ReadComponentFromBinaryStream(mem,TComponent(Self),@OnFindClass);
    if FItems<>nil then
    FItems.InitializeConnection;
  finally
    mem.Free;
  end;
end;

procedure TDbTablesInfo.SaveToFile(Filename: string);
var
  mem:TMemoryStream;
  ti :TTableInfo;
begin
  try
    mem := TMemoryStream.Create;
    WriteComponentAsBinaryToStream(mem,TComponent(Self));
    mem.SaveToFile(Filename);
  finally
    mem.Free;
  end;
end;

{ TExportedKeyInfo }

procedure TExportedKeyInfo.SetTablename(AValue: string);
begin
 if FTable=AValue then Exit;
 FTable:=AValue;

 FTableAlias:=TAsStringUtils.GetFriendlyAlias(FTable);

end;

procedure TExportedKeyInfo.SetColumnName(AValue: string);
begin
 if FColumnName=AValue then Exit;
 FColumnName:=AValue;
end;

procedure TExportedKeyInfo.SetForeignColumnName(AValue: string);
begin
 if FForeignColumnName=AValue then Exit;
 FForeignColumnName:=AValue;
end;

procedure TExportedKeyInfo.SetForeignSchemName(AValue: string);
begin
 if FForeignSchemName=AValue then Exit;
 FForeignSchemName:=AValue;
end;

function TExportedKeyInfo.GetDisplayName: string;
begin
 Result:=FColumnName;
end;

procedure TExportedKeyInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;
 if Source is TExportedKeyInfo then
 begin
   FColumnName:= TExportedKeyInfo(Source).ColumnName;
   FForeignColumnName:=TExportedKeyInfo(Source).ForeignColumnName;
   FForeignSchemName:=TExportedKeyInfo(Source).ForeignSchemName;
   FTable:=TExportedKeyInfo(Source).Table;
   FTableAlias:=TExportedKeyInfo(Source).TableAlias;
 end else
 inherited Assign(Source);
end;



{ TIndexInfos }

function TIndexInfos.GetItems(Index: Integer): TIndexInfo;
begin
 Result := TIndexInfo(inherited Items[index]);
end;

procedure TIndexInfos.SetItems(Index: Integer; AValue: TIndexInfo);
begin
  Items[Index] := AValue;
end;

constructor TIndexInfos.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner,TIndexInfo);
end;

function TIndexInfos.Add: TIndexInfo;
begin
  Result := TIndexInfo(inherited Add);
end;

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



{ TExportedKeyInfos }


function TExportedKeyInfos.GetItems(Index: Integer): TExportedKeyInfo;
begin
 Result := TExportedKeyInfo( inherited Items[Index] );
end;

procedure TExportedKeyInfos.SetItems(Index: Integer; AValue: TExportedKeyInfo);
begin
  inherited Items[Index] := AValue;
end;

constructor TExportedKeyInfos.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner,TExportedKeyInfo);
end;

function TExportedKeyInfos.Add: TExportedKeyInfo;
begin
 Result := TExportedKeyInfo(inherited Add);
end;

{ TFieldInfos }

function TFieldInfos.GetFieldInfo(Index: Integer): TFieldInfo;
begin
  Result := TFieldInfo(inherited  Items[Index]);
end;

constructor TFieldInfos.Create(aOwner: TPersistent);
begin
 inherited Create(aOwner,TFieldInfo);
end;

function TFieldInfos.Add: TFieldInfo;
begin
 Result := TFieldInfo(inherited Add);
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

procedure TFieldInfo.SetAllowNull(AValue: Boolean);
begin
 if FAllowNull=AValue then Exit;
 FAllowNull:=AValue;
end;

procedure TFieldInfo.SetControlType(AValue: TControlType);
begin
 if FControlType=AValue then Exit;
 FControlType:=AValue;
end;

procedure TFieldInfo.SetCSharpName(AValue: string);
begin
 if FCSharpName=AValue then Exit;
 FCSharpName:=AValue;
end;

procedure TFieldInfo.SetCSharpType(AValue: string);
begin
 if FCSharpType=AValue then Exit;
 FCSharpType:=AValue;
end;

procedure TFieldInfo.SetDataType(AValue: TFieldType);
begin
 if FDataType=AValue then Exit;
 FDataType:=AValue;
end;

procedure TFieldInfo.SetFCollection(AValue: TFieldInfos);
begin

end;

procedure TFieldInfo.SetField(AValue: TField);
begin
 if FFieldRef=AValue then Exit;
 FFieldRef:=AValue;
end;

procedure TFieldInfo.SetFieldDbType(AValue: TDatabaseType);
begin
 if FFieldDbType=AValue then Exit;
 FFieldDbType:=AValue;
end;

procedure TFieldInfo.SetFieldName(AValue: string);
begin
 if FFieldName=AValue then Exit;
 FFieldName:=AValue;
end;

procedure TFieldInfo.SetFieldType(AValue: string);
begin
 if FFieldType=AValue then Exit;
 FFieldType:=AValue;
end;

procedure TFieldInfo.SetIsIdentity(AValue: Boolean);
begin
 if FIsIdentity=AValue then Exit;
 FIsIdentity:=AValue;
end;

procedure TFieldInfo.SetIsPrimaryKey(AValue: Boolean);
begin
 if FIsPrimaryKey=AValue then Exit;
 FIsPrimaryKey:=AValue;
end;

procedure TFieldInfo.SetIsReference(AValue: Boolean);
begin
 if FIsReference=AValue then Exit;
 FIsReference:=AValue;
end;

procedure TFieldInfo.SetLength(AValue: Integer);
begin
 if FLength=AValue then Exit;
 FLength:=AValue;
end;

procedure TFieldInfo.SetPrecision(AValue: Integer);
begin
 if FPrecision=AValue then Exit;
 FPrecision:=AValue;
end;

procedure TFieldInfo.SetReserved(AValue: string);
begin
 if FReserved=AValue then Exit;
 FReserved:=AValue;
end;

procedure TFieldInfo.SetValidate(AValue: Boolean);
begin
 if FValidate=AValue then Exit;
 FValidate:=AValue;
end;

function TFieldInfo.GetDisplayName: string;
begin
  Result:=FFieldName
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
           if TDbUtils.IsBlobGUID(FFieldRef) then
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
           if TDbUtils.IsBlobGUID(FFieldRef) then
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
           if TDbUtils.IsBlobGUID(FFieldRef) then
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
           if TDbUtils.IsBlobGUID(FFieldRef) then
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
           if TDbUtils.IsBlobGUID(FFieldRef) then
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

procedure TFieldInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;

  if Source is TFieldInfo then
  begin
    FAllowNull:=TFieldInfo(Source).AllowNull;
    FControlType:=TFieldInfo(Source).ControlType;
    FCSharpName:=TFieldInfo(Source).CSharpName;
    FDataType:=TFieldInfo(Source).DataType;
    FFieldRef := TFieldInfo(Source).FieldRef;
    FFieldDbType:= TFieldInfo(Source).FieldDbType;
    FFieldName:=TFieldInfo(Source).FieldName;
    FFieldType:=TFieldInfo(Source).FieldType;
    FIsIdentity:=TFieldInfo(Source).IsIdentity;
    FIsPrimaryKey:=TFieldInfo(Source).IsPrimaryKey;
    FIsReference:=TFieldInfo(Source).IsReference;
    FLength:=TFieldInfo(Source).Length;
    FPrecision:=TFieldInfo(Source).Precision;
    FReserved:=TFieldInfo(Source).Reserved;
    FValidate:=TFieldInfo(Source).Validate;
  end else
 inherited Assign(Source);
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
   FieldRef := FieldInfo.FieldRef;
   FieldDbType:= FieldInfo.FieldDbType;
  end;
end;

{$ENDREGION}

{$REGION '   TTableInfo Implementation   ' }

{$REGION 'Constructor/Destructor'}

function TTableInfo.GetHasPrimaryKeys: Boolean;
begin
 Result := False;
 if FPrimaryKeys <> nil then
  Result := FPrimaryKeys.Count>0;
end;


procedure TTableInfo.SetTableName(AValue: string);
begin
 if FTablename=AValue then Exit;
 FTablename:=AValue;
 FTableAlias:=TAsStringUtils.GetFriendlyAlias(FTablename);
end;

function TTableInfo.GetDisplayName: string;
begin
  Result:=FTablename
end;


constructor TTableInfo.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);

  FFields := TFieldInfos.Create(FFields);
  FAllFields := TFieldInfos.Create(FAllFields);
  FIdentities := TFieldInfos.Create(FIdentities);
  FPrimaryKeys := TFieldInfos.Create(FPrimaryKeys);

  FExportedKeys := TExportedKeyInfos.Create(FExportedKeys);
  FImportedKeys := TImportedKeyInfos.Create(FImportedKeys);

  FTriggerInfos := TTriggerInfos.Create(FTriggerInfos);
  FIndexes :=  TIndexInfos.Create(FIndexes);

end;

destructor TTableInfo.Destroy;
begin
  FAllFields.Free;
  FFields.Free;
  FPrimaryKeys.Free;
  FIdentities.Free;
  FImportedKeys.Free;
  FExportedKeys.Free;
  FTriggerInfos.Free;
  FIndexes.Free;
  inherited Destroy;
end;

procedure TTableInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;
 if Source is TTableInfo then
 begin
  FExportedKeys.Assign(TTableInfo(Source).ExportedKeys);
  FAllFields.Assign(TTableInfo(Source).AllFields);
  FIndexes.Assign(TTableInfo(Source).Indexes);
  FPrimaryKeys.Assign(TTableInfo(Source).PrimaryKeys);
  FFields.Assign(TTableInfo(Source).Fields);
  FIdentities.Assign(TTableInfo(Source).Identities);
  FTableAlias:= TTableInfo(Source).TableAlias;
  FTablename:=TTableInfo(Source).Tablename;
  FSchema:= TTableInfo(Source).Schema;
  FImportedKeys.Assign(TTableInfo(Source).ImportedKeys);
  FHasPrimaryKey:=TTableInfo(Source).HasPrimaryKeys;
  FTriggerInfos.Assign(TTableInfo(Source).Triggers);
  FFormType:=TTableInfo(Source).FormType;
  FTableNameAsControlName:=TTableInfo(Source).TableNameAsControlName;
 end else
 inherited Assign(Source);
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

constructor TTableInfos.Create(aOwner: TComponent; DbInfo: TDbConnectionInfo;
 AutoConnect: Boolean);
var
 r : TResourceStream;
begin

  inherited Create(aOwner,TTableInfo);
  FAutoConnect:=AutoConnect;

  if DbInfo<>nil then
  begin
   FDBinfo := DbInfo;
   InitializeConnection;
  end

end;

destructor TTableInfos.Destroy;
begin
  if adoCon<>nil then
  adoCon.Destroy;
  if dsMetaData<>nil then
  dsMetaData.Destroy;

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

procedure TTableInfos.SetDBInfo(AValue: TDbConnectionInfo);
begin
 if FDBinfo=AValue then Exit;
 FDBinfo:=AValue;
 if FDBinfo<>nil then
 InitializeConnection;
end;

procedure TTableInfos.SetItem(Index: Integer; AValue: TTableInfo);
begin
 Items[Index] := AValue;
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

function TTableInfos.GetTableNameAsControl(TableName: string): string;
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

procedure TTableInfos.InitializeConnection;
begin
   if adoCon<>nil then
   adoCon.Free;

   adoCon := FDbInfo.ToZeosConnection;

   if dsMetaData<>nil then
   dsMetaData.Free;
   dsMetaData := TZSQLMetadata.Create(nil);
   dsMetaData.Connection := adoCon;
   dsMetaData.DisableControls;

   if FAutoConnect then
   adoCon.Connect;
end;

procedure TTableInfos.GetImportedKeyInfos(Schema: string; Tablename: string;
 var ConstraintInfos: TImportedKeyInfos);
var
 CI:TImportedKeyInfo;
  dsLocalHelper:TZSQLMetadata;
  strType:string;
  qr:TZQuery;
begin


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
           CI := ConstraintInfos.Add;
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

            //ConstraintInfos.Add(CI);
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
            CI := ConstraintInfos.Add;
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

            //ConstraintInfos.Add(CI);
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

  try
      if dsMetaData.Active then dsMetaData.Close;
      dsMetaData.MetadataType := mdCrossReference;
      dsMetaData.TableName:=Tablename;
      dsMetaData.Schema:= Schema;
      dsMetaData.Open;
      while not dsMetaData.EOF do
      begin
        ei := ExportKeyInfos.Add;
        ei.ColumnName:=dsMetaData.FieldByName('PKCOLUMN_NAME').AsString;

         if FDBinfo.DatabaseType<>dtFirebirdd then
        ei.ForeignSchemName:=dsMetaData.FieldByName('FKTABLE_SCHEM').AsString;

        ei.ForeignTableName:=dsMetaData.FieldByName('FKTABLE_NAME').AsString;
        ei.ColumnName:=dsMetaData.FieldByName('FKCOLUMN_NAME').AsString;
        //ExportKeyInfos.Add(ei);
        dsMetaData.Next;
      end;
      dsMetaData.Close;
  finally
  end;

end;

function TTableInfos.GetItem(Index: Integer): TTableInfo;
begin
  Result := TTableInfo(inherited Items[Index]);
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
               AddTable(tbl.ImportedKeys[J].ForeignSchema,tbl.ImportedKeys[J].ForeignTableName);
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
      AddTable(Schema,Tables[I]);
    end;

    //if table has constraint, if foreign tables not checked for generate,
    //then generate, because they will be needed for filling comboBoxes
    for I := 0 to Count - 1 do
    begin
    	CheckDependencies(Items[I].Tablename);
    end;

    WriteLog('Done');
end;

function TTableInfos.Add: TTableInfo;
begin
  Result := TTableInfo(inherited Add);
end;

function TTableInfos.Add(Schema: string; Tablename: string
  ): TTableInfo;
var
 lstPKs,lstIdentity:Tstringlist;
 table:TTableInfo;
 field,pkField,idField,nField:TFieldInfo;
 ado:TZQuery;
 strSqlCastExpr:string;
 ti:TTriggerInfo;
 ii:TIndexInfo;
 iks:TImportedKeyInfos;
 eks:TExportedKeyInfos;
begin
   try
     table := TTableInfo(inherited Add);
     table.Tablename := Tablename;
     table.TableNameAsControlName:=GetTableNameAsControl(Tablename);

     iks := table.ImportedKeys;
     eks := table.ExportedKeys;

     GetImportedKeyInfos(Schema,Tablename,iks);
     GetExportedKeyInfos(Schema,Tablename,eks);

     ado:=TZQuery.Create(nil);
     ado.Connection := adoCon;
     ado.DisableControls;

     lstPKs := TStringList.Create;
     lstIdentity := TStringList.Create;

     if FDBinfo.DatabaseType=dtSQLite then Schema:='';

     GetPrimaryKeys(Schema,Tablename,lstPKs);

     if FDBinfo.DatabaseType = dtSQLite then Schema:='';

     table.Schema := Schema;

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


      field := table.AllFields.Add;
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
        field.FieldRef := ado.Fields[0];
        field.AllowNull := dsMetaData.FieldByName('NULLABLE').AsBoolean;
        field.CSharpType := GetCSharpType(field.FieldType);
        field.CSharpName := GetCSharpName(Tablename,field.FieldName);
        field.Validate := (not field.AllowNull) or (lstPKs.IndexOf(field.FieldName)>-1);

        if table.ImportedKeys.ContainsColumn(field.FieldName) then
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
          pkField := table.PrimaryKeys.Add;
          pkField.CopyFrom(field);
        end else
        begin
         field.IsPrimaryKey := False;
         nField := table.Fields.Add;
         nField.CopyFrom(field);
        end;

        if (dsMetaData.FieldByName('AUTO_INCREMENT').AsBoolean) or (field.DataType=ftAutoInc) then
        begin
          field.IsIdentity := True;
          idField := table.Identities.Add;
          idField.CopyFrom(field);
        end;
        field.FieldDbType:=FDBinfo.DatabaseType;
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
      ti := table.Triggers.Add;
      ti.Schem:=dsMetaData.FieldByName('TRIGGER_SCHEM').AsString;
      ti.Cat:= dsMetaData.FieldByName('TRIGGER_CAT').AsString;
      ti.Description:= dsMetaData.FieldByName('TRIGGER_DESCRIPTION').AsString;
      ti.Inactive:= dsMetaData.FieldByName('TRIGGER_INACTIVE').AsBoolean;
      ti.TriggerType:=dsMetaData.FieldByName('TRIGGER_TYPE').AsInteger;
      ti.TriggerSource:=dsMetaData.FieldByName('TRIGGER_SOURCE').AsString;
      ti.Relation:=dsMetaData.FieldByName('TRIGGER_RELATION').AsString;
      dsMetaData.Next;
     end;

     //now get index infos
     dsMetaData.Close;
     dsMetaData.MetadataType:=mdIndexInfo;
     dsMetaData.TableName:=Tablename;
     dsMetaData.Open;

     while not dsMetaData.EOF do
     begin
      ii := table.Indexes.Add;
      ii.INDEX_Name:=dsMetaData.FieldByName('INDEX_NAME').AsString;
      ii.INDEX_Qualifier:=dsMetaData.FieldByName('INDEX_QUALIFIER').AsString;
      ii.IndexType:= dsMetaData.FieldByName('TYPE').AsInteger;
      ii.ASC_OR_DESC:= dsMetaData.FieldByName('ASC_OR_DESC').AsString;
      ii.FILTER_CONDITION:=dsMetaData.FieldByName('FILTER_CONDITION').AsString;
      ii.Ordinal_Position:=dsMetaData.FieldByName('Ordinal_Position').AsInteger;
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

procedure TTableInfos.Reconnect;
begin
  if adoCon<>nil then
  begin
    if adoCon.Connected then
    adoCon.Disconnect;
    adoCon.Connect;
  end;
end;

procedure TTableInfos.AddTable(Schema, Tablename: string);
var
  ti:TTableInfo;
begin
  Add(Schema,Tablename);
end;

{$ENDREGION}

{$ENDREGION}

{$REGION '  TImportedKeys Implementation  ' }

function TImportedKeyInfos.GetByField(Field: TFieldInfo): TImportedKeyInfo;
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

function TImportedKeyInfos.GetByName(Fieldname: string): TImportedKeyInfo;
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

function TImportedKeyInfos.GetRefInfo(Index: integer): TImportedKeyInfo;
begin
  Result := TImportedKeyInfo(inherited  Items[Index]);
end;

procedure TImportedKeyInfos.SetRefInfo(Index: integer; AValue: TImportedKeyInfo);
begin
 inherited Items[Index] := AValue;
end;

constructor TImportedKeyInfos.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner,TImportedKeyInfo);
end;


function TImportedKeyInfos.Add: TImportedKeyInfo;
begin
  Result := TImportedKeyInfo(inherited Add);
end;

function TImportedKeyInfos.ContainsColumn(Fieldname: string): Boolean;
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

function TImportedKeyInfos.ContainsTable(Tablename: string): Boolean;
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

function TImportedKeyInfos.GetIndex(Fieldname: string): Integer;
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

procedure TImportedKeyInfo.SetTablename(AValue: string);

begin
 if FTablename=AValue then Exit;
 FTablename:=AValue;
 FTableAlias:=TAsStringUtils.GetFriendlyAlias(FTablename);
end;

function TImportedKeyInfo.GetDisplayName: string;
begin
 Result:=ColumnName;
end;

procedure TImportedKeyInfo.SetForiegnTablename(AValue: string);
begin
 if FForeignTablename=AValue then Exit;
 FForeignTablename:=AValue;
 FForeignTableAlias:= TAsStringUtils.GetFriendlyAlias(FForeignTablename);
end;

procedure TImportedKeyInfo.SetColumnName(AValue: string);
begin
 if FColumnName=AValue then Exit;
 FColumnName:=AValue;
end;

procedure TImportedKeyInfo.SetConstraintName(AValue: string);
begin
 if FConstraintName=AValue then Exit;
 FConstraintName:=AValue;
end;

procedure TImportedKeyInfo.SetForeignColumnName(AValue: string);
begin
 if FForeignColumnName=AValue then Exit;
 FForeignColumnName:=AValue;
end;

procedure TImportedKeyInfo.SetForeignFirstTextField(AValue: string);
begin
 if FForeignFirstTextField=AValue then Exit;
 FForeignFirstTextField:=AValue;
end;

procedure TImportedKeyInfo.SetForeignSchema(AValue: string);
begin
 if FForeignSchema=AValue then Exit;
 FForeignSchema:=AValue;
end;

procedure TImportedKeyInfo.SetSelectFields(AValue: TStringList);
begin
 if FSelectFields=AValue then Exit;
 FSelectFields:=AValue;
end;


function TImportedKeyInfo.TableAndColumn: string;
begin
 Result:= TableName+ColumnName;
end;

constructor TImportedKeyInfo.Create(aCollection: TCollection);
begin
 inherited Create(aCollection);
 FSelectFields := TStringList.Create;
end;

destructor TImportedKeyInfo.Destroy;
begin
 FSelectFields.Free;
 inherited Destroy;
end;

function TImportedKeyInfo.GetCompatibleColumnName(dbType: TDatabaseType): string;
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

function TImportedKeyInfo.GetCompatibleForeignColumnName(dbType: TDatabaseType
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

procedure TImportedKeyInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;
 if Source is TImportedKeyInfo then
 begin
    FColumnName:=TImportedKeyInfo(Source).ColumnName;
    FConstraintName:=TImportedKeyInfo(Source).ConstraintName;
    FForeignColumnName:=TImportedKeyInfo(Source).ForeignColumnName;
    FForeignFirstTextField:=TImportedKeyInfo(Source).ForeignFirstTextField;
    FForeignSchema:=TImportedKeyInfo(Source).ForeignSchema;
    FSelectFields.Assign(TImportedKeyInfo(Source).SelectFields);
    FTablename:=TImportedKeyInfo(Source).Tablename;
    FTableAlias:=TImportedKeyInfo(Source).TableAlias;
    FForeignTableAlias:=TImportedKeyInfo(Source).ForeignTableAlias;
    FForeignTablename:=TImportedKeyInfo(Source).ForeignTableName;
 end else
 inherited Assign(Source);
end;

{$ENDREGION}

end.

