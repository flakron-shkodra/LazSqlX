{*******************************************************************
AUTHOR : Flakron Shkodra
*******************************************************************
version 1.1, moddate:23.08.2011
*******************************************************************
Versopm 2 moddate 27.10.2014 (removed FGL usage, used collections instead)
*******************************************************************
}

unit AsTableInfo;

{$mode objfpc}{$H+}

interface

uses SysUtils,Classes,AsDbType,DB,typinfo, Forms,StdCtrls,AsStringUtils,Controls,LResources;


type


 TAsControlType = (ctUnknown,ctTextBox,ctComboBox,ctDateTimePicker,ctCheckBox,ctNumeric);

 {this enum not used yet}
 TAsControlNameStyle = (cnsShortPrefixName,cnsNameLongPrefix);

 TAsWinFormType = (wfTransaction,wfDefinition);

 { TAsTriggerInfo }

 TAsTriggerInfo = class(TCollectionItem)
 private
  FBody: string;
  FEvent: string;
  FName: string;
  FOwner: string;
  FRelation: string;
  FStatus: string;
  procedure SetBody(AValue: string);
  procedure SetEvent(AValue: string);
  procedure SetName(AValue: string);
  procedure SetOwner(AValue: string);
  procedure SetStatus(AValue: string);
 public
  procedure Assign(Source: TPersistent); override;
 published
  property Owner:string read FOwner write SetOwner;
  property Name:string read FName write SetName;
  property Event:string read FEvent write SetEvent;
  property Body:string read FBody write SetBody;
  property Status:string read FStatus write SetStatus;
 end;

 { TAsTriggerInfos }

 TAsTriggerInfos = class(TOwnedCollection)
 private
  function GetItems(Index: Integer): TAsTriggerInfo;
  procedure SetItems(Index: Integer; AValue: TAsTriggerInfo);
 public
  constructor Create(AOwner:TPersistent);
  function Add:TAsTriggerInfo;
  property Items[Index:Integer]:TAsTriggerInfo read GetItems write SetItems;default;
 end;

 { TAsIndexInfo }

 TAsIndexInfo = class(TCollectionItem)
 private
  FASC_OR_DESC: string;
  FColumn_Name: string;
  FINDEX_Name: string;
  FOwner:TCollection;
  procedure SetASC_OR_DESC(AValue: string);
  procedure SetColumn_Name(AValue: string);
  procedure SetINDEX_Name(AValue: string);
 protected
    function GetDisplayName: string; override;
 public
    procedure Assign(Source: TPersistent); override;
 published
  property INDEX_Name:string read FINDEX_Name write SetINDEX_Name;
  property Column_Name:string read FColumn_Name write SetColumn_Name;
  property ASC_OR_DESC:string read FASC_OR_DESC write SetASC_OR_DESC;
 end;

 { TAsIndexInfos }

 TAsIndexInfos = class(TOwnedCollection)
 private
  function GetItems(Index: Integer): TAsIndexInfo;
  procedure SetItems(Index: Integer; AValue: TAsIndexInfo);
 public
  constructor Create(AOwner:TPersistent);
  function Add:TAsIndexInfo;
  function GetByName(IndexName:string):TAsIndexInfo;
  property Items[Index:Integer]:TAsIndexInfo read GetItems write SetItems;default;
 end;

 { TAsFieldInfo }
 TAsFieldInfos = class;

 TAsFieldInfo = class(TCollectionItem)
 private
  FAllowNull: Boolean;
  FControlType: TAsControlType;
  FCSharpName: string;
  FCSharpType: string;
  FDataType: TFieldType;
  FFieldRef: TField;
  FFieldDbType: TAsDatabaseType;
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
  procedure SetControlType(AValue: TAsControlType);
  procedure SetCSharpName(AValue: string);
  procedure SetCSharpType(AValue: string);
  procedure SetDataType(AValue: TFieldType);
  procedure SetFCollection(AValue: TAsFieldInfos);
  procedure SetField(AValue: TField);
  procedure SetFieldDbType(AValue: TAsDatabaseType);
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
  function WebControlName:string;
  function WebControlNameWithProp:string;
  function IsNumeric:Boolean;
  function WebControlPrefix:string;
  function WinControlPrefix:string;
  function WinControlName:string;
  function GetCompatibleFieldName(AsDbType: TAsDatabaseType): string;
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
  property ControlType:TAsControlType read FControlType write SetControlType;
  property Validate:Boolean read FValidate write SetValidate;
  property IsReference:Boolean read FIsReference write SetIsReference;
  property Reserved:string read FReserved write SetReserved;
  property FieldDbType:TAsDatabaseType read FFieldDbType write SetFieldDbType;
 end;

 { TAsFieldInfos }

 TAsFieldInfos = class(TOwnedCollection)
 private
   function GetFieldInfo(Index: Integer): TAsFieldInfo;
 public
    constructor Create(aOwner:TPersistent);
    function Add:TAsFieldInfo;
    function GetIndex(FieldName:string):Integer;
    function ToStringList:TStringList;
    property Items[Index:Integer]:TAsFieldInfo read GetFieldInfo;default;
 end;

 { TAsImportedKeyInfo }

 TAsImportedKeyInfos = class;

 TAsImportedKeyInfo = class(TCollectionItem)
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
   function GetCompatibleColumnName(AsDbType: TAsDatabaseType): string;
   function GetCompatibleForeignColumnName(AsDbType: TAsDatabaseType): string;
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

  { TAsImportedKeyInfos }

 TAsImportedKeyInfos = class(TOwnedCollection)
 private
   function GetRefInfo(Index: integer): TAsImportedKeyInfo;
   procedure SetRefInfo(Index: integer; AValue: TAsImportedKeyInfo);
 public
    constructor Create(AOwner:TPersistent);
    function Add:TAsImportedKeyInfo;
    function ContainsColumn(Fieldname:string):Boolean;
    function ContainsTable(Tablename:string):Boolean;
    function GetIndex(Fieldname:string):Integer;
    function GetByName(Fieldname:string):TAsImportedKeyInfo;
    function GetByField(Field:TAsFieldInfo):TAsImportedKeyInfo;
    property Items[Index:integer]:TAsImportedKeyInfo read GetRefInfo write SetRefInfo; default;
 end;

 { TAsTableInfo }

 TAsTableInfos = class;

 TAsTableInfo = class(TCollectionItem)
 strict private
  FAllFields:TAsFieldInfos;//to contain all fields
  FIndexes: TAsIndexInfos;
  FPrimaryKeys:TAsFieldInfos;//to contain only Pks
  FFields:TAsFieldInfos; //to contain only fields that are not PKs
  FIdentities:TAsFieldInfos;
  FTableAlias: string;
  FTablename:string;
  FSchema:string;
  FImportedKeys:TAsImportedKeyInfos;
  FHasPrimaryKey:Boolean;
  FTriggerInfos: TAsTriggerInfos;
  FFormType:TAsWinFormType;
  FTableNameAsControlName:string;
  function GetHasPrimaryKeys: Boolean;
  procedure SetTableName(AValue: string);
 protected
  function GetDisplayName: string; override;
 public

  constructor Create(aCollection:TCollection);override;
  destructor Destroy;override;
  procedure Assign(Source: TPersistent); override;
  function FieldByName(Fieldname:string):TAsFieldInfo;
  function PrimaryKeysAsDelText:string;
  {Compares this instance to given tableInfo returns SQL valid code for altering if they are different}
  function Compare(otherTable:TAsTableInfo;AsDbType:TAsDatabaseType):string;
 published
  property Tablename:string read FTablename write SetTableName;
  property TableNameAsControlName:string read FTableNameAsControlName write FTableNameAsControlName;
  property Schema:string read FSchema write FSchema;
  {All fields}
  property AllFields:TAsFieldInfos read FAllFields write FAllFields;
  {PrimaryKey fields only}
  property PrimaryKeys:TAsFieldInfos read FPrimaryKeys write FPrimaryKeys;
  {Fields that are not primary keys or identities}
  property Fields:TAsFieldInfos read FFields write FFields;
  {Fields that are identities only}
  property Identities:TAsFieldInfos read FIdentities write FIdentities;
  {References to other tables}
  property ImportedKeys:TAsImportedKeyInfos read FImportedKeys write FImportedKeys;
  {Basic trigger info}
  property Triggers:TAsTriggerInfos read FTriggerInfos write FTriggerInfos;
  {Basic index info}
  property Indexes:TAsIndexInfos read FIndexes write FIndexes;
  {Indicates if the table has primary keys}
  property HasPrimaryKeys:Boolean read GetHasPrimaryKeys;
  {used when generating sql query}
  property TableAlias:string read FTableAlias write FTableAlias;

 end;

 { TAsTableInfos }

 TAsTableInfos = class(TOwnedCollection)
 private

    FLogStrings:TStrings;
    FApplication:TApplication;

    FLogListBox:TListBox;
    FDBinfo:TAsDbConnectionInfo;

    procedure GetImportedKeyInfos(Schema:string;Tablename:string;var ConstraintInfos:TAsImportedKeyInfos);
    procedure GetIndexeInfos(Schema:string; Tablename:string; var indexInfos:TAsIndexInfos);
    procedure GetTriggersInfos(Schema:string;Tablename:string; var TriggerInfos:TAsTriggerInfos);

    function GetItem(Index: Integer): TAsTableInfo;

    function GetSqlType(AdoType:TFieldType):string;
    function GetCSharpType(SqlType:String):string;
    function GetFieldType(SqlType:String):TFieldType;

    function GetCompatibleControlName(Tablename,SqlFieldName:string):string;
    function GetTableNameAsControl(TableName:string):string;
    function GetControlType(SqlType:string):TAsControlType;
    procedure SetDBInfo(AValue: TAsDbConnectionInfo);
    procedure SetItem(Index: Integer; AValue: TAsTableInfo);
    procedure WriteLog(msg:string);
    function GetTable(Index: integer): TAsTableInfo;

 public
    constructor Create(aOwner: TComponent; DbInfo: TAsDbConnectionInfo);
    destructor Destroy;override;
    {Adds given tables as TAsTableInfo filled with info}
    procedure LoadFromTables(Schema:string; Tables:TStrings);
    {Adds an empty TAsTableInfo }
    function Add:TAsTableInfo;overload;
    {Adds a TAsTableInfo filled with table info from database; if FullInfo= true triggers&indexes are retrieved too}
    function Add(Schema:string; Tablename:string; FullInfo:Boolean = true):TAsTableInfo;overload;
    {Gets table by it's name}
    function TableByName(Tablename:string):TAsTableInfo;
    {Gets index of given tablename}
    function IndexOf(TableName:string):Integer;
    {gets create table script for sql}
    function GetCreateSQL(TableIndex:Integer):string;
    {backward compatibilty; calls Add(Schema,Tablename)}
    procedure AddTable(Schema,Tablename:string; FullInfo:Boolean=True);
    {Log info}
    property LogStrings:TStrings read FLogStrings write FLogStrings;
    {Log listbox}
    property LogList:TListBox read FLogListBox write FLogListBox;
    property Items[Index:Integer]:TAsTableInfo read GetItem write SetItem;default;
    {Database information object}
    property DbInfo:TAsDbConnectionInfo read FDBinfo write SetDBInfo;
 end;

 { TAsDbTables }

 TAsDbTables = class(TComponent)
 private
    FItems:TAsTableInfos;
    FName: string;
    FDBInfo:TAsDbConnectionInfo;
    FShouldFreeDbInfo : Boolean;
    procedure SetDBInfo(AValue: TAsDbConnectionInfo);
    procedure SetName(AValue: string);
    procedure OnFindClass(Reader: TReader; const AClassName: string; var ComponentClass: TComponentClass);
 public
    constructor Create(aDbInfo:TAsDbConnectionInfo; AutoConnect:Boolean=True);overload;
    {this constructor could be used when the object is going to be loaded from file}
    constructor Create;overload;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(Filename:string);
    procedure SaveToFile(Filename:string);
 published
    property Name:string read FName write SetName;
    property TableInfos:TAsTableInfos read FItems write FItems;
    property DbInfo:TAsDbConnectionInfo read FDBInfo write SetDBInfo;
 end;



implementation

{ TAsTriggerInfo }

procedure TAsTriggerInfo.SetBody(AValue: string);
begin
 if FBody=AValue then Exit;
 FBody:=AValue;
end;

procedure TAsTriggerInfo.SetEvent(AValue: string);
begin
 if FEvent=AValue then Exit;
 FEvent:=AValue;
end;

procedure TAsTriggerInfo.SetName(AValue: string);
begin
 if FName=AValue then Exit;
 FName:=AValue;
end;

procedure TAsTriggerInfo.SetOwner(AValue: string);
begin
 if FOwner=AValue then Exit;
 FOwner:=AValue;
end;

procedure TAsTriggerInfo.SetStatus(AValue: string);
begin
 if FStatus=AValue then Exit;
 FStatus:=AValue;
end;

procedure TAsTriggerInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;

 if Source is TAsTriggerInfo then
 begin
   FOwner:= TAsTriggerInfo(Source).Owner;
   FName:= TAsTriggerInfo(Source).Name;
   FEvent:= TAsTriggerInfo(Source).Event;
   FBody:= TAsTriggerInfo(Source).Body;
   FStatus:=TAsTriggerInfo(Source).Status;
 end else
 inherited Assign(Source);
end;

{ TAsTriggerInfos }

function TAsTriggerInfos.GetItems(Index: Integer): TAsTriggerInfo;
begin
 Result := TAsTriggerInfo(inherited Items[Index]);
end;

procedure TAsTriggerInfos.SetItems(Index: Integer; AValue: TAsTriggerInfo);
begin
 Items[Index] := AValue;
end;

constructor TAsTriggerInfos.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner,TAsTriggerInfo);
end;

function TAsTriggerInfos.Add: TAsTriggerInfo;
begin
 Result := TAsTriggerInfo(inherited Add);
end;

{ TAsIndexInfo }

procedure TAsIndexInfo.SetASC_OR_DESC(AValue: string);
begin
 if FASC_OR_DESC=AValue then Exit;
 FASC_OR_DESC:=AValue;
end;

procedure TAsIndexInfo.SetColumn_Name(AValue: string);
begin
 if FColumn_Name=AValue then Exit;
 FColumn_Name:=AValue;
end;

procedure TAsIndexInfo.SetINDEX_Name(AValue: string);
begin
 if FINDEX_Name=AValue then Exit;
 FINDEX_Name:=AValue;
end;

function TAsIndexInfo.GetDisplayName: string;
begin
 Result:=FINDEX_Name;
end;

procedure TAsIndexInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;

 if Source is TAsIndexInfo then
 begin
   FASC_OR_DESC:= TAsIndexInfo(Source).ASC_OR_DESC;
   FColumn_Name:=TAsIndexInfo(Source).Column_Name;
   FINDEX_Name:=TAsIndexInfo(Source).INDEX_Name;
 end else
 inherited Assign(Source);
end;

{ TAsDbTables }

procedure TAsDbTables.SetName(AValue: string);
begin
 if FName=AValue then Exit;
 FName:=AValue;
end;

procedure TAsDbTables.SetDBInfo(AValue: TAsDbConnectionInfo);
begin
 FDBInfo := AValue;
 FItems.DbInfo:=AValue;
 FShouldFreeDbInfo:=False;
end;


procedure TAsDbTables.OnFindClass(Reader: TReader; const AClassName: string;
 var ComponentClass: TComponentClass);
begin
  if CompareText(AClassName,'TAsDbTables')=0 then
    ComponentClass:=TAsDbTables;
end;

constructor TAsDbTables.Create(aDbInfo: TAsDbConnectionInfo; AutoConnect: Boolean
 );
begin
 inherited Create(nil);
 FDBInfo := aDbInfo;
 if FDBInfo=nil then
 begin
   FDBInfo := TAsDbConnectionInfo.Create;
   FShouldFreeDbInfo:=True;
 end;
 FItems := TAsTableInfos.Create(Self,aDbInfo);
end;

constructor TAsDbTables.Create;
begin
  inherited Create(nil);
  FDBInfo := TAsDbConnectionInfo.Create;
  FShouldFreeDbInfo:=True;
  FItems := TAsTableInfos.Create(Self,FDBInfo);
end;

destructor TAsDbTables.Destroy;
begin
 FItems.Destroy;

  if FShouldFreeDbInfo then
   FDBInfo.Destroy;

 inherited Destroy;
end;

procedure TAsDbTables.Clear;
begin
 FItems.Clear;
end;

procedure TAsDbTables.LoadFromFile(Filename: string);
var
  mem:TMemoryStream;
begin
  try
    mem := TMemoryStream.Create;
    mem.LoadFromFile(Filename);
    ReadComponentFromBinaryStream(mem,TComponent(Self),@OnFindClass);
  finally
    mem.Free;
  end;
end;

procedure TAsDbTables.SaveToFile(Filename: string);
var
  mem:TMemoryStream;
  ti :TAsTableInfo;
begin
  try
    mem := TMemoryStream.Create;
    WriteComponentAsBinaryToStream(mem,TComponent(Self));
    mem.SaveToFile(Filename);
  finally
    mem.Free;
  end;
end;

{ TAsIndexInfos }

function TAsIndexInfos.GetItems(Index: Integer): TAsIndexInfo;
begin
 Result := TAsIndexInfo(inherited Items[index]);
end;

procedure TAsIndexInfos.SetItems(Index: Integer; AValue: TAsIndexInfo);
begin
  Items[Index] := AValue;
end;

constructor TAsIndexInfos.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner,TAsIndexInfo);
end;

function TAsIndexInfos.Add: TAsIndexInfo;
begin
  Result := TAsIndexInfo(inherited Add);
end;

function TAsIndexInfos.GetByName(IndexName: string): TAsIndexInfo;
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


{ TAsFieldInfos }

function TAsFieldInfos.GetFieldInfo(Index: Integer): TAsFieldInfo;
begin
  Result := TAsFieldInfo(inherited  Items[Index]);
end;

constructor TAsFieldInfos.Create(aOwner: TPersistent);
begin
 inherited Create(aOwner,TAsFieldInfo);
end;

function TAsFieldInfos.Add: TAsFieldInfo;
begin
 Result := TAsFieldInfo(inherited Add);
end;

function TAsFieldInfos.GetIndex(FieldName: string): Integer;
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

function TAsFieldInfos.ToStringList: TStringList;
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

procedure TAsFieldInfo.SetAllowNull(AValue: Boolean);
begin
 if FAllowNull=AValue then Exit;
 FAllowNull:=AValue;
end;

procedure TAsFieldInfo.SetControlType(AValue: TAsControlType);
begin
 if FControlType=AValue then Exit;
 FControlType:=AValue;
end;

procedure TAsFieldInfo.SetCSharpName(AValue: string);
begin
 if FCSharpName=AValue then Exit;
 FCSharpName:=AValue;
end;

procedure TAsFieldInfo.SetCSharpType(AValue: string);
begin
 if FCSharpType=AValue then Exit;
 FCSharpType:=AValue;
end;

procedure TAsFieldInfo.SetDataType(AValue: TFieldType);
begin
 if FDataType=AValue then Exit;
 FDataType:=AValue;
end;

procedure TAsFieldInfo.SetFCollection(AValue: TAsFieldInfos);
begin

end;

procedure TAsFieldInfo.SetField(AValue: TField);
begin
 if FFieldRef=AValue then Exit;
 FFieldRef:=AValue;
end;

procedure TAsFieldInfo.SetFieldDbType(AValue: TAsDatabaseType);
begin
 if FFieldDbType=AValue then Exit;
 FFieldDbType:=AValue;
end;

procedure TAsFieldInfo.SetFieldName(AValue: string);
begin
 if FFieldName=AValue then Exit;
 FFieldName:=AValue;
end;

procedure TAsFieldInfo.SetFieldType(AValue: string);
begin
 if FFieldType=AValue then Exit;
 FFieldType:=AValue;
end;

procedure TAsFieldInfo.SetIsIdentity(AValue: Boolean);
begin
 if FIsIdentity=AValue then Exit;
 FIsIdentity:=AValue;
end;

procedure TAsFieldInfo.SetIsPrimaryKey(AValue: Boolean);
begin
 if FIsPrimaryKey=AValue then Exit;
 FIsPrimaryKey:=AValue;
end;

procedure TAsFieldInfo.SetIsReference(AValue: Boolean);
begin
 if FIsReference=AValue then Exit;
 FIsReference:=AValue;
end;

procedure TAsFieldInfo.SetLength(AValue: Integer);
begin
 if FLength=AValue then Exit;
 FLength:=AValue;
end;

procedure TAsFieldInfo.SetPrecision(AValue: Integer);
begin
 if FPrecision=AValue then Exit;
 FPrecision:=AValue;
end;

procedure TAsFieldInfo.SetReserved(AValue: string);
begin
 if FReserved=AValue then Exit;
 FReserved:=AValue;
end;

procedure TAsFieldInfo.SetValidate(AValue: Boolean);
begin
 if FValidate=AValue then Exit;
 FValidate:=AValue;
end;

function TAsFieldInfo.GetDisplayName: string;
begin
  Result:=FFieldName
end;

function TAsFieldInfo.IsNumeric: Boolean;
begin
   Result := (CSharpType='int') or (CSharpType='decimal');
end;

function TAsFieldInfo.WebControlName: string;
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

function TAsFieldInfo.WebControlNameWithProp: string;
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

function TAsFieldInfo.WebControlPrefix: string;
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

function TAsFieldInfo.WinControlName: string;
begin
  Result := WinControlPrefix+CSharpName;
end;

function TAsFieldInfo.GetCompatibleFieldName(AsDbType:TAsDatabaseType): string;
var
 o,c:string;
begin
 case AsDbType of
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
     o:='`';
     c:='`';
   end;
  dtSQLite:
     begin
     o:='"';
     c:='"';
   end;
 end;
 Result:=o+FieldName+c;
end;

procedure TAsFieldInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;

  if Source is TAsFieldInfo then
  begin
    FAllowNull:=TAsFieldInfo(Source).AllowNull;
    FControlType:=TAsFieldInfo(Source).ControlType;
    FCSharpName:=TAsFieldInfo(Source).CSharpName;
    FDataType:=TAsFieldInfo(Source).DataType;
    FFieldRef := TAsFieldInfo(Source).FieldRef;
    FFieldDbType:= TAsFieldInfo(Source).FieldDbType;
    FFieldName:=TAsFieldInfo(Source).FieldName;
    FFieldType:=TAsFieldInfo(Source).FieldType;
    FIsIdentity:=TAsFieldInfo(Source).IsIdentity;
    FIsPrimaryKey:=TAsFieldInfo(Source).IsPrimaryKey;
    FIsReference:=TAsFieldInfo(Source).IsReference;
    FLength:=TAsFieldInfo(Source).Length;
    FPrecision:=TAsFieldInfo(Source).Precision;
    FReserved:=TAsFieldInfo(Source).Reserved;
    FValidate:=TAsFieldInfo(Source).Validate;
  end else
 inherited Assign(Source);
end;

function TAsFieldInfo.WinControlPrefix: string;
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

{$ENDREGION}

{$REGION '   TTableInfo Implementation   ' }

{$REGION 'Constructor/Destructor'}

function TAsTableInfo.GetHasPrimaryKeys: Boolean;
begin
 Result := False;
 if FPrimaryKeys <> nil then
  Result := FPrimaryKeys.Count>0;
end;


procedure TAsTableInfo.SetTableName(AValue: string);
begin
 if FTablename=AValue then Exit;
 FTablename:=AValue;
 FTableAlias:=TAsStringUtils.GetFriendlyAlias(FTablename);
end;

function TAsTableInfo.GetDisplayName: string;
begin
  Result:=FTablename
end;


constructor TAsTableInfo.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);

  FFields := TAsFieldInfos.Create(FFields);
  FAllFields := TAsFieldInfos.Create(FAllFields);
  FIdentities := TAsFieldInfos.Create(FIdentities);
  FPrimaryKeys := TAsFieldInfos.Create(FPrimaryKeys);

  FImportedKeys := TAsImportedKeyInfos.Create(FImportedKeys);

  FTriggerInfos := TAsTriggerInfos.Create(FTriggerInfos);
  FIndexes :=  TAsIndexInfos.Create(FIndexes);

end;

destructor TAsTableInfo.Destroy;
begin
  FAllFields.Free;
  FFields.Free;
  FPrimaryKeys.Free;
  FIdentities.Free;
  FImportedKeys.Free;
  FTriggerInfos.Free;
  FIndexes.Free;
  inherited Destroy;
end;

procedure TAsTableInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;
 if Source is TAsTableInfo then
 begin
  //FExportedKeys.Assign(TAsTableInfo(Source).ExportedKeys);
  FAllFields.Assign(TAsTableInfo(Source).AllFields);
  FIndexes.Assign(TAsTableInfo(Source).Indexes);
  FPrimaryKeys.Assign(TAsTableInfo(Source).PrimaryKeys);
  FFields.Assign(TAsTableInfo(Source).Fields);
  FIdentities.Assign(TAsTableInfo(Source).Identities);
  FTableAlias:= TAsTableInfo(Source).TableAlias;
  FTablename:=TAsTableInfo(Source).Tablename;
  FSchema:= TAsTableInfo(Source).Schema;
  FImportedKeys.Assign(TAsTableInfo(Source).ImportedKeys);
  FHasPrimaryKey:=TAsTableInfo(Source).HasPrimaryKeys;
  FTriggerInfos.Assign(TAsTableInfo(Source).Triggers);
  FTableNameAsControlName:=TAsTableInfo(Source).TableNameAsControlName;
 end else
 inherited Assign(Source);
end;

function TAsTableInfo.FieldByName(Fieldname: string): TAsFieldInfo;
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

function TAsTableInfo.PrimaryKeysAsDelText: string;
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

function TAsTableInfo.Compare(otherTable: TAsTableInfo;AsDbType:TAsDatabaseType): string;
var
  I: Integer;
  lst:TStringList;
  SqlFromDot: string;
  SqlOpenBr: string;
  SqlCloseBr: string;
  SqlDot: string;
begin

 SqlFromDot:='.';

  case AsDbType of
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

constructor TAsTableInfos.Create(aOwner: TComponent; DbInfo: TAsDbConnectionInfo
 );
var
 r : TResourceStream;
begin

  inherited Create(aOwner,TAsTableInfo);

  if DbInfo<>nil then
  begin
   FDBinfo := DbInfo;
  end

end;

destructor TAsTableInfos.Destroy;
begin
  inherited Destroy;
end;


{$ENDREGION}

{$REGION 'Private'}

function TAsTableInfos.GetFieldType(SqlType:String):TFieldType;
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


function TAsTableInfos.GetControlType(SqlType: string): TAsControlType;
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

procedure TAsTableInfos.SetDBInfo(AValue: TAsDbConnectionInfo);
begin
 if FDBinfo=AValue then Exit;
 FDBinfo:=AValue;
end;

procedure TAsTableInfos.SetItem(Index: Integer; AValue: TAsTableInfo);
begin
 Items[Index] := AValue;
end;

function TAsTableInfos.GetCompatibleControlName(Tablename,SqlFieldName: string): string;
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

    Result := CType;
end;

function TAsTableInfos.GetTableNameAsControl(TableName: string): string;
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

    Result := CType;
end;

function TAsTableInfos.GetCSharpType(SqlType: String): string;
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


function TAsTableInfos.GetTable(Index: integer): TAsTableInfo;
begin
  Result := Items[Index];
end;

procedure TAsTableInfos.GetImportedKeyInfos(Schema: string; Tablename: string;
 var ConstraintInfos: TAsImportedKeyInfos);
var
  CI:TAsImportedKeyInfo;
  fks:TAsForeignKeys;
  fk:TAsForeignKey;
  lst:TStringList;
begin

  if ConstraintInfos=nil then
  Exit;

  ConstraintInfos.Clear;

  try
    fks := TAsDbUtils.GetForeignKeys(FDBinfo,Tablename);
    for fk in fks do
    begin
     ci := ConstraintInfos.Add;
     ci.ConstraintName:=fk.Constraint_Name;
     ci.ColumnName:=fk.Column_Name;
     ci.Tablename:=fk.Table_Name;
     ci.ForeignSchema:=fk.Foreign_Schema;
     ci.ForeignTableName:=fk.Foreign_Table;
     ci.ForeignColumnName:=fk.Foreign_Column;
     try
      lst := TAsDbUtils.GetTextFields(FDBinfo,ci.ForeignTableName);
      if lst.Count>0 then
      begin
        ci.ForeignFirstTextField:= lst[0];
        ci.SelectFields.Add(lst[0]);
      end;
     finally
      lst.Free;
     end;
    end;
  finally
    fks.Free;
  end;
end;

procedure TAsTableInfos.GetIndexeInfos(Schema: string; Tablename: string;
 var indexInfos: TAsIndexInfos);
var
  lst:TAsIndexes;
  i:TAsIndex;
  inf:TAsIndexInfo;
begin
 if indexInfos=nil then exit;
 indexInfos.Clear;

 try
  lst := TAsDbUtils.GetIndexes(FDBinfo,Tablename);
  for i in lst do
  begin
   inf := indexInfos.Add;
   inf.Column_Name:= i.Column_Name;
   inf.INDEX_Name:= i.Index_Name;
   inf.ASC_OR_DESC:=i.Descend;
  end;
 finally
   lst.Free;
 end;
end;

procedure TAsTableInfos.GetTriggersInfos(Schema: string; Tablename: string;
 var TriggerInfos: TAsTriggerInfos);
var
  c:TAsTriggerInfo;
  ts:TAsTriggers;
  t:TAsTrigger;
begin
 if TriggerInfos=nil then exit;
 TriggerInfos.Clear;

 try
  ts := TAsDbUtils.GetTriggers(FDBinfo,Tablename);
  for t in ts do
  begin
   c := TriggerInfos.Add;
   c.Owner:=t.Trigger_Owner;
   c.Name:=t.Trigger_Name;
   c.Event:=t.Trigger_Event;
   c.Body:=t.Trigger_Body;
   c.Status:=t.Trigger_Status;
  end;
 finally
   ts.Free;
 end;
end;

function TAsTableInfos.GetItem(Index: Integer): TAsTableInfo;
begin
  Result := TAsTableInfo(inherited Items[Index]);
end;

function TAsTableInfos.GetSqlType(AdoType: TFieldType): string;
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


procedure TAsTableInfos.WriteLog(msg: string);
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

procedure TAsTableInfos.LoadFromTables(Schema:string; Tables: TStrings);
  //CheckDependencies will also do recursive
	procedure CheckDependencies(tablename:string);
  var
  	J:Integer;
    tbl:TAsTableInfo;
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

function TAsTableInfos.Add: TAsTableInfo;
begin
  Result := TAsTableInfo(inherited Add);
end;

function TAsTableInfos.Add(Schema: string; Tablename: string;
 FullInfo: Boolean): TAsTableInfo;
var
 lstPKs,lstIdentity:Tstringlist;
 table:TAsTableInfo;
 field,pkField,idField,nField:TAsFieldInfo;
 strSqlCastExpr:string;
 tis:TAsTriggerInfos;
 ii:TAsIndexInfos;
 iks:TAsImportedKeyInfos;
 columns:TAsColumns=nil;
 c:TAsColumn;
 ds:TAsQuery=nil;
 sql:string;
begin

   try
    Screen.Cursor:=crHourGlass;
     table := TAsTableInfo(inherited Add);
     table.Tablename := Tablename;
     table.TableNameAsControlName:=GetTableNameAsControl(Tablename);

     lstIdentity := TStringList.Create;
     lstPKs := TAsDbUtils.GetPrimaryKeys(FDBinfo,Tablename);

     if FDBinfo.DbType=dtSQLite then Schema:='';
     if FDBinfo.DbType = dtSQLite then Schema:='';
     table.Schema := Schema;

     iks := table.ImportedKeys;
     ii:=table.Indexes;
     tis := table.Triggers;

     columns := TAsDbUtils.GetColumns(FDBinfo,Tablename);

     GetImportedKeyInfos(Schema,Tablename,iks);

     if FullInfo then
     begin
      GetIndexeInfos(Schema,Tablename,ii);
      GetTriggersInfos(Schema,Tablename,tis);
     end;

     ds := TAsQuery.Create(FDBinfo);
     //Get FIELD INFOS
     for c in columns do
     begin

      field := table.AllFields.Add;
      field.FieldName := c.Column_Name;
      field.FieldType := LowerCase(c.Data_Type);
      if field.FieldType='int identity' then field.FieldType:='int';
      if FDBinfo.DbType in [dtOracle,dtMySql] then
      field.Length := (c.Max_Length DIV 4 )
      else
      field.Length := c.Max_Length;

      field.Precision := c.Data_Precision;

      strSqlCastExpr:='';

      if FDBinfo.DbType= dtMsSql then
      begin
          if (field.FieldType='nchar') or (field.FieldType='nvarchar')
          then
            field.Length := c.Max_Length div 2;
          if (field.FieldType='ntext') then
          strSqlCastExpr:='text';
          if (field.FieldType='nvarchar') or (field.FieldType='xml') then
          strSqlCastExpr:='varchar(max)';
      end;

      try
        ds.Open(TAsDbUtils.GetTopRecordsSelect(FDBinfo.DbType,Schema,Tablename, field.FieldName,1));

        field.DataType := ds.Fields[0].DataType;
        field.FieldRef := ds.Fields[0];
        field.AllowNull := c.Allow_Null;
        field.CSharpType := GetCSharpType(field.FieldType);
        field.CSharpName := GetCompatibleControlName(Tablename,field.FieldName);
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
          pkField.Assign(field);
        end else
        begin
         field.IsPrimaryKey := False;
         nField := table.Fields.Add;
         nField.Assign(field);
        end;

        if (field.DataType=ftAutoInc) then
        begin
          field.IsIdentity := True;
          idField := table.Identities.Add;
          idField.Assign(field);
        end;
        field.FieldDbType:=FDBinfo.DbType;
      except
        if Assigned(field) then
        field.Free;
      end;
     end;

   finally
    Screen.Cursor:=crDefault;
    lstIdentity.Free;
    lstPKs.Free;
    if columns<>nil then
      columns.Free;
    if ds<>nil then
      ds.Free;
   end;

   Result := table;

end;

function TAsTableInfos.TableByName(Tablename: string): TAsTableInfo;
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

function TAsTableInfos.IndexOf(TableName: string): Integer;
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

function TAsTableInfos.GetCreateSQL(TableIndex:Integer): string;
var
  lst:TStringList;
  j: Integer;
  f:TAsFieldInfo;
  s:string;
  SqlFromDot: string;
  SqlOpenBr: string;
  SqlCloseBr: string;
  SqlDot: string;
  i: Integer;
  tbl:TAsTableInfo;
begin

 SqlFromDot:='.';

  case FDBinfo.DbType of
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

procedure TAsTableInfos.AddTable(Schema, Tablename: string; FullInfo: Boolean);
var
  ti:TAsTableInfo;
begin
  Add(Schema,Tablename,FullInfo);
end;

{$ENDREGION}

{$ENDREGION}

{$REGION '  TImportedKeys Implementation  ' }

function TAsImportedKeyInfos.GetByField(Field: TAsFieldInfo): TAsImportedKeyInfo;
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

function TAsImportedKeyInfos.GetByName(Fieldname: string): TAsImportedKeyInfo;
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

function TAsImportedKeyInfos.GetRefInfo(Index: integer): TAsImportedKeyInfo;
begin
  Result := TAsImportedKeyInfo(inherited  Items[Index]);
end;

procedure TAsImportedKeyInfos.SetRefInfo(Index: integer; AValue: TAsImportedKeyInfo);
begin
 inherited Items[Index] := AValue;
end;

constructor TAsImportedKeyInfos.Create(AOwner: TPersistent);
begin
 inherited Create(AOwner,TAsImportedKeyInfo);
end;


function TAsImportedKeyInfos.Add: TAsImportedKeyInfo;
begin
  Result := TAsImportedKeyInfo(inherited Add);
end;

function TAsImportedKeyInfos.ContainsColumn(Fieldname: string): Boolean;
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

function TAsImportedKeyInfos.ContainsTable(Tablename: string): Boolean;
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

function TAsImportedKeyInfos.GetIndex(Fieldname: string): Integer;
var
  I: Integer;
  fn: String;
begin
  Result := -1;
  fn := TAsStringUtils.RemoveChars(Fieldname,['[',']','"','`']);
  for I := 0 to Count - 1 do
  begin
    if (Items[I].ColumnName=fn) and (Items[I].TableName<>Items[I].ForeignTableName) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TAsImportedKeyInfo.SetTablename(AValue: string);

begin
 if FTablename=AValue then Exit;
 FTablename:=AValue;
 FTableAlias:=TAsStringUtils.GetFriendlyAlias(FTablename);
end;

function TAsImportedKeyInfo.GetDisplayName: string;
begin
 Result:=ColumnName;
end;

procedure TAsImportedKeyInfo.SetForiegnTablename(AValue: string);
begin
 if FForeignTablename=AValue then Exit;
 FForeignTablename:=AValue;
 FForeignTableAlias:= TAsStringUtils.GetFriendlyAlias(FForeignTablename);
end;

procedure TAsImportedKeyInfo.SetColumnName(AValue: string);
begin
 if FColumnName=AValue then Exit;
 FColumnName:=AValue;
end;

procedure TAsImportedKeyInfo.SetConstraintName(AValue: string);
begin
 if FConstraintName=AValue then Exit;
 FConstraintName:=AValue;
end;

procedure TAsImportedKeyInfo.SetForeignColumnName(AValue: string);
begin
 if FForeignColumnName=AValue then Exit;
 FForeignColumnName:=AValue;
end;

procedure TAsImportedKeyInfo.SetForeignFirstTextField(AValue: string);
begin
 if FForeignFirstTextField=AValue then Exit;
 FForeignFirstTextField:=AValue;
end;

procedure TAsImportedKeyInfo.SetForeignSchema(AValue: string);
begin
 if FForeignSchema=AValue then Exit;
 FForeignSchema:=AValue;
end;

procedure TAsImportedKeyInfo.SetSelectFields(AValue: TStringList);
begin
 if FSelectFields=AValue then Exit;
 FSelectFields:=AValue;
end;


function TAsImportedKeyInfo.TableAndColumn: string;
begin
 Result:= TableName+ColumnName;
end;

constructor TAsImportedKeyInfo.Create(aCollection: TCollection);
begin
 inherited Create(aCollection);
 FSelectFields := TStringList.Create;
end;

destructor TAsImportedKeyInfo.Destroy;
begin
 FSelectFields.Free;
 inherited Destroy;
end;

function TAsImportedKeyInfo.GetCompatibleColumnName(AsDbType: TAsDatabaseType): string;
var
 o,c:string;
begin
 case AsDbType of
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

function TAsImportedKeyInfo.GetCompatibleForeignColumnName(AsDbType: TAsDatabaseType
 ): string;
var
 o,c:string;
begin
 case AsDbType of
  dtMsSql,dtSQLite:
   begin
     o:='[';
     c:=']';
   end;
  dtOracle,dtFirebirdd, dtPostgreSql:
      begin
     o:='"';
     c:='"';
   end;
  dtMySql:
       begin
     o:='`';
     c:='`';
   end;
 end;
 Result:=o+ForeignColumnName+c;
end;

procedure TAsImportedKeyInfo.Assign(Source: TPersistent);
begin
  if Source=nil then
  Exit;
 if Source is TAsImportedKeyInfo then
 begin
    FColumnName:=TAsImportedKeyInfo(Source).ColumnName;
    FConstraintName:=TAsImportedKeyInfo(Source).ConstraintName;
    FForeignColumnName:=TAsImportedKeyInfo(Source).ForeignColumnName;
    FForeignFirstTextField:=TAsImportedKeyInfo(Source).ForeignFirstTextField;
    FForeignSchema:=TAsImportedKeyInfo(Source).ForeignSchema;
    FSelectFields.Assign(TAsImportedKeyInfo(Source).SelectFields);
    FTablename:=TAsImportedKeyInfo(Source).Tablename;
    FTableAlias:=TAsImportedKeyInfo(Source).TableAlias;
    FForeignTableAlias:=TAsImportedKeyInfo(Source).ForeignTableAlias;
    FForeignTablename:=TAsImportedKeyInfo(Source).ForeignTableName;
 end else
 inherited Assign(Source);
end;

{$ENDREGION}

end.


