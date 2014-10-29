//*******************
//Flakron Shkodra
//Version 1 (23.10.2014)
//*******************

unit AsDbFormUtils;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,Forms,Graphics,Dialogs,Controls,StdCtrls,ExtCtrls,ComCtrls, DbCtrls,DBGrids,Grids, TableInfo,DbType,
 DB, sqldb, fgl,Spin, EditBtn,Buttons,mssqlconn,{$ifndef win64}oracleconnection,{$endif} IBConnection,
 mysql55conn, mysql51conn, mysql50conn, mysql40conn, mysql41conn, sqlite3conn,ZConnection,ZDataset;

type

  { TAsUIDataAcessObject }

  TAsUIDataAcessObject = class(TComponent)
  private
    FOwner:TComponent;
    FDataSource: TDataSource;
    FName: string;
    FSqlQuery:string;
    FSqlCon:TSQLConnector;
    FZCon:TZConnection;
    FQuery: TSQLQuery;
    FZQuery:TZQuery;
    FDBengine:TDatabaseEngine;
    function GetQueryComponent: TDataSet;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetName(AValue: string);
    procedure SetQuery(AValue: TSQLQuery);
    procedure AfterPost(DataSet:TDataSet);
    procedure AfterDelete(DataSet:TDataSet);
    procedure SetSqlQuery(AValue: string);
  public
    constructor Create(SqlCon:TSQLConnector; aSqlQuery:string);overload;
    constructor Create(ZCon:TZConnection; aSqlQuery:string);overload;
    destructor Destroy;override;
    procedure Open;
    procedure Close;
  published
    property Name:string read FName write SetName;
    property Query:TDataSet read GetQueryComponent;
    property DataSource:TDataSource read FDataSource write SetDataSource;
    property DbEngine:TDatabaseEngine read FDBengine;
    property SqlQuery:string read FSqlQuery write SetSqlQuery;
  end;


  { TAsUIDataAcessObjects }

  TAsUIDataAcessObjects = class(specialize TFPGObjectList<TAsUIDataAcessObject>)
  private
    FLastError:string;
  public
    property LastError:string read FLastError;
    function FindByName(Name:string):TAsUIDataAcessObject;
    function OpenAll:Boolean;
    function CloseAll:Boolean;
    procedure Refresh;
  end;

  { TAsDbDateEdit }

  TAsDbDateEdit = class(TDateEdit)
  private
     FDataField: string;
     FDataLink:TFieldDataLink;
     FDataSource: TDataSource;
     FField: TField;
     FDateTime:TDateTime;
     function GetField: TField;
     procedure SetDataField(AValue: string);
     procedure SetDataSource(AValue: TDataSource);
     procedure SetField(AValue: TField);
     procedure OnAccept(Sender: TObject; var ADate: TDateTime; var AcceptDate: Boolean);
     procedure OnDataChanged(Sender: TObject);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property DataSource:TDataSource read FDataSource write SetDataSource;
    property DataField:string read FDataField write SetDataField;
    property Field:TField read GetField;
  end;

  { TAsDbBlobEdit }

  TAsDbBlobEdit = class(TPanel)
  private
    FDataField: string;
    FDataLink:TFieldDataLink;
    FDataSource: TDataSource;
    FField: TField;
    FLabel:TLabel;
    FOpenButton:TSpeedButton;
    FOpenDialog:TOpenDialog;
    FSaveDialog:TSaveDialog;
    function GetField: TField;
    procedure SetDataField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure OnDataChanged(Sender: TObject);
    procedure OnOpenClick(Sender: TObject);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property DataSource:TDataSource read FDataSource write SetDataSource;
    property DataField:string read FDataField write SetDataField;
    property Field:TField read GetField;
  end;

  { TAsDBLookupComboBoxEdit }

  TAsDBLookupComboBoxEdit = class(TPanel)
  private
    FComboBox:TDBLookupComboBox;
    FEditButton:TSpeedButton;
    function GetDataFieldName: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetHint: string;
    function GetKeyField: string;
    function GetListField: string;
    function GetListSource: TDataSource;
    function GetOnEditButtonClick: TNotifyEvent;
    function GetStyle: TComboBoxStyle;
    procedure SetDataField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetHint(AValue: string);
    procedure SetKeyField(AValue: string);
    procedure SetListField(AValue: string);
    procedure SetListSource(AValue: TDataSource);
    procedure SetOnEditButtonClick(AValue: TNotifyEvent);
    procedure SetStyle(AValue: TComboBoxStyle);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property DataSource:TDataSource read GetDataSource write SetDataSource;
    property DataField:string read GetDataFieldName write SetDataField;
    property ListSource:TDataSource read GetListSource write SetListSource;
    property ListField:string read GetListField write SetListField;
    property KeyField:string read GetKeyField write SetKeyField;
    property OnEditButtonClick:TNotifyEvent read GetOnEditButtonClick write SetOnEditButtonClick;
    property Hint:string read GetHint write SetHint;
    property Style:TComboBoxStyle read GetStyle write SetStyle;
  end;

  { TAsQueryBuilderForm }

  TAsQueryBuilderForm = class(TForm)
  private
   FSqlQuery: string;
    FTableInfo:TTableInfo;
    FPanel:TPanel;
    FEdit:TEdit;
    FCombo:TComboBox;
    FCondition :TComboBox;
    FAdd:TSpeedBUtton;
    FListBox:TListBox;
    FBtnOk,FBtnCancel:TButton;
    procedure SetSqlQuery(AValue: string);
    procedure OnAddClick(Sender: TObject);
   procedure OnBtnOkClick(Sender: TObject);
  public
    constructor Create(aTableInfo:TTableInfo);
    property SqlQuery:string read FSqlQuery write SetSqlQuery;
  end;

  { TAsDbForm }

  TAsDbFormFilter = (dffNone,dffTopRecords,dffCustomFilter);

  TAsDbForm = class(TForm)
  private
   FControlWidth: Integer;
   FControlSpace:Integer;
   FDataGrid: TDBGrid;
   FDataNavigator: TDBNavigator;
   FDataObject: TAsUIDataAcessObject;
   FDbInfo:TDbConnectionInfo;
   FEditControlsBox: TScrollBox;
   FGridNavGroup:TGroupBox;
   FRefDataObjects: TAsUIDataAcessObjects;
   FTableInfo:TTableInfo;
   FCon:TSQLConnector;
   FZCon:TZConnection;
   FSchema:string;//used only when AsDbLookupComboEdit (table reference field is clicked for edit '..')
   FSqlQuery:string;
   FEditRowCount:Integer;
   FQuickSearchToolbar:TToolbar;
   FQuickSearchFieldsCmb:TComboBox;//created from MakeQuickSearchToolbar (populated from OpenData)
   FQuickSearchEdit:TEdit;//created from MakeQuickSearchToolbar (used in OnQuickSearchChange)
   procedure SetControlWidth(AValue: Integer);
   procedure SetDataGrid(AValue: TDBGrid);
   procedure SetDataNavigator(AValue: TDBNavigator);
   procedure SetDataObject(AValue: TAsUIDataAcessObject);
   procedure SetEditControlsPanel(AValue: TScrollBox);
   procedure SetRefDataObjects(AValue: TAsUIDataAcessObjects);
   procedure OnDBGridDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
   procedure OnCleanTlbButton(Sender:Tobject);
   procedure OnQuickSearchChange(Sender:TObject);
   procedure OnDbLookupComboEditClick(Sender:TObject);//assigned when generating edit controls for reference fields
   procedure MakeEditControls;
   function MakeQuickSearchToolbar(aOwner:TComponent):TToolbar;
   procedure SetSqlQuery(AValue: string);
   function GetTopRecordSelect(TopRecords:Integer):string;
  public
    constructor Create(aDbInfo:TDbConnectionInfo; aSchema:string; aTableInfo:TTableInfo);
    destructor Destroy;override;
    procedure OpenData;
    function ShowModal(FormFilter:TAsDbFormFilter=dffNone):TModalResult;
  published
    property EditControlsPanel:TScrollBox read FEditControlsBox;
    property DataObject:TAsUIDataAcessObject read FDataObject;
    property RefDataObjects:TAsUIDataAcessObjects read FRefDataObjects;
    property DataGrid:TDBGrid read FDataGrid;
    property DataNavigator:TDBNavigator read FDataNavigator;
    property ControlWidth:Integer read FControlWidth write SetControlWidth;
    property ControlSpace:Integer read FControlSpace write FControlSpace;
  end;


implementation

{ TAsQueryBuilderForm }

procedure TAsQueryBuilderForm.OnAddClick(Sender: TObject);
var
 s:string;
 fi:TFieldInfo;
begin
 if Trim(FEdit.Text)<>EmptyStr then
 begin
  fi := FTableInfo.FieldByName(FCombo.Text);
  if fi<>nil then
  begin
   case fi.DataType of
    ftInteger,ftSmallint,ftLargeint,ftFloat,ftAutoInc,ftCurrency,ftBoolean:
       begin
         if FCondition.Text='%A%' then
          FCondition.Text:='=';

         s :=FCondition.Text+' '+FEdit.Text+' ';
       end
   else
     if FCondition.Text='%A%' then
       s :=' LIKE ''%'+FEdit.Text+'%'''
     else
       s := FCondition.Text+' '''+FEdit.Text+'''';
   end;
  end;
  FListBox.Items.Add(FCombo.Text+s);
 end else
 begin
   ShowMessage('TextBox empty');
 end;
end;

procedure TAsQueryBuilderForm.OnBtnOkClick(Sender: TObject);
var
 I: Integer;
 app:string;
begin
 FSqlQuery:= 'SELECT * FROM '+FTableInfo.Tablename;
 if FListBox.Count>0 then
 begin
  FSqlQuery:=FSqlQuery+' WHERE ';
  for I:=0 to FListBox.Count-1 do
  begin
    if I< FListBox.Count-1 then
      app :=' AND '
    else
      app := '';

    FSqlQuery:=FSqlQuery+FListBox.Items[I]+app;
  end;

 end;
end;

procedure TAsQueryBuilderForm.SetSqlQuery(AValue: string);
begin
 if FSqlQuery=AValue then Exit;
 FSqlQuery:=AValue;
end;

constructor TAsQueryBuilderForm.Create(aTableInfo: TTableInfo);
var
 I: Integer;
 lbl:TLabel;
begin
 inherited CreateNew(nil);
 Width := 350;
 Height := 170 ;
 BorderStyle:=bsToolWindow;
 Left:=Mouse.CursorPos.x - (Width div 2);
 Top:=Mouse.CursorPos.y;
 Caption:=aTableInfo.Tablename;


 FTableInfo:=aTableInfo;

 FListBox := TListBox.Create(Self);
 FListBox.Parent:=Self;
 FListBox.Align:=alTop;
 FListBox.Height:=95;


 FPanel:=TPanel.Create(Self);
 FPanel.Parent:=Self;
 FPanel.Caption:='';
 FPanel.Height:=25;
 FPanel.Align:=alTop;

 FCondition := TComboBox.Create(FPanel);
 FCondition.Parent := FPanel;
 FCondition.Align:=alLeft;
 FCondition.Items.Add('=');
 FCondition.Items.Add('>');
 FCondition.Items.Add('<');
 FCondition.Items.Add('%A%');
 FCondition.Style:=csDropDownList;
 FCondition.ItemIndex:=0;
 FCondition.Width:=30;

 FCombo:=TComboBox.Create(FPanel);
 FCombo.Parent:=FPanel;
 FCombo.Align:=alLeft;
 FCombo.Style:=csDropDownList;
 FCombo.Width:=150;


 for I:=0 to FTableInfo.AllFields.Count-1 do
 begin
   FCombo.Items.Add(FTableInfo.AllFields[I].FieldName);
 end;
 if FCombo.Items.Count>0 then
 FCombo.ItemIndex:=0;

 FEdit := TEdit.Create(FPanel);
 FEdit.Parent := FPanel;
 FEdit.Align:=alClient;

 FAdd := TSpeedButton.Create(FPanel);
 FAdd.Parent:=FPanel;
 FAdd.Align:=alRight;
 FAdd.Caption:='+';
 FAdd.OnClick:=@OnAddClick;

 lbl := TLabel.Create(Self);
 lbl.Parent:=Self;
 lbl.Align:=alTop;
 lbl.Alignment:=taCenter;
 lbl.Caption:='Server filter';
 lbl.Font.Style:=[fsBold];


 FBtnOk := TButton.Create(Self);
 FBtnOk.Parent:=Self;
 FBtnOk.Caption:='OK';
 FBtnOk.Left:=(Self.Width div 2 ) - (FBtnOk.Width div 2);
 FBtnOk.ModalResult:=mrOK;
 FBtnOk.OnClick:=@OnBtnOkClick;
 FBtnOk.Top := Height - FBtnOk.Height - 2;


end;

{ TAsDBLookupComboBoxEdit }

function TAsDBLookupComboBoxEdit.GetField: TField;
begin
  Result := FComboBox.Field;
end;

function TAsDBLookupComboBoxEdit.GetHint: string;
begin
 Result := FEditButton.Hint;
end;

function TAsDBLookupComboBoxEdit.GetKeyField: string;
begin
 Result := FComboBox.KeyField;
end;

function TAsDBLookupComboBoxEdit.GetListField: string;
begin
 Result := FComboBox.ListField;
end;

function TAsDBLookupComboBoxEdit.GetListSource: TDataSource;
begin
 Result:=FComboBox.ListSource;
end;

function TAsDBLookupComboBoxEdit.GetOnEditButtonClick: TNotifyEvent;
begin
 Result := FEditButton.OnClick;
end;

function TAsDBLookupComboBoxEdit.GetStyle: TComboBoxStyle;
begin
 Result := FComboBox.Style;
end;

function TAsDBLookupComboBoxEdit.GetDataFieldName: string;
begin
  Result := FComboBox.DataField;
end;

function TAsDBLookupComboBoxEdit.GetDataSource: TDataSource;
begin
 Result:=FComboBox.DataSource;
end;

procedure TAsDBLookupComboBoxEdit.SetDataField(AValue: string);
begin
 FComboBox.DataField:=AValue;
end;

procedure TAsDBLookupComboBoxEdit.SetDataSource(AValue: TDataSource);
begin
 FComboBox.DataSource:=AValue;
end;

procedure TAsDBLookupComboBoxEdit.SetHint(AValue: string);
begin
 FEditButton.Hint:=AValue;
end;

procedure TAsDBLookupComboBoxEdit.SetKeyField(AValue: string);
begin
FComboBox.KeyField:= AValue;
end;

procedure TAsDBLookupComboBoxEdit.SetListField(AValue: string);
begin
FComboBox.ListField:=AValue;
end;

procedure TAsDBLookupComboBoxEdit.SetListSource(AValue: TDataSource);
begin
  FComboBox.ListSource:=AValue;
end;

procedure TAsDBLookupComboBoxEdit.SetOnEditButtonClick(AValue: TNotifyEvent);
begin
 FEditButton.OnClick:=AValue;
end;

procedure TAsDBLookupComboBoxEdit.SetStyle(AValue: TComboBoxStyle);
begin
 FComboBox.Style:=AValue;
end;

constructor TAsDBLookupComboBoxEdit.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 Caption:=' ';
 Height:=23;
 BevelOuter:=bvNone;

 FComboBox := TDBLookupComboBox.Create(Self);
 FComboBox.Parent:=Self;
 FComboBox.Align:=alClient;

 FEditButton := TSpeedButton.Create(Self);
 FEditButton.Parent := Self;
 FEditButton.Align:=alRight;
 FEditButton.Caption:='...';
end;

destructor TAsDBLookupComboBoxEdit.Destroy;
begin
 inherited Destroy;
end;

{ TAsDbBlobEdit }

procedure TAsDbBlobEdit.OnDataChanged(Sender: TObject);
begin
 if FDataLink.Field <> nil then
 begin

  if FDataLink.Field.IsNull then
  begin
    FLabel.Caption:='Empty';
  end else
  begin
    FLabel.Caption:='(BLOB)';
  end;
 end else
 begin
  FLabel.Caption:='';
 end;

end;

procedure TAsDbBlobEdit.OnOpenClick(Sender: TObject);
var
 dlg:TForm;
 btnLoad,btnSave:TButton;
 lbl:TLabel;
 dr:TModalResult;
begin
 if FDataLink.Field<>nil then
 begin

   if not FDataLink.Field.IsBlob then
   Exit;

   dlg := TForm.Create(nil);
   dlg.Width:= 165;
   dlg.Height:= 60;
   dlg.BorderStyle:=bsToolWindow;

   dlg.Left:= Mouse.CursorPos.x;
   dlg.Top:=Mouse.CursorPos.y;

   lbl := TLabel.Create(dlg);
   lbl.Parent := dlg;
   lbl.Caption:= 'Edit  ['+FDataLink.Field.FieldName+']';
   lbl.Align:=alTop;
   lbl.Layout:=tlCenter;
   lbl.Font.Style:=[fsBold];
   lbl.Alignment:=taCenter;

   btnLoad:=TButton.Create(dlg);
   btnLoad.Parent := dlg;
   btnLoad.Caption:='Add New';
   btnLoad.Left:=5;
   btnLoad.Top:= 25;
   btnLoad.ModalResult:=mrYes;

   btnSave := TButton.Create(dlg);
   btnSave.Parent := dlg;
   btnSave.Caption:='Save As';
   btnSave.ModalResult:=mrRetry;
   btnSave.Left:=btnLoad.Left+btnLoad.Width+5;
   btnSave.Top:=25;
   btnSave.Enabled := not FDataLink.Field.IsNull;

   try
      dr := dlg.ShowModal;
     if dr=mrYes then
     begin
      if FOpenDialog.Execute then
      begin
        FDataLink.Edit;
        TBlobField(FDataLink.Field).LoadFromFile(FOpenDialog.FileName);
      end;
     end else
     if dr=mrRetry then
     begin
        if FSaveDialog.Execute then
        begin
          TBlobField(FDataLink.Field).SaveToFile(FSaveDialog.FileName);
        end;
     end;

   finally
     dlg.Free;
   end;

 end;
end;

function TAsDbBlobEdit.GetField: TField;
begin
 Result := FDataLink.Field;
end;

procedure TAsDbBlobEdit.SetDataField(AValue: string);
begin
 if FDataField=AValue then Exit;
 FDataField:=AValue;
 FDataLink.FieldName:= FDataField;
end;

procedure TAsDbBlobEdit.SetDataSource(AValue: TDataSource);
begin
 if FDataSource=AValue then Exit;
 FDataSource:=AValue;
 FDataLink.DataSource:=DataSource;
end;

constructor TAsDbBlobEdit.Create(AOwner: TComponent);
begin

  inherited Create(AOwner);
  Caption:='';
  Height := 23;
  BevelInner:=bvLowered;
  Color:=clWhite;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange:=@OnDataChanged;

  FLabel:=TLabel.Create(Self);
  FLabel.Parent := Self;

  FOpenButton:=TSpeedButton.Create(Self);
  FOpenButton.Parent := Self;
  FOpenButton.OnClick:=@OnOpenClick;

  FLabel.Align:=alClient;

  FOpenButton.Align:=alRight;
  FOpenButton.Caption:='...';

  FOpenDialog:=TOpenDialog.Create(Self);
  FSaveDialog:=TSaveDialog.Create(Self);
end;

destructor TAsDbBlobEdit.Destroy;
begin
 FDataLink.Destroy;
 inherited Destroy;
end;

{ TAsDbDateEdit }

procedure TAsDbDateEdit.OnAccept(Sender: TObject; var ADate: TDateTime;
 var AcceptDate: Boolean);
begin

 if FDataLink.Field.DataType in [ftDate,ftDateTime] then
 begin
  FDataSource.Edit;
  FDataLink.Field.Value:=ADate;
 end;

end;

procedure TAsDbDateEdit.OnDataChanged(Sender: TObject);
begin
 if FDataLink.Field <> nil then
 begin
  FDateTime:= FDataLink.Field.AsDateTime;
  Text:=DateToStr(FDataLink.Field.AsDateTime);
 end else
 begin
   Text:='';
 end;
end;

function TAsDbDateEdit.GetField: TField;
begin
 Result:=FDataLink.Field;
end;

procedure TAsDbDateEdit.SetDataField(AValue: string);
begin
 if FDataField=AValue then Exit;
 FDataField:=AValue;
 FDataLink.FieldName:= AValue;
end;

procedure TAsDbDateEdit.SetDataSource(AValue: TDataSource);
begin
 if FDataSource=AValue then Exit;
 FDataSource:=AValue;
 FDataLink.DataSource:=DataSource;
end;

procedure TAsDbDateEdit.SetField(AValue: TField);
begin
 if FField=AValue then Exit;
 FField:=AValue;
end;

constructor TAsDbDateEdit.Create(AOwner: TComponent);
begin
 inherited Create(Owner);
 FDataLink := TFieldDataLink.Create;
 FDataLink.Control := Self;
 FDataLink.OnDataChange:=@OnDataChanged;
 Self.OnAcceptDate:=@OnAccept;
end;

destructor TAsDbDateEdit.Destroy;
begin
 FDataLink.Destroy;
 inherited Destroy;
end;

{ TAsDbForm }

procedure TAsDbForm.SetDataGrid(AValue: TDBGrid);
begin
 if FDataGrid=AValue then Exit;
 FDataGrid:=AValue;
end;

procedure TAsDbForm.SetControlWidth(AValue: Integer);
begin
 if FControlWidth=AValue then Exit;
 FControlWidth:=AValue;
end;

procedure TAsDbForm.SetDataNavigator(AValue: TDBNavigator);
begin
 if FDataNavigator=AValue then Exit;
 FDataNavigator:=AValue;
end;

procedure TAsDbForm.SetDataObject(AValue: TAsUIDataAcessObject);
begin
 if FDataObject=AValue then Exit;
 FDataObject:=AValue;
end;

procedure TAsDbForm.SetEditControlsPanel(AValue: TScrollBox);
begin
 if FEditControlsBox=AValue then Exit;
 FEditControlsBox:=AValue;
end;

procedure TAsDbForm.SetRefDataObjects(AValue: TAsUIDataAcessObjects);
begin
 if FRefDataObjects=AValue then Exit;
 FRefDataObjects:=AValue;
end;

procedure TAsDbForm.OnDBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
 DataCol: integer; Column: TColumn; State: TGridDrawState);
var
  OverrideDraw: boolean;
  OurDisplayString: string;
  CurrentField: TField;
  DataRow: integer;
begin
  OverrideDraw := False;

  if (gdSelected in State) then
  begin
    (Sender as TDBGrid).Canvas.Brush.Color := clHighlight;
  end
  else
  begin
    (Sender as TDBGrid).Canvas.Brush.Color := (Sender as TDBGrid).Color;
  end;

  (Sender as TDBGrid).Canvas.FillRect(Rect);

  try
    CurrentField := Column.Field;
    if CurrentField.DataType = ftMemo then
    begin
      OverrideDraw := True;
    end;
  except
    on E: Exception do
    begin
      OverrideDraw := False;
    end;
  end;

  if (gdFixed in State) then
  begin
    OverrideDraw := False;
  end;

  if OverrideDraw = False then
  begin
    (Sender as TDBGrid).DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end
  else
  begin

    OurDisplayString := '';
    if CurrentField <> nil then
    begin

      try
        OurDisplayString := CurrentField.AsString;
      except

      end;
    end;
    (Sender as TDBGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, OurDisplayString);
  end;
end;

procedure TAsDbForm.OnCleanTlbButton(Sender: Tobject);
begin
 FQuickSearchEdit.Clear;
end;

procedure TAsDbForm.OnQuickSearchChange(Sender: TObject);
var
  expr: string;
begin
  if FQuickSearchEdit.Text = EmptyStr then
  begin
    FDataObject.Query.Filtered := False;
    FDataObject.Query.Filter := EmptyStr;
  end
  else
  begin

    try
     expr := FQuickSearchFieldsCmb.Text;

     case FDataObject.Query.FieldByName(FQuickSearchFieldsCmb.Text).DataType of
       ftString, ftMemo, ftWideString, ftWideMemo, ftDate, ftDateTime: expr :=
           expr + ' =''*' + FQuickSearchEdit.Text + '*''';
       ftInteger, ftSmallint,ftLargeint, ftFloat, ftAutoInc, ftBoolean: expr :=
           expr + ' = ' + FQuickSearchEdit.Text;
     end;
     FDataObject.Query.Filtered := False;
     FDataObject.Query.Filter := expr;
     FDataObject.Query.Filtered := True;
    except
    end;

  end;
end;

procedure TAsDbForm.OnDbLookupComboEditClick(Sender: TObject);
var
 tblInfos:TTableInfos;
 frm:TAsDbForm;
 tblName:string;
begin
  if not (Sender is TSpeedButton) then
  Exit;

  tblName := (Sender as TSpeedButton).Hint;
  if tblName=EmptyStr then
  Exit;

  tblInfos := TTableInfos.Create(Self,FDbInfo);
  try
    tblInfos.AddTable(FSchema,tblName);
    frm := TAsDbForm.Create(FDbInfo,FSchema,tblInfos[0]);
    try
      frm.OpenData;
      frm.ShowModal;
    except on E:Exception do
      ShowMessage(E.Message);
    end;

  finally
    frm.Free;
    tblInfos.Free;
  end;
  FRefDataObjects.Refresh;
end;

procedure TAsDbForm.MakeEditControls;
var
 X, Y, HorCount, I,FHeight: Integer;
 dao:TAsUIDataAcessObject;
 fieldName: String;
 lbl:TLabel;
 edt:TDBedit;
 cmbLookup:TAsDBLookupComboBoxEdit;
 chk:TDBCheckBox;
 dt :TAsDbDateEdit;
 be:TAsDbBlobEdit;
 mem:TDBMemo;
 LastControlHeight: Integer;
 ik: TImportedKeyInfo;
 lastControl:TControl;
 controlName:string;
begin
  X := 10;
  Y := 10;
  HorCount:= 1;

  //Create edit controls
  for I:=0 to FTableInfo.AllFields.Count-1 do
  begin
    fieldName:= FTableInfo.AllFields[I].FieldName;
    controlName:= FTableInfo.AllFields[I].CSharpName;
    lastControl := nil;
    lbl := TLabel.Create(FEditControlsBox);
    lbl.Parent := FEditControlsBox;
    lbl.Left:= X;
    lbl.Top := Y;
    lbl.Name:='lbl'+controlName;
    lbl.AutoSize:=False;
    lbl.OptimalFill:=True;
    lbl.Caption:= fieldName;
    lbl.Width:=FControlWidth;

    //create controls if the field is not a reference to another table
    if not FTableInfo.AllFields[I].IsReference then
    begin
      case FTableInfo.AllFields[I].DataType of
      ftBoolean:
        begin
          chk := TDBCheckBox.Create(FEditControlsBox);
          chk.Name:='chk'+controlName;
          chk.DataField:= fieldName;
          chk.DataSource:=FDataObject.DataSource;
          chk.Caption:='';
          lastControl :=chk;
        end;
     ftDate,ftDateTime:
        begin
         dt := TAsDbDateEdit.Create(FEditControlsBox);
         dt.DataField:= fieldName;
         dt.DataSource := FDataObject.DataSource;
         dt.Name:= 'dt'+controlName;
         lastControl := dt;
        end;
     ftMemo,ftWideMemo:
        begin
        mem := TDBMemo.Create(FEditControlsBox);
        mem.Name:='mem'+fieldName;
        mem.DataSource := FDataObject.DataSource;
        mem.DataField:= FTableInfo.AllFields[I].FieldName;
        mem.Height:=23;
        lastControl := mem;
        end;
     ftBlob,ftOraBlob:
        begin
           be := TAsDbBlobEdit.Create(FEditControlsBox);
           be.DataField:=fieldName;
           be.DataSource:=FDataObject.DataSource;
           be.Name:='be'+controlName;
           be.Caption:='';
           lastControl := be;
        end;
     else
       begin
        edt := TDBEdit.Create(FEditControlsBox);
        edt.Name:='edt'+fieldName;
        edt.DataSource := FDataObject.DataSource;
        edt.DataField:= FTableInfo.AllFields[I].FieldName;
        lastControl := edt;
       end;
      end;//end case
    end else
    //if field is reference to another table create dblookup accordingly
    begin
      if FTableInfo.ImportedKeys.ContainsColumn(fieldName) then
      begin
        cmbLookup := TAsDBLookupComboBoxEdit.Create(FEditControlsBox);
        cmbLookup.Name:='cmb'+controlName;
        cmbLookup.OnEditButtonClick:=@OnDbLookupComboEditClick;
        ik :=FTableInfo.ImportedKeys.GetByName(fieldName);
        if FDbInfo.DbEngine=deSqlDB then
          dao := TAsUIDataAcessObject.Create(FCon,'select * from '+ik.ForeignTableName)
        else
         if  FDbInfo.DbEngine=deZeos then
          dao := TAsUIDataAcessObject.Create(FZCon,'select * from '+ik.ForeignTableName);


        FRefDataObjects.Add(dao);
        cmbLookup.ListSource := dao.DataSource;
        cmbLookup.ListField:=ik.ForeignFirstTextField;
        cmbLookup.KeyField:= ik.ForeignColumnName;
        cmbLookup.DataSource := FDataObject.DataSource;
        cmbLookup.DataField:= ik.ColumnName;
        cmbLookup.Hint:=ik.ForeignTableName; //used when clicking on editbutton
        cmbLookup.Style:=csDropDownList;
        lastControl := cmbLookup;
      end;
    end;

     //position control
     lastControl.Parent:=FEditControlsBox;
     lastControl.Top:=lbl.Top + lbl.Height + 2;
     lastControl.Left:= lbl.Left;
     if not (lastControl is TAsDbDateEdit) then
      lastControl.Width:= FControlWidth
     else
      lastControl.Width:=FControlWidth - 23;  //Because of the SpeedButton

     //Max number of DBControls horizontally
     if HorCount = 5 then
     begin
       HorCount:=0;
       X := 10;
       Y := Y + lastControl.Height +lbl.Height + 10;
       Inc(FEditRowCount,1);
     end else
     begin
       X := X + FControlWidth + FControlSpace;
     end;
     Inc(HorCount,1);
  end;

  Inc(FEditRowCount,1);
  FEditControlsBox.Height:=lastControl.Top+lastControl.Height + 20;

  if FEditControlsBox.Height >= 340 then
  FEditControlsBox.Height := 340;

end;

function TAsDbForm.MakeQuickSearchToolbar(aOwner: TComponent): TToolbar;
var
 tlb:TToolBar;
 lbl:TLabel;
 btnClean:TToolButton;
 sep1:TToolButton;
begin
 tlb := TToolBar.Create(aOwner);
 tlb.ShowCaptions:=True;

 lbl := TLabel.Create(tlb);
 lbl.AutoSize:=False;
 lbl.Caption:='Search';
 lbl.Parent:=tlb;
 lbl.Layout:=tlCenter;
 lbl.Font.Color:=clGray;

 FQuickSearchFieldsCmb:=TComboBox.Create(tlb);
 FQuickSearchFieldsCmb.Parent := tlb;
 FQuickSearchFieldsCmb.Style:=csDropDownList;
 FQuickSearchFieldsCmb.Width:=FControlWidth;

 sep1 := TToolButton.Create(tlb);
 sep1.Style:=tbsSeparator;
 sep1.Parent:=tlb;

 FQuickSearchEdit := TEdit.Create(tlb);
 FQuickSearchEdit.Parent := tlb;
 FQuickSearchEdit.Width:=FControlWidth;
 FQuickSearchEdit.OnChange:=@OnQuickSearchChange;

 btnClean := TToolButton.Create(tlb);
 btnClean.Parent := tlb;
 btnClean.Caption:='x';
 btnClean.OnClick:=@OnCleanTlbButton;

 Result := tlb;

end;

procedure TAsDbForm.SetSqlQuery(AValue: string);
begin
 if FSqlQuery=AValue then Exit;
 FSqlQuery:=AValue;
end;

function TAsDbForm.GetTopRecordSelect(TopRecords: Integer): string;
begin
 case FDbInfo.DatabaseType of
  dtMsSql:Result :='SELECT TOP '+IntToStr(TopRecords)+' * FROM ['+FTableInfo.Tablename+']';
  dtOracle:Result := 'SELECT * FROM "'+FTableInfo.Tablename+'" WHERE ROWNUM <= '+IntToStr(TopRecords);
  dtMySql,dtSQLite: Result:='SELECT * FROM '+FTableInfo.Tablename+' LIMIT '+IntToStr(TopRecords);
  dtFirebirdd: Result:='SELECT FIRST '+IntToStr(TopRecords)+' * FROM '+FTableInfo.Tablename;
 end;
end;


constructor TAsDbForm.Create(aDbInfo: TDbConnectionInfo; aSchema: string;
 aTableInfo: TTableInfo);
var
  grp:TGroupBox;
begin
 inherited CreateNew(nil);


 FSchema:= aSchema;
 FControlWidth:= 150;
 FControlSpace:= 5;
 FEditRowCount:=1;
 BorderStyle:= bsSingle;
 Caption:=aTableInfo.Tablename;

 Position := poScreenCenter;
 Height:= 550;
 Width:= (5 * FControlWidth) + (5 * FControlSpace) + 30;

 FDbInfo:=aDbInfo;
 FTableInfo := aTableInfo;
 FSqlQuery:= 'Select * from '+FTableInfo.Tablename;

 if FDbInfo.DbEngine=deSqlDB then
 begin
  FCon := FDbInfo.ToSqlConnector;
  FDataObject := TAsUIDataAcessObject.Create(FCon,FSqlQuery);
 end else
 if FDbInfo.DbEngine=deZeos then
 begin
  FZCon := FDbInfo.ToZeosConnection;
  FDataObject := TAsUIDataAcessObject.Create(FZCon,FSqlQuery);
 end;

 FRefDataObjects := TAsUIDataAcessObjects.Create;

 FEditControlsBox := TScrollBox.Create(Self);
 FEditControlsBox.Parent := Self;
 FEditControlsBox.Align:=alTop;
 FEditControlsBox.HorzScrollBar.Visible:=False;

 MakeEditControls;

 FGridNavGroup := TGroupBox.Create(Self);
 FGridNavGroup.Parent := Self;
 FGridNavGroup.Align:=alClient;
 FGridNavGroup.Name:='grpData';
 FGridNavGroup.Caption:='';


 grp := TGroupBox.Create(FGridNavGroup);
 grp.Parent := FGridNavGroup;
 grp.Name:='grpSearch';
 grp.Caption:='';
 grp.Align:=alTop;
 grp.Height:=45;

 FQuickSearchToolbar := MakeQuickSearchToolbar(grp);
 FQuickSearchToolbar.Parent := grp;

 FDataNavigator := TDBNavigator.Create(FGridNavGroup);
 FDataNavigator.Parent := FGridNavGroup;
 FDataNavigator.Align:=alTop;
 FDataNavigator.DataSource := FDataObject.DataSource;

 FDataGrid := TDBGrid.Create(FGridNavGroup);
 FDataGrid.Parent := FGridNavGroup;
 FDataGrid.Align:=alClient;
 FDataGrid.DataSource := FDataObject.DataSource;
 FDataGrid.Options:=FDataGrid.Options+[dgRowSelect];
 FDataGrid.TitleStyle:=tsNative;
 FDataGrid.OnDrawColumnCell:=@OnDBGridDrawColumnCell;
end;

destructor TAsDbForm.Destroy;
begin
  if FCon<>nil then
  FCon.Destroy;

  FDataObject.Destroy;
  FRefDataObjects.Destroy;
 inherited Destroy;
end;

procedure TAsDbForm.OpenData;
var
 I: Integer;
 lst:TStringList;
begin
  if FRefDataObjects.OpenAll then
  begin
    FDataObject.SqlQuery:=FSqlQuery;
    FDataObject.Query.Open;
    for I:=0 to FDataGrid.Columns.Count-1 do
    begin
      FDataGrid.Columns[I].Width:=FControlWidth;
    end;

    if FQuickSearchFieldsCmb <> nil then
    try
      lst := TStringList.Create;
      TDbUtils.GetColumnNames(FDbInfo,FTableInfo.Tablename,lst);
      FQuickSearchFieldsCmb.Clear;
      FQuickSearchFieldsCmb.Items.AddStrings(lst);
      if FQuickSearchFieldsCmb.Items.Count>0 then
        FQuickSearchFieldsCmb.ItemIndex:=0;
    finally
      lst.Free;
    end;

  end else
  begin
    ShowMessage(FRefDataObjects.LastError);
  end;
end;

function TAsDbForm.ShowModal(FormFilter: TAsDbFormFilter): TModalResult;

var
 frm:TAsQueryBuilderForm;
 f:TForm;
 ok:TButton;
 se:TSpinEdit;
 lbl:TLabel;
 OpenForm:Boolean;
begin
 OpenForm:=False;

 case FormFilter of
  dffCustomFilter:
     begin
        frm := TAsQueryBuilderForm.Create(FTableInfo);
        try
          if frm.ShowModal = mrOK then
          begin
            FSqlQuery:=frm.SqlQuery;
            OpenForm := True;
          end;
        finally
          frm.Free;
        end;
     end;
  dffTopRecords:
     begin
       f := TForm.Create(nil);
       f.Height:=60;
       f.Width:= 120;
       f.BorderStyle:=bsToolWindow;
       f.Left:=Mouse.CursorPos.x - f.Width div 2;
       f.Top:=Mouse.CursorPos.y;

       se:=TSpinEdit.Create(f);
       se.Parent := f;
       se.Align:=alClient;
       se.MaxValue:=999999999;
       se.Value:=200;

       ok := TButton.Create(f);
       ok.Parent:=f;
       ok.ModalResult:=mrOK;
       ok.Align:=alBottom;
       ok.Caption:='OK';

       lbl:=TLabel.Create(f);
       lbl.Parent:=f;
       lbl.Align:=alTop;
       lbl.Alignment:=taCenter;
       lbl.Caption:='Limit records to ';
       lbl.Font.Style:=[fsBold];

       try
        if f.ShowModal = mrOK then
        begin
          OpenForm:= True;
          FSqlQuery:=GetTopRecordSelect(se.Value);
        end;
       finally
         f.Free;
       end;
     end;
  dffNone: OpenForm:=True;
 end;

 if OpenForm then
 begin
  OpenData;
  Result := inherited ShowModal
 end;

end;

{ TAsUIDataAcessObject }

procedure TAsUIDataAcessObject.SetDataSource(AValue: TDataSource);
begin
 if FDataSource=AValue then Exit;
 FDataSource:=AValue;
end;

function TAsUIDataAcessObject.GetQueryComponent: TDataSet;
begin
 case FDBengine of
  deSqlDB: Result := FQuery;
  deZeos:Result:=FZQuery;
 end;
end;



procedure TAsUIDataAcessObject.SetName(AValue: string);
begin
 if FName=AValue then Exit;
 FName:=AValue;
end;

procedure TAsUIDataAcessObject.SetQuery(AValue: TSQLQuery);
begin
 if FQuery=AValue then Exit;
 FQuery:=AValue;
end;

procedure TAsUIDataAcessObject.AfterPost(DataSet: TDataSet);
begin
 {some tables give errors on TMSSQLConnection and don't update  (but Zeos doesn't fail on those tables)}
 FQuery.ApplyUpdates;
end;

procedure TAsUIDataAcessObject.AfterDelete(DataSet: TDataSet);
begin
 FQuery.ApplyUpdates;
end;

procedure TAsUIDataAcessObject.SetSqlQuery(AValue: string);
begin
 if FSqlQuery=AValue then Exit;
 FSqlQuery:=AValue;

 case FDBengine of
  deZeos:FZQuery.SQL.Text:=FSqlQuery;
  deSqlDB:FQuery.SQL.Text:=FSqlQuery;
 end;


end;

constructor TAsUIDataAcessObject.Create(SqlCon: TSQLConnector; aSqlQuery: string
 );
begin
  inherited Create(nil);
  FSqlQuery:= aSqlQuery;
  FSqlCon := SqlCon;
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := FSqlCon;
  FQuery.AfterPost:=@AfterPost;
  FQuery.AfterDelete:=@AfterDelete;
  FQuery.SQL.Text:=FSqlQuery;
  FQuery.FilterOptions:=[foCaseInsensitive];
  FDataSource := TDataSource.Create(nil);
  FDataSource.DataSet := FQuery;
  FDBengine:= deSqlDB;
end;

constructor TAsUIDataAcessObject.Create(ZCon: TZConnection; aSqlQuery: string);
begin
  inherited Create(nil);
  FSqlQuery:= aSqlQuery;
  FZCon := ZCon;
  FZQuery := TZQuery.Create(nil);
  FZQuery.Connection := FZCon;
  FZQuery.SQL.Text:=FSqlQuery;
  FZQuery.FilterOptions:=[foCaseInsensitive];
  FDataSource := TDataSource.Create(nil);
  FDataSource.DataSet := FZQuery;
  FDBengine:= deZeos;
end;

destructor TAsUIDataAcessObject.Destroy;
begin
  if FDBengine = deSqlDB then
  FQuery.Destroy;

  If FDBengine = deZeos then
  FZQuery.Destroy;

  FDataSource.Destroy;

  inherited Destroy;

end;

procedure TAsUIDataAcessObject.Open;
begin
 FQuery.Open;
end;

procedure TAsUIDataAcessObject.Close;
begin
 FQuery.Close;
end;

{ TAsUIDataAcessObjects }

function TAsUIDataAcessObjects.FindByName(Name: string): TAsUIDataAcessObject;
var
 I: Integer;
begin
 for I:=0 to Count-1 do
 begin
   if Self.Items[I].Name = Name then
   begin
     Result := Items[I];
     Break;
   end;
 end;
end;

function TAsUIDataAcessObjects.OpenAll: Boolean;
var
 I: Integer;
begin
 Result := True;
 for I:=0 to Count-1 do
 begin
   try
    Items[I].Query.Open;
   except ON E:Exception do
    begin
      Result := False;
      FLastError := 'Error opening ['+Items[I].Name+ '] '+  E.Message;
      Break;
    end;
   end;
 end;
end;

function TAsUIDataAcessObjects.CloseAll: Boolean;
var
 I: Integer;
begin
 Result := True;
 for I:=0 to Count-1 do
 begin
   try
    Items[I].Query.Close;
   except ON E:Exception do
      begin
        Result := False;
        FLastError := 'Error closing ['+Items[I].Name+ '] '+  E.Message;
        Break;
      end;
   end;
 end;
end;

procedure TAsUIDataAcessObjects.Refresh;
var
 I: Integer;
begin

 for I:=0 to Count-1 do
 begin
   try
    Items[I].Query.Refresh;
   except ON E:Exception do
      begin
        FLastError := 'Error refreshing ['+Items[I].Name+ '] '+  E.Message;
        Break;
      end;
   end;
 end;
end;

end.

