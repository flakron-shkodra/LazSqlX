{*******************
Flakron Shkodra
  Version 1 (23.10.2014)

  mod 1.1 (31.10.2014)
    -added LazReport->FrPrintGrid
    -fix edt.Name as controlName
  mod 1.2 (20.11.2014)
   -Set to use TAsQuery a
   -other tweaks
******************}

unit AsDbFormUtils;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils,Forms,Graphics,Dialogs,Controls,StdCtrls,ExtCtrls,ComCtrls, DbCtrls,DBGrids,Grids, AsTableInfo,AsDbType,
 DB, sqldb, fgl,Spin, EditBtn,Buttons,mssqlconn,{$ifndef win64}oracleconnection,{$endif} IBConnection,
 mysql55conn, mysql51conn, mysql50conn, mysql40conn, mysql41conn, sqlite3conn,ZConnection,ZDataset,LR_PGrid,
 CheckLst,LazSqlXResources,DBDateTimePicker,DateTimePicker;

type

  { TAsUIDataAcessObject }

  TAsUIDataAcessObject = class(TComponent)
  private
   FDBengine: TAsDatabaseEngineType;
    FOwner:TComponent;
    FDataSource: TDataSource;
    FName: string;
    FSqlQuery:string;
    FQuery:TAsQuery;
    FDBInfo:TAsDbConnectionInfo;
    function GetQueryComponent: TDataSet;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetName(AValue: string);
    procedure SetQuery(AValue: TAsQuery);
    procedure SetSqlQuery(AValue: string);
  public
    constructor Create(DbInfo:TAsDbConnectionInfo; aSqlQuery:string);
    destructor Destroy;override;
    procedure Open;
    procedure Close;
  published
    property Name:string read FName write SetName;
    property Query:TAsQuery read FQuery;
    property DataSource:TDataSource read FDataSource write SetDataSource;
    property DbEngine:TAsDatabaseEngineType read FDBengine;
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

  TAsDBSpinEdit = class(TCustomFloatSpinEdit)
  private
   FDataField: string;
    FDataLink:TFieldDataLink;
    FDataSource: TDataSource;
    FMaxValue: Double;
    FValuePassedFromSource:Boolean;
    function GetField: TField;
    procedure SetDataField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure OnDataChange(Sender: TObject);
    procedure SetMaxValue(AValue: Double);
    procedure SpinOnchange(Sender: TObject);
    procedure OnActiveChange(Sender: TObject);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  published
    property DataSource:TDataSource read FDataSource write SetDataSource;
    property DataField:string read FDataField write SetDataField;
    property Field:TField read GetField;
    property MaximumValue:Double read FMaxValue write SetMaxValue;
  end;

  { TAsQueryBuilderForm }

  TAsQueryBuilderForm = class(TForm)
  private
   FSqlQuery: string;
    FTableInfo:TAsTableInfo;
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
    constructor Create(aTableInfo:TAsTableInfo);
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
   FDbInfo:TAsDbConnectionInfo;
   FEditControlsBox: TScrollBox;
   FGridNavGroup:TGroupBox;
   FRefDataObjects: TAsUIDataAcessObjects;
   FTableInfo:TAsTableInfo;
   FFRPrintGrid:TfrPrintGrid;
   FSchema:string;//used only when AsDbLookupComboEdit (table reference field is clicked for edit '..')
   FSqlQuery:string;
   FEditRowCount:Integer;
   FControlColumns:Integer;
   FQuickSearchToolbar:TToolbar;
   FQuickSearchFieldsCmb:TComboBox;//created from MakeQuickSearchToolbar (populated from OpenData)
   FQuickSearchEdit:TEdit;//created from MakeQuickSearchToolbar (used in OnQuickSearchChange)
   FShowColumnsButton:TToolButton; //created from MakeQuickSearchToolbar (accessed also from OnGridColumnListExit)
   FColumnsList:TCheckListBox;
   procedure AfterEdit(DataSet:TDataSet);
   procedure AfterInsert(DataSet:TDataSet);
   procedure AfterPost(DataSet:TDataSet);
   procedure AfterCancel(DataSet:TDataSet);
   procedure OnFrPrintGridGetValue(const ParName: String; var ParValue: Variant);
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
   procedure OnPrintGridButtonClick(Sender:TObject);
   procedure OnGridColumsListClick(Sender: TObject; Index: integer);
   procedure OnGridColumnListExit(Sender: TObject);
   procedure OnFormResize(Sender:TObject);
   procedure ShowColumnsButtonClick(Sender:TObject);
   procedure MakeEditControls;
   procedure RepositionEditControls;
   procedure ClearEditControls;
   function MakeQuickSearchToolbar(aOwner:TComponent):TToolbar;
   procedure SetSqlQuery(AValue: string);
   procedure FillColumnList;
  public
    constructor Create(aDbInfo:TAsDbConnectionInfo; aSchema:string; aTableInfo:TAsTableInfo);
    destructor Destroy;override;
    procedure OpenData;
    procedure CloseData;//if this is not called after form is used, SQLDB with MsSql can cause troubles
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
 fi:TAsFieldInfo;
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

constructor TAsQueryBuilderForm.Create(aTableInfo: TAsTableInfo);
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

procedure TAsDbForm.OnGridColumsListClick(Sender: TObject; Index: integer);
begin
 if FColumnsList.ItemIndex>-1 then
 begin
   FDataGrid.Columns.Items[FColumnsList.ItemIndex].Visible:= FColumnsList.Checked[FColumnsList.ItemIndex]
 end;
end;

procedure TAsDbForm.ShowColumnsButtonClick(Sender: TObject);
begin
 FColumnsList.Left:=FDataGrid.Width-FColumnsList.Width-5;
 FColumnsList.Top:=FGridNavGroup.Top+60;
 FColumnsList.Height:=FDataGrid.Height;
 FColumnsList.Visible:=not FColumnsList.Visible;

 if (Sender is TToolButton) then
 (Sender as TToolButton).Down:=FColumnsList.Visible;

 if FColumnsList.Visible then
 FColumnsList.SetFocus;
end;

procedure TAsDbForm.OnGridColumnListExit(Sender: TObject);
begin
 FShowColumnsButton.Down:=False;
 FColumnsList.Visible:=False;
end;

procedure TAsDbForm.OnFormResize(Sender: TObject);
begin
  RepositionEditControls;
end;

procedure TAsDbForm.AfterEdit(DataSet: TDataSet);
begin
 FEditControlsBox.Enabled:=True;
end;

procedure TAsDbForm.AfterInsert(DataSet: TDataSet);
begin
 FEditControlsBox.Enabled:=True;
end;

procedure TAsDbForm.AfterPost(DataSet: TDataSet);
begin
 FEditControlsBox.Enabled:=False;
 FDataObject.FQuery.ApplyUpdates;
end;

procedure TAsDbForm.AfterCancel(DataSet: TDataSet);
begin
 FEditControlsBox.Enabled:=False;
end;

procedure TAsDbForm.OnFrPrintGridGetValue(const ParName: String;
 var ParValue: Variant);
begin
 if ParName='title' then ParValue:= FTableInfo.Tablename;
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
 tblInfos:TAsTableInfos;
 frm:TAsDbForm;
 tblName:string;
begin
  if not (Sender is TSpeedButton) then
  Exit;

  tblName := (Sender as TSpeedButton).Hint;
  if tblName=EmptyStr then
  Exit;

  tblInfos := TAsTableInfos.Create(Self,FDbInfo);
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

procedure TAsDbForm.OnPrintGridButtonClick(Sender: TObject);
begin
 FFRPrintGrid.PreviewReport;
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
 dt :TDBDateTimePicker;
 sed:TAsDBSpinEdit;
 be:TAsDbBlobEdit;
 mem:TDBMemo;
 LastControlHeight: Integer;
 ik: TAsImportedKeyInfo;
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
         dt := TDBDateTimePicker.Create(FEditControlsBox);
         dt.Name:= 'dt'+controlName;
         dt.DataField:= fieldName;
         dt.DataSource := FDataObject.DataSource;
         dt.AutoSize:= False;
         dt.Kind:=dtkDateTime;
         lastControl := dt;
        end;
     ftMemo,ftWideMemo:
        begin
        mem := TDBMemo.Create(FEditControlsBox);
        mem.Name:='mem'+controlName;
        mem.DataSource := FDataObject.DataSource;
        mem.DataField:= FTableInfo.AllFields[I].FieldName;
        mem.Height:=23;
        lastControl := mem;
        end;
     ftBlob,ftOraBlob:
        begin
           be := TAsDbBlobEdit.Create(FEditControlsBox);
           be.Name:='be'+controlName;
           be.DataField:=fieldName;
           be.DataSource:=FDataObject.DataSource;
           be.Caption:='';
           lastControl := be;
        end;
      ftInteger,ftWord, ftLargeint,ftSmallint,ftFloat,ftCurrency,ftBCD:
        begin
           sed := TAsDBSpinEdit.Create(FEditControlsBox);
           sed.Name:='se'+controlName;
           sed.DataField:=fieldName;
           sed.DataSource:=FDataObject.DataSource;
           lastControl:=sed;
        end;
     else
       begin
        edt := TDBEdit.Create(FEditControlsBox);
        edt.Name:='edt'+controlName;
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
        dao := TAsUIDataAcessObject.Create(FDbInfo,'select * from '+TAsDbUtils.SafeWrap(FDbInfo.DbType,ik.ForeignTableName));
        FRefDataObjects.Add(dao);
        cmbLookup.ListSource := dao.DataSource;
        cmbLookup.ListField:=ik.ForeignFirstTextField;
        cmbLookup.KeyField:= ik.ForeignColumnName;
        cmbLookup.DataSource := FDataObject.DataSource;
        cmbLookup.DataField:= ik.ColumnName;
        cmbLookup.Hint:=ik.ForeignTableName; //used when clicking on editbutton
        cmbLookup.Style:=csDropDownList;
        lastControl := cmbLookup;
      end else
      begin
        edt := TDBEdit.Create(FEditControlsBox);
        edt.Name:='edt'+controlName;
        edt.DataSource := FDataObject.DataSource;
        edt.DataField:= FTableInfo.AllFields[I].FieldName;
        lastControl := edt;
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

procedure TAsDbForm.RepositionEditControls;
var
 X, Y, HorCount, I,FHeight : Integer;
 dao:TAsUIDataAcessObject;
 fieldName: String;
 lbl:TLabel;
 edt:TDBedit;
 cmbLookup:TAsDBLookupComboBoxEdit;
 chk:TDBCheckBox;
 dt :TDBDateTimePicker;
 be:TAsDbBlobEdit;
 sed:TAsDBSpinEdit;
 mem:TDBMemo;
 LastControlHeight: Integer;
 ik: TAsImportedKeyInfo;
 lastControl:TControl;
 controlName:string;
 howMany, K:integer;
 refSql, csharpName: String;
 refObj: TAsTableInfo=nil;
 ft: TFieldType;
begin

  X := 5;
  Y := 5;
  HorCount:= 1;
  FControlColumns :=Width div (FControlWidth +  FControlSpace);

  //Reposition edit controls
  for I:=0 to FTableInfo.AllFields.Count-1 do
  begin

    fieldName:= FTableInfo.AllFields[I].FieldName;
    controlName:= EmptyStr;
    csharpName := FTableInfo.AllFields[I].CSharpName;
    lastControl := nil;
    lbl := FEditControlsBox.FindChildControl('lbl'+csharpName) as TLabel;

    if lbl=nil then
    continue;

    lbl.Parent := FEditControlsBox;
    lbl.Left:= X;
    lbl.Top := Y;
    lbl.Width:=FControlWidth;

    if not FTableInfo.AllFields[I].IsReference then
    begin
      ft := FTableInfo.AllFields[I].DataType;
      case FTableInfo.AllFields[I].DataType of
        ftBoolean: controlName := 'chk'+csharpName;
        ftDate,ftDateTime: controlName := 'dt'+csharpName;
        ftMemo,ftWideMemo: controlName := 'mem'+csharpName;
        ftBlob,ftOraBlob:  controlName := 'be'+csharpName;
        ftInteger,ftWord, ftLargeint,ftSmallint,ftFloat,ftCurrency,ftBCD: controlName := 'se'+csharpName;
        else controlName := 'edt'+csharpName;
      end;//end case
    end else
    begin
      if FTableInfo.ImportedKeys.ContainsColumn(fieldName) then
        controlName := 'cmb'+csharpName;
    end;

    lastControl := FEditControlsBox.FindChildControl(controlName);

    if lastControl<>nil then
    begin

    lastControl.Top:=lbl.Top + lbl.Height + 2;
    lastControl.Left:= lbl.Left;
    lastControl.Width:= FControlWidth;

     //Max number of DBControls horizontally
     if HorCount = FControlColumns then
     begin
       HorCount:=0;
       X := 5;
       Y := Y + lastControl.Height +lbl.Height + 10;
       Inc(FEditRowCount,1);
     end else
     begin
       X := X + FControlWidth + FControlSpace;
     end;
     Inc(HorCount,1);


      Inc(FEditRowCount,1);
      FEditControlsBox.Height:=lastControl.Top+lastControl.Height + 20;

      if FEditControlsBox.Height >= 340 then
      FEditControlsBox.Height := 340;

    end;

  end;

end;

procedure TAsDbForm.ClearEditControls;
begin
 while FEditControlsBox.ControlCount>0 do
  FEditControlsBox.Controls[0].Free;
end;

function TAsDbForm.MakeQuickSearchToolbar(aOwner: TComponent): TToolbar;
var
 tlb:TToolBar;
 lbl:TLabel;
 btnClean,btnPrint:TToolButton;
 sep1:TToolButton;
begin
 tlb := TToolBar.Create(aOwner);
 tlb.ShowCaptions:=True;
 tlb.ShowHint:=True;

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
 btnClean.Hint:='Clear search box';
 btnClean.OnClick:=@OnCleanTlbButton;


 sep1 := TToolButton.Create(tlb);
 sep1.Style:=tbsSeparator;
 sep1.Parent:=tlb;

 btnPrint := TToolButton.Create(tlb);
 btnPrint.Parent := tlb;
 btnPrint.Caption:='Print';
 btnPrint.Hint:='Prints data grid';
 btnPrint.OnClick:=@OnPrintGridButtonClick;

 sep1 := TToolButton.Create(tlb);
 sep1.Style:=tbsSeparator;
 sep1.Parent:=tlb;

 FShowColumnsButton := TToolButton.Create(tlb);
 FShowColumnsButton.Parent := tlb;
 FShowColumnsButton.Caption:='Columns';
 FShowColumnsButton.Hint:='Shows/Hides grid columns (for printing)';
 FShowColumnsButton.OnClick:=@ShowColumnsButtonClick;

 Result := tlb;

end;

procedure TAsDbForm.SetSqlQuery(AValue: string);
begin
 if FSqlQuery=AValue then Exit;
 FSqlQuery:=AValue;
end;

procedure TAsDbForm.FillColumnList;
var
 I: Integer;
begin
 FColumnsList.Clear;
 for I:=0 to FTableInfo.AllFields.Count-1 do
 begin
   FColumnsList.Items.Add(FTableInfo.AllFields[I].FieldName);
 end;
 FColumnsList.CheckAll(cbChecked);
end;


constructor TAsDbForm.Create(aDbInfo: TAsDbConnectionInfo; aSchema: string;
 aTableInfo: TAsTableInfo);
var
  grp:TGroupBox;
begin
 inherited CreateNew(nil);


 FSchema:= aSchema;
 FControlWidth:= 150;
 FControlSpace:= 5;
 FEditRowCount:=1;
 BorderStyle:= bsSizeable;
 Caption:=aTableInfo.Tablename;
 OnResize:=@OnFormResize;

 Position := poScreenCenter;
 Height:= 550;
 Width:= (5 * FControlWidth) + (5 * FControlSpace) + 30;

 if aDbInfo.DbType<>dtSQLite then
 begin
  FDbInfo:=TAsDbConnectionInfo.Create(False);
  FDbInfo.Assign(aDbInfo);
 end else
 FDbInfo := aDbInfo;

 FTableInfo := aTableInfo;
 FSqlQuery:= 'Select * from '+TAsDbUtils.SafeWrap(FDbInfo.DbType, FTableInfo.Tablename);

 FDataObject := TAsUIDataAcessObject.Create(FDbInfo,FSqlQuery);
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
 FDataGrid.Font.Size:=10;
 FDataGrid.DataSource := FDataObject.DataSource;
 FDataGrid.Options:=FDataGrid.Options+[dgRowSelect];
 FDataGrid.TitleStyle:=tsNative;
 FDataGrid.OnDrawColumnCell:=@OnDBGridDrawColumnCell;

 FFRPrintGrid := TfrPrintGrid.Create(Self);
 FFRPrintGrid.DBGrid := FDataGrid;
 FFRPrintGrid.Caption:= FTableInfo.Tablename;
 FFRPrintGrid.ShowCaption:=True;
 FFRPrintGrid.ShowProgress:=True;
 FFRPrintGrid.OnGetValue:=@OnFrPrintGridGetValue;
 FFRPrintGrid.Font.Size:=10;
 FFRPrintGrid.TitleFont.Size:=10;
 if TLazSqlXResources.ReportTemplatePath<>EmptyStr then
 FFRPrintGrid.Template:= TLazSqlXResources.ReportTemplatePath;


 FColumnsList := TCheckListBox.Create(Self);
 FColumnsList.Parent := Self;
 FColumnsList.Visible:= False;
 FColumnsList.OnItemClick:=@OnGridColumsListClick;
 FColumnsList.OnExit:=@OnGridColumnListExit;
 FColumnsList.Width:=200;
 FillColumnList;
end;

destructor TAsDbForm.Destroy;
begin
  ClearEditControls;
  FDataObject.Destroy;
  FRefDataObjects.Destroy;
  //only sqlite won't create new connection to avoid dbLock
  if FDbInfo.DbType<>dtSQLite then
  FDbInfo.Free;

 inherited Destroy;
end;

procedure TAsDbForm.OpenData;
var
 I: Integer;
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
    begin
      FQuickSearchFieldsCmb.Clear;
      for I:= 0 to FTableInfo.AllFields.Count-1 do
      begin
        FQuickSearchFieldsCmb.Items.Add(FTableInfo.AllFields[I].FieldName);
      end;
      if FQuickSearchFieldsCmb.Items.Count>0 then
        FQuickSearchFieldsCmb.ItemIndex:=0;
    end;


  end else
  begin
    ShowMessage(FRefDataObjects.LastError);
  end;
end;

procedure TAsDbForm.CloseData;
begin
 FRefDataObjects.CloseAll;
 FDataObject.Close;
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
          FSqlQuery:= TAsDbUtils.GetTopRecordsSelect(FDbInfo.DbType,FSchema, FTableInfo.Tablename, se.Value);
        end;
       finally
         f.Free;
       end;
     end;
  dffNone: OpenForm:=True;
 end;

 if OpenForm then
 begin
  try
    OpenData;
  except on E:Exception do
   ShowMessage(E.Message);
  end;
  Result := inherited ShowModal
 end;

end;

{ TAsDBSpinEdit }

procedure TAsDBSpinEdit.OnDataChange(Sender: TObject);
begin
 FValuePassedFromSource:=True;
 if FDataLink.Field<> nil then
 begin
  if not FDataLink.Field.IsNull then
    Value:=FDataLink.Field.Value
  else
    Value:=0;
 end;
 FValuePassedFromSource:=False;
end;

procedure TAsDBSpinEdit.SetMaxValue(AValue: Double);
begin
 if FMaxValue=AValue then Exit;
 FMaxValue:=AValue;

 MaxValue:=FMaxValue;
end;



procedure TAsDBSpinEdit.SpinOnchange(Sender: TObject);
begin
 if not FValuePassedFromSource then
 if FDataLink.Field.Value <> Value then
 if FDataLink.Edit then
 begin
  FDataLink.Field.Value:=Value;
 end;
end;

procedure TAsDBSpinEdit.OnActiveChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
 case FDataLink.Field.DataType of
  ftInteger,ftLargeint,ftSmallint: DecimalPlaces:=0;
  ftFloat: DecimalPlaces:=2;
  ftCurrency: DecimalPlaces:= 4;
 end;
end;

function TAsDBSpinEdit.GetField: TField;
begin
 Result := FDataLink.Field;
end;

procedure TAsDBSpinEdit.SetDataField(AValue: string);
begin
 if FDataField=AValue then Exit;
 FDataField:=AValue;
 FDataLink.FieldName:= FDataField;


end;

procedure TAsDBSpinEdit.SetDataSource(AValue: TDataSource);
begin
 if FDataSource=AValue then Exit;
 FDataSource:=AValue;
 FDataLink.DataSource := FDataSource;
end;

constructor TAsDBSpinEdit.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FDataLink := TFieldDataLink.Create;
 FDataLink.Control := Self;
 FDataLink.OnDataChange:=@OnDataChange;
 FDataLink.OnActiveChange:=@OnActiveChange;
 OnChange:=@SpinOnchange;
 MaxValue:=9999999999;
 MinValue:=-9999999999;
end;

destructor TAsDBSpinEdit.Destroy;
begin
 FDataLink.Free;
 inherited Destroy;
end;

{ TAsUIDataAcessObject }

procedure TAsUIDataAcessObject.SetDataSource(AValue: TDataSource);
begin
 if FDataSource=AValue then Exit;
 FDataSource:=AValue;
end;

function TAsUIDataAcessObject.GetQueryComponent: TDataSet;
begin
 Result:= FQuery.DataSet;
end;



procedure TAsUIDataAcessObject.SetName(AValue: string);
begin
 if FName=AValue then Exit;
 FName:=AValue;
end;

procedure TAsUIDataAcessObject.SetQuery(AValue: TAsQuery);
begin
 if FQuery=AValue then Exit;
 FQuery:=AValue;
end;

procedure TAsUIDataAcessObject.SetSqlQuery(AValue: string);
begin
 if FSqlQuery=AValue then Exit;
 FSqlQuery:=AValue;

 FQuery.SQL.Text:=FSqlQuery;
end;

constructor TAsUIDataAcessObject.Create(DbInfo: TAsDbConnectionInfo;
 aSqlQuery: string);
begin
 inherited Create(nil);
 FDBInfo := DbInfo;
 FSqlQuery:= aSqlQuery;
 FQuery := TAsQuery.Create(FDBInfo);
 FQuery.SQL.Text:=aSqlQuery;
 FDataSource := TDataSource.Create(nil);


 FDataSource.DataSet := FQuery.DataSet;
 FQuery.FilterOptions:= [foCaseInsensitive];
end;

destructor TAsUIDataAcessObject.Destroy;
begin
  FQuery.Destroy;
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

