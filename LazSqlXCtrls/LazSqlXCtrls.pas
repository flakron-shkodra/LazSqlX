unit LazSqlXCtrls;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics,Dialogs, sqldb, db,
 ZDataset, ZConnection, DbCtrls, DBGrids, SynEdit, SynCompletion, SynEditTypes,
 Grids, Menus, AsStringUtils, DbType, Utils, SynHighlighterSQL, Types, strutils,
 LCLType,MouseAndKeyInput,SqlExecThread, SqlGenerator;


type


  { TLazSqlXTabSheet }

  TLazSqlXScanNeeded = procedure (ScanText:string) of object;
  TLazSqlXLastWordChanged = procedure (var LastWord:string) of object;

  TLazSqlXTabSheet = class(TTabSheet)
  private
   FMessage: string;
   FOnExecutionFinished: TOnSqlExecThreadFinish;
   FOnLastWordChanged: TLazSqlXLastWordChanged;
   FOnTextScanNeeded: TLazSqlXScanNeeded;
    FTopPanel, FBottomPanel: TPanel;
    FDataGrid: TDBGrid;
    FDBNavigator: TDBNavigator;
    FDataSource: TDataSource;
    FQuery: TSQLQuery;
    FQueryEditor: TSynEdit;
    FSplitter: TSplitter;
    FErrorMemo: TMemo;
    FNumbering: string;
    FZQuery: TZQuery;
    FParent:TPageControl;
    FCurrentExecutor:TSqlExecThread;
    FExecutionInProgress:Boolean;
    FEditMode:Boolean;
    function GetDbInfo:TDbConnectionInfo;
    function GetHasActiveData: Boolean;
    function GetSqlQuery: string;
    procedure OnDBGridDrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure OnQueryEditorChange(Sender: TObject);
    procedure OnQueryEditorKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnQueryEditorKeyPress(Sender: TObject; var Key: char);
    procedure OnQueryEditorKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnQueryEditorPaste(Sender: TObject; var AText: string;
      var AMode: TSynSelectionMode; ALogStartPos: TPoint;
      var AnAction: TSynCopyPasteAction);
    procedure OnQyeryAfterPost(DataSet:TDataSet);
    procedure SetDataGrid(AValue: TDBGrid);
    procedure SetMessage(AValue: string);
    procedure SetOnExecutionFinished(AValue: TOnSqlExecThreadFinish);
    procedure SetOnLastWordChanged(AValue: TLazSqlXLastWordChanged);
    procedure SetOnTextScanNeeded(AValue: TLazSqlXScanNeeded);
    procedure SetQuery(AValue: TSQLQuery);
    procedure SetQueryEditor(AValue: TSynEdit);
    procedure SetZQuery(AValue: TZQuery);
    procedure InternalOnExecutionFinished(Sender: TObject; IsTableData: boolean);
    procedure ResizeColumns(ColumnWidth:integer=100);
  public
    constructor Create(ParentPage:TPageControl);
    destructor Destroy; override;
    procedure DisplayMessage(Msg:string; IsError:Boolean);
    property Query:TSQLQuery read FQuery write SetQuery;
    property ZQuery:TZQuery read FZQuery write SetZQuery;
    property DataGrid:TDBGrid read FDataGrid write SetDataGrid;
    property DataSource:TDataSource read FDataSource;
    property DbNavigator:TDBNavigator read FDBNavigator;
    property QueryEditor:TSynEdit read FQueryEditor write SetQueryEditor;
    property Message:string read FMessage write SetMessage;
    procedure ResizeDataGrid(ColumnWidth:Integer=100);
    procedure CheckSyntax;
    property ExecutionInProgress:Boolean read FExecutionInProgress;
    procedure RunQuery(EditMode:Boolean=false;Schema:string='';Table:string='');
    property HasActiveData:Boolean read GetHasActiveData;
    property SQLQuery:string read GetSqlQuery;
    property OnTextScanNeeded:TLazSqlXScanNeeded read FOnTextScanNeeded write SetOnTextScanNeeded;
    property OnLastWordChanged:TLazSqlXLastWordChanged read FOnLastWordChanged write SetOnLastWordChanged;
    property OnExecutionFinished:TOnSqlExecThreadFinish read FOnExecutionFinished write SetOnExecutionFinished;
  end;


  { TLazSqlXPageControl }

  TLazSqlXPageControl = class (TPageControl)
  private
    FTrans:TSQLTransaction;
    FCon:TSQLConnector;
    FDataGridPopUpMenu: TPopupMenu;
    FDBInfo: TDbConnectionInfo;
    FFieldIcon: TBitmap;
    FFunctionIcon: TBitmap;
    FHighlighter: TSynSQLSyn;
    FKeywords: TStringList;
    FOnDataGridDblClick: TNotifyEvent;
    FOnExecutionFinished: TOnSqlExecThreadFinish;
    FProcedureIcon: TBitmap;
    FProcedures: TStringList;
    FQueryEditorPopUpMenu: TPopupMenu;
    FTableIcon: TBitmap;
    FTables: TStringList;
    FVarIcon: TBitmap;
    FZCon:TZConnection;
    FSynComplete:TSynCompletion;
    FlstCompletionItemType: TStringList;
    FlstTableAlias: TStringList;
    FlstAlias: TStringList;
    FLastWord:string;
    function GetActiveTabSheet: TLazSqlXTabSheet;
    procedure OnCompletionExecute(Sender: TObject); // Complete Execute
    function OnCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
    procedure OnCompletionSearchPos(var Posi: integer); //Complete SearchPos

    procedure SetDataGridPopUpMenu(AValue: TPopupMenu);
    procedure SetDBInfo(AValue: TDbConnectionInfo);
    procedure SetFieldIcon(AValue: TBitmap);
    procedure SetFunctionIcon(AValue: TBitmap);
    procedure SetHighlighter(AValue: TSynSQLSyn);
    procedure SetKeywords(AValue: TStringList);
    procedure SetOnDataGridDblClick(AValue: TNotifyEvent);
    procedure SetOnExecutionFinished(AValue: TOnSqlExecThreadFinish);
    procedure SetProcedureIcon(AValue: TBitmap);
    procedure SetProcedures(AValue: TStringList);
    procedure SetQueryEditorPopUpMenu(AValue: TPopupMenu);
    //procedure OnPageChange(Sender:TObject);
    procedure GetVariables(QueryText: string; var VarWords: TStringList);
    procedure GetBracketWords(QueryText: string; var BracketWords: TStringList);
    procedure GetFieldNames(table: string; fieldMask: string; var fieldList: TStringList);
    procedure SetTableIcon(AValue: TBitmap);
    procedure SetTables(AValue: TStringList);
    procedure SetVarIcon(AValue: TBitmap);
    procedure ScanTableAliases(const CurrentLine: string);
    procedure FullScanTableAliases;
    procedure OnTextScanNeeded(ScanText:String);
    procedure OnLastWordChanged(var LastWord:string);
  public
    constructor Create(AOwner:TComponent; Con:TSQLConnector; ZCon:TZConnection);overload;
    destructor Destroy;override;
    property ActiveTab:TLazSqlXTabSheet read GetActiveTabSheet;
    function AddTab:TLazSqlXTabSheet;
    procedure RemoveTab(Tab:TLazSqlXTabSheet);
    procedure RemoveAllTabsButActive;
    procedure RemoveAllTabs;
    property DBInfo:TDbConnectionInfo read FDBInfo write SetDBInfo;
    property Highlighter:TSynSQLSyn read FHighlighter write SetHighlighter;
    property QueryEditorPopUpMenu:TPopupMenu read FQueryEditorPopUpMenu write SetQueryEditorPopUpMenu;
    property DataGridPopUpMenu:TPopupMenu read FDataGridPopUpMenu write SetDataGridPopUpMenu;


    property Tables:TStringList read FTables write SetTables;
    property Keywords:TStringList read FKeywords write SetKeywords;
    property Procedures:TStringList read FProcedures write SetProcedures;
    property TableIcon: TBitmap read FTableIcon write SetTableIcon;
    property FunctionIcon: TBitmap read FFunctionIcon write SetFunctionIcon;
    property FieldIcon: TBitmap read FFieldIcon write SetFieldIcon;
    property VarIcon: TBitmap read FVarIcon write SetVarIcon;
    property ProcedureIcon: TBitmap read FProcedureIcon write SetProcedureIcon;
    property OnExecutionFinished:TOnSqlExecThreadFinish read FOnExecutionFinished write SetOnExecutionFinished;
    property OnDataGridDblClick:TNotifyEvent read FOnDataGridDblClick write SetOnDataGridDblClick;
    procedure ScanNeeded;
  end;


implementation

{ TLazSqlXTabSheet }

procedure TLazSqlXTabSheet.OnDBGridDrawColumnCell(Sender: TObject;
 const Rect: TRect; DataCol: integer; Column: TColumn; State: TGridDrawState);
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

function TLazSqlXTabSheet.GetDbInfo: TDbConnectionInfo;
begin
 Result := (FParent as TLazSqlXPageControl).DBInfo;
end;

function TLazSqlXTabSheet.GetHasActiveData: Boolean;
begin
 Result:=FQuery.Active or FZQuery.Active;
end;

function TLazSqlXTabSheet.GetSqlQuery: string;
begin
 if FQuery.Active then
 Result:=FQuery.SQL.Text
 else
 if FZQuery.Active then;
 Result:=FZQuery.SQL.Text;
end;

procedure TLazSqlXTabSheet.OnQueryEditorChange(Sender: TObject);
begin
 if (Sender is TSynEdit) then
 begin
   if Assigned(FOnTextScanNeeded) then
   FOnTextScanNeeded((Sender as TSynEdit).LineText);
 end;
end;

procedure TLazSqlXTabSheet.OnQueryEditorKeyDown(Sender: TObject; var Key: word;
 Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    (FParent as TLazSqlXPageControl).FullScanTableAliases;
  end;
end;

procedure TLazSqlXTabSheet.OnQueryEditorKeyPress(Sender: TObject; var Key: char
 );
begin

end;

procedure TLazSqlXTabSheet.OnQueryEditorKeyUp(Sender: TObject; var Key: word;
 Shift: TShiftState);
var
  s:string;
begin

  case key of
  VK_OEM_PERIOD:
    begin
      s:='';

      KeyInput.Apply([ssCtrl]);
      try
        KeyInput.Press(VK_SPACE);
      finally
        KeyInput.Unapply([ssCtrl]);
      end;

      if Assigned(FOnLastWordChanged) then
      FOnLastWordChanged(s);

    end;
    VK_OEM_COMMA: KeyInput.Press(VK_SPACE);
  end;

end;

procedure TLazSqlXTabSheet.OnQueryEditorPaste(Sender: TObject;
 var AText: string; var AMode: TSynSelectionMode; ALogStartPos: TPoint;
 var AnAction: TSynCopyPasteAction);
begin
 if Assigned(FOnTextScanNeeded) then
 FOnTextScanNeeded(EmptyStr);
end;

procedure TLazSqlXTabSheet.OnQyeryAfterPost(DataSet: TDataSet);
var
  ds: TSQLQuery;
  I: integer;
begin
  if (DataSet is TSQLQuery) then
  begin
    ds := (DataSet as TSQLQuery);
    I := ds.RecNo;
    ds.UpdateMode:=upWhereKeyOnly;
    ds.ApplyUpdates;
    (ds.DataBase as TSQLConnector).Transaction.Commit;

    if not ds.Active then
    begin
      ds.Open;
      ds.RecNo := I;
      ResizeDataGrid;
    end;
  end
end;

procedure TLazSqlXTabSheet.SetDataGrid(AValue: TDBGrid);
begin
 if FDataGrid=AValue then Exit;
 FDataGrid:=AValue;
end;

procedure TLazSqlXTabSheet.SetMessage(AValue: string);
begin
 if FMessage=AValue then Exit;
 FMessage:=AValue;
end;

procedure TLazSqlXTabSheet.SetOnExecutionFinished(AValue: TOnSqlExecThreadFinish
 );
begin
 if FOnExecutionFinished=AValue then Exit;
 FOnExecutionFinished:=AValue;

end;

procedure TLazSqlXTabSheet.SetOnLastWordChanged(AValue: TLazSqlXLastWordChanged
 );
begin
 if FOnLastWordChanged=AValue then Exit;
 FOnLastWordChanged:=AValue;
end;

procedure TLazSqlXTabSheet.SetOnTextScanNeeded(AValue: TLazSqlXScanNeeded);
begin
 if FOnTextScanNeeded=AValue then Exit;
 FOnTextScanNeeded:=AValue;
end;

procedure TLazSqlXTabSheet.SetQuery(AValue: TSQLQuery);
begin
 if FQuery=AValue then Exit;
 FQuery:=AValue;
end;

procedure TLazSqlXTabSheet.SetQueryEditor(AValue: TSynEdit);
begin
 if FQueryEditor=AValue then Exit;
 FQueryEditor:=AValue;
end;

procedure TLazSqlXTabSheet.SetZQuery(AValue: TZQuery);
begin
 if FZQuery=AValue then Exit;
 FZQuery:=AValue;
end;

procedure TLazSqlXTabSheet.InternalOnExecutionFinished(Sender: TObject;
 IsTableData: boolean);
var
  cmd: TSqlExecThread;
  I: integer;
begin

  FBottomPanel.Height:=200;

  try
    cmd := Sender as TSqlExecThread;

    if cmd.IsSelect then
      FMessage := cmd.Message
    else
    begin
      FMessage := EmptyStr;
    end;


    FErrorMemo.Font.Color := clWindowText;
    if (cmd.LastError <> EmptyStr) then
    begin
      DisplayMessage(cmd.LastError, True);
    end
    else
    begin

      FDataGrid.Visible:=True;
      FErrorMemo.Visible:=False;

      if IsTableData then
      begin
        Self.Tag := 0;
        FQueryEditor.Parent.Visible := False;
        FDataGrid.Parent.Align := alClient;
        FDataGrid.ReadOnly := False;
        FDBNavigator.Visible := True;
      end
      else
      begin
        Self.Tag := 1;
        FQueryEditor.Parent.Visible := True;
        FDataGrid.Parent.Align := alBottom;
        FDataGrid.ReadOnly := True;
        FDBNavigator.Visible := False;
      end;

    end;
  finally

    FExecutionInProgress := False;
    ResizeColumns;

    if Assigned(FOnExecutionFinished) then
    FOnExecutionFinished(Self,FEditMode);
  end;

end;

procedure TLazSqlXTabSheet.ResizeColumns(ColumnWidth: integer);
var
 I: Integer;
begin
 try
   FDataGrid.BeginUpdate;
   for I:=0 to FDataGrid.Columns.Count-1 do
   begin
     FDataGrid.Columns[I].Width:=ColumnWidth;
   end;
 finally
   FDataGrid.EndUpdate;
 end;
end;

constructor TLazSqlXTabSheet.Create(ParentPage: TPageControl);
begin

 inherited Create(Parent);

 FParent:=ParentPage as TLazSqlXPageControl;

  FNumbering := IntToStr(ParentPage.PageCount);

  Caption := 'Query(' + FNumbering + ')';
  Name := 'QueryTab' + FNumbering;

  FQuery := TSQLQuery.Create(nil);
  FQuery.Name := 'qr' + FNumbering;
  FQuery.AfterPost := @OnQyeryAfterPost;


  FZQuery := TZQuery.Create(nil);
  FZQuery.Name := 'FZQuery' + FNumbering;

  FDataSource := TDataSource.Create(nil);
  FDataSource.DataSet := FQuery;
  FDataSource.Name := 'FDataSource' + FNumbering;


  FTopPanel := TPanel.Create(nil);
  FTopPanel.Name := 'FTopPanel' + FNumbering;
  FTopPanel.Caption := '';
  FQueryEditor := TSynEdit.Create(Self);
  FQueryEditor.Name := 'txtSyn' + FNumbering;
  FQueryEditor.Font.Pitch := fpFixed;
  FQueryEditor.Font.Quality := fqDraft;
  FQueryEditor.Options := FQueryEditor.Options - [eoSmartTabs, eoScrollPastEol] + [eoTabIndent];
  FQueryEditor.RightEdge := 120;
  FQueryEditor.MouseOptions := FQueryEditor.MouseOptions + [TSynEditorMouseOption.emCtrlWheelZoom];

  FQueryEditor.OnChange := @OnQueryEditorChange;
  FQueryEditor.OnPaste := @OnQueryEditorPaste;
  FQueryEditor.OnKeyDown:=@OnQueryEditorKeyDown;
  FQueryEditor.OnKeyPress:=@OnQueryEditorKeyPress;
  FQueryEditor.OnKeyUp:=@OnQueryEditorKeyUp;

  FQueryEditor.Align := alClient;
  FQueryEditor.Gutter.Visible := True;
  FQueryEditor.Options := FQueryEditor.Options - [eoScrollPastEol];
  FQueryEditor.Lines.Clear;
  FQueryEditor.Parent := FTopPanel;

  FBottomPanel := TPanel.Create(nil);
  FBottomPanel.Name := 'FBottomPanel' + FNumbering;
  FBottomPanel.Caption := '';
  FBottomPanel.Height := 200;

  FErrorMemo := TMemo.Create(nil);
  FErrorMemo.Name := 'FErrorMemo' + FNumbering;
  FErrorMemo.Align := alClient;
  FErrorMemo.ReadOnly := True;
  FErrorMemo.ScrollBars := ssAutoBoth;
  FErrorMemo.Parent := FBottomPanel;
  FErrorMemo.Height:=2;

  FDataGrid := TDBGrid.Create(nil);
  FDataGrid.Name := 'grd' + FNumbering;
  FDataGrid.Align := alClient;
  FDataGrid.DataSource := FDataSource;
  FDataGrid.TitleStyle := tsNative;

  FDataGrid.Options := FDataGrid.Options + [dgMultiselect];
  FDataGrid.Visible := True;
  FDataGrid.OnDrawColumnCell := @OnDBGridDrawColumnCell;
  FDataGrid.Parent := FBottomPanel;

  FDBNavigator := TDBNavigator.Create(FBottomPanel);
  FDBNavigator.Align := alBottom;
  FDBNavigator.DataSource := FDataSource;
  FDBNavigator.Name := 'DBNavigator' + FNumbering;
  FDBNavigator.Visible := True;
  FDBNavigator.Parent := FBottomPanel;

  FSplitter := TSplitter.Create(nil);
  FSplitter.Name:='FSplitter'+FNumbering;


  FTopPanel.Parent := Self;
  FTopPanel.Align := alClient;
  FSplitter.Parent:=Self;
  FSplitter.Align := alBottom;
  FBottomPanel.Parent:=Self;
  FBottomPanel.Align := alBottom;
  FBottomPanel.Visible := True;
  FBottomPanel.Height:=2; {instead of Visible=False as a workaround}
end;

destructor TLazSqlXTabSheet.Destroy;
begin

   FDataGrid.Free;
   FDBNavigator.Free;
   FDataSource.Free;
   FQuery.Free;

   {This control is Parented, because if not,
   some keyboards keys don't work on SynEdit like back,arrows,home,end,pgup,pdwn,insert; delete works on selection only,}

   //FQueryEditor.Free;

   FSplitter.Free;
   FErrorMemo.Free;
   FZQuery.Free;
   FTopPanel.Free;
   FBottomPanel.Free;
 inherited Destroy;
end;

procedure TLazSqlXTabSheet.DisplayMessage(Msg: string; IsError: Boolean);
begin

  FDataGrid.Visible:=False;
  FDBNavigator.Visible:=False;
  FErrorMemo.Height:=200;
  FErrorMemo.Visible:=True;
  FBottomPanel.Height:=200;
  FErrorMemo.Text := msg;
  if IsError then
    FErrorMemo.Font.Color := clRed
  else
    FErrorMemo.Font.Color := clGreen;

  FErrorMemo.Lines.Add('');
end;

procedure TLazSqlXTabSheet.ResizeDataGrid(ColumnWidth: Integer);
var
 i: Integer;
begin
 FDataGrid.BeginUpdate;
 try
 for i:=0 to FDataGrid.Columns.Count-1 do
 begin
   FDataGrid.Columns[I].Width:=ColumnWidth;
 end;

 finally
   FDataGrid.EndUpdate;
 end;
end;

procedure TLazSqlXTabSheet.CheckSyntax;
var
  error: TSqlSyntaxError;
begin
  if TDbUtils.CheckSqlSyntax(FQueryEditor.Text, error) then
  begin
    DisplayMessage('Syntax check completed', False);
  end
  else
  begin
    {FActiveSynMemo.CaretXY:= Point(error.Line,error.Position); //set caret to error line}
    DisplayMessage(error.Message, True);
  end;
end;

procedure TLazSqlXTabSheet.RunQuery(EditMode: Boolean; Schema: string;
 Table: string);
var
  sqlCommand: string;
  I: integer;
  CommandExecutor: TSqlExecThread;
begin

  if FExecutionInProgress then
  begin
    if not Assigned(FCurrentExecutor) then
    begin
      FExecutionInProgress := False;
    end
    else
    if MessageDlg('Confirm',
      'An execution is already running. Do you like to stop current execution?',
      mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      FCurrentExecutor.Terminate;
    end;
    Exit;
  end;


    if not EditMode then
    begin
      if FQueryEditor.SelAvail then
      begin
        sqlCommand := FQueryEditor.SelText;
      end
      else
      begin
        sqlCommand := FQueryEditor.Text;
      end;
      if (FParent as TLazSqlXPageControl).DBInfo.DatabaseType = dtOracle then
        sqlCommand := StringReplace(sqlCommand, ';', '', [rfReplaceAll]);
    end
    else
    begin
      sqlCommand := 'SELECT * FROM ' +Schema+'.'+ Table;
      if (FParent as TLazSqlXPageControl).DBInfo.DatabaseType in [dtSQLite, dtFirebirdd] then
        sqlCommand := 'SELECT * FROM ' + Table;
      Self.Caption := Table;
    end;

    try

      case (FParent as TLazSqlXPageControl).DBInfo.DbEngine of
        deSqlDB:
        begin
          CommandExecutor :=
            TSqlExecThread.Create(Schema, FQuery, @InternalOnExecutionFinished);
          FDataSource.DataSet := FQuery;
        end;
        deZeos:
        begin
          CommandExecutor := TSqlExecThread.Create(Schema,
            FZQuery, @InternalOnExecutionFinished);
          FDataSource.DataSet := FZQuery;
        end;
      end;
      FEditMode := EditMode;
      FCurrentExecutor := CommandExecutor;
      CommandExecutor.ExecuteSQL(sqlCommand, EditMode);
      FExecutionInProgress := True;
    except
      on e: Exception do
      begin
        DisplayMessage(e.Message, True);
      end;
    end;


end;


{ TLazSqlXPageControl }

procedure TLazSqlXPageControl.SetHighlighter(AValue: TSynSQLSyn);
begin
 if FHighlighter=AValue then Exit;
 FHighlighter:=AValue;
end;

procedure TLazSqlXPageControl.SetKeywords(AValue: TStringList);
begin
 if FKeywords=AValue then Exit;
 FKeywords:=AValue;
end;

procedure TLazSqlXPageControl.SetOnDataGridDblClick(AValue: TNotifyEvent);
begin
 if FOnDataGridDblClick=AValue then Exit;
 FOnDataGridDblClick:=AValue;
end;

procedure TLazSqlXPageControl.SetOnExecutionFinished(AValue: TOnSqlExecThreadFinish);
var
 I: Integer;
begin
 if FOnExecutionFinished=AValue then Exit;
 FOnExecutionFinished:=AValue;
end;

procedure TLazSqlXPageControl.SetProcedureIcon(AValue: TBitmap);
begin
 if FProcedureIcon=AValue then Exit;
 FProcedureIcon:=AValue;
end;

procedure TLazSqlXPageControl.SetProcedures(AValue: TStringList);
begin
 if FProcedures=AValue then Exit;
 FProcedures:=AValue;
end;

procedure TLazSqlXPageControl.OnCompletionExecute(Sender: TObject);

  procedure Add(s: string; typ: string);
  begin
    if (pos(lowercase(FsynComplete.CurrentString), lowercase(s)) = 1) or
      (typ = 'FIELD') or (typ = 'VAR') then
    begin
      if (typ = 'FIELD') then
      begin
        if TAsStringUtils.ContainsChar(S, ' ') then
          case DbInfo.DatabaseType of
            dtMsSql: s := '[' + s + ']';
            dtOracle: s := '"' + s + '"';
          end;
      end;

      FSynComplete.ItemList.Add(s);
      FlstCompletionItemType.Add(typ);
    end;
  end;

var
  I: integer;
  tbl, t: string;
  lst: TStringList;
  lstVars: TStringList;
  fieldMask: string;
  currLineSplit: TStringDynArray;
begin

  FsynComplete.ItemList.Clear;
  FlstCompletionItemType.Clear;

  tbl := TAsStringUtils.RemoveChars(FsynComplete.CurrentString, ['"', '[', ']']);

  lstVars := TStringList.Create;
  GetVariables( (ActivePage as TLazSqlXTabSheet).QueryEditor.Text, lstVars);

  if tbl = '' then
  begin

    for I := 0 to FTables.Count - 1 do
    begin
      t := FTables[I];
      if TAsStringUtils.ContainsChar(t, ' ') then
      begin
        case DbInfo.DatabaseType of
          dtMsSql: t := '[' + t + ']';
          dtOracle: t := '"' + t + '"'
        end;
      end;
      FlstCompletionItemType.Add('TABLE');
      FsynComplete.ItemList.Add(t);
    end;

    FsynComplete.ItemList.AddStrings(FKeywords);
    for I := 0 to FKeywords.Count - 1 do
    begin
      FlstCompletionItemType.Add('FUNC');
    end;


    for I := 0 to FProcedures.Count - 1 do
    begin
      FlstCompletionItemType.Add('PROC');
      FsynComplete.ItemList.Add(FProcedures[I]);
    end;

    for I := 0 to lstVars.Count - 1 do
    begin
      FsynComplete.ItemList.Add(StringReplace(lstVars[I], '@', '', [rfReplaceAll]));
      FlstCompletionItemType.Add('VAR');
    end;

  end
  else
  begin

    if AnsiContainsStr(tbl, '.') then
    begin
      fieldMask := '';

      currLineSplit := TAsStringUtils.SplitString(tbl, '.');

      tbl := currLineSplit[0];

      if Length(currLineSplit) > 1 then
        fieldMask := currLineSplit[1];


      if FTables.IndexOf(tbl) < 0 then
      begin
        try
          tbl := FlstAlias.Values[tbl];
        except
        end;
      end;

      //field names if table is last word and
      if FTables.IndexOf(tbl) > -1 then
      begin
        lst := TStringList.Create;
        try
          GetFieldNames(tbl, fieldMask, lst);
          for I := 0 to lst.Count - 1 do
          begin
            Add(lst[I], 'FIELD');
          end;
        finally
          lst.Free;
        end;
      end;
    end
    else
    begin
      //Tablenames
      for I := 0 to FTables.Count - 1 do
      begin
        Add(FTables[I], 'TABLE');
      end;

      //SQL Keywords
      for I := 0 to FKeywords.Count - 1 do
      begin
        Add(FKeywords[I], 'FUNC');
      end;

      for I := 0 to lstVars.Count - 1 do
      begin
        Add(StringReplace(lstVars[I], '@', '', [rfReplaceAll]), 'VAR');
      end;

      for I := 0 to FProcedures.Count - 1 do
      begin
        Add(FProcedures[I], 'PROC');
      end;

    end;

  end;
  lstVars.Free;

end;

function TLazSqlXPageControl.GetActiveTabSheet: TLazSqlXTabSheet;
begin
 Result := ActivePage as TLazSqlXTabSheet;
end;

function TLazSqlXPageControl.OnCompletionPaintItem(const AKey: string;
 ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
var
  r: TRect;
begin
  r := Rect(x, y, FsynComplete.Width, Y + 17);
  ACanvas.Refresh;

  if Selected then
  begin
    ACanvas.GradientFill(r, $00E2E2E2, clGray, gdVertical);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Color := clBlack;
  end;

  try

    if FlstCompletionItemType[Index] = 'TABLE' then
    begin
      ACanvas.Draw(x, y, FtableIcon);
    end
    else
    if FlstCompletionItemType[Index] = 'FUNC' then
    begin
      ACanvas.Draw(x, y, FprocedureIcon);
    end
    else
    if FlstCompletionItemType[Index] = 'FIELD' then
    begin
      ACanvas.Draw(x, y, FfieldIcon);
    end
    else
    if FlstCompletionItemType[Index] = 'VAR' then
    begin
      ACanvas.Draw(x, y, FvarIcon);
    end
    else
    if FlstCompletionItemType[Index] = 'PROC' then
    begin
      ACanvas.Draw(x, y, FfunctionIcon);
    end;

  except
  end;

  ACanvas.TextOut(X + 16, Y, AKey);
  Result := True;
end;

procedure TLazSqlXPageControl.OnCompletionSearchPos(var Posi: integer);
  procedure Add(s: string; typ: string);
  begin

    if (Pos(lowercase(FsynComplete.CurrentString), lowercase(s)) = 1) or
      (typ = 'FIELD') or (typ = 'VAR') then
    begin
      if (typ = 'FIELD') then
      begin
        if TAsStringUtils.ContainsChar(S, ' ') then
          case DbInfo.DatabaseType of
            dtMsSql: s := '[' + s + ']';
            dtOracle: s := '"' + s + '"';
          end;
      end;
      FsynComplete.ItemList.Add(s);
      FlstCompletionItemType.Add(typ);
    end;
  end;

var
  I: integer;
  tbl: string;
  lst: TStringList;
  lstVars: TStringList;
  fieldmask: string;
  currLineSplit: TStringDynArray;
begin

  FsynComplete.ItemList.Clear;
  FlstCompletionItemType.Clear;
  lstVars := TStringList.Create;
  GetVariables((ActivePage as TLazSqlXTabSheet).QueryEditor.Text, lstVars);
  tbl := TAsStringUtils.RemoveChars(FsynComplete.CurrentString, ['"', '[', ']']);
  if tbl = '' then
  begin
    FsynComplete.ItemList.AddStrings(FTables);
    for I := 0 to FTables.Count - 1 do
    begin
      FlstCompletionItemType.Add('TABLE');
    end;

    FsynComplete.ItemList.AddStrings(FKeywords);
    for I := 0 to FKeywords.Count - 1 do
    begin
      FlstCompletionItemType.Add('FUNC');
    end;


    FsynComplete.ItemList.AddStrings(lstVars);
    for I := 0 to lstVars.Count - 1 do
    begin
      FlstCompletionItemType.Add('VAR');
    end;

    for I := 0 to FProcedures.Count - 1 do
    begin
      FlstCompletionItemType.Add('PROC');
      FsynComplete.ItemList.Add(FProcedures[I]);
    end;


    if FsynComplete.ItemList.Count > 0 then
      Posi := 0
    else
      Posi := -1;

  end
  else
  begin

    if AnsiContainsStr(tbl, '.') then
    begin

      fieldmask := '';//means all
      currLineSplit := TAsStringUtils.SplitString(tbl, '.');

      tbl := currLineSplit[0];

      if Length(currLineSplit) > 1 then
        fieldmask := currLineSplit[1];


      if FTables.IndexOf(tbl) < 0 then
      begin
        try
          tbl := FlstAlias.Values[tbl];
        except
        end;
      end;


      //field names if table is last word and
      if FTables.IndexOf(tbl) > -1 then
      begin
        lst := TStringList.Create;
        try
          GetFieldNames(tbl, fieldmask, lst);
          for I := 0 to lst.Count - 1 do
          begin
            Add(lst[I], 'FIELD');
          end;


        finally
          lst.Free;
        end;
      end;


      if FsynComplete.ItemList.Count > 0 then
        Posi := 0
      else
        Posi := -1;

    end
    else
    begin

      for I := 0 to FTables.Count - 1 do
      begin
        Add(FTables[I], 'TABLE');
      end;

      //SQL Keywords
      for I := 0 to FKeywords.Count - 1 do
      begin
        Add(FKeywords[I], 'FUNC');
      end;

      for I := 0 to lstVars.Count - 1 do
      begin
        Add(lstVars[I], 'FUNC');
      end;

      for I := 0 to FProcedures.Count - 1 do
      begin
        Add(FProcedures[I], 'PROC');
      end;

    end;

  end;
  lstVars.Free;

end;

procedure TLazSqlXPageControl.SetDataGridPopUpMenu(AValue: TPopupMenu);
begin
 if FDataGridPopUpMenu=AValue then Exit;
 FDataGridPopUpMenu:=AValue;
end;

procedure TLazSqlXPageControl.SetDBInfo(AValue: TDbConnectionInfo);
begin
 if FDBInfo=AValue then Exit;
 FDBInfo:=AValue;
end;

procedure TLazSqlXPageControl.SetFieldIcon(AValue: TBitmap);
begin
 if FFieldIcon=AValue then Exit;
 FFieldIcon:=AValue;
end;

procedure TLazSqlXPageControl.SetFunctionIcon(AValue: TBitmap);
begin
 if FFunctionIcon=AValue then Exit;
 FFunctionIcon:=AValue;
end;

procedure TLazSqlXPageControl.SetQueryEditorPopUpMenu(AValue: TPopupMenu);
begin
 if FQueryEditorPopUpMenu=AValue then Exit;
 FQueryEditorPopUpMenu:=AValue;
end;

//procedure TLazSqlXPageControl.OnPageChange(Sender: TObject);
//begin
//  if PageCount>0 then
//    FSynComplete.Editor := ActiveTab.QueryEditor;
//end;

procedure TLazSqlXPageControl.GetVariables(QueryText: string;
 var VarWords: TStringList);
var
  expr: string;
begin
  expr := '\B@\w*\b';
  TRegExUtils.RunRegex(QueryText, expr, VarWords);
end;

procedure TLazSqlXPageControl.GetBracketWords(QueryText: string;
 var BracketWords: TStringList);
var
  expr: string;
begin
  expr := '\[(.*?)\]';
  TRegExUtils.RunRegex(QueryText, expr, BracketWords);
end;

procedure TLazSqlXPageControl.GetFieldNames(table: string; fieldMask: string;
 var fieldList: TStringList);
var
  lst: TStringList;
  I: integer;
begin

  try
    lst := TStringList.Create;

    case DbInfo.DatabaseType of
      dtMsSql, dtOracle, dtFirebirdd:
      begin
        FCon.GetFieldNames(table, lst);
      end;
      dtMySql: FZCon.GetColumnNames(table, '', lst);
      dtSQLite: TDbUtils.GetColumnNames(DbInfo, table, lst);
    end;



    for I := 0 to lst.Count - 1 do
    begin
      if trim(fieldMask) = EmptyStr then
      begin
        fieldList.Add(lst[I]);
      end
      else
      begin
        if AnsiStartsText(fieldMask, lst[I]) then
          fieldList.Add(lst[I]);
      end;

    end;
  finally
    lst.Free;
  end;

end;

procedure TLazSqlXPageControl.SetTableIcon(AValue: TBitmap);
begin
 if FTableIcon=AValue then Exit;
 FTableIcon:=AValue;
end;

procedure TLazSqlXPageControl.SetTables(AValue: TStringList);
begin
 if FTables=AValue then Exit;
 FTables:=AValue;
end;

procedure TLazSqlXPageControl.SetVarIcon(AValue: TBitmap);
begin
 if FVarIcon=AValue then Exit;
 FVarIcon:=AValue;
end;

procedure TLazSqlXPageControl.OnTextScanNeeded(ScanText: String);
begin
 if ScanText=EmptyStr then
  FullScanTableAliases
 else
  ScanTableAliases(ScanText);
end;

procedure TLazSqlXPageControl.OnLastWordChanged(var LastWord: string);
begin
 FLastWord:=LastWord;
end;

procedure TLazSqlXPageControl.ScanTableAliases(const CurrentLine: string);
var
  Added: boolean;
  textAfterTbl: string;
  lstBracketWords: TStringList;
  strTablename: string;
  currentString: string;
  aliasIndex: integer;
  strAlias: string;
  Posi: integer;
  J: integer;
  lstDel: TStringList;
  I: integer;
  tblIndex: integer;
  CurrentStringLine: string;
begin
  Added := True;
  CurrentStringLine := CurrentLine;


  if Trim(CurrentStringLine) <> EmptyStr then
    try
      try
        lstBracketWords := TStringList.Create;
        lstDel := TStringList.Create;
        lstDel.Delimiter := ' ';
        lstDel.StrictDelimiter := True;
        lstDel.DelimitedText := CurrentStringLine;

        for J := 0 to lstDel.Count - 1 do
        begin
          currentString := TAsStringUtils.RemoveChars(lstDel[J],
            ['[', ']', '"']);
          //get rid of [Tablename]

          tblIndex := FTables.IndexOf(currentString);

          if tblIndex > -1 then
          begin
            strTablename := currentString;
            //add alias then table, like bc=borderCrossing
            if J < lstDel.Count - 1 then
              if lstDel[J + 1] <> EmptyStr then
              begin
                strAlias := lstDel[J + 1] + '=' + strTablename;

                aliasIndex := FlstTableAlias.IndexOf(strTablename);

                if aliasIndex < 0 then
                begin
                  FlstTableAlias.Add(strTablename);
                  FlstAlias.Add(strAlias);
                end
                else
                begin
                  FlstAlias[aliasIndex] := strAlias;
                end;
              end;
          end;

        end;

        //if a tablename contains spaces in name, then it must be sorrounded with [ and ].
        //so we will check for those tableNames in query
        GetBracketWords(CurrentStringLine, lstBracketWords);

        for I := 0 to lstBracketWords.Count - 1 do
        begin

          currentString := TAsStringUtils.RemoveChars(lstBracketWords[I],
            ['[', ']', '"']);  //get rid of [Tablename]
          tblIndex := FTables.IndexOf(currentString);

          if tblIndex > -1 then
          begin
            strTablename := currentString;
            Posi := Pos(strTablename, CurrentStringLine);
            //add alias then table, like bc=borderCrossing
            if Posi >= 1 then
            begin
              textAfterTbl :=
                Copy(CurrentStringLine, Posi + Length(strTablename) +
                1, Length(CurrentStringLine) - Posi + Length(strTablename));

              lstDel.Clear;
              lstDel.DelimitedText := textAfterTbl;
              for J := 0 to lstDel.Count - 1 do
              begin
                if Trim(lstDel[J]) <> EmptyStr then
                begin
                  strAlias := lstDel[j] + '=' + strTablename;
                  aliasIndex := FlstTableAlias.IndexOf(strTablename);

                  if aliasIndex < 0 then
                  begin
                    FlstTableAlias.Add(strTablename);
                    FlstAlias.Add(strAlias);
                  end
                  else
                  begin
                    FlstAlias[aliasIndex] := strAlias;
                  end;
                  break;
                end;
              end;

            end;
          end;
        end;

        FLastWord := lstDel[lstDel.Count - 1];

        //if UpperCase(LastWord) = 'FROM' then
        //begin
        //  x := 0;
        //end;


      except
      end;

    finally
      lstDel.Free;
      lstBracketWords.Free;
    end;

end;

procedure TLazSqlXPageControl.FullScanTableAliases;
var
  I: integer;
  qe:TSynEdit;
begin
  qe := (ActivePage as TLazSqlXTabSheet).QueryEditor;
  if Trim(qe.Text) = EmptyStr then
  begin
    FlstAlias.Clear;
    FlstTableAlias.Clear;
  end
  else
    for I := 0 to qe.Lines.Count - 1 do
    begin
      ScanTableAliases(qe.Lines[I]);
    end;
end;

constructor TLazSqlXPageControl.Create(AOwner: TComponent; Con: TSQLConnector;
 ZCon: TZConnection);
begin

 inherited Create(Owner);
 FCon:=Con;
 FZCon:=ZCon;
 //OnChange:=@OnPageChange;
 FsynComplete := TSynCompletion.Create(nil);
 FsynComplete.CaseSensitive := False;
 FsynComplete.OnExecute := @OnCompletionExecute;
 FsynComplete.OnSearchPosition := @OnCompletionSearchPos;
 FsynComplete.ShowSizeDrag := True;
 FsynComplete.DoubleClickSelects := True;
 FsynComplete.OnPaintItem := @OnCompletionPaintItem;
 FsynComplete.EndOfTokenChr := ' ';

  FlstCompletionItemType:= TStringList.Create;
  FlstTableAlias:= TStringList.Create;
  FlstAlias := TStringList.Create;

  FTables := TStringList.Create;
  FProcedures := TStringList.Create;
  FKeywords := TStringList.Create;
end;

destructor TLazSqlXPageControl.Destroy;
begin
  FsynComplete.Free;
  FlstCompletionItemType.Free;
  FlstTableAlias.Free;
  FlstAlias.Free;

  FTables.Free;
  FProcedures.Free;
  FKeywords.Free;
 inherited Destroy;
end;

function TLazSqlXPageControl.AddTab: TLazSqlXTabSheet;
var
 Tab:TLazSqlXTabSheet;
 t:TTabSheet;
begin
  Tab := TLazSqlXTabSheet.Create(Self);
  Tab.Parent:=Self;
  Tab.ZQuery.Connection:=FZCon;
  Tab.Query.DataBase:=FCon;
  Tab.Query.Transaction:=FCon.Transaction;

 if Assigned(FHighlighter) then
  Tab.QueryEditor.Highlighter := FHighlighter;

 if Assigned(FQueryEditorPopUpMenu) then
 Tab.QueryEditor.PopupMenu := FQueryEditorPopUpMenu;

 if Assigned(FDataGridPopUpMenu) then
 Tab.DataGrid.PopupMenu := FDataGridPopUpMenu;

 Self.ActivePage := Tab;

 tab.OnExecutionFinished:=FOnExecutionFinished;
 tab.DataGrid.OnDblClick:=FOnDataGridDblClick;
 tab.OnTextScanNeeded:=@OnTextScanNeeded; {for autocomplete,fieldnames invoke}
 if Tab.QueryEditor<>nil then
 begin
  FSynComplete.AddEditor(Tab.QueryEditor);
 end;
 Result := Tab;
end;

procedure TLazSqlXPageControl.RemoveTab(Tab: TLazSqlXTabSheet);
var
 I: Integer;
begin
  for I:=0 to PageCount-1 do
  begin
    if Pages[I] = Tab then
    begin
      FSynComplete.RemoveEditor(Tab.QueryEditor);
      Pages[I].Free;
      Break;
    end;
  end;
end;

procedure TLazSqlXPageControl.RemoveAllTabsButActive;
var
 I: Integer;
 tab:TLazSqlXTabSheet;
begin
 if PageCount<1 then exit;
 ActivePage.PageIndex:=PageCount-1;

 for I:=0 to PageCount-1 do
 begin
   if Pages[0]<>ActivePage then
   begin
     if (Pages[0] is TLazSqlXTabSheet) then
     begin
      tab := Pages[0] as TLazSqlXTabSheet;
      FSynComplete.RemoveEditor(Tab.QueryEditor);
      tab.Free;
     end;
   end;
 end;
end;

procedure TLazSqlXPageControl.RemoveAllTabs;
var
 I: Integer;
 tab:TLazSqlXTabSheet;
begin
 for I:=0 to PageCount-1 do
 begin
   if (Pages[0] is TLazSqlXTabSheet) then
   begin
    tab := Pages[0] as TLazSqlXTabSheet;
    FSynComplete.RemoveEditor(tab.QueryEditor);
    tab.Free;
   end;
 end;
end;

procedure TLazSqlXPageControl.ScanNeeded;
begin
 FullScanTableAliases;
end;

end.

