unit LazSqlXCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics,Dialogs, sqldb, db,
  ZDataset, ZConnection, DbCtrls, DBGrids, SynEdit, SynCompletion, SynEditTypes,
  Grids, Menus, AsStringUtils, AsDbType, Utils, SynHighlighterSQL, Types, strutils,
  LCLType, SqlExecThread, Forms;


type


  { TLazSqlXTabSheet }

  TLazSqlXPageControl = class;

  TLazSqlXScanNeeded = procedure (ScanText:string) of object;
  TLazSqlXLastWordChanged = procedure (var LastWord:string) of object;
  TLazSqlXCaretPositionChanged = procedure (Line, Column : Integer) of object;

  TLazSqlXTabSheet = class(TTabSheet)
  private
    FMessage: string;
    FOnCaretPositionChanged: TLazSqlXCaretPositionChanged;
    FOnExecutionFinished: TOnSqlExecThreadFinish;
    FOnExecutionStopped: TNotifyEvent;
    FOnLastWordChanged: TLazSqlXLastWordChanged;
    FOnTextScanNeeded: TLazSqlXScanNeeded;
    FTopPanel, FBottomPanel: TPanel;
    FDataGrid: TDBGrid;
    FDBNavigator: TDBNavigator;
    FDataSource: TDataSource;
    FQuery: TAsQuery;
    FQueryEditor: TSynEdit;
    FSplitter: TSplitter;
    FErrorMemo: TMemo;
    FNumbering: string;
    FParent:TLazSqlXPageControl;
    FCurrentExecutor:TSqlExecThread;
    FEditMode:Boolean;
    FTransaction:TSQLTransaction;
    FDbInfoNew:TAsDbConnectionInfo;
    function GetDataSet: TDataSet;
    function GetExecutionInProgress: Boolean;
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
    procedure OnQueryAfterDelete(DataSet:TDataSet);
    procedure SetDataGrid(AValue: TDBGrid);
    procedure SetMessage(AValue: string);
    procedure SetOnCaretPositionChanged(AValue: TLazSqlXCaretPositionChanged);
    procedure SetOnExecutionFinished(AValue: TOnSqlExecThreadFinish);
    procedure SetOnExecutionStopped(AValue: TNotifyEvent);
    procedure SetOnLastWordChanged(AValue: TLazSqlXLastWordChanged);
    procedure SetOnTextScanNeeded(AValue: TLazSqlXScanNeeded);
    procedure SetQueryEditor(AValue: TSynEdit);
    procedure InternalOnExecutionFinished(Sender: TObject; IsTableData: boolean);
    procedure ResizeColumns(ColumnWidth:integer=100);
  public
    constructor Create(ParentPage:TLazSqlXPageControl);
    destructor Destroy; override;
    procedure DisplayMessage(Msg:string; IsError:Boolean);
    property Query:TAsQuery read FQuery write FQuery;
    property DataGrid:TDBGrid read FDataGrid write SetDataGrid;
    property DataSource:TDataSource read FDataSource;
    property DataSet:TDataSet read GetDataSet;
    property DbNavigator:TDBNavigator read FDBNavigator;
    property QueryEditor:TSynEdit read FQueryEditor write SetQueryEditor;
    property Message:string read FMessage write SetMessage;
    procedure ResizeDataGrid(ColumnWidth:Integer=100);
    procedure CheckSyntax;
    property ExecutionInProgress:Boolean read GetExecutionInProgress;
    procedure RunQuery(EditMode:Boolean=false;Schema:string='';Table:string='');
    property HasActiveData:Boolean read GetHasActiveData;
    property SQLQuery:string read GetSqlQuery;
    property OnTextScanNeeded:TLazSqlXScanNeeded read FOnTextScanNeeded write SetOnTextScanNeeded;
    property OnLastWordChanged:TLazSqlXLastWordChanged read FOnLastWordChanged write SetOnLastWordChanged;
    property OnExecutionFinished:TOnSqlExecThreadFinish read FOnExecutionFinished write SetOnExecutionFinished;
    property OnCaretPositionChanged:TLazSqlXCaretPositionChanged read FOnCaretPositionChanged write SetOnCaretPositionChanged;
    property OnExecutionStopped:TNotifyEvent read FOnExecutionStopped write SetOnExecutionStopped;

  end;


  { TLazSqlXPageControl }

  TLazSqlXPageControl = class (TPageControl)
  private
   FActiveTabPositionChanged: TLazSqlXCaretPositionChanged;
   FOnExecutionStopped: TNotifyEvent;
    FTrans:TSQLTransaction;
    FCon:TSQLConnector;
    FDataGridPopUpMenu: TPopupMenu;
    FDBInfo: TAsDbConnectionInfo;
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
    FSynComplete: TSynCompletion;
    FlstCompletionItemType: TStringList;
    FlstTableAlias: TStringList;
    FlstAlias: TStringList;
    FLastWord:string;
    function GetActiveTabSheet: TLazSqlXTabSheet;
    function GetLazSqlXTab(Index: Integer): TLazSqlXTabSheet;
    procedure OnCompletionExecute(Sender: TObject); // Complete Execute
    function OnCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
    procedure OnCompletionSearchPos(var Posi: integer); //Complete SearchPos
    procedure SetActiveTabPositionChanged(AValue: TLazSqlXCaretPositionChanged);

    procedure SetDataGridPopUpMenu(AValue: TPopupMenu);
    procedure SetDBInfo(AValue: TAsDbConnectionInfo);
    procedure SetFieldIcon(AValue: TBitmap);
    procedure SetFunctionIcon(AValue: TBitmap);
    procedure SetHighlighter(AValue: TSynSQLSyn);
    procedure SetKeywords(AValue: TStringList);
    procedure SetLazSqlXTab(Index: Integer; AValue: TLazSqlXTabSheet);
    procedure SetOnDataGridDblClick(AValue: TNotifyEvent);
    procedure SetOnExecionStopped(AValue: TNotifyEvent);
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
    constructor Create(AOwner:TComponent; DbInfo:TAsDbConnectionInfo);overload;
    destructor Destroy;override;

    property ActiveTab:TLazSqlXTabSheet read GetActiveTabSheet;
    function AddTab:TLazSqlXTabSheet;
    procedure RemoveTab(Tab:TLazSqlXTabSheet);
    procedure RemoveAllTabsButActive;
    procedure RemoveAllTabs;

    property DBInfo:TAsDbConnectionInfo read FDBInfo;
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
    property OnExecutionStopped:TNotifyEvent read FOnExecutionStopped write SetOnExecionStopped;
    property OnDataGridDblClick:TNotifyEvent read FOnDataGridDblClick write SetOnDataGridDblClick;
    // Tells control to call autocompletion
    procedure PopupAutoComplete;
    procedure ScanNeeded;

    property Pages[Index:Integer]:TLazSqlXTabSheet read GetLazSqlXTab write SetLazSqlXTab;

    property OnCaretPositionChanged:TLazSqlXCaretPositionChanged read FActiveTabPositionChanged write SetActiveTabPositionChanged;
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



function TLazSqlXTabSheet.GetDataSet: TDataSet;
begin
 Result := FQuery.DataSet;
end;

function TLazSqlXTabSheet.GetExecutionInProgress: Boolean;
begin
 if Assigned(FCurrentExecutor) then
  Result :=not FCurrentExecutor.IsTerminated
 else
  Result:=False;
end;

function TLazSqlXTabSheet.GetHasActiveData: Boolean;
begin
  Result:=FQuery.Active;
end;

function TLazSqlXTabSheet.GetSqlQuery: string;
begin
  Result:=FQuery.SQL.Text;
end;

procedure TLazSqlXTabSheet.OnQueryEditorChange(Sender: TObject);
begin
 if (Sender is TSynEdit) then
 begin
   if Assigned(FOnTextScanNeeded) then
   FOnTextScanNeeded((Sender as TSynEdit).LineText);
 end;
 if Assigned(FOnCaretPositionChanged) then
 FOnCaretPositionChanged(FQueryEditor.CaretX,FQueryEditor.CaretY);
end;

procedure TLazSqlXTabSheet.OnQueryEditorKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
  procedure InvokeSynCompleteKey;
  var
    s: string;
  begin
    s:='';
    // Swallow key
    key:=VK_UNKNOWN;

    // Tell synedit to call autocompletion popup
    FParent.PopupAutoComplete;

    if Assigned(FOnLastWordChanged) then
      FOnLastWordChanged(s);
  end;

begin
  case key of
    VK_SPACE: //Ctrl-Space
      if ssCtrl in Shift then InvokeSynCompleteKey;
     VK_DELETE:
    FParent.FullScanTableAliases;
  end;
end;
procedure TLazSqlXTabSheet.OnQueryEditorKeyPress(Sender: TObject; var Key: char
 );
begin
end;

procedure TLazSqlXTabSheet.OnQueryEditorKeyUp(Sender: TObject; var Key: word;
 Shift: TShiftState);
  procedure InvokeSynCompleteKey;
  var
    s: string;
  begin
    s:='';
    // Swallow key
    key:=VK_UNKNOWN;

    // Tell synedit to call autocompletion popup
    FParent.PopupAutoComplete;

    if Assigned(FOnLastWordChanged) then
      FOnLastWordChanged(s);
  end;

begin
  case key of
    VK_OEM_PERIOD: //.
      InvokeSynCompleteKey;
  end;
  if Assigned(FOnCaretPositionChanged) then
  FOnCaretPositionChanged(FQueryEditor.CaretX,FQueryEditor.CaretY);
end;

procedure TLazSqlXTabSheet.OnQueryEditorPaste(Sender: TObject;
  var AText: string; var AMode: TSynSelectionMode; ALogStartPos: TPoint;
  var AnAction: TSynCopyPasteAction);
begin
  if Assigned(FOnTextScanNeeded) then
    FOnTextScanNeeded(EmptyStr);
end;

procedure TLazSqlXTabSheet.OnQyeryAfterPost(DataSet: TDataSet);
begin
 FQuery.ApplyUpdates;
end;


procedure TLazSqlXTabSheet.OnQueryAfterDelete(DataSet: TDataSet);
begin
 FQuery.ApplyUpdates;
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

procedure TLazSqlXTabSheet.SetOnCaretPositionChanged(
 AValue: TLazSqlXCaretPositionChanged);
begin
 if FOnCaretPositionChanged=AValue then Exit;
 FOnCaretPositionChanged:=AValue;
end;

procedure TLazSqlXTabSheet.SetOnExecutionFinished(AValue: TOnSqlExecThreadFinish
 );
begin
  if FOnExecutionFinished=AValue then Exit;
  FOnExecutionFinished:=AValue;
end;

procedure TLazSqlXTabSheet.SetOnExecutionStopped(AValue: TNotifyEvent);
begin
 if FOnExecutionStopped=AValue then Exit;
 FOnExecutionStopped:=AValue;
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


procedure TLazSqlXTabSheet.SetQueryEditor(AValue: TSynEdit);
begin
  if FQueryEditor=AValue then Exit;
  FQueryEditor:=AValue;
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

    FMessage := cmd.Message;

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

      if not cmd.IsSelect then
      DisplayMessage(FMessage,False);

    end;
  finally

    ResizeColumns;
    FCurrentExecutor := nil;
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

constructor TLazSqlXTabSheet.Create(ParentPage: TLazSqlXPageControl);
begin

  inherited Create(Parent);

  FParent:=ParentPage;

  FNumbering := IntToStr(ParentPage.PageCount);

  Caption := 'Query(' + FNumbering + ')';
  Name := 'QueryTab' + FNumbering;

  //in order to avoid dbLock don't create new connection
  if FParent.DBInfo.DbType <> dtSQLite then
  begin
    FDbInfoNew := TAsDbConnectionInfo.Create;
    FDbInfoNew.Assign(FParent.DBInfo);
  end else
  FDbInfoNew := FParent.DBInfo;

  FQuery := TAsQuery.Create(FDbInfoNew);
  FQuery.Name := 'qr' + FNumbering;
  FQuery.DataSet.AfterPost:=@OnQyeryAfterPost;
  FQuery.DataSet.AfterDelete:=@OnQueryAfterDelete;

  FDataSource := TDataSource.Create(nil);
  FDataSource.DataSet := FQuery.DataSet;
  FDataSource.Name := 'FDataSource' + FNumbering;


  FTopPanel := TPanel.Create(nil);
  FTopPanel.Name := 'FTopPanel' + FNumbering;
  FTopPanel.Caption := '';
  FQueryEditor := TSynEdit.Create(Self);
  FQueryEditor.Name := 'txtSyn' + FNumbering;
  FQueryEditor.Font.Pitch := fpFixed;
  FQueryEditor.Font.Quality := fqDraft;
  FQueryEditor.Font.Size:=8;
  if Screen.Fonts.IndexOf('Lucida Sans Typewriter')>-1 then
  begin
    FQueryEditor.Font.Name:='Lucida Sans Typewriter';
  end;
  FQueryEditor.TabWidth:=2;
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
  FDataSource.DataSet := DataSet;

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
   FTransaction.Free;
   FSplitter.Free;
   FErrorMemo.Free;
   FTopPanel.Free;
   FBottomPanel.Free;

   try
     //only sqlite connection will use main Connection
     if FParent.DBInfo.DbType<>dtSQLite then
     FDbInfoNew.Free; //sorry for memleak (if)
   except
   end;

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
  FErrorMemo.Font.Style:=[fsBold];
  if IsError then
    FErrorMemo.Font.Color := clMaroon
  else
    FErrorMemo.Font.Color := $00006200;

  FErrorMemo.Lines.Add('');
end;

procedure TLazSqlXTabSheet.ResizeDataGrid(ColumnWidth: Integer);
var
  i: Integer;
begin
  Exit;
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
  error: TAsSyntaxError=nil;
begin
  try
    if TAsDbUtils.CheckSqlSyntax(FQueryEditor.Text, error) then
    begin
      DisplayMessage('Syntax check completed', False);
    end
    else
    begin
      DisplayMessage(error.Message, True);
    end;
  finally
   if error<>nil then
    error.Free;
  end;
end;

procedure TLazSqlXTabSheet.RunQuery(EditMode: Boolean; Schema: string;
 Table: string);
var
  sqlCommand: string;
  I: integer;
  CommandExecutor: TSqlExecThread;
  c:Integer;
begin


  if ExecutionInProgress then
  begin
    if MessageDlg('Confirm', 'An execution is running. Would you like to stop current execution?',
    mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      if not FCurrentExecutor.IsTerminated then
      begin
        FCurrentExecutor.Terminate;
        if Assigned(FOnExecutionStopped) then
        begin
          while not FCurrentExecutor.IsTerminated do
          begin

          end;
          FOnExecutionStopped(Self);
          try
            FQuery.Free;
            FDbInfoNew.Free;
          except //sorry for this memleak
          end;

          FDbInfoNew := TAsDbConnectionInfo.Create;
          FDbInfoNew.Assign(FParent.DBInfo);
          //assign new reference
          FQuery := TAsQuery.Create(FDbInfoNew);
          FDataSource.DataSet := FQuery.DataSet;
        end;
        FCurrentExecutor:=nil;
      end;
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
    if FParent.DBInfo.DbType = dtOracle then
      sqlCommand := StringReplace(sqlCommand, ';', '', [rfReplaceAll]);
  end
  else
  begin
    sqlCommand := 'SELECT * FROM ' +Schema+'.'+ Table;
    if FParent.DBInfo.DbType in [dtSQLite, dtFirebirdd] then
      sqlCommand := 'SELECT * FROM ' + Table;
    Self.Caption := Table;
  end;

  try
    CommandExecutor := TSqlExecThread.Create(Schema, FQuery, @InternalOnExecutionFinished);
    FEditMode := EditMode;
    FCurrentExecutor := CommandExecutor;
    CommandExecutor.ExecuteSQL(sqlCommand, EditMode);
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

procedure TLazSqlXPageControl.SetLazSqlXTab(Index: Integer;
 AValue: TLazSqlXTabSheet);
begin

end;

procedure TLazSqlXPageControl.SetOnDataGridDblClick(AValue: TNotifyEvent);
begin
  if FOnDataGridDblClick=AValue then Exit;
  FOnDataGridDblClick:=AValue;
end;

procedure TLazSqlXPageControl.SetOnExecionStopped(AValue: TNotifyEvent);
begin
 if FOnExecutionStopped=AValue then Exit;
 FOnExecutionStopped:=AValue;
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
    if (pos(lowercase(FSynComplete.CurrentString), lowercase(s)) = 1) or
      (typ = 'FIELD') or (typ = 'VAR') then
    begin
      if (typ = 'FIELD') then
      begin
        if TAsStringUtils.ContainsChar(S, ' ') then
         S :=TAsDbUtils.SafeWrap(FDBInfo.DbType,S);
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

  FSynComplete.ItemList.Clear;
  FlstCompletionItemType.Clear;

  tbl := TAsStringUtils.RemoveChars(FSynComplete.CurrentString, ['"', '[', ']','`']);

  lstVars := TStringList.Create;
  GetVariables( (ActivePage as TLazSqlXTabSheet).QueryEditor.Text, lstVars);

  if tbl = '' then
  begin

    for I := 0 to FTables.Count - 1 do
    begin
      t := FTables[I];
      if TAsStringUtils.ContainsChar(t, ' ') then
      begin
        case DbInfo.DbType of
          dtMsSql: t := '[' + t + ']';
          dtOracle: t := '"' + t + '"'
        end;
      end;
      FlstCompletionItemType.Add('TABLE');
      FSynComplete.ItemList.Add(t);
    end;

    FSynComplete.ItemList.AddStrings(FKeywords);
    for I := 0 to FKeywords.Count - 1 do
    begin
      FlstCompletionItemType.Add('FUNC');
    end;


    for I := 0 to FProcedures.Count - 1 do
    begin
      FlstCompletionItemType.Add('PROC');
      FSynComplete.ItemList.Add(FProcedures[I]);
    end;

    for I := 0 to lstVars.Count - 1 do
    begin
      FSynComplete.ItemList.Add(StringReplace(lstVars[I], '@', '', [rfReplaceAll]));
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

function TLazSqlXPageControl.GetLazSqlXTab(Index: Integer): TLazSqlXTabSheet;
begin
 Result := inherited Page[Index] as TLazSqlXTabSheet;
end;

function TLazSqlXPageControl.OnCompletionPaintItem(const AKey: string;
 ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
var
  r: TRect;
begin
  r := Rect(x, y, FSynComplete.Width, Y + 17);
  ACanvas.Refresh;

  if Selected then
  begin
    ACanvas.GradientFill(r, clWhite, clSkyBlue, gdVertical);
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

    if (Pos(lowercase(FSynComplete.CurrentString), lowercase(s)) = 1) or
      (typ = 'FIELD') or (typ = 'VAR') then
    begin
      if (typ = 'FIELD') then
      begin
         if TAsStringUtils.ContainsChar(S, ' ') then
         S :=TAsDbUtils.SafeWrap(FDBInfo.DbType,S);
      end;
      FSynComplete.ItemList.Add(s);
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

  FSynComplete.ItemList.Clear;
  FlstCompletionItemType.Clear;
  lstVars := TStringList.Create;
  GetVariables((ActivePage as TLazSqlXTabSheet).QueryEditor.Text, lstVars);
  tbl := TAsStringUtils.RemoveChars(FSynComplete.CurrentString, ['"', '[', ']','`']);
  if tbl = '' then
  begin
    FSynComplete.ItemList.AddStrings(FTables);
    for I := 0 to FTables.Count - 1 do
    begin
      FlstCompletionItemType.Add('TABLE');
    end;

    FSynComplete.ItemList.AddStrings(FKeywords);
    for I := 0 to FKeywords.Count - 1 do
    begin
      FlstCompletionItemType.Add('FUNC');
    end;


    FSynComplete.ItemList.AddStrings(lstVars);
    for I := 0 to lstVars.Count - 1 do
    begin
      FlstCompletionItemType.Add('VAR');
    end;

    for I := 0 to FProcedures.Count - 1 do
    begin
      FlstCompletionItemType.Add('PROC');
      FSynComplete.ItemList.Add(FProcedures[I]);
    end;

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


   if FSynComplete.ItemList.Count > 0 then
     Posi := 0
   else
     Posi := -1;

  end;
  lstVars.Free;

end;

procedure TLazSqlXPageControl.SetActiveTabPositionChanged(
 AValue: TLazSqlXCaretPositionChanged);
var
 I: Integer;
begin
 FActiveTabPositionChanged:=AValue;
 for I:=0 to PageCount-1 do
 begin
  Pages[I].OnCaretPositionChanged:=FActiveTabPositionChanged;
 end;
end;

procedure TLazSqlXPageControl.SetDataGridPopUpMenu(AValue: TPopupMenu);
begin
  if FDataGridPopUpMenu=AValue then Exit;
  FDataGridPopUpMenu:=AValue;
end;

procedure TLazSqlXPageControl.SetDBInfo(AValue: TAsDbConnectionInfo);
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
  TAsRegExUtils.RunRegex(QueryText, expr, VarWords);
end;

procedure TLazSqlXPageControl.GetBracketWords(QueryText: string;
 var BracketWords: TStringList);
var
  expr: string;
begin
  expr := '\[(.*?)\]';
  TAsRegExUtils.RunRegex(QueryText, expr, BracketWords);
end;

procedure TLazSqlXPageControl.GetFieldNames(table: string; fieldMask: string;
 var fieldList: TStringList);
var
  lst: TStringList;
  I: integer;
begin

  try

    lst := TAsDbUtils.GetColumnNames(FDBInfo,table);

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
          currentString := TAsStringUtils.RemoveChars(lstDel[J],['[', ']', '"','`']);
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

          currentString := TAsStringUtils.RemoveChars(lstBracketWords[I],['[', ']', '"','`']);  //get rid of [Tablename]
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
 FlstAlias.Clear;
 FlstTableAlias.Clear;
 if PageCount>0 then
 begin
  qe := (ActivePage as TLazSqlXTabSheet).QueryEditor;
  if Trim(qe.Text) <> EmptyStr then
  begin
    for I := 0 to qe.Lines.Count - 1 do
    begin
      ScanTableAliases(qe.Lines[I]);
    end;
  end;
 end;
end;

constructor TLazSqlXPageControl.Create(AOwner: TComponent;
 DbInfo: TAsDbConnectionInfo);
begin

  inherited Create(Owner);
  FDBInfo:=DbInfo;
  FCon := DbInfo.SqlConnection;
  FZCon := DbInfo.ZeosConnection;
  //OnChange:=@OnPageChange;
  FSynComplete := TSynCompletion.Create(nil);
  // Basically do not allow a key to directly call the shortcut.
  // We do our own key handling that calls the code completion popup.
  FSynComplete.ShortCut := ShortCut(VK_UNKNOWN,[]);
  FSynComplete.CaseSensitive := False;
  FSynComplete.OnExecute := @OnCompletionExecute;
  FSynComplete.OnSearchPosition := @OnCompletionSearchPos;
  FSynComplete.ShowSizeDrag := True;
  FSynComplete.DoubleClickSelects := True;
  FSynComplete.OnPaintItem := @OnCompletionPaintItem;
  FSynComplete.EndOfTokenChr := ' ()[]:,';

  FlstCompletionItemType:= TStringList.Create;
  FlstTableAlias:= TStringList.Create;
  FlstAlias := TStringList.Create;

  FTables := TStringList.Create;
  FProcedures := TStringList.Create;
  FKeywords := TStringList.Create;
end;

destructor TLazSqlXPageControl.Destroy;
begin
  FSynComplete.Free;
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
  Tab: TLazSqlXTabSheet;
  t: TTabSheet;
begin
  Tab := TLazSqlXTabSheet.Create(Self);
  Tab.Parent:=Self;

  if Assigned(FHighlighter) then
  Tab.QueryEditor.Highlighter := FHighlighter;

  if Assigned(FQueryEditorPopUpMenu) then
  Tab.QueryEditor.PopupMenu := FQueryEditorPopUpMenu;

  if Assigned(FDataGridPopUpMenu) then
  Tab.DataGrid.PopupMenu := FDataGridPopUpMenu;

  Self.ActivePage := Tab;

  tab.OnExecutionFinished:=FOnExecutionFinished;
  tab.OnExecutionStopped:=FOnExecutionStopped;
  tab.DataGrid.OnDblClick:=FOnDataGridDblClick;
  tab.OnTextScanNeeded:=@OnTextScanNeeded; {for autocomplete,fieldnames invoke}
  tab.OnCaretPositionChanged:=FActiveTabPositionChanged;

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

procedure TLazSqlXPageControl.PopupAutoComplete;
begin
  if (Self.ActivePage is TLazSQLxTabSheet) then
    TLazSQLxTabSheet(Self.ActivePage).QueryEditor.CommandProcessor(FSynComplete.ExecCommandID,'', nil);
end;

procedure TLazSqlXPageControl.ScanNeeded;
begin
  FullScanTableAliases;
end;

end.

