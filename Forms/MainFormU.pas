unit MainFormU;

interface

uses
  Classes, SysUtils, DB, fpstdexports, fpcsvexport, fpSimpleXMLExport,
  fpsimplejsonexport, fprtfexport, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, DBGrids, DBCtrls, ComCtrls, Buttons, Menus, ActnList, StdCtrls,
  ZDataset, ZConnection, ZSqlUpdate, ZAbstractRODataset, ZStoredProcedure,
  ZSqlMetadata, SynHighlighterSQL, SynMemo, SynEdit, SynCompletion, RTTIGrids,
  LCL, LCLType, LCLIntf, Grids, EditBtn, Spin, StdActns, fpDBExport,
  fpdbfexport, fpSQLExport, sqldb, sqldblib, IBConnection, FBAdmin,
  oracleconnection, SdfData, sqlite3conn, mysql55conn, mysql51conn, mysql50conn,
  mysql40conn, mysql41conn, mssqlconn, strutils, SqlConnBuilderFormU,
  BlobFieldFormU, Clipbrd, types, EditMemoFormU, TableInfoFormU, TableInfo,
  DbType, ProcedureInfo, SqlGenerator, FtDetector, SqlExecThread, AsSqlParser,
  AsSqlKeywords, RegExpr, Regex, versionresource, MouseAndKeyInput,
  UnitGetSetText, QueryDesignerFormU, LoadingIndicator,
  SynEditMarkupSpecialLine, SynEditTypes, SynEditKeyCmds, fpsqlparser,LazSqlXCtrls,
  fpsqltree;

var
  AppVersion: string = '';

type

  { TMainForm }

  TMainForm = class(TForm)
    actExecute: TAction;
    actCloseTab: TAction;
    actExportCSV: TAction;
    actExportXML: TAction;
    actExportSQL: TAction;
    actExportRTF: TAction;
    actExportJSON: TAction;
    actConnect: TAction;
    actFormatQuery: TAction;
    actCloseAllButThis: TAction;
    actGenerateSelectQuery: TAction;
    actGenerateSelectItemQuery: TAction;
    actGenerateInsertQuery: TAction;
    actGenerateUpdateQuery: TAction;
    actGenerateDeleteQuery: TAction;
    actGenerateAllQuery: TAction;
    actGenerateSelectProc: TAction;
    actGenerateSelectItemProc: TAction;
    actGenerateInsertProc: TAction;
    actGenerateUpdateProc: TAction;
    actGenerateDeleteProc: TAction;
    actGenerateAllproc: TAction;
    actDropTable: TAction;
    actGridCopy: TAction;
    actGridCopyAll: TAction;
    actGridCopyRow: TAction;
    actGridCopyAllWithHeaders: TAction;
    actGridCopyRowsWithHeaders: TAction;
    actGridCopyRowsAsInsert: TAction;
    actDatabaseCloner: TAction;
    actDataImporter: TAction;
    actGenerateCreateScript: TAction;
    actFind: TAction;
    actCheckSyntax: TAction;
    actDisconnect: TAction;
    actDropDatabase: TAction;
    actEditTable: TAction;
    actOpenTable: TAction;
    actSelectAllRows: TAction;
    actQueryDesigner: TAction;
    actRunStoredProcedure: TAction;
    actShowStoredProcedureText: TAction;
    actNewTable: TAction;
    actRefreshTables: TAction;
    actNewTab: TAction;
    actSaveAs: TAction;
    actClose: TAction;
    ApplicationActions: TActionList;
    ApplicationProperties: TApplicationProperties;
    btnExportJson: TToolButton;
    cmbSchema: TComboBox;
    CsvExporter: TCSVExporter;
    FindDialog1: TFindDialog;
    imgLogo: TImage;
    itmDataIporter: TMenuItem;
    mitDropDB: TMenuItem;
    sep2: TMenuItem;
    mitDisconnect: TMenuItem;
    mitSelectAll: TMenuItem;
    mitFind: TMenuItem;
    mitSep7: TMenuItem;
    mitGenerateCreateScript: TMenuItem;
    mitGenerateScript: TMenuItem;
    mitQueryDesigner: TMenuItem;
    mitCloneDatabase: TMenuItem;
    mitTools: TMenuItem;
    mitSPRun: TMenuItem;
    mitSPShowText: TMenuItem;
    mitGridSep4: TMenuItem;
    mitSep8: TMenuItem;
    mitGridSep2: TMenuItem;
    mitGridSep1: TMenuItem;
    mitGridCopyAllWithHeaders: TMenuItem;
    mitGridCopyRowsWithHeaders: TMenuItem;
    mitGridCopyRow: TMenuItem;
    mitGridCopy: TMenuItem;
    mitGridCopyAll: TMenuItem;
    mitDropTable: TMenuItem;
    mitNewTable: TMenuItem;
    mitSep6: TMenuItem;
    mitRefreshTables: TMenuItem;
    mitSep5: TMenuItem;
    GridPopupMenu: TPopupMenu;
    pnlIndicator: TPanel;
    SPPopupMenu: TPopupMenu;
    btnQueryDesigner: TToolButton;
    btnDatabseCloner: TToolButton;
    btnDataImporter: TToolButton;
    SqlConn: TSQLConnector;
    LibraryLoader: TSQLDBLibraryLoader;
    btnCheckSyntax: TToolButton;
    Transaction: TSQLTransaction;
    txtSearchTable: TEdit;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    ApplicationImages: TImageList;
    lstTables: TListBox;
    lstProcedures: TListBox;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mitTabExportToSqlInsert: TMenuItem;
    mitTabExportToRTF: TMenuItem;
    mitTabExportToJason: TMenuItem;
    mitTabExportToXml: TMenuItem;
    mitTabExportToCSV: TMenuItem;
    mitDataExport: TMenuItem;
    mitSep4: TMenuItem;
    mitCloseAllButActive: TMenuItem;
    mitAllProc: TMenuItem;
    mitSep3: TMenuItem;
    mitDeleteProc: TMenuItem;
    mitUpdateProc: TMenuItem;
    mitInsertProc: TMenuItem;
    mitSelectItemProc: TMenuItem;
    mitSelectProc: TMenuItem;
    mitGenerateSP: TMenuItem;
    mitSep2: TMenuItem;
    mitQueryAll: TMenuItem;
    mitEditorSelectAll: TMenuItem;
    mitEditorSep2: TMenuItem;
    mitEditorPaste: TMenuItem;
    mitEditorCopy: TMenuItem;
    mitEditorCut: TMenuItem;
    mitEditorUndo: TMenuItem;
    mitEditorSep1: TMenuItem;
    mitDeleteQuery: TMenuItem;
    mitUpdateQuery: TMenuItem;
    mitInsertQuery: TMenuItem;
    mitSelectItemQuery: TMenuItem;
    mitSelectQuery: TMenuItem;
    mitQuery: TMenuItem;
    mitTableInfo: TMenuItem;
    mitOpenData: TMenuItem;
    mitSep10: TMenuItem;
    mitConnect: TMenuItem;
    MenuItem2: TMenuItem;
    mitSqlExport: TMenuItem;
    mitJsonExport: TMenuItem;
    mitRtfExport: TMenuItem;
    mitExportToXml: TMenuItem;
    mitExportToCSV: TMenuItem;
    mitExport: TMenuItem;
    pgcLeft: TPageControl;
    pnlMain: TPanel;
    TableListPopupMenu: TPopupMenu;
    pnlTables: TPanel;
    QueryEditorPopupMenu: TPopupMenu;
    RtfExporter: TRTFExporter;
    JsonExporter: TSimpleJSONExporter;
    sbMain: TStatusBar;
    Splitter2: TSplitter;
    SqlExporter: TSQLExporter;
    tabTables: TTabSheet;
    tabProcedures: TTabSheet;
    tlbMain: TToolBar;
    btnConnect: TToolButton;
    ToolButton1: TToolButton;
    btnSave: TToolButton;
    ToolButton2: TToolButton;
    btnExportCsv: TToolButton;
    btnExportXml: TToolButton;
    btnExecute: TToolButton;
    ToolButton4: TToolButton;
    btnNew: TToolButton;
    ToolButton6: TToolButton;
    txtSearchproc: TEdit;
    XmlExporter: TSimpleXMLExporter;
    ApplicationMainMenu: TMainMenu;
    OpenDialog: TOpenDialog;
    pmiClose: TMenuItem;
    pmiNew: TMenuItem;
    mitClose: TMenuItem;
    mitSep1: TMenuItem;
    mitSaveQuery: TMenuItem;
    mitFile: TMenuItem;
    TabPopupMenu: TPopupMenu;
    SaveDialog: TSaveDialog;
    SqlSyntax: TSynSQLSyn;
    ZCon: TZConnection;
    procedure actCheckSyntaxExecute(Sender: TObject);
    procedure actDatabaseClonerExecute(Sender: TObject);
    procedure actCloseAllButThisExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actDropDatabaseExecute(Sender: TObject);
    procedure actDropTableExecute(Sender: TObject);
    procedure actEditTableExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCloseTabExecute(Sender: TObject);
    procedure actExportCSVExecute(Sender: TObject);
    procedure actExportDBFExecute(Sender: TObject);
    procedure actExportSQLExecute(Sender: TObject);
    procedure actExportXMLExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFormatQueryExecute(Sender: TObject);
    procedure actGenerateAllprocExecute(Sender: TObject);
    procedure actGenerateAllQueryExecute(Sender: TObject);
    procedure actGenerateCreateScriptExecute(Sender: TObject);
    procedure actGenerateDeleteProcExecute(Sender: TObject);
    procedure actGenerateDeleteQueryExecute(Sender: TObject);
    procedure actGenerateInsertProcExecute(Sender: TObject);
    procedure actGenerateInsertQueryExecute(Sender: TObject);
    procedure actGenerateSelectItemProcExecute(Sender: TObject);
    procedure actGenerateSelectItemQueryExecute(Sender: TObject);
    procedure actGenerateSelectProcExecute(Sender: TObject);
    procedure actGenerateSelectQueryExecute(Sender: TObject);
    procedure actGenerateUpdateProcExecute(Sender: TObject);
    procedure actGenerateUpdateQueryExecute(Sender: TObject);
    procedure actGridCopyAllExecute(Sender: TObject);
    procedure actGridCopyAllWithHeadersExecute(Sender: TObject);
    procedure actGridCopyExecute(Sender: TObject);
    procedure actGridCopyRowExecute(Sender: TObject);
    procedure actGridCopyRowsAsInsertExecute(Sender: TObject);
    procedure actGridCopyRowsWithHeadersExecute(Sender: TObject);
    procedure actDataImporterExecute(Sender: TObject);
    procedure actNewTabExecute(Sender: TObject);
    procedure actExportJSONExecute(Sender: TObject);
    procedure actExportRTFExecute(Sender: TObject);
    procedure actNewTableExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actOpenTableExecute(Sender: TObject);
    procedure actQueryDesignerExecute(Sender: TObject);
    procedure actRefreshTablesExecute(Sender: TObject);
    procedure actRunStoredProcedureExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSelectAllRowsExecute(Sender: TObject);
    procedure actShowStoredProcedureTextExecute(Sender: TObject);
    procedure ApplicationPropertiesActivate(Sender: TObject);
    procedure ApplicationPropertiesDropFiles(Sender: TObject;
      const FileNames: array of string);
    procedure ApplicationPropertiesException(Sender: TObject; E: Exception);
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: boolean);
    procedure cmbSchemaChange(Sender: TObject);

    procedure EditSelectAll1Execute(Sender: TObject);
    procedure EditSpecialField(Sender: TObject);   {*****Edit Special Fields*****}
    procedure EditUndo1Execute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lstTablesDblClick(Sender: TObject);
    procedure lstTablesDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure MenuItem3Click(Sender: TObject);
    procedure mitOpenDataClick(Sender: TObject);
    procedure mitRefreshTablesClick(Sender: TObject);
    procedure OnDynamicEditKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure pgcMainChange(Sender: TObject);
    procedure pgcMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pmiCloseClick(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure sbMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure Splitter1CanOffset(Sender: TObject; var NewOffset: integer;
      var Accept: boolean);
    procedure tlbMainClick(Sender: TObject);
    procedure txtSearchprocChange(Sender: TObject);
    procedure txtSearchprocEnter(Sender: TObject);
    procedure txtSearchprocExit(Sender: TObject);
    procedure txtSearchprocKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure txtSearchprocKeyPress(Sender: TObject; var Key: char);
    procedure txtSearchTableChange(Sender: TObject);
    procedure txtSearchTableEnter(Sender: TObject);
    procedure txtSearchTableExit(Sender: TObject);
    procedure txtSearchTableKeyPress(Sender: TObject; var Key: char);



  private

    FtableIcon: TBitmap;
    FfunctionIcon: TBitmap;
    FfieldIcon: TBitmap;
    FvarIcon: TBitmap;
    FprocedureIcon: TBitmap;

    FPageControl:TLazSqlXPageControl;

    FLoadingIndicator: TLoadingIndicator;


    FFound: boolean;
    FPos: integer;

    FQuickSearchLastWord: string;
    FExecutionInProgress: boolean;
    FConnected: boolean;
    FCurrentExecutor: TSqlExecThread;

    procedure DoDisconnect;
    procedure DoSelectiveConnect;
    procedure UpdateGUI(IsConnected: boolean);
    procedure Connect;
    procedure CreateIntelliSense;
    procedure DoExport(exporter: TCustomDatasetExporter; FileExt: string);
    procedure EnableExporters;

    procedure FillSchemas;
    procedure FillTables;
    procedure FillpProcedures;

    procedure RemoveTabPage(tab: TTabSheet);

    procedure RunProcedure(procname: string);
    function GetProcedureText(procname: string): string;

    function getCurrentType: TDatabaseType;
    procedure OnFieldGetText(Sender: TField; var aText: string; DisplayText: boolean);
    procedure OnFieldSetText(Sender: TField; const aText: string);

    procedure ExecuteQuery(IsTableData: boolean);

    procedure GetFieldNames(table: string; fieldMask: string;
      var fieldList: TStringList);
    procedure GenerateSqlQuery(table: string; queryType: TQueryType;
      IsStoredProcedure: boolean);

    procedure CopySelectedRows(IncludeHeaders: boolean);
    procedure CopySelectedRowsAsSqlInsert(tblName: string);
    procedure CopyAllRows(IncludeHeaders: boolean);

    procedure FindText(aText: string);
    procedure QuickSearchTables;
    procedure QuickSearchProcedure;

    procedure ResizeGridColumns(ColumnWidth: integer = 100);
    function ParseSql(Sql: string; out ResultStr: string): boolean;
    procedure OnExecutionFinished(Sender: TObject; IsTableData: boolean);
    procedure OnDBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: integer; Column: TColumn; State: TGridDrawState);
    procedure OnQueryAfterPost(DataSet: TDataSet);
  public
    DbInfo: TDbConnectionInfo;
    ArrowImageLeft: TBitmap;
    ArrowImageRight: TBitmap;
    RectImage: TBitmap;

  end;



var
  MainForm: TMainForm;
implementation

uses AboutFormU, DatabaseClonerFormU, ProgressFormU, DataImporterFormU,
  DataImporterDialogU, Utils, AsStringUtils, AsDatabaseCloner;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.OnDBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: integer; Column: TColumn; State: TGridDrawState);
var
  //determine if we're going to override normal Lazarus draw routines
  OverrideDraw: boolean;
  OurDisplayString: string;
  CurrentField: TField;
  DataRow: integer;
begin
  OverrideDraw := False;

  // Make sure selected cells are highlighted
  if (gdSelected in State) then
  begin
    (Sender as TDBGrid).Canvas.Brush.Color := clHighlight;
  end
  else
  begin
    (Sender as TDBGrid).Canvas.Brush.Color := (Sender as TDBGrid).Color;
  end;

  // Draw background in any case - thanks to ludob on the forum:
  (Sender as TDBGrid).Canvas.FillRect(Rect);

  //Foreground
  try
    CurrentField := Column.Field;
    if CurrentField.DataType = ftMemo then
    begin
      OverrideDraw := True;
    end;
  except
    on E: Exception do
    begin
      // We might have an inactive datalink or whatever,
      // in that case, pass on our problems to the LCL
      OverrideDraw := False;
    end;
  end;

  //Exception: fixed header should always be drawn like normal:
  // this never gets picked up as OnDrawColumnCell apparently only deals with data cells!!!
  if (gdFixed in State) then
  begin
    OverrideDraw := False;
  end;

  if OverrideDraw = False then
  begin
    // Call normal procedure to handle drawing for us.
    (Sender as TDBGrid).DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end
  else
  begin
    // Get to work displaying our memo contents
    // Basically shamelessly ripped from
    // DefaultDrawColumnCell
    OurDisplayString := '';
    if CurrentField <> nil then
    begin
      //DO display memo ;) OurDisplayString is string to be displayed
      try
        OurDisplayString := CurrentField.AsString; //DisplayText will only show (Memo)
      except
        // Ignore errors; use empty string as specified above
      end;
    end;
    //Actual foreground drawing, taken from Grids.DrawCellText coding:
    (Sender as TDBGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, OurDisplayString);
  end;
end;

procedure TMainForm.OnQueryAfterPost(DataSet: TDataSet);
var
  ds: TSQLQuery;
  I: integer;
begin
  if (DataSet is TSQLQuery) then
  begin
    ds := (DataSet as TSQLQuery);
    I := ds.RecNo;
    ds.ApplyUpdates;
    Transaction.Commit;

    if not ds.Active then
    begin
      ds.Open;
      ds.RecNo := I;
      ResizeGridColumns(100);
    end;
  end;
end;

procedure TMainForm.GetFieldNames(table: string; fieldMask: string;
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
        SqlConn.GetFieldNames(table, lst);
      end;
      dtMySql: ZCon.GetColumnNames(table, '', lst);
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

procedure TMainForm.GenerateSqlQuery(table: string; queryType: TQueryType;
  IsStoredProcedure: boolean);
var
  p: TProcedureNames;
  s: TSqlGenerator;
  tis: TTableInfos;
  ti: TTableInfo;
  outPut: TStringList;
  tab:TLazSqlXTabSheet;
begin

  try
    FLoadingIndicator.StartAnimation;
    sbMain.Panels[1].Text := 'Generating query...';
    Application.ProcessMessages;


    p.PnDelete := table + '_delete';
    p.PnInsert := table + '_insert';
    p.PnSelect := table + '_select';
    p.PnSelectItem := table + '_selectItem';
    p.PnUpdate := table + '_update';

    outPut := TStringList.Create;

    actSaveAs.Enabled := True;
    actExecute.Enabled := True;

    try
      s := TSqlGenerator.Create(ZCon.HostName, ZCon.Database, ZCon.User,
        ZCon.Password, p, SqlConnBuilderForm.ConDatabaseType, ZCon.Port);
      tis := TTableInfos.Create(ZCon.HostName, ZCon.Database, ZCon.User,
        ZCon.Password, SqlConnBuilderForm.ConDatabaseType, ZCon.Port);
      ti := tis.GetTableInfo(cmbSchema.Text, table);

      if not IsStoredProcedure then
        Output := s.GenerateQuery(0, ti, queryType)
      else
        outPut.Text := s.GenerateStoredProcedure(ti, queryType);

      tab:=FPageControl.AddTab;

      tab.QueryEditor.Lines.Add('');
      tab.QueryEditor.Lines.Text := tab.QueryEditor.Lines.Text + outPut.Text;
    except
      on e: Exception do
        ShowMessage(e.Message);
    end;
    FPageControl.ScanNeeded;
  finally
    FLoadingIndicator.StopAnimation;
    sbMain.Panels[1].Text := EmptyStr;
    tis.Free;
    ti.Free;
    s.Free;
  end;
end;

procedure TMainForm.CopySelectedRows(IncludeHeaders: boolean);
var
  RowStrings: TStringList;
  row, strGuid: string;
  blob: TBlobField;
  I, J: integer;
  g: TGuid;
  Hold_Bytes: TBytesField;
  dsize: integer;
begin
  RowStrings := TStringList.Create;
  try
    with FPageControl.ActiveTab.DataGrid do
    begin
      RowStrings.BeginUpdate();
      DataSource.DataSet.DisableControls();
      try

        if IncludeHeaders then
        begin
          for J := 0 to DataSource.DataSet.FieldCount - 1 do
          begin
            row := row + DataSource.DataSet.Fields[J].FieldName + #9;
          end;
          RowStrings.Add(Row);
        end;

        for i := 0 to (SelectedRows.Count - 1) do
        begin
          Datasource.DataSet.GotoBookmark(Pointer(SelectedRows[i]));
          row := '';
          for J := 0 to DataSource.DataSet.FieldCount - 1 do
          begin
            if DataSource.DataSet.Fields[J].IsBlob then
            begin
              strGuid := TAsStringUtils.BlobToString(DataSource.DataSet.Fields[J]);
              row := row + strGuid + #9;
            end
            else
            begin
              row := row + DataSource.DataSet.Fields[J].AsString + #9;
            end;
          end;
          RowStrings.Add(row);
        end;
      finally
        DataSource.DataSet.EnableControls();
        RowStrings.EndUpdate();
      end;
    end;
    Clipboard.AsText := RowStrings.Text;
  finally
    RowStrings.Free;
  end;
end;

procedure TMainForm.CopySelectedRowsAsSqlInsert(tblName: string);
var
  RowStrings: TStringList;
  row: string;
  blob: TBlobField;
  I, J: integer;
  FieldNames: string;
  FieldValue: string;
begin
  RowStrings := TStringList.Create;
  try
    with FPageControl.ActiveTab.DataGrid do
    begin
      RowStrings.BeginUpdate();
      DataSource.DataSet.DisableControls();
      try

        for I := 0 to DataSource.DataSet.FieldCount - 1 do
        begin
          FieldNames := FieldNames + DataSource.DataSet.Fields[I].FieldName +
            LoopSeperator[integer(I < DataSource.DataSet.FieldCount - 1)];
        end;

        for i := 0 to (SelectedRows.Count - 1) do
        begin
          Datasource.DataSet.GotoBookmark(Pointer(SelectedRows[i]));
          row := '';
          for J := 0 to DataSource.DataSet.FieldCount - 1 do
          begin

            if not DataSource.DataSet.Fields[J].IsNull then
              if DataSource.DataSet.Fields[J].IsBlob then
              begin
                FieldValue := TAsStringUtils.BlobToString(DataSource.DataSet.Fields[J]);
              end
              else
              begin
                FieldValue := DataSource.DataSet.Fields[J].AsString;
              end;

            if DataSource.DataSet.Fields[J].IsNull then
              FieldValue := 'NULL'
            else
              FieldValue := '''' + FieldValue + '''';

            row := row + FieldValue + LoopSeperator[integer(
              J < DataSource.DataSet.FieldCount - 1)];

          end;
          RowStrings.Add('INSERT INTO ' + tblName + '(' + Trim(FieldNames) +
            ') VALUES (' + Trim(row) + ')');
        end;
      finally
        DataSource.DataSet.EnableControls();
        RowStrings.EndUpdate();
      end;
    end;
    Clipboard.AsText := RowStrings.Text;
  finally
    RowStrings.Free;
  end;
end;

procedure TMainForm.CopyAllRows(IncludeHeaders: boolean);
var
  RowStrings: TStringList;
  row, strGuid: string;
  J: integer;
  b: TBookmark;
  blob: TBlobField;
begin
  RowStrings := TStringList.Create;
  try
    with FPageControl.ActiveTab.DataGrid do
    begin
      if (SelectedRows.Count > 0) then
        b := SelectedRows[0];

      if IncludeHeaders then
      begin
        for J := 0 to DataSource.DataSet.FieldCount - 1 do
        begin
          row := row + DataSource.DataSet.Fields[J].FieldName + #9;
        end;
        RowStrings.Add(Row);
      end;

      RowStrings.BeginUpdate();
      DataSource.DataSet.DisableControls();
      DataSource.DataSet.First;
      try
        while not DataSource.DataSet.EOF do
        begin
          row := '';
          for J := 0 to DataSource.DataSet.FieldCount - 1 do
          begin
            if DataSource.DataSet.Fields[J].IsBlob then
            begin
              strGuid := TAsStringUtils.BlobToString(DataSource.DataSet.Fields[J]);
              row := row + strGuid + #9;
            end
            else
            begin
              row := row + DataSource.DataSet.Fields[J].AsString + #9;
            end;
          end;
          RowStrings.Add(row);
          DataSource.DataSet.Next;
        end;
      finally
        DataSource.DataSet.EnableControls();
        RowStrings.EndUpdate();
      end;

      if b <> nil then
        try
          DataSource.DataSet.GotoBookmark(b);
        except
        end;
    end;
    Clipboard.AsText := RowStrings.Text;
  finally
    RowStrings.Free;
  end;
end;

procedure TMainForm.FindText(aText: string);
var
  FindS: string;
  IPos, FLen, SLen: integer; {Internpos, Lengde søkestreng, lengde memotekst}
  Res: integer;
  qe:TSynEdit;
begin

  {FPos is global}
  FFound := False;
  FLen := Length(aText);
  SLen := Length(qe.Text);
  FindS := aText;

  qe:=FPageControl.ActiveTab.QueryEditor;

  //following 'if' added by mike
  if frMatchcase in findDialog1.Options then
    IPos := Pos(FindS, Copy(qe.Text, FPos + 1, SLen - FPos))
  else
    IPos := Pos(AnsiUpperCase(FindS), AnsiUpperCase(
      Copy(qe.Text, FPos + 1, SLen - FPos)));

  if IPos > 0 then
  begin
    FPos := FPos + IPos;
    qe.SetFocus;
    Self.ActiveControl := qe;
    qe.SelStart := FPos;  // -1;   mike   {Select the string FFound by POS}
    qe.SelEnd := FPos + Length(aText);
    FFound := True;
    FPos := FPos + FLen - 1;   //mike - move just past end of FFound item
  end
  else
  begin
    FPos := 0;     //mike  nb user might cancel dialog, so setting here is not enough
  end;

end;

procedure TMainForm.QuickSearchTables;
var
  sPos: integer;
begin
  if (lstTables.Count < 1) or (lstTables.ItemIndex < 0) then
    exit;
  if (FQuickSearchLastWord = txtSearchTable.Text) then
    sPos := lstTables.ItemIndex
  else
    sPos := 0;

  lstTables.ItemIndex := TControlUtils.FindItem(lstTables.Items, txtSearchTable.Text, sPos);
  FQuickSearchLastWord := txtSearchTable.Text;
end;

procedure TMainForm.QuickSearchProcedure;
var
  sPos: integer;
begin
  if (lstProcedures.Count < 1) or (lstProcedures.ItemIndex < 0) then
    exit;

  if (FQuickSearchLastWord = txtSearchproc.Text) then
    sPos := lstProcedures.ItemIndex
  else
    sPos := 0;

  lstProcedures.ItemIndex := TControlUtils.FindItem(
    lstProcedures.Items, txtSearchproc.Text, sPos);
  FQuickSearchLastWord := txtSearchproc.Text;
end;

procedure TMainForm.ResizeGridColumns(ColumnWidth: integer);
var
  I: integer;
  qe:TDBGrid;
begin
  qe:=FPageControl.ActiveTab.DataGrid;
  qe.BeginUpdate;
  try
    for I := 0 to qe.Columns.Count - 1 do
    begin
      qe.Columns[I].Width := ColumnWidth;
    end;
  finally
    qe.EndUpdate(True);
  end;
end;

function TMainForm.ParseSql(Sql: string; out ResultStr: string): boolean;
var
  parser: TSQLParser;
  m: TMemoryStream;
  script: TStringList;
  ResultList: TSQLElementList;
  I: integer;
  output: TStringList;
begin
  m := TMemoryStream.Create;
  script := TStringList.Create;
  output := TStringList.Create;
  script.Text := Sql;
  script.SaveToStream(m);
  parser := TSQLParser.Create(m);
  try
    ResultList := parser.ParseScript(False);
    for I := 0 to ResultList.Count - 1 do
    begin
      output.Add(ResultList[i].GetAsSQL([sfoDoubleQuoteIdentifier]));
    end;
    ResultStr := Output.Text;
  finally
    script.Free;
    output.Free;
    parser.Free;
    m.Free;
  end;

end;

procedure TMainForm.OnExecutionFinished(Sender: TObject; IsTableData: boolean);
begin
  FLoadingIndicator.StopAnimation;
  sbMain.Panels[1].Text:=FPageControl.ActiveTab.Message;
  EnableExporters;
end;


procedure TMainForm.pgcMainChange(Sender: TObject);
begin

end;

procedure TMainForm.pgcMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbMiddle then
    actCloseTab.Execute;
end;

procedure TMainForm.pmiCloseClick(Sender: TObject);
begin

end;

procedure TMainForm.ReplaceDialog1Replace(Sender: TObject);
begin

end;

procedure TMainForm.sbMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  if Panel = StatusBar.Panels[1] then
  begin
    StatusBar.Canvas.Brush.Style := bsClear;
    StatusBar.Canvas.TextOut(Rect.Left + 18, Rect.Top + 1, Panel.Text);
  end;
end;

procedure TMainForm.Splitter1CanOffset(Sender: TObject; var NewOffset: integer;
  var Accept: boolean);
begin

end;

procedure TMainForm.tlbMainClick(Sender: TObject);
begin

end;

procedure TMainForm.txtSearchprocChange(Sender: TObject);
begin
  QuickSearchProcedure;
end;

procedure TMainForm.txtSearchprocEnter(Sender: TObject);
begin
  txtSearchproc.Clear;
  txtSearchproc.Font.Color := clWindowText;
end;

procedure TMainForm.txtSearchprocExit(Sender: TObject);
begin
  txtSearchproc.Font.Color := clSilver;
  txtSearchproc.Text := 'Search Procedure';
end;

procedure TMainForm.txtSearchprocKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin

end;

procedure TMainForm.txtSearchprocKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    QuickSearchProcedure;
end;

procedure TMainForm.txtSearchTableChange(Sender: TObject);
begin
  QuickSearchTables;
end;

procedure TMainForm.txtSearchTableEnter(Sender: TObject);
begin
  txtSearchTable.Clear;
  txtSearchTable.Font.Color := clWindowText;
end;

procedure TMainForm.txtSearchTableExit(Sender: TObject);
begin
  txtSearchTable.Font.Color := clSilver;
  txtSearchTable.Text := 'Search Table';
end;

procedure TMainForm.txtSearchTableKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    QuickSearchTables;
end;


procedure TMainForm.Connect;
var
  p: TProcedureNames;
  wt: TStringWrapType;
begin

  DoDisconnect;
  SqlConnBuilderForm.txtAdvancedProperties.Text := ZCon.Properties.Text;

  if SqlConnBuilderForm.ShowModal = mrOk then
  begin
    DbInfo := SqlConnBuilderForm.DbInfo;
    FPageControl.DBInfo:=DbInfo;

    ZCon.Properties.Text := SqlConnBuilderForm.txtAdvancedProperties.Text;
    if SqlConnBuilderForm.chkAlternateLibLocation.Checked then
    begin
      ZCon.LibraryLocation := SqlConnBuilderForm.txtLibraryFilename.Text;
      LibraryLoader.LibraryName := SqlConnBuilderForm.txtLibraryFilename.Text;
      LibraryLoader.ConnectionType :=
        TDbUtils.DatabaseTypeAsConnectorType(DbInfo.DatabaseType);
      LibraryLoader.Enabled := True;
    end
    else
    begin
      LibraryLoader.Enabled := False;
      ZCon.LibraryLocation := EmptyStr;
      LibraryLoader.LibraryName := '';
    end;

    lstTables.Clear;

    QueryDesignerForm.Clear;

    if DbInfo.DatabaseType = dtOracle then
      ZCon.Database := SqlConnBuilderForm.OracleDatabaseDescriptor
    else if DbInfo.DatabaseType = dtMsSql then
    begin
      ZCon.Database := TAsStringUtils.WrapString(DbInfo.Database,
        TStringWrapType.swtBrackets);
    end
    else
    begin
      ZCon.Database := DbInfo.Database;
    end;

    ZCon.HostName := DbInfo.Server;
    ZCon.User := DbInfo.Username;
    ZCon.Password := DbInfo.Password;
    ZCon.Protocol := TDbUtils.DatabaseTypeAsString(DbInfo.DatabaseType, True);
    ZCon.Port := DbInfo.Port;
    SqlSyntax.TableNames.Clear;
    SqlSyntax.TableNames.AddStrings(lstTables.Items);


    SqlConn.ConnectorType := TDbUtils.DatabaseTypeAsConnectorType(DbInfo.DatabaseType);
    SqlConn.UserName := DbInfo.Username;
    SqlConn.Password := DbInfo.Password;
    SqlConn.HostName := DbInfo.Server;
    SqlConn.DatabaseName := DbInfo.Database;


    try

      DoSelectiveConnect;

      if FPageControl.PageCount = 0 then
      begin
        actNewTabExecute(nil);
      end;

      UpdateGUI(True);

      FillSchemas;
      FillTables;
      FillpProcedures;
      pgcLeft.ActivePageIndex := 0;

    except
      on e: Exception do
        ShowMessage(e.Message);
    end;
  end;
end;

procedure TMainForm.UpdateGUI(IsConnected: boolean);
begin

  if IsConnected then
  begin
    pnlTables.Visible := True;
    FPageControl.Visible := True;
    pnlMain.Color := clDefault;
    lstTables.PopupMenu := TableListPopupMenu;
    lstProcedures.PopupMenu := SPPopupMenu;
    actNewTab.Enabled := True;
    tabProcedures.TabVisible := DbInfo.DatabaseType <> dtSQLite;
    mitGenerateSP.Visible := tabProcedures.TabVisible;
    actGenerateAllproc.Visible := tabProcedures.TabVisible;
    actGenerateSelectItemProc.Visible := tabProcedures.TabVisible;
    actGenerateInsertProc.Visible := tabProcedures.TabVisible;
    actGenerateUpdateProc.Visible := tabProcedures.TabVisible;
    actGenerateDeleteProc.Visible := tabProcedures.TabVisible;
    actGenerateSelectProc.Visible := tabProcedures.TabVisible;
    sbMain.Panels[0].Text := DbInfo.Server + '/' + DbInfo.Database;
  end
  else
  begin
    FPageControl.Visible := False;
    pnlTables.Visible := False;
    pnlMain.Color := clWindow;
  end;

end;

procedure TMainForm.DoSelectiveConnect;
begin

    case DbInfo.DbEngine of
      deSqlDB:
      begin
        SqlConn.Open;
        FConnected := True;
      end;
      deZeos:
      begin
        ZCon.Connect;
        FConnected := True;
      end;
    end;

end;

procedure TMainForm.DoDisconnect;
begin
  ZCon.Disconnect;
  SqlConn.Close(True);
  FConnected := ZCon.Connected and SqlConn.Connected;
end;


procedure TMainForm.DoExport(exporter: TCustomDatasetExporter; FileExt: string);
var
  tblName: string;
  se: TSQLExporter;
  ds: TDataSource;
var
  i: integer;
begin

  ds := FPageControl.ActiveTab.DataSource;

  if ds = nil then
  begin
    ShowMessage('Active data source is null');
    Exit;
  end;

  if not ds.DataSet.Active then
  begin
    ShowMessage('There is no data to export. Please run a query then try again.');
    Exit;
  end;

  with TSaveDialog.Create(nil) do
  begin
    try

      if (exporter is TSQLExporter) then
      begin
        se := exporter as TSQLExporter;
        tblName := InputBox('Input', 'Tablename', lstTables.Items[lstTables.ItemIndex]);
        se.FormatSettings.TableName := tblName;

        se.ExportFields.Clear;

        for i := 0 to se.Dataset.Fields.Count - 1 do
        begin
          if (se.Dataset.Fields[I].DataType <> ftAutoInc) and
            (se.Dataset.Fields[I].DataType <> ftGuid) then
          begin
            se.ExportFields.AddField(se.Dataset.Fields[I].FieldName);
          end;
        end;
      end;

      DefaultExt := FileExt;
      Filter := '(*' + FileExt + ')|*' + FileExt;
      if Execute then
      begin
        (exporter as TCustomFileExporter).FileName := FileName;
        ds.DataSet.DisableControls;
        try
          exporter.Dataset := ds.DataSet;
          exporter.Dataset.First;
          exporter.Execute;
        finally
          ds.DataSet.First;
          ds.DataSet.EnableControls;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TMainForm.EnableExporters;
begin
  try
    actExportCSV.Enabled := (FPageControl.ActiveTab.Query.Active or FPageControl.ActiveTab.ZQuery.Active);

    actExportXML.Enabled := actExportCSV.Enabled;
    actExportRTF.Enabled := actExportCSV.Enabled;

    actExportSQL.Enabled := actExportCSV.Enabled;


    actExportJSON.Enabled := actExportCSV.Enabled;


  except
  end;
end;

procedure TMainForm.CreateIntelliSense;
begin

end;

procedure TMainForm.FillSchemas;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    cmbSchema.Clear;
    TDbUtils.GetSchemas(DbInfo, list);
    cmbSchema.Items.AddStrings(list);
  finally
    list.Free;
  end;


  if DbInfo.DatabaseType = dtOracle then
    cmbSchema.ItemIndex := cmbSchema.Items.IndexOf(SqlConn.UserName)
  else
    cmbSchema.ItemIndex := 0;
end;

procedure TMainForm.FillTables;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    lstTables.Clear;
    TDbUtils.GetTablenames(DbInfo, cmbSchema.Items[cmbSchema.ItemIndex], list);
    lstTables.Items.AddStrings(list);

    FPageControl.Tables.Clear;
    FPageControl.Tables.AddStrings(list);

  finally
    list.Free;
  end;

  if lstTables.Count > 0 then
    lstTables.ItemIndex := 0;



end;

procedure TMainForm.FillpProcedures;
var
  lst: TStringList;
  procInfo: TProcedureInfo;
begin
  procInfo := TProcedureInfo.Create(Self, cmbSchema.Text, ZCon.HostName,
    ZCon.Database, ZCon.User, ZCon.Password, DbInfo.DatabaseType, ZCon.Port);
  lstProcedures.Clear;
  try
    lst := procInfo.GetProcedureNames;
    lstProcedures.Items.AddStrings(lst);
    if lstProcedures.Count > 0 then
      lstProcedures.ItemIndex := 0;
  finally
    procInfo.Free;
    lst.Free;
  end;

end;


procedure TMainForm.RemoveTabPage(tab: TTabSheet);
begin

end;


procedure TMainForm.RunProcedure(procname: string);
var
  I: integer;
  lst: TList;
  s: string;
  ProcInfo: TProcedureInfo;
  proc: TDataSet;
  tab:TLazSqlXTabSheet;
begin

  try
    ProcInfo := TProcedureInfo.Create(Self, cmbSchema.Text,
      ZCon.HostName, ZCon.Database, ZCon.User, ZCon.Password, DbInfo.DatabaseType, ZCon.Port);

    try
      proc := ProcInfo.RunProcedure(procname);
      if proc <> nil then
      begin

        actNewTab.Execute;
        tab:=FPageControl.ActiveTab;
        tab.InsertComponent(proc);
        tab.DataSource.DataSet := proc;
        tab.QueryEditor.Parent.Visible := False;
        tab.DataGrid.Parent.Align := alClient;
        tab.DbNavigator.Visible := True;
        tab.Caption := procname + ' [ProcedureResult]';
        tab.Tag := 0;
        tab.DbNavigator.Visible := False;
        tab.ResizeDataGrid;
      end;
    except
      on e: Exception do
      begin
        ShowMessage(e.Message);
        actCloseTab.Execute;
      end;
    end;

  finally
  end;

end;

function TMainForm.GetProcedureText(procname: string): string;
var
  qr: TZQuery;
begin

  qr := TZQuery.Create(nil);
  qr.Connection := ZCon;
  try

    case DbInfo.DatabaseType of
      dtMsSql:
      begin
        qr.SQL.Text := 'sp_helptext ''' + procname + '''';
      end;
      dtOracle:
      begin
        qr.SQL.Text := 'select text from user_source where name = ''' +
          procname + ''' order by line';
      end;
      dtMySql:
      begin
        qr.SQL.Text := 'SELECT ROUTINE_DEFINITION FROM INFORMATION_SCHEMA.ROUTINES ' +
          ' WHERE ROUTINE_SCHEMA = ''' + ZCon.Database +
          ''' AND ROUTINE_TYPE = ''PROCEDURE'' AND ROUTINE_NAME = "' + procname + '";';
      end;
      dtFirebirdd:
      begin
        qr.SQL.Text :=
          'SELECT r.RDB$PROCEDURE_SOURCE FROM RDB$PROCEDURES r where r.RDB$PROCEDURE_NAME=''' + procname
          + '''';
      end;
    end;

    if DbInfo.DatabaseType <> dtSQLite then
    begin
      qr.Open;

      while not qr.EOF do
      begin
        Result := Result + qr.Fields[0].AsString;
        qr.Next;
      end;

    end;

  finally
    qr.Free;
  end;

end;

function TMainForm.getCurrentType: TDatabaseType;
begin
  Result := SqlConnBuilderForm.ConDatabaseType;
end;

procedure TMainForm.OnFieldGetText(Sender: TField; var aText: string;
  DisplayText: boolean);
begin
  aText := AnsiToUtf8(Sender.AsString);
  if Sender.IsBlob then
  begin
    aText := TAsStringUtils.BlobToString(Sender);
  end;
end;

procedure TMainForm.OnFieldSetText(Sender: TField; const aText: string);
begin
  if Sender <> nil then
  begin
    Sender.Value := Utf8ToAnsi(aText);
  end;

end;

procedure TMainForm.ExecuteQuery(IsTableData: boolean);
begin
 FLoadingIndicator.StartAnimation;
 FPageControl.ActiveTab.RunQuery(IsTableData,cmbSchema.Text,lstTables.Items[lstTables.ItemIndex] );
end;



procedure TMainForm.EditSpecialField(Sender: TObject);
var
  blob: TBlobField;
  grd: TDBGrid;
  DialogResult: integer;
  fDetect: TFileDetector;
  fType: TFileType;
  m: TMemoryStream;

begin


 if not (Sender is TDBGrid) then
      Exit;

 grd := (Sender as TDBGrid);

 if (not grd.DataSource.DataSet.Active) then
  exit;

  try

    BlobFieldForm.btnLoadFromfile.Visible := not grd.ReadOnly;
    EditMemoForm.btnOk.Visible := not grd.ReadOnly;

    case grd.SelectedField.DataType of
      ftBlob, ftOraBlob:
      begin
        try
          fDetect := TFileDetector.Create;
          m := TMemoryStream.Create;
          TBlobField(grd.SelectedField).SaveToStream(m);
          fType := fDetect.Detect(m);
          BlobFieldForm.lblFileType.Caption := fType.Description;
          BlobFieldForm.HasImagePreview := False;
          BlobFieldForm.imgPreview.Picture := nil;
          if fType.PreviewType <> ftpNone then
          begin
            try
              m.Position := 0;
              BlobFieldForm.imgPreview.Picture.LoadFromStream(m);
              BlobFieldForm.HasImagePreview := True;
            except
              on e: Exception do
                BlobFieldForm.pnlPreview.Caption := e.Message;
            end;
          end;

          if not BlobFieldForm.HasImagePreview then
          begin
            BlobFieldForm.lblFileType.Caption :=
              TAsStringUtils.BlobToString(grd.SelectedField);
          end;

          DialogResult := BlobFieldForm.ShowModal;
        finally
          m.Free;
          fDetect.Free;
          fType.Free;
        end;


        if DialogResult = mrOk then

          with TOpenDialog.Create(nil) do
          begin
            if Execute then
            begin
              try
                grd.DataSource.DataSet.Edit;
                blob := TBlobField(grd.SelectedField);
                blob.LoadFromFile(FileName);
              except
                on e: Exception do
                begin
                  ShowMessage(e.Message);
                end;
              end;
            end;
            Free;
          end;
        if DialogResult = mrYes then
          with TSaveDialog.Create(nil) do
          begin
            if Execute then
            begin
              try
                if grd.SelectedField <> nil then
                begin
                  blob := TBlobField(grd.SelectedField);
                  blob.SaveToFile(FileName);
                end
                else
                begin
                  ShowMessage('Selected binary is empty!');
                end;
              except
                on e: Exception do
                begin
                  ShowMessage(e.Message);
                end;
              end;
            end;
            Free;
          end;
      end;
      ftMemo, ftWideMemo:
      begin

        if (grd.SelectedField is TWideMemoField) then
          EditMemoForm.memEdit.Text := (grd.SelectedField as TWideMemoField).Value;

        if (grd.SelectedField is TMemoField) then
          EditMemoForm.memEdit.Text := (grd.SelectedField as TMemoField).Value;

        if EditMemoForm.ShowModal = mrOk then
        begin
          try
            grd.DataSource.DataSet.Edit;
            if (grd.SelectedField is TWideMemoField) then
              (grd.SelectedField as TWideMemoField).Value := EditMemoForm.memEdit.Text;
            if (grd.SelectedField is TMemoField) then
              (grd.SelectedField as TMemoField).Value := EditMemoForm.memEdit.Text;
          except
            on e: Exception do
            begin
              ShowMessage(e.Message);
            end;
          end;
        end;
      end;
    end;
  except

  end;
end;

procedure TMainForm.EditUndo1Execute(Sender: TObject);
begin
  FPageControl.ActiveTab.QueryEditor.Undo;
end;

procedure TMainForm.FindDialog1Find(Sender: TObject);
begin
  FindText(FindDialog1.FindText);
  FindDialog1.CloseDialog;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  I: integer;
begin
  AppVersion := TFileUtils.GetApplicationVersion;

  FtableIcon := TBitmap.Create;
  FfunctionIcon := TBitmap.Create;
  FprocedureIcon := TBitmap.Create;
  FvarIcon := TBitmap.Create;
  FfieldIcon := TBitmap.Create;

  sbMain.Panels[1].Style := psOwnerDraw;
  FLoadingIndicator := TLoadingIndicator.Create(pnlIndicator);
  FLoadingIndicator.Parent := pnlIndicator;
  FLoadingIndicator.Align := alClient;

  ApplicationImages.GetBitmap(18, FtableIcon);
  ApplicationImages.GetBitmap(45, FfunctionIcon);
  ApplicationImages.GetBitmap(4, FfieldIcon);
  ApplicationImages.GetBitmap(37, FvarIcon);
  ApplicationImages.GetBitmap(20, FprocedureIcon);

  ApplicationImages.GetBitmap(46, ArrowImageLeft);
  ApplicationImages.GetBitmap(47, ArrowImageRight);
  ApplicationImages.GetBitmap(48, RectImage);

  FPageControl := TLazSqlXPageControl.Create(Self,SqlConn,ZCon);
  FPageControl.Highlighter := SqlSyntax;
  FPageControl.Keywords.AddStrings(TSqlKeywords.ReservedKeywords);
  FPageControl.OnExecutionFinished := @OnExecutionFinished;
  FPageControl.OnDataGridDblClick:=@EditSpecialField;
  FPageControl.QueryEditorPopUpMenu:=QueryEditorPopupMenu;
  FPageControl.DataGridPopUpMenu:=GridPopupMenu;
  FPageControl.PopupMenu := TabPopupMenu;

  FPageControl.TableIcon:=FtableIcon;
  FPageControl.FunctionIcon := FfunctionIcon;
  FPageControl.FieldIcon := FfieldIcon;
  FPageControl.VarIcon:=FvarIcon;
  FPageControl.ProcedureIcon:=FprocedureIcon;


  FPageControl.Parent := pnlMain;
  FPageControl.Visible:= False;
  FPageControl.Align:=alClient;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FvarIcon.Free;
  FtableIcon.Free;
  FfunctionIcon.Free;
  FfieldIcon.Free;

  ArrowImageRight.Free;
  ArrowImageLeft.Free;
  RectImage.Free;

end;

procedure TMainForm.actOpenExecute(Sender: TObject);
begin

    if OpenDialog.Execute then
    begin
      FPageControl.ActiveTab.QueryEditor.Lines.LoadFromFile(OpenDialog.FileName);
      FPageControl.ActiveTab.Caption := ExtractFileNameWithoutExt(OpenDialog.FileName);
    end;
end;

procedure TMainForm.actOpenTableExecute(Sender: TObject);
begin
 actNewTabExecute(nil);
 ExecuteQuery(True);
end;

procedure TMainForm.actQueryDesignerExecute(Sender: TObject);
begin
  if FConnected then
  begin
    QueryDesignerForm.Schema := cmbSchema.Text;
    if QueryDesignerForm.ShowModal(DbInfo) = mrOk then
    begin
      actNewTab.Execute;
      FPageControl.ActiveTab.QueryEditor.Text := QueryDesignerForm.SQLQuery;
    end;
  end;
end;

procedure TMainForm.actRefreshTablesExecute(Sender: TObject);
begin
  FillTables;
end;

procedure TMainForm.actRunStoredProcedureExecute(Sender: TObject);
begin
  if lstProcedures.ItemIndex > -1 then
    RunProcedure(lstProcedures.Items[lstProcedures.ItemIndex]);
end;

procedure TMainForm.actExecuteExecute(Sender: TObject);
begin
  ExecuteQuery(False);
end;

procedure TMainForm.actConnectExecute(Sender: TObject);
begin
  Connect;
end;

procedure TMainForm.actDisconnectExecute(Sender: TObject);
begin
  DoDisconnect;
  FPageControl.RemoveAllTabsButActive;
  FPageControl.ActiveTab.QueryEditor.Text:=EmptyStr;
  UpdateGUI(False);
end;

procedure TMainForm.actDropDatabaseExecute(Sender: TObject);
begin
  try
    if MessageDlg('EXTRA WARNING', 'You''re about to DROP THE DATABASE. Continue?',
      mtWarning, mbYesNo, 0) = mrYes then
    begin
      actDisconnect.Execute;
      Application.ProcessMessages;
      Sleep(3000);
      TDbUtils.ExecuteQuery('DROP DATABASE ' + DbInfo.Database, DbInfo, deZeos);
    end;
  except
    on e: Exception do
      ShowMessage(e.Message);
  end;
end;

procedure TMainForm.actDropTableExecute(Sender: TObject);
begin

  if MessageDlg('Confirm', 'Are you sure you want to drop the table?',
    mtConfirmation, mbYesNo, 0) = mrYes then
    try
      DoDisconnect;
      try
        TDbUtils.ExecuteQuery('DROP TABLE ' +
          lstTables.Items[lstTables.ItemIndex], DbInfo);
      finally
        DoSelectiveConnect;
        FillTables;
      end;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;

end;

procedure TMainForm.actEditTableExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
    begin
      DoDisconnect;
      TableInfoForm.Showmodal(DbInfo, cmbSchema.Text, lstTables.Items[lstTables.ItemIndex]);
      DoSelectiveConnect;
    end;
end;

procedure TMainForm.actCloseAllButThisExecute(Sender: TObject);
begin
  FPageControl.RemoveAllTabsButActive;
end;

procedure TMainForm.actDatabaseClonerExecute(Sender: TObject);
var
  infos: TTableInfos;
  I: integer;
  db: string;
  lstErrors: TStringList;
begin
  try
    lstErrors := TStringList.Create;
    try

      ProgressForm.Show;
      Application.ProcessMessages;

      infos := TTableInfos.Create(ZCon.HostName, ZCon.Database,
        ZCon.User, ZCon.Password, DbInfo.DatabaseType, ZCon.Port);


      ProgressForm.MaxProgress := lstTables.Count;

      for I := 0 to lstTables.Count - 1 do
      begin

        try
          //skip sequential tables for ORA
          if DbInfo.DatabaseType = dtOracle then
            if AnsiContainsStr(lstTables.Items[I], '_SEQ') then
              Continue;

          infos.AddTable(cmbSchema.Items[cmbSchema.ItemIndex], lstTables.Items[I]);
        except
          on e: Exception do
          begin
            lstErrors.Add(lstTables.Items[I]);
          end;
        end;

        ProgressForm.Message :=
          'Extracting Table Informations [' + lstTables.Items[I] + '] ... ';
        ProgressForm.StepProgress;
        Application.ProcessMessages;
      end;
    finally
      ProgressForm.Close;

    end;

    if DbInfo.DatabaseType <> dtSQLite then
      DatabaseClonerForm.txtDestinationDbName.Text := SqlConnBuilderForm.cmbDatabase.Text
    else
      DatabaseClonerForm.txtDestinationDbName.Text :=
        ExtractFileNameOnly(SqlConnBuilderForm.cmbDatabase.Text);


    if lstErrors.Count > 0 then
    begin
      MessageDlg('Warning', 'The following tables could not be added' +
        #13#10 + lstErrors.Text, mtWarning, [mbOK], 0);
    end;

    DatabaseClonerForm.ShowModal(infos);
  finally
    lstErrors.Free;
    infos.Free;
  end;
end;

procedure TMainForm.actCheckSyntaxExecute(Sender: TObject);
begin
  FPageControl.ActiveTab.CheckSyntax;
end;

procedure TMainForm.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.actCloseTabExecute(Sender: TObject);
begin
  FPageControl.RemoveTab(FPageControl.ActiveTab);
end;

procedure TMainForm.actExportCSVExecute(Sender: TObject);
begin
  CsvExporter.Dataset := FPageControl.ActiveTab.DataSource.DataSet;
  DoExport(CsvExporter, '.csv');
end;

procedure TMainForm.actExportDBFExecute(Sender: TObject);
begin
  // DoExport(DbfExporter,'.dbf');
end;

procedure TMainForm.actExportSQLExecute(Sender: TObject);
begin
  SqlExporter.Dataset := FPageControl.ActiveTab.DataGrid.DataSource.DataSet;
  DoExport(SqlExporter, '.sql');
end;

procedure TMainForm.actExportXMLExecute(Sender: TObject);
begin
  XmlExporter.Dataset := FPageControl.ActiveTab.DataGrid.DataSource.DataSet;
  DoExport(XmlExporter, '.xml');
end;

procedure TMainForm.actFindExecute(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TMainForm.actFormatQueryExecute(Sender: TObject);
begin

end;

procedure TMainForm.actGenerateAllprocExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), True);
  end;
end;

procedure TMainForm.actGenerateAllQueryExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), False);
  end;
end;

procedure TMainForm.actGenerateCreateScriptExecute(Sender: TObject);
var
  dbC: TAsDatabaseCloner;
  ti: TTableInfos;
  t: TTableInfo;
begin

  dbc := TAsDatabaseCloner.Create(DbInfo, ZCon.Database);
  ti := TTableInfos.Create(ZCon.HostName, ZCon.Database, ZCon.User,
    ZCon.Password, DbInfo.DatabaseType, ZCon.Port);
  t := ti.GetTableInfo(cmbSchema.Text, lstTables.Items[lstTables.ItemIndex]);
  try
    if actNewTab.Execute then
      FPageControl.ActiveTab.QueryEditor.Lines.Add(dbc.GetCreateScript(t, True, False));
  finally
    dbc.Free;
    ti.Free;
    t.Free;
  end;

end;

procedure TMainForm.actGenerateDeleteProcExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), True);
  end;
end;

procedure TMainForm.actGenerateDeleteQueryExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), False);
  end;
end;

procedure TMainForm.actGenerateInsertProcExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), True);
  end;
end;

procedure TMainForm.actGenerateInsertQueryExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), False);
  end;
end;

procedure TMainForm.actGenerateSelectItemProcExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), False);
  end;
end;

procedure TMainForm.actGenerateSelectItemQueryExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), False);
  end;
end;

procedure TMainForm.actGenerateSelectProcExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), True);
  end;
end;

procedure TMainForm.actGenerateSelectQueryExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), False);
  end;
end;

procedure TMainForm.actGenerateUpdateProcExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), True);
  end;
end;

procedure TMainForm.actGenerateUpdateQueryExecute(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
  begin
    GenerateSqlQuery(lstTables.Items[lstTables.ItemIndex], TQueryType(
      (Sender as TAction).Tag), False);
  end;
end;

procedure TMainForm.actGridCopyAllExecute(Sender: TObject);
begin
  CopyAllRows(False);
end;

procedure TMainForm.actGridCopyAllWithHeadersExecute(Sender: TObject);
begin
  CopyAllRows(True);
end;

procedure TMainForm.actGridCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := FPageControl.ActiveTab.DataGrid.SelectedField.AsString;
end;

procedure TMainForm.actGridCopyRowExecute(Sender: TObject);
begin
  CopySelectedRows(False);
end;

procedure TMainForm.actGridCopyRowsAsInsertExecute(Sender: TObject);
var
  t: string;
  parser: TAsSqlParser;
begin

  if FPageControl.ActiveTab.HasActiveData then
  begin
    parser := TAsSqlParser.Create(cmbSchema.Text, SqlConnBuilderForm.DbInfo);
    try
      parser.ParseCommand(FPageControl.ActiveTab.SQLQuery);
      if parser.FromTables.Count > 0 then
      begin
        t := parser.FromTables[0].Name;
      end;
    finally
      parser.Free;
    end;
  end;

  CopySelectedRowsAsSqlInsert(t);
end;

procedure TMainForm.actGridCopyRowsWithHeadersExecute(Sender: TObject);
begin
  CopySelectedRows(True);
end;

procedure TMainForm.actDataImporterExecute(Sender: TObject);
begin
  with DataImporterDialog do
  begin
    cmbTablename.Clear;
    cmbTablename.Items.AddStrings(lstTables.Items);
    if lstTables.ItemIndex > -1 then
      cmbTablename.ItemIndex := lstTables.ItemIndex;


    if ShowModal = mrOk then
    begin
      if cmbTablename.ItemIndex > -1 then
      begin
        DoDisconnect;
        DataImporterForm.ShowModal(SqlConnBuilderForm.DbInfo,
          cmbSchema.Text, cmbTablename.Items[cmbTablename.ItemIndex]);
        DoSelectiveConnect;
      end
      else
        ShowMessage('You have to select table where to import to');
    end;
  end;
end;

procedure TMainForm.actNewTabExecute(Sender: TObject);
begin
  if FConnected then
  begin
    FPageControl.AddTab;
    UpdateGUI(True);
  end;
end;

procedure TMainForm.actExportJSONExecute(Sender: TObject);
begin
  JsonExporter.Dataset := FPageControl.ActiveTab.DataGrid.DataSource.DataSet;
  DoExport(JsonExporter, '.json');
end;

procedure TMainForm.actExportRTFExecute(Sender: TObject);
begin
  RtfExporter.Dataset := FPageControl.ActiveTab.DataGrid.DataSource.DataSet;
  DoExport(RtfExporter, '.rtf');
end;

procedure TMainForm.actNewTableExecute(Sender: TObject);
begin
  DoDisconnect;
  TableInfoForm.Showmodal(DbInfo,cmbSchema.Text, '');
  DoSelectiveConnect;
  FillTables;
end;

procedure TMainForm.actSaveAsExecute(Sender: TObject);
begin
    if SaveDialog.Execute then
    begin
      FPageControl.ActiveTab.QueryEditor.Lines.SaveToFile(SaveDialog.FileName);
      actSaveAs.Enabled := False;
    end;
end;

procedure TMainForm.actSelectAllRowsExecute(Sender: TObject);
var
  RecPos: longint;
begin
  with FPageControl.ActiveTab.DataGrid.DataSource.DataSet do
  begin
    DisableControls;
    RecPos := RecNo;
    First;
    while not EOF do
    begin
      FPageControl.ActiveTab.DataGrid.SelectedRows.CurrentRowSelected := True;
      Next;
    end;
    RecNo := RecPos;
    EnableControls;
  end;
end;

procedure TMainForm.actShowStoredProcedureTextExecute(Sender: TObject);
begin
  if lstProcedures.ItemIndex > -1 then
  begin
    actNewTab.Execute;
    FPageControl.ActiveTab.QueryEditor.Text := GetProcedureText(
      lstProcedures.Items[lstProcedures.ItemIndex]);
  end;
end;

procedure TMainForm.ApplicationPropertiesActivate(Sender: TObject);
begin

end;

procedure TMainForm.ApplicationPropertiesDropFiles(Sender: TObject;
  const FileNames: array of string);
begin

end;

procedure TMainForm.ApplicationPropertiesException(Sender: TObject; E: Exception);
begin
  ShowMessage('Unhandled exception' + LineEnding + e.Message);
end;

procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject; var Done: boolean);
begin

  if (not FConnected) or (FPageControl.PageCount=0) then exit;


  actGridCopy.Enabled := (FPageControl.ActiveTab.HasActiveData);
  actGridCopyRow.Enabled := actGridCopy.Enabled;
  actGridCopyAll.Enabled := actGridCopyAll.Enabled;
  actExecute.Enabled := (Trim(FPageControl.ActiveTab.QueryEditor.Text) <> EmptyStr );
  actCheckSyntax.Enabled := actExecute.Enabled;
  actQueryDesigner.Enabled := FConnected;
  actDatabaseCloner.Enabled := FConnected;
  actDataImporter.Enabled := FConnected;
  actDisconnect.Enabled := FConnected;
  actNewTab.Enabled:=FConnected;


  if Assigned(DbInfo) then
  begin
    mitGenerateSP.Visible := DbInfo.DatabaseType <> dtFirebirdd;
    actDropDatabase.Enabled := DbInfo.DatabaseType in [dtMsSql, dtMySql, dtOracle];
  end;

end;

procedure TMainForm.cmbSchemaChange(Sender: TObject);
begin
  FillTables;
  FillpProcedures;
end;

procedure TMainForm.EditSelectAll1Execute(Sender: TObject);
begin
  FPageControl.ActiveTab.QueryEditor.SelectAll;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin

  if ssCtrl in Shift then
    if Key = VK_F then
      FindDialog1.Execute;


  if (Key = VK_F3) and (Trim(FindDialog1.FindText) <> '') then
    FindText(FindDialog1.FindText);

end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  Caption := 'LazSqlX ' + AppVersion + ' (Beta)';
  pgcLeft.ActivePageIndex := 0;
end;


procedure TMainForm.lstTablesDblClick(Sender: TObject);
var
  t: string;
begin
  t := lstTables.Items[lstTables.ItemIndex];
  if TAsStringUtils.ContainsChar(t, ' ') then
  begin
    case DbInfo.DatabaseType of
      dtMsSql: t := '[' + t + ']';
      dtOracle: t := '"' + t + '"';
    end;
  end;
  FPageControl.ActiveTab.QueryEditor.Lines.Add('SELECT * FROM ' + t + ' t');
  FPageControl.ActiveTab.QueryEditor.Lines[FPageControl.ActiveTab.QueryEditor.Lines.Count - 1] :=
    FPageControl.ActiveTab.QueryEditor.Lines[FPageControl.ActiveTab.QueryEditor.Lines.Count - 1] + ' ';
end;

procedure TMainForm.lstTablesDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  c: TListBox;
begin
  c := (Control as TListBox);


  with c.Canvas do
  begin

    if Odd(Index) then
      Brush.Color := clWhite
    else
      Brush.Color := $00F9F9F9;


    FillRect(ARect);

    if (odSelected in State) then
    begin
      FrameRect(ARect);
      GradientFill(ARect, clSilver, clGray, gdVertical);
      //Font.Color := $0000CCFF;
      Font.Color := clBlack;
    end
    else if (odFocused in State) then
    begin
      FrameRect(ARect);
      GradientFill(ARect, clSilver, clGray, gdVertical);
      Font.Color := clWhite;
    end
    else
    begin
      Font.Color := clBlack;
      Brush.Color := clWhite;
      c.Hint := '';
    end;

      if c.Name = 'lstTables' then
        Draw(ARect.Left, ARect.Top, FtableIcon)
      else
        Draw(ARect.Left, ARect.Top, FfunctionIcon);


    Brush.Style := bsClear;
    TextOut(ARect.Left + 17, ARect.Top, c.Items[Index]);
  end;
end;


procedure TMainForm.MenuItem3Click(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.mitOpenDataClick(Sender: TObject);
begin
  if lstTables.ItemIndex > -1 then
    ExecuteQuery(True);
end;


procedure TMainForm.mitRefreshTablesClick(Sender: TObject);
begin
  FillTables;
end;


procedure TMainForm.OnDynamicEditKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key = VK_TAB) then
    if not (ssShift in Shift) then
      (Sender as TWinControl).PerformTab(True);
end;

end.
