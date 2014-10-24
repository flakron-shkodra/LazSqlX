{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit DataImporterFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, FileUtil, DividerBevel, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls, EditBtn, Spin, ComCtrls, DBGrids, Menus,
  ExtCtrls, TableInfo, DbType, ZDataset, ZConnection, fileimport;

type

  { TDataImporterForm }

  TDataImporterForm = class(TForm)
    Bevel1: TBevel;
    btnAccept: TBitBtn;
    btnDeleteMapping: TButton;
    btnCancel: TBitBtn;
    btnAddMapping: TButton;
    chkUseTabDelimiter: TCheckBox;
    cmbSourceColumns: TComboBox;
    cmbDestColumns: TComboBox;
    divColumnMapping: TDividerBevel;
    Label1: TLabel;
    lblDesstColumn: TLabel;
    lblSourceColumn: TLabel;
    lblProgress: TLabel;
    lstColumnsMapping: TListBox;
    mitClearAll: TMenuItem;
    mitClearItem: TMenuItem;
    pbProgress: TProgressBar;
    PopupMenu1: TPopupMenu;
    txtFilename: TFileNameEdit;
    lblStartPosition: TLabel;
    lblFilename: TLabel;
    lblEndRow: TLabel;
    speStartPosition: TSpinEdit;
    speEndPosition: TSpinEdit;
    txtDelimiter: TEdit;
    grpOptions: TGroupBox;
    lblDelimiter: TLabel;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnAddMappingClick(Sender: TObject);
    procedure chkFirstRowAsSchemaChange(Sender: TObject);
    procedure chkUseTabDelimiterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mitClearAllClick(Sender: TObject);
    procedure mitClearItemClick(Sender: TObject);
    procedure txtDelimiterChange(Sender: TObject);
    procedure txtFilenameAcceptFileName(Sender: TObject; var Value: string);
  private
    { private declarations }
    FDestQuery: TZQuery;
    FImporter: TFileImport;
    // Number of lines in input file (0 or more or -1 for invalid).
    // Only valid after FCSVParser has loaded file
    FInputLineCount: integer;
    FSchema: string;
    //todo: implement using sqldb sqlconnection instead of zeos depending on general connection settings
    FConn: TZConnection;
    FDestTable: string;
    FDBInfo: TDbConnectionInfo;
    FErrors: TStringList;
    // Updates listbox with mappings
    procedure UpdateMappingListBox;
    // Opens target/destination table in databse where data is to be written
    procedure OpenDestinationTable(FirstRowOnly: boolean);
    // Loads input file; updates min/max lines spinedits etc
    procedure PrepareFile(Filename: string);
    // Fills comboboxes with database and source file fields
    // Also sets up auto mapping
    procedure FillCombosAutoMap;
    // Run the import; returns result
    function ImportData: boolean;
    // (Re)load file filename
    // Update mapping/GUI
    // Calls PrepareFile internally.
    procedure LoadInputFile(FileName: string);
    // Enables/disables controls as needed
    procedure UpdateGUI(EnableControls: boolean);
    procedure CleanUp;
  public
    { public declarations }
    function ShowModal(DbInfo: TDbConnectionInfo;
      Schema, DestinationTable: string): TModalResult;
  end;

var
  DataImporterForm: TDataImporterForm;

implementation

uses AsStringUtils, AsSqlParser;

{$R *.lfm}

{ TDataImporterForm }

procedure TDataImporterForm.btnAcceptClick(Sender: TObject);
begin
  try
    UpdateGUI(False);
    if ImportData then
    begin
      ShowMessage('Data imported succsefully');
      CleanUp;
    end else
    begin
      ShowMessage('Errors encountered'+LineEnding+FErrors.Text);
    end;
  finally
    UpdateGUI(True);
  end;
end;

procedure TDataImporterForm.btnAddMappingClick(Sender: TObject);
var
  strItem: string;
  i: integer;
begin
  if (cmbSourceColumns.ItemIndex > -1) and (cmbDestColumns.ItemIndex > -1) then
    FImporter.AddMapping(cmbSourceColumns.Items[cmbSourceColumns.ItemIndex],
      cmbDestColumns.Items[cmbDestColumns.ItemIndex]);
  UpdateMappingListBox;
end;

procedure TDataImporterForm.chkFirstRowAsSchemaChange(Sender: TObject);
begin
  speStartPosition.Value := 0;
end;

procedure TDataImporterForm.chkUseTabDelimiterChange(Sender: TObject);
begin
  if chkUseTabDelimiter.Checked then
  begin
    txtDelimiter.Text := #9;
    txtDelimiter.Enabled := False;
  end
  else
  begin
    txtDelimiter.Text := ';';
    txtDelimiter.Enabled := True;
  end;

end;

procedure TDataImporterForm.FormCreate(Sender: TObject);
begin
  FConn := nil; //will be created in showmodal
  FDestQuery := nil; //will be created in showmodal
  FErrors := TStringList.Create;
  FInputLineCount := -1;
end;

procedure TDataImporterForm.FormDestroy(Sender: TObject);
begin
  if assigned(FImporter) then
    FImporter.Free;
  if assigned(FDestQuery) then
    FreeAndNil(FDestQuery);
  if assigned(FConn) then
    FreeAndNil(FConn);
  FErrors.Free;
end;

procedure TDataImporterForm.FormHide(Sender: TObject);
begin
  if Assigned(FImporter) then
    FreeAndNil(FImporter);
  if Assigned(FConn) then
    FreeAndNil(FConn);
end;

procedure TDataImporterForm.FormShow(Sender: TObject);
begin
  OpenDestinationTable(True);
  UpdateGUI(True);
  CleanUp;
end;

procedure TDataImporterForm.mitClearAllClick(Sender: TObject);
begin
  FImporter.DeleteMapping(-1);
end;

procedure TDataImporterForm.mitClearItemClick(Sender: TObject);
var
  ItemNo: integer;
begin
  ItemNo := lstColumnsMapping.ItemIndex;
  if ItemNo > -1 then
  begin
    FImporter.DeleteMapping(ItemNo);
    lstColumnsMapping.Items.Delete(ItemNo);
  end;
end;

procedure TDataImporterForm.txtDelimiterChange(Sender: TObject);
begin
  LoadInputFile(txtFilename.Text);
end;

procedure TDataImporterForm.txtFilenameAcceptFileName(Sender: TObject;
  var Value: string);
begin
  // Close any current open input file
  FImporter.FileName := '';
  if not fileexists(Value) then
    exit;
  try
    FImporter.FileName := Value;
    txtDelimiter.Text := FImporter.Delimiter;
    if FImporter.Delimiter = #9 then
    begin
      chkUseTabDelimiter.Checked := True;
      txtDelimiter.Enabled := False;
    end;

    LoadInputFile(Value);
  except
    // File access error etc
    chkUseTabDelimiter.Checked := False;
    txtDelimiter.Text := ',';
    txtDelimiter.Enabled := true;
  end;
end;

procedure TDataImporterForm.PrepareFile(Filename: string);
var
  Row: integer;
begin
  FInputLineCount := -1;
  if FileExists(Filename) then
  begin
    try
      if txtDelimiter.Text <> EmptyStr then
        FImporter.Delimiter := txtDelimiter.Text[1];

      // Go through entire file once to get number of lines
      // OS caching should help with following read
      Screen.Cursor := crHourglass;
      try
        FInputLineCount := FImporter.GetRowCount;
      finally
        Screen.Cursor := crDefault;
      end;
      speStartPosition.MaxValue := FInputLineCount;
      speEndPosition.MaxValue := FInputLineCount;
      speStartPosition.MinValue := 0;
      speEndPosition.MinValue := 0;
      // Keep user preference if he wants to import only a subset of lines:
      if speEndPosition.Value = 0 then
        speEndPosition.Value := FInputLineCount;
    except
      on E: Exception do
      begin
        ShowMessage(E.Message);
      end;
    end;
  end;
end;

procedure TDataImporterForm.FillCombosAutoMap;
var
  I: integer;
begin

  cmbSourceColumns.Clear;
  for I := 0 to FImporter.SourceFields.Count - 1 do
  begin
    cmbSourceColumns.Items.Add(FImporter.SourceFields[i]);
  end;
  if cmbSourceColumns.Items.Count > -1 then
    cmbSourceColumns.ItemIndex := 0;

  assert(assigned(FDestQuery));
  if FDestQuery.Active then
  begin
    cmbDestColumns.Clear;
    for I := 0 to FDestQuery.FieldCount - 1 do
    begin
      cmbDestColumns.Items.Add(FDestQuery.Fields[I].FieldName);
    end;
    if cmbDestColumns.Items.Count > -1 then
      cmbDestColumns.ItemIndex := 0;
  end;

  // Add automapped data
  UpdateMappingListBox;
end;

procedure TDataImporterForm.OpenDestinationTable(FirstRowOnly: boolean);
var
  aDbType: TDatabaseType;
  sqlParser: TAsSqlParser;
begin
  assert(assigned(FConn));
  assert(assigned(FDestQuery));
  aDbType := TDbUtils.DatabaseTypeFromString(FConn.Protocol);
  FDestQuery.Close;
  FDestQuery.SQL.Text := 'select * from ' + FDestTable;

  if FirstRowOnly then
  // Apparently select the first 5 rows?!?! Is this correct?
  begin
    // There is an SQL standard for specifying row x..y but not all dbs use it...
    case aDbType of
      dtMsSql:
      begin
        FDestQuery.SQL.Text :=
          'SELECT TOP 1 * FROM [' + FSchema + '].[' + FDestTable + ']';

        try
          sqlParser := TAsSqlParser.Create(FSchema, FDBInfo);
          if sqlParser.ParseCommand(FDestQuery.SQL.Text) then
          begin
            FDestQuery.SQL.Text := sqlParser.RegenerateSelect(True);
          end;
        finally
          sqlParser.Free;
        end;

      end;
      dtOracle:
        FDestQuery.SQL.Text :=
        'SELECT * FROM "' + FSchema + '"."' + FDestTable + '" WHERE ROWNUM < 6';
      dtMySql:
        FDestQuery.SQL.Text := 'SELECT * FROM ' + FDestTable + ' LIMIT 5';
      dtSQLite:
        FDestQuery.SQL.Text := 'SELECT * FROM ' + FDestTable + ' LIMIT 5';
      dtFirebirdd:
        // Firebird 2+ also accepts standard SQL (see below)
        FDestQuery.SQL.Text := 'SELECT FIRST 1 * FROM ' + FDestTable;
      else
        // Use SQL standard and hope db supports it:
        FDestQuery.SQL.Text := 'SELECT * FROM ' + FDestTable + ' ROWS 1 TO 5';
    end;
  end;
  FDestQuery.Connection := FConn;
  FDestQuery.Open;

end;

function TDataImporterForm.ImportData: boolean;
var
  CurrentRow: integer; // helps keep track of row changes
  I: integer;
  SrcValue: string;
  SrcColumn: string;
  DestColumn: string;
  LastRow: integer;
begin
  Result := False; //fail by default
  Screen.Cursor:=crHourglass;;
  try
    FErrors.Clear;

    pbProgress.Max := speEndPosition.MaxValue;
    OpenDestinationTable(False);
    // todo: check to make sure file is loaded?
    if speEndPosition.Value <= 0 then
      LastRow := FImporter.GetRowCount
    else
      LastRow := speEndPosition.Value;

    // Initialize GUI
    lblProgress.Caption :=
      'Processing 0/' + IntToStr(LastRow - 1);
    pbProgress.Position := 0;
    Application.ProcessMessages;

    try
      // Skip past header line(s)
      if speStartPosition.Value>0 then
      begin
        for i:=1 to speStartPosition.Value do
        begin
          if not(FImporter.ReadRow) then
            break; //error: end of file?
        end;
      end;

      while (FImporter.ReadRow) and (FImporter.Row+1<=LastRow) do
      begin
        CurrentRow:=FImporter.Row;
        if FDestQuery.State in [dsEdit,dsInsert] then
        begin
          try
            FDestQuery.Post;
          except on E:exception do
            begin
              Result:=False;
              FErrors.Add('Error on row [' + inttostr(CurrentRow) + '] with message: ' + E.Message);
              Break;
            end;
          end;
        end;
        FDestQuery.Insert;

        // Update GUI
        if CurrentRow mod 100=0 then
        begin
          lblProgress.Caption :=
            'Processing ' + IntToStr(CurrentRow) + '/' + IntToStr(LastRow - 1);
          pbProgress.Position := CurrentRow;
        end;
        Application.ProcessMessages;

        for I := 0 to FImporter.MappingCount-1 do
        begin
          DestColumn := FImporter.Mapping[I].DestinationField;
          // Note: csv import sees everything as strings so let the db convert if possible
          if (FDestQuery.FieldByName(DestColumn).DataType <> ftAutoInc) and
            (FDestQuery.FieldByName(DestColumn).ReadOnly = false) then
            FDestQuery.FieldByName(DestColumn).AsString := FImporter.GetData(i);
        end;
      end;
      pbProgress.StepIt;
      Result := True;
    except
      on E: Exception do
      begin
        FErrors.Add('Error on row [' + IntToStr(I) + '] with message: ' + E.Message);
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TDataImporterForm.LoadInputFile(FileName: string);
begin
  if not FileExists(FileName) then
    exit;

  FErrors.Clear;
  PrepareFile(FileName);
  OpenDestinationTable(True);
  FillCombosAutoMap;
  UpdateGUI(True);
end;

procedure TDataImporterForm.UpdateGUI(EnableControls: boolean);
begin
  btnAccept.Enabled := EnableControls;
  btnAccept.Enabled := EnableControls;
  btnCancel.Enabled := EnableControls;
  grpOptions.Enabled := EnableControls;

  if EnableControls then
    pbProgress.Position := 0;
end;

procedure TDataImporterForm.UpdateMappingListBox;
var
  i: integer;
  MappingCount: integer;
begin
  lstColumnsMapping.Clear;
  // MappingCount will map fields if necessary so we need destination fields
  if FImporter.DestinationFields.Count=0 then
  begin
    if not(assigned(FDestQuery)) then
      raise Exception.Create('Cannot update mapping info without valid destination query.');
    MappingCount := FDestQuery.Fields.Count;
    for i := 0 to MappingCount - 1 do
    begin
      FImporter.DestinationFields.Add(FDestQuery.Fields[i].FieldName);
    end;
  end;

  MappingCount := FImporter.MappingCount;
  for i := 0 to MappingCount-1 do
  begin
    lstColumnsMapping.Items.Add(FImporter.Mapping[i].SourceField + '=>' +
      FImporter.Mapping[i].DestinationField);
  end;
end;

procedure TDataImporterForm.CleanUp;
begin
  txtFilename.Text := EmptyStr;
  txtDelimiter.Text := EmptyStr;
  cmbSourceColumns.Clear;
  cmbDestColumns.Clear;
  lstColumnsMapping.Clear;
  FErrors.Clear;
end;

function TDataImporterForm.ShowModal(DbInfo: TDbConnectionInfo;
  Schema, DestinationTable: string): TModalResult;
begin
  FSchema := Schema;
  Assert(assigned(DbInfo));
  FImporter := TFileImport.Create;
  FConn := DbInfo.ToZeosConnection;
  FDestTable := DestinationTable;
  if not(assigned(FDestQuery)) then
    FDestQuery := TZQuery.Create(nil);
  assert(assigned(FDestQuery));
  FDestQuery.Connection := FConn;
  FDBInfo := DbInfo;
  Result := inherited ShowModal;

end;

end.
