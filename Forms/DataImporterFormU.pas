{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit DataImporterFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, DB, FileUtil, DividerBevel, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls, EditBtn, Spin, ComCtrls, DBGrids, Menus,
  ExtCtrls, TableInfo, DbType, ZDataset, ZConnection, CsvDocument;

type

  { TDataImporterForm }

  TDataImporterForm = class(TForm)
    Bevel1: TBevel;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    btnAddMapping: TButton;
    chkUseTabDelimiter: TCheckBox;
    cmbSourceColumns: TComboBox;
    cmbDestColumns: TComboBox;
    CsvDataSource: TDatasource;
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
    procedure btnPrepareClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure chkFirstRowAsSchemaChange(Sender: TObject);
    procedure chkUseTabDelimiterChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grpOptionsClick(Sender: TObject);
    procedure mitClearAllClick(Sender: TObject);
    procedure mitClearItemClick(Sender: TObject);
    procedure txtDelimiterChange(Sender: TObject);
    procedure txtDelimiterKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure txtDelimiterKeyPress(Sender: TObject; var Key: char);
    procedure txtFilenameAcceptFileName(Sender: TObject; var Value: string);
    procedure txtFilenameChange(Sender: TObject);
  private
    { private declarations }
    FCsvLoaded: boolean;
    FDestQuery: TZQuery;
    FSchema: string;
    FConn: TZConnection;
    FCSVDocument: TCSVDocument; //Source data
    FDestTable: string;
    FDBInfo: TDbConnectionInfo;
    FErrors: TStringList;
    function DetectDelimiter(Line: string): string;
    procedure OpenDestinationTable(FirstRowOnly: boolean);
    procedure PrepareFile(filename: string);
    procedure AutoMap;
    procedure FillColumnMappings;
    function ImportData: boolean;
    procedure Reload(Value: string);
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
      FCsvLoaded := False;
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
begin
  if (cmbSourceColumns.ItemIndex > -1) and (cmbDestColumns.ItemIndex > -1) then
  begin
    strItem := cmbSourceColumns.Items[cmbSourceColumns.ItemIndex] + '=' +
      cmbDestColumns.Items[cmbDestColumns.ItemIndex];
    if lstColumnsMapping.Items.IndexOf(strItem) < 0 then
      lstColumnsMapping.Items.Add(strItem)
    else
      ShowMessage('Selected mapping is already in the list.');
  end;
end;

procedure TDataImporterForm.btnPrepareClick(Sender: TObject);
begin

end;

procedure TDataImporterForm.btnValidateClick(Sender: TObject);
begin

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

procedure TDataImporterForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TDataImporterForm.FormCreate(Sender: TObject);
begin
  FCSVDocument := TCSVDocument.Create;
  FDestQuery := TZQuery.Create(Self);
  FErrors := TStringList.Create;
end;

procedure TDataImporterForm.FormDestroy(Sender: TObject);
begin
  FCSVDocument.Free;
  FDestQuery.Free;
  FErrors.Free;
end;

procedure TDataImporterForm.FormHide(Sender: TObject);
begin
  if Assigned(FConn) then
    FConn.Free;
end;

procedure TDataImporterForm.FormShow(Sender: TObject);
begin
  FCsvLoaded := False;
  OpenDestinationTable(True);
  UpdateGUI(True);
  CleanUp;
end;

procedure TDataImporterForm.grpOptionsClick(Sender: TObject);
begin

end;

procedure TDataImporterForm.mitClearAllClick(Sender: TObject);
begin
  lstColumnsMapping.Clear;
end;

procedure TDataImporterForm.mitClearItemClick(Sender: TObject);
begin
  if lstColumnsMapping.ItemIndex > -1 then
    lstColumnsMapping.Items.Delete(lstColumnsMapping.ItemIndex);
end;

procedure TDataImporterForm.txtDelimiterChange(Sender: TObject);
begin
  Reload(txtFilename.Text);
end;

procedure TDataImporterForm.txtDelimiterKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin

end;

procedure TDataImporterForm.txtDelimiterKeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TDataImporterForm.txtFilenameAcceptFileName(Sender: TObject;
  var Value: string);
var
  lst: TStringList;
begin
  try
    lst := TStringList.Create;
    lst.LoadFromFile(Value);
    if lst.Count > 0 then
    begin
      txtDelimiter.Text := DetectDelimiter(lst[0]);
      if txtDelimiter.Text[1] = #9 then
      begin
        chkUseTabDelimiter.Checked := False;
        txtDelimiter.Enabled := False;
      end;
    end;

    Reload(Value);

  finally
    lst.Free;
  end;
end;

procedure TDataImporterForm.txtFilenameChange(Sender: TObject);
begin

end;

function TDataImporterForm.DetectDelimiter(Line: string): string;
var
  d: char;
  I: integer;
begin

  for I := 1 to Length(Line) do
  begin
    if (Line[I] in [#9, ';', '|', ',']) then
    begin
      Result := Line[I];
      Break;
    end;
  end;

end;

procedure TDataImporterForm.PrepareFile(filename: string);
var
  I: integer;
begin
  if FileExists(filename) then
  begin
    try
      FCSVDocument.Clear;
      if txtDelimiter.Text <> EmptyStr then
        FCSVDocument.Delimiter := txtDelimiter.Text[1]
      else
        FCSVDocument.Delimiter := ',';

      FCsvDocument.LoadFromFile(filename);
      FCsvLoaded := True;
      pbProgress.Max := FCSVDocument.RowCount - 1;
      speStartPosition.MaxValue := FCSVDocument.RowCount;
      speEndPosition.MaxValue := FCSVDocument.RowCount;
      speStartPosition.MinValue := 0;
      speEndPosition.MinValue := 0;
      speEndPosition.Value := FCSVDocument.RowCount;
    except
      on E: Exception do
      begin
        ShowMessage(E.Message);
      end;
    end;
  end;
end;

procedure TDataImporterForm.AutoMap;
var
  I: integer;
begin

  lstColumnsMapping.Clear;

  for I := 0 to FCSVDocument.ColCount[0] - 1 do
  begin
    try
      lstColumnsMapping.Items.Add(FCSVDocument.Cells[I, 0] + '=' +
        FDestQuery.Fields[I].FieldName);
    except
    end;
  end;

end;

procedure TDataImporterForm.FillColumnMappings;
var
  I: integer;
begin
  if FCsvLoaded then
  begin
    cmbSourceColumns.Clear;
    for I := 0 to FCSVDocument.ColCount[0] - 1 do
    begin
      cmbSourceColumns.Items.Add(FCSVDocument.Cells[I, 0]);
    end;
    if cmbSourceColumns.Items.Count > -1 then
      cmbSourceColumns.ItemIndex := 0;
  end;

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

end;

procedure TDataImporterForm.OpenDestinationTable(FirstRowOnly: boolean);
var
  aDbType: TDatabaseType;
  sqlParser: TAsSqlParser;
begin

  aDbType := TDbUtils.DatabaseTypeFromString(FConn.Protocol);
  FDestQuery.Close;
  FDestQuery.SQL.Text := 'select * from ' + FDestTable;

  if FirstRowOnly then
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
      dtOracle: FDestQuery.SQL.Text :=
          'SELECT * FROM "' + FSchema + '"."' + FDestTable + '" WHERE ROWNUM < 6';

      dtMySql: FDestQuery.SQL.Text := 'SELECT * FROM ' + FDestTable + ' LIMIT 5';
      dtSQLite: FDestQuery.SQL.Text := 'SELECT * FROM ' + FDestTable + ' LIMIT 5';
      dtFirebirdd: FDestQuery.SQL.Text := 'SELECT FIRST 1 * FROM ' + FDestTable;
    end;
  FDestQuery.Connection := FConn;
  FDestQuery.Open;
end;

function TDataImporterForm.ImportData: boolean;
var
  I: integer;
  J: integer;
  SrcValue: string;
  SrcColumn: string;
  DestColumn: string;
  LastRow: integer;
begin

  Result := True;
  FErrors.Clear;

  OpenDestinationTable(False);

  if speEndPosition.Value = 0 then
    LastRow := FCSVDocument.RowCount
  else
    LastRow := speEndPosition.Value;

  try
    for I := speStartPosition.Value to LastRow - 1 do
    begin

      lblProgress.Caption :=
        'Processing ' + IntToStr(I) + '\' + IntToStr(LastRow - 1);
      Application.ProcessMessages;
      FDestQuery.Insert;
      for J := 0 to lstColumnsMapping.Count - 1 do
      begin
        SrcColumn := TAsStringUtils.SplitString(lstColumnsMapping.Items[J], '=')[0];
        SrcValue := FCSVDocument.Cells[cmbSourceColumns.Items.IndexOf(SrcColumn), I];
        DestColumn := TAsStringUtils.SplitString(lstColumnsMapping.Items[J], '=')[1];
        if FDestQuery.FieldByName(DestColumn).DataType <> ftAutoInc then
        begin
          if not FDestQuery.FieldByName(DestColumn).ReadOnly then
            FDestQuery.FieldByName(DestColumn).Value := SrcValue;
        end;
      end;

      try
        FDestQuery.Post;
      except on E:exception do
        begin
          Result:=False;
          FErrors.Add('Error on row [' + IntToStr(I) + '] with message: ' + E.Message);
          Break;
        end;
      end;
      pbProgress.StepIt;
    end;

  except
    on E: Exception do
    begin
      Result:=False;
      FErrors.Add('Error on row [' + IntToStr(I) + '] with message: ' + E.Message);

    end;
  end;

end;

procedure TDataImporterForm.Reload(Value: string);
begin
  if not FileExists(Value) then
    exit;

  FErrors.Clear;
  PrepareFile(Value);
  OpenDestinationTable(True);
  FillColumnMappings;
  AutoMap;
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

procedure TDataImporterForm.CleanUp;
begin
  txtFilename.Text := EmptyStr;
  txtDelimiter.Text := EmptyStr;
  cmbSourceColumns.Clear;
  cmbDestColumns.Clear;
  lstColumnsMapping.Clear;
  FCsvLoaded := False;
  FErrors.Clear;
end;

function TDataImporterForm.ShowModal(DbInfo: TDbConnectionInfo;
  Schema, DestinationTable: string): TModalResult;
begin
  FSchema := Schema;
  FConn := DbInfo.ToZeosConnection;
  FDestTable := DestinationTable;
  FDestQuery.Connection := FConn;
  FDBInfo := DbInfo;
  Result := inherited ShowModal;

end;

end.
