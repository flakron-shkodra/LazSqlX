{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit QueryDesignerFormU;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, QueryDesignerU, StdCtrls, ComCtrls, ExtCtrls,ZConnection,DbType, TableInfo;


type

  { TQueryDesignerForm }

  TQueryDesignerForm = class(TForm)
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    QueryDesigner1: TQueryDesigner;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbAreaClick(Sender: TObject);
    procedure pbAreaPaint(Sender: TObject);
    procedure QueryDesignerFrame1pbAreaDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure cmbSchemaChange(Sender: TObject);
    procedure QueryDesignerbtnClearClick(Sender: TObject);
    procedure QueryDesignerbtnGenerateQueryClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure QueryDesignerpbAreaDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    FDBConnection:TZConnection;
    FDBInfo: TZConnection;
    FSchema: string;
    FSQLQuery: string;
    { Private declarations }
    procedure PopulateSchemas;
    procedure PopulateTables;
    procedure SetDBInfo(AValue: TZConnection);
    procedure SetSchema(AValue: string);
    procedure SetSQLQuery(const Value: string);
    function GetSqlQuery: string;
  public
    { Public declarations }
    property DBInfo:TZConnection read FDBInfo write SetDBInfo;
    property SQLQuery:string read GetSqlQuery;
    property Schema:string read FSchema write SetSchema;

    function ShowModal(aDBInfo: TDbConnectionInfo): TModalResult;

    procedure Clear;

  end;

var
  QueryDesignerForm: TQueryDesignerForm;

implementation

uses MainFormU;

{$R *.lfm}

procedure TQueryDesignerForm.btnOKClick(Sender: TObject);
begin
//  QueryDesigner.GenerateQuery;
end;

procedure TQueryDesignerForm.cmbSchemaChange(Sender: TObject);
begin
  //PopulateTables;
end;

procedure TQueryDesignerForm.FormDestroy(Sender: TObject);
begin

end;

procedure TQueryDesignerForm.FormCreate(Sender: TObject);
begin

end;

procedure TQueryDesignerForm.FormShow(Sender: TObject);
begin

  FDBConnection:=FDBInfo;

  if not FDBConnection.Connected then
  begin
    FDBConnection.Connect;
  end;
  PopulateSchemas;
  PopulateTables;

  if QueryDesigner1.DatabaseInfo=nil then
  QueryDesigner1.DatabaseInfo := DBInfo;

  QueryDesigner1.pbArea.OnPaint:= QueryDesigner1.pbAreaPaint;
  QueryDesigner1.Schema:=MainForm.cmbSchema.Text;
  QueryDesigner1.DatabaseInfo:=FDBInfo;

end;

procedure TQueryDesignerForm.pbAreaClick(Sender: TObject);
begin

end;

procedure TQueryDesignerForm.pbAreaPaint(Sender: TObject);
begin

end;

function TQueryDesignerForm.GetSqlQuery: string;
begin
  Result := QueryDesigner1.SqlQuery;
end;

procedure TQueryDesignerForm.PopulateSchemas;
begin
  //if not FDBConnection.Connected then
  //  Exit;
  //cmbSchema.Clear;
  //
  //case DatabaseTypeFromString(FDBInfo.Protocol) of
  //  dtMsSql, dtOracle:
  //    FDBConnection.GetSchemaNames(cmbSchema.Items);
  //  dtMySql:
  //    cmbSchema.Items.Add(FDBConnection.Database);
  //  dtSQLite:
  //    cmbSchema.Items.Add
  //      (FDBConnection.Database);
  //end;
  //
  //cmbSchema.ItemIndex := 0;
end;

procedure TQueryDesignerForm.PopulateTables;
var
  schem: string;
  lst: TStringList;
  s: string;
begin
  //if FDBConnection.Connected then
  //begin
  //  lvTables.Clear;
  //  schem := EmptyStr;
  //  lst := TStringList.Create;
  //  try
  //    if (DatabaseTypeFromString(FDBInfo.Protocol) <> dtSQLite) then
  //    begin
  //      schem := cmbSchema.Items[cmbSchema.ItemIndex];
  //    end;
  //    FDBConnection.GetTableNames(schem,'',lst);
  //
  //    for s in lst do
  //    begin
  //      with lvTables.Items.Add do
  //      begin
  //        Caption := s;
  //        ImageIndex := 23;
  //      end;
  //    end;
  //
  //    if (DatabaseTypeFromString(FDBInfo.Protocol) = dtSQLite) then
  //    begin
  //      try
  //        lvTables.Items.Delete(lvTables.FindCaption(0, 'sqlite_sequence', True,
  //          True, True).Index);
  //      except
  //      end;
  //    end;
  //  finally
  //    lst.Free;
  //  end;
  //
  //end;
end;

procedure TQueryDesignerForm.SetDBInfo(AValue: TZConnection);
begin
 if FDBInfo=AValue then Exit;
 FDBInfo:=AValue;
end;

procedure TQueryDesignerForm.SetSchema(AValue: string);
begin
 if FSchema=AValue then Exit;
 FSchema:=AValue;

 QueryDesigner1.Schema:=AValue;

end;


procedure TQueryDesignerForm.QueryDesignerbtnClearClick(Sender: TObject);
begin
  QueryDesigner1.btnClearClick(Sender);
end;

procedure TQueryDesignerForm.QueryDesignerbtnGenerateQueryClick(Sender: TObject);
begin
  QueryDesigner1.btnGenerateQueryClick(Sender);

end;

procedure TQueryDesignerForm.QueryDesignerFrame1pbAreaDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 Accept := False;


  if (Source is TListView) then
    if (Source as TListView).Selected<>nil then
      Accept := True;


end;

procedure TQueryDesignerForm.QueryDesignerpbAreaDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
    if (Source is TListView) then
      QueryDesigner1.AddTable(FSchema,(Source as TListView).Selected.Caption,Point(X,y));
end;



procedure TQueryDesignerForm.SetSQLQuery(const Value: string);
begin
  FSQLQuery := Value;
end;

function TQueryDesignerForm.ShowModal(aDBInfo: TDbConnectionInfo): TModalResult;
begin
  DBInfo := aDBInfo.ToZeosConnection;
  Result := inherited ShowModal;
end;

procedure TQueryDesignerForm.Clear;
begin
 QueryDesigner1.Clear;
end;

end.
