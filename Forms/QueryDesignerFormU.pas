{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit QueryDesignerFormU;

{$MODE objfpc}

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, QueryDesignerU, StdCtrls, ComCtrls, ExtCtrls, AsDbType, AsTableInfo;


type

  { TQueryDesignerForm }

  TQueryDesignerForm = class(TForm)
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbAreaClick(Sender: TObject);
    procedure pbAreaPaint(Sender: TObject);
    procedure QueryDesignerFrame1pbAreaDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure cmbSchemaChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FDBInfo: TAsDbConnectionInfo;
    FSchema: string;
    FSQLQuery: string;
    FQueryDesigner:TQueryDesigner;
    { Private declarations }
    procedure PopulateSchemas;
    procedure PopulateTables;

    procedure SetSchema(AValue: string);
    procedure SetSQLQuery(const Value: string);
    function GetSqlQuery: Tstrings;
  public
    { Public declarations }
    property SQLQuery:TStrings read GetSqlQuery;
    property Schema:string read FSchema write SetSchema;

    function ShowModal(aDBInfo: TAsDbConnectionInfo): TModalResult;

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
 if Assigned(FQueryDesigner) then
 FQueryDesigner.Free;
end;

procedure TQueryDesignerForm.FormCreate(Sender: TObject);
begin
  if FQueryDesigner=nil then
  begin
    FQueryDesigner:=TQueryDesigner.Create(Self);
    FQueryDesigner.Parent := Self;
    FQueryDesigner.Align:=alClient;
  end;
end;

procedure TQueryDesignerForm.FormShow(Sender: TObject);
begin

  PopulateSchemas;
  PopulateTables;

  FQueryDesigner.Schema:=MainForm.cmbSchema.Text;
  FQueryDesigner.DbInfo:=FDBInfo;

end;

procedure TQueryDesignerForm.pbAreaClick(Sender: TObject);
begin

end;

procedure TQueryDesignerForm.pbAreaPaint(Sender: TObject);
begin

end;

function TQueryDesignerForm.GetSqlQuery: Tstrings;
begin
  Result := FQueryDesigner.SqlQuery;
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


procedure TQueryDesignerForm.SetSchema(AValue: string);
begin
 if FSchema=AValue then Exit;
 FSchema:=AValue;

FQueryDesigner.Schema:=AValue;

end;


procedure TQueryDesignerForm.QueryDesignerFrame1pbAreaDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 Accept := False;


  if (Source is TListView) then
    if (Source as TListView).Selected<>nil then
      Accept := True;


end;


procedure TQueryDesignerForm.SetSQLQuery(const Value: string);
begin
  FSQLQuery := Value;
end;

function TQueryDesignerForm.ShowModal(aDBInfo: TAsDbConnectionInfo
 ): TModalResult;
begin
  FDBInfo := aDBInfo;
  FQueryDesigner.DbInfo := FDBInfo;
  Result := inherited ShowModal;
end;

procedure TQueryDesignerForm.Clear;
begin
 if Assigned(FQueryDesigner) then
 FQueryDesigner.Clear;
end;

end.
