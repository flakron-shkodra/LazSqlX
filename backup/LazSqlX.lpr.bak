program LazSqlX;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormU, SqlConnBuilderFormU, EditMemoFormU, BlobFieldFormU,
  TableInfoFormU, AboutFormU, EditColumnFormU, EditConstraintsFormU,
  EditIndexFormU, QueryDesignerFormU, ProgressFormU, DatabaseClonerFormU,
  DataImporterFormU, Utils, SqlGenerator, FtDetector, lazdbexport, zcomponent,
  TableInfo, DbType, SqlExecThread, QueryDesignerDialogU,QueryDesignerTablesU,DataImporterDialogU;


{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSqlConnBuilderForm, SqlConnBuilderForm);
  Application.CreateForm(TBlobFieldForm, BlobFieldForm);
  Application.CreateForm(TEditMemoForm, EditMemoForm);
  Application.CreateForm(TTableInfoForm, TableInfoForm);
  Application.CreateForm(TAboutForm,AboutForm);
  Application.CreateForm(TEditColumnForm,EditColumnForm);
  Application.CreateForm(TEditConstraintForm,EditConstraintForm);
  Application.CreateForm(TEditIndexForm,EditIndexForm);
  Application.CreateForm(TQueryDesignerForm,QueryDesignerForm);
  Application.CreateForm(TQueryDesignerDialog,QueryDesignerDialog);
  Application.CreateForm(TQueryDesignerTables,QueryDesignerTables);
  Application.CreateForm(TProgressForm,ProgressForm);
  Application.CreateForm(TDatabaseClonerForm, DatabaseClonerForm);
  Application.CreateForm(TDataImporterForm,DataImporterForm);
  Application.CreateForm(TDataImporterDialog,DataImporterDialog);

  Application.Run;
end.

