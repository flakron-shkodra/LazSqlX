program LazSqlX;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF DEBUG}
  SysUtils, // needed for heaptrace
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormU, SqlConnBuilderFormU, EditMemoFormU, BlobFieldFormU,
  DesignTableFormU, AboutFormU, EditColumnFormU, EditConstraintsFormU,
  EditIndexFormU, QueryDesignerFormU, ProgressFormU, DatabaseClonerFormU,
  DataImporterFormU, Utils, AsSqlGenerator, FtDetector, lazdbexport, zcomponent,
  AsTableInfo, AsDbType, SqlExecThread, QueryDesignerDialogU, QueryDesignerTablesU,
  QueryDesignerPropertyGridU, DataImporterDialogU;


{$R *.res}

begin
  // Enable -dDEBUG/the DEBUG conditional define to: write all memory leaks to disk
  // If it is not enabled, then leaks are reported immediately in the IDE (if -gh is on)
  {$IFDEF DEBUG}
  SetHeapTraceOutput(ChangeFileExt(Application.Exename, '.trc'));
  {$ENDIF}

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSqlConnBuilderForm, SqlConnBuilderForm);
  Application.CreateForm(TBlobFieldForm, BlobFieldForm);
  Application.CreateForm(TEditMemoForm, EditMemoForm);
 Application.CreateForm(TDesignTableForm, DesignTableForm);
  Application.CreateForm(TAboutForm,AboutForm);
  Application.CreateForm(TEditColumnForm,EditColumnForm);
  Application.CreateForm(TEditConstraintForm,EditConstraintForm);
  Application.CreateForm(TEditIndexForm,EditIndexForm);
  Application.CreateForm(TQueryDesignerForm,QueryDesignerForm);
  Application.CreateForm(TQueryDesignerDialog,QueryDesignerDialog);
  Application.CreateForm(TQueryDesignerTables,QueryDesignerTables);
  Application.CreateForm(TQueryDesignerPropertyGrid,QueryDesignerPropertyGrid);
  Application.CreateForm(TProgressForm,ProgressForm);
  Application.CreateForm(TDatabaseClonerForm, DatabaseClonerForm);
  Application.CreateForm(TDataImporterForm,DataImporterForm);
  Application.CreateForm(TDataImporterDialog,DataImporterDialog);

  Application.Run;
end.

