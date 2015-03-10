{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}

unit DatabaseClonerFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Spin, types, LCLType, ComCtrls,AsTableInfo, AsDbType,strutils;

type

  { TDatabaseClonerForm }

  TLogType = (ltInfo,ltError);

  TDatabaseClonerForm = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    chkDbStructure: TCheckBox;
    chkCreateConstraints: TCheckBox;
    chkCopyData: TCheckBox;
    chkIntegratedSecurity: TCheckBox;
    cmbDatabaseType: TComboBox;
    cmbServerName: TComboBox;
    grpLog: TGroupBox;
    grpDestServer: TGroupBox;
    imgDatabaseTypes: TImageList;
    Label2: TLabel;
    lblDatabase1: TLabel;
    lblPassword: TLabel;
    lblPort: TLabel;
    lblProgress: TLabel;
    lblSever: TLabel;
    lblUseraname: TLabel;
    lstLog: TListBox;
    btnOpenFile: TSpeedButton;
    OpenDialog1: TOpenDialog;
    pbCopyDataProgress: TProgressBar;
    txtErrors: TMemo;
    pgLog: TPageControl;
    pbProgressBar: TProgressBar;
    pnlMain: TPanel;
    tabProgress: TTabSheet;
    tabErrors: TTabSheet;
    txtConnStr: TMemo;
    txtPassword: TEdit;
    txtPort: TSpinEdit;
    txtUserName: TEdit;
    txtDestinationDbName: TEdit;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure cmbDatabaseTypeChange(Sender: TObject);
    procedure cmbDatabaseTypeDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnCopyDataRow(CurrentRow, TotalRows: Integer);
    procedure pnlMainClick(Sender: TObject);
  private
    FTableInfos: TAsTableInfos;
    FDBInfo: TAsDbConnectionInfo;
    procedure WriteLog(Msg:string; LogType:TLogType);
    procedure UpdateGUI(EnableControls:Boolean);
    { private declarations }
  public
    { public declarations }
    function ShowModal(dbInfo:TAsDbConnectionInfo; Infos: TAsTableInfos): TModalResult;
  end;

var
  DatabaseClonerForm: TDatabaseClonerForm;

  bmpSqlType: TBitmap;
  bmpOracleType: TBitmap;
  bmpMySqlType: TBitmap;
  bmpSqliteType: TBitmap;
  bmpFirebird: TBitmap;
  bmpPostgreSql:TBitmap;

implementation

uses AsDatabaseCloner, AsStringUtils, ProgressFormU;

{$R *.lfm}

{ TDatabaseClonerForm }

procedure TDatabaseClonerForm.cmbDatabaseTypeDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  cmb: TComboBox;
begin
  cmb := Control as TComboBox;

  if odSelected in State then
  begin
    cmb.Canvas.GradientFill(ARect, clWhite, clSkyBlue, gdVertical);
  end
  else
  begin
    cmb.Canvas.Brush.Color := clWindow;
    cmb.Canvas.FillRect(ARect);
  end;


  case Index of
    0: cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpSqlType);
    1: cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpOracleType);
    2: cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpMySqlType);
    3: cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpSqliteType);
    4: cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpFirebird);
    5: cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpPostgreSql);
  end;

  cmb.Canvas.Font.Size := 10;
  cmb.Canvas.Brush.Style := bsClear;
  cmb.Canvas.TextOut(ARect.Left + 30, ARect.Top + 3, cmb.Items[Index]);
end;

procedure TDatabaseClonerForm.cmbDatabaseTypeChange(Sender: TObject);
begin

  txtDestinationDbName.Width:= 581;
  btnOpenFile.Visible:=False;
  lblUseraname.Visible := True;
  txtUserName.Visible := True;
  lblPassword.Visible := True;
  txtPassword.Visible := True;
  lblPort.Visible := True;
  txtPort.Visible := True;
  lblSever.Visible := True;
  cmbServerName.Visible := True;
  lblDatabase1.Caption := 'Destination Database Name';

  case cmbDatabaseType.ItemIndex of
    0:
    begin
      txtPort.Text := '0';
      txtUserName.Text := 'sa';
      txtPassword.Text := '';
      lblDatabase1.Caption := 'Destination Database Name (Creates new database)';
    end;
    1:
    begin
      txtPort.Text := '1521';
      txtUserName.Text := 'sa';
      txtPassword.Text := '';
      lblDatabase1.Caption := 'Destination Database Name (Uses an existing database)';
    end;
    2:
    begin
      txtPort.Text := '3306';
      txtUserName.Text := 'root';
      txtPassword.Text := '';
      lblDatabase1.Caption := 'Destination Database Name (Creates a new database)';
    end;
    3:
    begin
      //cmbDatabase.Width := 217;

      lblUseraname.Visible := False;
      txtUserName.Visible := False;
      lblPassword.Visible := False;
      txtPassword.Visible := False;
      lblPort.Visible := False;
      txtPort.Visible := False;
      lblSever.Visible := False;
      cmbServerName.Visible := False;
      lblDatabase1.Caption := 'Destination Database Name (Creates a new database, provide full physical path like D:\Sample.sqlite)';
      txtDestinationDbName.Text:=ChangeFileExt(txtDestinationDbName.Text,'')+'.sqlite';
    end;
    4:
    begin
      txtDestinationDbName.Width := 557;
      btnOpenFile.Visible:=True;
      lblUseraname.Visible := True;
      txtUserName.Visible := True;
      lblPassword.Visible := True;
      txtPassword.Visible := True;
      lblPort.Visible := False;
      txtPort.Visible := False;
      lblSever.Visible := False;
      cmbServerName.Visible := False;
      lblDatabase1.Caption := 'Destination Database Name (Creates a new database file, provide full physical path like D:\Sample.fdb)';
      txtDestinationDbName.Text:=ChangeFileExt(txtDestinationDbName.Text,'')+'.fdb';
    end;
  end;

end;

procedure TDatabaseClonerForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TDatabaseClonerForm.btnOpenFileClick(Sender: TObject);
begin
 if OpenDialog1.Execute then
 begin
   txtDestinationDbName.Text:=OpenDialog1.FileName;
 end;
end;

procedure TDatabaseClonerForm.btnAcceptClick(Sender: TObject);
var
  dbc: TAsDatabaseCloner;
  I: integer;
  destDb:string;
  connDb:string;
  dbi:TAsDbConnectionInfo;
  lst:TStringList;
  CanMake:boolean;
begin
  try

    UpdateGUI(False);

    destDb:= txtDestinationDbName.Text;

    lstLog.Clear;
    txtErrors.Clear;


    pgLog.ActivePageIndex:=0;

    dbi:=TAsDbConnectionInfo.Create;
    dbi.DbType := TAsDatabaseType(cmbDatabaseType.ItemIndex);

    dbi.Server:=cmbServerName.Text;
    dbi.Port := StrToInt(txtPort.Text);

    dbi.Username := txtUserName.Text;
    dbi.Password := txtPassword.Text;


    dbi.DbEngineType:= deZeos;

    lst := TAsDbUtils.GetCatalogNames(dbi);

    try
      CanMake:=True;
     case TAsDatabaseType(cmbDatabaseType.ItemIndex) of
       dtSQLite,dtFirebirdd,dtOracle: connDb:=destDb;
       dtMsSql,dtMySql,dtPostgreSql:
       begin
         if lst.Count>0 then
         begin
          connDb:= lst[0];
          CanMake:=True;
         end else
         CanMake:=False;
       end;
     end;
    finally
      lst.Free;
    end;

    if not CanMake then
    begin
      ShowMessage('Cannot make a database in that server');
      UpdateGUI(true);
      Exit;
    end;

    dbi.Database:=connDb;

    WriteLog('Starting...',ltInfo);
    WriteLog('',ltInfo);



    if txtPort.Visible then
      if not TryStrToInt(txtPort.Text, i) then
      begin
        WriteLog('Port must be number.',ltError);
        WriteLog('Process stopped.',ltInfo);
        Exit;
      end;

    dbc := TAsDatabaseCloner.Create(dbi,txtDestinationDbName.Text);

    try

      dbc.MakeDatabase;

      pbProgressBar.Max := FTableInfos.Count;
      pbProgressBar.Step:=1;

      //create tables
      for I := 0 to FTableInfos.Count - 1 do
      begin
        try
         lblProgress.Caption := 'Processing [' + FTableInfos[I].Tablename + ']';

          if AnsiContainsText(FTableInfos[I].Tablename,'_SEQ') then
          begin
            pbProgressBar.StepIt;
            WriteLog('Skipped ['+FTableInfos[I].Tablename+']',ltInfo);
            Continue;
          end;

         //create table; create constraints now only for sqlite, otherwise later
         dbc.MakeTable(FTableInfos[I],(chkCreateConstraints.Checked) and (dbi.DbType=dtSQLite),true);
         pbProgressBar.StepIt;
         WriteLog('SUCCESS: Table [' + FTableInfos[I].Tablename + ']',ltInfo);
         Application.ProcessMessages;
        except on e:Exception do
          begin
            WriteLog('FAIL: Table ['+FTableInfos[I].Tablename+'] : '+e.Message,ltError);
          end;
        end;
      end;

      pbProgressBar.Position:=0;

      //creat autonumber for oracle and firebird
      if dbi.DbType in [dtOracle,dtFirebirdd] then
      for I := 0 to FTableInfos.Count - 1 do
      begin
        try
         if FTableInfos[I].Identities.Count > 0 then
         begin
          lblProgress.Caption := 'Creating  autonumbers [' + FTableInfos[I].Tablename + ']';
          dbc.MakeAutonumber(FTableInfos[I]);
          WriteLog('SUCCESS: Create autonumbers for ['+FTableInfos[I].Tablename+']',ltInfo);
         end;
         pbProgressBar.StepIt;
         Application.ProcessMessages;
        except on e:exception do
          begin
            WriteLog('FAIL: Create autonumbers for ['+FTableInfos[I].Tablename+'] '+e.Message ,ltError);
          end;
        end;
      end;

      pbProgressBar.Position:=0;
      //copy data
      if chkCopyData.Checked then
      for I := 0 to FTableInfos.Count - 1 do
        begin
          try
           lblProgress.Caption := 'Copy  data [' + FTableInfos[I].Tablename + ']';
           dbc.CopyData(FDBInfo,FTableInfos[I].Tablename,FTableInfos[I].Tablename,@OnCopyDataRow);
           pbProgressBar.StepIt;
           Application.ProcessMessages;
           WriteLog('SUCCESS: Copy data for ['+FTableInfos[I].Tablename+']',ltInfo);
           Application.ProcessMessages;
          except on e:exception do
            begin
              WriteLog('FAIL: Copy data for ['+FTableInfos[I].Tablename+'] '+e.Message,ltError);
            end;
          end;
        end;

      //create relations
      pbProgressBar.Position:=0;
      if (chkCreateConstraints.Checked) and (dbi.DbType<>dtSQLite) then
      begin
        WriteLog('',ltInfo);
        WriteLog('CONSTRAINTS',ltInfo);
        WriteLog('',ltInfo);

        if FDBInfo.DbType<>dtSQLite then
        for I := 0 to FTableInfos.Count - 1 do
        begin
          try
           lblProgress.Caption := 'Creating  relations [' + FTableInfos[I].Tablename + ']';
           dbc.CreateConstraints(FTableInfos[I]);
           pbProgressBar.StepIt;
           WriteLog('SUCCESS: Create realtions for ['+FTableInfos[I].Tablename+']',ltInfo);
           Application.ProcessMessages;
          except on e:exception do
            begin
              WriteLog('FAIL: Create relations for ['+FTableInfos[I].Tablename+'] '+e.Message,ltError);
            end;
          end;
        end;
      end;

      WriteLog('',ltInfo);
      WriteLog('PROCESS FINISHED.',ltInfo);

    except
      on E: Exception do
      begin
        //dbc.UnmakeDatabase;
        WriteLog(e.Message,ltError);
        ShowMessage(e.Message);
      end;
    end;

    WriteLog('End',ltInfo);

  finally
    pbProgressBar.Position:=0;
    lblProgress.Caption:='Progress';
    UpdateGUI(True);
    dbc.Free;
    try
      dbi.Free;
    except
        {Firebird with zeos SQL Error:  cannot disconnect database with open transactions (1 active). Error Code: -901.
        Unsuccessful execution caused by system error that does not preclude successful execution of subsequent statements}
    end;
  end;
end;

procedure TDatabaseClonerForm.FormCreate(Sender: TObject);
begin
  bmpSqlType := TBitmap.Create;
  bmpOracleType := TBitmap.Create;
  bmpMySqlType := TBitmap.Create;
  bmpSqliteType := TBitmap.Create;
  bmpFirebird := TBitmap.Create;
  bmpPostgreSql := TBitmap.Create;

  imgDatabaseTypes.GetBitmap(0, bmpSqlType);
  imgDatabaseTypes.GetBitmap(1, bmpOracleType);
  imgDatabaseTypes.GetBitmap(2, bmpMySqlType);
  imgDatabaseTypes.GetBitmap(3, bmpSqliteType);
  imgDatabaseTypes.GetBitmap(4,bmpFirebird);
  imgDatabaseTypes.GetBitmap(5,bmpPostgreSql);


end;

procedure TDatabaseClonerForm.FormDestroy(Sender: TObject);
begin
  bmpMySqlType.Free;
  bmpSqliteType.Free;
  bmpOracleType.Free;
  bmpSqlType.Free;
  bmpFirebird.Free;
  bmpPostgreSql.Free;
end;

procedure TDatabaseClonerForm.FormShow(Sender: TObject);
begin
 pgLog.ActivePageIndex:=0;
end;

procedure TDatabaseClonerForm.OnCopyDataRow(CurrentRow, TotalRows: Integer);
begin
  if pbCopyDataProgress.Max<>TotalRows then
  pbCopyDataProgress.Max:=TotalRows;
  pbCopyDataProgress.Position:=CurrentRow;
  Application.ProcessMessages;
end;

procedure TDatabaseClonerForm.pnlMainClick(Sender: TObject);
begin

end;

procedure TDatabaseClonerForm.WriteLog(Msg: string; LogType: TLogType);
var
  s:string;
begin

  if Trim(Msg)=EmptyStr then
  begin
    s := '';
  end
  else
  begin
    s := TimeToStr(Now)+'   '+Msg;
  end;

  case LogType of
    ltInfo:lstLog.Items.Add(s);
    ltError:txtErrors.Lines.Add(s);
  end;

  lstLog.ItemIndex:=lstLog.Count-1;
end;

procedure TDatabaseClonerForm.UpdateGUI(EnableControls: Boolean);
begin
pnlMain.Enabled:=EnableControls;
  btnAccept.Enabled:=EnableControls;
  btnCancel.Enabled:=EnableControls;
  txtDestinationDbName.Enabled:=EnableControls;
end;

function TDatabaseClonerForm.ShowModal(dbInfo: TAsDbConnectionInfo;
 Infos: TAsTableInfos): TModalResult;
begin
  FDBInfo := dbInfo;
  FTableInfos := Infos;
  Result := inherited ShowModal;
end;

end.
