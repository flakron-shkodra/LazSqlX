{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}

unit ProcedureInfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DbType, ZConnection, ZDataset, ZStoredProcedure,
  ZDbcIntfs, ZSqlMetadata,
  DB, typinfo, fgl, Forms, Controls, StdCtrls, ExtCtrls, Buttons, EditBtn, Spin, Dialogs,
  ZAbstractRODataset,sqldb;

type

  { TProcedureInfo }

  TProcedureInfo = class
  private
    FUsername: string;
    FPassword: string;
    FDatabase: string;
    FServer: string;
    FPort: integer;
    FDatabaseType: TDatabaseType;
    FSchema: string;
    qr: TZQuery;
    adoCon: TZConnection;
    dsMetaData: TZSQLMetadata;
    FOwner:TComponent;
    function GetFieldType(SqlType: string): TFieldType;
  public
    constructor Create; overload;
    constructor Create(Owner: TComponent; ASchema, Server, DatabaseName, Username,
     Password: string; ADatabaseType: TDatabaseType; Port: integer); overload;
    function GetProcedureNames: TStringList;

    function RunProcedure(procName: string): TDataSet;

    property DatabaseType: TDatabaseType read FDatabaseType write FDatabaseType;
    property Server: string read FServer write FServer;
    property Database: string read FDatabase write FDatabase;
    property User: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Port: integer read FPort write FPort;
    destructor Destroy; override;
  end;



implementation

uses AsStringUtils;

{ TProcedureInfo }

function TProcedureInfo.GetFieldType(SqlType: string): TFieldType;
begin
  SqlType:= LowerCase(SqlType);

  if (SqlType = 'varchar') or (SqlType = 'nvarchar') or (SqlType = 'text') or
    (SqlType = 'nchar') or (SqlType = 'char') or (SqlType = 'ntext') or
    (SqlType = 'xml') or (SqlType='varchar2') then
    Result := ftString
  else
  if SqlType = 'image' then
    Result := ftBlob
  else
  if (SqlType = 'tinyint') or (SqlType = 'int') or (SqlType = 'bigint') or
    (SqlType = 'smallint') then
    Result := ftInteger
  else
  if (SqlType = 'real') or (SqlType = 'float') or (SqlType = 'decimal') or
    (SqlType = 'money') or (SqlType = 'smallmoney') or (SqlType='number') then
    Result := ftFloat
  else
  if SqlType = 'bit' then
    Result := ftBoolean
  else
  if (SqlType = 'datetime') or (SqlType='date') then
    Result := ftDateTime
  else
  if (SqlType = 'REF CURSOR') then
    Result := ftCursor
  else
    Result := ftString;
end;

constructor TProcedureInfo.Create;
begin
  inherited Create;
  adoCon := TZConnection.Create(nil);
  dsMetaData := TZSQLMetadata.Create(nil);
end;


constructor TProcedureInfo.Create(Owner: TComponent; ASchema, Server,
 DatabaseName, Username, Password: string; ADatabaseType: TDatabaseType;
 Port: integer);
begin

  Create;
  FOwner:=Owner;
  FSchema := ASchema;
  FServer := Server;
  FDatabase := DatabaseName;
  FUsername := Username;
  FPassword := Password;
  adoCon.Protocol := TDbUtils.DatabaseTypeAsString(ADatabaseType, True);
  adoCon.Database := FDatabase;

  if ADatabaseType <> dtSQLite then
  begin
    adoCon.Catalog := FDatabase;
    adoCon.HostName := FServer;
    adoCon.User := Username;
    adoCon.Password := FPassword;
    adoCon.Port := Port;
    adoCon.LoginPrompt := False;
    dsMetaData.Catalog := FDatabase;
  end;
  DatabaseType := ADatabaseType;

  dsMetaData.Connection := adoCon;
  dsMetaData.DisableControls;

  adoCon.Connect;

  qr := TZQuery.Create(nil);
  qr.Connection := adoCon;

  qr.DisableControls;

end;

function TProcedureInfo.GetProcedureNames: TStringList;
var
  storedProcedures: TStringList;
  I: integer;
begin
  storedProcedures := TStringList.Create;

  case FDatabaseType of
    dtMsSql,dtFirebirdd:
    begin
      adoCon.GetStoredProcNames('', storedProcedures);
      storedProcedures.Text:= StringReplace(storedProcedures.Text,';1','',[rfReplaceAll]);
      storedProcedures.Text:= StringReplace(storedProcedures.Text,';0','',[rfReplaceAll]);
    end;
    dtOracle:
    begin
      if qr.Active then
        qr.Close;
      qr.SQL.Text := 'select OBJECT_NAME from SYS.ALL_OBJECTS' +
        ' where upper(OBJECT_TYPE) = upper(''PROCEDURE'') and' +
        ' Owner=''' + UpperCase(adoCon.User) + '''' + ' order by OBJECT_NAME';
      try
        qr.Open;
        while not qr.EOF do
        begin
          storedProcedures.Add(qr.Fields[0].AsString);
          qr.Next;
        end;
      finally
        qr.Close;
      end;
    end;
    dtMySql:
    begin
      if qr.Active then
        qr.Close;
      qr.SQL.Text := 'SELECT SPECIFIC_NAME FROM INFORMATION_SCHEMA.ROUTINES ' +
        ' WHERE ROUTINE_SCHEMA=''' + adoCon.Database + '''';
      try

        qr.Open;
        while not qr.EOF do
        begin
          storedProcedures.Add(qr.Fields[0].AsString);
          qr.Next;
        end;
      finally
        qr.Close;
      end;

      for I := 0 to storedProcedures.Count - 1 do
      begin
        storedProcedures[I] :=
          StringReplace(storedProcedures[I], ';1', '', [rfReplaceAll]);
      end;

    end;
  end;

  Result := storedProcedures;

end;

function TProcedureInfo.RunProcedure(procName: string): TDataSet;
var
  I: integer;

  lbl: TLabel;
  ctl: TWinControl;
  x, y: integer;

  frm: TForm;
  pnl: TPanel;
  lst: TList;
  s: string;
  md: TDataSet;
  metaDat:TZSQLMetadata;
  proc: TZStoredProc;
  tab: integer;
  paramName: string;
  sqlExecSp: string;
  prm: TParam;
  paramType: TFieldType;

  btnOk, btnCancel: TButton;

  prmPrefix: string;
  beginSp: string;
  endSp: string;
  equalOp:string;
  dateFormat:TFormatSettings;
  g: TGuid;
  pt: TParamType;
  ValuesOnly:string;
  outResultVar:string;
  prmCount: Integer;
  ColumnType: String;
  IntRecNo: LongInt;

begin
  Result := nil;

  metaDat := TZSQLMetadata.Create(nil);
  proc := TZStoredProc.Create(nil);



  frm := TForm.Create(nil);
  frm.Width := 335;
  frm.Height := 200;
  frm.Position := poScreenCenter;
  frm.BorderStyle := bsToolWindow;


  pnl := TPanel.Create(frm);
  pnl.Align := alBottom;
  pnl.Height := 30;


  frm.InsertControl(pnl);

  btnOk := TButton.Create(pnl);
  btnOk.ModalResult := mrOk;
  btnOk.Caption := 'OK';

  btnCancel := TButton.Create(pnl);
  btnCancel.ModalResult := mrCancel;
  btnCancel.Caption := 'Cancel';

  pnl.InsertControl(btnOk);
  pnl.InsertControl(btnCancel);

  btnCancel.Left := frm.Width - btnCancel.Width - 5;
  btnOk.Left := frm.Width - btnOk.Width - btnCancel.Width - 5;

  btnCancel.Top := 2;
  btnOk.Top := 2;
  btnOk.Default := True;


  x := 5;
  y := 5;
  procName := StringReplace(procName, ';1', '', [rfReplaceAll]);

  case DatabaseType of
    dtOracle:
    begin
      prmPrefix := '';
      beginSp := 'CALL ' + LineEnding + procName + ' (';

      endSp := ') ' + LineEnding + ' ';
    end;
    dtMsSql:
    begin
      prmPrefix := '@';
      beginSp := 'exec ' + procName + ' ';
      endSp := '';
    end;
    dtMySql:
    begin
      prmPrefix := '@';
      beginSp := '' + procName + ' ';
      endSp := '';
    end;
    dtSQLite:
    begin
      prmPrefix := '';
      beginSp := '';
      endSp := '';
    end;
    dtFirebirdd:
    begin
      prmPrefix := '';
      beginSp := '';
      endSp := '';
    end;
  end;

  try

    metaDat.Connection := adoCon;
    metaDat.ProcedureName := procname;
    metaDat.MetadataType := mdProcedureColumns;


    proc.Connection := adoCon;
    proc.StoredProcName := procname;

    prmCount := proc.Params.Count;
    metaDat.Open;


    if DatabaseType = dtOracle then
    begin
      qr.Close;
      qr.SQL.Text:=
      'SELECT Argument_Name as COLUMN_NAME,DATA_TYPE as TYPE_NAME '+
      ' FROM SYS.ALL_ARGUMENTS WHERE OBJECT_NAME='''+procName+'''';
      qr.Open;
      md :=  (qr as TDataSet);
    end else
    begin
      md := (metaDat as TDataSet);
    end;

    //create control dynamically for each procedure parameter
    tab := 5;

    while not md.EOF do
    begin

      paramName := StringReplace(md.FieldByName('COLUMN_NAME').AsString,
        '@', '', [rfReplaceAll]);

      if FDatabaseType=dtMySql then
      ColumnType :=md.FieldByName('COLUMN_TYPE').AsString;

      paramType := GetFieldType(md.FieldByName('TYPE_NAME').AsString);

      if (md.FieldByName('COLUMN_NAME').AsString = '@RETURN_VALUE') or
          (md.FieldByName('TYPE_NAME').AsString = 'REF CURSOR') then
      begin
        if DatabaseType = dtOracle then
        begin
        if proc.Params.ParamByName(paramName) = nil then
         proc.Params.CreateParam(paramType, paramName, ptOutput);
        end;
        md.Next;
        Continue;
      end;

      if FDatabaseType=dtMySql then
      if (ColumnType <> '1') then//means input only
      begin
        md.Next;
        Continue;
      end;

      IntRecNo := md.RecNo;

        if proc.Params.FindParam(paramName) = nil then
        proc.Params.CreateParam(paramType, paramName, ptInput);

      case paramType of
        ftString:
        begin
          ctl := TEdit.Create(frm);
           //default value for guid,
          if (lowercase(md.FieldByName('TYPE_NAME').AsString)='uniqueidentifier') then
          begin
            CreateGUID(g);
            (ctl as TEdit).Text:=TAsStringUtils.RemoveChars( GUIDToString(g),['{','}']);
          end;
        end;
        ftInteger: ctl := TSpinEdit.Create(frm);
        ftDate, ftDateTime:
        begin
          ctl := TDateEdit.Create(frm);
          (ctl as TDateEdit).Date:=Date;
        end;
        ftFloat:
          begin
            ctl := TFloatSpinEdit.Create(frm);
            (ctl as TFloatSpinEdit).MaxValue:=9999999;
          end;
        ftBoolean: ctl := TCheckBox.Create(frm);
        else
        begin
          ctl := TEdit.Create(frm);
        end;
      end;

      lbl := TLabel.Create(frm);

      lbl.Parent := frm;
      ctl.Parent := frm;


      lbl.Caption := paramName;
      lbl.Name := 'lbl' + paramName;
      lbl.Left := x;
      lbl.Top := y;

      lbl.Width := 150;


      ctl.Name := paramName;
      ctl.Left := lbl.Width + x + 2;
      ctl.Top := y + 4;
      ctl.Width := 150;
      Inc(tab, 1);
      ctl.TabOrder := tab;

      frm.InsertControl(lbl);
      frm.InsertControl(ctl);

      Inc(Y, ctl.Height + 2);

      md.Next;
    end;


    prmCount:=proc.Params.Count;
    //frm.AutoSize := True;
    frm.Height := y + 50;

    sqlExecSp := beginSp;

    if frm.ShowModal = mrOk then
    begin

      for I := 0 to proc.Params.Count - 1 do
      begin

        prm := proc.Params[I];

        pt := prm.ParamType;

        if prm <> nil then
        begin
          paramType := prm.DataType;
          case DatabaseType of
            dtOracle:
            begin
              paramName := '';
              equalOp:='';
            end;
            dtMsSql:
            begin
              paramName := prm.Name;
              equalOp:=' = ';
            end;
            dtMySql:
            begin
              paramName := prm.Name;
              equalOp:=' = ';
            end;
            dtSQLite:
            begin
              paramName := prm.Name;
              equalOp:=' = ';
            end;
          end;

          ctl := frm.FindChildControl(prm.Name) as TWinControl;
          if ctl <> nil then
          begin
            if (ctl is TSpinEdit) then
            begin
              prm.AsInteger := (ctl as TSpinEdit).Value;
              sqlExecSp := sqlExecSp + prmPrefix + paramName +equalOp+
                IntToStr(prm.AsInteger);
              ValuesOnly:=ValuesOnly+IntToStr(prm.AsInteger);
            end
            else
            if (ctl is TDateEdit) then
            begin
              dateFormat.ShortDateFormat:='yyyy-MM-dd';
              prm.AsDate := (ctl as TDateEdit).Date;
              sqlExecSp := sqlExecSp + prmPrefix + paramName + equalOp
                +''''+DateToStr(prm.AsDateTime,dateFormat) + '''';
              ValuesOnly:=''''+ValuesOnly+prm.AsString+'''';
            end
            else
            if (ctl is TFloatSpinEdit) then
            begin
              prm.AsFloat := (ctl as TFloatSpinEdit).Value;
              sqlExecSp := sqlExecSp + prmPrefix + paramName + equalOp +
                FloatToStr(prm.AsFloat);
              ValuesOnly:=ValuesOnly+prm.AsString;
            end
            else
            if (ctl is TCheckBox) then
            begin
              prm.AsBoolean := (ctl as TCheckBox).Checked;
              sqlExecSp := sqlExecSp + prmPrefix + paramName + equalOp +
                IntToStr(integer(prm.AsBoolean));
              ValuesOnly:=IntToStr(integer(prm.AsBoolean));
            end
            else
            if (ctl is TEdit) then
            begin
              prm.AsString := (ctl as TEdit).Text;
              sqlExecSp := sqlExecSp + prmPrefix + paramName +equalOp + '''' +
                prm.AsString + '''';
              ValuesOnly:=''''+ValuesOnly+prm.AsString+'''';
            end;
            if I < proc.Params.Count - 1 then
            begin
              sqlExecSp := sqlExecSp + ', ';
              ValuesOnly:=ValuesOnly+', ';
            end;
          end;
        end;
      end;

      try

        {
          doOemTranslate, doCalcDefaults, doAlwaysDetailResync,
          doSmartOpen, doPreferPrepared, doPreferPreparedResolver, doDontSortOnPost
        }


        //proc.Options:=proc.Options+[doSmartOpen];
        //proc.Open;

        ValuesOnly:=Trim(ValuesOnly);
        if ValuesOnly<>EmptyStr then
        begin
         if ValuesOnly[Length(ValuesOnly)]=',' then
         ValuesOnly:= Copy(ValuesOnly,1,Length(ValuesOnly)-1);
        end;

        sqlExecSp := sqlExecSp + endSp;
        case DatabaseType of
          dtMsSql:
          begin
            qr.SQL.Text := //'SET ARITHABORT ON '+
            'SET CONCAT_NULL_YIELDS_NULL ON ' +
            //'SET QUOTED_IDENTIFIER ON '+
            'SET ANSI_NULLS ON ' + 'SET ANSI_PADDING ON ' +
            'SET ANSI_WARNINGS ON ' +
            //'SET NUMERIC_ROUNDABORT OFF '+
            sqlExecSp;
            qr.Open;
            Result := qr;
          end;
          dtOracle:
          begin
            qr.SQL.Text:='CALL '+procName+'('+ValuesOnly+')';
            qr.Open;
            Result := qr;
          end;
          dtMySql:
          begin
              qr.Close;

              if proc.Params.Count>0 then
              ValuesOnly:=ValuesOnly+',@outtxt';

              qr.sql.Text:= 'CALL '+procName+'('+ValuesOnly+')';
              qr.Open;
              Result:=qr;
          end;
          dtFirebirdd:
          begin
            proc.Open;
            Result := proc;
          end;
        end;



      except
        on e: Exception do
          ShowMessage(e.Message);
      end;

    end;

  finally
    metaDat.Free;
    frm.Free;
  end;

end;

destructor TProcedureInfo.Destroy;
begin
  adoCon.Free;
  dsMetaData.Free;
  inherited Destroy;
end;

end.





