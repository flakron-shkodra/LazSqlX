{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}

unit AsProcedureInfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, AsDbType,  DB, typinfo, Forms, Controls, StdCtrls, ExtCtrls, Buttons, EditBtn, Spin, Dialogs,
  sqldb,dynlibs;

type

  { TAsProcedureInfo }

  TAsProcedureInfo = class
  private
    RemHandle:THandle;
    FDbInfo:TAsDbConnectionInfo;
    function GetFieldType(SqlType: string): TFieldType;
    procedure ExecuteOracleProcedure(ProcName: string; Params: array of Variant);
  public
    constructor Create(aDbInfo:TAsDbConnectionInfo);
    function GetRunProcedureText(procName: string; ShowGUI: Boolean): string;
  end;



implementation

uses AsStringUtils;

{ TAsProcedureInfo }

function TAsProcedureInfo.GetFieldType(SqlType: string): TFieldType;
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
  if (SqlType = 'tinyint') or (SqlType = 'int') or (SqlType = 'bigint') or  (SqlType='int64') or
    (SqlType = 'smallint') then
    Result := ftInteger
  else
  if (SqlType = 'real') or (SqlType = 'float') or (SqlType = 'decimal') or (SqlType='d_float') or (SqlType='double') or
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

procedure TAsProcedureInfo.ExecuteOracleProcedure(ProcName: string;
 Params: array of Variant);
type
 TExeOraSp = procedure(a:THandle; out b:THandle; Db,User,Password,ProcName:string; const Params: array of variant);stdcall;
var
  dll:THandle;
  dllProc:TExeOraSp;
  db,usr,pwd:string;
begin
  dll := LoadLibrary('OraSpLib.dll');
  try
    Pointer(dllProc) := GetProcAddress(dll,'Execute');
    if dllProc <> nil then
    begin
     db :=  TAsDbUtils.GetOracleDescriptor(FDbInfo);
     usr := FDbInfo.Username;
     pwd := FDbInfo.Password;

     dllProc(0,RemHandle, db,usr,pwd,ProcName, Params);
    end;
  finally
    FreeLibrary(dll);
  end;
end;


constructor TAsProcedureInfo.Create(aDbInfo: TAsDbConnectionInfo);
begin
  FDbInfo := aDbInfo;
end;


function TAsProcedureInfo.GetRunProcedureText(procName: string; ShowGUI: Boolean
 ): string;
var
  I: integer;
  lbl: TLabel;
  ctl: TWinControl;
  x, y: integer;
  frm: TForm;
  pnl: TPanel;
  tab: integer;
  paramName: string;
  sqlExecSp: string;
  btnOk, btnCancel: TButton;
  prmPrefix: string;
  beginSp: string;
  endSp: string;
  equalOp:string;
  dateFormat:TFormatSettings;
  g: TGuid;
  ValuesOnly:string;
  AllProcParams,ProcParams:TAsProcedureParams;

  p:TAsProcedureParam;
  paramType:TFieldType;
  ShouldContinue:Boolean;
  sVal: String;
  fs:TFormatSettings;
  oraParam:array of Variant;
begin


  frm := TForm.Create(nil);
  frm.Width := 335;
  frm.Height := 200;
  frm.Position := poScreenCenter;
  frm.BorderStyle := bsToolWindow;

  pnl := TPanel.Create(frm);
  pnl.Parent := frm;
  pnl.Align := alBottom;
  pnl.Height := 30;



  btnOk := TButton.Create(pnl);
  btnOk.Parent:=pnl;
  btnOk.ModalResult := mrOk;
  btnOk.Caption := 'OK';

  btnCancel := TButton.Create(pnl);
  btnCancel.Parent:=pnl;
  btnCancel.ModalResult := mrCancel;
  btnCancel.Caption := 'Cancel';



  btnCancel.Left := frm.Width - btnCancel.Width - 5;
  btnOk.Left := frm.Width - btnOk.Width - btnCancel.Width - 5;

  btnCancel.Top := 2;
  btnOk.Top := 2;
  btnOk.Default := True;


  x := 5;
  y := 5;
  procName := StringReplace(procName, ';1', '', [rfReplaceAll]);

  case FDbInfo.DbType of
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


    AllProcParams := TAsDbUtils.GetProcedureParams(FDbInfo,procName);

    ProcParams := TAsProcedureParams.Create;

    for p in AllProcParams do
    begin
     paramType:= GetFieldType(p.Data_Type);
     if (CompareText(p.Param_Type,'OUT')=0) or (p.Param_name = '@RETURN_VALUE') or (p.Data_Type = 'REF CURSOR') then
     begin
        Continue;
     end else
     ProcParams.Add(p);
    end;

    //create control dynamically for each procedure parameter
    tab := 5;

    SetLength(oraParam,ProcParams.Count);

    for p in ProcParams do
    begin

      paramName := StringReplace(p.Param_name,'@', '', [rfReplaceAll]);
      paramType:= GetFieldType(p.Data_Type);
      case paramType of
        ftString:
        begin
          ctl := TEdit.Create(frm);
           //default value for guid,
          if (lowercase(p.Data_Type)='uniqueidentifier') then
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
            (ctl as TFloatSpinEdit).MinValue:=-9999999;
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

      Inc(Y, ctl.Height + 2);

    end;


    frm.Height := y + 50;

    sqlExecSp := beginSp;

      ShouldContinue:=True;

     if (ShowGUI) then
     if frm.ShowModal <> mrOk then
     begin
       ShouldContinue:=False;
     end;

    if ShouldContinue then
    begin


     for I:=0 to ProcParams.Count-1 do
     begin

       paramType:=GetFieldType(p.Data_Type);

         paramName := StringReplace(AllProcParams[I].Param_name,'@', '', [rfReplaceAll]);
         case FDbInfo.DbType of
           dtOracle:equalOp:='';
           dtFirebirdd:equalOp:='';
           dtMsSql: equalOp:=' = ';
           dtMySql: equalOp:=' = ';
           dtSQLite: equalOp:=' = ';
           dtPostgreSql: equalOp:='';
         end;

         ctl := frm.FindChildControl(paramName) as TWinControl;
         if ctl <> nil then
         begin
           if (ctl is TSpinEdit) then
           begin
             sqlExecSp := sqlExecSp + prmPrefix + paramName +equalOp+
             IntToStr((ctl as TSpinEdit).Value);
             ValuesOnly:=ValuesOnly+IntToStr((ctl as TSpinEdit).Value);
             oraParam[I] := (ctl as TSpinEdit).Value;
           end
           else
           if (ctl is TDateEdit) then
           begin
             fs.ShortDateFormat:='yyyy-MM-dd';
             sqlExecSp := sqlExecSp + prmPrefix + paramName + equalOp
               +''''+DateToStr((ctl as TDateEdit).Date,dateFormat) + '''';
             sVal :=''''+DateToStr((ctl as TDateEdit).Date)+'''';
             if FDbInfo.DbType = dtOracle then
             begin
              sVal := 'TO_DATE('''+DateToStr((ctl as TDateEdit).Date,fs)+''','''+fs.ShortDateFormat+''')';
             end;
             ValuesOnly:=ValuesOnly+sVal;
             oraParam[I] := (ctl as TDateEdit).Date;
           end
           else
           if (ctl is TFloatSpinEdit) then
           begin
             sqlExecSp := sqlExecSp + prmPrefix + paramName + equalOp +
               FloatToStr((ctl as TFloatSpinEdit).Value);
             ValuesOnly:=ValuesOnly+ FloatToStr((ctl as TFloatSpinEdit).Value);
             oraParam[I] := (ctl as TFloatSpinEdit).Value;
           end
           else
           if (ctl is TCheckBox) then
           begin
             sqlExecSp := sqlExecSp + prmPrefix + paramName + equalOp +
               IntToStr(integer((ctl as TCheckBox).Checked));

             ValuesOnly:=ValuesOnly+ IntToStr( integer((ctl as TCheckBox).Checked));
             oraParam[I] := integer((ctl as TCheckBox).Checked);
           end
           else
           if (ctl is TEdit) then
           begin
             sqlExecSp := sqlExecSp + prmPrefix + paramName +equalOp + '''' +
               (ctl as TEdit).Text + '''';
             ValuesOnly:=ValuesOnly+''''+(ctl as TEdit).Text+'''';
             oraParam[I] := (ctl as TEdit).Text;
           end;
           if I < AllProcParams.Count - 1 then
           begin
             sqlExecSp := sqlExecSp + ', ';
             ValuesOnly:=ValuesOnly+', ';
           end;
         end;
     end;

      try

        ValuesOnly:=Trim(ValuesOnly);
        if ValuesOnly<>EmptyStr then
        begin
         if ValuesOnly[Length(ValuesOnly)]=',' then
         ValuesOnly:= Copy(ValuesOnly,1,Length(ValuesOnly)-1);
        end;

        sqlExecSp := sqlExecSp + endSp;
        case FDbInfo.DbType of
          dtMsSql:
          begin
            Result := sqlExecSp;
          end;
          dtOracle:
          begin
            Result := 'CALL '+procName+'('+ValuesOnly+')';
          end;
          dtMySql:
          begin
            Result := 'CALL '+procName+'('+ValuesOnly+')';
          end;
          dtFirebirdd:
          begin
            Result := 'execute procedure '+procName+' '+ValuesOnly;
          end;
          dtPostgreSql:
          begin
            Result := 'select '+procName+'('+ValuesOnly+')';
          end;
        end;

        if (FDbInfo.DbType=dtOracle) and (ShowGUI) then
        begin
         Result :='';
         ExecuteOracleProcedure(procName,oraParam);
        end;

      except
        on e: Exception do
          ShowMessage(e.Message);
      end;

    end;

  finally
    ProcParams.Free;
    frm.Free;
  end;

end;


end.





