unit AsParamDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, Db, Forms, Dialogs, StdCtrls, Controls, ExtCtrls, spin, DateTimePicker, AsDbType,
  AsTableInfo, AsStringUtils, AsSqlParser;

type

 { TAsParamDialog }

 TAsParamDialog = object
 public
  {indicates if query has null parameters, (ie. if you should call ShowParamDialog)}
  class function HasNullParams(aQuery:TAsQuery):Boolean;

  {
    Creates a modal dialog based on given arguments , [ShowNonNullParams] if you want to show already filled parameters, default:hidden
    Note: If Param.Name is different than actual FieldName, the datatype won't be detected so as a result TEdit will be generated
  }
  class function ShowParamDialog(aQuery: TAsQuery; ShowNonNullParams: Boolean
    ): TModalResult;
 end;

implementation

{ TAsParamDialog }

class function TAsParamDialog.HasNullParams(aQuery: TAsQuery): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I:=0 to aQuery.Params.Count-1 do
  begin
    if aQuery.Params[I].IsNull then
    begin
      Result := True;
      break;
    end;
  end;
end;

class function TAsParamDialog.ShowParamDialog(aQuery:TAsQuery;ShowNonNullParams: Boolean): TModalResult;
var
  lbl: TLabel;
  ctl: TWinControl;
  x, y, I, tab: integer;

  frm: TForm;
  pnl: TPanel;

  btnOk, btnCancel: TButton;
  fieldname: string;
  ik: TAsImportedKeyInfo;
  qr: TAsQuery;
  fieldInfo:TAsFieldInfo;
  fieldType:TFieldType;
  lstValues:TStringList;
  aParams:TParams;
  p:TAsSqlParser;
  fromTable:String;
  tis:TAsTableInfos;
  aTableInfo,ti:TAsTableInfo;
  aDbInfo:TAsDbConnectionInfo;
begin
  aTableInfo := nil;
  aDbInfo := aQuery.DbInfo;
  aParams := aQuery.Params;
  try
    p := TAsSqlParser.Create(aDbInfo.Schema, aDbInfo);
    p.ParseCommand(aQuery.SQL.Text);
    if (p.FromTables.Count>0) then
     fromTable:=p.FromTables[0].Name;
  finally
   p.Free;
  end;

  if Trim(fromTable)<>EmptyStr then
  begin
    try
      tis := TAsTableInfos.Create(nil,aDbInfo);
      ti := tis.Add(aDbInfo.Schema,fromTable,False);
      if ti<> nil then
      begin
        aTableInfo := TAsTableInfo.Create(nil);
        aTableInfo.Assign(ti);
      end;
    finally
      tis.Free;
    end;
  end;

  frm := TForm.Create(nil);
  frm.Width := 400;
  frm.Height := 200;
  frm.Position := poScreenCenter;
  frm.BorderStyle := bsToolWindow;
  frm.Caption:= fromTable;

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

  lstValues:=TStringList.Create;

  try

    for I:=0 to aParams.Count-1 do
    begin

      if (not aParams[I].IsNull) and (not ShowNonNullParams) then
      begin
        Continue;
      end;

      fieldName:=TAsStringUtils.SplitString( aParams[I].Name,'_')[0];

      if aTableInfo<>nil then
      begin
        if aTableInfo.ImportedKeys <> nil then
        ik := aTableInfo.ImportedKeys.GetByName(fieldname);

        fieldInfo := aTableInfo.FieldByName(aParams[I].Name);
      end;

      fieldType:=ftString;

      if fieldInfo<>nil then
      fieldType:=fieldInfo.DataType;

      if aParams[I].ParamType in [ptInput,ptInputOutput] then
      begin
        if ik<> nil then
        begin
          ctl := TComboBox.Create(frm);
            with (ctl as TComboBox) do
            begin
             AutoComplete:=True;
             qr := TAsQuery.Create(aDbInfo);
             try
               try
                qr.Open('SELECT DISTINCT '+ik.ForeignColumnName+','+ik.ForeignFirstTextField+' FROM '+ik.ForeignTableName);
                while not qr.EOF do
                begin
                 lstValues.Add(qr.Fields[0].AsString);
                 Items.Add(qr.Fields[1].AsString+' - '+qr.Fields[0].AsString);
                 qr.Next;
                end;
               except
               end;
               Text:='';
             finally
               qr.Free;
             end;

            end;
        end else
        case fieldType of
        ftInteger,ftLargeint,ftSmallint:
           begin

            ctl := TSpinEdit.Create(frm);
            (ctl as TSpinEdit).MaxValue:=9999999;
            (ctl as TSpinEdit).MinValue:=-9999999;
            if not aParams[I].IsNull then
            try
            if not aParams[I].IsNull then
            (ctl as TSpinEdit).Value:=aParams[I].Value;
            except
            end;

           end;
        ftDate,ftDateTime,ftTime:
          begin
            ctl := TDateTimePicker.Create(frm);
            (ctl as TDateTimePicker).DateTime:=Now;
            (ctl as TDateTimePicker).Kind:=dtkDateTime;
            (ctl as TDateTimePicker).AutoSize:=False;
            try
             if not aParams[I].IsNull then
            (ctl as TDateTimePicker).DateTime:=aParams[I].Value;
            except
            end;
          end;
        ftFloat,ftBCD,ftFMTBcd,ftCurrency:
          begin
            ctl := TFloatSpinEdit.Create(frm);
            (ctl as TFloatSpinEdit).DecimalPlaces:=8;
            (ctl as TFloatSpinEdit).MaxValue:=9999999;
            (ctl as TFloatSpinEdit).MinValue:=-9999999;
            try
            if not aParams[I].IsNull then
            (ctl as TFloatSpinEdit).Value:=aParams[I].Value;
            except
            end;

          end;
        ftBoolean: ctl := TCheckBox.Create(frm);
          else
          begin
              ctl := TEdit.Create(frm);
              (ctl as TEdit).Text:='';
               try
                if not aParams[I].IsNull then
                (ctl as TEdit).Text:=aParams[I].Value;
               except
               end;

          end;
        end;
        lbl := TLabel.Create(frm);
        lbl.Parent := frm;
        ctl.Parent := frm;
        ctl.Name := aParams[I].Name;

        if (ctl is TEdit) then
        (ctl as TEdit).Text:='';

        if (ctl is TComboBox) then
        (ctl as TComboBox).Text:='';

        lbl.Caption := TAsStringUtils.SplitByUppercaseLetter(fieldname);
        lbl.Name := 'lbl' + aParams[I].Name;
        lbl.Left := x;
        lbl.Top := y+2;

        lbl.Width := 150;

        ctl.Left := lbl.Width + x + 2;
        ctl.Top := y + 4;
        ctl.Width := 230;

        Inc(tab, 1);
        ctl.TabOrder := tab;

        Inc(Y, ctl.Height + 2);
      end; //if param is input

    end; //for each param

    frm.Height := y + 50;

    Result := frm.ShowModal;

    //Now assign values
   if Result=mrOK then
   begin
    for I:=0 to aParams.Count-1 do
    begin

      if (not aParams[I].IsNull) and (not ShowNonNullParams) then
      begin
        Continue;
      end;

      ctl := frm.FindChildControl(aParams[I].Name) as TWinControl;
      if ctl <> nil then
      begin
       if (ctl is TSpinEdit) then
       begin
         aParams[I].Value := (ctl as TSpinEdit).Value;

       end
       else
       if (ctl is TDateTimePicker) then
       begin
         aParams[I].Value:= (ctl as TDateTimePicker).Date;
       end
       else
       if (ctl is TFloatSpinEdit) then
       begin
         aParams[I].Value := (ctl as TFloatSpinEdit).Value
       end
       else
       if (ctl is TCheckBox) then
       begin
          aParams[I].Value:=(ctl as TCheckBox).Checked;
       end
       else
       if (ctl is TEdit) then
       begin
          aParams[I].Value := (ctl as TEdit).Text;
       end else
       if (ctl is TComboBox) then
       begin
          if (ctl as TComboBox).ItemIndex>-1 then
          aParams[I].Value:= lstValues[(ctl as TComboBox).ItemIndex]
          else
            ShowMessage(Format('You didn''t select a value from list [%s]',[(ctl as TComboBox).Name]));
       end;
      end; //if ctrl not null
    end; //foreach param value assignment
   end; //if ModalResult OK

  finally
    if aTableInfo<>nil then aTableInfo.Free;
    frm.Free;
    lstValues.Free;
  end;
end;


end.

