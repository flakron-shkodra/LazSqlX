unit QueryDesignerDialogU;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  AsTableInfo, AsDbType, QueryDesignerU, Buttons,
  ExtCtrls,ZConnection;

type

  { TQueryDesignerDialog }

  TQueryDesignerDialog = class(TForm)
    grbConfigure: TGroupBox;
    lblForeignKey: TLabel;
    lblForeignTable: TLabel;
    cmbValueMember: TComboBox;
    cmbForeignTable: TComboBox;
    pnlBottom: TPanel;
    btnAccept: TButton;
    btnCancel: TButton;
    pnlSelectFields: TPanel;
    cmbDisplayMember: TComboBox;
    btnAddField: TButton;
    btnDel: TSpeedButton;
    lblDisplayField: TLabel;
    lblSourceField: TLabel;
    cmbSourceField: TComboBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure cmbForeignTableChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbSelectFieldChanged(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnAddFieldClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
  private
    { Private declarations }
    FDBInfo:TAsDbConnectionInfo;
    FDBSchema: string;
    FKeyField: string;
    FSourceTable:String;
    FTableName: string;
    FSourceField:string;
    procedure AddSelectFieldControl(selectedItem: string);
    procedure ClearSelectFields;
    procedure RearrangeTags;
  public
    { Public declarations }
    SelectFields: TStringList;
    function ShowModal(Con:TAsDbConnectionInfo;
      Schema, SourceTable,SourceField,ForiegnTable, ForeignKeyField: string; aSelectFields: TStringList)
      : TModalResult;
  end;

var
  QueryDesignerDialog: TQueryDesignerDialog;

implementation

{$R *.lfm}



{ TConfigureRelationForm }

procedure TQueryDesignerDialog.AddSelectFieldControl(selectedItem: string);
var
  pnl: TPanel;
  cmb: TComboBox;
  lbl: TLabel;
  btn: TSpeedButton;
begin
  pnl := TPanel.Create(pnlSelectFields);
  pnlSelectFields.InsertControl(pnl);
  pnl.Caption := '';
  pnl.Height := 25;
  pnl.Hint := selectedItem;
  pnl.Align := alTop;

  lbl := TLabel.Create(pnl);
  cmb := TComboBox.Create(pnl);
  btn := TSpeedButton.Create(pnl);

  pnl.InsertControl(lbl);
  pnl.InsertControl(cmb);
  pnl.InsertControl(btn);

  lbl.Caption := 'Select Field';
  lbl.Left := 2;
  lbl.Top := 3;
  lbl.AutoSize := True;

  cmb.Items.AddStrings(cmbDisplayMember.Items);
  cmb.Left := cmbDisplayMember.Left;
  cmb.Width := cmbDisplayMember.Width;
  cmb.Top := 3;
  cmb.Text := selectedItem;
  cmb.Tag := 0;
  cmb.Style := csSimple;
  cmb.Enabled := False;

  cmb.OnChange := cmbSelectFieldChanged;

  if SelectFields.Count > 0 then
    cmb.Tag := SelectFields.Count - 1;

  btn.Caption := '-';
  btn.Left := btnDel.Left;
  btn.Top := 3;
  btn.OnClick := btnDelClick;

  Self.Height := Self.Height + 25;
end;

procedure TQueryDesignerDialog.btnAddClick(Sender: TObject);
begin
  AddSelectFieldControl('');
end;

procedure TQueryDesignerDialog.btnAddFieldClick(Sender: TObject);
begin
  if cmbDisplayMember.Text <> '' then
  begin
    if SelectFields.IndexOf(cmbDisplayMember.Text) > -1 then
    begin
      ShowMessage('Already Exists');
    end
    else
    begin
      cmbDisplayMember.Color := clWindow;
      SelectFields.Add(cmbDisplayMember.Text);
      AddSelectFieldControl(cmbDisplayMember.Text);
    end;
  end
  else
  begin
    cmbDisplayMember.Color := clPurple;
  end;
end;

procedure TQueryDesignerDialog.btnDelClick(Sender: TObject);
var
  t: TPanel;
begin
  if (Sender is TSpeedButton) then
    if ((Sender as TSpeedButton).Parent is TPanel) then
    begin
      t := (Sender as TSpeedButton).Parent as TPanel;
      if t.Hint <> '' then
        if (SelectFields.IndexOf(t.Hint) > -1) then
          SelectFields.Delete(SelectFields.IndexOf(t.Hint));
      t.Free;
    end;
  RearrangeTags;
end;

procedure TQueryDesignerDialog.ClearSelectFields;
var
  I: Integer;
begin
  for I := 0 to pnlSelectFields.ControlCount - 1 do
    pnlSelectFields.Controls[0].Free;
  SelectFields.Clear;
end;

procedure TQueryDesignerDialog.cmbForeignTableChange(Sender: TObject);
var
  lst:TStringList;
begin

  if cmbForeignTable.Items.Count=0 then
  exit;

  cmbValueMember.Items.Clear;
  cmbDisplayMember.Items.Clear;

  try
    lst := TAsDbUtils.GetColumnNames(FDBInfo,cmbForeignTable.Items[cmbForeignTable.ItemIndex]);
    cmbValueMember.Items.AddStrings(lst);
    cmbDisplayMember.Items.AddStrings(lst);
  finally
    lst.Free;
  end;
end;

procedure TQueryDesignerDialog.cmbSelectFieldChanged(Sender: TObject);
var
  c: TComboBox;
begin
  if Sender is TComboBox then
  begin
    c := Sender as TComboBox;
    SelectFields[c.Tag] := c.Text;
  end;
end;

procedure TQueryDesignerDialog.FormCreate(Sender: TObject);
begin
  SelectFields := TStringList.Create;

end;

procedure TQueryDesignerDialog.FormDestroy(Sender: TObject);
begin
  SelectFields.Free;
end;

procedure TQueryDesignerDialog.FormClose(Sender: TObject;
 var CloseAction: TCloseAction);
begin
end;

procedure TQueryDesignerDialog.FormShow(Sender: TObject);
var
  I: Integer;
  lst:TStringList;
begin
  cmbDisplayMember.Color := clWindow;
  Height := 211;

    lst := TAsDbUtils.GetTableNames(FDBInfo);
    try
      cmbForeignTable.Items.AddStrings(lst);
    finally
      lst.Free;
    end;


    lst := TAsDbUtils.GetColumnNames(FDBInfo,FSourceTable);
    try
      cmbSourceField.Items.AddStrings(lst);
    finally
      lst.Free;
    end;

    cmbForeignTable.ItemIndex := cmbForeignTable.Items.IndexOf(FTableName);
    cmbForeignTableChange(nil);


    cmbValueMember.Text := FKeyField;

    if FSourceField<>'' then
    cmbSourceField.Text := FSourceField
    else
    begin
      if cmbSourceField.Items.Count>0 then
      cmbSourceField.ItemIndex := 0;
    end;



    // cmbDisplayMember.Text := FDisplayField[0];

    for I := 0 to SelectFields.Count - 1 do
    begin
      AddSelectFieldControl(SelectFields[I]);
    end;

end;

procedure TQueryDesignerDialog.RearrangeTags;
var
  I: Integer;
  t: TPanel;
  c: TComboBox;
  J: Integer;
begin
  if pnlSelectFields.ControlCount = SelectFields.Count then
    for I := 0 to SelectFields.Count - 1 do
    begin
      if pnlSelectFields.Controls[I] is TPanel then
      begin
        t := pnlSelectFields.Controls[I] as TPanel;
        for J := 0 to t.ControlCount - 1 do
        begin
          if t.Controls[J] is TComboBox then
          begin
            c := t.Controls[J] as TComboBox;
            c.Tag := I;
          end;
        end;

      end;

    end;
end;

function TQueryDesignerDialog.ShowModal(Con: TAsDbConnectionInfo; Schema,
 SourceTable, SourceField, ForiegnTable, ForeignKeyField: string;
 aSelectFields: TStringList): TModalResult;
begin
  FDBInfo := Con;
  FDBSchema := Schema;
  FTableName := ForiegnTable;
  FKeyField := ForeignKeyField;
  SelectFields := SelectFields;
  FSourceTable := SourceTable;
  FSourceField := SourceField;

  ClearSelectFields;
  if SelectFields <> nil then
    SelectFields.AddStrings(aSelectFields);

  Result := inherited ShowModal;

end;

end.
