unit ConnectionBuilderFormU;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ZConnection, TableInfo, strutils,Utils;

type

  { TConnectinoBuilderForm }

  TConnectinoBuilderForm = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    cmbDatabaseType: TComboBox;
    Label2: TLabel;
    lblPort: TLabel;
    btnOpenDatabase: TSpeedButton;
    txtConnStr: TMemo;
    pnlMain: TPanel;
    Bevel1: TBevel;
    chkIntegratedSecurity: TCheckBox;
    lblUseraname: TLabel;
    txtPort: TEdit;
    txtUserName: TEdit;
    lblPassword: TLabel;
    txtPassword: TEdit;
    lblSever: TLabel;
    cmbServerName: TComboBox;
    lblDatabase: TLabel;
    cmbDatabase: TComboBox;
    txtSchema: TEdit;
    procedure btnOpenDatabaseClick(Sender: TObject);
    procedure cmbDatabaseTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure cmbDatabaseChange(Sender: TObject);
    procedure chkIntegratedSecurityClick(Sender: TObject);
    procedure cmbDatabaseEnter(Sender: TObject);
  private
    conStr: string;
    udlFilename: string;
    function GetConDatabaseType: TDatabaseType;

    { Private declarations }
  public
    sqlConStr: string;
    OracleDatabaseDescriptor: string;
    property ConDatabaseType: TDatabaseType read GetConDatabaseType;
    function GetFriendlyConnStr: string;
  end;

type
  TSqlConAttributes = packed record
    DataSource: string;
    InitialCatalog: string;
    IntegratedSecurity: boolean;
    Username: string;
    Password: string;
    ApplicationName: string;
  end;

var
  ConnectinoBuilderForm: TConnectinoBuilderForm;

implementation

uses SqlDmo;

{$R *.lfm}

function GetSqlConAttribs(DataLinkFilename: string): TSqlConAttributes;
var
  s: TSqlConAttributes;
  I, J, K: integer;
  _NewStr, Current, ProcessingString: string;
  strLoader, dlmStr: TStringList;
begin

  strLoader := TStringList.Create;
  dlmStr := TStringList.Create;
  _NewStr := '';
  try
    strLoader.Delimiter := ';';
    strLoader.LoadFromFile(DataLinkFilename);
    strLoader.Delete(0);    //[OleDb]
    strLoader.Delete(0); //;everything after this line bla bla ...,
    dlmStr.Delimiter := ';';
    dlmStr.StrictDelimiter := True;
    dlmStr.DelimitedText := strLoader.Text;

    strLoader.Clear;
    strLoader.Add('Data Source=');
    strLoader.Add('User ID=');
    strLoader.Add('Initial Catalog=');
    strLoader.Add('Password=');
    strLoader.Add('Integrated Security=');

    for I := 0 to dlmStr.Count - 1 do
    begin
      Current := dlmStr[I];

      ProcessingString := '';
      for J := 0 to strLoader.Count - 1 do
      begin
        if Pos(strLoader[J], Current) > 0 then
        begin
          ProcessingString := strLoader[J];
          break;
        end;
      end;

      _NewStr := '';
      if ProcessingString <> '' then
      begin
        for J := Length(ProcessingString) + 1 to Length(Current) do
        begin
          _NewStr := _NewStr + Current[J];
        end;

        if ProcessingString = 'Data Source=' then
          s.DataSource := _NewStr;
        if ProcessingString = 'User ID=' then
          s.Username := _NewStr;
        if ProcessingString = 'Initial Catalog=' then
          s.InitialCatalog := _NewStr;
        if ProcessingString = 'Password=' then
          s.Password := _NewStr;
        if (ProcessingString = 'Integrated Security=') and (_NewStr = 'SSPI') then
        begin
          s.IntegratedSecurity := True;
        end;
      end;

    end;

  finally
    strLoader.Free;
    dlmStr.Free;
  end;

  s.DataSource := (StringReplace(Trim(s.DataSource), '\', '\\', [rfReplaceAll]));
  s.InitialCatalog := StringReplace(Trim(s.InitialCatalog), '\', '\\', [rfReplaceAll]);
  s.Username := StringReplace(Trim(s.Username), '\', '\\', [rfReplaceAll]);
  s.Password := StringReplace(Trim(s.Password), '\', '\\', [rfReplaceAll]);
  s.ApplicationName := StringReplace(Trim(s.Password), '\', '\\', [rfReplaceAll]);
  Result := s;
end;

procedure TConnectinoBuilderForm.btnAcceptClick(Sender: TObject);
var
  DbConnection: TZConnection;
  i: integer;
begin

  if txtPort.Visible then
    if not TryStrToInt(txtPort.Text, i) then
    begin
      ShowMessage('Port must be number');
    end;


  DbConnection := TZConnection.Create(nil);
  try

    try
      DbConnection.Protocol := LowerCase(cmbDatabaseType.Text);

      if LowerCase(cmbDatabaseType.Text) <> 'sqlite' then
      begin
        DbConnection.HostName := cmbServerName.Text;
        DbConnection.User := txtUserName.Text;
        DbConnection.Password := txtPassword.Text;
        DbConnection.Catalog := cmbDatabase.Text;
        DBConnection.LoginPrompt := False;
        DbConnection.Port := i;
      end;
      if LowerCase(cmbDatabaseType.Text) = 'oracle' then
      begin
        OracleDatabaseDescriptor :=
          '(DESCRIPTION = (ADDRESS_LIST = (ADDRESS = (PROTOCOL = TCP)(HOST = ' +
          cmbServerName.Text + ')(PORT = ' + IntToStr(I) +
          '))) (CONNECT_DATA = (SERVER = DEDICATED)(SERVICE_NAME = ' + cmbDatabase.Text + ')))';
        DbConnection.Database := OracleDatabaseDescriptor;
      end
      else
      begin
        DbConnection.Database := cmbDatabase.Text;
      end;
      DBConnection.Connect;
      txtConnStr.Text := DbConnection.DbcConnection.GetParameters.Text;
      DbConnection.Disconnect;
    except
      on e: Exception do
      begin
        Self.ModalResult := mrCancel;
        ShowMessage('Invalid ConnectionString' + LineEnding + e.Message);
      end
    end;

  finally
    DbConnection.Free;
  end;
end;

procedure TConnectinoBuilderForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TConnectinoBuilderForm.chkIntegratedSecurityClick(Sender: TObject);
begin

  if chkIntegratedSecurity.Checked then
  begin
    txtUserName.Enabled := False;
    txtPassword.Enabled := False;
  end
  else
  begin
    txtUserName.Enabled := True;
    txtPassword.Enabled := True;
  end;

end;

procedure TConnectinoBuilderForm.cmbDatabaseChange(Sender: TObject);
begin

  if cmbDatabase.Text = '' then
    exit;


  if chkIntegratedSecurity.Checked = False then
    conStr := 'Provider=SQLOLEDB.1;Password=' + txtPassword.Text +
      ';Persist Security Info=True;User ID=' + txtUserName.Text +
      ';Initial Catalog=' + cmbDatabase.Text + ';Data Source=' + cmbServerName.Text
  else
    conStr := 'Provider=SQLOLEDB.1;Integrated Security = sspi;Initial Catalog='
      + cmbDatabase.Text + ';Data Source=' + cmbServerName.Text;

  txtConnStr.Text := conStr;

  sqlConStr := 'Integrated Security = sspi;Initial Catalog=' +
    cmbDatabase.Text + ';Data Source=' + cmbServerName.Text;
end;

procedure TConnectinoBuilderForm.cmbDatabaseEnter(Sender: TObject);
begin

  if ConDatabaseType <> dtMsSql then
    exit;

  try
    DataBasesOnServer(cmbDatabase.Items, cmbServerName.Text, txtUserName.Text,
      txtPassword.Text);
  except
    on e: Exception do
      ShowMessage(e.Message);
  end;

end;

function TConnectinoBuilderForm.GetConDatabaseType: TDatabaseType;
begin
  Result := TDatabaseType(cmbDatabaseType.ItemIndex);
end;

procedure TConnectinoBuilderForm.FormShow(Sender: TObject);
begin
  //cmbServerName.Clear;
  ListSQLServers(cmbServerName.Items);
  TControlUtils.EnableControls(pnlMain, True);
  udlFilename := '';
  // lblOpenSQLite.Caption:='Open Data Link File (UDL)';
end;

procedure TConnectinoBuilderForm.cmbDatabaseTypeChange(Sender: TObject);
begin
  cmbDatabase.Width := 241;
  lblUseraname.Visible := True;
  txtUserName.Visible := True;
  lblPassword.Visible := True;
  txtPassword.Visible := True;
  lblPort.Visible := True;
  txtPort.Visible := True;
  lblSever.Visible := True;
  cmbServerName.Visible := True;
  case cmbDatabaseType.ItemIndex of
    0:
    begin
      txtPort.Text := '0';
      txtUserName.Text := 'sa';
      txtPassword.Text := '';
    end;
    1:
    begin
      txtPort.Text := '1521';
      txtUserName.Text := 'sa';
      txtPassword.Text := '';
    end;
    2:
    begin
      txtPort.Text := '3306';
      txtUserName.Text := 'root';
      txtPassword.Text := '';
    end;
    3:
    begin
      cmbDatabase.Width := 217;
      lblUseraname.Visible := False;
      txtUserName.Visible := False;
      lblPassword.Visible := False;
      txtPassword.Visible := False;
      lblPort.Visible := False;
      txtPort.Visible := False;
      lblSever.Visible := False;
      cmbServerName.Visible := False;
    end;
  end;
end;

procedure TConnectinoBuilderForm.btnOpenDatabaseClick(Sender: TObject);
var
  SqlConAttrib: TSqlConAttributes;
begin
  with TOpenDialog.Create(self) do
  begin
    Filter := 'Any File (*.*)|*.*';
    if Execute then
    begin
      cmbDatabase.Text := FileName;
    end;
    Free;
  end;
end;

function TConnectinoBuilderForm.GetFriendlyConnStr: string;
var
  server: string;
begin
  case ConDatabaseType of
    dtMsSql:
    begin
      if Trim(cmbServerName.Text) = '' then
        server := '.';
      if not chkIntegratedSecurity.Checked then
        Result := 'Data Source=' + cmbServerName.Text + ';user id=' + txtUserName.Text + ';' +
          'password=' + txtPassword.Text + ';Initial Catalog=' + cmbDatabase.Text
      else
        Result := 'Data Source=' + server + ';Integrated Security = sspi;Initial Catalog=' +
          cmbDatabase.Text;
    end;
    dtOracle:
    begin
      Result := 'SERVER=' + OracleDatabaseDescriptor + ';uid=' + txtUserName.Text +
        ';pwd=' + txtPassword.Text + ';';
    end;
    dtMySql: Result := 'Server=' + server + ';Database=' + cmbDatabase.Text +
        ';Uid=' + txtUserName.Text + ';Pwd=' + txtPassword.Text + ';';
    dtSQLite: Result := 'Data Source=' + AnsiReplaceStr(cmbDatabase.Text, '\', '\\') +
        ';Pooling=true;FailIfMissing=true';
  end;

end;

end.

