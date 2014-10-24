{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  mod 2013
  *******************************************************************
}

unit SqlConnBuilderFormU;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Utils, ZConnection, TableInfo, DbType, IBConnection,
  strutils, types, LCLType, Spin, EditBtn, Menus, IniFiles;

type

  { TSqlConnBuilderForm }

  TSqlConnBuilderForm = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    chkAlternateLibLocation: TCheckBox;
    cmbDatabaseType: TComboBox;
    cmbDbEngine: TComboBox;
    imgDbEngines: TImageList;
    Label1: TLabel;
    lblDBEngine: TLabel;
    lblExpander: TLabel;
    mitDelete: TMenuItem;
    popRecent: TPopupMenu;
    txtAdvancedProperties: TMemo;
    txtLibraryFilename: TFileNameEdit;
    grpAdvanced: TGroupBox;
    imgDatabaseTypes: TImageList;
    lblClearRecent: TLabel;
    lblRecentConnections: TLabel;
    lblPort: TLabel;
    btnOpenDatabase: TSpeedButton;
    lstRecentConnections: TListBox;
    txtPort: TSpinEdit;
    pnlMain: TPanel;
    Bevel1: TBevel;
    chkIntegratedSecurity: TCheckBox;
    lblUseraname: TLabel;
    txtUserName: TEdit;
    lblPassword: TLabel;
    txtPassword: TEdit;
    lblSever: TLabel;
    cmbServerName: TComboBox;
    lblDatabase: TLabel;
    cmbDatabase: TComboBox;
    procedure btnOpenDatabaseClick(Sender: TObject);
    procedure chkAlternateLibLocationChange(Sender: TObject);
    procedure cmbDatabaseTypeChange(Sender: TObject);
    procedure cmbDatabaseTypeDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cmbDbEngineDrawItem(Control: TWinControl; Index: Integer;
     ARect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure chkIntegratedSecurityClick(Sender: TObject);
    procedure cmbDatabaseEnter(Sender: TObject);
    procedure lblExpanderClick(Sender: TObject);
    procedure lblClearRecentClick(Sender: TObject);
    procedure lstRecentConnectionsClick(Sender: TObject);
    procedure lstRecentConnectionsDblClick(Sender: TObject);
    procedure lstRecentConnectionsDrawItem(Control: TWinControl;
     Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure mitDeleteClick(Sender: TObject);
    procedure txtPasswordChange(Sender: TObject);
  private
    conStr: string;
    FDBInfo: TDbConnectionInfo;
    udlFilename: string;

    bmpSqlType: TBitmap;
    bmpOracleType: TBitmap;
    bmpMySqlType: TBitmap;
    bmpSqliteType: TBitmap;
    bmpFirebird : TBitmap;
    bmpSqlDBEngine: TBitmap;
    bmpZeosEngine : TBitmap;

    procedure FillRecentConnectionListBox;
    function GetConDatabaseType: TDatabaseType;
    procedure PerformConnection;
    { Private declarations }
  public
    sqlConStr: string;
    OracleDatabaseDescriptor: string;
    RecentConnectionsFilename:string;
    RecentConnections:TDbConnectionInfos;
    property ConDatabaseType: TDatabaseType read GetConDatabaseType;
    function GetFriendlyConnStr: string;
    procedure SaveRecentConnToFile;
    procedure LoadRecentConnFromFile;
    property DbInfo:TDbConnectionInfo read FDBInfo;
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
  SqlConnBuilderForm: TSqlConnBuilderForm;
  Is64Bit:Boolean;

implementation

uses AsStringUtils;



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

procedure TSqlConnBuilderForm.btnAcceptClick(Sender: TObject);
begin
  PerformConnection;
end;

procedure TSqlConnBuilderForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TSqlConnBuilderForm.chkIntegratedSecurityClick(Sender: TObject);
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

procedure TSqlConnBuilderForm.cmbDatabaseEnter(Sender: TObject);
var
   DbConnection:TZConnection;
begin
 try
    try
        DbConnection := TZConnection.Create(nil);
        DbConnection.HostName := cmbServerName.Text;
        DbConnection.User := txtUserName.Text;
        DbConnection.Password := txtPassword.Text;
        DbConnection.Catalog := cmbDatabase.Text;
        DBConnection.LoginPrompt := False;
        DbConnection.Port := txtPort.Value;
        DbConnection.Protocol:= TDbUtils.DatabaseTypeAsString(TDatabaseType(cmbDatabaseType.ItemIndex),True);


       case ConDatabaseType of
         dtMsSql:
         begin
              //DataBasesOnServer(cmbDatabase.Items, cmbServerName.Text, txtUserName.Text,
              //txtPassword.Text);
            DbConnection.Connect;
            DbConnection.GetCatalogNames(cmbDatabase.Items);
         end;

         dtMySql:
         begin
           DbConnection.Connect;
           DbConnection.GetCatalogNames(cmbDatabase.Items);
         end;

       end
    except on e:Exception do
      begin
        ShowMessage(e.Message);
      end;

    end;
  finally
    DbConnection.Free;
  end;

end;

procedure TSqlConnBuilderForm.lblExpanderClick(Sender: TObject);
begin
 if Height = 358 then
 begin
  Height:=595;
  lblExpander.Caption:='Less';
 end
 else
 begin
   Height:=358;
   lblExpander.Caption:='More';
 end;
end;

procedure TSqlConnBuilderForm.lblClearRecentClick(Sender: TObject);
begin
 RecentConnections.Clear;
 RecentConnections.SaveToFile(RecentConnectionsFilename);
 RecentConnections.LoadFromFile(RecentConnectionsFilename);
 FillRecentConnectionListBox;
end;

procedure TSqlConnBuilderForm.lstRecentConnectionsClick(Sender: TObject);
var
  c: TDbConnectionInfo;
begin
  if lstRecentConnections.ItemIndex>-1 then
  begin
    try
      c:=RecentConnections[lstRecentConnections.ItemIndex];
      cmbDatabaseType.ItemIndex:=Integer(c.DatabaseType);
      cmbDatabaseTypeChange(nil);
      cmbServerName.Text:=c.Server;
      cmbDatabase.Text:=c.Database;
      txtUserName.Text:=c.Username;
      txtPassword.Text:=c.Password;
      txtPort.Value:=c.Port;
    except
      // ignore errors
    end;
  end;
end;

procedure TSqlConnBuilderForm.lstRecentConnectionsDblClick(Sender: TObject);
begin
  PerformConnection;
end;

procedure TSqlConnBuilderForm.lstRecentConnectionsDrawItem(
 Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  c: TListBox;
  strDbType: string;
  aDbtype: TDatabaseType;
begin
  c := (Control as TListBox);

  with c.Canvas do
  begin
    if Odd(Index) then
      Brush.Color := clWhite
    else
      Brush.Color := $00F9F9F9;

    FillRect(ARect);

    if (odSelected in State) then
    begin
      FrameRect(ARect);
      GradientFill(ARect, clSilver, clGray, gdVertical);
      //Font.Color := $0000CCFF;
      Font.Color := clWhite;
    end
    else if (odFocused in State) then
    begin
      FrameRect(ARect);
      GradientFill(ARect, clSilver, clGray, gdVertical);
      Font.Color := clWhite;
    end
    else
    begin
      Font.Color := clBlack;
      Brush.Color := clWhite;
      c.Hint := '';
    end;

    strDbType := TAsStringUtils.SplitString(c.Items[Index],':')[0];

    aDbType := TDbUtils.DatabaseTypeFromString(strDbType);

    case aDbtype of
      dtMsSql: Draw(ARect.Left, ARect.Top,bmpSqlType);
      dtOracle: Draw(ARect.Left, ARect.Top,bmpOracleType);
      dtMySql: Draw(ARect.Left, ARect.Top,bmpMySqlType);
      dtSQLite: Draw(ARect.Left, ARect.Top,bmpSqliteType);
      dtFirebirdd: Draw(ARect.Left, ARect.Top,bmpFirebird);
    end;

    Brush.Style := bsClear;
    TextOut(ARect.Left + 35, ARect.Top+5, c.Items[Index]);
  end;
end;

procedure TSqlConnBuilderForm.mitDeleteClick(Sender: TObject);
begin
  if lstRecentConnections.ItemIndex>-1 then
  begin
    RecentConnections.Delete(lstRecentConnections.ItemIndex);
    RecentConnections.SaveToFile(RecentConnectionsFilename);
    RecentConnections.LoadFromFile(RecentConnectionsFilename);
    FillRecentConnectionListBox;
  end;
end;

procedure TSqlConnBuilderForm.txtPasswordChange(Sender: TObject);
begin
  if GetConDatabaseType=dtSQLite then
  begin
    txtAdvancedProperties.Clear;

    if Trim(txtPassword.Text)<>EmptyStr then
      txtAdvancedProperties.Lines.Add('encrypted=true');
  end;
end;

function TSqlConnBuilderForm.GetConDatabaseType: TDatabaseType;
begin
  Result := TDatabaseType(cmbDatabaseType.ItemIndex);
end;

procedure TSqlConnBuilderForm.PerformConnection;
var
  DbConnection: TZConnection;
  i: integer;
  wt:TStringWrapType;
  s: TCaption;
begin
  if (txtPort.Visible) and
    (not TryStrToInt(txtPort.Text, i)) then
  begin
    ShowMessage('Port must be a number');
  end;

  DbInfo.DatabaseType:=TDatabaseType(cmbDatabaseType.ItemIndex);
  DbInfo.Server:=cmbServerName.Text;
  DbInfo.Database:=cmbDatabase.Text;
  DbInfo.Username:=txtUserName.Text;
  DbInfo.Password:=txtPassword.Text;
  DbInfo.Port:=txtPort.Value;
  DbInfo.DbEngine:=TDatabaseEngine(cmbDbEngine.ItemIndex);

  DbConnection := TZConnection.Create(nil);
  try
    try
      DbConnection.Protocol := LowerCase(cmbDatabaseType.Text);

      if LowerCase(cmbDatabaseType.Text) <> 'sqlite' then
      begin
        DbConnection.HostName := cmbServerName.Text;
        DbConnection.User := txtUserName.Text;
        DbConnection.Password := txtPassword.Text;

        case DbInfo.DatabaseType of
          dtMsSql:wt:=swtBrackets;
          dtOracle:wt:=swtQuotes;
          else
          wt := swtNone;
        end;

        DbConnection.Catalog := TAsStringUtils.WrapString(cmbDatabase.Text,wt);

        DBConnection.LoginPrompt := False;
        DbConnection.Port := i;
      end;
      if (LowerCase(cmbDatabaseType.Text) = 'oracle') or
        (LowerCase(cmbDatabaseType.Text) = 'oracle-9i') then
      begin
        OracleDatabaseDescriptor := TDbUtils.GetOracleDescriptor(DbInfo);
        DbConnection.Database := OracleDatabaseDescriptor;
      end
      else
      begin
        if (LowerCase(cmbDatabaseType.Items[cmbDatabaseType.ItemIndex])='mssql') then
          DbConnection.Database := TAsStringUtils.WrapString(cmbDatabase.Text,TStringWrapType.swtBrackets)
        else
          DbConnection.Database :=cmbDatabase.Text;
      end;
      DBConnection.Connect;
      DbConnection.Disconnect;

      if not RecentConnections.Exists(dbInfo) then
        RecentConnections.Add(dbInfo);

      if RecentConnections.Count>9 then
        RecentConnections.Delete(0);

      SaveRecentConnToFile;
      // Close form and pass on success:
      Self.ModalResult := mrOK;
    except
      on e: Exception do
      begin
        // Close form and pass on failure
        Self.ModalResult := mrCancel;
        ShowMessage('Invalid ConnectionString' + LineEnding + e.Message);
      end
    end;
  finally
    DbConnection.Free;
  end;
end;

procedure TSqlConnBuilderForm.FillRecentConnectionListBox;
var
  I: Integer;
begin
  lstRecentConnections.Clear;
  for I:=0 to RecentConnections.Count-1 do
  begin
    lstRecentConnections.Items.Add
      (
      TDbUtils.DatabaseTypeAsString(RecentConnections[I].DatabaseType, true)+':'+
      RecentConnections[I].Server+'\'+
      RecentConnections[I].Database
      );
  end;
end;

procedure TSqlConnBuilderForm.FormShow(Sender: TObject);
begin
  TControlUtils.EnableControls(pnlMain, True);
  udlFilename := '';
  LoadRecentConnFromFile;
  FillRecentConnectionListBox;
  Height:=358;
  lblExpander.Caption:='More';
  chkAlternateLibLocation.Checked:=False;
  chkAlternateLibLocationChange(chkAlternateLibLocation);
end;

procedure TSqlConnBuilderForm.cmbDatabaseTypeChange(Sender: TObject);
begin
 cmbDatabase.Clear;
  cmbDatabase.Width := 241;
  lblUseraname.Visible := True;
  txtUserName.Visible := True;
  lblPassword.Visible := True;
  txtPassword.Visible := True;
  lblPort.Visible := True;
  txtPort.Visible := True;
  lblSever.Visible := True;
  cmbServerName.Visible := True;
  cmbDbEngine.Enabled:=True;
  cmbDbEngine.ItemIndex:=0;

  case cmbDatabaseType.ItemIndex of
    0: //MS SQL Server
    begin
      txtPort.Text := '0';
      txtUserName.Text := 'sa';
      txtPassword.Text := '';
    end;
    1: //Oracle
    begin
      txtPort.Text := '1521';
      txtUserName.Text := 'sa';
      txtPassword.Text := '';
      if Is64Bit then
      begin
        cmbDbEngine.ItemIndex:=1;
        cmbDbEngine.Enabled:=False;
      end;
    end;
    2: // MySQL
    begin
      txtPort.Text := '3306';
      txtUserName.Text := 'root';
      txtPassword.Text := '';
      cmbDbEngine.ItemIndex:=1;
      cmbDbEngine.Enabled:=False;
    end;
    3: // SQLite
    begin
      cmbDatabase.Width := 217;
      lblUseraname.Visible := False;
      txtUserName.Visible := False;
      lblPassword.Visible := true;
      txtPassword.Visible := true;
      lblPort.Visible := False;
      txtPort.Visible := False;
      lblSever.Visible := False;
      cmbServerName.Visible := False;
    end;
    4: // Firebird
    begin
      cmbDatabase.Width := 217;
      lblUseraname.Visible := True;
      txtUserName.Visible := True;
      lblPassword.Visible := true;
      txtPassword.Visible := true;
      lblPort.Visible := True;
      txtPort.Visible := True;
      lblSever.Visible := True;
      cmbServerName.Visible := True;
      txtPort.Text:='3050';
      txtUserName.Text := 'SYSDBA';
      txtPassword.Text := '';
    end;
  end;
end;

procedure TSqlConnBuilderForm.cmbDatabaseTypeDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  cmb: TComboBox;
begin
  cmb := Control as TComboBox;

  if odSelected in State then
  begin
    cmb.Canvas.GradientFill(ARect, $00E2E2E2, clGray, gdVertical);
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
    4 :cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpFirebird);
  end;

  cmb.Canvas.Font.Size := 10;
  cmb.Canvas.Brush.Style := bsClear;
  cmb.Canvas.TextOut(ARect.Left + 30, ARect.Top + 3, cmb.Items[Index]);
end;

procedure TSqlConnBuilderForm.cmbDbEngineDrawItem(Control: TWinControl;
 Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  cmb: TComboBox;
begin
  cmb := Control as TComboBox;

  if odSelected in State then
  begin
    cmb.Canvas.GradientFill(ARect, $00E2E2E2, clGray, gdVertical);
  end
  else
  begin
    cmb.Canvas.Brush.Color := clWindow;
    cmb.Canvas.FillRect(ARect);
  end;


  case Index of
    0: cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpSqlDBEngine);
    1: cmb.Canvas.Draw(ARect.Left, ARect.Top, bmpZeosEngine);
  end;

  cmb.Canvas.Font.Size := 10;
  cmb.Canvas.Brush.Style := bsClear;
  cmb.Canvas.TextOut(ARect.Left + 30, ARect.Top + 3, cmb.Items[Index]);
end;

procedure TSqlConnBuilderForm.FormCreate(Sender: TObject);
begin

  {$ifdef win64}
    Is64Bit:=True;
  {$else}
    Is64Bit:=False;
  {$endif}

  bmpSqlType := TBitmap.Create;
  bmpOracleType := TBitmap.Create;
  bmpMySqlType := TBitmap.Create;
  bmpSqliteType := TBitmap.Create;
  bmpFirebird := TBitmap.Create;

  bmpSqlDBEngine:=TBitmap.Create;
  bmpZeosEngine := TBitmap.Create;

  imgDatabaseTypes.GetBitmap(0, bmpSqlType);
  imgDatabaseTypes.GetBitmap(1, bmpOracleType);
  imgDatabaseTypes.GetBitmap(2, bmpMySqlType);
  imgDatabaseTypes.GetBitmap(3, bmpSqliteType);
  imgDatabaseTypes.GetBitmap(4, bmpFirebird);

  imgDbEngines.GetBitmap(0,bmpSqlDBEngine);
  imgDbEngines.GetBitmap(1,bmpZeosEngine);

  FDBInfo := TDbConnectionInfo.Create;

  RecentConnections:=TDbConnectionInfos.Create;
  RecentConnectionsFilename := GetTempDir+ ExtractFileName(ChangeFileExt(Application.ExeName,'.ini'))
end;

procedure TSqlConnBuilderForm.FormDestroy(Sender: TObject);
begin
  bmpSqlDBEngine.Free;
  bmpZeosEngine.Free;
  bmpMySqlType.Free;
  bmpSqliteType.Free;
  bmpOracleType.Free;
  bmpSqlType.Free;
  bmpFirebird.Free;
  RecentConnections.Free;
  FDBInfo.Free;
end;

procedure TSqlConnBuilderForm.btnOpenDatabaseClick(Sender: TObject);
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

procedure TSqlConnBuilderForm.chkAlternateLibLocationChange(Sender: TObject);
begin
 txtLibraryFilename.Enabled:=chkAlternateLibLocation.Checked;
end;

function TSqlConnBuilderForm.GetFriendlyConnStr: string;
begin
end;

procedure TSqlConnBuilderForm.SaveRecentConnToFile;
begin
 RecentConnections.SaveToFile(RecentConnectionsFilename);
end;

procedure TSqlConnBuilderForm.LoadRecentConnFromFile;
begin
  RecentConnections.LoadFromFile(RecentConnectionsFilename);
end;

end.

