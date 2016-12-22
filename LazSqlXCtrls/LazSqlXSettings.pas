unit LazSqlXSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Graphics, Forms;

const
  FONT_SECTION = 'Font';
  FONT_NAME_IDENT = 'FontName';
  FONT_SIZE_IDENT = 'FontSize';
  FONT_COLOR_IDENT = 'FontColor';
  FONT_STYLE_IDENT = 'FontStyle';

type

  { TLazSqlXSettings }

  TLazSqlXSettings = class
  private
    FQueryEditorFont: TFont;
    function GetFilename: string;
    function GetQueryEditorFont: TFont;
    procedure SetQueryEditorFont(AValue: TFont);
  public
    constructor Create;
    destructor Destroy;override;
    procedure Load;
    procedure Save;
    property Filename: string read GetFilename;

    property QueryEditorFont: TFont read GetQueryEditorFont write SetQueryEditorFont;
  end;


var
  Settings:TLazSqlXSettings;

implementation

{ TLazSqlXSettings }

function TLazSqlXSettings.GetQueryEditorFont: TFont;
begin
  Result := FQueryEditorFont;
end;

function TLazSqlXSettings.GetFilename: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

procedure TLazSqlXSettings.SetQueryEditorFont(AValue: TFont);
begin
  FQueryEditorFont := AValue;
end;

constructor TLazSqlXSettings.Create;
begin
  FQueryEditorFont := TFont.Create;
end;

destructor TLazSqlXSettings.Destroy;
begin
  FQueryEditorFont.Free;
end;

procedure TLazSqlXSettings.Load;
begin
  with TIniFile.Create(Filename) do
  begin
    try
      with FQueryEditorFont do
      begin
        Name := ReadString(FONT_SECTION, FONT_NAME_IDENT, 'Lucida Sans Typewriter');
        Size := ReadInteger(FONT_SECTION, FONT_SIZE_IDENT, 9);
        Color := ReadInteger(FONT_SECTION, FONT_COLOR_IDENT, clWindowText);
      end;

    finally
      Free;
    end;
  end;
end;

procedure TLazSqlXSettings.Save;
begin
   with TIniFile.Create(Filename) do
  begin
    try
      with FQueryEditorFont do
      begin
        WriteString(FONT_SECTION, FONT_NAME_IDENT, FQueryEditorFont.Name);
        WriteInteger(FONT_SECTION, FONT_SIZE_IDENT, FQueryEditorFont.Size);
        WriteInteger(FONT_SECTION, FONT_COLOR_IDENT, FQueryEditorFont.Color);
      end;
    finally
      Free;
    end;
  end;
end;

initialization
   Settings := TLazSqlXSettings.Create;
   Settings.Load;
finalization
  Settings.Free;

end.


