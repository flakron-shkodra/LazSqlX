unit OptionsFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, LazSqlXSettings;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnQueryEditorFont: TButton;
    divFont: TDividerBevel;
    txtFontName: TEdit;
    FontDialog1: TFontDialog;
    procedure btnOKClick(Sender: TObject);
    procedure btnQueryEditorFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

{ TOptionsForm }

procedure TOptionsForm.btnQueryEditorFontClick(Sender: TObject);
begin
  with Settings.QueryEditorFont do
  begin
    FontDialog1.Font.Name  := Name;
    FontDialog1.Font.Size  := Size;
    FontDialog1.Font.Color := Color;
    if FontDialog1.Execute then
    begin
      Name := FontDialog1.Font.Name;
      Size := FontDialog1.Font.Size;
      Color := FontDialog1.Font.Color;
    end;
  end;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  txtFontName.Text := Settings.QueryEditorFont.Name + ',' + IntToStr(
    Settings.QueryEditorFont.Size);
  txtFontName.Font.Name := Settings.QueryEditorFont.Name;
  txtFontName.Font.Color:=Settings.QueryEditorFont.Color;
end;

procedure TOptionsForm.btnOKClick(Sender: TObject);
begin
  Settings.Save;
end;

end.
