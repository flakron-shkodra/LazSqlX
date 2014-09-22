{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}

unit AboutFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btnOK: TButton;
    Image1: TImage;
    lblVersion: TLabel;
    lblOwner: TLabel;
    lblProduct: TLabel;
    lblDesc: TLabel;
    txtDescription: TMemo;
    pnlMain: TPanel;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses MainFormU;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormShow(Sender: TObject);
begin
  lblVersion.Caption:= AppVersion;
end;

procedure TAboutForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

end.

