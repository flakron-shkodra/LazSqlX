{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}

unit BlobFieldFormU;
{$mode objfpc}{$H+}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type

  { TBlobFieldForm }

  TBlobFieldForm = class(TForm)
    btnLoadFromfile: TBitBtn;
    btnSave: TBitBtn;
    imgPreview: TImage;
    Label1: TLabel;
    pnlPreview: TPanel;
    lblFileType: TLabel;
    procedure btnLoadFromfileClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure pnlPreviewClick(Sender: TObject);
  private
    FHasImagePreview: Boolean;
    procedure SetHasImagePreview(AValue: Boolean);
    { Private declarations }
  public
    { Public declarations }
    property HasImagePreview:Boolean read FHasImagePreview write SetHasImagePreview;
  end;

var
  BlobFieldForm: TBlobFieldForm;

implementation

{$R *.lfm}

{ TBlobFieldForm }

procedure TBlobFieldForm.btnSaveClick(Sender: TObject);
begin

end;

procedure TBlobFieldForm.pnlPreviewClick(Sender: TObject);
begin

end;

procedure TBlobFieldForm.SetHasImagePreview(AValue: Boolean);
begin
  if FHasImagePreview=AValue then Exit;
  FHasImagePreview:=AValue;

  pnlPreview.Visible := FHasImagePreview;

  if FHasImagePreview then
    Height:=272
  else
    Height:=94;
end;

procedure TBlobFieldForm.btnLoadFromfileClick(Sender: TObject);
begin

end;

end.
