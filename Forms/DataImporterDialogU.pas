{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit DataImporterDialogU;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
 Buttons, ExtCtrls;

type

 { TDataImporterDialog }

 TDataImporterDialog = class(TForm)
  Bevel1: TBevel;
  btnOK: TBitBtn;
  btnCancel: TBitBtn;
  cmbTablename: TComboBox;
  lblTablename: TLabel;
 private
  { private declarations }
 public
  { public declarations }
 end;

var
 DataImporterDialog: TDataImporterDialog;

implementation

{$R *.lfm}

end.

