unit PropertyGridFormU;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
 ExtCtrls, Buttons;

type

 { TPropertyGridForm }

 TPropertyGridForm = class(TForm)
  btnOK: TBitBtn;
  btnCancel: TBitBtn;
  pnlBottom: TPanel;
  TIPropertyGrid1: TTIPropertyGrid;
 private
  { private declarations }
 public
  { public declarations }
 end;

var
 PropertyGridForm: TPropertyGridForm;

implementation

{$R *.lfm}

end.

