{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}
unit EditMemoFormU;
{$mode objfpc}{$H+}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TEditMemoForm = class(TForm)
    memEdit: TMemo;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditMemoForm: TEditMemoForm;

implementation

{$R *.lfm}

end.
