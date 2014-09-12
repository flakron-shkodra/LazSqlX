unit RunProcedureFormU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls;

type

  { TRunProcedureForm }

  TRunProcedureForm = class(TForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    pnlButtons: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  RunProcedureForm: TRunProcedureForm;

implementation

{$R *.lfm}

end.

