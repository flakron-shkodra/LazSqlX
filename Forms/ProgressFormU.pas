{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}

unit ProgressFormU;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
 StdCtrls;

type

 { TProgressForm }

 TProgressForm = class(TForm)
  lblProgress: TLabel;
  pbProgressBar: TProgressBar;
  procedure FormShow(Sender: TObject);
 private
  function GetCurrentPosition: Integer;
  function GetMaxProgress: Integer;
  function GetMessageInfo: string;
  procedure SetCurrentPosition(AValue: Integer);
  procedure SetMaxProgress(AValue: Integer);
  procedure SetMessageInfo(AValue: string);
  { private declarations }
 public
  { public declarations }
  property MaxProgress:Integer read GetMaxProgress write SetMaxProgress;
  property Message:string read  GetMessageInfo write SetMessageInfo;
  procedure StepProgress;
  procedure Reset;
  property CurrentProgress:Integer read GetCurrentPosition write SetCurrentPosition;
 end;


var
 ProgressForm: TProgressForm;

implementation

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.FormShow(Sender: TObject);
begin
 pbProgressBar.Position:=0;
end;

function TProgressForm.GetCurrentPosition: Integer;
begin
 Result := pbProgressBar.Position;
end;

function TProgressForm.GetMaxProgress: Integer;
begin
 Result:=pbProgressBar.Max;
end;

function TProgressForm.GetMessageInfo: string;
begin
 Result := lblProgress.Caption;
end;

procedure TProgressForm.SetCurrentPosition(AValue: Integer);
begin
  pbProgressBar.Position:=AValue;
end;

procedure TProgressForm.SetMaxProgress(AValue: Integer);
begin
 pbProgressBar.Max:=AValue;
end;

procedure TProgressForm.SetMessageInfo(AValue: string);
begin
 lblProgress.Caption:=AValue;
end;

procedure TProgressForm.StepProgress;
begin
 pbProgressBar.StepIt;
end;

procedure TProgressForm.Reset;
begin
  pbProgressBar.Position:=0;
end;

end.

