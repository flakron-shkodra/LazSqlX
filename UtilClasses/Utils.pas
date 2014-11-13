{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2011
  *******************************************************************
}
unit Utils;

{$mode objfpc}{$H+}
{$Static ON}

interface

uses SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Dialogs, StrUtils, DB, ZDataset;

type



  { TControlUtils }

  TControlUtils = class
  public
    class procedure EnableControls(control: TWinControl; Enable: boolean);
    class function FindItem(const List: TStrings; SearchValue: string;StartIndex:Integer=0): integer;
  end;

  { TFileUtils }

  TFileUtils = class
  public
    class procedure EraseDir(Directory: string);
    class function GetApplicationVersion: string;
  end;



procedure SetStringFieldSizes(DS: TDataSet);


implementation

uses VersionSupport;

{ TControlUtils }

procedure SetStringFieldSizes(DS: TDataSet);
var
  I: integer;
  SF: TStringField;
begin
  for I := 0 to DS.FieldCount - 1 do
    if DS.Fields.Fields[I] is TStringField then
    begin
      SF := DS.Fields.Fields[I] as TStringField;
      SF.Size := 2 * SF.DisplayWidth;
{
     this should work for all latin, cirilic or greek letters,
     but if you expect non-european characters, then
     in the previous line, instead of 2 *, you should
     put 3 * or maybe 4 *
}
    end;

end;

{ TAsRegExUtils }


class procedure TControlUtils.EnableControls(control: TWinControl; Enable: boolean);
var
  I: integer;
begin

  for I := 0 to Control.ControlCount - 1 do
  begin

    //Recursive
    if (Control.Controls[i] is TPanel) then
    begin
      EnableControls((Control.Controls[i] as TPanel), Enable);
    end;

    if (Control.Controls[i] is TPanel) then
    begin
      EnableControls((Control.Controls[i] as TPanel), Enable);
    end;

    if (Control.Controls[i] is TPageControl) then
    begin
      EnableControls((Control.Controls[i] as TPanel), Enable);
    end;


    if (Control.Controls[i] is TTabSheet) then
    begin
      EnableControls((Control.Controls[i] as TTabSheet), Enable);
    end;

    if (Control.Controls[i] is TGroupBox) then
    begin
      EnableControls((Control.Controls[i] as TGroupBox), Enable);
    end;

    control.Controls[I].Enabled := Enable;
  end;

end;

class function TControlUtils.FindItem(const List: TStrings;
 SearchValue: string; StartIndex: Integer): integer;
var
  I: integer;
begin
  if StartIndex + 1 < List.Count then
    Inc(StartIndex, 1);

  for I := StartIndex to List.Count - 1 do
  begin
    if AnsiStartsText(SearchValue, List[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{ TFileUtils }

class procedure TFileUtils.EraseDir(Directory: string);
var
  f: TSearchRec;
begin
  if SysUtils.FindFirst(Directory + '\*.*', faAnyFile, f) = 0 then
    repeat
      if FileExists(Directory + '\' + f.Name) then
        SysUtils.DeleteFile(Directory + '\' + f.Name);
    until (FindNext(f) <> 0);
  SysUtils.FindClose(f);
end;



class function TFileUtils.GetApplicationVersion: string;


begin
  Result := VersionSupport.GetFileVersion;
end;



end.
