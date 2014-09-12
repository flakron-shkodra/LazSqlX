unit UnitGetSetText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LConvEncoding, db;

type

  { TGetSetTextClass }

  TGetSetTextClass = class
  private
    procedure GetText(Sender: TField; var aText: string;
           DisplayText: Boolean);
    procedure SetText(Sender: TField; const aText: string);
  public
    procedure AssignGetTextToQueries(F: TCustomForm);
  end;

var
  GetSetTextClass: TGetSetTextClass;

implementation

{ TGetSetTextClass }

procedure TGetSetTextClass.GetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  aText := CP1251ToUTF8(Sender.AsString);
end;

procedure TGetSetTextClass.SetText(Sender: TField; const aText: string);
begin
  Sender.AsString := UTF8ToCP1251(aText);
end;

procedure TGetSetTextClass.AssignGetTextToQueries(F: TCustomForm);
var
  I, J: Integer;
  Q: TDataSet; // doesn't matter if it's TQuery or TTable or TZQuery...
  SF: TStringField;
begin
  for I := 0 to F.ComponentCount - 1 do
    if F.Components[I] is TDataSet then begin
      Q := TDataSet(F.Components[I]);
      for J := 0 to Q.FieldCount - 1 do
        if Q.Fields[J] is TStringField then begin
          SF := TStringField(Q.Fields[J]);
          SF.OnGetText := @GetText;
          SF.OnSetText := @SetText;
        end;

    end;

end;

initialization
  GetSetTextClass := TGetSetTextClass.Create;

finalization
  GetSetTextClass.Free;

end.
