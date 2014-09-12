unit StrHelper;

{$mode objfpc}


interface

uses
  Classes, SysUtils;

type

TArrayOfString = array of string;

function SplitString(input:string; seperator:char):TArrayOfString;
function RemoveBrackets(input:string):string;

implementation

function SplitString(input: string; seperator: char): TArrayOfString;
var
  lst:TStringList;
  I: Integer;
begin
  try
    lst := TStringList.Create;
    lst.Delimiter:=seperator;
    lst.DelimitedText:=input;

    SetLength(Result,lst.Count);

    for I:=0 to lst.Count-1 do
    begin
      Result[I] := lst[I];
    end;

  finally
    lst.Free;
  end;
end;

function RemoveBrackets(input: string): string;
begin
  Result := StringReplace(input,'[', '',[rfReplaceAll]);
  Result := StringReplace(input,']','',[rfReplaceAll]);
end;


end.

