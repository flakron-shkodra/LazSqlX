{
  *******************************************************************
  AUTHOR : Flakron Shkodra 2013
  *******************************************************************
}

unit AsStringUtils;

{$mode objfpc}{$H+}
{$Static ON}


interface

uses
  Classes, SysUtils,db;

type

  { TAsStringUtils }

  TAsStringWrapType = (swtBrackets,swtQuotes,swtNone);

  TArrayOfString = array of string;

  TAsStringUtils = object
  public
    {Counts given word appearance at given Text var; It's matchcase wholeword only}
    class function CountWord(Text:string; Word:string):Integer;

    {Counts given char appearance at given Text}
    class function CountChar(Text:string; aChar:Char):Integer;

    {Remove given chars in Text and returns string without those chars}
    class function RemoveChars(Text: string; Chars: array of char): string;

    {Splits string into array of string based on delimiter char given}
    class function SplitString(Text: string; Delimiter: char): TArrayOfString;

    {Checks if in given input text there are Digits '0'...'9' }
    class function ContainsDigits(Input:string):Boolean;

    {Checks if in given input text there are Letters 'a'..'z' }
    class function ContainsLetters(Input:string):Boolean;

    {Checks if given string can be parsed as number}
    class function IsNumber(Input:string):boolean;

    class function IsLetter(Input:Char):Boolean;

    class function ContainsChars(Text:string; Chars:Array of char):Boolean;

    class function ContainsChar(Text:string; aChar:Char):Boolean;

    class function ByteGUIDToString(field:TField):string;

    class function ByteToString(field:TField):string;

    class function BlobToString(field:TField):string;

    {surrounds a string with brackets or quotations, depending the TAsStringWrapType}
    class function WrapString(aInput:string; typ:TAsStringWrapType):string;

    {Removes vowels if any found uppercase and lowercase}
    class function RemoveVowels(aInput:string):string;

    {Checksi if a given char is in Uppercase}
    class function IsUppercase(c:char):Boolean;

    {Returns only capitalized letter from a given input string}
    class function ExtractUppercaseLetters(aInput:string):String;

    {extracts numbers from a strin, ie if "asdb223nkk2k" it returns "2232"}
    class function ExtractNumbers(aInput:string):string;

    {Splits the string with spaces accordinf to capitalized letters, ie 'IAmNewBee' should return 'I am new bee"}
    class function SplitByUppercaseLetter(aInput:string):string;

    {returns a string excluding symbols and spaces}
    class function GetSafeName(aInput:string):string;


    class function GetFriendlyAlias(Tablename:string):string;

  end;

implementation

{ TAsStringUtils }

class function TAsStringUtils.CountWord(Text: string; Word: string): Integer;
var
  I:Integer;
  Count:Integer;
begin

end;

class function TAsStringUtils.CountChar(Text: string; aChar: Char): Integer;
var
  I: Integer;
  Count:Integer;
begin
  Count := 0;
  for I:=1 to Length(Text) do
  begin
    if Text[I] = aChar then
    Inc(Count,1);
  end;
  Result:=Count;
end;

class function TAsStringUtils.RemoveChars(Text: string; Chars: array of char): string;
var
  s:string;
  c: integer;
  I: Integer;
begin
  c := Length(Chars);
  s:=Text;;
  for I:=0 to c -1 do
  begin
    s := StringReplace(s,Chars[I],'',[rfReplaceAll]);
  end;
  Result := s;
end;

class function TAsStringUtils.SplitString(Text: string;
  Delimiter: char): TArrayOfString;
var
  lst: TStringList;
  I: integer;
begin
  try
    lst := TStringList.Create;
    lst.Delimiter := Delimiter;
    lst.DelimitedText := Text;

    SetLength(Result, lst.Count);

    for I := 0 to lst.Count - 1 do
    begin
      Result[I] := lst[I];
    end;

  finally
    lst.Free;
  end;
end;

class function TAsStringUtils.ContainsDigits(Input: string): Boolean;
var
  I: Integer;
begin
  for I:= 1 to Length(Input) do
  begin
    if Input[I] in ['0'..'9'] then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function TAsStringUtils.ContainsLetters(Input: string): Boolean;
var
  I: Integer;
begin
  for I:= 1 to Length(Input) do
  begin
    if Input[I] in ['a'..'z'] then
    begin
      Result := True;
      Break;
    end;
  end;
end;

class function TAsStringUtils.IsNumber(Input: string): boolean;
var
  N:Integer;
begin
  Result:= TryStrToInt(Input,N);
end;

class function TAsStringUtils.IsLetter(Input: Char): Boolean;
begin
 Result := (Input in ['a'..'z']) or (Input in ['A'..'Z']);
end;

class function TAsStringUtils.ContainsChars(Text: string; Chars: array of char
  ): Boolean;
var
  s:string;
  c: integer;
  I: Integer;
  J: Integer;
begin
  Result:=False;
  c := Length(Text);
  s:=Text;;
  for I:=1 to c do
  begin
    for J:=0 to Length(Chars)-1 do
    begin
      if s[I] = Chars[J] then
      begin
        Result:=True;
        break;
      end;
      if Result then
      Break;
    end;
  end;
end;

class function TAsStringUtils.ContainsChar(Text: string; aChar: Char): Boolean;
var
  s:string;
  c: integer;
  I: Integer;
  J: Integer;
begin
  Result:=False;
  c := Length(Text);
  s:=Text;;
  for I:=1 to c do
  begin
    if s[I] = aChar then
    begin
      Result:=True;
      break;
    end;
  end;
end;


class function TAsStringUtils.ByteGUIDToString(field: TField): string;

procedure Reorder(var InBytes:TBytes);
var
  tmp:Byte;
  tmpArray:TBytes;
  I: Integer;
begin
  SetLength(tmpArray,Length(InBytes));

  for I:=1 to Length(InBytes) do
  begin
    tmpArray[I-1] := InBytes[I];
  end;
  tmp := InBytes[0];

  tmpArray[Length(tmpArray)-1] := tmp;

{0 - 3 - 0
1 - 2 - 1
4 - 5 - 4
6 - 7 - 6}

  tmp := tmpArray[3];
  tmpArray[3] := tmpArray[0];
  tmpArray[0] := tmp;

  tmp := tmpArray[2];
  tmpArray[2] := tmpArray[1];
  tmpArray[1] := tmp;

  tmp := tmpArray[5];
  tmpArray[5] := tmpArray[4];
  tmpArray[4] := tmp;


  tmp := tmpArray[7];
  tmpArray[7] := tmpArray[6];
  tmpArray[6] := tmp;


  InBytes := tmpArray;


end;

var
  s:AnsiString;
  blob : TBlobField;
  m:TMemoryStream;
  b:Byte;
  by:TBytes;
  I: Integer;
  size:Integer;
begin

  blob :=  TBlobField(field);

  m := TMemoryStream.Create;
  try
    blob.SaveToStream(m);
    size := m.Size;
    SetLength(by,size);

    m.Position:= 0;
    while m.Position<m.Size do
    begin
      I:= m.Position;
      m.Read(b,1);
      by[m.Position] := b;
    end;

    m.Position:=m.Size;
    m.Read(b,1);
    by[0] := b;

    Reorder(by);
    for I:=0 to Length(by)-1 do
    begin
      s:=s+IntToHex(by[I],2);

      if Length(s)=8 then
      s:=s+'-';

      if Length(s)=13 then
      s:=s+'-';

      if Length(s)=18 then
      s:=s+'-';

      if Length(s)=23 then
      s:=s+'-';

    end;
  finally
    m.Free;
  end;

  //Result := RemoveChars(GUIDToString(Hold_GUID),['{','}']);
  Result := s;
end;

class function TAsStringUtils.ByteToString(field: TField): string;

var
  s:AnsiString;
  blob : TBlobField;
  m:TMemoryStream;
  b:Byte;
  by:TBytes;
  I: Integer;
  size:Integer;
begin

  blob :=  TBlobField(field);

  m := TMemoryStream.Create;
  try
    blob.SaveToStream(m);
    size := m.Size;
    SetLength(by,size);

    m.Position:= 0;
    while m.Position<m.Size do
    begin
      I:= m.Position;
      m.Read(b,1);
      by[m.Position] := b;
    end;

    s :='0x';

    for I:=0 to Length(by)-1 do
    begin
      s:=s+IntToHex(by[I],2);
    end;

  finally
    m.Free;
  end;

  //Result := RemoveChars(GUIDToString(Hold_GUID),['{','}']);
  Result := s;
end;

class function TAsStringUtils.BlobToString(field: TField): string;
var
  m:TMemoryStream;
  b:TBlobField;
begin
  b := TBlobField(field);
  try
    m := TMemoryStream.Create;
    b.SaveToStream(m);

    if m.Size=16 then
      Result := ByteGUIDToString(field)
    else
      Result := ByteToString(field);

  finally
    m.Free;
  end;
end;

class function TAsStringUtils.WrapString(aInput: string; typ: TAsStringWrapType
 ): string;
begin
 case typ of
  swtBrackets:Result:='['+aInput+']';
  swtQuotes:Result:='"'+aInput+'"';
  swtNone:Result:=aInput;
 end;

end;

class function TAsStringUtils.RemoveVowels(aInput: string): string;
var
 I: Integer;
begin
 Result:='';

 for I:=1 to Length(aInput) do
 begin

   if (aInput[I] in ['a','e','i','o','u','y']) or (aInput[I] in ['A','E','I','O','U','Y']) then
   continue;

   Result:=Result+aInput[I];

 end;
end;

class function TAsStringUtils.IsUppercase(c: char): Boolean;
begin
 Result := LowerCase(c) <> c;
end;

class function TAsStringUtils.ExtractUppercaseLetters(aInput: string): String;
var
 I: Integer;
begin
 Result:='';
 for I:=1 to Length(aInput) do
 begin
   if IsUppercase(aInput[I]) then
   Result:=Result+aInput[I];
 end;
end;

class function TAsStringUtils.ExtractNumbers(aInput: string): string;
var
 I: Integer;
begin
 Result:='';
 for I:=1 to Length(aInput) do
 begin
   if IsNumber(aInput[I]) then
   Result:=Result+aInput[I];
 end;
end;

class function TAsStringUtils.SplitByUppercaseLetter(aInput: string): string;
var
 space: string;
 inChar:string;
 I: Integer;
begin
 Result:='';

 for I:=1 to Length(aInput) do
 begin
  if IsUppercase(aInput[I]) then
    space:=' '
  else
    space:=EmptyStr;

  inChar := aInput[I];
  if I>1 then inChar:=LowerCase(inChar);
  Result:=Result+space+inChar;
 end;

end;

class function TAsStringUtils.GetSafeName(aInput: string): string;
var
 I: Integer;
begin
 Result := EmptyStr;
 for I:=1 to Length(aInput) do
 begin
  if (aInput[I] in ['a'..'z']) or (aInput[I] in ['A'..'Z']) or (aInput[I] in ['0'..'9']) then
  Result := Result+aInput[I];
 end;
end;

class function TAsStringUtils.GetFriendlyAlias(Tablename: string): string;
var
  s:string;
begin
  if Trim(Tablename)=EmptyStr then
  begin
    Result:='';
    Exit;
  end;

 s := LowerCase(TAsStringUtils.ExtractUppercaseLetters(Tablename));
 if Length(s) = Length(Tablename) then
  Result:=s[1]
 else
  Result:=s;
end;

end.


