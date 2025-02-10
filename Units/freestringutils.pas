unit FreeStringUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, LazUTF8;

resourcestring
  rsAllFiles = 'All files';
  rsJPEGFiles = 'JPEG files';
  rsImageFiles = 'Image files';
  rsBitmapFiles = 'Bitmap files';


function Len(s: string): PtrInt;
function Pos(const SearchForText, SearchInText: string): PtrInt; inline;
function Copy(const s: string; StartCharIndex, CharCount: PtrInt): string; inline;
procedure Delete(var s: String; StartCharIndex, CharCount: PtrInt); inline;
procedure Insert(const source: String; var s: string; StartCharIndex: PtrInt); inline;

function UpperCase(const s: string): string;
function LowerCase(const s: string): string;
Function ReplaceStr(const AText, AFromText, AToText: string): string;inline;
Function ReplaceText(const AText, AFromText, AToText: string): string;inline;

function createDialogFilter(FilterName: string; extensions: array of string;
                            NeedsAll: boolean = True): string;
implementation

function Len(s: string): PtrInt;
begin
     result := UTF8Length(s);
end;

function Pos(const SearchForText, SearchInText: string): PtrInt; inline;
begin
     result := UTF8Pos(SearchForText, SearchInText);
end;

function Copy(const s: string; StartCharIndex, CharCount: PtrInt): string; inline;
begin
     result := UTF8Copy(s, StartCharIndex, CharCount);
end;

procedure Delete(var s: String; StartCharIndex, CharCount: PtrInt); inline;
begin
     UTF8Delete(s, StartCharIndex, CharCount);
end;

procedure Insert(const source: String; var s: string; StartCharIndex: PtrInt); inline;
begin
     UTF8Insert(source, s, StartCharIndex);
end;

function UpperCase(const s: string): string;
begin
     result := UTF8UpperCase(s);
end;

function LowerCase(const s: string): string;
begin
     result := UTF8LowerCase(s);
end;

Function ReplaceStr(const AText, AFromText, AToText: string): string;inline;
begin
     result := UTF8StringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

Function ReplaceText(const AText, AFromText, AToText: string): string;inline;
begin
     result := UTF8StringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

// creates dialog filter for Windows (case insensitive) or GTK (case sensitive)
function createDialogFilter(FilterName: string; extensions: array of string;
                            NeedsAll: boolean = True): string;
var i:integer; ext, fltr: string;
  function makeGTKfilter(ext:string):string;
  var i:integer;
  begin
    Result := '';
    for i:=1 to length(ext) do
      Result += '['+uppercase(ext[i])+lowercase(ext[i])+']';
  end;
begin
  ext:=''; fltr:='';
  Result := FilterName+' (';
  for i:=0 to length(extensions)-1 do
  begin
    ext += '*.' + extensions[i]+';';
    {$IF defined(LCLGtk2) or defined(LCLGtk3)}
    //fltr += '*.' + makeGTKfilter(extensions[i])+';'; // does not work. Change to case insensitive
    fltr += '*.' + extensions[i]+';';
    {$ELSE}
    fltr += '*.' + extensions[i]+';';
    {$ENDIF}
  end;
  ext := LeftStr(ext, length(ext)-1);
  fltr := LeftStr(fltr, length(fltr)-1);
  Result += ext + ')|' + fltr;
  if NeedsAll then
    Result += '|' + rsAllFiles + ' (*.*)|*.*';
end;

end.

