unit FreeStringUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, LazUTF8;

function Len(s: string): PtrInt;
function Pos(const SearchForText, SearchInText: string): PtrInt; inline;
function Copy(const s: string; StartCharIndex, CharCount: PtrInt): string; inline;
procedure Delete(var s: String; StartCharIndex, CharCount: PtrInt); inline;
procedure Insert(const source: String; var s: string; StartCharIndex: PtrInt); inline;

function UpperCase(const s: string): string;
function LowerCase(const s: string): string;
Function ReplaceStr(const AText, AFromText, AToText: string): string;inline;
Function ReplaceText(const AText, AFromText, AToText: string): string;inline;

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


end.

