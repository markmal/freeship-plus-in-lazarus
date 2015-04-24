unit ColorIniFile;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles, Graphics;

type
  TColorIniFile = class(TIniFile)
  public
    function ReadColor(const Section, Ident: string; Default: TColor): TColor; virtual;
    procedure WriteColor(const Section, Ident: string; Value: TColor); virtual;
  end;


implementation

function TColorIniFile.ReadColor(const Section, Ident: string; Default: TColor): TColor;
begin
  Result := StringToColor(ReadString(Section, Ident, ColorToString(Default)));
end;

procedure TColorIniFile.WriteColor(const Section, Ident: string; Value: TColor);
begin
  WriteString(Section, Ident, ColorToString(Value));
end;

end.

